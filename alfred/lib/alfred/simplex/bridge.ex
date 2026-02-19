defmodule Alfred.Simplex.Bridge do
  @moduledoc """
  Bridge SimpleX Chat — Alfred écoute via WebSocket et répond via Mistral.
  GenServer event-driven (réception asynchrone {:tcp, socket, data}).
  """

  use GenServer

  alias Alfred.Simplex.{Client, WebSocket}
  alias Alfred.Chat.Commands
  alias Alfred.Memory.Learner

  @config_file "simplex_config.json"
  @reconnect_delay 5_000
  @ping_interval 30_000

  # -- API publique --

  def start_link(config) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  def stop do
    if running?() do
      GenServer.stop(__MODULE__, :normal)
    end
  end

  def running? do
    Process.whereis(__MODULE__) != nil
  end

  def status do
    if running?() do
      GenServer.call(__MODULE__, :status)
    else
      :not_running
    end
  end

  def send_notification(text) do
    if running?() do
      GenServer.cast(__MODULE__, {:notify, text})
    end
  end

  def send_message(contact, text) do
    if running?() do
      GenServer.cast(__MODULE__, {:send, contact, text})
    end
  end

  @doc "Charge la config SimpleX depuis le disque."
  def load_config do
    case Alfred.Storage.Local.read(@config_file) do
      data when is_map(data) and map_size(data) > 0 -> {:ok, data}
      _ -> :no_config
    end
  end

  @doc "Sauvegarde la config SimpleX."
  def save_config(config) do
    Alfred.Storage.Local.write(@config_file, config)
  end

  # -- Callbacks GenServer --

  @impl true
  def init(config) do
    {token, soul, culture} =
      case Commands.authenticate() do
        {:ok, t, s, c} -> {t, s, c}
        _ -> {nil, nil, []}
      end

    session = if token, do: Commands.build_session("simplex", soul, culture), else: nil

    state = %{
      config: config,
      socket: nil,
      token: token,
      soul: soul,
      culture: culture,
      session: session,
      message_count: 0,
      pending_learn: [],
      ws_buffer: <<>>,
      started_at: DateTime.utc_now(),
      last_message: nil
    }

    send(self(), :connect)
    {:ok, state}
  end

  @impl true
  def handle_call(:status, _from, state) do
    info = %{
      started_at: state.started_at,
      host: state.config["host"] || "localhost",
      port: state.config["port"] || 5225,
      contact: state.config["contact"],
      connected: state.socket != nil,
      message_count: state.message_count,
      last_message: state.last_message,
      authenticated: state.token != nil
    }

    {:reply, info, state}
  end

  @impl true
  def handle_cast({:notify, text}, state) do
    contact = state.config["contact"]

    if state.socket && contact do
      Client.send_direct_message(state.socket, contact, text)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:send, contact, text}, state) do
    if state.socket do
      Client.send_direct_message(state.socket, contact, text)
    end

    {:noreply, state}
  end

  # -- Connection management --

  @impl true
  def handle_info(:connect, state) do
    host = state.config["host"] || "localhost"
    port = state.config["port"] || 5225

    case Client.connect(to_charlist(host), port) do
      {:ok, socket} ->
        schedule_ping()
        {:noreply, %{state | socket: socket, ws_buffer: <<>>}}

      {:error, _reason} ->
        Process.send_after(self(), :connect, @reconnect_delay)
        {:noreply, state}
    end
  end

  # -- Incoming TCP data (WebSocket frames) --

  @impl true
  def handle_info({:tcp, _socket, data}, state) do
    {frames, new_buffer} = WebSocket.decode_frames(data, state.ws_buffer)

    state =
      Enum.reduce(frames, %{state | ws_buffer: new_buffer}, fn frame, acc ->
        handle_frame(frame, acc)
      end)

    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, _socket}, state) do
    Process.send_after(self(), :connect, @reconnect_delay)
    {:noreply, %{state | socket: nil, ws_buffer: <<>>}}
  end

  @impl true
  def handle_info({:tcp_error, _socket, _reason}, state) do
    Process.send_after(self(), :connect, @reconnect_delay)
    {:noreply, %{state | socket: nil, ws_buffer: <<>>}}
  end

  @impl true
  def handle_info(:ping, state) do
    if state.socket do
      try do
        WebSocket.send_text(state.socket, "")
      catch
        _, _ -> :ok
      end

      schedule_ping()
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    if state.socket do
      try do
        WebSocket.close(state.socket)
      catch
        _, _ -> :ok
      end
    end

    :ok
  end

  # -- Frame handling --

  defp handle_frame({:text, payload}, state) do
    if state.config["debug"] do
      IO.puts("[SimpleX debug] #{payload}")
    end

    case Client.parse_response(payload) do
      {:event, resp} ->
        handle_event(resp, state)

      {:response, _corr_id, _resp} ->
        state

      _ ->
        state
    end
  end

  defp handle_frame(:ping, state) do
    if state.socket do
      pong = WebSocket.pong_frame()

      try do
        :gen_tcp.send(state.socket, pong)
      catch
        _, _ -> :ok
      end
    end

    state
  end

  defp handle_frame(:close, state) do
    Process.send_after(self(), :connect, @reconnect_delay)
    %{state | socket: nil, ws_buffer: <<>>}
  end

  defp handle_frame(_other, state), do: state

  # -- Event handling (incoming messages) --

  defp handle_event(resp, state) do
    case extract_incoming_message(resp) do
      {:ok, sender, text} ->
        process_incoming_message(sender, text, state)

      :ignore ->
        state
    end
  end

  defp extract_incoming_message(resp) do
    # SimpleX Chat envoie des événements avec différentes structures
    # Le type principal pour les messages entrants est "newChatItem"
    case resp do
      %{"type" => "newChatItem", "chatItem" => item} ->
        sender =
          get_in(item, ["chatInfo", "contact", "localDisplayName"]) ||
            "unknown"

        text =
          get_in(item, ["chatItem", "content", "text"]) ||
            get_in(item, ["meta", "itemText"]) ||
            ""

        if text != "" and sender != "unknown" do
          {:ok, sender, text}
        else
          :ignore
        end

      _ ->
        :ignore
    end
  end

  defp process_incoming_message(sender, text, state) do
    if state.token == nil or state.session == nil do
      state
    else
      case Commands.send_message(state.session, state.token, text, state.soul, state.culture) do
        {:ok, response, new_session} ->
          # Répondre au sender
          if state.socket do
            Client.send_direct_message(state.socket, sender, response)
          end

          # Accumulate for batch learning
          new_pending =
            state.pending_learn ++
              [
                %{"role" => "user", "content" => text},
                %{"role" => "assistant", "content" => response}
              ]

          new_pending =
            if length(new_pending) >= 10 do
              safe_learn(new_pending, state.token)
              []
            else
              new_pending
            end

          %{
            state
            | session: new_session,
              message_count: state.message_count + 1,
              pending_learn: new_pending,
              last_message: DateTime.utc_now()
          }

        {:error, _reason, new_session} ->
          if state.socket do
            Client.send_direct_message(
              state.socket,
              sender,
              "Je suis navré, une erreur est survenue."
            )
          end

          %{state | session: new_session}
      end
    end
  rescue
    _ -> state
  end

  defp safe_learn(messages, token) do
    Learner.learn_from_messages(messages, token, "simplex")
  rescue
    _ -> :ok
  end

  defp schedule_ping do
    Process.send_after(self(), :ping, @ping_interval)
  end
end
