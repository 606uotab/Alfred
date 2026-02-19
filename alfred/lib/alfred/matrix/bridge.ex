defmodule Alfred.Matrix.Bridge do
  @moduledoc """
  Bridge Matrix/Element — Alfred écoute une room et répond via Mistral.
  GenServer avec boucle de synchronisation.
  """

  use GenServer

  alias Alfred.Matrix.Client
  alias Alfred.Chat.Commands
  alias Alfred.Memory.Learner

  @config_file "matrix_config.json"
  @sync_interval 5_000

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

  @doc """
  Charge la config Matrix depuis le disque.
  """
  def load_config do
    case Alfred.Storage.Local.read(@config_file) do
      data when is_map(data) and map_size(data) > 0 -> {:ok, data}
      _ -> :no_config
    end
  end

  @doc """
  Sauvegarde la config Matrix.
  """
  def save_config(config) do
    Alfred.Storage.Local.write(@config_file, config)
  end

  # -- Callbacks GenServer --

  @impl true
  def init(config) do
    # Authenticate for Mistral
    {token, soul, culture} =
      case Commands.authenticate() do
        {:ok, t, s, c} -> {t, s, c}
        _ -> {nil, nil, []}
      end

    session = if token, do: Commands.build_session("matrix", soul, culture), else: nil

    state = %{
      config: config,
      token: token,
      soul: soul,
      culture: culture,
      session: session,
      since: config["since"],
      message_count: 0,
      pending_learn: [],
      started_at: DateTime.utc_now(),
      last_sync: nil
    }

    # Start sync loop
    send(self(), :sync)
    {:ok, state}
  end

  @impl true
  def handle_call(:status, _from, state) do
    info = %{
      started_at: state.started_at,
      room_id: state.config["room_id"],
      homeserver: state.config["homeserver"],
      message_count: state.message_count,
      last_sync: state.last_sync,
      authenticated: state.token != nil
    }

    {:reply, info, state}
  end

  @impl true
  def handle_cast({:notify, text}, state) do
    room_id = state.config["room_id"]

    if room_id do
      Client.send_message(state.config, room_id, text)
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(:sync, state) do
    state = do_sync(state)
    Process.send_after(self(), :sync, @sync_interval)
    {:noreply, state}
  end

  # -- Sync logic --

  defp do_sync(state) do
    case Client.sync(state.config, state.since) do
      {:ok, response} ->
        next_batch = response["next_batch"]
        new_messages = extract_messages(response, state.config)

        # Only process messages if we have a since token (skip initial sync)
        state =
          if state.since != nil and state.token != nil do
            process_messages(new_messages, state)
          else
            state
          end

        # Persist since token
        updated_config = Map.put(state.config, "since", next_batch)
        save_config(updated_config)

        %{state |
          config: updated_config,
          since: next_batch,
          last_sync: DateTime.utc_now()
        }

      {:error, _reason} ->
        state
    end
  end

  defp extract_messages(response, config) do
    my_user_id = config["user_id"]
    room_id = config["room_id"]

    rooms = get_in(response, ["rooms", "join"]) || %{}

    case rooms[room_id] do
      nil ->
        []

      room_data ->
        events = get_in(room_data, ["timeline", "events"]) || []

        events
        |> Enum.filter(fn event ->
          event["type"] == "m.room.message" and
            event["sender"] != my_user_id and
            get_in(event, ["content", "msgtype"]) == "m.text"
        end)
        |> Enum.map(fn event ->
          %{
            sender: event["sender"],
            body: get_in(event, ["content", "body"]),
            event_id: event["event_id"]
          }
        end)
    end
  end

  defp process_messages([], state), do: state

  defp process_messages(messages, state) do
    Enum.reduce(messages, state, fn msg, acc ->
      process_one_message(msg, acc)
    end)
  end

  defp process_one_message(msg, state) do
    input = msg.body

    case Commands.send_message(state.session, state.token, input, state.soul, state.culture) do
      {:ok, response, new_session} ->
        # Send response back to Matrix
        Client.send_message(state.config, state.config["room_id"], response)

        # Accumulate for batch learning
        new_pending = state.pending_learn ++ [
          %{"role" => "user", "content" => input},
          %{"role" => "assistant", "content" => response}
        ]

        # Learn every 10 messages
        new_pending =
          if length(new_pending) >= 10 do
            safe_learn(new_pending, state.token)
            []
          else
            new_pending
          end

        %{state |
          session: new_session,
          message_count: state.message_count + 1,
          pending_learn: new_pending
        }

      {:error, _reason, new_session} ->
        Client.send_message(state.config, state.config["room_id"],
          "Je suis navré, une erreur est survenue.")

        %{state | session: new_session}
    end
  rescue
    _ ->
      state
  end

  defp safe_learn(messages, token) do
    Learner.learn_from_messages(messages, token, "matrix")
  rescue
    _ -> :ok
  end
end
