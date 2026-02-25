defmodule Alfred.Simplex.Bridge do
  @moduledoc """
  Bridge SimpleX Chat — Alfred écoute via WebSocket et répond via Mistral.
  GenServer event-driven avec queue de messages et worker async.
  Le GenServer reste toujours réactif (ping, TCP, nouveaux messages).
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
      GenServer.cast(__MODULE__, {:notify_group, text})
    end
  end

  @doc "Envoie une notification dans le groupe SimpleX."
  def send_group_notification(text) do
    if running?() do
      GenServer.cast(__MODULE__, {:notify_group, text})
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
    # Trap exits pour que terminate/2 soit appelé sur Ctrl+C
    Process.flag(:trap_exit, true)

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
      last_message: nil,
      # Queue + worker async
      queue: :queue.new(),
      busy: false,
      worker_ref: nil,
      last_wait_msg: nil
    }

    if token do
      IO.puts("  [Bridge] Mistral authentifié")
    else
      IO.puts("  [Bridge] Mistral NON authentifié — les réponses seront désactivées")
    end

    send(self(), :connect)
    {:ok, state}
  end

  @impl true
  def handle_call(:status, _from, state) do
    info = %{
      started_at: state.started_at,
      host: state.config["host"] || "localhost",
      port: state.config["port"] || 5227,
      contact: state.config["contact"],
      connected: state.socket != nil,
      message_count: state.message_count,
      last_message: state.last_message,
      authenticated: state.token != nil,
      queue_size: :queue.len(state.queue),
      busy: state.busy
    }

    {:reply, info, state}
  end

  @impl true
  def handle_cast({:notify_group, text}, state) do
    if state.socket do
      group = state.config["group"] || "Alfred_1"
      Client.send_group_message(state.socket, group, text)
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
    port = state.config["port"] || 5227

    case Client.connect(to_charlist(host), port) do
      {:ok, socket} ->
        IO.puts("[Bridge] Connecté à #{host}:#{port}")
        # Forcer la re-souscription aux groupes/contacts (critique après restart Tor)
        try do
          Client.send_command(socket, "/_resubscribe all")
          IO.puts("[Bridge] Resubscribe envoyé")
        catch
          _, _ -> :ok
        end
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
        ping_frame = <<1::1, 0::3, 0x9::4, 1::1, 0::7, 0::32>>
        :gen_tcp.send(state.socket, ping_frame)
      catch
        _, _ -> :ok
      end

      schedule_ping()
    end

    {:noreply, state}
  end

  # -- Worker async : résultat ou crash --

  @impl true
  def handle_info({:worker_done, result}, state) do
    IO.puts("[Bridge] Worker terminé")
    # Arrêter de surveiller le process (flush le :DOWN s'il arrive)
    if state.worker_ref, do: Process.demonitor(state.worker_ref, [:flush])
    state = apply_worker_result(state, result)
    dispatch_next(state)
  end

  # Worker crashé avant d'envoyer :worker_done
  @impl true
  def handle_info({:DOWN, ref, :process, _pid, reason}, %{worker_ref: ref} = state) do
    IO.puts("[Bridge] Worker crash: #{inspect(reason)}")
    dispatch_next(%{state | busy: false, worker_ref: nil})
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    # Flush pending_learn avant de mourir (sauvegarde Ctrl+C)
    if is_list(state.pending_learn) and state.pending_learn != [] and state.token do
      try do
        # Sauvegarde rapide en épisode brut (pas de pipeline Mistral complet)
        now = DateTime.utc_now() |> DateTime.to_iso8601()
        episode = %{
          "started_at" => now,
          "ended_at" => now,
          "mode" => "simplex_flush",
          "messages" => state.pending_learn,
          "message_count" => length(state.pending_learn),
          "summary" => nil,
          "topics" => [],
          "extracted_fact_ids" => []
        }
        Alfred.Memory.Episodic.save_episode(episode)
      catch
        _, _ -> :ok
      end
    end

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
        event_type = resp["type"] || get_in(resp, ["data", "type"]) || "?"
        IO.puts("[Bridge] Event: #{event_type}")
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
      {:ok, sender, text, :direct} ->
        IO.puts("[Bridge] Message direct de #{sender}: #{String.slice(text, 0, 50)}")

        case parse_simplex_command(text) do
          {:command, cmd, args} ->
            handle_simplex_command(cmd, args, state, {:direct, sender})

          :not_command ->
            enqueue_message(state, %{target: sender, text: text, context: :direct})
        end

      {:ok, sender, text, {:group, group_name}} ->
        IO.puts("[Bridge] Message groupe ##{group_name} de #{sender}: #{String.slice(text, 0, 50)}")

        case parse_simplex_command(text) do
          {:command, cmd, args} ->
            handle_simplex_command(cmd, args, state, {:group, group_name})

          :not_command ->
            enqueue_message(state, %{target: group_name, text: text, context: :group})
        end

      :ignore ->
        state
    end
  end

  defp extract_incoming_message(resp) do
    case resp do
      # Message direct
      %{"type" => "newChatItem", "chatItem" => item} ->
        extract_from_chat_item(item)

      # Messages de groupe (newChatItems, plural)
      %{"type" => "newChatItems", "chatItems" => items} when is_list(items) ->
        Enum.find_value(items, :ignore, fn item ->
          case extract_from_chat_item(item) do
            {:ok, _, _, _} = result -> result
            :ignore -> nil
          end
        end)

      _ ->
        :ignore
    end
  end

  defp extract_from_chat_item(item) do
    sender =
      get_in(item, ["chatInfo", "contact", "localDisplayName"]) ||
        get_in(item, ["chatItem", "chatDir", "groupMember", "localDisplayName"]) ||
        nil

    text =
      get_in(item, ["chatItem", "content", "msgContent", "text"]) ||
        get_in(item, ["chatItem", "meta", "itemText"]) ||
        ""

    dir_type = get_in(item, ["chatItem", "chatDir", "type"])
    chat_type = get_in(item, ["chatInfo", "type"])
    group_name = get_in(item, ["chatInfo", "groupInfo", "localDisplayName"])

    if text != "" and sender != nil and dir_type != "directSnd" and dir_type != "groupSnd" do
      context = if chat_type == "group" and group_name, do: {:group, group_name}, else: :direct
      {:ok, sender, text, context}
    else
      :ignore
    end
  end

  # -- Commandes SimpleX (/status, /report, etc.) --

  defp parse_simplex_command(text) do
    Alfred.Simplex.CommandParser.parse(text)
  end

  defp handle_simplex_command(cmd, args, state, context) do
    case execute_command(cmd, args, state) do
      nil ->
        # Commande inconnue → traiter comme message normal via Mistral
        text = "/" <> cmd <> if(args != [], do: " " <> Enum.join(args, " "), else: "")
        {target, ctx} = command_context_to_msg(context)
        enqueue_message(state, %{target: target, text: text, context: ctx})

      {:async, fun} ->
        # Commande lente → message d'attente + exécution dans un Task séparé
        IO.puts("[Bridge] Commande /#{cmd} async...")
        send_command_reply(state, context, "Un instant, je réfléchis...")
        socket = state.socket

        Task.start(fn ->
          result =
            try do
              fun.()
            rescue
              e ->
                IO.puts("[Bridge] Erreur async /#{cmd}: #{Exception.message(e)}")
                "Erreur : #{Exception.message(e)}"
            end

          if socket && result do
            try do
              case context do
                {:group, g} -> Client.send_group_message(socket, g, result)
                {:direct, c} -> Client.send_direct_message(socket, c, result)
              end
            catch
              _, _ -> :ok
            end
          end
        end)

        state

      reply ->
        IO.puts("[Bridge] Commande /#{cmd} exécutée")
        send_command_reply(state, context, reply)
        state
    end
  end

  defp execute_command("status", _, state) do
    uptime = DateTime.diff(DateTime.utc_now(), state.started_at, :second)
    uptime_str = format_command_uptime(uptime)

    daemon_status = if Alfred.Daemon.running?(), do: "actif", else: "inactif"
    bridge_status = if state.socket, do: "connecté", else: "déconnecté"
    queue_size = :queue.len(state.queue)

    lines = [
      "Alfred — État actuel",
      "",
      "Daemon : #{daemon_status}",
      "Bridge : #{bridge_status} (#{uptime_str})",
      "Messages traités : #{state.message_count}",
      "File d'attente : #{queue_size}",
      "Mistral : #{if state.token, do: "authentifié", else: "non configuré"}"
    ]

    Enum.join(lines, "\n")
  end

  defp execute_command("report", _, _state) do
    case Alfred.DailyReport.generate() do
      {:ok, _report, text} -> text
      _ -> "Erreur lors de la génération du rapport."
    end
  rescue
    _ -> "Erreur lors de la génération du rapport."
  end

  defp execute_command("library", args, state) do
    case args do
      [] ->
        lines = []

        # Lecture en cours
        lines =
          case Alfred.Library.State.load_current() do
            nil ->
              lines ++ ["Aucune lecture en cours."]

            reading_state ->
              book = reading_state["current_book"]
              day = Alfred.Library.State.current_day()
              logs = reading_state["daily_logs"] || []
              pages_read = length(logs) * (book["pages_per_day"] || 0)
              total = book["total_pages"] || 0
              pct = if total > 0, do: round(pages_read / total * 100), else: 0

              lines ++ ["Lecture en cours : \"#{book["title"]}\" de #{book["author"]}",
                         "Jour #{day}/7 — #{pct}% lu (#{pages_read}/#{total} pages)"]
          end

        # Historique
        history = Alfred.Library.State.load_history()

        lines =
          if history != [] do
            books =
              history
              |> Enum.take(5)
              |> Enum.map(fn h -> "  - \"#{h["title"]}\" de #{h["author"]}" end)

            lines ++ ["", "Livres lus (#{length(history)}) :"] ++ books ++
              if length(history) > 5, do: ["  ...et #{length(history) - 5} autre(s)."], else: []
          else
            lines
          end

        Enum.join(lines, "\n")

      ["next"] ->
        {:async, fn ->
          case Alfred.Library.Scheduler.start_next_book(state.token) do
            {:ok, reading_state} ->
              book = reading_state["current_book"]
              "Nouveau livre : \"#{book["title"]}\" de #{book["author"]}. Bonne lecture !"

            {:error, reason} ->
              "Erreur : #{inspect(reason)}"
          end
        end}

      ["read"] ->
        {:async, fn ->
          case Alfred.Library.Scheduler.read_now(state.token) do
            {:ok, _} -> "Lecture du jour terminée."
            {:error, reason} -> "Erreur : #{inspect(reason)}"
          end
        end}

      _ ->
        nil
    end
  rescue
    _ -> "Erreur lors de la consultation de la bibliothèque."
  end

  defp execute_command("health", _, _state) do
    checks = :alfred_health.check_all()

    lines =
      Enum.map(checks, fn {organ, info} ->
        icon = case info.status do
          :ok -> "[OK]"
          :warning -> "[!!]"
          _ -> "[KO]"
        end

        "#{icon} #{organ}"
      end)

    "Diagnostic Alfred :\n\n" <> Enum.join(lines, "\n")
  rescue
    _ -> "Erreur lors du diagnostic."
  end

  defp execute_command("brain", _, _state) do
    {:async, fn ->
      projects = Alfred.ProjectData.all_for_brain()
      episodes = Alfred.Memory.Episodic.list_episodes() |> Enum.take(-20)
      facts = Alfred.Memory.Semantic.all_facts()

      case Alfred.Brain.Port.send_command(%{
             cmd: "briefing",
             projects: projects,
             episodes: episodes,
             facts: facts,
             culture: [],
             reminders: [],
             now: DateTime.utc_now() |> DateTime.to_iso8601()
           }) do
        {:ok, resp} -> resp["briefing"] || "Aucune analyse disponible."
        {:error, msg} -> "Cerveau indisponible : #{msg}"
      end
    end}
  end

  defp execute_command("cortex", _, _state) do
    {:async, fn ->
      projects = Alfred.ProjectData.all_for_startup()
      episodes = Alfred.Memory.Episodic.list_episodes()
      facts = Alfred.Memory.Semantic.all_facts()

      case Alfred.Cortex.Port.send_command(%{
             cmd: "productivity_stats",
             projects: projects,
             episodes: episodes,
             facts: facts
           }) do
        {:ok, resp} ->
          format_cortex_productivity(resp)

        {:error, msg} ->
          "Cortex indisponible : #{msg}"
      end
    end}
  end

  defp execute_command("soul", _, _state) do
    soul = Alfred.Soul.State.load()
    convictions = Alfred.Soul.Convictions.load()
    traits = soul.traits
    all_conv = convictions["convictions"] || []
    mature = Enum.count(all_conv, fn c -> (c["confidence"] || 0) >= 0.6 end)

    lines = ["Âme d'Alfred :", ""]

    lines =
      lines ++
        Enum.map(traits, fn {k, v} ->
          "  #{k}: #{round(v * 100)}%"
        end)

    lines = lines ++ ["", "Convictions : #{mature} mûre(s) / #{length(all_conv)} total", "Humeur : #{soul.mood}"]

    Enum.join(lines, "\n")
  rescue
    _ -> "Erreur lors de la consultation de l'âme."
  end

  defp execute_command("help", _, _) do
    """
    Commandes disponibles :
    /status — État d'Alfred
    /report — Rapport d'activité du jour
    /library — Livre en cours
    /library next — Commencer un nouveau livre
    /library read — Lire la portion du jour
    /brain — Briefing du cerveau (Julia)
    /cortex — Productivité (R)
    /soul — Traits d'âme et convictions
    /health — Diagnostic des organes
    /help — Cette aide
    """
    |> String.trim()
  end

  defp execute_command(_, _, _), do: nil

  defp send_command_reply(state, context, text) do
    if state.socket do
      try do
        case context do
          {:group, group} -> Client.send_group_message(state.socket, group, text)
          {:direct, contact} -> Client.send_direct_message(state.socket, contact, text)
        end
      catch
        _, _ -> IO.puts("[Bridge] Erreur envoi réponse commande")
      end
    end
  end

  defp command_context_to_msg({:group, group}), do: {group, :group}
  defp command_context_to_msg({:direct, contact}), do: {contact, :direct}

  defp format_cortex_productivity(resp) do
    lines = ["Productivité Alfred :", ""]

    if resp["completion_rate"] do
      lines = lines ++ ["Taux de complétion : #{resp["completion_rate"]}%"]
      lines = if resp["summary"], do: lines ++ [resp["summary"]], else: lines
      Enum.join(lines, "\n")
    else
      resp["summary"] || "Pas de données de productivité."
    end
  end

  defp format_command_uptime(seconds) do
    cond do
      seconds < 60 -> "#{seconds}s"
      seconds < 3600 -> "#{div(seconds, 60)}min"
      seconds < 86400 -> "#{div(seconds, 3600)}h #{rem(div(seconds, 60), 60)}min"
      true -> "#{div(seconds, 86400)}j #{rem(div(seconds, 3600), 24)}h"
    end
  end

  # -- Queue + dispatch async --

  defp enqueue_message(state, msg) do
    if state.token == nil or state.session == nil do
      IO.puts("[Bridge] Pas de token Mistral, message ignoré")
      state
    else
      if state.busy do
        IO.puts("[Bridge] Alfred occupé, message en file d'attente (queue: #{:queue.len(state.queue) + 1})")
        state = maybe_send_waiting(state, msg)
        %{state | queue: :queue.in(msg, state.queue)}
      else
        dispatch_worker(state, msg)
      end
    end
  end

  defp dispatch_worker(state, msg) do
    IO.puts("[Bridge] Dispatch worker pour: #{String.slice(msg.text, 0, 50)}")
    bridge_pid = self()
    socket = state.socket
    session = state.session
    token = state.token
    soul = state.soul
    culture = state.culture
    pending_learn = state.pending_learn

    {:ok, pid} =
      Task.start(fn ->
        result = do_process_message(msg, socket, session, token, soul, culture, pending_learn)
        send(bridge_pid, {:worker_done, result})
      end)

    ref = Process.monitor(pid)
    %{state | busy: true, worker_ref: ref}
  end

  defp dispatch_next(state) do
    case :queue.out(state.queue) do
      {{:value, next_msg}, new_queue} ->
        IO.puts("[Bridge] Dépile message suivant (restant: #{:queue.len(new_queue)})")
        state = %{state | queue: new_queue, busy: false, worker_ref: nil}
        {:noreply, dispatch_worker(state, next_msg)}

      {:empty, _} ->
        {:noreply, %{state | busy: false, worker_ref: nil}}
    end
  end

  defp apply_worker_result(state, result) do
    case result do
      {:ok, new_session, new_pending} ->
        %{
          state
          | session: new_session,
            message_count: state.message_count + 1,
            pending_learn: new_pending,
            last_message: DateTime.utc_now()
        }

      {:error, new_session} ->
        %{state | session: new_session}

      _ ->
        state
    end
  end

  # -- Worker : traitement du message (tourne dans un process séparé) --

  defp do_process_message(msg, socket, session, token, soul, culture, pending_learn) do
    IO.puts("[Bridge] Envoi à Mistral...")

    case Commands.send_message(session, token, msg.text, soul, culture) do
      {:ok, response, new_session} ->
        IO.puts("[Bridge] Réponse Mistral reçue (#{String.length(response)} chars)")

        # Envoyer la réponse via socket
        if socket do
          try do
            case msg.context do
              :group -> Client.send_group_message(socket, msg.target, response)
              :direct -> Client.send_direct_message(socket, msg.target, response)
            end
          catch
            _, _ -> IO.puts("[Bridge] Erreur envoi réponse")
          end
        end

        # Accumulate for batch learning
        new_pending =
          pending_learn ++
            [
              %{"role" => "user", "content" => msg.text},
              %{"role" => "assistant", "content" => response}
            ]

        new_pending =
          if length(new_pending) >= 10 do
            safe_learn(new_pending, token)
            []
          else
            new_pending
          end

        {:ok, new_session, new_pending}

      {:error, reason, new_session} ->
        IO.puts("[Bridge] ERREUR Mistral: #{inspect(reason)}")

        if socket do
          try do
            err_msg = "Je suis navré, une erreur est survenue."

            case msg.context do
              :group -> Client.send_group_message(socket, msg.target, err_msg)
              :direct -> Client.send_direct_message(socket, msg.target, err_msg)
            end
          catch
            _, _ -> :ok
          end
        end

        {:error, new_session}
    end
  rescue
    e ->
      IO.puts("[Bridge] CRASH worker: #{Exception.message(e)}")
      :crash
  end

  # -- Salle d'attente --

  defp maybe_send_waiting(state, msg) do
    now = System.monotonic_time(:second)
    last = state.last_wait_msg || 0

    # Anti-spam : pas plus d'un message d'attente toutes les 10s
    if now - last > 10 and state.socket do
      wait_msg = "⏳ Un instant, Monsieur..."

      try do
        case msg.context do
          :group -> Client.send_group_message(state.socket, msg.target, wait_msg)
          :direct -> Client.send_direct_message(state.socket, msg.target, wait_msg)
        end
      catch
        _, _ -> :ok
      end

      %{state | last_wait_msg: now}
    else
      state
    end
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
