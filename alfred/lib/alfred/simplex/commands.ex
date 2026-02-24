defmodule Alfred.Simplex.Commands do
  @moduledoc """
  Commandes CLI pour le bridge SimpleX Chat.
  """

  alias Alfred.Butler
  alias Alfred.Colors
  alias Alfred.Simplex.Bridge

  def handle(["connect"]) do
    case Bridge.load_config() do
      {:ok, config} ->
        Butler.say("Configuration SimpleX existante détectée.")
        IO.puts("  Hôte    : #{config["host"] || "localhost"}")
        IO.puts("  Port    : #{config["port"] || 5227}")
        IO.puts("  Contact : #{config["contact"]}\n")

        connect_with_config(config)

      :no_config ->
        Butler.say("Configuration du bridge SimpleX.\n")
        config = prompt_config()
        connect_with_config(config)
    end
  end

  def handle(["status"]) do
    case Bridge.status() do
      :not_running ->
        case Bridge.load_config() do
          {:ok, config} ->
            Butler.say("Bridge SimpleX configuré mais inactif.\n")
            IO.puts("  Hôte    : #{config["host"] || "localhost"}")
            IO.puts("  Port    : #{config["port"] || 5227}")
            IO.puts("  Contact : #{config["contact"]}")
            IO.puts("\n  Lancez : alfred simplex connect\n")

          :no_config ->
            Butler.say("Bridge SimpleX non configuré. Lancez : alfred simplex connect")
        end

      info ->
        Butler.say("Bridge SimpleX — statut :\n")
        connected_icon = if info.connected, do: Colors.icon_ok(), else: Colors.icon_err()
        IO.puts("  #{connected_icon} #{info.host}:#{info.port}")
        IO.puts("  Contact  : #{info.contact || "aucun"}")
        IO.puts("  Messages : #{info.message_count} traités")
        IO.puts("  Mistral  : #{if info.authenticated, do: "connecté", else: "non connecté"}")

        if info.last_message do
          IO.puts("  Dernier  : #{DateTime.to_iso8601(info.last_message)}")
        end

        IO.puts("")
    end
  end

  def handle(["send" | rest]) when rest != [] do
    case Bridge.load_config() do
      {:ok, config} ->
        {contact, text} = parse_send_args(rest, config["contact"])

        cond do
          contact == nil ->
            Butler.say("Veuillez préciser un contact : alfred simplex send <contact> <message>")

          not Bridge.running?() ->
            Butler.say("Le bridge n'est pas actif. Lancez : alfred simplex connect")

          true ->
            Bridge.send_message(contact, text)
            Butler.say("Message envoyé à #{contact}.")
        end

      :no_config ->
        Butler.say("Bridge non configuré. Lancez : alfred simplex connect")
    end
  end

  def handle(["disconnect"]) do
    if Bridge.running?() do
      Bridge.stop()
      Butler.say("Bridge SimpleX déconnecté, Monsieur.")
    else
      Butler.say("Le bridge n'est pas actif, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes SimpleX sont :\n")

    IO.puts("""
      alfred simplex connect              Connecter à SimpleX Chat
      alfred simplex status                État du bridge
      alfred simplex send <texte>          Envoyer au contact par défaut
      alfred simplex send <contact> <msg>  Envoyer à un contact
      alfred simplex disconnect            Déconnecter le bridge
    """)
  end

  # -- Privé --

  defp prompt_config do
    host = IO.gets("  Hôte (défaut: localhost) : ") |> String.trim()
    host = if host == "", do: "localhost", else: host

    port_str = IO.gets("  Port (défaut: 5227) : ") |> String.trim()

    port =
      case Integer.parse(port_str) do
        {p, ""} -> p
        _ -> 5227
      end

    contact = IO.gets("  Contact par défaut (nom SimpleX) : ") |> String.trim()
    contact = if contact == "", do: nil, else: contact

    %{
      "host" => host,
      "port" => port,
      "contact" => contact
    }
  end

  defp connect_with_config(config) do
    host = to_charlist(config["host"] || "localhost")
    port = config["port"] || 5227

    Butler.say("Vérification de simplex-chat sur #{config["host"] || "localhost"}:#{port}...")

    # Quick connectivity check
    case :gen_tcp.connect(host, port, [], 2_000) do
      {:ok, test_socket} ->
        :gen_tcp.close(test_socket)

        Bridge.save_config(config)

        # S'assurer que la supervision (scheduler, clock, TaskSupervisor) tourne
        Alfred.Application.start()

        if Bridge.running?() do
          Bridge.stop()
          Process.sleep(100)
        end

        case Bridge.start_link(config) do
          {:ok, _pid} ->
            IO.puts("  #{Colors.icon_ok()} Connecté\n")
            Butler.say("Bridge SimpleX actif. J'écoute les messages, Monsieur.")
            IO.puts("  #{Colors.dim("Ctrl+C pour arrêter le bridge.")}\n")

            # Bloquer le processus pour garder la VM en vie (comme le daemon)
            bridge_loop()

          {:error, reason} ->
            Butler.say("Erreur de démarrage : #{inspect(reason)}")
        end

      {:error, :econnrefused} ->
        Butler.say(
          "SimpleX Chat ne semble pas actif sur le port #{port}.\n" <>
            "  Lancez : bash alfred/scripts/simplex-sandbox.sh"
        )

      {:error, reason} ->
        Butler.say("Erreur de connexion : #{inspect(reason)}")
    end
  end

  defp bridge_loop do
    receive do
      :stop -> :ok
    end
  end

  defp parse_send_args([single], default_contact) do
    {default_contact, single}
  end

  defp parse_send_args([contact | rest], _default_contact) do
    {contact, Enum.join(rest, " ")}
  end

  defp parse_send_args([], _default_contact), do: {nil, ""}
end
