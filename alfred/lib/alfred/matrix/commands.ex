defmodule Alfred.Matrix.Commands do
  @moduledoc """
  Commandes CLI pour le bridge Matrix/Element.
  """

  alias Alfred.Butler
  alias Alfred.Colors
  alias Alfred.Matrix.{Client, Bridge}

  def handle(["connect"]) do
    case Bridge.load_config() do
      {:ok, config} ->
        Butler.say("Configuration Matrix existante détectée.")
        IO.puts("  Serveur : #{config["homeserver"]}")
        IO.puts("  Room    : #{config["room_id"]}\n")

        connect_with_config(config)

      :no_config ->
        Butler.say("Configuration du bridge Matrix.\n")
        config = prompt_config()
        connect_with_config(config)
    end
  end

  def handle(["status"]) do
    case Bridge.status() do
      :not_running ->
        case Bridge.load_config() do
          {:ok, config} ->
            Butler.say("Bridge Matrix configuré mais inactif.\n")
            IO.puts("  Serveur : #{config["homeserver"]}")
            IO.puts("  Room    : #{config["room_id"]}")
            IO.puts("\n  Lancez : alfred matrix connect\n")

          :no_config ->
            Butler.say("Bridge Matrix non configuré. Lancez : alfred matrix connect")
        end

      info ->
        Butler.say("Bridge Matrix — statut :\n")
        IO.puts("  #{Colors.icon_ok()} Connecté à #{info.homeserver}")
        IO.puts("  Room     : #{info.room_id}")
        IO.puts("  Messages : #{info.message_count} traités")
        IO.puts("  Mistral  : #{if info.authenticated, do: "connecté", else: "non connecté"}")

        if info.last_sync do
          IO.puts("  Dernière sync : #{DateTime.to_iso8601(info.last_sync)}")
        end

        IO.puts("")
    end
  end

  def handle(["send" | rest]) when rest != [] do
    text = Enum.join(rest, " ")

    case Bridge.load_config() do
      {:ok, config} ->
        room_id = config["room_id"]

        case Client.send_message(config, room_id, text) do
          {:ok, _} ->
            Butler.say("Message envoyé dans la room.")

          {:error, reason} ->
            Butler.say("Je suis navré Monsieur, erreur d'envoi : #{reason}")
        end

      :no_config ->
        Butler.say("Bridge non configuré. Lancez : alfred matrix connect")
    end
  end

  def handle(["disconnect"]) do
    if Bridge.running?() do
      Bridge.stop()
      Butler.say("Bridge Matrix déconnecté, Monsieur.")
    else
      Butler.say("Le bridge n'est pas actif, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes Matrix sont :\n")

    IO.puts("""
      alfred matrix connect      Connecter Alfred à une room Element
      alfred matrix status        État du bridge
      alfred matrix send <texte>  Envoyer un message dans la room
      alfred matrix disconnect    Déconnecter le bridge
    """)
  end

  # -- Privé --

  defp prompt_config do
    homeserver = IO.gets("  Homeserver (ex: https://matrix.org) : ") |> String.trim()
    token = IO.gets("  Access token : ") |> String.trim()
    room_id = IO.gets("  Room ID (ex: !abc:matrix.org) : ") |> String.trim()

    %{
      "homeserver" => homeserver,
      "access_token" => token,
      "room_id" => room_id,
      "user_id" => nil,
      "since" => nil
    }
  end

  defp connect_with_config(config) do
    Butler.say("Vérification de la connexion...")

    case Client.whoami(config) do
      {:ok, %{"user_id" => user_id}} ->
        config = Map.put(config, "user_id", user_id)
        Bridge.save_config(config)

        IO.puts("  #{Colors.icon_ok()} Connecté en tant que #{user_id}\n")

        # Join room if specified
        if config["room_id"] do
          Client.join_room(config, config["room_id"])
        end

        # Start bridge
        if Bridge.running?() do
          Bridge.stop()
          Process.sleep(100)
        end

        {:ok, _pid} = Bridge.start_link(config)
        Butler.say("Bridge Matrix actif. J'écoute la room, Monsieur.")

      {:error, reason} ->
        Butler.say("Je suis navré Monsieur, connexion impossible : #{reason}")
    end
  end
end
