defmodule Alfred.Daemon.Commands do
  @moduledoc """
  Commandes CLI pour le mode daemon d'Alfred.
  `alfred daemon` — demande le mot de passe, lance tout en background.
  """

  alias Alfred.Butler
  alias Alfred.Colors

  def handle([]) do
    # Vérifier si déjà actif
    case Alfred.Launcher.read_pid_file() do
      {:ok, pid} ->
        if Alfred.Launcher.process_alive?(pid) do
          Butler.say("Alfred est déjà actif (PID #{pid}), Monsieur.")
          IO.puts("  Log  : ~/.alfred/alfred.log")
          IO.puts("  Stop : alfred daemon stop\n")
          return_early()
        end

      :error ->
        :ok
    end

    # Demander le mot de passe
    password = Alfred.Input.prompt_password("  Mot de passe : ")

    if password == "" do
      Butler.say("Mot de passe requis, Monsieur.")
      return_early()
    end

    # Authentifier
    case Alfred.Chat.Commands.authenticate_with_password(password) do
      {:ok, token, _soul, _culture} ->
        Butler.say("Bien, Monsieur. Démarrage d'Alfred...\n")
        launch_background(token)

      {:error, reason} ->
        Butler.say(reason)
    end
  end

  def handle(["status"]) do
    case Alfred.Launcher.read_pid_file() do
      {:ok, pid} ->
        if Alfred.Launcher.process_alive?(pid) do
          Butler.say("Alfred est actif (PID #{pid}), Monsieur.")
          IO.puts("  Log : ~/.alfred/alfred.log\n")
        else
          Butler.say("Alfred n'est pas actif, Monsieur. (PID #{pid} mort)")
          Alfred.Launcher.delete_pid_file()
        end

      :error ->
        Butler.say("Alfred n'est pas actif, Monsieur.")
    end
  end

  def handle(["stop"]) do
    Alfred.Launcher.stop()
  end

  def handle(["log"]) do
    log_file = Path.expand("~/.alfred/alfred.log")

    if File.exists?(log_file) do
      case File.read(log_file) do
        {:ok, content} ->
          lines = String.split(content, "\n") |> Enum.take(-30)
          Butler.say("Dernières lignes du log :\n")
          Enum.each(lines, &IO.puts/1)
          IO.puts("")

        _ ->
          Butler.say("Impossible de lire le log, Monsieur.")
      end
    else
      Butler.say("Pas de fichier log, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes daemon sont :\n")

    IO.puts("""
      alfred daemon              Démarrer Alfred (daemon complet)
      alfred daemon status       État du daemon
      alfred daemon stop         Arrêter Alfred
      alfred daemon log          Voir les derniers logs
    """)
  end

  # -- Private --

  defp launch_background(token) do
    project_root = Alfred.Launcher.find_project_root()
    log_file = Path.expand("~/.alfred/alfred.log")

    # Passer la clé Mistral via env au process background
    env_prefix = "MISTRAL_API_KEY=#{token}"

    System.cmd("bash", [
      "-c",
      "cd #{project_root} && #{env_prefix} nohup mix run --no-halt -e 'Alfred.Launcher.start()' > #{log_file} 2>&1 & echo $!"
    ])
    |> case do
      {output, 0} ->
        bg_pid = String.trim(output)
        IO.puts("  #{Colors.icon_ok()} Alfred démarré (PID #{bg_pid})")
        IO.puts("  #{Colors.dim("Log  : ~/.alfred/alfred.log")}")
        IO.puts("  #{Colors.dim("Stop : alfred daemon stop")}\n")

      {_, _} ->
        Butler.say("Erreur au lancement, Monsieur.")
    end
  end

  defp return_early, do: :ok
end
