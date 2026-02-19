defmodule Alfred.Daemon.Commands do
  @moduledoc """
  Commandes CLI pour le mode daemon d'Alfred.
  """

  alias Alfred.Butler
  alias Alfred.Colors

  def handle(["start"]) do
    if Alfred.Daemon.running?() do
      Butler.say("Le daemon est déjà actif, Monsieur.")
    else
      Butler.say("Daemon Alfred activé. Je veille sur vos affaires.")
      IO.puts("  #{Colors.dim("Vérification toutes les 60 secondes. Ctrl+C pour arrêter.")}\n")

      {:ok, _pid} = Alfred.Daemon.start_link()

      # Bloquer le processus pour garder le daemon en vie
      daemon_loop()
    end
  end

  def handle(["status"]) do
    case Alfred.Daemon.status() do
      :not_running ->
        Butler.say("Le daemon n'est pas actif, Monsieur. Lancez : alfred daemon start")

      info ->
        Butler.say("Daemon Alfred — statut :\n")
        IO.puts("  Actif depuis   : #{info.uptime_human}")
        IO.puts("  Vérifications  : #{info.check_count}")
        IO.puts("  Rappels notifiés : #{info.reminders_notified}")

        if info.last_check do
          IO.puts("  Dernière vérif : #{DateTime.to_iso8601(info.last_check)}")
        end

        IO.puts("")
    end
  end

  def handle(["stop"]) do
    if Alfred.Daemon.running?() do
      Alfred.Daemon.stop()
      Butler.say("Daemon arrêté, Monsieur.")
    else
      Butler.say("Le daemon n'est pas actif, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes daemon sont :\n")

    IO.puts("""
      alfred daemon start     Démarrer le daemon (foreground)
      alfred daemon status    État du daemon
      alfred daemon stop      Arrêter le daemon
    """)
  end

  defp daemon_loop do
    receive do
      :stop -> :ok
    end
  end
end
