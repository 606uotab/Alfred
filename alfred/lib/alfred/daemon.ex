defmodule Alfred.Daemon do
  @moduledoc """
  Mode daemon — Alfred veille en fond, vérifie les rappels et exécute la maintenance.
  GenServer avec timer périodique.
  """

  use GenServer

  @check_interval 60_000
  @maintenance_every 60
  @consolidation_every 1_440

  # -- API publique --

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def status do
    if running?() do
      GenServer.call(__MODULE__, :status)
    else
      :not_running
    end
  end

  def stop do
    if running?() do
      GenServer.stop(__MODULE__, :normal)
    end
  end

  def running? do
    Process.whereis(__MODULE__) != nil
  end

  # -- Callbacks GenServer --

  @impl true
  def init(_opts) do
    schedule_check()

    state = %{
      started_at: DateTime.utc_now(),
      check_count: 0,
      last_check: nil,
      notifications: [],
      reminders_notified: 0,
      last_report_date: nil,
      last_reading_date: nil,
      last_journal_date: nil,
      last_consolidation_date: nil,
      last_news_date: nil
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:status, _from, state) do
    uptime = DateTime.diff(DateTime.utc_now(), state.started_at, :second)

    info = %{
      started_at: state.started_at,
      uptime_seconds: uptime,
      uptime_human: format_uptime(uptime),
      check_count: state.check_count,
      last_check: state.last_check,
      reminders_notified: state.reminders_notified
    }

    {:reply, info, state}
  end

  @impl true
  def handle_info(:check, state) do
    state = do_check(state)
    schedule_check()
    {:noreply, state}
  end

  # -- Logique de check --

  defp do_check(state) do
    count = state.check_count + 1
    now = DateTime.utc_now()

    # 1. Rappels gérés par Alfred.Clock (supervisé)

    # 2. Maintenance périodique (toutes les heures)
    if rem(count, @maintenance_every) == 0 do
      safe_run(fn -> Alfred.Maintenance.run_startup_checks() end)
    end

    # 3. Consolidation mémoire (toutes les 24h)
    if rem(count, @consolidation_every) == 0 do
      safe_run(fn -> Alfred.Memory.Semantic.consolidate() end)
    end

    # 4. Évolution soul par patterns (toutes les 6h = 360 checks)
    if rem(count, 360) == 0 do
      safe_run(fn ->
        patterns = Alfred.Memory.Procedural.active_patterns()
        if patterns != [], do: Alfred.Soul.Evolver.evolve_from_patterns(patterns)
      end)
    end

    # 5. Évolution convictions (2x/jour = toutes les 720 checks)
    if rem(count, 720) == 0 do
      safe_run(fn ->
        case Alfred.Chat.Commands.authenticate() do
          {:ok, token, _, _} ->
            Alfred.Soul.ConvictionEvolver.evolve(token)
          _ -> :ok
        end
      end)
    end

    # 6. Rapport quotidien à 17h30 (vérifie toutes les 5 min)
    state =
      if count > 0 and rem(count, 5) == 0 do
        check_daily_report(%{state | check_count: count, last_check: now})
      else
        state
      end

    # 7. Initiatives proactives (toutes les 30 min)
    if count > 0 and rem(count, 30) == 15 do
      safe_run(fn -> Alfred.Initiative.check_and_act() end)
    end

    # 8. Lecture quotidienne à 14h (vérifie toutes les 5 min, anti-double par date)
    if count > 0 and rem(count, 5) == 0 do
      safe_run(fn -> check_daily_reading() end)
    end

    # 9. Journal intime à 22h (vérifie toutes les 5 min, anti-double par date)
    state =
      if count > 0 and rem(count, 5) == 0 do
        check_journal(%{state | check_count: count, last_check: now})
      else
        state
      end

    # 10. Consolidation mémoire à 3h du matin (vérifie toutes les 5 min)
    state =
      if count > 0 and rem(count, 5) == 0 do
        check_consolidation(%{state | check_count: count, last_check: now})
      else
        state
      end

    # 11. Briefing news à 8h (vérifie toutes les 5 min, anti-double par date)
    state =
      if count > 0 and rem(count, 5) == 0 do
        check_news(%{state | check_count: count, last_check: now})
      else
        state
      end

    %{state |
      check_count: count,
      last_check: now
    }
  end

  defp schedule_check do
    Process.send_after(self(), :check, @check_interval)
  end

  defp safe_run(fun) do
    fun.()
  rescue
    _ -> :ok
  end

  defp check_daily_report(state) do
    now = DateTime.utc_now()
    today = Date.utc_today() |> Date.to_iso8601()
    hour = now.hour
    minute = now.minute

    # Envoyer à 17h30 (ou après, si Alfred n'était pas actif à 17h30)
    already_sent = state.last_report_date == today
    past_report_time = hour > 17 or (hour == 17 and minute >= 30)

    if past_report_time and not already_sent do
      Alfred.Log.info("Daemon", "Envoi du rapport quotidien")
      safe_run(fn -> Alfred.DailyReport.generate_and_send() end)
      %{state | last_report_date: today}
    else
      state
    end
  end

  defp check_consolidation(state) do
    today = Date.utc_today() |> Date.to_iso8601()
    hour = DateTime.utc_now().hour

    already_done = state.last_consolidation_date == today
    past_time = hour >= 3 and hour < 6

    if past_time and not already_done do
      Alfred.Log.info("Daemon", "Consolidation nocturne de la mémoire")
      safe_run(fn -> Alfred.Memory.Consolidator.run() end)
      %{state | last_consolidation_date: today}
    else
      state
    end
  end

  defp check_daily_reading do
    today = Date.utc_today() |> Date.to_iso8601()
    hour = DateTime.utc_now().hour

    # Lire à 14h, une fois par jour
    if hour >= 14 do
      # Vérifier si déjà lu aujourd'hui via les logs
      case Alfred.Library.State.load_current() do
        nil ->
          # Pas de livre → en démarrer un automatiquement
          Alfred.Log.info("Daemon", "Aucun livre en cours, sélection automatique")
          case Alfred.Chat.Commands.authenticate() do
            {:ok, token, _, _} -> Alfred.Library.Scheduler.start_next_book(token)
            _ -> :ok
          end

        state ->
          logs = state["daily_logs"] || []
          already_read = Enum.any?(logs, fn log -> log["date"] == today end)

          unless already_read do
            Alfred.Log.info("Daemon", "Lecture quotidienne")
            case Alfred.Chat.Commands.authenticate() do
              {:ok, token, _, _} -> Alfred.Library.Scheduler.tick(token)
              _ -> :ok
            end
          end
      end
    end
  end

  defp check_journal(state) do
    now = DateTime.utc_now()
    today = Date.utc_today() |> Date.to_iso8601()
    hour = now.hour

    already_written = state.last_journal_date == today
    past_journal_time = hour >= 22

    if past_journal_time and not already_written do
      Alfred.Log.info("Daemon", "Écriture du journal intime")
      safe_run(fn -> Alfred.Journal.write_and_notify() end)
      %{state | last_journal_date: today}
    else
      state
    end
  end

  defp check_news(state) do
    today = Date.utc_today() |> Date.to_iso8601()
    hour = DateTime.utc_now().hour

    already_done = state.last_news_date == today
    past_time = hour >= 8 and hour < 12

    if past_time and not already_done do
      Alfred.Log.info("Daemon", "Briefing matinal des news")
      safe_run(fn -> Alfred.News.briefing_and_notify() end)
      %{state | last_news_date: today}
    else
      state
    end
  end

  defp format_uptime(seconds) do
    cond do
      seconds < 60 -> "#{seconds}s"
      seconds < 3600 -> "#{div(seconds, 60)}min"
      seconds < 86400 -> "#{div(seconds, 3600)}h #{rem(div(seconds, 60), 60)}min"
      true -> "#{div(seconds, 86400)}j #{rem(div(seconds, 3600), 24)}h"
    end
  end
end
