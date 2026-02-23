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
      reminders_notified: 0
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

    # 6. Lecture hebdomadaire (1x/jour = 1440 checks, offset 60 pour ~1h après démarrage)
    if count > 0 and rem(count, 1440) == 60 do
      safe_run(fn ->
        case Alfred.Chat.Commands.authenticate() do
          {:ok, token, _, _} ->
            Alfred.Library.Scheduler.tick(token)
          _ -> :ok
        end
      end)
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

  defp format_uptime(seconds) do
    cond do
      seconds < 60 -> "#{seconds}s"
      seconds < 3600 -> "#{div(seconds, 60)}min"
      seconds < 86400 -> "#{div(seconds, 3600)}h #{rem(div(seconds, 60), 60)}min"
      true -> "#{div(seconds, 86400)}j #{rem(div(seconds, 3600), 24)}h"
    end
  end
end
