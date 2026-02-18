defmodule Alfred.Maintenance do
  @moduledoc """
  Maintenance autonome — Alfred vérifie et agit au démarrage.
  Garde un journal des dernières exécutions dans maintenance.json.
  """

  alias Alfred.Colors

  @maintenance_file "maintenance.json"
  @backup_interval_days 7
  @consolidation_interval_hours 24

  @doc """
  Exécute les tâches de maintenance silencieusement au démarrage.
  N'affiche que les alertes et actions effectuées.
  """
  def run_startup_checks do
    state = load_state()
    now = System.system_time(:second)

    state = maybe_auto_backup(state, now)
    state = maybe_consolidate_memory(state, now)
    check_disk_space()

    save_state(state)
  rescue
    _ -> :ok
  end

  # -- Auto-backup (hebdomadaire) --

  defp maybe_auto_backup(state, now) do
    last_backup = state["last_backup"] || 0
    days_since = (now - last_backup) / 86400

    if days_since >= @backup_interval_days do
      arms_info = :alfred_health.check_arms()

      if arms_info.binary_found do
        data_dir = Path.expand("~/.alfred")

        case Alfred.Arms.Port.send_command(%{cmd: "backup", data_dir: data_dir}) do
          {:ok, %{"backup" => backup}} ->
            size = format_bytes(backup["size_bytes"] || 0)
            IO.puts("  #{Colors.green("✓")} Sauvegarde automatique (#{size})")
            Map.put(state, "last_backup", now)

          _ ->
            state
        end
      else
        state
      end
    else
      state
    end
  rescue
    _ -> state
  end

  # -- Consolidation mémoire (quotidienne) --

  defp maybe_consolidate_memory(state, now) do
    last_consolidation = state["last_consolidation"] || 0
    hours_since = (now - last_consolidation) / 3600

    if hours_since >= @consolidation_interval_hours do
      cortex_info = :alfred_health.check_cortex()

      if cortex_info.r_found and cortex_info.script_found do
        episodes = Alfred.Memory.Episodic.list_episodes()
        facts = Alfred.Memory.Semantic.all_facts()

        if length(episodes) > 0 or length(facts) > 0 do
          # Run behavioral analysis for consolidation
          case Alfred.Cortex.Port.send_command(%{
                 cmd: "behavioral_analysis",
                 episodes: episodes,
                 facts: facts
               }) do
            {:ok, %{"insights" => insights}} when insights != [] ->
              IO.puts("  #{Colors.green("✓")} Consolidation mémoire (#{length(insights)} insight(s))")

            _ ->
              :ok
          end
        end
      end

      Map.put(state, "last_consolidation", now)
    else
      state
    end
  rescue
    _ -> state
  end

  # -- Alerte espace disque --

  defp check_disk_space do
    arms_info = :alfred_health.check_arms()

    if arms_info.binary_found do
      case Alfred.Arms.Port.send_command(%{cmd: "disk_usage"}) do
        {:ok, %{"alert" => true, "partitions" => parts}} ->
          critical = Enum.filter(parts, fn p -> p["percent_used"] >= 90 end)

          Enum.each(critical, fn p ->
            avail_gb = Float.round(p["available_mb"] / 1024, 1)

            IO.puts(
              "  #{Colors.red("!")} Disque #{p["mount"]} : #{avail_gb} Go restants (#{p["percent_used"]}%)"
            )
          end)

        _ ->
          :ok
      end
    end
  rescue
    _ -> :ok
  end

  # -- Persistence --

  defp state_path do
    Path.join([System.user_home!(), ".alfred", "data", @maintenance_file])
  end

  defp load_state do
    case File.read(state_path()) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, state} when is_map(state) -> state
          _ -> %{}
        end

      _ ->
        %{}
    end
  end

  defp save_state(state) do
    File.write(state_path(), Jason.encode!(state, pretty: true))
  rescue
    _ -> :ok
  end

  defp format_bytes(bytes) when is_integer(bytes) and bytes >= 1024 do
    kb = Float.round(bytes / 1024, 1)
    "#{kb} Ko"
  end

  defp format_bytes(bytes) when is_integer(bytes), do: "#{bytes} o"
  defp format_bytes(_), do: "?"
end
