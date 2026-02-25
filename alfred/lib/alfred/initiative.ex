defmodule Alfred.Initiative do
  @moduledoc """
  Initiatives proactives — Alfred agit de lui-même.
  Vérifié toutes les 30 minutes par le daemon.
  Anti-spam : une notification par type par jour max.
  """

  @state_file "initiative_state.json"

  @doc "Vérifie les conditions et envoie des notifications proactives."
  def check_and_act do
    cooldowns = load_cooldowns()
    today = Date.utc_today() |> Date.to_iso8601()

    cooldowns
    |> check_upcoming_reminders(today)
    |> check_stale_tasks(today)
    |> check_unread_library(today)
    |> check_pending_learn(today)
    |> check_brain_suggestions(today)
    |> check_cortex_trends(today)
    |> save_cooldowns()
  end

  # -- Rappels bientôt dus (dans les 30 min) --

  defp check_upcoming_reminders(cooldowns, today) do
    if already_notified?(cooldowns, "reminders", today), do: cooldowns, else: do_check_reminders(cooldowns, today)
  end

  defp do_check_reminders(cooldowns, today) do
    case :alfred_scheduler.list_reminders() do
      {:ok, reminders} when reminders != [] ->
        now = :erlang.system_time(:second)

        upcoming =
          Enum.filter(reminders, fn r ->
            r[:status] == :pending and
              r[:due_at] != nil and
              r[:due_at] - now > 0 and
              r[:due_at] - now <= 1800
          end)

        if upcoming != [] do
          names = Enum.map(upcoming, fn r -> r[:text] end) |> Enum.join(", ")
          count = length(upcoming)
          msg = "Monsieur, #{count} rappel(s) dans les 30 prochaines minutes : #{names}"
          notify(msg)
          mark_notified(cooldowns, "reminders", today)
        else
          cooldowns
        end

      _ ->
        cooldowns
    end
  rescue
    _ -> cooldowns
  end

  # -- Tâches haute priorité en retard --

  defp check_stale_tasks(cooldowns, today) do
    if already_notified?(cooldowns, "stale_tasks", today), do: cooldowns, else: do_check_stale_tasks(cooldowns, today)
  end

  defp do_check_stale_tasks(cooldowns, today) do
    projects = Alfred.Projects.Manager.list()

    stale =
      Enum.flat_map(projects, fn p ->
        name = p["name"] || p[:name]
        tasks = Alfred.Projects.Task.list_for_project(name)

        Enum.filter(tasks, fn t ->
          t.priority == "high" and t.status != "done" and stale_days(t) >= 3
        end)
        |> Enum.map(fn t -> {name, t} end)
      end)

    if stale != [] do
      lines =
        Enum.map(stale, fn {project, task} ->
          days = stale_days(task)
          "  - #{task.description} (#{project}) — #{days} jours"
        end)

      msg = "Monsieur, certaines tâches prioritaires attendent depuis longtemps :\n" <> Enum.join(lines, "\n")
      notify(msg)
      mark_notified(cooldowns, "stale_tasks", today)
    else
      cooldowns
    end
  rescue
    _ -> cooldowns
  end

  # -- Lecture du jour pas encore faite (après 14h) --

  defp check_unread_library(cooldowns, today) do
    if already_notified?(cooldowns, "library", today), do: cooldowns, else: do_check_library(cooldowns, today)
  end

  defp do_check_library(cooldowns, today) do
    hour = DateTime.utc_now().hour

    if hour >= 14 do
      case Alfred.Library.State.load_current() do
        nil ->
          cooldowns

        state ->
          logs = state["daily_logs"] || []
          today_read = Enum.any?(logs, fn log -> log["date"] == today end)

          unless today_read do
            book = state["current_book"]
            msg = "Je n'ai pas encore lu aujourd'hui. Lecture en cours : \"#{book["title"]}\"."
            notify(msg)
            mark_notified(cooldowns, "library", today)
          else
            cooldowns
          end
      end
    else
      cooldowns
    end
  rescue
    _ -> cooldowns
  end

  # -- Beaucoup de messages sans apprentissage --

  defp check_pending_learn(cooldowns, today) do
    if already_notified?(cooldowns, "learn", today), do: cooldowns, else: do_check_learn(cooldowns, today)
  end

  defp do_check_learn(cooldowns, today) do
    case Alfred.Simplex.Bridge.status() do
      :not_running ->
        cooldowns

      info ->
        if info.message_count > 0 and rem(info.message_count, 20) == 0 do
          msg = "#{info.message_count} messages traités aujourd'hui. Je continue d'apprendre."
          notify(msg)
          mark_notified(cooldowns, "learn", today)
        else
          cooldowns
        end
    end
  rescue
    _ -> cooldowns
  end

  # -- Suggestions du cerveau (Julia, 1x/jour) --

  defp check_brain_suggestions(cooldowns, today) do
    if already_notified?(cooldowns, "brain", today), do: cooldowns, else: do_check_brain(cooldowns, today)
  end

  defp do_check_brain(cooldowns, today) do
    projects = Alfred.ProjectData.all_for_startup()

    case Alfred.Brain.Port.send_command(%{
           cmd: "suggest",
           projects: projects,
           now: DateTime.utc_now() |> DateTime.to_iso8601()
         }) do
      {:ok, %{"suggestions" => suggestions}} when suggestions != [] ->
        text =
          "Suggestions du cerveau :\n" <>
            Enum.map_join(Enum.take(suggestions, 3), "\n", &"  - #{&1}")

        notify(text)
        mark_notified(cooldowns, "brain", today)

      _ ->
        cooldowns
    end
  rescue
    _ -> cooldowns
  end

  # -- Tendances cortex (R, 1x/jour) --

  defp check_cortex_trends(cooldowns, today) do
    if already_notified?(cooldowns, "cortex", today), do: cooldowns, else: do_check_cortex(cooldowns, today)
  end

  defp do_check_cortex(cooldowns, today) do
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
        summary = resp["summary"]

        if summary do
          notify("Cortex : #{summary}")
          mark_notified(cooldowns, "cortex", today)
        else
          cooldowns
        end

      _ ->
        cooldowns
    end
  rescue
    _ -> cooldowns
  end

  # -- Cooldown management --

  defp already_notified?(cooldowns, type, today) do
    cooldowns[type] == today
  end

  defp mark_notified(cooldowns, type, today) do
    Map.put(cooldowns, type, today)
  end

  defp load_cooldowns do
    case Alfred.Storage.Local.read(@state_file) do
      data when is_map(data) -> data
      _ -> %{}
    end
  end

  defp save_cooldowns(cooldowns) do
    Alfred.Storage.Local.write(@state_file, cooldowns)
    cooldowns
  end

  # -- Helpers --

  defp notify(text) do
    IO.puts("[Initiative] #{text}")
    Alfred.Simplex.Bridge.send_group_notification(text)
  end

  defp stale_days(task) do
    created = task.created_at || task[:created_at]

    case created do
      nil -> 0
      date_str when is_binary(date_str) ->
        case Date.from_iso8601(String.slice(date_str, 0, 10)) do
          {:ok, date} -> Date.diff(Date.utc_today(), date)
          _ -> 0
        end
      _ -> 0
    end
  end
end
