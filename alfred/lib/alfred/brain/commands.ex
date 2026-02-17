defmodule Alfred.Brain.Commands do
  @moduledoc """
  Commandes d'analyse — Le Cerveau de Alfred réfléchit.
  Utilise le moteur Julia pour l'analyse intelligente.
  """

  alias Alfred.Butler
  alias Alfred.Brain.Port, as: Brain
  alias Alfred.Projects.Manager, as: Projects
  alias Alfred.Projects.Task, as: Tasks
  alias Alfred.Projects.Note, as: Notes
  alias Alfred.Memory.{Episodic, Procedural}

  @doc """
  Analyse approfondie d'un projet.
  """
  def handle_analyze(project_name) do
    unless Projects.exists?(project_name) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project_name}\" n'existe pas.")
    else
      project_data = build_project_data(project_name)

      case Brain.send_command(%{cmd: "analyze", project: project_data, now: now()}) do
        {:ok, resp} ->
          insights = Map.get(resp, "insights", [])
          stats = Map.get(resp, "stats", %{})

          Butler.say("Monsieur, voici mon analyse du projet \"#{project_name}\" :\n")

          Enum.each(insights, fn insight ->
            IO.puts("  💡 #{insight}")
          end)

          IO.puts("")
          print_stats(stats)

        {:error, msg} ->
          Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
      end
    end
  end

  @doc """
  Résumé concis d'un projet.
  """
  def handle_summarize(project_name) do
    unless Projects.exists?(project_name) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project_name}\" n'existe pas.")
    else
      project_data = build_project_data(project_name)

      case Brain.send_command(%{cmd: "summarize", project: project_data}) do
        {:ok, resp} ->
          summary = Map.get(resp, "summary", "")
          Butler.say("Monsieur, voici le résumé :\n")
          IO.puts("  #{String.replace(summary, "\n", "\n  ")}")
          IO.puts("")

        {:error, msg} ->
          Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
      end
    end
  end

  @doc """
  Suggestions transversales sur tous les projets.
  """
  def handle_suggest do
    projects = Projects.list()

    projects_data =
      Enum.map(projects, fn p ->
        build_project_data(p.name)
      end)

    case Brain.send_command(%{cmd: "suggest", projects: projects_data, now: now()}) do
      {:ok, resp} ->
        suggestions = Map.get(resp, "suggestions", [])
        Butler.say("Monsieur, permettez-moi quelques suggestions :\n")

        Enum.each(suggestions, fn s ->
          IO.puts("  🧠 #{s}")
        end)

        IO.puts("")

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
    end
  end

  @doc """
  Synthèse quotidienne — croise projets, rappels, culture, patterns, conversations.
  """
  def handle_briefing do
    Butler.say("Votre briefing du jour, Monsieur :\n")

    # Collect all data
    projects_data = collect_all_projects()
    reminders_data = collect_all_reminders()
    culture_data = collect_culture()
    patterns_data = collect_patterns()
    last_episode = collect_last_episode()

    payload = %{
      cmd: "briefing",
      projects: projects_data,
      reminders: reminders_data,
      culture: culture_data,
      patterns: patterns_data,
      last_episode: last_episode,
      now: now()
    }

    case Brain.send_command(payload) do
      {:ok, resp} ->
        sections = Map.get(resp, "sections", [])
        conclusion = Map.get(resp, "conclusion", "")

        Enum.each(sections, fn section ->
          icon = Map.get(section, "icon", "•")
          title = Map.get(section, "title", "")
          lines = Map.get(section, "lines", [])

          IO.puts("  #{icon} #{title}")
          IO.puts("  #{String.duplicate("─", String.length(title) + 2)}")

          Enum.each(lines, fn line ->
            IO.puts("  #{line}")
          end)

          IO.puts("")
        end)

        if conclusion != "" do
          Butler.say(conclusion)
        end

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
    end
  end

  # -- Data collectors for briefing --

  defp collect_all_projects do
    Projects.list()
    |> Enum.map(fn p -> build_project_data(p.name) end)
  end

  defp collect_all_reminders do
    case :alfred_scheduler.list_reminders() do
      {:ok, reminders} ->
        Enum.map(reminders, fn r ->
          %{
            "project" => r.project,
            "text" => r.text,
            "status" => Atom.to_string(r.status),
            "due_at" => r.due_at
          }
        end)

      _ ->
        []
    end
  end

  defp collect_culture do
    # Culture is in the vault — we can't read it without a password in non-interactive mode.
    # Return empty; briefing will still work with other data.
    # In interactive mode (chat), culture is passed separately.
    []
  end

  defp collect_patterns do
    Procedural.active_patterns()
    |> Enum.map(fn p ->
      %{
        "description" => p["description"],
        "pattern_type" => p["pattern_type"],
        "confidence" => p["confidence"]
      }
    end)
  end

  defp collect_last_episode do
    case Episodic.list_episodes() do
      [latest | _] ->
        %{
          "summary" => latest["summary"],
          "message_count" => latest["message_count"],
          "mode" => latest["mode"]
        }

      _ ->
        nil
    end
  end

  # -- Helpers --

  defp build_project_data(project_name) do
    tasks = Tasks.list_for_project(project_name)
    notes = Notes.list_for_project(project_name)

    tasks_data =
      Enum.map(tasks, fn t ->
        %{
          "description" => t.description,
          "status" => t.status,
          "priority" => t.priority,
          "created_at" => t.created_at
        }
      end)

    notes_data =
      Enum.map(notes, fn n ->
        %{
          "text" => n.text,
          "created_at" => n.created_at
        }
      end)

    # Get reminders for this project
    reminders_data =
      case :alfred_scheduler.list_reminders() do
        {:ok, reminders} ->
          reminders
          |> Enum.filter(&(&1.project == project_name))
          |> Enum.map(fn r ->
            %{
              "status" => Atom.to_string(r.status),
              "due_at" => r.due_at,
              "text" => r.text
            }
          end)

        _ ->
          []
      end

    %{
      "name" => project_name,
      "tasks" => tasks_data,
      "notes" => notes_data,
      "reminders" => reminders_data
    }
  end

  defp print_stats(stats) when map_size(stats) > 0 do
    IO.puts("  Statistiques :")

    if Map.has_key?(stats, "completion_rate") do
      IO.puts("    Accomplissement : #{stats["completion_rate"]}%")
    end

    if Map.has_key?(stats, "avg_priority") do
      IO.puts("    Priorité moyenne : #{stats["avg_priority"]}")
    end

    if Map.has_key?(stats, "oldest_task_days") do
      IO.puts("    Tâche la plus ancienne : #{stats["oldest_task_days"]} jours")
    end

    if Map.has_key?(stats, "keywords") do
      IO.puts("    Mots-clés : #{Enum.join(stats["keywords"], ", ")}")
    end

    IO.puts("")
  end

  defp print_stats(_), do: :ok

  defp now, do: System.system_time(:second)
end
