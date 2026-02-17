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

  @doc """
  Analyse intelligente de la culture — croise sujets, sources, tags, temporalité.
  Nécessite le mot de passe pour lire le vault culture.
  """
  def handle_analyze_culture do
    password = Alfred.Input.prompt_password("Mot de passe (maître ou admin) : ")

    case Alfred.Culture.list_all(password) do
      {:ok, entries} ->
        Butler.say("Analyse de votre culture en cours...\n")

        culture_data =
          Enum.map(entries, fn e ->
            %{
              "topic" => e["topic"],
              "content" => e["content"],
              "source" => e["source"],
              "tags" => e["tags"] || [],
              "learned_at" => e["learned_at"]
            }
          end)

        case Brain.send_command(%{cmd: "analyze_culture", culture: culture_data, now: now()}) do
          {:ok, resp} ->
            print_culture_analysis(resp)

          {:error, msg} ->
            Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
        end

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Impossible d'accéder à la culture : #{msg}")
    end
  end

  @doc """
  Analyse culture avec données pré-chargées (pour tests ou mode chat).
  """
  def handle_analyze_culture(culture_data) when is_list(culture_data) do
    case Brain.send_command(%{cmd: "analyze_culture", culture: culture_data, now: now()}) do
      {:ok, resp} ->
        print_culture_analysis(resp)

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
    end
  end

  defp print_culture_analysis(resp) do
    insights = Map.get(resp, "insights", [])
    topics = Map.get(resp, "topics", [])
    suggestions = Map.get(resp, "suggestions", [])
    stats = Map.get(resp, "stats", %{})

    Butler.say("Monsieur, voici mon analyse de votre culture :\n")

    # Insights
    Enum.each(insights, fn insight ->
      IO.puts("  💡 #{insight}")
    end)

    IO.puts("")

    # Topics detail
    if length(topics) > 0 do
      IO.puts("  📚 Sujets :")
      IO.puts("  #{String.duplicate("─", 40)}")

      Enum.each(topics, fn topic ->
        name = Map.get(topic, "name", "?")
        count = Map.get(topic, "count", 0)
        IO.puts("    #{name} (#{count})")

        # Sources
        sources = Map.get(topic, "sources", %{})

        if map_size(sources) > 0 do
          src_str =
            sources
            |> Enum.map(fn {name, count} -> "#{name} (#{count})" end)
            |> Enum.join(", ")

          IO.puts("      Sources : #{src_str}")
        end

        # Tags
        tags = Map.get(topic, "top_tags", [])

        if length(tags) > 0 do
          IO.puts("      Tags : #{Enum.join(tags, ", ")}")
        end
      end)

      IO.puts("")
    end

    # Connections
    connections = Map.get(stats, "connections", [])

    if length(connections) > 0 do
      IO.puts("  🔗 Connexions :")

      Enum.each(connections, fn c ->
        IO.puts("    #{c}")
      end)

      IO.puts("")
    end

    # Suggestions
    if length(suggestions) > 0 do
      IO.puts("  🧠 Suggestions :")

      Enum.each(suggestions, fn s ->
        IO.puts("    #{s}")
      end)

      IO.puts("")
    end
  end

  @doc """
  Priorisation intelligente des tâches d'un projet.
  """
  def handle_prioritize(project_name) do
    unless Projects.exists?(project_name) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project_name}\" n'existe pas.")
    else
      project_data = build_project_data(project_name)

      case Brain.send_command(%{cmd: "prioritize", project: project_data, now: now()}) do
        {:ok, resp} ->
          ranked = Map.get(resp, "ranked", [])
          insights = Map.get(resp, "insights", [])
          actions = Map.get(resp, "actions", [])

          Butler.say("Monsieur, voici l'ordre de priorité recommandé pour \"#{project_name}\" :\n")

          Enum.each(ranked, fn item ->
            rank = Map.get(item, "rank", 0)
            desc = Map.get(item, "description", "")
            prio = Map.get(item, "priority", 1)
            score = Map.get(item, "score", 0)
            age = Map.get(item, "age_days", 0)

            age_str = if age > 0, do: " · #{trunc(age)}j", else: ""
            IO.puts("  #{rank}. #{desc}  [P#{prio}, score #{score}#{age_str}]")
          end)

          IO.puts("")

          if length(insights) > 0 do
            Enum.each(insights, fn i ->
              IO.puts("  💡 #{i}")
            end)

            IO.puts("")
          end

          if length(actions) > 0 do
            IO.puts("  📌 Actions recommandées :")

            Enum.each(actions, fn a ->
              IO.puts("    #{a}")
            end)

            IO.puts("")
          end

        {:error, msg} ->
          Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
      end
    end
  end

  @doc """
  Recherche universelle — cherche dans projets, tâches, notes, mémoire, rappels.
  """
  def handle_search(query) do
    Butler.say("Recherche de \"#{query}\" en cours...\n")

    # Collect all searchable data
    projects_data = collect_search_projects()
    tasks_data = collect_search_tasks()
    notes_data = collect_search_notes()
    facts_data = collect_search_facts()
    episodes_data = collect_search_episodes()
    reminders_data = collect_all_reminders()

    payload = %{
      cmd: "search",
      query: query,
      projects: projects_data,
      tasks: tasks_data,
      notes: notes_data,
      facts: facts_data,
      episodes: episodes_data,
      reminders: reminders_data,
      culture: []
    }

    case Brain.send_command(payload) do
      {:ok, resp} ->
        print_search_results(resp, query)

      {:error, msg} ->
        Butler.say("Erreur de recherche : #{msg}")
    end
  end

  defp collect_search_projects do
    Projects.list()
    |> Enum.map(fn p -> %{"name" => p.name} end)
  end

  defp collect_search_tasks do
    Tasks.list_all()
    |> Enum.map(fn t ->
      %{
        "description" => t.description,
        "project" => t.project,
        "status" => t.status,
        "priority" => t.priority
      }
    end)
  end

  defp collect_search_notes do
    Notes.list_all()
    |> Enum.map(fn n ->
      %{"text" => n.text, "project" => n.project}
    end)
  end

  defp collect_search_facts do
    Alfred.Memory.Semantic.all_facts()
  end

  defp collect_search_episodes do
    Episodic.list_episodes()
  end

  defp print_search_results(resp, query) do
    results = Map.get(resp, "results", [])
    total = Map.get(resp, "total", 0)
    by_type = Map.get(resp, "by_type", %{})

    if total == 0 do
      Butler.say("Aucun résultat pour \"#{query}\", Monsieur.")
    else
      shown = length(results)
      Butler.say("#{total} résultat(s) pour \"#{query}\" :\n")

      # Type labels and icons
      type_info = %{
        "project" => {"📁", "Projet"},
        "task" => {"✅", "Tâche"},
        "note" => {"📝", "Note"},
        "fact" => {"🧠", "Mémoire"},
        "episode" => {"💬", "Conversation"},
        "reminder" => {"🔔", "Rappel"},
        "culture" => {"📚", "Culture"}
      }

      # Print results grouped by type
      results
      |> Enum.group_by(& &1["type"])
      |> Enum.each(fn {type, items} ->
        {icon, label} = Map.get(type_info, type, {"•", type})
        IO.puts("  #{icon} #{label} (#{length(items)})")
        IO.puts("  #{String.duplicate("─", 30)}")

        Enum.each(items, fn item ->
          title = Map.get(item, "title", "")
          excerpt = Map.get(item, "excerpt", "")
          IO.puts("    #{title}")
          if excerpt != "" do
            IO.puts("      #{excerpt}")
          end
        end)

        IO.puts("")
      end)

      if total > shown do
        IO.puts("  ... et #{total - shown} autres résultats.\n")
      end

      # Summary by type
      if map_size(by_type) > 1 do
        parts =
          by_type
          |> Enum.map(fn {type, count} ->
            {_, label} = Map.get(type_info, type, {"", type})
            "#{count} #{String.downcase(label)}(s)"
          end)
          |> Enum.join(", ")

        IO.puts("  Répartition : #{parts}\n")
      end
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
