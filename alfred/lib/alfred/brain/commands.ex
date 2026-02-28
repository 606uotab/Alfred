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
  Essaie d'abord smart_prioritize (avec historique), fallback sur prioritize.
  """
  def handle_prioritize(project_name) do
    unless Projects.exists?(project_name) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project_name}\" n'existe pas.")
    else
      project_data = build_project_data(project_name)
      episodes = collect_search_episodes() |> Enum.take(-50)
      patterns = collect_patterns()

      payload = %{
        cmd: "smart_prioritize",
        project: project_data,
        episodes: episodes,
        patterns: patterns,
        now: now()
      }

      case Brain.send_command(payload) do
        {:ok, resp} ->
          print_smart_prioritize(resp, project_name)

        {:error, _} ->
          # Fallback sur l'ancien prioritize
          case Brain.send_command(%{cmd: "prioritize", project: project_data, now: now()}) do
            {:ok, resp} ->
              print_legacy_prioritize(resp, project_name)

            {:error, msg} ->
              Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
          end
      end
    end
  end

  defp print_smart_prioritize(resp, project_name) do
    ranked = Map.get(resp, "ranked", [])
    velocity = Map.get(resp, "velocity", %{})
    insights = Map.get(resp, "insights", [])

    Butler.say("Monsieur, voici la priorisation intelligente pour \"#{project_name}\" :\n")

    Enum.each(ranked, fn item ->
      desc = Map.get(item, "description", "")
      prio = Map.get(item, "priority", 1)
      score = Map.get(item, "score", 0)
      effort = Map.get(item, "effort", "moyen")
      risk = Map.get(item, "risk", "normal")
      reason = Map.get(item, "reason", "")

      risk_icon = case risk do
        "élevé" -> "🔴"
        "normal" -> "🟡"
        _ -> "🟢"
      end

      IO.puts("  #{risk_icon} #{desc}")
      IO.puts("    P#{prio} · score #{score} · effort #{effort} · #{reason}")
    end)

    IO.puts("")

    if velocity != %{} do
      tasks_week = Map.get(velocity, "tasks_per_week", 0)
      if tasks_week > 0, do: IO.puts("  ⚡ Vélocité : #{tasks_week} tâches/semaine")
      IO.puts("")
    end

    if length(insights) > 0 do
      Enum.each(insights, fn i -> IO.puts("  💡 #{i}") end)
      IO.puts("")
    end
  end

  defp print_legacy_prioritize(resp, project_name) do
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
      Enum.each(insights, fn i -> IO.puts("  💡 #{i}") end)
      IO.puts("")
    end

    if length(actions) > 0 do
      IO.puts("  📌 Actions recommandées :")
      Enum.each(actions, fn a -> IO.puts("    #{a}") end)
      IO.puts("")
    end
  end

  @doc """
  Tendances temporelles — analyse les trends d'interaction, topics, mood.
  """
  def handle_trends do
    Butler.say("Analyse des tendances en cours...\n")

    episodes = collect_search_episodes()
    facts = collect_search_facts()
    activity = Alfred.Initiative.Smart.load_data()
    journal = collect_journal_entries()

    payload = %{
      cmd: "trends",
      episodes: episodes,
      facts: facts,
      activity_log: activity["interactions"] || [],
      journal: journal,
      now: now()
    }

    case Brain.send_command(payload) do
      {:ok, resp} ->
        print_trends(resp)

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
    end
  end

  defp print_trends(resp) do
    interaction = Map.get(resp, "interaction_trend", %{})
    topics = Map.get(resp, "topic_trends", [])
    mood = Map.get(resp, "mood_trajectory", %{})
    activity = Map.get(resp, "activity_patterns", %{})
    insights = Map.get(resp, "insights", [])

    # Interaction trend
    if interaction != %{} do
      direction = Map.get(interaction, "direction", "stable")
      icon = case direction do
        "hausse" -> "📈"
        "baisse" -> "📉"
        _ -> "➡️"
      end
      IO.puts("  #{icon} Interactions : #{direction}")
      recent = Map.get(interaction, "recent_7d", 0)
      previous = Map.get(interaction, "previous_7d", 0)
      IO.puts("    7 derniers jours : #{recent} (vs #{previous} avant)")
      IO.puts("")
    end

    # Topic trends
    if length(topics) > 0 do
      IO.puts("  📊 Tendances des sujets :")
      Enum.each(Enum.take(topics, 10), fn t ->
        name = Map.get(t, "topic", "?")
        trend = Map.get(t, "trend", "stable")
        icon = case trend do
          "rising" -> "🔺"
          "falling" -> "🔻"
          _ -> "▪️"
        end
        IO.puts("    #{icon} #{name} (#{trend})")
      end)
      IO.puts("")
    end

    # Mood trajectory
    if mood != %{} do
      current = Map.get(mood, "current", "neutre")
      trend = Map.get(mood, "trend", "stable")
      IO.puts("  🎭 Humeur : #{current} (#{trend})")
      IO.puts("")
    end

    # Activity patterns
    if activity != %{} do
      peak = Map.get(activity, "peak_hours", [])
      if peak != [] do
        IO.puts("  🕐 Heures de pointe : #{Enum.join(peak, ", ")}h")
        IO.puts("")
      end
    end

    if length(insights) > 0 do
      Enum.each(insights, fn i -> IO.puts("  💡 #{i}") end)
      IO.puts("")
    end
  end

  @doc """
  Clustering des conversations — regroupe les épisodes par thèmes.
  """
  def handle_cluster do
    Butler.say("Cartographie des conversations en cours...\n")

    episodes = collect_search_episodes()

    payload = %{
      cmd: "cluster",
      episodes: episodes,
      now: now()
    }

    case Brain.send_command(payload) do
      {:ok, resp} ->
        print_clusters(resp)

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
    end
  end

  defp print_clusters(resp) do
    clusters = Map.get(resp, "clusters", [])
    insights = Map.get(resp, "insights", [])

    if clusters == [] do
      Butler.say("Pas assez de données pour dégager des groupes thématiques, Monsieur.")
    else
      IO.puts("  Groupes thématiques détectés :\n")

      Enum.each(clusters, fn c ->
        label = Map.get(c, "label", "Groupe")
        count = Map.get(c, "episode_count", 0)
        topics = Map.get(c, "top_topics", [])

        IO.puts("  🗂️  #{label} (#{count} conversations)")
        if topics != [] do
          IO.puts("      Thèmes : #{Enum.join(topics, ", ")}")
        end
      end)

      IO.puts("")
    end

    if length(insights) > 0 do
      Enum.each(insights, fn i -> IO.puts("  💡 #{i}") end)
      IO.puts("")
    end
  end

  @doc """
  Recommandations personnalisées — suggestions de topics, culture, lectures.
  """
  def handle_recommend do
    Butler.say("Analyse de votre profil en cours...\n")

    episodes = collect_search_episodes()
    facts = collect_search_facts()
    journal = collect_journal_entries()
    patterns = collect_patterns()
    library = collect_library_history()

    payload = %{
      cmd: "recommend",
      episodes: episodes,
      facts: facts,
      culture: [],
      library: library,
      journal: journal,
      patterns: patterns,
      now: now()
    }

    case Brain.send_command(payload) do
      {:ok, resp} ->
        print_recommendations(resp)

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
    end
  end

  defp print_recommendations(resp) do
    topics = Map.get(resp, "topics_to_explore", [])
    gaps = Map.get(resp, "culture_gaps", [])
    books = Map.get(resp, "book_suggestions", [])
    connections = Map.get(resp, "connections", [])
    insights = Map.get(resp, "insights", [])

    if topics != [] do
      IO.puts("  🔍 Sujets à explorer :")
      Enum.each(topics, fn t -> IO.puts("    • #{t}") end)
      IO.puts("")
    end

    if gaps != [] do
      IO.puts("  📚 Lacunes culturelles :")
      Enum.each(gaps, fn g -> IO.puts("    • #{g}") end)
      IO.puts("")
    end

    if books != [] do
      IO.puts("  📖 Suggestions de lecture :")
      Enum.each(books, fn b -> IO.puts("    • #{b}") end)
      IO.puts("")
    end

    if connections != [] do
      IO.puts("  🔗 Connexions intéressantes :")
      Enum.each(connections, fn c -> IO.puts("    • #{c}") end)
      IO.puts("")
    end

    if length(insights) > 0 do
      Enum.each(insights, fn i -> IO.puts("  💡 #{i}") end)
      IO.puts("")
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

  # -- Data collectors for new features --

  defp collect_journal_entries do
    dir = Path.join([System.user_home!(), ".alfred", "data", "journal"])

    case File.ls(dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.sort(:desc)
        |> Enum.take(30)
        |> Enum.map(fn f ->
          case Alfred.Storage.Local.read(Path.join("journal", f)) do
            data when is_map(data) -> data
            _ -> nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      _ ->
        []
    end
  end

  defp collect_library_history do
    case Alfred.Storage.Local.read("library/history.json") do
      data when is_list(data) -> data
      data when is_map(data) -> Map.get(data, "books", [])
      _ -> []
    end
  end

  defp now, do: System.system_time(:second)
end
