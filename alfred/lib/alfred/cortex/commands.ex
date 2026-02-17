defmodule Alfred.Cortex.Commands do
  @moduledoc """
  Commandes du cortex — Alfred analyse ses tendances à long terme.
  Utilise le moteur R pour l'analyse statistique.
  """

  alias Alfred.Butler
  alias Alfred.Cortex.Port, as: Cortex
  alias Alfred.Memory.{Episodic, Semantic}
  alias Alfred.Projects.{Manager, Task, Note}

  def handle(["trends"]) do
    episodes = Episodic.list_episodes()

    case Cortex.send_command(%{cmd: "interaction_trends", episodes: episodes}) do
      {:ok, resp} ->
        trends = resp["trends"]
        Butler.say("Monsieur, voici les tendances de nos interactions :\n")

        IO.puts("  Conversations  : #{trends["total_conversations"]}")
        IO.puts("  Messages total : #{trends["total_messages"]}")
        IO.puts("  Moyenne/session: #{trends["avg_messages_per_session"]}")

        if is_map(trends["top_topics"]) && map_size(trends["top_topics"]) > 0 do
          topics =
            trends["top_topics"]
            |> Enum.sort_by(fn {_k, v} -> -v end)
            |> Enum.map(fn {k, v} -> "#{k} (#{v})" end)
            |> Enum.join(", ")

          IO.puts("  Sujets favoris : #{topics}")
        end

        IO.puts("")

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, le cortex rencontre une difficulté : #{msg}")
    end
  end

  def handle(["stats"]) do
    facts = Semantic.all_facts()

    case Cortex.send_command(%{cmd: "memory_stats", facts: facts}) do
      {:ok, resp} ->
        stats = resp["stats"]
        Butler.say("Monsieur, voici l'état de ma mémoire :\n")

        IO.puts("  Faits mémorisés    : #{stats["total_facts"]}")
        IO.puts("  Confiance moyenne  : #{round(stats["avg_confidence"] * 100)}%")
        IO.puts("  Haute confiance    : #{stats["high_confidence_count"]}")
        IO.puts("  Jamais consultés   : #{stats["never_accessed"]}")

        if is_map(stats["by_category"]) do
          IO.puts("\n  Par catégorie :")

          Enum.each(stats["by_category"], fn {cat, count} ->
            IO.puts("    #{cat}: #{count}")
          end)
        end

        IO.puts("")

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, le cortex rencontre une difficulté : #{msg}")
    end
  end

  def handle(["analyze"]) do
    episodes = Episodic.list_episodes()
    facts = Semantic.all_facts()

    case Cortex.send_command(%{cmd: "behavioral_analysis", episodes: episodes, facts: facts}) do
      {:ok, resp} ->
        insights = resp["insights"]
        Butler.say("Monsieur, voici mon analyse comportementale :\n")

        Enum.each(insights, fn insight ->
          IO.puts("  📊 #{insight}")
        end)

        IO.puts("")

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, le cortex rencontre une difficulté : #{msg}")
    end
  end

  def handle(["productivity"]) do
    projects_data = collect_projects_with_reminders()

    case Cortex.send_command(%{cmd: "productivity_stats", projects: projects_data}) do
      {:ok, resp} ->
        stats = resp["stats"]
        Butler.say("Monsieur, voici vos statistiques de productivité :\n")

        IO.puts("  Accomplissement global : #{stats["overall_completion"]}%")
        IO.puts("  Tâches accomplies      : #{stats["total_done"]}")
        IO.puts("  Tâches en attente      : #{stats["total_pending"]}")
        IO.puts("  Rappels en retard      : #{stats["overdue_count"]}")

        health = stats["project_health"]

        if is_list(health) and health != [] do
          IO.puts("\n  Par projet :")

          Enum.each(health, fn p ->
            icon = if p["health_score"] >= 70, do: "✓", else: "▸"
            IO.puts("    #{icon} #{p["name"]} — #{p["completion"]}% (#{p["pending"]} en attente)")
          end)
        end

        IO.puts("")

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, le cortex rencontre une difficulté : #{msg}")
    end
  end

  def handle(["culture"]) do
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    case Alfred.Culture.list_all(password) do
      {:ok, entries} ->
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

        case Cortex.send_command(%{cmd: "culture_trends", culture: culture_data}) do
          {:ok, resp} ->
            trends = resp["trends"]
            Butler.say("Monsieur, voici les tendances de ma culture :\n")

            IO.puts("  Connaissances    : #{trends["total"]}")
            IO.puts("  Sujets           : #{trends["topic_count"]}")
            IO.puts("  Sources          : #{trends["source_count"]}")
            IO.puts("  Vélocité         : #{trends["velocity_per_week"]}/semaine")
            IO.puts("  Tendance         : #{format_trend(trends["growth_trend"])}")

            if is_map(trends["domain_distribution"]) do
              IO.puts("\n  Répartition :")

              trends["domain_distribution"]
              |> Enum.sort_by(fn {_k, v} -> -v end)
              |> Enum.each(fn {topic, pct} ->
                IO.puts("    #{topic} : #{pct}%")
              end)
            end

            IO.puts("")

          {:error, msg} ->
            Butler.say("Je suis navré Monsieur, le cortex rencontre une difficulté : #{msg}")
        end

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["correlations"]) do
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    episodes = Episodic.list_episodes()
    projects_data = collect_projects_with_notes()

    culture =
      case Alfred.Culture.list_all(password) do
        {:ok, entries} ->
          Enum.map(entries, fn e ->
            %{"topic" => e["topic"], "content" => e["content"], "tags" => e["tags"] || []}
          end)

        _ ->
          []
      end

    case Cortex.send_command(%{
           cmd: "correlation_analysis",
           episodes: episodes,
           projects: projects_data,
           culture: culture
         }) do
      {:ok, resp} ->
        correlations = resp["correlations"] || []
        insights = resp["insights"] || []

        Butler.say("Monsieur, voici l'analyse croisée de vos organes :\n")

        if correlations != [] do
          IO.puts("  Corrélations :")

          Enum.each(correlations, fn c ->
            IO.puts("    🔗 #{c}")
          end)

          IO.puts("")
        end

        Enum.each(insights, fn i ->
          IO.puts("  📊 #{i}")
        end)

        IO.puts("")

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, le cortex rencontre une difficulté : #{msg}")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes du cortex sont :\n")

    IO.puts("""
      alfred cortex trends                Tendances des interactions
      alfred cortex stats                 Statistiques de mémoire
      alfred cortex analyze               Analyse comportementale
      alfred cortex productivity           Statistiques de productivité
      alfred cortex culture                Tendances culturelles
      alfred cortex correlations           Analyse croisée des organes
    """)
  end

  # -- Helpers --

  defp collect_projects_with_reminders do
    Manager.list()
    |> Enum.map(fn p ->
      tasks = Task.list_for_project(p.name)

      reminders =
        case :alfred_scheduler.list_reminders() do
          {:ok, rs} ->
            rs
            |> Enum.filter(&(&1.project == p.name))
            |> Enum.map(fn r ->
              %{"status" => Atom.to_string(r.status), "due_at" => r.due_at, "text" => r.text}
            end)

          _ ->
            []
        end

      %{
        "name" => p.name,
        "tasks" =>
          Enum.map(tasks, fn t ->
            %{"status" => t.status, "priority" => t.priority, "description" => t.description}
          end),
        "reminders" => reminders
      }
    end)
  end

  defp collect_projects_with_notes do
    Manager.list()
    |> Enum.map(fn p ->
      notes = Note.list_for_project(p.name)

      %{
        "name" => p.name,
        "notes" => Enum.map(notes, fn n -> %{"text" => n.text} end)
      }
    end)
  end

  defp format_trend("accelerating"), do: "en accélération"
  defp format_trend("decelerating"), do: "en ralentissement"
  defp format_trend(_), do: "stable"
end
