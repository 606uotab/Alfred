defmodule Alfred.Cortex.Commands do
  @moduledoc """
  Commandes du cortex — Alfred analyse ses tendances à long terme.
  Utilise le moteur R pour l'analyse statistique.
  """

  alias Alfred.Butler
  alias Alfred.Cortex.Port, as: Cortex
  alias Alfred.Memory.{Episodic, Semantic}

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

  def handle(_) do
    Butler.say("Monsieur, les commandes du cortex sont :\n")

    IO.puts("""
      alfred cortex trends                Tendances des interactions
      alfred cortex stats                 Statistiques de mémoire
      alfred cortex analyze               Analyse comportementale
    """)
  end
end
