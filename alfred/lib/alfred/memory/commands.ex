defmodule Alfred.Memory.Commands do
  @moduledoc """
  Commandes mémoire — Alfred révèle ce qu'il sait et se souvient.
  """

  alias Alfred.Butler
  alias Alfred.Memory.{Episodic, Semantic, Procedural}

  def handle(["facts"]) do
    facts = Semantic.all_facts()

    if Enum.empty?(facts) do
      Butler.say("Monsieur, ma mémoire est vierge. Conversez avec moi pour que j'apprenne.")
    else
      Butler.say("Monsieur, voici ce que j'ai retenu :\n")

      facts
      |> Enum.group_by(& &1["category"])
      |> Enum.each(fn {category, cat_facts} ->
        IO.puts("  ── #{format_category(category)} ──")

        Enum.each(cat_facts, fn f ->
          confidence = round(f["confidence"] * 100)
          IO.puts("  ##{f["id"]} #{f["content"]}  (#{confidence}%)")
        end)

        IO.puts("")
      end)
    end
  end

  def handle(["search" | words]) when words != [] do
    query = Enum.join(words, " ")
    results = Semantic.search(query, limit: 10)

    if Enum.empty?(results) do
      Butler.say("Monsieur, je n'ai aucun souvenir correspondant à \"#{query}\".")
    else
      Butler.say("Monsieur, voici ce que je sais sur \"#{query}\" :\n")

      Enum.each(results, fn f ->
        IO.puts("  ##{f["id"]} [#{f["category"]}] #{f["content"]}")
      end)

      IO.puts("")
    end
  end

  def handle(["episodes"]) do
    episodes = Episodic.list_episodes()

    if Enum.empty?(episodes) do
      Butler.say("Monsieur, nous n'avons pas encore conversé. Utilisez 'alfred chat' pour commencer.")
    else
      Butler.say("Monsieur, voici l'historique de nos conversations :\n")

      Enum.each(episodes, fn ep ->
        date = String.slice(ep["started_at"] || "", 0, 16)
        count = ep["message_count"] || 0
        summary = ep["summary"] || "Pas de résumé"
        mode = if ep["mode"] == "ask", do: "question", else: "conversation"
        IO.puts("  [#{date}] #{mode}, #{count} messages — #{summary}")
      end)

      IO.puts("")
    end
  end

  def handle(["forget", id_str]) do
    case Integer.parse(id_str) do
      {id, ""} ->
        case Semantic.delete_fact(id) do
          :ok ->
            Butler.say("Fait ##{id} oublié, Monsieur.")

          {:error, :not_found} ->
            Butler.say("Je suis navré Monsieur, le fait ##{id} n'existe pas.")
        end

      _ ->
        Butler.say("Monsieur, veuillez indiquer un numéro de fait valide.")
    end
  end

  def handle(["patterns"]) do
    patterns = Procedural.active_patterns()

    if Enum.empty?(patterns) do
      Butler.say("Monsieur, je n'ai pas encore détecté de patterns comportementaux.")
    else
      Butler.say("Monsieur, voici les tendances que j'ai observées :\n")

      Enum.each(patterns, fn p ->
        confidence = round(p["confidence"] * 100)
        IO.puts("  ##{p["id"]} #{p["description"]}  (#{confidence}%)")
      end)

      IO.puts("")
    end
  end

  def handle(["consolidate"]) do
    Butler.say("Consolidation de la mémoire en cours, Monsieur...")

    case Alfred.Memory.Consolidator.run() do
      {:ok, stats} ->
        Butler.say("Consolidation terminée.")
        IO.puts("  Épisodes archivés : #{stats["episodes_archived"]}")
        IO.puts("  Faits avant/après : #{stats["facts_before"]} → #{stats["facts_after"]}")
        IO.puts("  Faits oubliés     : #{stats["facts_decayed"]}")
        IO.puts("  Patterns élagués  : #{stats["patterns_pruned"]}")
        IO.puts("  Synthèse          : #{if stats["synthesis_generated"], do: "oui", else: "non"}")
        IO.puts("  Durée             : #{stats["duration_ms"]}ms")
        IO.puts("")

      {:error, reason} ->
        Butler.say("Erreur lors de la consolidation : #{inspect(reason)}")
    end
  end

  def handle(["stats"]) do
    stats = Alfred.Memory.Consolidator.stats()
    Butler.say("Monsieur, voici l'état de ma mémoire :\n")
    IO.puts("  Épisodes  : #{stats.episodes}")
    IO.puts("  Faits     : #{stats.facts}")
    IO.puts("  Patterns  : #{stats.patterns}")
    IO.puts("  Synthèse  : #{if stats.synthesis, do: "oui", else: "non"}")
    IO.puts("  Dernière consolidation : #{stats.last_consolidation || "jamais"}")
    IO.puts("")
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes de mémoire sont :\n")

    IO.puts("""
      alfred memory facts                 Faits mémorisés
      alfred memory search <mots>         Rechercher dans la mémoire
      alfred memory episodes              Historique des conversations
      alfred memory patterns              Patterns détectés
      alfred memory forget <id>           Oublier un fait
      alfred memory consolidate           Consolider la mémoire
      alfred memory stats                 Statistiques mémoire
    """)
  end

  defp format_category("preferences"), do: "Préférences"
  defp format_category("knowledge"), do: "Connaissances"
  defp format_category("project_context"), do: "Contexte projets"
  defp format_category("behavioral_patterns"), do: "Comportement"
  defp format_category("personal_info"), do: "Informations personnelles"
  defp format_category("technical_context"), do: "Contexte technique"
  defp format_category(other), do: other
end
