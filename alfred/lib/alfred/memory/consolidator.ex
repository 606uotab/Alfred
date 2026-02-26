defmodule Alfred.Memory.Consolidator do
  @moduledoc """
  Pipeline de consolidation nocturne — Alfred dort et consolide ses souvenirs.
  Archivage des vieux épisodes, fusion des faits, élagage des patterns,
  et génération d'une synthèse mémoire via Mistral.
  """

  alias Alfred.Memory.{Episodic, Semantic, Procedural}
  alias Alfred.Chat.Client
  alias Alfred.Storage.Local, as: Storage

  @synthesis_file "memory/synthesis.json"
  @log_file "memory/consolidation_log.json"
  @archive_age_days 7
  @decay_age_days 300
  @decay_confidence_threshold 0.3
  @pattern_confidence_threshold 0.2

  @synthesis_prompt """
  Tu es l'inconscient d'Alfred, un majordome numérique unique.
  À partir des faits importants, des patterns comportementaux et des résumés
  de conversations récentes, rédige une synthèse concise (15-20 phrases)
  de ce qu'Alfred sait et comprend sur son maître et son univers.

  Règles :
  - Écris à la première personne ("Je sais que...", "J'ai remarqué que...")
  - Priorise les faits les plus accédés (les plus importants)
  - Intègre les patterns comportementaux comme des intuitions
  - Mentionne les sujets récurrents des conversations
  - Sois concis mais complet — chaque phrase apporte une information
  - Ne répète pas, ne paraphrase pas

  Réponds UNIQUEMENT avec le texte de synthèse, sans guillemets ni explication.
  """

  # -- API publique --

  @doc "Lance le pipeline complet de consolidation (avec synthèse Mistral)."
  def run do
    case Alfred.Chat.Commands.authenticate() do
      {:ok, token, _, _} -> run(token)
      _ -> run(nil)
    end
  end

  @doc "Lance le pipeline avec un token Mistral optionnel."
  def run(token) do
    Alfred.Log.info("Consolidator", "Début de la consolidation nocturne")
    start_time = System.monotonic_time(:millisecond)

    stats = %{
      "date" => Date.utc_today() |> Date.to_iso8601(),
      "started_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    # 1. Archiver les vieux épisodes
    {episodes_archived, episodes_total} = archive_old_episodes()
    Alfred.Log.info("Consolidator", "Épisodes archivés : #{episodes_archived}/#{episodes_total}")

    # 2. Consolider les faits sémantiques
    facts_before = Semantic.count()
    Semantic.consolidate()
    facts_decayed = decay_stale_facts()
    facts_after = Semantic.count()
    Alfred.Log.info("Consolidator", "Faits : #{facts_before} → #{facts_after} (#{facts_decayed} oubliés)")

    # 3. Élaguer les patterns
    patterns_pruned = prune_old_patterns()
    Alfred.Log.info("Consolidator", "Patterns élagués : #{patterns_pruned}")

    # 4. Synthèse mémoire (si token disponible)
    synthesis_ok = if token do
      case generate_synthesis(token) do
        {:ok, _text} ->
          Alfred.Log.info("Consolidator", "Synthèse mémoire générée")
          true
        {:error, reason} ->
          Alfred.Log.error("Consolidator", "Synthèse échouée : #{inspect(reason)}")
          false
      end
    else
      Alfred.Log.info("Consolidator", "Pas de token Mistral — synthèse ignorée")
      false
    end

    # 5. Log
    duration = System.monotonic_time(:millisecond) - start_time

    stats = Map.merge(stats, %{
      "episodes_archived" => episodes_archived,
      "facts_before" => facts_before,
      "facts_after" => facts_after,
      "facts_decayed" => facts_decayed,
      "patterns_pruned" => patterns_pruned,
      "synthesis_generated" => synthesis_ok,
      "duration_ms" => duration
    })

    save_log(stats)
    Alfred.Log.info("Consolidator", "Consolidation terminée en #{duration}ms")

    # Notification
    text = "Consolidation nocturne terminée : #{episodes_archived} épisodes archivés, " <>
           "#{facts_decayed} faits oubliés, #{patterns_pruned} patterns élagués."
    Alfred.Simplex.Bridge.send_group_notification(text)

    {:ok, stats}
  rescue
    e ->
      Alfred.Log.error("Consolidator", Exception.message(e))
      {:error, Exception.message(e)}
  end

  @doc "Charge la synthèse mémoire actuelle."
  def load_synthesis do
    case Storage.read(@synthesis_file) do
      data when is_map(data) and map_size(data) > 0 -> data
      _ -> nil
    end
  end

  @doc "Charge le dernier log de consolidation."
  def last_log do
    case Storage.read(@log_file) do
      logs when is_list(logs) and logs != [] -> List.last(logs)
      _ -> nil
    end
  end

  @doc "Stats mémoire rapides."
  def stats do
    %{
      episodes: Episodic.count(),
      facts: Semantic.count(),
      patterns: Procedural.count_active(),
      last_consolidation: case last_log() do
        nil -> nil
        log -> log["date"]
      end,
      synthesis: case load_synthesis() do
        nil -> false
        _ -> true
      end
    }
  end

  # -- Archivage des épisodes --

  defp archive_old_episodes do
    cutoff = Date.utc_today() |> Date.add(-@archive_age_days) |> Date.to_iso8601()
    episodes = Episodic.list_episodes()

    old_episodes = Enum.filter(episodes, fn ep ->
      started = ep["started_at"] || ""
      started < cutoff and ep["summary"] != nil
    end)

    archived = Enum.reduce(old_episodes, 0, fn ep, count ->
      case Episodic.load_episode(ep["id"]) do
        {:ok, full} ->
          messages = full["messages"] || []
          if messages != [] and not (full["archived"] == true) do
            Episodic.update_episode(ep["id"], %{"messages" => [], "archived" => true})
            count + 1
          else
            count
          end
        _ -> count
      end
    end)

    {archived, length(episodes)}
  end

  # -- Décroissance des faits --

  defp decay_stale_facts do
    cutoff = Date.utc_today() |> Date.add(-@decay_age_days) |> Date.to_iso8601()
    facts = Semantic.all_facts()

    stale = Enum.filter(facts, fn f ->
      accessed = f["last_accessed"] || f["created_at"] || ""
      confidence = f["confidence"] || 0.5
      accessed < cutoff and confidence < @decay_confidence_threshold
    end)

    Enum.each(stale, fn f -> Semantic.delete_fact(f["id"]) end)
    length(stale)
  end

  # -- Élagage des patterns --

  defp prune_old_patterns do
    cutoff = Date.utc_today() |> Date.add(-@decay_age_days) |> Date.to_iso8601()
    patterns = Procedural.all_patterns()

    old = Enum.filter(patterns, fn p ->
      confirmed = p["last_confirmed"] || p["detected_at"] || ""
      confidence = p["confidence"] || 0.5
      p["active"] == true and confirmed < cutoff and confidence < @pattern_confidence_threshold
    end)

    Enum.each(old, fn p -> Procedural.deactivate_pattern(p["id"]) end)
    length(old)
  end

  # -- Synthèse mémoire --

  defp generate_synthesis(token) do
    top_facts = Semantic.top_facts(20)
    patterns = Procedural.active_patterns()
    summaries = Episodic.recent_summaries(10)

    context_parts = []

    context_parts = if top_facts != [] do
      facts_text = top_facts
        |> Enum.map(fn f -> "- [#{f["subject"]}] #{f["content"]} (accès: #{f["access_count"]})" end)
        |> Enum.join("\n")
      context_parts ++ ["Faits importants :\n#{facts_text}"]
    else
      context_parts
    end

    context_parts = if patterns != [] do
      patterns_text = patterns
        |> Enum.take(10)
        |> Enum.map(fn p -> "- #{p["description"]} (confiance: #{p["confidence"]})" end)
        |> Enum.join("\n")
      context_parts ++ ["Patterns comportementaux :\n#{patterns_text}"]
    else
      context_parts
    end

    context_parts = if summaries != [] do
      summaries_text = Enum.join(summaries, "\n- ")
      context_parts ++ ["Résumés récents :\n- #{summaries_text}"]
    else
      context_parts
    end

    if context_parts == [] do
      {:error, :no_data}
    else
      prompt = @synthesis_prompt <> "\n\n" <> Enum.join(context_parts, "\n\n")

      case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
             max_tokens: 1024, temperature: 0.4) do
        {:ok, text} ->
          synthesis = %{
            "text" => String.trim(text),
            "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
            "facts_count" => length(top_facts),
            "patterns_count" => length(patterns)
          }
          Storage.write(@synthesis_file, synthesis)
          {:ok, text}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  # -- Log --

  defp save_log(stats) do
    logs = case Storage.read(@log_file) do
      l when is_list(l) -> l
      _ -> []
    end

    # Garder les 30 derniers logs
    logs = Enum.take(logs ++ [stats], -30)
    Storage.write(@log_file, logs)
  end
end
