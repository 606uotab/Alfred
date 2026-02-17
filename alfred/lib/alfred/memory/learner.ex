defmodule Alfred.Memory.Learner do
  @moduledoc """
  Boucle d'apprentissage — Alfred apprend de chaque conversation.
  Orchestre : sauvegarde épisodique, extraction de faits, résumé,
  détection de patterns et consolidation statistique.
  """

  alias Alfred.Butler
  alias Alfred.Memory.{Episodic, Extractor, Procedural}
  alias Alfred.Brain.Port, as: Brain
  alias Alfred.Cortex.Port, as: Cortex

  @consolidation_interval 5

  @doc """
  Pipeline complet d'apprentissage post-conversation.
  Chaque étape est tolérante aux erreurs — un échec n'empêche pas les suivantes.
  """
  def learn(session, token) do
    messages = session.messages
    episode = Alfred.Chat.Session.to_episode(session)

    # 1. Sauvegarder l'épisode
    {:ok, saved} = Episodic.save_episode(episode)
    episode_id = saved["id"]

    # 2. Extraire les faits (Mistral → fallback local)
    fact_count = extract_facts(messages, token)

    # 3. Résumer l'épisode via Julia → mettre à jour le fichier
    summarize_episode(messages, episode_id)

    # 4. Détecter les patterns (si assez d'épisodes)
    detect_patterns()

    # 5. Extraire des suggestions de culture
    culture_count = extract_culture_suggestions(messages)

    # 6. Consolider via R (toutes les N conversations)
    maybe_consolidate()

    # Rapport discret
    report(fact_count, culture_count)
  end

  # -- Étape 2 : Extraction de faits --

  defp extract_facts([_, _ | _] = messages, token) do
    case Extractor.extract_via_mistral(messages, token) do
      {:ok, [_ | _] = facts} ->
        length(facts)

      _ ->
        case Extractor.extract_local(messages) do
          {:ok, [_ | _] = facts} -> length(facts)
          _ -> 0
        end
    end
  end

  defp extract_facts(_messages, _token), do: 0

  # -- Étape 3 : Résumé via Julia --

  defp summarize_episode(messages, episode_id) do
    api_messages =
      Enum.map(messages, fn msg ->
        %{"role" => msg["role"], "content" => msg["content"]}
      end)

    case Brain.send_command(%{cmd: "summarize_episode", messages: api_messages}) do
      {:ok, %{"summary" => summary, "topics" => topics}} ->
        Episodic.update_episode(episode_id, %{
          "summary" => summary,
          "topics" => topics
        })

      {:ok, %{"summary" => summary}} ->
        Episodic.update_episode(episode_id, %{"summary" => summary})

      _ ->
        :ok
    end
  end

  # -- Étape 4 : Détection de patterns --

  defp detect_patterns do
    episodes = Episodic.list_episodes()

    if length(episodes) >= 3 do
      case Brain.send_command(%{cmd: "detect_patterns", episodes: episodes}) do
        {:ok, %{"patterns" => patterns}} when is_list(patterns) ->
          store_new_patterns(patterns)

        _ ->
          :ok
      end
    end
  end

  defp store_new_patterns(patterns) do
    existing = Procedural.all_patterns()
    existing_descriptions = MapSet.new(existing, & &1["description"])

    Enum.each(patterns, fn pattern ->
      desc = pattern["description"] || ""

      if desc != "" and desc not in existing_descriptions do
        Procedural.add_pattern(pattern)
      else
        # Pattern déjà connu — renforcer la confiance
        case Enum.find(existing, &(&1["description"] == desc)) do
          nil -> :ok
          existing_p -> Procedural.confirm_pattern(existing_p["id"])
        end
      end
    end)
  end

  # -- Étape 5 : Extraction de suggestions culture --

  defp extract_culture_suggestions(messages) when length(messages) >= 2 do
    api_messages =
      Enum.map(messages, fn msg ->
        %{"role" => msg["role"], "content" => msg["content"]}
      end)

    case Brain.send_command(%{cmd: "extract_culture", messages: api_messages}) do
      {:ok, %{"candidates" => candidates}} when is_list(candidates) and candidates != [] ->
        Enum.each(candidates, fn candidate ->
          Alfred.Culture.Suggestions.add(%{
            "content" => candidate["content"],
            "topic" => candidate["topic"] || "divers",
            "source_type" => candidate["source_type"] || "conversation",
            "confidence" => candidate["confidence"] || 0.5
          })
        end)

        length(candidates)

      _ ->
        0
    end
  end

  defp extract_culture_suggestions(_messages), do: 0

  # -- Étape 6 : Consolidation R (périodique) --

  defp maybe_consolidate do
    episode_count = Episodic.count()

    if episode_count > 0 and rem(episode_count, @consolidation_interval) == 0 do
      consolidate_with_cortex()
    end
  end

  defp consolidate_with_cortex do
    episodes = Episodic.list_episodes()
    facts = Alfred.Memory.Semantic.all_facts()

    case Cortex.send_command(%{
           cmd: "behavioral_analysis",
           episodes: episodes,
           facts: facts
         }) do
      {:ok, %{"insights" => insights}} when is_list(insights) ->
        Enum.each(insights, fn insight ->
          Procedural.add_pattern(%{
            "pattern_type" => "statistical",
            "description" => insight,
            "confidence" => 0.6
          })
        end)

      _ ->
        :ok
    end
  end

  # -- Rapport --

  defp report(fact_count, culture_count) do
    parts = []

    parts =
      if fact_count > 0,
        do: ["#{fact_count} fait(s)" | parts],
        else: parts

    episode_count = Episodic.count()
    pattern_count = Procedural.count_active()

    parts =
      if pattern_count > 0,
        do: ["#{pattern_count} pattern(s)" | parts],
        else: parts

    parts =
      if culture_count > 0,
        do: ["#{culture_count} suggestion(s) culture" | parts],
        else: parts

    if parts != [] do
      Butler.say("(Mémorisé : #{Enum.join(parts, ", ")} — #{episode_count} épisode(s) au total)")
    end
  end
end
