defmodule Alfred.Soul.Evolver do
  @moduledoc """
  Évolution de la personnalité d'Alfred — micro-ajustements basés sur les conversations.
  Utilise Mistral pour analyser le ton et proposer des changements de traits.
  """

  alias Alfred.Soul.State
  alias Alfred.Chat.Client

  @evolution_prompt """
  Tu es un psychologue qui analyse une conversation entre un majordome IA (Alfred) et son maître.

  Voici les traits actuels d'Alfred (0.0 à 1.0) :
  TRAITS_PLACEHOLDER

  Voici la conversation :
  CONVERSATION_PLACEHOLDER

  Analyse le ton et le contenu de la conversation. Propose des micro-ajustements (entre -0.05 et +0.05) UNIQUEMENT si la conversation le justifie clairement. Ne change rien si rien ne le justifie.

  Réponds UNIQUEMENT avec un JSON valide, sans markdown :
  [{"trait": "nom_du_trait", "delta": 0.02, "reason": "explication courte"}]

  Si aucun ajustement n'est nécessaire, réponds : []
  """

  @doc """
  Analyse une conversation terminée et fait évoluer les traits d'Alfred.
  """
  def evolve_from_conversation(session, token) do
    state = State.load()
    messages = session.messages

    if length(messages) < 2 do
      :ok
    else
      prompt = build_evolution_prompt(state, messages)

      case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
             max_tokens: 256, temperature: 0.1) do
        {:ok, response} ->
          apply_adjustments(state, response)

        _ ->
          :ok
      end
    end
  end

  @doc """
  Évolution basée sur les patterns comportementaux détectés.
  """
  def evolve_from_patterns(patterns) when is_list(patterns) do
    state = State.load()

    adjustments =
      patterns
      |> Enum.flat_map(&pattern_to_adjustments/1)
      |> Enum.group_by(fn {trait, _delta, _reason} -> trait end)
      |> Enum.map(fn {trait, entries} ->
        avg_delta = entries |> Enum.map(fn {_, d, _} -> d end) |> Enum.sum() |> Kernel./(length(entries))
        reason = entries |> Enum.map(fn {_, _, r} -> r end) |> Enum.join("; ")
        {trait, avg_delta, reason}
      end)

    state =
      Enum.reduce(adjustments, state, fn {trait, delta, reason}, acc ->
        State.apply_adjustment(acc, trait, delta, reason)
      end)

    State.save(state)
    :ok
  end

  def evolve_from_patterns(_), do: :ok

  # -- Privé --

  defp build_evolution_prompt(state, messages) do
    traits_text =
      state.traits
      |> Enum.map(fn {k, v} -> "  #{k}: #{v}" end)
      |> Enum.join("\n")

    conversation_text =
      messages
      |> Enum.take(20)
      |> Enum.map(fn msg ->
        role = if msg["role"] == "user", do: "Maître", else: "Alfred"
        "#{role}: #{String.slice(msg["content"] || "", 0, 200)}"
      end)
      |> Enum.join("\n")

    @evolution_prompt
    |> String.replace("TRAITS_PLACEHOLDER", traits_text)
    |> String.replace("CONVERSATION_PLACEHOLDER", conversation_text)
  end

  defp apply_adjustments(state, response) do
    case Jason.decode(clean_json(response)) do
      {:ok, adjustments} when is_list(adjustments) ->
        state =
          Enum.reduce(adjustments, state, fn adj, acc ->
            trait = adj["trait"]
            delta = adj["delta"]
            reason = adj["reason"]

            if is_binary(trait) and is_number(delta) and abs(delta) <= 0.05 do
              State.apply_adjustment(acc, trait, delta, reason)
            else
              acc
            end
          end)

        State.save(state)
        :ok

      _ ->
        :ok
    end
  end

  defp clean_json(text) do
    text
    |> String.trim()
    |> String.replace(~r/^```json\s*/, "")
    |> String.replace(~r/\s*```$/, "")
    |> String.trim()
  end

  defp pattern_to_adjustments(pattern) do
    desc = String.downcase(pattern["description"] || "")

    cond do
      String.contains?(desc, "sessions courtes") or String.contains?(desc, "concis") ->
        [{"verbosity", -0.02, "sessions courtes détectées"}]

      String.contains?(desc, "questions") or String.contains?(desc, "curieux") ->
        [{"curiosity", 0.02, "curiosité observée"}]

      String.contains?(desc, "humour") or String.contains?(desc, "blague") ->
        [{"humor", 0.02, "humour apprécié"}]

      String.contains?(desc, "formel") or String.contains?(desc, "professionnel") ->
        [{"formality", 0.02, "contexte formel"}]

      String.contains?(desc, "proactif") or String.contains?(desc, "initiative") ->
        [{"proactivity", 0.02, "proactivité appréciée"}]

      true ->
        []
    end
  end
end
