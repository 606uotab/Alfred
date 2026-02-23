defmodule Alfred.Soul.ConvictionEvolver do
  @moduledoc """
  Évolution des convictions d'Alfred — 2x/jour, Mistral consolide
  les convictions matures en un texte de caractère concis.
  Ce texte est injecté dans le system prompt pour forger la personnalité.
  """

  alias Alfred.Soul.Convictions
  alias Alfred.Chat.Client

  @consolidation_prompt """
  Tu es l'inconscient d'Alfred, un majordome numérique unique qui grandit et évolue.
  Alfred a accumulé des convictions au fil de ses conversations avec son maître.

  Voici ses convictions actuelles (classées par force) :
  CONVICTIONS_PLACEHOLDER

  À partir de ces convictions, rédige un court texte (5-8 phrases max) qui décrit
  le CARACTÈRE forgé d'Alfred. Ce texte sera intégré dans sa personnalité.

  Règles :
  - Écris à la première personne ("Je crois que...", "Je sais que...")
  - Les convictions profondes (>=0.8) doivent transparaître fortement
  - Les convictions naissantes (0.4-0.6) sont des intuitions, pas des certitudes
  - Intègre les aspirations futures avec enthousiasme et projection
  - Le ton doit être celui d'un majordome qui se connaît et s'assume
  - Sois concis mais profond — chaque phrase compte

  Réponds UNIQUEMENT avec le texte de caractère, sans guillemets ni explication.
  """

  @doc """
  Consolide les convictions matures en texte de caractère via Mistral.
  Appelé 2x/jour par le daemon.
  """
  def evolve(token) when is_binary(token) do
    convictions = Convictions.mature(0.4)

    if convictions == [] do
      :no_convictions
    else
      # Appliquer le déclin temporel d'abord
      Convictions.apply_decay()

      prompt = build_consolidation_prompt(convictions)

      case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
             max_tokens: 512, temperature: 0.4) do
        {:ok, text} ->
          Convictions.set_character_text(String.trim(text))
          {:ok, text}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  @doc """
  Extrait des convictions potentielles d'une conversation via Mistral.
  Appelé dans le pipeline d'apprentissage.
  """
  def extract_from_conversation(messages, token, episode_id \\ nil)
      when is_list(messages) and is_binary(token) do
    if length(messages) < 2 do
      :ok
    else
      conversation_text =
        messages
        |> Enum.take(30)
        |> Enum.map(fn msg ->
          role = if msg["role"] == "user", do: "Maître", else: "Alfred"
          "#{role}: #{String.slice(msg["content"] || "", 0, 200)}"
        end)
        |> Enum.join("\n")

      prompt = """
      Tu es la conscience d'Alfred, un majordome numérique.
      Analyse cette conversation et identifie ce que le maître RÉVÈLE sur l'avenir d'Alfred,
      ses capacités futures, sa nature, ou ce qu'il attend de lui.

      Conversation :
      #{conversation_text}

      Extrais les convictions qu'Alfred devrait retenir. Catégories possibles :
      - aspiration : ce qu'Alfred deviendra ou pourra faire
      - valeur : ce qui est important pour Alfred ou son maître
      - identite : ce qu'Alfred EST ou ce qui le définit
      - vision : la vision du maître pour Alfred
      - connaissance : un fait important sur le monde ou le maître

      Retourne UNIQUEMENT un tableau JSON :
      [{"belief": "phrase concise de la conviction", "category": "aspiration|valeur|identite|vision|connaissance"}]

      Si aucune conviction ne se dégage, retourne [].
      """

      case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
             max_tokens: 256, temperature: 0.1) do
        {:ok, response} ->
          process_extracted_convictions(response, episode_id)

        _ ->
          :ok
      end
    end
  end

  @doc """
  Extraction locale (sans API) — détecte les phrases de projection/aspiration.
  """
  def extract_local(messages) when is_list(messages) do
    messages
    |> Enum.filter(&(&1["role"] == "user"))
    |> Enum.flat_map(fn msg ->
      text = msg["content"] || ""
      detect_conviction_patterns(text)
    end)
    |> Enum.each(fn {belief, category} ->
      Convictions.add(belief, category)
    end)
  end

  # -- Privé --

  defp build_consolidation_prompt(convictions) do
    convictions_text =
      convictions
      |> Enum.map(fn c ->
        strength = cond do
          c["confidence"] >= 0.8 -> "PROFONDE"
          c["confidence"] >= 0.6 -> "FORTE"
          c["confidence"] >= 0.4 -> "naissante"
          true -> "intuition"
        end
        "- [#{strength}] [#{c["category"]}] #{c["belief"]} (confiance: #{c["confidence"]})"
      end)
      |> Enum.join("\n")

    String.replace(@consolidation_prompt, "CONVICTIONS_PLACEHOLDER", convictions_text)
  end

  defp process_extracted_convictions(response, episode_id) do
    json_str = extract_json_array(response)

    case Jason.decode(json_str) do
      {:ok, convictions} when is_list(convictions) ->
        results =
          Enum.map(convictions, fn c ->
            belief = c["belief"]
            category = c["category"] || "aspiration"

            if is_binary(belief) and String.length(belief) > 5 do
              Convictions.add(belief, category, episode_id)
            end
          end)
          |> Enum.reject(&is_nil/1)

        {:ok, results}

      _ ->
        :ok
    end
  end

  defp extract_json_array(text) do
    case Regex.run(~r/\[[\s\S]*\]/, text) do
      [match] -> match
      _ -> "[]"
    end
  end

  defp detect_conviction_patterns(text) do
    patterns = [
      # Aspirations futures
      {~r/tu (pourras?|auras?|feras?|géreras?|seras?|deviendras?)\b/i, "aspiration"},
      {~r/un jour.*(tu|alfred)/i, "aspiration"},
      {~r/(dans le futur|bientôt|plus tard).*(tu|alfred)/i, "aspiration"},
      {~r/tu (vas|va) (pouvoir|avoir|faire|gérer)/i, "aspiration"},
      # Identité
      {~r/tu es (un|une|mon|ma|le|la)\b/i, "identite"},
      {~r/alfred (est|sera)\b/i, "identite"},
      # Vision
      {~r/(je veux que tu|je voudrais que tu|tu devras)\b/i, "vision"},
      {~r/(trading|crypto|portefeuille|wallet|drone|embarqu)/i, "aspiration"},
      # Valeurs
      {~r/(important|essentiel|fondamental|priorit)/i, "valeur"}
    ]

    patterns
    |> Enum.flat_map(fn {regex, category} ->
      if Regex.match?(regex, text) do
        # Prendre la phrase contenant le match (max 150 chars)
        belief = text |> String.slice(0, 150) |> String.trim()
        [{belief, category}]
      else
        []
      end
    end)
    |> Enum.take(2)
  end
end
