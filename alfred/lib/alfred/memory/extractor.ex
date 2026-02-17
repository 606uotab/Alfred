defmodule Alfred.Memory.Extractor do
  @moduledoc """
  Extraction de faits — Alfred apprend de chaque conversation.
  Utilise Mistral pour identifier les faits importants dans un échange.
  """

  alias Alfred.Chat.Client
  alias Alfred.Memory.Semantic

  @extraction_prompt """
  Analyse la conversation suivante entre un majordome (Alfred) et son maître.
  Extrais les faits importants à retenir sur le maître.

  Retourne UNIQUEMENT un tableau JSON (pas de texte autour) avec ce format :
  [{"category": "...", "subject": "...", "content": "...", "confidence": 0.8}]

  Catégories possibles : preferences, knowledge, project_context, behavioral_patterns, personal_info, technical_context.

  Le "content" doit être une phrase concise en français décrivant le fait.
  Le "subject" est un mot-clé court.
  La "confidence" est entre 0.0 et 1.0.

  Si aucun fait intéressant, retourne [].

  Conversation :
  """

  @doc """
  Extrait les faits d'une conversation via Mistral.
  """
  def extract_via_mistral(messages, api_token) do
    conversation_text =
      messages
      |> Enum.map(fn msg ->
        role = if msg["role"] == "user", do: "Maître", else: "Alfred"
        "#{role}: #{msg["content"]}"
      end)
      |> Enum.join("\n")

    prompt = @extraction_prompt <> conversation_text

    api_messages = [
      %{"role" => "system", "content" => "Tu es un analyseur de conversation. Tu extrais des faits structurés en JSON."},
      %{"role" => "user", "content" => prompt}
    ]

    case Client.chat_completion(api_token, api_messages) do
      {:ok, response} ->
        parse_and_store_facts(response, messages)

      {:error, _reason} ->
        # Extraction silencieuse — pas grave si ça échoue
        {:ok, []}
    end
  end

  @doc """
  Extraction locale légère (sans API) — analyse basique des messages du maître.
  """
  def extract_local(messages) do
    user_messages =
      messages
      |> Enum.filter(&(&1["role"] == "user"))
      |> Enum.map(&(&1["content"]))

    facts =
      user_messages
      |> Enum.flat_map(&detect_simple_facts/1)
      |> Enum.uniq_by(& &1["content"])

    Enum.each(facts, fn fact ->
      Semantic.add_fact(fact)
    end)

    {:ok, facts}
  end

  # -- Privé --

  defp parse_and_store_facts(response, _messages) do
    # Tenter d'extraire le JSON du response
    json_str = extract_json_array(response)

    case Jason.decode(json_str) do
      {:ok, facts} when is_list(facts) ->
        stored =
          Enum.map(facts, fn fact ->
            case Semantic.add_fact(fact) do
              {:ok, stored_fact} -> stored_fact
              _ -> nil
            end
          end)
          |> Enum.reject(&is_nil/1)

        {:ok, stored}

      _ ->
        {:ok, []}
    end
  end

  defp extract_json_array(text) do
    # Chercher un tableau JSON dans le texte
    case Regex.run(~r/\[[\s\S]*\]/, text) do
      [match] -> match
      _ -> "[]"
    end
  end

  defp detect_simple_facts(text) do
    facts = []

    # Détection de préférences ("je préfère", "j'aime", "je n'aime pas")
    facts =
      if Regex.match?(~r/je (préfère|prefere|aime|adore|déteste|deteste)/i, text) do
        [%{"category" => "preferences", "subject" => "preference", "content" => String.slice(text, 0, 200), "confidence" => 0.6, "source" => "local_extraction"} | facts]
      else
        facts
      end

    # Détection d'informations personnelles ("je suis", "je travaille", "mon")
    facts =
      if Regex.match?(~r/je (suis|travaille|fais|habite|vis)/i, text) do
        [%{"category" => "personal_info", "subject" => "info", "content" => String.slice(text, 0, 200), "confidence" => 0.5, "source" => "local_extraction"} | facts]
      else
        facts
      end

    # Détection de contexte projet ("projet", "application", "système")
    facts =
      if Regex.match?(~r/(projet|application|système|systeme|architecture)/i, text) && String.length(text) > 20 do
        [%{"category" => "project_context", "subject" => "project", "content" => String.slice(text, 0, 200), "confidence" => 0.4, "source" => "local_extraction"} | facts]
      else
        facts
      end

    facts
  end
end
