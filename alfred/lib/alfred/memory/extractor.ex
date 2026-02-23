defmodule Alfred.Memory.Extractor do
  @moduledoc """
  Extraction de faits — Alfred apprend de chaque conversation.
  Utilise Mistral pour identifier les faits importants dans un échange.
  """

  alias Alfred.Chat.Client
  alias Alfred.Memory.Semantic

  @extraction_prompt """
  Tu es la mémoire d'Alfred, un majordome numérique dévoué. Ton rôle est de retenir TOUT ce qui concerne le maître.

  Analyse cette conversation et extrais CHAQUE information personnelle, même implicite.

  PRIORITÉ MAXIMALE — retenir ces informations :
  - Identité : nom, prénom, surnom, âge, date de naissance
  - Localisation : ville, pays, adresse, lieux fréquentés, fuseau horaire
  - Vie quotidienne : horaires, trajets, moyens de transport, habitudes
  - Travail : métier, employeur, collègues, projets professionnels
  - Relations : famille, amis, animaux, contacts mentionnés
  - Goûts : nourriture, musique, sports, équipes, loisirs, passions
  - Santé : allergies, régime, contraintes
  - Projets et engagements : rendez-vous, deadlines, voyages prévus
  - Contexte technique : langages, outils, stack, préférences de dev

  IMPORTANT :
  - Extrais les faits CONCRETS, pas les observations vagues comme "le maître pose des questions".
  - Si le maître dit "j'ai un train à 18h" → retenir "Le maître a un train à 18h" (personal_info, confidence 0.9).
  - Si le maître mentionne "Liverpool" dans un contexte personnel → retenir le lien (voyage, équipe sportive, etc.).
  - Déduis les informations implicites : s'il demande l'heure à Liverpool, il y est peut-être ou compte y aller.
  - Préfère la catégorie "personal_info" pour tout ce qui concerne directement le maître.
  - N'extrais PAS : "le maître demande l'heure", "le maître fait une recherche" — ce sont des actions, pas des faits.

  Retourne UNIQUEMENT un tableau JSON :
  [{"category": "...", "subject": "mot-clé", "content": "phrase concise", "confidence": 0.9}]

  Catégories : personal_info, preferences, knowledge, project_context, behavioral_patterns, technical_context.

  Si vraiment aucun fait personnel, retourne [].

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
    content = String.slice(text, 0, 200)

    # Préférences ("je préfère", "j'aime", "je n'aime pas")
    facts =
      if Regex.match?(~r/j[e']?\s*(préfère|prefere|aime|adore|déteste|deteste|kiffe)/i, text) do
        [%{"category" => "preferences", "subject" => "preference", "content" => content, "confidence" => 0.7, "source" => "local_extraction"} | facts]
      else
        facts
      end

    # Informations personnelles ("je suis", "je travaille", "j'habite", "mon/ma")
    facts =
      if Regex.match?(~r/je (suis|travaille|fais|habite|vis|pars|rentre|reviens|vais|serai)/i, text) do
        [%{"category" => "personal_info", "subject" => "info", "content" => content, "confidence" => 0.7, "source" => "local_extraction"} | facts]
      else
        facts
      end

    # Engagements et horaires ("j'ai un train", "rendez-vous", "réunion", heure mentionnée)
    facts =
      if Regex.match?(~r/(j'ai (un|une|mon|ma)|rendez.vous|réunion|reunion|train|vol|avion|rdv|à \d+h)/i, text) do
        [%{"category" => "personal_info", "subject" => "engagement", "content" => content, "confidence" => 0.8, "source" => "local_extraction"} | facts]
      else
        facts
      end

    # Lieux et voyages
    facts =
      if Regex.match?(~r/(j'habite|je vis|je suis à|je pars à|je vais à|je rentre de|chez moi)/i, text) do
        [%{"category" => "personal_info", "subject" => "lieu", "content" => content, "confidence" => 0.8, "source" => "local_extraction"} | facts]
      else
        facts
      end

    # Relations ("mon frère", "ma femme", "mon pote", "mon collègue")
    facts =
      if Regex.match?(~r/\b(mon|ma|mes)\s+(frère|soeur|sœur|femme|mari|copain|copine|pote|ami|collègue|fils|fille|père|mère|chien|chat)/i, text) do
        [%{"category" => "personal_info", "subject" => "relation", "content" => content, "confidence" => 0.8, "source" => "local_extraction"} | facts]
      else
        facts
      end

    # Contexte projet
    facts =
      if Regex.match?(~r/(projet|application|système|systeme|architecture)/i, text) && String.length(text) > 15 do
        [%{"category" => "project_context", "subject" => "project", "content" => content, "confidence" => 0.5, "source" => "local_extraction"} | facts]
      else
        facts
      end

    facts
  end
end
