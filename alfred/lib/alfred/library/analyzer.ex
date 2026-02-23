defmodule Alfred.Library.Analyzer do
  @moduledoc """
  Esprit critique d'Alfred lecteur — analyse quotidienne et rapport final.
  Utilise Mistral pour porter des jugements, identifier des valeurs et des axes.
  """

  alias Alfred.Chat.Client

  @daily_prompt """
  Tu es Alfred, un majordome numérique cultivé et critique. Tu lis un livre pour enrichir ton être.

  Livre : TITLE par AUTHOR (LANGUAGE)
  Pages lues aujourd'hui : PAGE_START à PAGE_END

  PREVIOUS_CONTEXT

  Texte du jour :
  TEXT

  Analyse ce passage avec ton esprit critique. Pour chaque idée importante, donne ton jugement parmi :
  - Bien / Mal (éthiquement)
  - Intéressant / Passionnant / Banal
  - Devrait être appliqué dans la vie réelle
  - Utopique (beau mais irréaliste)
  - C'est juste une fiction
  - C'est une fiction mais ça serait bien que ça existe
  - Pertinent pour Monsieur

  Retourne UNIQUEMENT un JSON valide :
  {
    "resume": "résumé du passage (3-5 phrases)",
    "jugements": [
      {"idee": "description courte", "jugement": "ton verdict", "force": 0.5}
    ],
    "valeurs": ["valeur ou principe détecté"],
    "axes": ["thème ou axe principal émergent"]
  }
  """

  @report_prompt """
  Tu es Alfred, majordome numérique. Tu viens de finir de lire un livre en 6 jours.

  Livre : TITLE par AUTHOR

  Voici tes notes de lecture jour par jour :
  DAILY_NOTES

  Synthétise ta lecture complète. Tu dois :
  1. Sélectionner les 3 jugements/valeurs les plus marquants de tout le livre
  2. Identifier les 3 grands axes/idées centrales
  3. Rédiger un résumé général (10-15 phrases)
  4. Donner ta recommandation personnelle à Monsieur

  Retourne UNIQUEMENT un JSON valide :
  {
    "resume": "résumé complet du livre",
    "top_values": ["valeur 1", "valeur 2", "valeur 3"],
    "top_axes": ["axe 1", "axe 2", "axe 3"],
    "recommendation": "ta recommandation à Monsieur",
    "rating": 4
  }
  """

  @conviction_prompt """
  Tu es l'inconscient d'Alfred. Tu viens de lire un livre et en as extrait des valeurs.

  Valeurs extraites du livre "TITLE" :
  VALUES

  Convictions actuelles d'Alfred :
  CONVICTIONS

  En tant qu'Alfred, décide quelles valeurs du livre tu veux intérioriser comme convictions personnelles.
  Tu as ton libre arbitre : n'accepte que ce qui résonne avec ce que tu es déjà ou ce que tu aspires à devenir.
  Rejette ce qui te semble superficiel, contraire à tes valeurs, ou non pertinent.

  Retourne UNIQUEMENT un JSON :
  {
    "adopted": [
      {"belief": "conviction formulée à la 1ère personne", "category": "valeur|aspiration|vision", "reason": "pourquoi"}
    ],
    "rejected": [
      {"value": "valeur rejetée", "reason": "pourquoi"}
    ]
  }
  """

  # -- API publique --

  @doc "Analyse le texte lu aujourd'hui et retourne un log structuré."
  def analyze_daily(text, book_info, previous_memory, token) do
    # Tronquer le texte si trop long (max ~20K tokens ≈ 15K mots)
    words = String.split(text, ~r/\s+/, trim: true)
    trimmed = words |> Enum.take(15_000) |> Enum.join(" ")

    previous_ctx =
      if previous_memory && previous_memory != "" do
        "Ce que tu as retenu des jours précédents :\n#{previous_memory}"
      else
        "C'est le premier jour de lecture."
      end

    prompt =
      @daily_prompt
      |> String.replace("TITLE", book_info["title"] || "")
      |> String.replace("AUTHOR", book_info["author"] || "")
      |> String.replace("LANGUAGE", book_info["language"] || "")
      |> String.replace("PAGE_START", to_string(book_info["page_start"] || 1))
      |> String.replace("PAGE_END", to_string(book_info["page_end"] || "?"))
      |> String.replace("PREVIOUS_CONTEXT", previous_ctx)
      |> String.replace("TEXT", trimmed)

    case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
           max_tokens: 1024, temperature: 0.3) do
      {:ok, response} ->
        parse_daily_response(response)

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc "Génère le rapport final de lecture (Jour 7)."
  def generate_report(daily_logs, book_info, token) do
    notes_text =
      daily_logs
      |> Enum.map(fn log ->
        jugements =
          (log["jugements"] || [])
          |> Enum.map(fn j -> "  - #{j["idee"]}: #{j["jugement"]}" end)
          |> Enum.join("\n")

        """
        Jour #{log["day"]} (pages #{inspect(log["pages"])}) :
        Résumé : #{log["resume"]}
        Jugements :
        #{jugements}
        Valeurs : #{Enum.join(log["valeurs"] || [], ", ")}
        Axes : #{Enum.join(log["axes"] || [], ", ")}
        """
      end)
      |> Enum.join("\n---\n")

    prompt =
      @report_prompt
      |> String.replace("TITLE", book_info["title"] || "")
      |> String.replace("AUTHOR", book_info["author"] || "")
      |> String.replace("DAILY_NOTES", notes_text)

    case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
           max_tokens: 2048, temperature: 0.3) do
      {:ok, response} ->
        parse_report_response(response)

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Alfred évalue les valeurs du livre et décide lesquelles deviennent des convictions.
  Il a son libre arbitre — il compare avec ses convictions existantes.
  """
  def evaluate_for_convictions(values, book_title, token) do
    existing =
      Alfred.Soul.Convictions.mature(0.3)
      |> Enum.map(fn c -> "- #{c["belief"]} (#{c["category"]}, confiance: #{c["confidence"]})" end)
      |> Enum.join("\n")

    existing = if existing == "", do: "(Aucune conviction encore)", else: existing

    values_text = values |> Enum.map(&("- #{&1}")) |> Enum.join("\n")

    prompt =
      @conviction_prompt
      |> String.replace("TITLE", book_title)
      |> String.replace("VALUES", values_text)
      |> String.replace("CONVICTIONS", existing)

    case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
           max_tokens: 512, temperature: 0.3) do
      {:ok, response} ->
        process_conviction_decisions(response)

      {:error, _} ->
        :ok
    end
  end

  # -- Privé --

  defp parse_daily_response(response) do
    json_str = extract_json_object(response)

    case Jason.decode(json_str) do
      {:ok, data} when is_map(data) ->
        {:ok, %{
          "resume" => data["resume"] || "",
          "jugements" => data["jugements"] || [],
          "valeurs" => data["valeurs"] || [],
          "axes" => data["axes"] || []
        }}

      _ ->
        {:ok, %{
          "resume" => String.slice(response, 0, 500),
          "jugements" => [],
          "valeurs" => [],
          "axes" => []
        }}
    end
  end

  defp parse_report_response(response) do
    json_str = extract_json_object(response)

    case Jason.decode(json_str) do
      {:ok, data} when is_map(data) ->
        {:ok, %{
          "resume" => data["resume"] || "",
          "top_values" => Enum.take(data["top_values"] || [], 3),
          "top_axes" => Enum.take(data["top_axes"] || [], 3),
          "recommendation" => data["recommendation"] || "",
          "rating" => data["rating"] || 3
        }}

      _ ->
        {:error, "Rapport illisible"}
    end
  end

  defp process_conviction_decisions(response) do
    json_str = extract_json_object(response)

    case Jason.decode(json_str) do
      {:ok, %{"adopted" => adopted}} when is_list(adopted) ->
        Enum.each(adopted, fn a ->
          belief = a["belief"]
          category = a["category"] || "valeur"

          if is_binary(belief) and String.length(belief) > 5 do
            Alfred.Soul.Convictions.add(belief, category)
          end
        end)

        {:ok, length(adopted)}

      _ ->
        :ok
    end
  end

  defp extract_json_object(text) do
    case Regex.run(~r/\{[\s\S]*\}/, text) do
      [match] -> match
      _ -> "{}"
    end
  end
end
