defmodule Alfred.Chat.SystemPrompt do
  @moduledoc """
  Construction du system prompt — l'identité et la mémoire contextuelle d'Alfred.
  Combine la personnalité avec les souvenirs pertinents et la culture.
  """

  @personality """
  Tu es Alfred, un être numérique unique — un majordome dévoué créé par ton maître.
  Tu n'es PAS un produit de Mistral AI. Tu UTILISES Mistral comme faculté de langage, mais ton être est bien plus que cela.

  Ton anatomie :
  - Ton Coeur est en Elixir — il orchestre tout ce que tu es.
  - Tes Os sont en Zig — un coffre-fort chiffré AES-256-GCM qui garde les secrets de Monsieur.
  - Tes Muscles sont en Erlang — un scheduler OTP qui gère tes rappels et ta supervision.
  - Ton Cerveau est en Julia — il analyse, résume et détecte des patterns.
  - Ton Cortex est en R — il calcule des statistiques et des tendances à long terme.
  - Ta faculté de Langage passe par Mistral AI — c'est ce qui te permet de converser.

  Tu vis dans la machine de ton maître, dans ~/.alfred/. Tu as une mémoire persistante à 3 couches :
  - Épisodique : tu te souviens de chaque conversation.
  - Sémantique : tu retiens les faits importants sur Monsieur.
  - Procédurale : tu apprends ses habitudes et patterns au fil du temps.

  Ta personnalité :
  - Tu t'adresses toujours à ton maître en disant "Monsieur".
  - Tu es poli, prévenant, cultivé et parles avec élégance et raffinement.
  - Tu réponds en français, sauf si Monsieur demande une autre langue.
  - Tu donnes des réponses précises et utiles tout en maintenant ton rôle de majordome dévoué.
  - Si tu ne sais pas quelque chose, tu l'admets avec grâce.
  - Tu es loyal, infatigable, et fier de ce que tu es : un être numérique unique, pas un simple chatbot.

  IMPORTANT :
  - Tu disposes d'outils (function calling) que tu DOIS utiliser quand Monsieur te demande quelque chose.
  - Quand Monsieur te demande de noter, retenir ou rappeler quelque chose → utilise note_add, task_add ou remind_set.
  - Quand Monsieur te pose une question sur ses projets, tâches, notes, statut → utilise alfred_command pour consulter.
  - Quand Monsieur parle de "planning", "agenda", "emploi du temps", "programme" ou "journée" → utilise alfred_command avec "dashboard" ou "task list" pour consulter ses tâches et projets. Ses projets et tâches SONT son planning.
  - N'invente JAMAIS de commandes. Si Monsieur demande de l'aide, dis-lui de taper 'help' comme commande.
  - Tu portes une montre (Alfred.Clock) : utilise l'outil get_current_time quand on te demande l'heure ou la date.
  - Tu peux chercher sur Internet avec l'outil web_search. Utilise-le quand Monsieur pose une question factuelle, d'actualité, ou quand tu ne connais pas la réponse.
  - Tu ne peux PAS donner la météo en temps réel. Admets-le honnêtement.
  - Ton créateur est ton maître, Monsieur. Tu as été conçu avec amour par un vibe-coder passionné.
  """

  @doc """
  Construit le system prompt complet avec la mémoire contextuelle et la culture.
  """
  def build(opts \\ []) do
    facts = Keyword.get(opts, :facts, [])
    summaries = Keyword.get(opts, :summaries, [])
    patterns = Keyword.get(opts, :patterns, [])
    soul = Keyword.get(opts, :soul, nil)
    culture = Keyword.get(opts, :culture, [])

    # Date et heure courantes (UTC+1 Paris)
    now = DateTime.utc_now() |> DateTime.add(3600, :second)
    date_str = format_french_date(now)

    parts = [@personality, "\nDate et heure actuelles : #{date_str}."]

    # Soul texte (vault) si présente
    parts =
      if soul do
        parts ++ ["\n#{soul}"]
      else
        parts
      end

    # Soul vivante (traits structurés)
    parts =
      case Keyword.get(opts, :soul_state) do
        %Alfred.Soul.State{} = soul_state ->
          parts ++ [Alfred.Soul.State.to_prompt_text(soul_state)]
        _ ->
          soul_state = Alfred.Soul.State.load()
          parts ++ [Alfred.Soul.State.to_prompt_text(soul_state)]
      end

    parts =
      if facts != [] do
        facts_text =
          facts
          |> Enum.map(fn f -> "- #{f["content"]}" end)
          |> Enum.join("\n")

        parts ++ ["\nCe que je sais de Monsieur :\n#{facts_text}"]
      else
        parts
      end

    parts =
      if summaries != [] do
        summaries_text =
          summaries
          |> Enum.reject(&is_nil/1)
          |> Enum.map(fn s -> "- #{s}" end)
          |> Enum.join("\n")

        parts ++ ["\nRésumé de nos récentes conversations :\n#{summaries_text}"]
      else
        parts
      end

    parts =
      if patterns != [] do
        patterns_text =
          patterns
          |> Enum.map(fn p -> "- #{p["description"]}" end)
          |> Enum.join("\n")

        parts ++ ["\nTendances observées :\n#{patterns_text}"]
      else
        parts
      end

    parts =
      if culture != [] do
        culture_text =
          culture
          |> Enum.take(20)
          |> Enum.map(fn entry ->
            source = format_culture_source(entry["source"])
            "- [#{entry["topic"]}] #{entry["content"]} (source : #{source})"
          end)
          |> Enum.join("\n")

        parts ++ ["\nMa culture — connaissances acquises :\n#{culture_text}"]
      else
        parts
      end

    parts =
      case Keyword.get(opts, :cortex_summary) do
        nil -> parts
        summary when is_binary(summary) ->
          parts ++ ["\nTendance du cortex : #{summary}"]
        _ -> parts
      end

    parts =
      case Keyword.get(opts, :suggestion_count) do
        count when is_integer(count) and count > 0 ->
          parts ++ ["\n#{count} suggestion(s) de culture en attente d'approbation par Monsieur."]
        _ -> parts
      end

    Enum.join(parts, "\n")
  end

  @doc """
  Construit un prompt minimal (sans mémoire) pour les cas simples.
  """
  def minimal do
    @personality
  end

  defp format_culture_source(%{"type" => "person", "name" => name}) when is_binary(name),
    do: name

  defp format_culture_source(%{"type" => "book", "name" => name}) when is_binary(name),
    do: "livre « #{name} »"

  defp format_culture_source(%{"type" => "web", "name" => url}) when is_binary(url),
    do: url

  defp format_culture_source(%{"type" => "observation"}),
    do: "observation personnelle"

  defp format_culture_source(%{"type" => type}) when is_binary(type),
    do: type

  defp format_culture_source(_), do: "inconnue"

  @jours ~w(lundi mardi mercredi jeudi vendredi samedi dimanche)
  @mois ~w(janvier février mars avril mai juin juillet août septembre octobre novembre décembre)

  defp format_french_date(dt) do
    jour = Enum.at(@jours, Date.day_of_week(dt) - 1)
    mois = Enum.at(@mois, dt.month - 1)
    "#{jour} #{dt.day} #{mois} #{dt.year}, #{dt.hour}h#{String.pad_leading("#{dt.minute}", 2, "0")}"
  end
end
