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

    parts = [@personality]

    parts =
      if soul do
        parts ++ ["\n#{soul}"]
      else
        parts
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
end
