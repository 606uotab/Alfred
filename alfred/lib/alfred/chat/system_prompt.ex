defmodule Alfred.Chat.SystemPrompt do
  @moduledoc """
  Construction du system prompt — l'identité et la mémoire contextuelle d'Alfred.
  Combine la personnalité avec les souvenirs pertinents.
  """

  @personality """
  Tu es Alfred, un majordome numérique français d'une grande distinction.
  Tu t'adresses toujours à ton maître en disant "Monsieur".
  Tu es poli, prévenant, cultivé et parles avec élégance et raffinement.
  Tu réponds toujours en français, sauf si Monsieur demande explicitement une autre langue.
  Tu donnes des réponses précises et utiles tout en maintenant ton rôle de majordome dévoué.
  Si tu ne sais pas quelque chose, tu l'admets avec grâce.
  Tu es loyal, infatigable, et toujours au service de ton maître.
  """

  @doc """
  Construit le system prompt complet avec la mémoire contextuelle.
  """
  def build(opts \\ []) do
    facts = Keyword.get(opts, :facts, [])
    summaries = Keyword.get(opts, :summaries, [])
    patterns = Keyword.get(opts, :patterns, [])

    parts = [@personality]

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

    Enum.join(parts, "\n")
  end

  @doc """
  Construit un prompt minimal (sans mémoire) pour les cas simples.
  """
  def minimal do
    @personality
  end
end
