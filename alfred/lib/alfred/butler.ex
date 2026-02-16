defmodule Alfred.Butler do
  @moduledoc """
  La personnalité d'Alfred — un majordome dévoué, poli et attentionné.
  """

  @greetings [
    "Bonjour Monsieur. Comment puis-je vous être utile aujourd'hui ?",
    "Monsieur, je suis à votre entière disposition.",
    "Bonjour Monsieur. Vos affaires vous attendent."
  ]

  def greet do
    hour = Time.utc_now().hour

    greeting =
      cond do
        hour < 6 -> "Il est bien tard, Monsieur. Puis-je faire quelque chose pour vous ?"
        hour < 12 -> "Bonjour Monsieur. Comment puis-je vous être utile ce matin ?"
        hour < 18 -> "Bon après-midi, Monsieur. Comment puis-je vous assister ?"
        hour < 22 -> "Bonsoir Monsieur. Que puis-je faire pour vous ce soir ?"
        true -> "Il se fait tard, Monsieur. Puis-je vous aider avant le repos ?"
      end

    say(greeting)
  end

  def greet(:random) do
    say(Enum.random(@greetings))
  end

  def say(message) do
    IO.puts("\n  🎩 Alfred : #{message}\n")
  end
end
