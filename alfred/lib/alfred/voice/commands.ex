defmodule Alfred.Voice.Commands do
  @moduledoc """
  Commandes CLI pour la voix d'Alfred.
  """

  alias Alfred.Butler
  alias Alfred.Voice

  def handle([]) do
    status = Voice.status()
    Butler.say("Monsieur, voici l'état de ma voix :\n")
    IO.puts("  Activée    : #{if status.enabled, do: "oui", else: "non"}")
    IO.puts("  Disponible : #{if status.available, do: "oui (espeak-ng)", else: "non"}")
    IO.puts("  Langue     : #{status.lang}")
    IO.puts("  Débit      : #{status.rate} mots/min")
    IO.puts("")

    unless status.available do
      IO.puts("  Pour installer : apt install espeak-ng")
      IO.puts("")
    end
  end

  def handle(["on"]) do
    if Voice.available?() do
      Voice.enable()
      Butler.say("Ma voix est activée, Monsieur.")
      Voice.speak("Bonjour Monsieur, ma voix est activée.")
    else
      Butler.say("Je suis navré Monsieur, espeak-ng n'est pas installé.")
      IO.puts("  Pour installer : apt install espeak-ng")
    end
  end

  def handle(["off"]) do
    Voice.disable()
    Butler.say("Ma voix est désactivée, Monsieur.")
  end

  def handle(["say" | words]) when words != [] do
    text = Enum.join(words, " ")

    if Voice.available?() do
      # Force speak even if disabled (test mode)
      was_enabled = Voice.enabled?()
      unless was_enabled, do: Voice.enable()
      Voice.speak(text)
      unless was_enabled, do: Voice.disable()
      Butler.say("Je dis : \"#{text}\"")
    else
      Butler.say("Je suis navré Monsieur, espeak-ng n'est pas installé.")
    end
  end

  def handle(["install"]) do
    Butler.say("Monsieur, pour installer ma voix :\n")
    IO.puts("  sudo apt install espeak-ng")
    IO.puts("")
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes vocales sont :\n")

    IO.puts("""
      alfred voice               État de la voix
      alfred voice on             Activer la voix
      alfred voice off            Désactiver la voix
      alfred voice say <texte>    Tester la voix
      alfred voice install        Comment installer espeak-ng
    """)
  end
end
