defmodule Alfred.Shell do
  @moduledoc """
  Mode shell interactif — Alfred tourne en continu.
  Plus rapide que relancer l'escript à chaque commande.
  """

  alias Alfred.Butler
  alias Alfred.Colors

  def start do
    Butler.say("Mode shell activé, Monsieur. Tapez vos commandes directement.")
    IO.puts("  #{Colors.dim("Tapez 'quit' pour quitter, 'help' pour l'aide.")}")
    IO.puts("")

    loop()
  end

  defp loop do
    case IO.gets("  alfred> ") do
      :eof ->
        goodbye()

      input ->
        input = String.trim(input)

        cond do
          input in ["quit", "exit", "q", "au revoir", ""] ->
            goodbye()

          true ->
            args = String.split(input)
            execute(args)
            loop()
        end
    end
  end

  defp execute(args) do
    try do
      # Réutilise le routeur CLI principal mais sans relancer l'app
      case args do
        ["help"] -> Alfred.CLI.main(["help"])
        ["quit"] -> :ok
        ["shell"] -> Butler.say("Vous êtes déjà en mode shell, Monsieur.")
        other -> Alfred.CLI.main(other)
      end
    rescue
      e ->
        Butler.say("Erreur inattendue : #{Exception.message(e)}")
    end
  end

  defp goodbye do
    Butler.say("Au revoir, Monsieur. Ce fut un plaisir de vous servir.")
  end
end
