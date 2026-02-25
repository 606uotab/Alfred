defmodule Alfred.Simplex.CommandParser do
  @moduledoc """
  Parseur de commandes SimpleX — extrait /commande [args] des messages.
  """

  @doc """
  Parse un message texte pour détecter une commande.
  Retourne {:command, cmd, args} ou :not_command.
  """
  def parse(text) do
    text = String.trim(text)

    if String.starts_with?(text, "/") do
      parts = text |> String.slice(1..-1//1) |> String.split()

      case parts do
        [cmd | args] -> {:command, String.downcase(cmd), args}
        _ -> :not_command
      end
    else
      :not_command
    end
  end
end
