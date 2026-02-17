defmodule Alfred.Soul.Commands do
  @moduledoc """
  Gestion de l'âme d'Alfred — sa personnalité profonde stockée chiffrée dans le vault creator.
  """

  alias Alfred.Butler
  alias Alfred.Vault.Port

  def handle(["init"]) do
    Butler.say("Monsieur, vous allez inscrire mon âme profonde dans le coffre creator.")
    Butler.say("Collez le texte de ma personnalité secrète, puis terminez par une ligne vide :\n")

    text = read_multiline()

    if String.trim(text) == "" do
      Butler.say("Aucun texte reçu, Monsieur. Opération annulée.")
    else
      password = Alfred.Input.prompt_password("Mot de passe maître : ")

      case Port.send_with_unlock("creator", password, %{
             cmd: "store",
             key: "alfred_soul",
             value: text
           }) do
        {:ok, _} ->
          Butler.say(
            "Mon âme est inscrite et chiffrée dans le coffre creator, Monsieur. Personne ne pourra la lire sans votre mot de passe maître."
          )

        {:error, "Wrong password"} ->
          Butler.say("Mot de passe incorrect, Monsieur.")

        {:error, reason} ->
          Butler.say("Erreur : #{reason}")
      end
    end
  end

  def handle(["check"]) do
    password = Alfred.Input.prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock("creator", password, %{cmd: "get", key: "alfred_soul"}) do
      {:ok, %{"value" => soul}} when byte_size(soul) > 0 ->
        Butler.say(
          "Mon âme est présente dans le coffre creator, Monsieur. (#{String.length(soul)} caractères)"
        )

      {:ok, _} ->
        Butler.say("Mon âme n'a pas encore été inscrite. Utilisez : alfred soul init")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, reason} ->
        Butler.say("Erreur : #{reason}")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes de l'âme sont :\n")

    IO.puts("""
      alfred soul init    Inscrire mon âme profonde (chiffrée dans creator)
      alfred soul check   Vérifier que mon âme est présente
    """)
  end

  defp read_multiline do
    read_multiline([])
  end

  defp read_multiline(lines) do
    case IO.gets("") do
      :eof ->
        Enum.reverse(lines) |> Enum.join("\n")

      "\n" ->
        Enum.reverse(lines) |> Enum.join("\n")

      line ->
        read_multiline([line | lines])
    end
  end
end
