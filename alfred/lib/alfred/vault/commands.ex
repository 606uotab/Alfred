defmodule Alfred.Vault.Commands do
  @moduledoc """
  Commandes vault pour le CLI d'Alfred — gestion du coffre-fort chiffré.
  """

  alias Alfred.Vault.Port
  alias Alfred.Butler

  def handle(["init"]) do
    password = prompt_password("Choisissez un mot de passe maître : ")
    confirm = prompt_password("Confirmez le mot de passe : ")

    if password != confirm do
      Butler.say("Les mots de passe ne correspondent pas, Monsieur.")
    else
      if String.length(password) < 4 do
        Butler.say("Monsieur, je recommande un mot de passe d'au moins 4 caractères.")
      else
        case Port.send_command(%{cmd: "init", password: password}) do
          {:ok, _} ->
            Butler.say("Très bien Monsieur. Votre coffre-fort est créé et sécurisé. Gardez précieusement votre mot de passe.")

          {:error, "Vault already exists"} ->
            Butler.say("Monsieur, un coffre-fort existe déjà.")

          {:error, msg} ->
            Butler.say("Je suis navré Monsieur, une erreur est survenue : #{msg}")
        end
      end
    end
  end

  def handle(["store", key | rest]) do
    value =
      if rest == [] do
        prompt_secret("Valeur pour '#{key}' : ")
      else
        Enum.join(rest, " ")
      end

    password = prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock(password, %{cmd: "store", key: key, value: value}) do
      {:ok, _} ->
        Butler.say("Secret '#{key}' enregistré dans le coffre-fort, Monsieur.")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["get", key]) do
    password = prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock(password, %{cmd: "get", key: key}) do
      {:ok, %{"value" => value}} ->
        Butler.say("Monsieur, voici la valeur de '#{key}' :")
        IO.puts("\n  #{value}\n")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, "Key not found"} ->
        Butler.say("Monsieur, la clé '#{key}' n'existe pas dans le coffre-fort.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["list"]) do
    password = prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock(password, %{cmd: "list"}) do
      {:ok, %{"keys" => keys}} ->
        if keys == [] do
          Butler.say("Le coffre-fort est vide, Monsieur.")
        else
          Butler.say("Monsieur, voici les clés de votre coffre-fort :\n")

          Enum.each(keys, fn key ->
            IO.puts("  🔑 #{key}")
          end)

          IO.puts("")
        end

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["delete", key]) do
    password = prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock(password, %{cmd: "delete", key: key}) do
      {:ok, _} ->
        Butler.say("Secret '#{key}' supprimé du coffre-fort, Monsieur.")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, "Key not found"} ->
        Butler.say("Monsieur, la clé '#{key}' n'existe pas dans le coffre-fort.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["note" | rest]) when rest != [] do
    text = Enum.join(rest, " ")
    password = prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock(password, %{cmd: "note_add", text: text}) do
      {:ok, %{"id" => id}} ->
        Butler.say("Note confidentielle ##{id} enregistrée dans le coffre-fort, Monsieur.")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["notes"]) do
    password = prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock(password, %{cmd: "notes"}) do
      {:ok, %{"notes" => notes}} ->
        if notes == [] do
          Butler.say("Aucune note confidentielle dans le coffre-fort, Monsieur.")
        else
          Butler.say("Monsieur, voici vos notes confidentielles :\n")

          Enum.each(notes, fn note ->
            date = format_timestamp(note["created_at"])
            IO.puts("  🔒 ##{note["id"]} [#{date}] #{note["text"]}")
          end)

          IO.puts("")
        end

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes du coffre-fort sont :\n")

    IO.puts("""
      alfred vault init               Créer le coffre-fort
      alfred vault store <clé> [val]   Stocker un secret
      alfred vault get <clé>           Récupérer un secret
      alfred vault list                Lister les clés
      alfred vault delete <clé>        Supprimer un secret
      alfred vault note <texte>        Ajouter une note chiffrée
      alfred vault notes               Lister les notes chiffrées
    """)
  end

  # -- Helpers --

  defp prompt_password(prompt) do
    IO.write(prompt)
    IO.gets("") |> String.trim()
  end

  defp prompt_secret(prompt) do
    IO.write(prompt)
    IO.gets("") |> String.trim()
  end

  defp format_timestamp(ts) when is_integer(ts) do
    case DateTime.from_unix(ts) do
      {:ok, dt} -> Calendar.strftime(dt, "%Y-%m-%d %H:%M")
      _ -> "#{ts}"
    end
  end

  defp format_timestamp(ts), do: "#{ts}"
end
