defmodule Alfred.Vault.Commands do
  @moduledoc """
  Commandes vault pour le CLI d'Alfred — gestion des 3 coffres-forts chiffrés.
  """

  alias Alfred.Vault.Port
  alias Alfred.Butler

  @valid_vaults ~w(creator users culture)

  # -- Setup: create all 3 vaults --

  def handle(["setup"]) do
    Butler.say("Création des 3 coffres-forts, Monsieur.\n")

    master_pw = prompt_password("Mot de passe maître (déverrouille tout) : ")
    master_confirm = prompt_password("Confirmez le mot de passe maître : ")

    if master_pw != master_confirm do
      Butler.say("Les mots de passe maître ne correspondent pas, Monsieur.")
    else
      if String.length(master_pw) < 4 do
        Butler.say("Monsieur, je recommande un mot de passe d'au moins 4 caractères.")
      else
        admin_pw = prompt_password("Mot de passe admin (users + culture) : ")
        admin_confirm = prompt_password("Confirmez le mot de passe admin : ")

        if admin_pw != admin_confirm do
          Butler.say("Les mots de passe admin ne correspondent pas, Monsieur.")
        else
          if String.length(admin_pw) < 4 do
            Butler.say("Monsieur, le mot de passe admin doit faire au moins 4 caractères.")
          else
            case Port.init_all(master_pw, admin_pw) do
              {:ok, _} ->
                Butler.say("Très bien Monsieur. Les 3 coffres-forts sont créés et sécurisés :")
                IO.puts("  🔒 creator.enc  — Âme, secrets du créateur (Maître seul)")
                IO.puts("  🔒 users.enc    — Profils utilisateurs (Admin + Maître)")
                IO.puts("  🔒 culture.enc  — Connaissances avec sources (Admin + Maître)\n")
                Butler.say("Gardez précieusement vos mots de passe.")

              {:error, "Vault already exists"} ->
                Butler.say("Monsieur, les coffres-forts existent déjà. Utilisez 'alfred vault status' pour vérifier.")

              {:error, msg} ->
                Butler.say("Je suis navré Monsieur, une erreur est survenue : #{msg}")
            end
          end
        end
      end
    end
  end

  # -- Status --

  def handle(["status"]) do
    case Port.vault_status() do
      {:ok, %{"vaults" => vaults}} ->
        Butler.say("État des coffres-forts, Monsieur :\n")

        Enum.each(["creator", "users", "culture"], fn name ->
          info = vaults[name]
          icon = if info["exists"], do: "🔒", else: "○"
          status = if info["exists"], do: "présent", else: "absent"
          IO.puts("  #{icon} #{name}.enc — #{status}")
        end)

        IO.puts("")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  # -- Store --

  def handle(["store", vault_name, key | rest]) when vault_name in @valid_vaults do
    value =
      if rest == [] do
        prompt_secret("Valeur pour '#{key}' : ")
      else
        Enum.join(rest, " ")
      end

    password = prompt_password("Mot de passe : ")

    case Port.send_with_unlock(vault_name, password, %{cmd: "store", key: key, value: value}) do
      {:ok, _} ->
        Butler.say("Secret '#{key}' enregistré dans #{vault_name}, Monsieur.")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  # -- Get --

  def handle(["get", vault_name, key]) when vault_name in @valid_vaults do
    password = prompt_password("Mot de passe : ")

    case Port.send_with_unlock(vault_name, password, %{cmd: "get", key: key}) do
      {:ok, %{"value" => value}} ->
        Butler.say("Monsieur, voici la valeur de '#{key}' (#{vault_name}) :")
        IO.puts("\n  #{value}\n")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, "Key not found"} ->
        Butler.say("Monsieur, la clé '#{key}' n'existe pas dans #{vault_name}.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  # -- List --

  def handle(["list", vault_name]) when vault_name in @valid_vaults do
    password = prompt_password("Mot de passe : ")

    case Port.send_with_unlock(vault_name, password, %{cmd: "list"}) do
      {:ok, %{"keys" => keys}} ->
        if keys == [] do
          Butler.say("Le coffre #{vault_name} est vide, Monsieur.")
        else
          Butler.say("Clés dans #{vault_name} :\n")

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

  # -- Delete --

  def handle(["delete", vault_name, key]) when vault_name in @valid_vaults do
    password = prompt_password("Mot de passe : ")

    case Port.send_with_unlock(vault_name, password, %{cmd: "delete", key: key}) do
      {:ok, _} ->
        Butler.say("Secret '#{key}' supprimé de #{vault_name}, Monsieur.")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, "Key not found"} ->
        Butler.say("Monsieur, la clé '#{key}' n'existe pas dans #{vault_name}.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  # -- Notes (per-vault) --

  def handle(["note", vault_name | rest]) when vault_name in @valid_vaults and rest != [] do
    text = Enum.join(rest, " ")
    password = prompt_password("Mot de passe : ")

    case Port.send_with_unlock(vault_name, password, %{cmd: "note_add", text: text}) do
      {:ok, %{"id" => id}} ->
        Butler.say("Note confidentielle ##{id} enregistrée dans #{vault_name}, Monsieur.")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["notes", vault_name]) when vault_name in @valid_vaults do
    password = prompt_password("Mot de passe : ")

    case Port.send_with_unlock(vault_name, password, %{cmd: "notes"}) do
      {:ok, %{"notes" => notes}} ->
        if notes == [] do
          Butler.say("Aucune note dans #{vault_name}, Monsieur.")
        else
          Butler.say("Notes dans #{vault_name} :\n")

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

  # -- Destroy --

  def handle(["destroy"]) do
    vault_dir = Port.vault_dir()

    unless File.dir?(vault_dir) do
      Butler.say("Monsieur, il n'y a aucun coffre-fort à détruire.")
    else
      confirm =
        IO.gets("Détruire TOUS les coffres-forts et leurs secrets ? (oui/non) : ")
        |> String.trim()

      if confirm == "oui" do
        File.rm_rf!(vault_dir)
        Butler.say("Les coffres-forts ont été détruits, Monsieur. Tous les secrets sont perdus.")
      else
        Butler.say("Destruction annulée, Monsieur.")
      end
    end
  end

  # -- Migrate --

  def handle(["migrate"]) do
    Alfred.Vault.Migration.run()
  end

  # -- Legacy fallback: old-style commands without vault name --

  def handle(["init"]) do
    Butler.say("Monsieur, le format a changé. Utilisez : alfred vault setup")
  end

  def handle(["store", key | rest]) do
    Butler.say("Monsieur, précisez le coffre : alfred vault store <creator|users|culture> #{key} #{Enum.join(rest, " ")}")
  end

  def handle(["get", key]) do
    Butler.say("Monsieur, précisez le coffre : alfred vault get <creator|users|culture> #{key}")
  end

  def handle(["list"]) do
    Butler.say("Monsieur, précisez le coffre : alfred vault list <creator|users|culture>")
  end

  def handle(["delete", key]) do
    Butler.say("Monsieur, précisez le coffre : alfred vault delete <creator|users|culture> #{key}")
  end

  # -- Help --

  def handle(_) do
    Butler.say("Monsieur, les commandes du coffre-fort sont :\n")

    IO.puts("""
      alfred vault setup                        Créer les 3 coffres-forts
      alfred vault status                       État des coffres-forts
      alfred vault store <coffre> <clé> [val]   Stocker un secret
      alfred vault get <coffre> <clé>           Récupérer un secret
      alfred vault list <coffre>                Lister les clés
      alfred vault delete <coffre> <clé>        Supprimer un secret
      alfred vault note <coffre> <texte>        Ajouter une note chiffrée
      alfred vault notes <coffre>               Lister les notes chiffrées
      alfred vault destroy                      Détruire tous les coffres
      alfred vault migrate                      Migrer depuis l'ancien format

      Coffres : creator, users, culture
    """)
  end

  # -- Helpers --

  defp prompt_password(prompt), do: Alfred.Input.prompt_password(prompt)
  defp prompt_secret(prompt), do: Alfred.Input.prompt_secret(prompt)

  defp format_timestamp(ts) when is_integer(ts) do
    case DateTime.from_unix(ts) do
      {:ok, dt} -> Calendar.strftime(dt, "%Y-%m-%d %H:%M")
      _ -> "#{ts}"
    end
  end

  defp format_timestamp(ts), do: "#{ts}"
end
