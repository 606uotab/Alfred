defmodule Alfred.Vault.Migration do
  @moduledoc """
  Migration de l'ancien vault unique vers le système multi-vault.
  Détecte ~/.alfred/vault.enc et transfère les données dans les 3 nouveaux coffres.
  """

  alias Alfred.Vault.Port, as: VaultPort
  alias Alfred.Butler

  @doc """
  Check if migration is needed (old vault.enc exists, new vaults dir doesn't).
  """
  def needed? do
    File.exists?(VaultPort.legacy_vault_path()) and not File.dir?(VaultPort.vault_dir())
  end

  @doc """
  Run the migration interactively.
  """
  def run do
    legacy_path = VaultPort.legacy_vault_path()

    unless File.exists?(legacy_path) do
      Butler.say("Monsieur, aucun ancien coffre-fort détecté. Rien à migrer.")
    else
      if File.dir?(VaultPort.vault_dir()) do
        Butler.say("Monsieur, les nouveaux coffres existent déjà. Migration annulée.")
      else
        Butler.say("Migration de l'ancien coffre-fort vers le nouveau système multi-vault.\n")
        do_migration(legacy_path)
      end
    end
  end

  @doc """
  Check and suggest migration at startup.
  """
  def check_and_suggest do
    if needed?() do
      Butler.say("Monsieur, j'ai détecté un ancien coffre-fort.")
      Butler.say("Utilisez 'alfred vault migrate' pour migrer vers le nouveau système multi-vault.\n")
    end
  end

  # -- Migration logic --

  defp do_migration(legacy_path) do
    old_password = Alfred.Input.prompt_password("Mot de passe de l'ancien coffre-fort : ")

    case read_legacy_vault(legacy_path, old_password) do
      {:error, reason} ->
        Butler.say("Erreur lors de la lecture de l'ancien coffre-fort : #{reason}")

      {:ok, secrets, notes} ->
        Butler.say("Données récupérées : #{length(secrets)} secrets, #{length(notes)} notes.\n")

        master_pw = Alfred.Input.prompt_password("Nouveau mot de passe maître : ")
        master_confirm = Alfred.Input.prompt_password("Confirmez le mot de passe maître : ")

        if master_pw != master_confirm do
          Butler.say("Les mots de passe ne correspondent pas.")
        else
          admin_pw = Alfred.Input.prompt_password("Nouveau mot de passe admin : ")
          admin_confirm = Alfred.Input.prompt_password("Confirmez le mot de passe admin : ")

          if admin_pw != admin_confirm do
            Butler.say("Les mots de passe admin ne correspondent pas.")
          else
            case VaultPort.init_all(master_pw, admin_pw) do
              {:ok, _} ->
                transfer_data(master_pw, secrets, notes)
                backup_legacy(legacy_path)
                Butler.say("Migration terminée avec succès, Monsieur !")

              {:error, reason} ->
                Butler.say("Erreur lors de la création des nouveaux coffres : #{reason}")
            end
          end
        end
    end
  end

  defp read_legacy_vault(legacy_path, password) do
    binary = vault_binary_path()

    unless File.exists?(binary) do
      {:error, "Vault binary not found"}
    else
      # The binary now expects a dir, so create a temp dir with the old vault as creator.enc
      tmp_dir = Path.join(System.tmp_dir!(), "alfred_migration_#{System.system_time(:second)}")
      File.mkdir_p!(tmp_dir)
      File.cp!(legacy_path, Path.join(tmp_dir, "creator.enc"))

      port =
        Elixir.Port.open({:spawn_executable, binary}, [
          :binary,
          :exit_status,
          args: [tmp_dir],
          line: 65536
        ])

      result = read_all_data(port, password)

      send(port, {self(), :close})
      flush_port(port)
      File.rm_rf!(tmp_dir)

      result
    end
  end

  defp read_all_data(port, password) do
    send_cmd(port, %{cmd: "unlock", vault: "creator", password: password})

    case receive_response(port) do
      {:error, reason} ->
        {:error, reason}

      {:ok, _} ->
        send_cmd(port, %{cmd: "list", vault: "creator"})

        case receive_response(port) do
          {:ok, %{"keys" => keys}} ->
            secrets =
              Enum.reduce(keys, [], fn key, acc ->
                send_cmd(port, %{cmd: "get", vault: "creator", key: key})

                case receive_response(port) do
                  {:ok, %{"value" => value}} -> [{key, value} | acc]
                  _ -> acc
                end
              end)

            send_cmd(port, %{cmd: "notes", vault: "creator"})

            notes =
              case receive_response(port) do
                {:ok, %{"notes" => n}} -> n
                _ -> []
              end

            {:ok, Enum.reverse(secrets), notes}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp transfer_data(master_pw, secrets, notes) do
    Butler.say("Transfert des données dans le coffre creator...")

    Enum.each(secrets, fn {key, value} ->
      case VaultPort.send_with_master_unlock(master_pw, "creator", %{
             cmd: "store",
             key: key,
             value: value
           }) do
        {:ok, _} ->
          IO.puts("  ✓ Secret '#{key}' transféré")

        {:error, reason} ->
          IO.puts("  ✗ Erreur pour '#{key}' : #{reason}")
      end
    end)

    Enum.each(notes, fn note ->
      text = note["text"] || ""

      case VaultPort.send_with_master_unlock(master_pw, "creator", %{
             cmd: "note_add",
             text: text
           }) do
        {:ok, _} ->
          IO.puts("  ✓ Note transférée")

        {:error, reason} ->
          IO.puts("  ✗ Erreur note : #{reason}")
      end
    end)

    IO.puts("")
  end

  defp backup_legacy(legacy_path) do
    backup_path = legacy_path <> ".backup"
    File.rename!(legacy_path, backup_path)
    Butler.say("Ancien coffre-fort sauvegardé : #{backup_path}")
  end

  defp send_cmd(port, command) do
    json = Jason.encode!(command)
    Elixir.Port.command(port, json <> "\n")
  end

  defp receive_response(port) do
    receive do
      {^port, {:data, {:eol, line}}} ->
        case Jason.decode(line) do
          {:ok, %{"status" => "ok"} = resp} -> {:ok, resp}
          {:ok, %{"status" => "error", "message" => msg}} -> {:error, msg}
          _ -> {:error, "Invalid response"}
        end

      {^port, {:exit_status, _}} ->
        {:error, "Process exited"}
    after
      30_000 -> {:error, "Timeout"}
    end
  end

  defp flush_port(port) do
    receive do
      {^port, _} -> flush_port(port)
    after
      100 -> :ok
    end
  end

  defp vault_binary_path do
    project_path =
      Path.expand("native/vault/zig-out/bin/alfred-vault", Path.expand("../..", __DIR__))

    if File.exists?(project_path) do
      project_path
    else
      Path.expand("native/vault/zig-out/bin/alfred-vault", File.cwd!())
    end
  end
end
