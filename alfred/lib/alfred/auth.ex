defmodule Alfred.Auth do
  @moduledoc """
  Authentification et rôles — gère l'accès aux 3 coffres d'Alfred.

  Rôles :
  - :master → accès complet (creator + users + culture)
  - :admin  → accès users + culture
  - :user   → lecture culture via Alfred uniquement
  """

  alias Alfred.Vault.Port

  @access_matrix %{
    master: %{creator: :write, users: :write, culture: :write},
    admin: %{creator: :none, users: :write, culture: :write},
    user: %{creator: :none, users: :none, culture: :read}
  }

  @doc """
  Authenticate as master — unlocks all 3 vaults.
  Returns {:ok, :master} or {:error, reason}.
  """
  def authenticate_master(password) do
    case Port.send_commands([%{cmd: "unlock_all", password: password}]) do
      {:ok, _} -> {:ok, :master}
      {:error, _} = err -> err
    end
  end

  @doc """
  Authenticate as admin — retrieves admin password from creator vault,
  then unlocks users + culture.
  Returns {:ok, :admin} or {:error, reason}.
  """
  def authenticate_admin(password) do
    # Try to unlock users and culture with the given password
    case Port.send_commands([
           %{cmd: "unlock", vault: "users", password: password},
           %{cmd: "unlock", vault: "culture", password: password}
         ]) do
      {:ok, _} -> {:ok, :admin}
      {:error, _} = err -> err
    end
  end

  @doc """
  Identify a user by name — no vault access, just a name for conversation.
  Returns {:ok, :user, name}.
  """
  def identify_user(name) when is_binary(name) and byte_size(name) > 0 do
    {:ok, :user, name}
  end

  @doc """
  Detect role from password — tries master first, then admin.
  Returns {:ok, role} or {:error, reason}.
  """
  def detect_role(password) do
    case authenticate_master(password) do
      {:ok, :master} ->
        {:ok, :master}

      {:error, _} ->
        case authenticate_admin(password) do
          {:ok, :admin} -> {:ok, :admin}
          {:error, _} -> {:error, "Wrong password"}
        end
    end
  end

  @doc """
  Check if a role can access a vault (read or write).
  """
  def can_access?(role, vault_name) when is_atom(role) and is_binary(vault_name) do
    vault = String.to_existing_atom(vault_name)
    access = get_in(@access_matrix, [role, vault])
    access in [:read, :write]
  rescue
    ArgumentError -> false
  end

  @doc """
  Check if a role can write to a vault.
  """
  def can_write?(role, vault_name) when is_atom(role) and is_binary(vault_name) do
    vault = String.to_existing_atom(vault_name)
    get_in(@access_matrix, [role, vault]) == :write
  rescue
    ArgumentError -> false
  end

  @doc """
  Returns the access level for a role on a vault.
  """
  def access_level(role, vault_name) when is_atom(role) and is_binary(vault_name) do
    vault = String.to_existing_atom(vault_name)
    get_in(@access_matrix, [role, vault]) || :none
  rescue
    ArgumentError -> :none
  end
end
