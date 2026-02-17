defmodule Alfred.Vault.Port do
  @moduledoc """
  Communication avec le binaire Zig alfred-vault via Erlang Port.
  Le binaire prend un répertoire de vaults et gère 3 coffres (creator, users, culture).
  Chaque session ouvre un process, envoie une séquence de commandes, puis ferme.
  """

  @vault_names ~w(creator users culture)

  def vault_dir do
    Path.join(System.user_home!(), ".alfred/vaults")
  end

  def legacy_vault_path do
    Path.join(System.user_home!(), ".alfred/vault.enc")
  end

  def vault_names, do: @vault_names

  # -- Multi-vault commands --

  @doc """
  Initialize all 3 vaults with master and admin passwords.
  """
  def init_all(master_password, admin_password) do
    send_command(%{cmd: "init_all", password: master_password, value: admin_password})
  end

  @doc """
  Get status of all 3 vaults.
  """
  def vault_status do
    send_command(%{cmd: "status"})
  end

  # -- Per-vault commands --

  @doc """
  Send a single command (no unlock needed — for init, status).
  """
  def send_command(command) when is_map(command) do
    send_commands([command])
  end

  @doc """
  Send a command targeting a specific vault with prior unlock.
  """
  def send_with_unlock(vault_name, password, command)
      when is_binary(vault_name) and is_binary(password) and is_map(command) do
    send_commands([
      %{cmd: "unlock", vault: vault_name, password: password},
      Map.put(command, :vault, vault_name)
    ])
  end

  @doc """
  Unlock all vaults with master password, then send a command to a specific vault.
  """
  def send_with_master_unlock(master_password, vault_name, command)
      when is_binary(master_password) and is_binary(vault_name) and is_map(command) do
    send_commands([
      %{cmd: "unlock_all", password: master_password},
      Map.put(command, :vault, vault_name)
    ])
  end

  @doc """
  Unlock all vaults then send multiple commands.
  Returns the result of the LAST command.
  """
  def send_with_master_unlock_multi(master_password, commands)
      when is_binary(master_password) and is_list(commands) do
    send_commands([%{cmd: "unlock_all", password: master_password} | commands])
  end

  @doc """
  Send a sequence of commands in one session.
  Returns the result of the LAST command.
  If any command before the last fails, returns that error immediately.
  """
  def send_commands(commands) when is_list(commands) and length(commands) > 0 do
    binary = vault_binary_path()

    unless File.exists?(binary) do
      {:error, "Vault binary not found. Run: cd native/vault && zig build"}
    else
      port =
        Port.open({:spawn_executable, binary}, [
          :binary,
          :exit_status,
          args: [vault_dir()],
          line: 65536
        ])

      result = send_and_receive(port, commands)

      send(port, {self(), :close})
      flush_port(port)

      result
    end
  end

  defp send_and_receive(_port, []), do: {:ok, %{}}

  defp send_and_receive(port, [command]) do
    json = Jason.encode!(command)
    Port.command(port, json <> "\n")
    receive_response(port)
  end

  defp send_and_receive(port, [command | rest]) do
    json = Jason.encode!(command)
    Port.command(port, json <> "\n")

    case receive_response(port) do
      {:ok, _} -> send_and_receive(port, rest)
      {:error, _} = err -> err
    end
  end

  defp receive_response(port) do
    receive do
      {^port, {:data, {:eol, line}}} ->
        case Jason.decode(line) do
          {:ok, %{"status" => "ok"} = resp} -> {:ok, resp}
          {:ok, %{"status" => "error", "message" => msg}} -> {:error, msg}
          {:error, _} -> {:error, "Invalid response from vault"}
        end

      {^port, {:exit_status, _status}} ->
        {:error, "Vault process exited unexpectedly"}
    after
      30_000 ->
        {:error, "Vault operation timed out"}
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
    project_path = Path.expand("native/vault/zig-out/bin/alfred-vault", project_root())

    if File.exists?(project_path) do
      project_path
    else
      Path.expand("native/vault/zig-out/bin/alfred-vault", File.cwd!())
    end
  end

  defp project_root do
    Path.expand("../..", __DIR__)
  end
end
