defmodule Alfred.Vault.Port do
  @moduledoc """
  Communication avec le binaire Zig alfred-vault via Erlang Port.
  Le binaire lit des commandes JSON sur stdin et écrit des réponses JSON sur stdout.
  Chaque session ouvre un process, envoie une séquence de commandes, puis ferme.
  """

  def vault_path do
    Path.join(System.user_home!(), ".alfred/vault.enc")
  end

  @doc """
  Send a single command (no unlock needed — for init).
  """
  def send_command(command) when is_map(command) do
    send_commands([command])
  end

  @doc """
  Send a command with prior unlock — for operations that need the vault open.
  Sends unlock first, checks it succeeds, then sends the actual command.
  Returns the result of the second command.
  """
  def send_with_unlock(password, command) when is_binary(password) and is_map(command) do
    send_commands([%{cmd: "unlock", password: password}, command])
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
          args: [vault_path()],
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
