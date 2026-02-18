defmodule Alfred.Arms.Port do
  @moduledoc """
  Communication avec le programme Ada (Les Bras) via Erlang Port.
  Le binaire Ada lit des commandes JSON sur stdin et ecrit des reponses JSON sur stdout.
  """

  @doc """
  Send a command to the Ada arms binary and return the parsed response.
  """
  def send_command(command) when is_map(command) do
    binary = arms_binary_path()

    cond do
      binary == nil ->
        {:error, "Binaire Ada introuvable. Compilez : cd native/arms && make"}

      not File.exists?(binary) ->
        {:error, "Binaire Ada introuvable: #{binary}"}

      true ->
        port =
          Port.open({:spawn_executable, binary}, [
            :binary,
            :exit_status,
            line: 65_536
          ])

        json = Jason.encode!(command)
        Port.command(port, json <> "\n")
        result = receive_response(port)

        send(port, {self(), :close})
        flush_port(port)

        result
    end
  end

  defp receive_response(port) do
    receive do
      {^port, {:data, {:eol, line}}} ->
        case Jason.decode(line) do
          {:ok, %{"status" => "ok"} = resp} -> {:ok, resp}
          {:ok, %{"status" => "error", "message" => msg}} -> {:error, msg}
          {:error, _} -> {:error, "Reponse invalide des bras"}
        end

      {^port, {:exit_status, _status}} ->
        {:error, "Le processus Ada s'est arrete de maniere inattendue"}
    after
      30_000 ->
        {:error, "Operation Ada — delai depasse"}
    end
  end

  defp flush_port(port) do
    receive do
      {^port, _} -> flush_port(port)
    after
      100 -> :ok
    end
  end

  defp arms_binary_path do
    project_path = Path.expand("native/arms/bin/alfred-arms", project_root())

    if File.exists?(project_path) do
      project_path
    else
      cwd_path = Path.expand("native/arms/bin/alfred-arms", File.cwd!())

      if File.exists?(cwd_path) do
        cwd_path
      else
        nil
      end
    end
  end

  defp project_root do
    Path.expand("../..", __DIR__)
  end
end
