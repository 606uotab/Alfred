defmodule Alfred.Brain.Port do
  @moduledoc """
  Communication avec le moteur Julia (Le Cerveau) via Erlang Port.
  Le script Julia lit des commandes JSON sur stdin et écrit des réponses JSON sur stdout.
  """

  @doc """
  Send a command to the Julia brain and return the parsed response.
  """
  def send_command(command) when is_map(command) do
    julia = julia_path()
    script = brain_script_path()

    cond do
      julia == nil ->
        {:error, "Julia introuvable. Installez Julia via juliaup."}

      not File.exists?(script) ->
        {:error, "Script brain introuvable: #{script}"}

      true ->
        port =
          Port.open({:spawn_executable, julia}, [
            :binary,
            :exit_status,
            args: ["--startup-file=no", script],
            line: 131_072
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
          {:error, _} -> {:error, "Réponse invalide du cerveau"}
        end

      {^port, {:exit_status, _status}} ->
        {:error, "Le processus Julia s'est arrêté de manière inattendue"}
    after
      120_000 ->
        {:error, "Opération Julia — délai dépassé"}
    end
  end

  defp flush_port(port) do
    receive do
      {^port, _} -> flush_port(port)
    after
      100 -> :ok
    end
  end

  defp julia_path do
    # Try common locations
    paths = [
      Path.expand("~/.juliaup/bin/julia"),
      "/usr/local/bin/julia",
      "/usr/bin/julia"
    ]

    Enum.find(paths, &File.exists?/1) || System.find_executable("julia")
  end

  defp brain_script_path do
    relative = "native/brain/src/main.jl"

    candidates = [
      Path.expand(relative, project_root()),
      Path.expand(relative, File.cwd!()),
      Path.expand("alfred/" <> relative, File.cwd!())
    ]

    Enum.find(candidates, &File.exists?/1)
  end

  defp project_root do
    Path.expand("../../..", __DIR__)
  end
end
