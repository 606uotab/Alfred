defmodule Alfred.Cortex.Port do
  @moduledoc """
  Communication avec le moteur R (Le Cortex) via Erlang Port.
  Le script R lit des commandes JSON sur stdin et écrit des réponses JSON sur stdout.
  """

  @doc """
  Send a command to the R cortex and return the parsed response.
  """
  def send_command(command) when is_map(command) do
    rscript = rscript_path()
    script = cortex_script_path()

    cond do
      rscript == nil ->
        {:error, "Rscript introuvable. Installez R."}

      not File.exists?(script) ->
        {:error, "Script cortex introuvable: #{script}"}

      true ->
        port =
          Port.open({:spawn_executable, rscript}, [
            :binary,
            :exit_status,
            args: ["--vanilla", script],
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
          {:error, _} -> {:error, "Réponse invalide du cortex"}
        end

      {^port, {:exit_status, _status}} ->
        {:error, "Le processus R s'est arrêté de manière inattendue"}
    after
      60_000 ->
        {:error, "Opération R — délai dépassé"}
    end
  end

  defp flush_port(port) do
    receive do
      {^port, _} -> flush_port(port)
    after
      100 -> :ok
    end
  end

  defp rscript_path do
    paths = ["/usr/bin/Rscript", "/usr/local/bin/Rscript"]
    Enum.find(paths, &File.exists?/1) || System.find_executable("Rscript")
  end

  defp cortex_script_path do
    relative = "native/cortex/src/main.R"

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
