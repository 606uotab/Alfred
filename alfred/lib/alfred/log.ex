defmodule Alfred.Log do
  @moduledoc """
  Logger minimaliste pour le mode daemon — écrit dans ~/.alfred/alfred.log
  au lieu de stdout. Zéro spam dans le terminal.
  """

  @log_file Path.expand("~/.alfred/alfred.log")

  @doc "Log un message info. Silencieux sur stdout."
  def info(tag, message) do
    write("[#{tag}] #{message}")
  end

  @doc "Log un message d'erreur."
  def error(tag, message) do
    write("[#{tag}] ERROR #{message}")
  end

  @doc "Log un message debug (seulement si debug activé)."
  def debug(tag, message) do
    if System.get_env("ALFRED_DEBUG") do
      write("[#{tag}] DEBUG #{message}")
    end
  end

  defp write(line) do
    timestamp = DateTime.utc_now() |> Calendar.strftime("%Y-%m-%d %H:%M:%S")
    File.mkdir_p!(Path.dirname(@log_file))
    File.write(@log_file, "#{timestamp} #{line}\n", [:append])
  rescue
    _ -> :ok
  end
end
