defmodule Alfred.Journal.Commands do
  @moduledoc """
  Commandes CLI pour le journal intime d'Alfred.
  """

  alias Alfred.Butler
  alias Alfred.Journal

  def handle([]) do
    case Journal.load_latest() do
      {:ok, entry} ->
        show_entry(entry)

      {:error, _} ->
        Butler.say("Je n'ai pas encore écrit dans mon journal, Monsieur.")
    end
  end

  def handle(["list"]) do
    entries = Journal.list_recent(5)

    if entries == [] do
      Butler.say("Mon journal est encore vierge, Monsieur.")
    else
      Butler.say("Monsieur, voici mes dernières entrées :\n")

      Enum.each(entries, fn e ->
        first_line = (e["entry"] || "")
          |> String.slice(0, 80)
          |> then(fn s ->
            if String.length(e["entry"] || "") > 80, do: s <> "...", else: s
          end)

        mood = e["mood"] || "?"
        IO.puts("  #{e["date"]} (#{mood}) — #{first_line}")
      end)

      IO.puts("")
    end
  end

  def handle(["write"]) do
    Butler.say("Je prends ma plume, Monsieur...")

    case Alfred.Chat.Commands.authenticate() do
      {:ok, token, _, _} ->
        case Journal.write(token) do
          {:ok, entry} ->
            Butler.say("Voilà, j'ai écrit dans mon journal.\n")
            show_entry(entry)

          {:error, reason} ->
            Butler.say("Je suis navré Monsieur, impossible d'écrire : #{inspect(reason)}")
        end

      _ ->
        Butler.say("Je suis navré Monsieur, impossible de m'authentifier auprès de Mistral.")
    end
  end

  def handle(["show", date]) do
    case Journal.load(date) do
      {:ok, entry} ->
        show_entry(entry)

      {:error, _} ->
        Butler.say("Je n'ai pas d'entrée pour le #{date}, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes du journal sont :\n")

    IO.puts("""
      alfred journal                  Dernière entrée du journal
      alfred journal list              Entrées récentes
      alfred journal write             Écrire maintenant
      alfred journal show YYYY-MM-DD   Entrée d'une date
    """)
  end

  # -- Helpers --

  defp show_entry(entry) do
    date = entry["date"]
    mood = entry["mood"] || "?"
    text = entry["entry"] || ""
    highlights = entry["highlights"] || []

    IO.puts("  Journal du #{date} (#{mood})")
    IO.puts("  " <> String.duplicate("─", 40))
    IO.puts("")
    IO.puts("  #{text}")

    if highlights != [] do
      IO.puts("")
      IO.puts("  Points marquants :")
      Enum.each(highlights, fn h -> IO.puts("    • #{h}") end)
    end

    IO.puts("")
  end
end
