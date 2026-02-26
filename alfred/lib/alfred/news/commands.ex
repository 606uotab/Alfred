defmodule Alfred.News.Commands do
  @moduledoc """
  Commandes CLI pour le briefing news.
  """

  alias Alfred.Butler
  alias Alfred.News

  def handle([]) do
    case News.load_latest() do
      nil ->
        Butler.say("Aucun briefing disponible, Monsieur. Tapez 'alfred news refresh' pour en générer un.")

      data ->
        date = data["date"] || "?"
        count = data["article_count"] || 0
        briefing = data["briefing"] || "(vide)"

        Butler.say("Briefing du #{date} (#{count} articles analysés) :\n")
        IO.puts(briefing)
        IO.puts("")
    end
  end

  def handle(["refresh"]) do
    Butler.say("Je prépare votre briefing, Monsieur... Un instant.")

    case News.briefing() do
      {:ok, text} ->
        IO.puts("")
        IO.puts(text)
        IO.puts("")
        Butler.say("Voilà qui est fait, Monsieur.")

      {:error, reason} ->
        Butler.say("Je suis navré, Monsieur. Erreur : #{inspect(reason)}")
    end
  end

  def handle(["list"]) do
    files = News.list_briefings()

    if files == [] do
      Butler.say("Aucun briefing archivé, Monsieur.")
    else
      Butler.say("Briefings disponibles :\n")

      Enum.each(files, fn f ->
        date = String.replace(f, ".json", "")
        IO.puts("  #{date}")
      end)

      IO.puts("")
    end
  end

  def handle(["show", date]) do
    file = "news/#{date}.json"

    case Alfred.Storage.Local.read(file) do
      data when is_map(data) ->
        count = data["article_count"] || 0
        briefing = data["briefing"] || "(vide)"

        Butler.say("Briefing du #{date} (#{count} articles) :\n")
        IO.puts(briefing)
        IO.puts("")

      _ ->
        Butler.say("Aucun briefing trouvé pour le #{date}, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes news sont :\n")

    IO.puts("""
      alfred news                  Dernier briefing
      alfred news refresh          Générer un nouveau briefing
      alfred news list             Briefings archivés
      alfred news show YYYY-MM-DD  Briefing d'une date
    """)
  end
end
