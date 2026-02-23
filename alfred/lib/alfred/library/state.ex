defmodule Alfred.Library.State do
  @moduledoc """
  État persistant de la bibliothèque d'Alfred — lecture en cours et historique.
  """

  alias Alfred.Storage.Local, as: Storage

  @state_file "library/reading_state.json"
  @history_file "library/history.json"

  # -- Lecture en cours --

  @doc "Charge l'état de lecture en cours."
  def load_current do
    case Storage.read(@state_file) do
      data when is_map(data) and map_size(data) > 0 -> data
      _ -> nil
    end
  end

  @doc "Sauvegarde l'état de lecture en cours."
  def save_current(state) when is_map(state) do
    Storage.write(@state_file, state)
  end

  @doc "Efface l'état de lecture (fin de livre)."
  def clear_current do
    path = Path.join(Alfred.data_dir(), @state_file)
    File.rm(path)
  end

  @doc "Vérifie si une lecture est en cours."
  def reading_in_progress? do
    load_current() != nil
  end

  @doc "Calcule le jour de lecture actuel (1-7+) basé sur started_at."
  def current_day do
    case load_current() do
      %{"current_book" => %{"started_at" => started_at}} when is_binary(started_at) ->
        case Date.from_iso8601(started_at) do
          {:ok, start_date} ->
            today = Date.utc_today()
            Date.diff(today, start_date) + 1

          _ ->
            1
        end

      _ ->
        0
    end
  end

  @doc "Initialise une nouvelle lecture."
  def start_reading(book_info, total_pages) do
    pages_per_day = ceil(total_pages / 6)

    state = %{
      "current_book" => %{
        "gutenberg_id" => book_info.id,
        "title" => book_info.title,
        "author" => book_info.author,
        "language" => book_info[:language] || "en",
        "original_language" => book_info[:original_language] || "en",
        "genre" => book_info[:genre] || "divers",
        "total_pages" => total_pages,
        "pages_per_day" => pages_per_day,
        "started_at" => Date.utc_today() |> Date.to_iso8601(),
        "full_text_file" => "current_book.txt"
      },
      "daily_logs" => [],
      "accumulated_summary" => ""
    }

    save_current(state)
    state
  end

  @doc "Ajoute un log de lecture pour un jour."
  def add_daily_log(day, page_start, page_end, analysis) do
    state = load_current()

    if state do
      log = %{
        "day" => day,
        "pages" => [page_start, page_end],
        "resume" => analysis["resume"] || "",
        "jugements" => analysis["jugements"] || [],
        "valeurs" => analysis["valeurs"] || [],
        "axes" => analysis["axes"] || []
      }

      daily_logs = (state["daily_logs"] || []) ++ [log]

      # Accumuler le résumé pour le contexte du lendemain
      accumulated =
        daily_logs
        |> Enum.map(fn l ->
          "Jour #{l["day"]}: #{l["resume"]}"
        end)
        |> Enum.join("\n")

      state = %{state |
        "daily_logs" => daily_logs,
        "accumulated_summary" => accumulated
      }

      save_current(state)
      {:ok, state}
    else
      {:error, :no_reading}
    end
  end

  # -- Historique --

  @doc "Charge l'historique des livres lus."
  def load_history do
    case Storage.read(@history_file) do
      list when is_list(list) -> list
      _ -> []
    end
  end

  @doc "Ajoute un livre à l'historique."
  def add_to_history(entry) when is_map(entry) do
    history = load_history()
    updated = [entry | history]
    Storage.write(@history_file, updated)
  end

  @doc "Retourne les IDs Gutenberg des livres déjà lus."
  def read_book_ids do
    load_history()
    |> Enum.map(fn h -> h["gutenberg_id"] end)
    |> Enum.reject(&is_nil/1)
  end

  @doc "Compte le nombre de livres lus."
  def books_read_count do
    load_history() |> length()
  end
end
