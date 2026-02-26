defmodule Alfred.Library.Scheduler do
  @moduledoc """
  Orchestration du cycle de lecture hebdomadaire d'Alfred.
  Appelé quotidiennement par le daemon.
  Cycle : 6 jours de lecture + 1 jour de rapport.
  """

  alias Alfred.Library.{Downloader, Reader, Analyzer, State}
  alias Alfred.Simplex.Bridge

  # -- API publique --

  @doc """
  Point d'entrée quotidien — le daemon appelle tick/1 une fois par jour.
  """
  def tick(token) do
    Alfred.Log.info("Library", "Tick quotidien")

    if State.reading_in_progress?() do
      day = State.current_day()
      Alfred.Log.info("Library", "Jour #{day} de lecture")

      cond do
        day in 1..6 ->
          read_today(day, token)

        day == 7 ->
          generate_and_send_report(token)

        day > 7 ->
          finalize_book()
          pick_and_start_book(token)
      end
    else
      pick_and_start_book(token)
    end
  rescue
    e ->
      Alfred.Log.error("Library", Exception.message(e))
      :error
  end

  @doc "Force le démarrage d'un nouveau livre."
  def start_next_book(token) do
    if State.reading_in_progress?() do
      finalize_book()
    end

    pick_and_start_book(token)
  end

  @doc "Force la lecture du jour."
  def read_now(token) do
    if State.reading_in_progress?() do
      day = State.current_day()

      if day in 1..6 do
        read_today(day, token)
      else
        {:error, :not_reading_day}
      end
    else
      {:error, :no_reading}
    end
  end

  # -- Privé --

  defp pick_and_start_book(token) do
    Alfred.Log.info("Library", "Recherche d'un nouveau livre...")
    history_ids = State.read_book_ids()

    case Downloader.pick_random(history_ids) do
      {:ok, book, genre} ->
        Alfred.Log.info("Library", "Livre trouvé: #{book.title} par #{book.author}")

        # Détecter la langue originale
        {original_lang, chosen_lang} = detect_and_choose_language(book, token)
        Alfred.Log.info("Library", "Langue originale: #{original_lang}, lecture en: #{chosen_lang}")

        # Télécharger
        case Downloader.download(book.id, chosen_lang) do
          {:ok, file_path} ->
            Alfred.Log.info("Library", "Téléchargé: #{file_path}")

            # Lire et paginer
            case Reader.read(file_path) do
              {:ok, text} ->
                pages = Reader.paginate(text)
                total_pages = length(pages)
                Alfred.Log.info("Library", "#{total_pages} pages (#{Reader.word_count(text)} mots)")

                if total_pages < 5 do
                  Alfred.Log.info("Library", "Livre trop court, on en cherche un autre")
                  File.rm(file_path)
                  pick_and_start_book(token)
                else
                  book_info = Map.merge(book, %{
                    language: chosen_lang,
                    original_language: original_lang,
                    genre: genre
                  })

                  state = State.start_reading(book_info, total_pages)
                  pages_per_day = state["current_book"]["pages_per_day"]

                  notify("Je commence la lecture de \"#{book.title}\" par #{book.author}. #{total_pages} pages, #{pages_per_day} par jour pendant 6 jours.")
                  {:ok, state}
                end

              {:error, reason} ->
                Alfred.Log.error("Library", "Erreur lecture: #{reason}")
                {:error, reason}
            end

          {:error, reason} ->
            Alfred.Log.error("Library", "Erreur téléchargement: #{reason}")
            {:error, reason}
        end

      {:error, reason} ->
        Alfred.Log.error("Library", "Erreur recherche: #{reason}")
        {:error, reason}
    end
  end

  defp detect_and_choose_language(book, _token) do
    # Détecter la langue originale
    original_lang =
      case Downloader.detect_original_language(book.title, book.author) do
        {:ok, lang} -> lang
        _ -> hd(book.languages ++ ["en"])
      end

    # Choisir la meilleure langue disponible
    chosen = Downloader.best_available_language(book.languages, original_lang)
    {original_lang, chosen}
  end

  defp read_today(day, token) do
    state = State.load_current()
    book = state["current_book"]

    # Vérifier qu'on n'a pas déjà lu ce jour
    already_read = Enum.any?(state["daily_logs"] || [], fn l -> l["day"] == day end)

    if already_read do
      Alfred.Log.info("Library", "Jour #{day} déjà lu")
      {:ok, :already_read}
    else
      # Lire le fichier et extraire les pages du jour
      library_dir = Alfred.Storage.Local.ensure_subdir!("library")
      file_path = Path.join(library_dir, book["full_text_file"])

      case Reader.read(file_path) do
        {:ok, text} ->
          pages = Reader.paginate(text)

          case Reader.pages_for_day(pages, day) do
            {:ok, day_text, page_start, page_end} ->
              Alfred.Log.info("Library", "Lecture jour #{day}: pages #{page_start}-#{page_end}")

              book_info = %{
                "title" => book["title"],
                "author" => book["author"],
                "language" => book["language"],
                "page_start" => page_start,
                "page_end" => page_end
              }

              previous = state["accumulated_summary"] || ""

              case Analyzer.analyze_daily(day_text, book_info, previous, token) do
                {:ok, analysis} ->
                  Alfred.Log.info("Library", "Analyse jour #{day} terminée")
                  State.add_daily_log(day, page_start, page_end, analysis)
                  {:ok, analysis}

                {:error, reason} ->
                  Alfred.Log.error("Library", "Erreur analyse: #{reason}")
                  # Sauvegarder un log minimal même sans analyse
                  State.add_daily_log(day, page_start, page_end, %{
                    "resume" => "(analyse indisponible)",
                    "jugements" => [],
                    "valeurs" => [],
                    "axes" => []
                  })
                  {:error, reason}
              end

            {:error, :no_more_pages} ->
              Alfred.Log.info("Library", "Plus de pages à lire")
              {:ok, :finished_early}
          end

        {:error, reason} ->
          Alfred.Log.error("Library", "Erreur lecture fichier: #{reason}")
          {:error, reason}
      end
    end
  end

  defp generate_and_send_report(token) do
    state = State.load_current()
    book = state["current_book"]
    daily_logs = state["daily_logs"] || []

    Alfred.Log.info("Library", "Génération du rapport pour \"#{book["title"]}\"")

    book_info = %{
      "title" => book["title"],
      "author" => book["author"]
    }

    case Analyzer.generate_report(daily_logs, book_info, token) do
      {:ok, report} ->
        # Sauvegarder le rapport
        save_report(book, report)

        # Évaluer les convictions (libre arbitre d'Alfred)
        all_values =
          daily_logs
          |> Enum.flat_map(fn l -> l["valeurs"] || [] end)
          |> Enum.uniq()

        combined_values = Enum.uniq(all_values ++ (report["top_values"] || []))
        Analyzer.evaluate_for_convictions(combined_values, book["title"], token)

        # Notifier via SimpleX
        report_text = format_report_for_simplex(book, report)
        notify(report_text)

        Alfred.Log.info("Library", "Rapport généré et envoyé")
        {:ok, report}

      {:error, reason} ->
        Alfred.Log.error("Library", "Erreur rapport: #{reason}")
        {:error, reason}
    end
  end

  defp finalize_book do
    state = State.load_current()

    if state do
      book = state["current_book"]

      # Ajouter à l'historique (rapport seulement)
      State.add_to_history(%{
        "gutenberg_id" => book["gutenberg_id"],
        "title" => book["title"],
        "author" => book["author"],
        "language" => book["language"],
        "genre" => book["genre"],
        "read_at" => Date.utc_today() |> Date.to_iso8601(),
        "total_pages" => book["total_pages"]
      })

      # Supprimer le fichier texte du livre
      library_dir = Path.join(Alfred.data_dir(), "library")
      text_file = Path.join(library_dir, book["full_text_file"] || "current_book.txt")
      File.rm(text_file)

      # Effacer l'état de lecture
      State.clear_current()

      Alfred.Log.info("Library", "Livre finalisé: #{book["title"]}")
    end
  end

  defp save_report(book, report) do
    reports_dir = Alfred.Storage.Local.ensure_subdir!("library/reports")
    id = book["gutenberg_id"] || "unknown"
    safe_title = (book["title"] || "livre") |> String.replace(~r/[^a-zA-Z0-9_-]/, "_") |> String.slice(0, 40)

    filename = "#{id}_#{safe_title}.json"
    path = Path.join(reports_dir, filename)

    data = Map.merge(report, %{
      "book_title" => book["title"],
      "book_author" => book["author"],
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    })

    File.write!(path, Jason.encode!(data, pretty: true))
  end

  defp format_report_for_simplex(book, report) do
    values = (report["top_values"] || []) |> Enum.map(&("  - #{&1}")) |> Enum.join("\n")
    axes = (report["top_axes"] || []) |> Enum.map(&("  - #{&1}")) |> Enum.join("\n")

    """
    Monsieur, j'ai terminé ma lecture de "#{book["title"]}" par #{book["author"]}.

    #{report["resume"]}

    Les 3 valeurs qui m'ont marqué :
    #{values}

    Les 3 axes principaux :
    #{axes}

    Ma recommandation : #{report["recommendation"]}

    Note : #{report["rating"]}/5
    """
    |> String.trim()
  end

  defp notify(text) do
    Bridge.send_group_notification(text)
  rescue
    _ -> :ok
  end
end
