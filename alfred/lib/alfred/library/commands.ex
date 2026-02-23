defmodule Alfred.Library.Commands do
  @moduledoc """
  Commandes CLI pour la bibliothèque d'Alfred.
  """

  alias Alfred.Butler
  alias Alfred.Library.State

  def handle([]) do
    show_status()
  end

  def handle(["status"]) do
    show_status()
  end

  def handle(["history"]) do
    history = State.load_history()

    if history == [] do
      Butler.say("Je n'ai encore lu aucun livre, Monsieur. Ma bibliothèque est vierge.")
    else
      Butler.say("Monsieur, voici les livres que j'ai lus :\n")

      history
      |> Enum.with_index(1)
      |> Enum.each(fn {book, idx} ->
        date = book["read_at"] || "?"
        lang = book["language"] || "?"
        IO.puts("  #{idx}. \"#{book["title"]}\" par #{book["author"]} (#{lang}) — lu le #{date}")

        if book["total_pages"] do
          IO.puts("     #{book["total_pages"]} pages, genre: #{book["genre"] || "divers"}")
        end
      end)

      IO.puts("\n  #{length(history)} livre(s) lu(s) au total.")
    end
  end

  def handle(["report" | rest]) do
    reports_dir = Path.join(Alfred.data_dir(), "library/reports")

    if File.exists?(reports_dir) do
      case File.ls(reports_dir) do
        {:ok, files} when files != [] ->
          json_files = Enum.filter(files, &String.ends_with?(&1, ".json"))

          if rest == [] do
            # Lister les rapports disponibles
            Butler.say("Rapports de lecture disponibles :\n")

            json_files
            |> Enum.with_index(1)
            |> Enum.each(fn {file, idx} ->
              name = String.replace(file, ".json", "")
              IO.puts("  #{idx}. #{name}")
            end)

            IO.puts("\n  Utilisez 'alfred library report <numéro>' pour consulter un rapport.")
          else
            # Afficher un rapport spécifique
            idx_str = hd(rest)

            case Integer.parse(idx_str) do
              {idx, ""} when idx >= 1 and idx <= length(json_files) ->
                file = Enum.at(json_files, idx - 1)
                path = Path.join(reports_dir, file)

                case File.read(path) do
                  {:ok, content} ->
                    case Jason.decode(content) do
                      {:ok, report} ->
                        display_report(report)

                      _ ->
                        Butler.say("Rapport illisible, Monsieur.")
                    end

                  _ ->
                    Butler.say("Impossible de lire ce rapport, Monsieur.")
                end

              _ ->
                Butler.say("Numéro de rapport invalide, Monsieur.")
            end
          end

        _ ->
          Butler.say("Aucun rapport de lecture disponible, Monsieur.")
      end
    else
      Butler.say("Aucun rapport de lecture disponible, Monsieur.")
    end
  end

  def handle(["next"]) do
    Butler.say("Recherche d'un nouveau livre...")

    case Alfred.Chat.Commands.authenticate() do
      {:ok, token, _, _} ->
        case Alfred.Library.Scheduler.start_next_book(token) do
          {:ok, state} ->
            book = state["current_book"]
            Butler.say("Très bien ! Je commence \"#{book["title"]}\" par #{book["author"]}.")
            IO.puts("  #{book["total_pages"]} pages, #{book["pages_per_day"]} par jour.")
            IO.puts("  Langue : #{book["language"]} (originale : #{book["original_language"]})")

          {:error, reason} ->
            Butler.say("Je suis navré, Monsieur. Erreur : #{inspect(reason)}")
        end

      _ ->
        Butler.say("Impossible de m'authentifier auprès de Mistral, Monsieur.")
    end
  end

  def handle(["read"]) do
    case Alfred.Chat.Commands.authenticate() do
      {:ok, token, _, _} ->
        case Alfred.Library.Scheduler.read_now(token) do
          {:ok, %{"resume" => resume}} ->
            Butler.say("Lecture du jour terminée.\n")
            IO.puts("  Résumé : #{resume}")

          {:ok, :already_read} ->
            Butler.say("J'ai déjà lu ma portion du jour, Monsieur.")

          {:ok, :finished_early} ->
            Butler.say("J'ai terminé le livre plus tôt que prévu, Monsieur.")

          {:error, :no_reading} ->
            Butler.say("Aucune lecture en cours. Utilisez 'alfred library next' pour commencer.")

          {:error, :not_reading_day} ->
            Butler.say("Aujourd'hui est le jour du rapport, pas de lecture, Monsieur.")

          {:error, reason} ->
            Butler.say("Erreur de lecture : #{inspect(reason)}")
        end

      _ ->
        Butler.say("Impossible de m'authentifier auprès de Mistral, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes de bibliothèque sont :\n")

    IO.puts("""
      alfred library                 Livre en cours + progression
      alfred library status           Idem
      alfred library history          Historique des livres lus
      alfred library report           Rapports de lecture
      alfred library report <n>       Consulter un rapport
      alfred library next             Commencer un nouveau livre
      alfred library read             Lire la portion du jour
    """)
  end

  # -- Privé --

  defp show_status do
    case State.load_current() do
      nil ->
        count = State.books_read_count()
        Butler.say("Aucune lecture en cours, Monsieur. #{count} livre(s) lu(s) au total.")
        Butler.say("Utilisez 'alfred library next' pour commencer un nouveau livre.")

      state ->
        book = state["current_book"]
        day = State.current_day()
        logs = state["daily_logs"] || []
        pages_read = length(logs) * (book["pages_per_day"] || 0)
        total = book["total_pages"] || 0
        pct = if total > 0, do: round(pages_read / total * 100), else: 0

        Butler.say("Lecture en cours :\n")
        IO.puts("  \"#{book["title"]}\" par #{book["author"]}")
        IO.puts("  Langue : #{book["language"]} (originale : #{book["original_language"] || "?"})")
        IO.puts("  Genre : #{book["genre"] || "divers"}")
        IO.puts("  Jour #{day}/7 — #{pages_read}/#{total} pages (#{pct}%)")
        IO.puts("  #{book["pages_per_day"]} pages/jour")

        if logs != [] do
          IO.puts("\n  Dernière lecture :")
          last = List.last(logs)
          IO.puts("  Jour #{last["day"]} : #{last["resume"]}")
        end
    end
  end

  defp display_report(report) do
    Butler.say("Rapport de lecture :\n")
    IO.puts("  \"#{report["book_title"]}\" par #{report["book_author"]}")
    IO.puts("")
    IO.puts("  #{report["resume"]}")
    IO.puts("")

    if report["top_values"] do
      IO.puts("  Valeurs clés :")
      Enum.each(report["top_values"], fn v -> IO.puts("    - #{v}") end)
    end

    if report["top_axes"] do
      IO.puts("\n  Axes principaux :")
      Enum.each(report["top_axes"], fn a -> IO.puts("    - #{a}") end)
    end

    if report["recommendation"] do
      IO.puts("\n  Recommandation : #{report["recommendation"]}")
    end

    if report["rating"] do
      IO.puts("  Note : #{report["rating"]}/5")
    end
  end
end
