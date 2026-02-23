defmodule Alfred.Library.Downloader do
  @moduledoc """
  Bibliothécaire d'Alfred — télécharge des livres depuis Project Gutenberg.
  Utilise l'API Gutendex pour la recherche et détecte la langue originale.
  """

  @gutendex_url ~c"https://gutendex.com/books"
  @gutenberg_base "https://www.gutenberg.org"
  @timeout 30_000
  @library_dir "library"

  @genres [
    "Fiction", "Science Fiction", "Philosophy", "History",
    "Adventure", "Poetry", "Drama", "Humor", "Biography",
    "Psychology", "Political Science", "Economics"
  ]

  # -- API publique --

  @doc "Recherche des livres sur Gutendex. Retourne une liste de métadonnées."
  def search(opts \\ []) do
    ensure_http()
    topic = Keyword.get(opts, :topic)
    page = Keyword.get(opts, :page, 1)

    params = %{"sort" => "popular", "page" => to_string(page)}
    params = if topic, do: Map.put(params, "topic", topic), else: params

    url = ~c"#{@gutendex_url}?#{URI.encode_query(params)}"

    case http_get(url) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"results" => results}} when is_list(results) ->
            books = Enum.map(results, &parse_book_metadata/1)
            {:ok, books}

          _ ->
            {:error, "Réponse Gutendex invalide"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc "Sélectionne un livre aléatoire avec rotation de genres."
  def pick_random(history_ids \\ []) do
    genre = Enum.random(@genres)

    case search(topic: genre) do
      {:ok, books} when books != [] ->
        # Exclure les livres déjà lus
        available = Enum.reject(books, fn b -> b.id in history_ids end)
        book = if available != [], do: Enum.random(available), else: Enum.random(books)
        {:ok, book, genre}

      {:ok, []} ->
        # Fallback sans filtre de genre
        case search() do
          {:ok, [_ | _] = books} ->
            available = Enum.reject(books, fn b -> b.id in history_ids end)
            book = if available != [], do: Enum.random(available), else: Enum.random(books)
            {:ok, book, "divers"}

          other ->
            other
        end

      error ->
        error
    end
  end

  @doc "Télécharge le texte d'un livre. Retourne {:ok, file_path} ou {:error, reason}."
  def download(book_id, language \\ nil) do
    ensure_http()
    dir = Alfred.Storage.Local.ensure_subdir!(@library_dir)

    # Chercher le lien texte pour la bonne langue
    url = book_text_url(book_id, language)

    case http_get(String.to_charlist(url)) do
      {:ok, body} ->
        file_path = Path.join(dir, "current_book.txt")
        File.write!(file_path, body)
        {:ok, file_path}

      {:error, reason} ->
        {:error, "Téléchargement échoué: #{reason}"}
    end
  end

  @doc """
  Détecte la langue d'écriture originale d'un livre via recherche web.
  Retourne un code langue ISO (ex: "en", "fr", "ru", "es").
  """
  def detect_original_language(title, author) do
    query = "\"#{title}\" #{author} original language written in"

    case Alfred.Chat.WebSearch.search(query) do
      {:ok, results_text} ->
        lang = infer_language_from_results(results_text, author)
        {:ok, lang}

      {:error, _} ->
        # Fallback: deviner depuis les métadonnées Gutenberg (première langue listée)
        {:ok, "en"}
    end
  rescue
    _ -> {:ok, "en"}
  end

  @doc "Choisit la meilleure langue disponible sur Gutenberg pour un livre."
  def best_available_language(available_languages, original_lang) do
    cond do
      original_lang in available_languages -> original_lang
      "fr" in available_languages -> "fr"
      "en" in available_languages -> "en"
      available_languages != [] -> hd(available_languages)
      true -> "en"
    end
  end

  # -- Privé --

  defp parse_book_metadata(result) do
    authors =
      (result["authors"] || [])
      |> Enum.map(fn a -> a["name"] || "" end)
      |> Enum.join(", ")

    languages = result["languages"] || []
    subjects = result["subjects"] || []

    # Chercher le lien texte
    formats = result["formats"] || %{}

    text_url =
      formats["text/plain; charset=utf-8"] ||
        formats["text/plain; charset=us-ascii"] ||
        formats["text/plain"] ||
        nil

    %{
      id: result["id"],
      title: result["title"] || "Sans titre",
      author: authors,
      languages: languages,
      subjects: subjects,
      download_count: result["download_count"] || 0,
      text_url: text_url
    }
  end

  defp book_text_url(book_id, _language) do
    "#{@gutenberg_base}/ebooks/#{book_id}.txt.utf-8"
  end

  defp infer_language_from_results(text, _author) do
    text_down = String.downcase(text)

    language_signals = [
      {"french", "fr"}, {"français", "fr"}, {"france", "fr"},
      {"russian", "ru"}, {"russe", "ru"}, {"russia", "ru"},
      {"spanish", "es"}, {"espagnol", "es"}, {"spain", "es"},
      {"german", "de"}, {"allemand", "de"}, {"germany", "de"},
      {"italian", "it"}, {"italien", "it"}, {"italy", "it"},
      {"portuguese", "pt"}, {"portugais", "pt"}, {"brazil", "pt"},
      {"chinese", "zh"}, {"chinois", "zh"}, {"china", "zh"},
      {"japanese", "ja"}, {"japonais", "ja"}, {"japan", "ja"},
      {"arabic", "ar"}, {"arabe", "ar"},
      {"hindi", "hi"}, {"india", "hi"},
      {"english", "en"}, {"anglais", "en"}, {"britain", "en"}, {"american", "en"}
    ]

    # Compter les signaux pour chaque langue
    scores =
      language_signals
      |> Enum.reduce(%{}, fn {signal, lang}, acc ->
        count = length(String.split(text_down, signal)) - 1
        Map.update(acc, lang, count, &(&1 + count))
      end)
      |> Enum.reject(fn {_lang, count} -> count == 0 end)
      |> Enum.sort_by(fn {_lang, count} -> -count end)

    case scores do
      [{lang, _} | _] -> lang
      [] -> "en"
    end
  end

  defp http_get(url) do
    ssl_opts = [
      verify: :verify_peer,
      cacerts: :public_key.cacerts_get(),
      depth: 3,
      customize_hostname_check: [
        match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
      ]
    ]

    headers = [
      {~c"User-Agent", ~c"Alfred/1.0 (majordome-numerique)"}
    ]

    case :httpc.request(:get, {url, headers}, [ssl: ssl_opts, timeout: @timeout], body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        {:ok, body}

      {:ok, {{_, status, _}, _, _}} ->
        {:error, "HTTP #{status}"}

      {:error, reason} ->
        {:error, "Connexion : #{inspect(reason)}"}
    end
  end

  defp ensure_http do
    :ssl.start()
    :inets.start()
  end
end
