defmodule Alfred.Library.Reader do
  @moduledoc """
  Lecteur de livres d'Alfred — extraction de texte et pagination.
  Supporte .txt et .epub (via :zip Erlang, zéro dépendance).
  """

  @words_per_page 250

  # -- API publique --

  @doc "Lit un fichier livre et retourne le texte brut."
  def read(path) do
    ext = path |> Path.extname() |> String.downcase()

    case ext do
      ".txt" -> read_txt(path)
      ".epub" -> read_epub(path)
      _ -> {:error, "Format non supporté: #{ext}"}
    end
  end

  @doc "Découpe un texte en pages de ~250 mots."
  def paginate(text, words_per_page \\ @words_per_page) do
    text
    |> String.split(~r/\s+/, trim: true)
    |> Enum.chunk_every(words_per_page)
    |> Enum.with_index(1)
    |> Enum.map(fn {words, page_num} ->
      {page_num, Enum.join(words, " ")}
    end)
  end

  @doc "Retourne le texte des pages pour un jour donné (1-6)."
  def pages_for_day(pages, day, total_days \\ 6) do
    total = length(pages)
    per_day = ceil(total / total_days)
    start_idx = (day - 1) * per_day
    count = min(per_day, total - start_idx)

    if start_idx < total do
      day_pages = Enum.slice(pages, start_idx, count)

      text =
        day_pages
        |> Enum.map(fn {_num, content} -> content end)
        |> Enum.join("\n\n")

      {first_page, _} = hd(day_pages)
      {last_page, _} = List.last(day_pages)
      {:ok, text, first_page, last_page}
    else
      {:error, :no_more_pages}
    end
  end

  @doc "Compte le nombre de mots dans un texte."
  def word_count(text) do
    text |> String.split(~r/\s+/, trim: true) |> length()
  end

  # -- Lecture TXT --

  defp read_txt(path) do
    case File.read(path) do
      {:ok, content} ->
        # Nettoyer les en-têtes/footers Gutenberg
        text = strip_gutenberg_headers(content)
        {:ok, text}

      {:error, reason} ->
        {:error, "Lecture impossible: #{reason}"}
    end
  end

  # -- Lecture EPUB --

  defp read_epub(path) do
    charlist_path = String.to_charlist(path)

    with {:ok, files} <- :zip.unzip(charlist_path, [:memory]),
         {:ok, opf_path} <- find_opf_path(files),
         {:ok, opf_content} <- get_file_content(files, opf_path),
         {:ok, spine_items} <- parse_spine(opf_content, opf_path) do
      text =
        spine_items
        |> Enum.map(fn item_path ->
          case get_file_content(files, item_path) do
            {:ok, html} -> strip_html(html)
            _ -> ""
          end
        end)
        |> Enum.reject(&(String.trim(&1) == ""))
        |> Enum.join("\n\n")

      {:ok, text}
    else
      {:error, reason} -> {:error, "EPUB invalide: #{inspect(reason)}"}
      _ -> {:error, "EPUB illisible"}
    end
  end

  defp find_opf_path(files) do
    case get_file_content(files, "META-INF/container.xml") do
      {:ok, xml} ->
        case Regex.run(~r/full-path="([^"]+)"/, xml) do
          [_, path] -> {:ok, path}
          _ -> {:error, "OPF introuvable dans container.xml"}
        end

      _ ->
        {:error, "Pas de container.xml"}
    end
  end

  defp parse_spine(opf, opf_path) do
    opf_dir = Path.dirname(opf_path)

    # Manifest: id → href
    manifest =
      Regex.scan(~r/<item\s[^>]*?id="([^"]+)"[^>]*?href="([^"]+)"[^>]*?\/?>/, opf)
      |> Enum.map(fn [_, id, href] -> {id, href} end)
      |> Map.new()

    manifest2 =
      Regex.scan(~r/<item\s[^>]*?href="([^"]+)"[^>]*?id="([^"]+)"[^>]*?\/?>/, opf)
      |> Enum.map(fn [_, href, id] -> {id, href} end)
      |> Map.new()

    all_manifest = Map.merge(manifest2, manifest)

    # Spine: reading order
    spine_ids =
      Regex.scan(~r/<itemref\s[^>]*?idref="([^"]+)"/, opf)
      |> Enum.map(fn [_, id] -> id end)

    paths =
      spine_ids
      |> Enum.map(fn id ->
        case Map.get(all_manifest, id) do
          nil -> nil
          href -> if opf_dir == ".", do: href, else: Path.join(opf_dir, href)
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, paths}
  end

  defp get_file_content(files, target) do
    target_cl = String.to_charlist(target)

    case Enum.find(files, fn {path, _} -> path == target_cl end) do
      {_, content} -> {:ok, to_string(content)}
      nil -> {:error, :not_found}
    end
  end

  defp strip_html(html) do
    html
    |> String.replace(~r/<style[^>]*>.*?<\/style>/s, "")
    |> String.replace(~r/<script[^>]*>.*?<\/script>/s, "")
    |> String.replace(~r/<br\s*\/?>/, "\n")
    |> String.replace(~r/<\/p>/, "\n\n")
    |> String.replace(~r/<\/h[1-6]>/, "\n\n")
    |> String.replace(~r/<\/li>/, "\n")
    |> String.replace(~r/<[^>]+>/, "")
    |> String.replace(~r/&nbsp;/, " ")
    |> String.replace(~r/&amp;/, "&")
    |> String.replace(~r/&lt;/, "<")
    |> String.replace(~r/&gt;/, ">")
    |> String.replace(~r/&quot;/, "\"")
    |> String.replace(~r/&#\d+;/, "")
    |> String.replace(~r/\n{3,}/, "\n\n")
    |> String.trim()
  end

  defp strip_gutenberg_headers(text) do
    # Retirer l'en-tête Gutenberg (avant "*** START OF")
    text =
      case Regex.run(~r/\*\*\*\s*START OF.*?\*\*\*\s*\n(.*)/s, text) do
        [_, content] -> content
        _ -> text
      end

    # Retirer le footer (après "*** END OF")
    case Regex.run(~r/(.*?)\*\*\*\s*END OF/s, text) do
      [_, content] -> String.trim(content)
      _ -> String.trim(text)
    end
  end
end
