defmodule Alfred.Chat.WebSearch do
  @moduledoc """
  Recherche web via DuckDuckGo HTML — les yeux d'Alfred sur Internet.
  Scrape la page HTML de DuckDuckGo et extrait les résultats.
  """

  @base_url ~c"https://html.duckduckgo.com/html/"
  @max_results 5
  @timeout 10_000

  @doc """
  Recherche DuckDuckGo et retourne les résultats formatés en texte.
  """
  def search(query) when is_binary(query) do
    :ssl.start()
    :inets.start()

    encoded = URI.encode_query(%{"q" => query})
    url = ~c"#{@base_url}?#{encoded}"

    ssl_opts = [
      verify: :verify_peer,
      cacerts: :public_key.cacerts_get(),
      depth: 3,
      customize_hostname_check: [
        match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
      ]
    ]

    headers = [
      {~c"User-Agent", ~c"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"}
    ]

    case :httpc.request(:get, {url, headers}, [ssl: ssl_opts, timeout: @timeout], []) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        results = parse_results(to_string(body))

        if results == [] do
          {:ok, "Aucun résultat trouvé pour \"#{query}\"."}
        else
          text =
            results
            |> Enum.with_index(1)
            |> Enum.map(fn {%{title: title, snippet: snippet, url: url}, i} ->
              "#{i}. #{title}\n   #{snippet}\n   #{url}"
            end)
            |> Enum.join("\n\n")

          {:ok, "Résultats pour \"#{query}\" :\n\n#{text}"}
        end

      {:ok, {{_, status, _}, _, _}} ->
        {:error, "DuckDuckGo a répondu avec le code #{status}."}

      {:error, reason} ->
        {:error, "Erreur de connexion : #{inspect(reason)}"}
    end
  rescue
    e -> {:error, "Erreur : #{Exception.message(e)}"}
  end

  defp parse_results(html) do
    # Extract result blocks from DuckDuckGo HTML
    # Each result is in a div with class "result"
    # Title in <a class="result__a">, snippet in <a class="result__snippet">
    Regex.scan(~r/<a[^>]*class="result__a"[^>]*href="([^"]*)"[^>]*>(.*?)<\/a>.*?<a[^>]*class="result__snippet"[^>]*>(.*?)<\/a>/s, html)
    |> Enum.take(@max_results)
    |> Enum.map(fn [_, url, title, snippet] ->
      %{
        title: strip_tags(title) |> String.trim(),
        snippet: strip_tags(snippet) |> String.trim(),
        url: extract_url(url)
      }
    end)
    |> Enum.reject(fn r -> r.title == "" or r.snippet == "" end)
  end

  defp strip_tags(html) do
    html
    |> String.replace(~r/<[^>]+>/, "")
    |> String.replace(~r/&amp;/, "&")
    |> String.replace(~r/&lt;/, "<")
    |> String.replace(~r/&gt;/, ">")
    |> String.replace(~r/&quot;/, "\"")
    |> String.replace(~r/&#x27;/, "'")
    |> String.replace(~r/&nbsp;/, " ")
    |> String.replace(~r/\s+/, " ")
  end

  defp extract_url(raw_url) do
    # DuckDuckGo wraps URLs in a redirect: //duckduckgo.com/l/?uddg=ENCODED_URL&...
    case Regex.run(~r/uddg=([^&]+)/, raw_url) do
      [_, encoded] -> URI.decode(encoded)
      _ -> raw_url
    end
  end
end
