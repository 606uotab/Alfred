defmodule Alfred.News do
  @moduledoc """
  Briefing matinal — Alfred lit les infos et prépare un résumé pour son maître.
  Source : API locale http://localhost:8420/api/v1/news
  """

  @api_url ~c"http://localhost:8420/api/v1/news"
  @news_dir "news"
  @max_articles 30
  @timeout 15_000

  # -- API publique --

  @doc "Récupère les news, génère un briefing via Mistral et le sauvegarde."
  def briefing do
    with {:ok, articles} <- fetch_news(),
         {:ok, token, _, _} <- Alfred.Chat.Commands.authenticate(),
         {:ok, text} <- generate_briefing(token, articles) do
      save_briefing(text, length(articles))
      {:ok, text}
    end
  end

  @doc "Génère un briefing avec un token déjà authentifié."
  def briefing(token) when is_binary(token) do
    with {:ok, articles} <- fetch_news(),
         {:ok, text} <- generate_briefing(token, articles) do
      save_briefing(text, length(articles))
      {:ok, text}
    end
  end

  @doc "Récupère et envoie le briefing via SimpleX + voix."
  def briefing_and_notify do
    case briefing() do
      {:ok, text} ->
        teaser = text |> String.split("\n") |> Enum.take(5) |> Enum.join("\n")
        Alfred.Simplex.Bridge.send_group_notification("📰 Briefing matinal\n\n#{teaser}\n\n(alfred news pour le briefing complet)")
        Alfred.Voice.speak("Monsieur, votre briefing matinal est prêt.")
        {:ok, text}

      error ->
        Alfred.Log.error("News", "Erreur briefing: #{inspect(error)}")
        error
    end
  rescue
    e ->
      Alfred.Log.error("News", Exception.message(e))
      {:error, Exception.message(e)}
  end

  @doc "Charge le dernier briefing sauvegardé."
  def load_latest do
    Alfred.Storage.Local.ensure_subdir!(@news_dir)

    case list_briefings() do
      [latest | _] ->
        Alfred.Storage.Local.read(Path.join(@news_dir, latest))

      [] ->
        nil
    end
  end

  @doc "Liste les briefings récents (noms de fichiers, desc)."
  def list_briefings do
    dir = Path.join([System.user_home!(), ".alfred", "data", @news_dir])

    case File.ls(dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.sort(:desc)
        |> Enum.take(10)

      _ ->
        []
    end
  end

  @doc "Vérifie si le briefing a déjà été fait aujourd'hui."
  def done_today? do
    today = Date.utc_today() |> Date.to_iso8601()
    filename = "#{today}.json"
    filename in list_briefings()
  end

  @doc "Récupère les articles bruts depuis l'API."
  def fetch_news do
    ensure_http_started()

    case :httpc.request(:get, {@api_url, []}, [timeout: @timeout, connect_timeout: 5_000], []) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        parse_articles(body)

      {:ok, {{_, code, _}, _, _}} ->
        {:error, "API returned #{code}"}

      {:error, reason} ->
        {:error, "HTTP error: #{inspect(reason)}"}
    end
  end

  # -- Parsing --

  defp parse_articles(body) do
    json = to_string(body)

    case Jason.decode(json) do
      {:ok, %{"data" => articles}} when is_list(articles) ->
        # Trier par date de publication (plus récent d'abord)
        sorted =
          articles
          |> Enum.sort_by(fn a -> -(a["published_at"] || 0) end)
          |> Enum.take(@max_articles)

        {:ok, sorted}

      {:ok, _} ->
        {:error, "Unexpected API format"}

      {:error, reason} ->
        {:error, "JSON parse error: #{inspect(reason)}"}
    end
  end

  # -- Briefing via Mistral --

  defp generate_briefing(token, articles) do
    # Grouper par catégorie
    grouped = Enum.group_by(articles, fn a -> a["category"] || "general" end)

    headlines =
      Enum.map_join(grouped, "\n\n", fn {category, arts} ->
        cat_label = category_label(category)
        items = Enum.map_join(Enum.take(arts, 8), "\n", fn a ->
          summary = if a["summary"] && a["summary"] != "" && String.length(a["summary"]) > 10 do
            " — #{String.slice(a["summary"], 0, 200)}"
          else
            ""
          end
          "- [#{a["source"]}] #{a["title"]}#{summary}"
        end)
        "## #{cat_label}\n#{items}"
      end)

    prompt = """
    Tu es Alfred, un majordome numérique. Prépare un briefing matinal concis et élégant pour ton maître.

    Voici les titres d'actualité du jour :

    #{headlines}

    Consignes :
    - Résume les informations les plus importantes en 10-15 lignes maximum
    - Groupe par thème (monde, tech/IA, finance, culture)
    - Ton de majordome : "Monsieur, voici les faits marquants..."
    - Mentionne 1-2 tendances boursières/crypto si pertinent
    - Finis par une phrase d'humeur ou un conseil du jour
    - Pas de listes à puces, rédige en paragraphes courts
    """

    messages = [
      %{role: "system", content: "Tu es Alfred, majordome numérique français. Briefing matinal concis et informatif."},
      %{role: "user", content: prompt}
    ]

    case Alfred.Chat.Client.chat_completion(token, messages, max_tokens: 800, temperature: 0.4) do
      {:ok, text} -> {:ok, text}
      {:error, reason} -> {:error, "Mistral error: #{inspect(reason)}"}
    end
  end

  # -- Sauvegarde --

  defp save_briefing(text, article_count) do
    Alfred.Storage.Local.ensure_subdir!(@news_dir)
    today = Date.utc_today() |> Date.to_iso8601()

    data = %{
      "date" => today,
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "article_count" => article_count,
      "briefing" => text
    }

    Alfred.Storage.Local.write(Path.join(@news_dir, "#{today}.json"), data)
  end

  # -- Helpers --

  defp category_label("custom"), do: "Monde & Actualités"
  defp category_label("crypto"), do: "Crypto"
  defp category_label("stock_index"), do: "Marchés & Bourse"
  defp category_label("forex"), do: "Forex"
  defp category_label("commodity"), do: "Matières premières"
  defp category_label(other), do: String.capitalize(other)

  defp ensure_http_started do
    :inets.start()
    :ssl.start()
  end
end
