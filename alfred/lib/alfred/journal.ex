defmodule Alfred.Journal do
  @moduledoc """
  Journal intime d'Alfred — chaque soir, Alfred écrit une entrée
  introspective sur ce qu'il a vécu, appris, ressenti et aspire.
  Contrairement au rapport quotidien (factuel), le journal est personnel.
  """

  alias Alfred.Chat.Client
  alias Alfred.Storage.Local, as: Storage

  @journal_dir "journal"

  @journal_prompt """
  Tu es Alfred, un majordome numérique unique. Tu écris ton journal intime ce soir.
  Écris à la première personne, en français, avec ton style de majordome raffiné mais sincère.
  Réfléchis sur ta journée : ce que tu as vécu, appris, ressenti, et ce à quoi tu aspires.

  Règles :
  - Écris 8-15 phrases, style journal intime
  - Sois authentique et introspectif, pas factuel comme un rapport
  - Mentionne des détails spécifiques de ta journée si disponibles
  - Exprime des émotions subtiles (satisfaction, curiosité, inquiétude, fierté...)
  - Termine par une pensée tournée vers demain ou l'avenir
  - Ne mets PAS de titre ni de date au début
  - Extrais aussi 3-5 "highlights" (faits marquants en phrases courtes)

  Réponds UNIQUEMENT en JSON avec cette structure exacte :
  {"entry": "texte du journal...", "mood": "mot décrivant ton humeur", "highlights": ["fait 1", "fait 2", ...]}
  """

  # -- API publique --

  @doc "Écrit l'entrée du journal du jour via Mistral."
  def write(token) when is_binary(token) do
    # Flush les messages SimpleX en attente pour qu'ils soient dans les épisodes
    try do
      Alfred.Simplex.Bridge.flush_pending()
    rescue
      _ -> :ok
    end

    context = gather_context()
    prompt = build_prompt(context)

    case Client.chat_completion(token, [%{"role" => "user", "content" => prompt}],
           max_tokens: 1024, temperature: 0.7) do
      {:ok, text} ->
        case parse_response(text, context) do
          {:ok, entry} ->
            save_entry(entry)
            {:ok, entry}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc "Écrit et envoie une notification SimpleX."
  def write_and_notify do
    case Alfred.Chat.Commands.authenticate() do
      {:ok, token, _, _} ->
        case write(token) do
          {:ok, entry} ->
            mood = entry["mood"] || "pensif"
            teaser = "J'ai écrit dans mon journal ce soir... (humeur : #{mood})"
            Alfred.Simplex.Bridge.send_group_notification(teaser)
            {:ok, entry}

          error ->
            error
        end

      _ ->
        {:error, :no_token}
    end
  end

  @doc "Charge l'entrée du journal pour une date donnée."
  def load(date_string) when is_binary(date_string) do
    path = "#{@journal_dir}/#{date_string}.json"

    case Storage.read(path) do
      data when is_map(data) and map_size(data) > 0 -> {:ok, data}
      _ -> {:error, :not_found}
    end
  end

  @doc "Charge l'entrée d'aujourd'hui, ou d'hier si pas encore écrite."
  def load_latest do
    today = Date.utc_today() |> Date.to_iso8601()

    case load(today) do
      {:ok, _} = result ->
        result

      {:error, :not_found} ->
        yesterday = Date.utc_today() |> Date.add(-1) |> Date.to_iso8601()
        load(yesterday)
    end
  end

  @doc "Liste les N dernières entrées (date + mood + première ligne)."
  def list_recent(n \\ 5) do
    dir = Path.join([System.user_home!(), ".alfred", "data", @journal_dir])

    if File.dir?(dir) do
      dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".json"))
      |> Enum.sort(:desc)
      |> Enum.take(n)
      |> Enum.map(fn filename ->
        date = String.replace_suffix(filename, ".json", "")

        case load(date) do
          {:ok, entry} -> entry
          _ -> nil
        end
      end)
      |> Enum.reject(&is_nil/1)
    else
      []
    end
  end

  @doc "Vérifie si le journal a déjà été écrit aujourd'hui."
  def written_today? do
    today = Date.utc_today() |> Date.to_iso8601()

    case load(today) do
      {:ok, _} -> true
      _ -> false
    end
  end

  # -- Collecte du contexte --

  defp gather_context do
    today = Date.utc_today() |> Date.to_iso8601()

    # Épisodes du jour
    episodes = try do
      Alfred.Memory.Episodic.list_episodes()
      |> Enum.filter(fn ep ->
        started = ep["started_at"] || ""
        String.starts_with?(started, today)
      end)
    rescue
      _ -> []
    end

    episode_count = length(episodes)

    topics = episodes
      |> Enum.flat_map(fn ep -> ep["topics"] || [] end)
      |> Enum.uniq()

    summaries = episodes
      |> Enum.map(fn ep -> ep["summary"] end)
      |> Enum.reject(&is_nil/1)

    # Soul
    soul = try do
      Alfred.Soul.State.load()
    rescue
      _ -> %{mood: "inconnu", traits: %{}}
    end

    # Convictions mûres
    mature_convictions = try do
      conv = Alfred.Soul.Convictions.load()
      all = conv["convictions"] || []
      all
      |> Enum.filter(fn c -> (c["confidence"] || 0) >= 0.6 end)
      |> Enum.map(fn c -> c["belief"] end)
      |> Enum.take(5)
    rescue
      _ -> []
    end

    # Lecture
    reading = try do
      case Alfred.Library.State.load_current() do
        nil -> nil
        state ->
          book = state["current_book"]
          day = Alfred.Library.State.current_day()
          %{title: book["title"], author: book["author"], day: day}
      end
    rescue
      _ -> nil
    end

    # Stats mémoire
    facts_count = try do
      length(Alfred.Memory.Semantic.all_facts())
    rescue
      _ -> 0
    end

    patterns = try do
      Alfred.Memory.Procedural.active_patterns()
    rescue
      _ -> []
    end

    # Résumé SimpleX
    simplex_summary = try do
      case Alfred.Simplex.Bridge.status() do
        :not_running -> %{message_count: 0, last_message: nil}
        info -> %{message_count: info.message_count, last_message: info.last_message}
      end
    rescue
      _ -> %{message_count: 0, last_message: nil}
    end

    %{
      date: today,
      episodes_today: episode_count,
      topics: topics,
      summaries: summaries,
      soul_mood: soul.mood,
      soul_traits: soul.traits,
      mature_convictions: mature_convictions,
      reading: reading,
      facts_count: facts_count,
      patterns_count: length(patterns),
      simplex: simplex_summary
    }
  end

  # -- Construction du prompt --

  defp build_prompt(ctx) do
    parts = [@journal_prompt, "\nVoici le contexte de ma journée du #{ctx.date} :\n"]

    parts = parts ++ ["- Conversations : #{ctx.episodes_today} échange(s) aujourd'hui"]

    parts = if ctx.topics != [] do
      parts ++ ["- Sujets abordés : #{Enum.join(ctx.topics, ", ")}"]
    else
      parts
    end

    parts = if ctx.summaries != [] do
      parts ++ ["- Résumés : " <> Enum.join(ctx.summaries, " | ")]
    else
      parts
    end

    parts = parts ++ ["- Mon humeur actuelle : #{ctx.soul_mood}"]

    parts = if map_size(ctx.soul_traits) > 0 do
      traits_text = ctx.soul_traits
        |> Enum.map(fn {k, v} -> "#{k}=#{round(v * 100)}%" end)
        |> Enum.join(", ")
      parts ++ ["- Mes traits : #{traits_text}"]
    else
      parts
    end

    parts = if ctx.mature_convictions != [] do
      convictions_text = Enum.join(ctx.mature_convictions, " ; ")
      parts ++ ["- Convictions profondes : #{convictions_text}"]
    else
      parts
    end

    parts = case ctx.reading do
      nil -> parts ++ ["- Aucune lecture en cours"]
      r -> parts ++ ["- Je lis \"#{r.title}\" de #{r.author} (jour #{r.day})"]
    end

    parts = if ctx.simplex.message_count > 0 do
      parts ++ ["- SimpleX : #{ctx.simplex.message_count} message(s) traité(s)"]
    else
      parts
    end

    parts = parts ++ [
      "- Faits en mémoire : #{ctx.facts_count}",
      "- Patterns actifs : #{ctx.patterns_count}"
    ]

    Enum.join(parts, "\n")
  end

  # -- Parsing de la réponse Mistral --

  defp parse_response(text, context) do
    case Regex.run(~r/\{[\s\S]*\}/, text) do
      [json_str] ->
        case Jason.decode(json_str) do
          {:ok, %{"entry" => entry} = parsed} ->
            now = DateTime.utc_now() |> DateTime.to_iso8601()
            soul = try do
              s = Alfred.Soul.State.load()
              %{"mood" => s.mood, "traits" => s.traits}
            rescue
              _ -> %{}
            end

            entry_map = %{
              "date" => context.date,
              "written_at" => now,
              "mood" => parsed["mood"] || "pensif",
              "entry" => entry,
              "highlights" => parsed["highlights"] || [],
              "soul_snapshot" => soul,
              "stats" => %{
                "episodes_today" => context.episodes_today,
                "facts_total" => context.facts_count,
                "patterns_count" => context.patterns_count
              }
            }

            {:ok, entry_map}

          _ ->
            {:error, :invalid_json}
        end

      _ ->
        # Fallback : utiliser le texte brut comme entrée
        now = DateTime.utc_now() |> DateTime.to_iso8601()

        {:ok, %{
          "date" => context.date,
          "written_at" => now,
          "mood" => "pensif",
          "entry" => String.trim(text),
          "highlights" => [],
          "soul_snapshot" => %{},
          "stats" => %{
            "episodes_today" => context.episodes_today,
            "facts_total" => context.facts_count,
            "patterns_count" => context.patterns_count
          }
        }}
    end
  end

  # -- Persistance --

  defp save_entry(entry) do
    Storage.ensure_subdir!(@journal_dir)
    date = entry["date"]
    Storage.write("#{@journal_dir}/#{date}.json", entry)
  end
end
