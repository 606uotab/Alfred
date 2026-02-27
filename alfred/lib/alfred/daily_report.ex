defmodule Alfred.DailyReport do
  @moduledoc """
  Rapport quotidien — Alfred résume ce qu'il a fait dans la journée.
  Envoyé automatiquement via SimpleX et stocké dans ~/.alfred/data/reports/.
  """

  alias Alfred.Memory.{Episodic, Semantic, Procedural}
  alias Alfred.Library.State, as: LibState

  @reports_dir "reports"

  # -- API publique --

  @doc "Génère et envoie le rapport quotidien."
  def generate_and_send do
    # Flush les messages SimpleX en attente avant de collecter les métriques
    try do
      Alfred.Simplex.Bridge.flush_pending()
    rescue
      _ -> :ok
    end

    report = gather_metrics()
    text = format_report(report)

    # Sauvegarder
    save_report(report)

    # Envoyer via SimpleX
    Alfred.Simplex.Bridge.send_group_notification(text)

    Alfred.Log.info("DailyReport", "Rapport envoyé")
    {:ok, report}
  rescue
    e ->
      Alfred.Log.error("DailyReport", Exception.message(e))
      {:error, Exception.message(e)}
  end

  @doc "Génère le rapport sans l'envoyer (pour affichage CLI)."
  def generate do
    report = gather_metrics()
    {:ok, report, format_report(report)}
  end

  # -- Collecte des métriques --

  defp gather_metrics do
    today = Date.utc_today() |> Date.to_iso8601()

    %{
      "date" => today,
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "conversations" => conversation_metrics(),
      "knowledge" => knowledge_metrics(),
      "patterns" => pattern_metrics(),
      "reading" => reading_metrics(),
      "messages" => message_metrics(),
      "reminders" => reminder_metrics(),
      "soul" => soul_metrics(),
      "brain" => brain_metrics(),
      "cortex" => cortex_metrics()
    }
  end

  defp conversation_metrics do
    episodes = Episodic.list_episodes()
    today = Date.utc_today() |> Date.to_iso8601()

    today_episodes =
      Enum.filter(episodes, fn ep ->
        started = ep["started_at"] || ""
        String.starts_with?(started, today)
      end)

    total_messages = Enum.sum(Enum.map(today_episodes, fn ep -> ep["message_count"] || 0 end))

    topics =
      today_episodes
      |> Enum.flat_map(fn ep -> ep["topics"] || [] end)
      |> Enum.uniq()

    summaries =
      today_episodes
      |> Enum.map(fn ep -> ep["summary"] end)
      |> Enum.reject(&is_nil/1)

    %{
      "total_episodes" => Episodic.count(),
      "today_episodes" => length(today_episodes),
      "today_messages" => total_messages,
      "today_topics" => topics,
      "today_summaries" => summaries
    }
  end

  defp knowledge_metrics do
    %{
      "total_facts" => Semantic.count(),
      "active_patterns" => Procedural.count_active()
    }
  end

  defp pattern_metrics do
    patterns = Procedural.active_patterns()

    %{
      "count" => length(patterns),
      "types" => Enum.map(patterns, fn p -> p["pattern_type"] || "?" end) |> Enum.uniq()
    }
  end

  defp reading_metrics do
    case LibState.load_current() do
      nil ->
        %{"status" => "idle", "books_read" => LibState.books_read_count()}

      state ->
        book = state["current_book"]
        day = LibState.current_day()
        logs = state["daily_logs"] || []
        pages_read = length(logs) * (book["pages_per_day"] || 0)

        %{
          "status" => "reading",
          "book" => book["title"],
          "author" => book["author"],
          "day" => day,
          "pages_read" => pages_read,
          "total_pages" => book["total_pages"],
          "books_read" => LibState.books_read_count()
        }
    end
  end

  defp message_metrics do
    case Alfred.Simplex.Bridge.status() do
      :not_running ->
        %{"bridge" => "offline", "count" => 0}

      info ->
        %{
          "bridge" => if(info.connected, do: "online", else: "disconnected"),
          "count" => info.message_count,
          "last" => if(info.last_message, do: DateTime.to_iso8601(info.last_message), else: nil)
        }
    end
  end

  defp reminder_metrics do
    case :alfred_scheduler.count_pending() do
      {:ok, count} -> %{"pending" => count}
      _ -> %{"pending" => 0}
    end
  rescue
    _ -> %{"pending" => 0}
  end

  defp soul_metrics do
    soul = Alfred.Soul.State.load()
    convictions = Alfred.Soul.Convictions.load()
    all_conv = convictions["convictions"] || []
    mature = Enum.count(all_conv, fn c -> (c["confidence"] || 0) >= 0.6 end)

    %{
      "mood" => soul.mood,
      "traits" => soul.traits,
      "total_convictions" => length(all_conv),
      "mature_convictions" => mature
    }
  rescue
    _ -> %{"mood" => "?", "total_convictions" => 0, "mature_convictions" => 0}
  end

  defp brain_metrics do
    projects = Alfred.ProjectData.all_for_startup()

    case Alfred.Brain.Port.send_command(%{
           cmd: "suggest",
           projects: projects,
           now: DateTime.utc_now() |> DateTime.to_iso8601()
         }) do
      {:ok, resp} -> %{"suggestions" => resp["suggestions"] || []}
      _ -> %{"suggestions" => []}
    end
  rescue
    _ -> %{"suggestions" => []}
  end

  defp cortex_metrics do
    projects = Alfred.ProjectData.all_for_startup()
    episodes = Episodic.list_episodes()
    facts = Semantic.all_facts()

    case Alfred.Cortex.Port.send_command(%{
           cmd: "productivity_stats",
           projects: projects,
           episodes: episodes,
           facts: facts
         }) do
      {:ok, resp} ->
        %{
          "completion_rate" => resp["completion_rate"],
          "summary" => resp["summary"]
        }

      _ ->
        %{}
    end
  rescue
    _ -> %{}
  end

  # -- Formatage --

  defp format_report(report) do
    date = report["date"]
    conv = report["conversations"]
    reading = report["reading"]
    msgs = report["messages"]
    reminders = report["reminders"]
    knowledge = report["knowledge"]

    lines = ["Rapport du #{date} :", ""]

    # Conversations
    lines =
      if conv["today_episodes"] > 0 do
        lines ++
          ["Conversations : #{conv["today_episodes"]} échange(s), #{conv["today_messages"]} message(s)."] ++
          if conv["today_topics"] != [] do
            ["Sujets : #{Enum.join(conv["today_topics"], ", ")}."]
          else
            []
          end ++
          Enum.map(conv["today_summaries"], fn s -> "  - #{s}" end)
      else
        lines ++ ["Aucune conversation aujourd'hui."]
      end

    # Lecture
    lines =
      case reading["status"] do
        "reading" ->
          pct = if reading["total_pages"] > 0, do: round(reading["pages_read"] / reading["total_pages"] * 100), else: 0
          lines ++ ["", "Lecture : \"#{reading["book"]}\" — jour #{reading["day"]}/7, #{pct}% lu."]

        _ ->
          if reading["books_read"] > 0 do
            lines ++ ["", "Bibliothèque : #{reading["books_read"]} livre(s) lu(s)."]
          else
            lines
          end
      end

    # Messages SimpleX
    lines =
      if msgs["count"] > 0 do
        lines ++ ["", "Messages SimpleX : #{msgs["count"]} traité(s) (bridge #{msgs["bridge"]})."]
      else
        lines ++ ["", "Bridge SimpleX : #{msgs["bridge"]}."]
      end

    # Rappels
    lines =
      if reminders["pending"] > 0 do
        lines ++ ["Rappels en attente : #{reminders["pending"]}."]
      else
        lines
      end

    # Mémoire
    lines = lines ++ ["", "Mémoire : #{knowledge["total_facts"]} fait(s), #{knowledge["active_patterns"]} pattern(s)."]

    # Âme
    soul = report["soul"] || %{}
    lines =
      if soul["total_convictions"] && soul["total_convictions"] > 0 do
        lines ++ ["Âme : humeur \"#{soul["mood"]}\", #{soul["mature_convictions"]} conviction(s) mûre(s) / #{soul["total_convictions"]}."]
      else
        lines
      end

    # Cerveau (Julia)
    brain = report["brain"] || %{}
    suggestions = brain["suggestions"] || []
    lines =
      if suggestions != [] do
        lines ++ ["", "Cerveau : #{length(suggestions)} suggestion(s)."] ++
          Enum.map(Enum.take(suggestions, 3), fn s -> "  - #{s}" end)
      else
        lines
      end

    # Cortex (R)
    cortex = report["cortex"] || %{}
    lines =
      if cortex["summary"] do
        lines ++ ["", "Cortex : #{cortex["summary"]}"]
      else
        lines
      end

    Enum.join(lines, "\n")
  end

  # -- Persistance --

  defp save_report(report) do
    dir = Alfred.Storage.Local.ensure_subdir!(@reports_dir)
    date = report["date"]
    path = Path.join(dir, "#{date}.json")
    File.write!(path, Jason.encode!(report, pretty: true))
  end
end
