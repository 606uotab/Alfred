defmodule Alfred.Initiative.Smart do
  @moduledoc """
  Notifications intelligentes — Alfred apprend quand notifier
  en analysant les patterns d'interaction de son maître.
  """

  alias Alfred.Storage.Local, as: Storage

  @activity_file "initiative/activity_log.json"
  @min_interactions 20

  # -- Tracking --

  @doc "Enregistre une interaction (message, commande, notification)."
  def log_interaction(type, category \\ "general") do
    now = DateTime.utc_now()
    hour = now.hour
    day = Date.day_of_week(now) |> day_name()

    entry = %{
      "type" => type,
      "category" => category,
      "hour" => hour,
      "day" => day,
      "at" => DateTime.to_iso8601(now)
    }

    data = load_data()
    interactions = (data["interactions"] || []) ++ [entry]

    # Garder les 500 dernières interactions
    interactions = Enum.take(interactions, -500)

    # Recalculer les scores horaires
    hourly = compute_hourly_scores(interactions)
    windows = compute_optimal_windows(hourly)

    updated = %{
      "interactions" => interactions,
      "hourly_scores" => hourly,
      "optimal_windows" => windows,
      "updated_at" => DateTime.to_iso8601(now)
    }

    Storage.ensure_subdir!("initiative")
    Storage.write(@activity_file, updated)
    :ok
  end

  @doc "Vérifie si c'est un bon moment pour notifier."
  def should_notify_now? do
    data = load_data()
    interactions = data["interactions"] || []

    # Pas assez de données → toujours notifier
    if length(interactions) < @min_interactions do
      true
    else
      hour = DateTime.utc_now().hour
      windows = data["optimal_windows"] || []

      if windows == [] do
        true
      else
        Enum.any?(windows, fn [start_h, end_h] ->
          hour >= start_h and hour < end_h
        end)
      end
    end
  end

  @doc "Retourne les fenêtres optimales actuelles."
  def optimal_windows do
    data = load_data()
    data["optimal_windows"] || []
  end

  @doc "Retourne les scores horaires."
  def hourly_scores do
    data = load_data()
    data["hourly_scores"] || %{}
  end

  @doc "Retourne le nombre total d'interactions trackées."
  def interaction_count do
    data = load_data()
    length(data["interactions"] || [])
  end

  # -- Calculs --

  defp compute_hourly_scores(interactions) do
    interactions
    |> Enum.group_by(fn i -> i["hour"] end)
    |> Enum.map(fn {hour, entries} ->
      messages = Enum.count(entries, fn e -> e["type"] in ["message", "command"] end)
      notifications = Enum.count(entries, fn e -> e["type"] == "notification_sent" end)

      {to_string(hour), %{
        "total" => length(entries),
        "messages" => messages,
        "notifications" => notifications
      }}
    end)
    |> Map.new()
  end

  defp compute_optimal_windows(hourly) do
    if map_size(hourly) == 0 do
      []
    else
      # Trouver les heures avec le plus d'activité (messages + commandes)
      scored = hourly
        |> Enum.map(fn {hour_str, data} ->
          {String.to_integer(hour_str), data["messages"] || 0}
        end)
        |> Enum.sort_by(fn {_h, count} -> -count end)

      # Seuil : au moins 20% de l'heure la plus active
      max_count = case scored do
        [{_, c} | _] -> c
        _ -> 0
      end

      if max_count == 0 do
        []
      else
        threshold = max(1, round(max_count * 0.2))

        active_hours = scored
          |> Enum.filter(fn {_h, count} -> count >= threshold end)
          |> Enum.map(fn {h, _} -> h end)
          |> Enum.sort()

        # Grouper en fenêtres contiguës
        group_contiguous(active_hours)
      end
    end
  end

  defp group_contiguous([]), do: []
  defp group_contiguous(hours) do
    hours
    |> Enum.reduce([], fn hour, acc ->
      case acc do
        [] -> [[hour, hour + 1]]
        [[start, last] | rest] when hour == last ->
          [[start, hour + 1] | rest]
        _ ->
          [[hour, hour + 1] | acc]
      end
    end)
    |> Enum.reverse()
  end

  # -- Persistence --

  def load_data do
    case Storage.read(@activity_file) do
      data when is_map(data) and map_size(data) > 0 -> data
      _ -> %{"interactions" => [], "hourly_scores" => %{}, "optimal_windows" => []}
    end
  end

  defp day_name(1), do: "lundi"
  defp day_name(2), do: "mardi"
  defp day_name(3), do: "mercredi"
  defp day_name(4), do: "jeudi"
  defp day_name(5), do: "vendredi"
  defp day_name(6), do: "samedi"
  defp day_name(7), do: "dimanche"
end
