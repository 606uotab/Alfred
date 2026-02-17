defmodule Alfred.Memory.Episodic do
  @moduledoc """
  Mémoire épisodique — Alfred se souvient de chaque conversation.
  Chaque épisode est un fichier JSON individuel dans memory/episodes/.
  """

  alias Alfred.Storage.Local, as: Storage

  @episodes_dir "memory/episodes"

  def ensure_dir! do
    Storage.ensure_subdir!(@episodes_dir)
  end

  @doc """
  Sauvegarde un épisode de conversation.
  """
  def save_episode(episode) do
    ensure_dir!()
    id = generate_id()

    data =
      Map.merge(episode, %{
        "id" => id,
        "saved_at" => DateTime.utc_now() |> DateTime.to_iso8601()
      })

    filename = Path.join(@episodes_dir, "#{id}.json")
    Storage.write(filename, data)
    {:ok, data}
  end

  @doc """
  Liste tous les épisodes (métadonnées uniquement, sans les messages).
  """
  def list_episodes do
    dir = Path.join(Alfred.data_dir(), @episodes_dir)

    if File.exists?(dir) do
      dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".json"))
      |> Enum.sort(:desc)
      |> Enum.map(fn filename ->
        path = Path.join(dir, filename)
        data = path |> File.read!() |> Jason.decode!()

        %{
          "id" => data["id"],
          "started_at" => data["started_at"],
          "ended_at" => data["ended_at"],
          "mode" => data["mode"],
          "message_count" => data["message_count"],
          "summary" => data["summary"],
          "topics" => data["topics"]
        }
      end)
    else
      []
    end
  end

  @doc """
  Charge un épisode complet (avec messages).
  """
  def load_episode(id) do
    filename = Path.join(@episodes_dir, "#{id}.json")
    path = Path.join(Alfred.data_dir(), filename)

    if File.exists?(path) do
      {:ok, path |> File.read!() |> Jason.decode!()}
    else
      {:error, :not_found}
    end
  end

  @doc """
  Retourne les N épisodes les plus récents (métadonnées).
  """
  def recent_episodes(n \\ 3) do
    list_episodes() |> Enum.take(n)
  end

  @doc """
  Retourne les résumés des N épisodes récents.
  """
  def recent_summaries(n \\ 3) do
    recent_episodes(n)
    |> Enum.map(fn ep -> ep["summary"] end)
    |> Enum.reject(&is_nil/1)
  end

  @doc """
  Compte le nombre total d'épisodes.
  """
  def count do
    dir = Path.join(Alfred.data_dir(), @episodes_dir)

    if File.exists?(dir) do
      dir
      |> File.ls!()
      |> Enum.count(&String.ends_with?(&1, ".json"))
    else
      0
    end
  end

  @doc """
  Met à jour un épisode existant (merge des champs).
  """
  def update_episode(id, updates) when is_map(updates) do
    path = Path.join([Alfred.data_dir(), @episodes_dir, "#{id}.json"])

    if File.exists?(path) do
      data = path |> File.read!() |> Jason.decode!()
      updated = Map.merge(data, updates)
      File.write!(path, Jason.encode!(updated, pretty: true))
      {:ok, updated}
    else
      {:error, :not_found}
    end
  end

  @doc """
  Supprime un épisode.
  """
  def delete_episode(id) do
    path = Path.join([Alfred.data_dir(), @episodes_dir, "#{id}.json"])

    if File.exists?(path) do
      File.rm!(path)
      :ok
    else
      {:error, :not_found}
    end
  end

  defp generate_id do
    now = DateTime.utc_now()
    random = :crypto.strong_rand_bytes(2) |> Base.encode16(case: :lower)

    now
    |> Calendar.strftime("%Y%m%d_%H%M%S")
    |> Kernel.<>("_#{random}")
  end
end
