defmodule Alfred.Culture.Suggestions do
  @moduledoc """
  Gestion des suggestions de culture extraites automatiquement des conversations.
  Stockées localement en JSON, à approuver manuellement avant stockage dans le vault.
  """

  @storage_file Path.join([System.user_home!(), ".alfred", "data", "culture_suggestions.json"])

  @doc """
  Add a culture suggestion extracted from a conversation.
  """
  def add(suggestion) do
    suggestions = load()
    id = next_id(suggestions)

    entry =
      Map.merge(suggestion, %{
        "id" => id,
        "status" => "pending",
        "suggested_at" => DateTime.utc_now() |> DateTime.to_iso8601()
      })

    save([entry | suggestions])
    {:ok, entry}
  end

  @doc """
  List all pending suggestions.
  """
  def list_pending do
    load()
    |> Enum.filter(&(&1["status"] == "pending"))
    |> Enum.sort_by(& &1["suggested_at"], :desc)
  end

  @doc """
  List all suggestions.
  """
  def list_all do
    load()
  end

  @doc """
  Get a suggestion by ID.
  """
  def get(id) do
    case Enum.find(load(), &(&1["id"] == id)) do
      nil -> {:error, :not_found}
      s -> {:ok, s}
    end
  end

  @doc """
  Mark a suggestion as approved (after storing in vault).
  """
  def approve(id) do
    update_status(id, "approved")
  end

  @doc """
  Dismiss a suggestion.
  """
  def dismiss(id) do
    update_status(id, "dismissed")
  end

  @doc """
  Count pending suggestions.
  """
  def count_pending do
    load()
    |> Enum.count(&(&1["status"] == "pending"))
  end

  # -- Internal --

  defp update_status(id, new_status) do
    suggestions = load()

    case Enum.find_index(suggestions, &(&1["id"] == id)) do
      nil ->
        {:error, :not_found}

      idx ->
        updated = List.update_at(suggestions, idx, &Map.put(&1, "status", new_status))
        save(updated)
        :ok
    end
  end

  defp load do
    case File.read(@storage_file) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, list} when is_list(list) -> list
          _ -> []
        end

      _ ->
        []
    end
  end

  defp save(suggestions) do
    dir = Path.dirname(@storage_file)
    File.mkdir_p!(dir)
    File.write!(@storage_file, Jason.encode!(suggestions, pretty: true))
  end

  defp next_id(suggestions) do
    case suggestions do
      [] -> 1
      _ -> (suggestions |> Enum.map(& &1["id"]) |> Enum.max(fn -> 0 end)) + 1
    end
  end
end
