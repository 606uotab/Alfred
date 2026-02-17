defmodule Alfred.Culture do
  @moduledoc """
  Culture d'Alfred — modèle de données pour les connaissances avec attribution de source.
  Les connaissances sont stockées chiffrées dans le vault 'culture'.
  """

  alias Alfred.Vault.Port

  @source_types ~w(person book observation web other)

  def source_types, do: @source_types

  @doc """
  Create a new knowledge entry and store it in the culture vault.
  Requires the vault to be already unlocked in the current session.
  """
  def learn(topic, content, source, tags, password) do
    id = generate_id()
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    knowledge = %{
      id: id,
      topic: topic,
      content: content,
      source: source,
      tags: tags,
      learned_at: now
    }

    json = Jason.encode!(knowledge)
    key = "knowledge_#{id}"

    case Port.send_with_unlock("culture", password, %{cmd: "store", key: key, value: json}) do
      {:ok, _} ->
        # Also update the index
        update_index(password, id, topic, tags)
        {:ok, knowledge}

      {:error, _} = err ->
        err
    end
  end

  @doc """
  List all knowledge entries from culture vault.
  """
  def list_all(password) do
    case Port.send_with_unlock("culture", password, %{cmd: "list"}) do
      {:ok, %{"keys" => keys}} ->
        knowledge_keys = Enum.filter(keys, &String.starts_with?(&1, "knowledge_"))

        entries =
          Enum.reduce(knowledge_keys, [], fn key, acc ->
            case Port.send_with_unlock("culture", password, %{cmd: "get", key: key}) do
              {:ok, %{"value" => json}} ->
                case Jason.decode(json) do
                  {:ok, entry} -> [entry | acc]
                  _ -> acc
                end

              _ ->
                acc
            end
          end)

        {:ok, Enum.sort_by(entries, & &1["learned_at"], :desc)}

      {:error, _} = err ->
        err
    end
  end

  @doc """
  Search knowledge entries by keywords.
  """
  def search(query, password) do
    case list_all(password) do
      {:ok, entries} ->
        words =
          query
          |> String.downcase()
          |> String.split(~r/\s+/, trim: true)

        results =
          entries
          |> Enum.map(fn entry ->
            text =
              String.downcase(
                "#{entry["topic"]} #{entry["content"]} #{Enum.join(entry["tags"] || [], " ")}"
              )

            score = Enum.count(words, &String.contains?(text, &1))
            {entry, score}
          end)
          |> Enum.filter(fn {_, score} -> score > 0 end)
          |> Enum.sort_by(fn {_, score} -> score end, :desc)
          |> Enum.map(fn {entry, _} -> entry end)

        {:ok, results}

      {:error, _} = err ->
        err
    end
  end

  @doc """
  Get a single knowledge entry by ID.
  """
  def get(id, password) do
    key = "knowledge_#{id}"

    case Port.send_with_unlock("culture", password, %{cmd: "get", key: key}) do
      {:ok, %{"value" => json}} ->
        case Jason.decode(json) do
          {:ok, entry} -> {:ok, entry}
          _ -> {:error, "Invalid knowledge data"}
        end

      {:error, _} = err ->
        err
    end
  end

  # -- Internal --

  defp generate_id do
    :crypto.strong_rand_bytes(4) |> Base.hex_encode32(case: :lower, padding: false)
  end

  defp update_index(password, id, topic, tags) do
    index =
      case Port.send_with_unlock("culture", password, %{cmd: "get", key: "culture_index"}) do
        {:ok, %{"value" => json}} ->
          case Jason.decode(json) do
            {:ok, idx} -> idx
            _ -> %{"entries" => []}
          end

        _ ->
          %{"entries" => []}
      end

    entry = %{"id" => id, "topic" => topic, "tags" => tags}
    updated = %{index | "entries" => [entry | index["entries"]]}

    Port.send_with_unlock("culture", password, %{
      cmd: "store",
      key: "culture_index",
      value: Jason.encode!(updated)
    })
  end
end
