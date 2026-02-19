defmodule Alfred.Memory.Semantic do
  @moduledoc """
  Mémoire sémantique — Alfred retient les faits importants sur son maître.
  Faits structurés avec catégorie, sujet, contenu et confiance.
  """

  alias Alfred.Storage.Local, as: Storage

  @storage_file "memory/semantic.json"

  @valid_categories ~w(preferences knowledge project_context behavioral_patterns personal_info technical_context)

  def valid_categories, do: @valid_categories

  @doc """
  Retourne tous les faits mémorisés.
  """
  def all_facts do
    Storage.read(@storage_file)
  end

  @doc """
  Ajoute un fait à la mémoire sémantique.
  """
  def add_fact(fact) when is_map(fact) do
    facts = all_facts()
    next_id = next_id(facts)
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    new_fact =
      %{
        "id" => next_id,
        "category" => fact["category"] || "knowledge",
        "subject" => fact["subject"] || "",
        "content" => fact["content"] || "",
        "source" => fact["source"] || "unknown",
        "confidence" => fact["confidence"] || 0.5,
        "created_at" => now,
        "last_accessed" => now,
        "access_count" => 0
      }

    Storage.write(@storage_file, facts ++ [new_fact])
    {:ok, new_fact}
  end

  @doc """
  Recherche des faits par mots-clés. Retourne les plus pertinents.
  """
  def search(query, opts \\ []) do
    limit = Keyword.get(opts, :limit, 5)

    query_words =
      query
      |> String.downcase()
      |> String.split(~r/\s+/, trim: true)

    all_facts()
    |> Enum.map(fn fact ->
      score = relevance_score(fact, query_words)
      {fact, score}
    end)
    |> Enum.filter(fn {_fact, score} -> score > 0 end)
    |> Enum.sort_by(fn {_fact, score} -> -score end)
    |> Enum.take(limit)
    |> Enum.map(fn {fact, _score} ->
      update_access(fact)
      fact
    end)
  end

  @doc """
  Retourne les faits d'une catégorie donnée.
  """
  def list_by_category(category) do
    all_facts()
    |> Enum.filter(&(&1["category"] == category))
  end

  @doc """
  Met à jour la date d'accès et incrémente le compteur.
  """
  def update_access(fact) do
    facts = all_facts()
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    updated =
      Enum.map(facts, fn f ->
        if f["id"] == fact["id"] do
          %{f | "last_accessed" => now, "access_count" => (f["access_count"] || 0) + 1}
        else
          f
        end
      end)

    Storage.write(@storage_file, updated)
  end

  @doc """
  Supprime un fait par ID.
  """
  def delete_fact(id) do
    facts = all_facts()

    case Enum.find(facts, &(&1["id"] == id)) do
      nil ->
        {:error, :not_found}

      _fact ->
        updated = Enum.reject(facts, &(&1["id"] == id))
        Storage.write(@storage_file, updated)
        :ok
    end
  end

  @doc """
  Retourne les faits les plus accédés (les plus importants pour Alfred).
  """
  def top_facts(n \\ 10) do
    all_facts()
    |> Enum.sort_by(&(-(&1["access_count"] || 0)))
    |> Enum.take(n)
  end

  @doc """
  Nombre total de faits.
  """
  def count do
    all_facts() |> length()
  end

  @doc """
  Consolide la mémoire sémantique — fusionne les faits redondants.
  Regroupe par subject, garde le plus confiant, merge les contenus.
  """
  def consolidate do
    facts = all_facts()

    if length(facts) < 2, do: :ok

    # Group by subject (lowercase)
    groups =
      facts
      |> Enum.group_by(fn f ->
        (f["subject"] || "") |> String.downcase() |> String.trim()
      end)

    consolidated =
      groups
      |> Enum.flat_map(fn {_subject, group_facts} ->
        if length(group_facts) <= 1 do
          group_facts
        else
          merge_fact_group(group_facts)
        end
      end)

    if length(consolidated) < length(facts) do
      Storage.write(@storage_file, consolidated)
    end

    :ok
  end

  defp merge_fact_group(facts) do
    # Sort by confidence desc, then access_count desc
    sorted = Enum.sort_by(facts, fn f ->
      {-(f["confidence"] || 0), -(f["access_count"] || 0)}
    end)

    primary = hd(sorted)
    others = tl(sorted)

    # Merge unique contents
    all_contents =
      [primary | others]
      |> Enum.map(fn f -> f["content"] || "" end)
      |> Enum.uniq()
      |> Enum.reject(&(&1 == ""))

    merged_content =
      if length(all_contents) > 1 do
        Enum.join(all_contents, " | ")
      else
        hd(all_contents)
      end

    merged = %{primary |
      "content" => merged_content,
      "access_count" => Enum.sum(Enum.map(facts, fn f -> f["access_count"] || 0 end)),
      "confidence" => Enum.max(Enum.map(facts, fn f -> f["confidence"] || 0 end))
    }

    [merged]
  end

  # -- Privé --

  defp relevance_score(fact, query_words) do
    fact_text =
      "#{fact["subject"]} #{fact["content"]} #{fact["category"]}"
      |> String.downcase()

    Enum.count(query_words, fn word ->
      String.contains?(fact_text, word)
    end)
  end

  defp next_id([]), do: 1

  defp next_id(facts) do
    facts
    |> Enum.map(& &1["id"])
    |> Enum.max(fn -> 0 end)
    |> Kernel.+(1)
  end
end
