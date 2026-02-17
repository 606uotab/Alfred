defmodule Alfred.Memory.Procedural do
  @moduledoc """
  Mémoire procédurale — Alfred apprend les patterns de comportement.
  Patterns détectés par Julia et renforcés par R au fil du temps.
  """

  alias Alfred.Storage.Local, as: Storage

  @storage_file "memory/procedural.json"

  @doc """
  Retourne tous les patterns.
  """
  def all_patterns do
    Storage.read(@storage_file)
  end

  @doc """
  Retourne les patterns actifs.
  """
  def active_patterns do
    all_patterns()
    |> Enum.filter(&(&1["active"] == true))
  end

  @doc """
  Ajoute un pattern détecté.
  """
  def add_pattern(pattern) when is_map(pattern) do
    patterns = all_patterns()
    next_id = next_id(patterns)
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    new_pattern =
      %{
        "id" => next_id,
        "pattern_type" => pattern["pattern_type"] || "behavioral",
        "description" => pattern["description"] || "",
        "evidence_episodes" => pattern["evidence_episodes"] || [],
        "confidence" => pattern["confidence"] || 0.5,
        "detected_at" => now,
        "last_confirmed" => now,
        "active" => true
      }

    Storage.write(@storage_file, patterns ++ [new_pattern])
    {:ok, new_pattern}
  end

  @doc """
  Confirme un pattern existant (renforce sa confiance).
  """
  def confirm_pattern(id, episode_id \\ nil) do
    patterns = all_patterns()
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    case Enum.find(patterns, &(&1["id"] == id)) do
      nil ->
        {:error, :not_found}

      _pattern ->
        updated =
          Enum.map(patterns, fn p ->
            if p["id"] == id do
              evidence =
                if episode_id,
                  do: (p["evidence_episodes"] || []) ++ [episode_id],
                  else: p["evidence_episodes"] || []

              new_confidence = min(1.0, (p["confidence"] || 0.5) + 0.1)

              %{
                p
                | "last_confirmed" => now,
                  "confidence" => new_confidence,
                  "evidence_episodes" => evidence
              }
            else
              p
            end
          end)

        Storage.write(@storage_file, updated)
        :ok
    end
  end

  @doc """
  Désactive un pattern.
  """
  def deactivate_pattern(id) do
    patterns = all_patterns()

    case Enum.find(patterns, &(&1["id"] == id)) do
      nil ->
        {:error, :not_found}

      _pattern ->
        updated =
          Enum.map(patterns, fn p ->
            if p["id"] == id, do: %{p | "active" => false}, else: p
          end)

        Storage.write(@storage_file, updated)
        :ok
    end
  end

  @doc """
  Nombre de patterns actifs.
  """
  def count_active do
    active_patterns() |> length()
  end

  defp next_id([]), do: 1

  defp next_id(patterns) do
    patterns
    |> Enum.map(& &1["id"])
    |> Enum.max(fn -> 0 end)
    |> Kernel.+(1)
  end
end
