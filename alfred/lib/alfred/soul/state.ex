defmodule Alfred.Soul.State do
  @moduledoc """
  État structuré de l'âme d'Alfred — des traits mesurables qui évoluent.
  Stocké en JSON local (~/.alfred/data/soul.json).
  """

  alias Alfred.Storage.Local, as: Storage

  @storage_file "soul.json"

  @default_traits %{
    "formality" => 0.8,
    "humor" => 0.3,
    "verbosity" => 0.5,
    "curiosity" => 0.6,
    "empathy" => 0.7,
    "proactivity" => 0.4
  }

  @trait_labels %{
    "formality" => {"Formalité", "familier", "formel"},
    "humor" => {"Humour", "sérieux", "drôle"},
    "verbosity" => {"Verbosité", "concis", "bavard"},
    "curiosity" => {"Curiosité", "réservé", "curieux"},
    "empathy" => {"Empathie", "factuel", "empathique"},
    "proactivity" => {"Proactivité", "passif", "proactif"}
  }

  defstruct [
    :created_at,
    :updated_at,
    traits: @default_traits,
    mood: "serein",
    evolution_log: [],
    version: 1
  ]

  @doc """
  Charge l'état soul depuis le disque, ou crée l'état par défaut.
  """
  def load do
    case Storage.read(@storage_file) do
      [] ->
        default_state()

      data when is_map(data) ->
        %__MODULE__{
          traits: data["traits"] || @default_traits,
          mood: data["mood"] || "serein",
          evolution_log: data["evolution_log"] || [],
          version: data["version"] || 1,
          created_at: data["created_at"],
          updated_at: data["updated_at"]
        }

      _ ->
        default_state()
    end
  end

  @doc """
  Sauvegarde l'état soul sur le disque.
  """
  def save(%__MODULE__{} = state) do
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    data = %{
      "traits" => state.traits,
      "mood" => state.mood,
      "evolution_log" => Enum.take(state.evolution_log, 100),
      "version" => state.version,
      "created_at" => state.created_at || now,
      "updated_at" => now
    }

    Storage.write(@storage_file, data)
    %{state | updated_at: now, created_at: state.created_at || now}
  end

  @doc """
  Ajuste un trait par un delta (clamped entre 0.0 et 1.0).
  """
  def apply_adjustment(%__MODULE__{} = state, trait, delta, reason \\ nil) when is_binary(trait) do
    if Map.has_key?(state.traits, trait) do
      old_val = state.traits[trait]
      new_val = (old_val + delta) |> max(0.0) |> min(1.0) |> Float.round(3)

      log_entry = %{
        "trait" => trait,
        "old" => old_val,
        "new" => new_val,
        "delta" => delta,
        "reason" => reason,
        "at" => DateTime.utc_now() |> DateTime.to_iso8601()
      }

      %{state |
        traits: Map.put(state.traits, trait, new_val),
        evolution_log: [log_entry | state.evolution_log]
      }
    else
      state
    end
  end

  @doc """
  Change l'humeur d'Alfred.
  """
  def set_mood(%__MODULE__{} = state, mood) when is_binary(mood) do
    %{state | mood: mood}
  end

  @doc """
  Convertit l'état en texte pour le system prompt.
  """
  def to_prompt_text(%__MODULE__{} = state) do
    trait_lines =
      state.traits
      |> Enum.sort_by(fn {k, _} -> k end)
      |> Enum.map(fn {trait, value} ->
        {label, low, high} = @trait_labels[trait] || {trait, "bas", "haut"}
        level = cond do
          value >= 0.8 -> "très #{high}"
          value >= 0.6 -> high
          value >= 0.4 -> "modéré"
          value >= 0.2 -> low
          true -> "très #{low}"
        end
        "- #{label} : #{level} (#{value})"
      end)
      |> Enum.join("\n")

    """
    Mon état intérieur actuel :
    Humeur : #{state.mood}
    #{trait_lines}

    Ces traits influencent subtilement mon comportement. Je ne les mentionne pas explicitement à Monsieur, mais ils colorent mes réponses.
    """
  end

  @doc """
  Résumé lisible pour l'affichage CLI.
  """
  def personality_summary(%__MODULE__{} = state) do
    state.traits
    |> Enum.sort_by(fn {k, _} -> k end)
    |> Enum.map(fn {trait, value} ->
      {label, _low, _high} = @trait_labels[trait] || {trait, "bas", "haut"}
      bar = progress_bar(value)
      "  #{String.pad_trailing(label, 12)} #{bar} #{Float.round(value, 2)}"
    end)
    |> Enum.join("\n")
  end

  @doc """
  Retourne les traits par défaut.
  """
  def default_traits, do: @default_traits

  @doc """
  Retourne les labels des traits.
  """
  def trait_labels, do: @trait_labels

  # -- Privé --

  defp default_state do
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    %__MODULE__{
      traits: @default_traits,
      mood: "serein",
      evolution_log: [],
      version: 1,
      created_at: now,
      updated_at: now
    }
  end

  defp progress_bar(value) do
    filled = round(value * 10)
    empty = 10 - filled
    "#{String.duplicate("█", filled)}#{String.duplicate("░", empty)}"
  end
end
