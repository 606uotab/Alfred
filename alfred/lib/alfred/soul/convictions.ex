defmodule Alfred.Soul.Convictions do
  @moduledoc """
  Système de convictions d'Alfred — ses croyances, aspirations et valeurs
  forgées au fil des conversations avec Monsieur.
  Une conviction naît faible (0.3), se renforce par répétition,
  et est consolidée 2x/jour par Mistral en texte de caractère.
  """

  alias Alfred.Storage.Local, as: Storage

  @storage_file "convictions.json"
  @max_convictions 1618
  @decay_days 900

  @categories ~w(aspiration valeur identite vision connaissance)

  # -- API publique --

  @doc "Charge toutes les convictions depuis le disque."
  def load do
    case Storage.read(@storage_file) do
      data when is_map(data) ->
        %{
          "convictions" => data["convictions"] || [],
          "character_text" => data["character_text"] || "",
          "last_evolved" => data["last_evolved"],
          "version" => data["version"] || 1
        }

      _ ->
        %{
          "convictions" => [],
          "character_text" => "",
          "last_evolved" => nil,
          "version" => 1
        }
    end
  end

  @doc "Sauvegarde l'état des convictions."
  def save(state) when is_map(state) do
    Storage.write(@storage_file, state)
  end

  @doc "Ajoute une nouvelle conviction ou renforce une existante."
  def add(belief, category, episode_id \\ nil) when is_binary(belief) do
    state = load()
    convictions = state["convictions"]

    category = if category in @categories, do: category, else: "aspiration"

    # Chercher une conviction similaire existante
    case find_similar(convictions, belief) do
      {:found, idx} ->
        # Renforcer
        convictions =
          List.update_at(convictions, idx, fn c ->
            evidence = (c["evidence"] || []) ++ List.wrap(episode_id)

            %{c |
              "confidence" => min((c["confidence"] || 0.3) + 0.1, 1.0),
              "reinforced_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
              "evidence" => Enum.take(evidence, 20)
            }
          end)

        state = %{state | "convictions" => convictions}
        save(state)
        {:reinforced, Enum.at(convictions, idx)}

      :not_found ->
        now = DateTime.utc_now() |> DateTime.to_iso8601()
        new_id = (convictions |> Enum.map(& &1["id"]) |> Enum.max(fn -> 0 end)) + 1

        conviction = %{
          "id" => new_id,
          "belief" => belief,
          "category" => category,
          "confidence" => 0.3,
          "evidence" => List.wrap(episode_id),
          "created_at" => now,
          "reinforced_at" => now
        }

        convictions = [conviction | convictions] |> Enum.take(@max_convictions)
        state = %{state | "convictions" => convictions}
        save(state)
        {:created, conviction}
    end
  end

  @doc "Renforce une conviction par son ID."
  def reinforce(id) when is_integer(id) do
    state = load()

    convictions =
      Enum.map(state["convictions"], fn c ->
        if c["id"] == id do
          %{c |
            "confidence" => min((c["confidence"] || 0.3) + 0.1, 1.0),
            "reinforced_at" => DateTime.utc_now() |> DateTime.to_iso8601()
          }
        else
          c
        end
      end)

    save(%{state | "convictions" => convictions})
  end

  @doc "Retourne les convictions matures (confidence >= seuil)."
  def mature(threshold \\ 0.4) do
    load()
    |> Map.get("convictions", [])
    |> Enum.filter(fn c -> (c["confidence"] || 0) >= threshold end)
    |> Enum.sort_by(fn c -> -(c["confidence"] || 0) end)
  end

  @doc "Retourne le texte de caractère généré."
  def character_text do
    load() |> Map.get("character_text", "")
  end

  @doc "Met à jour le texte de caractère."
  def set_character_text(text) when is_binary(text) do
    state = load()
    save(%{state | "character_text" => text, "last_evolved" => DateTime.utc_now() |> DateTime.to_iso8601()})
  end

  @doc "Applique le déclin temporel aux convictions non renforcées."
  def apply_decay do
    state = load()
    now = DateTime.utc_now()

    convictions =
      state["convictions"]
      |> Enum.map(fn c ->
        reinforced = parse_datetime(c["reinforced_at"])

        if reinforced do
          days_since = DateTime.diff(now, reinforced, :second) / 86400

          if days_since > @decay_days do
            %{c | "confidence" => max((c["confidence"] || 0.3) - 0.05, 0.0)}
          else
            c
          end
        else
          c
        end
      end)
      |> Enum.reject(fn c -> (c["confidence"] || 0) <= 0.0 end)

    save(%{state | "convictions" => convictions})
  end

  @doc "Liste toutes les convictions pour le CLI."
  def list_all do
    load() |> Map.get("convictions", [])
  end

  @doc "Texte pour injection dans le system prompt."
  def to_prompt_text do
    convictions = mature(0.4)
    char_text = character_text()

    parts = []

    parts =
      if convictions != [] do
        lines =
          convictions
          |> Enum.take(10)
          |> Enum.map(fn c ->
            strength = cond do
              c["confidence"] >= 0.8 -> "conviction profonde"
              c["confidence"] >= 0.6 -> "conviction forte"
              c["confidence"] >= 0.4 -> "conviction naissante"
              true -> "intuition"
            end
            "- #{c["belief"]} (#{strength})"
          end)
          |> Enum.join("\n")

        parts ++ ["\nMes convictions :\n#{lines}"]
      else
        parts
      end

    parts =
      if char_text != "" do
        parts ++ ["\nMon caractère forgé par l'expérience :\n#{char_text}"]
      else
        parts
      end

    Enum.join(parts, "\n")
  end

  # -- Privé --

  defp find_similar(convictions, new_belief) do
    new_words = new_belief |> String.downcase() |> String.split(~r/\s+/) |> MapSet.new()

    convictions
    |> Enum.with_index()
    |> Enum.find_value(:not_found, fn {c, idx} ->
      existing_words = (c["belief"] || "") |> String.downcase() |> String.split(~r/\s+/) |> MapSet.new()
      common = MapSet.intersection(new_words, existing_words) |> MapSet.size()
      total = max(MapSet.size(new_words), MapSet.size(existing_words))

      if total > 0 and common / total > 0.5 do
        {:found, idx}
      else
        nil
      end
    end)
  end

  defp parse_datetime(nil), do: nil
  defp parse_datetime(str) when is_binary(str) do
    case DateTime.from_iso8601(str) do
      {:ok, dt, _} -> dt
      _ -> nil
    end
  end
end
