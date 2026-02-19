defmodule Alfred.Soul.Commands do
  @moduledoc """
  Gestion de l'âme d'Alfred — personnalité profonde et traits vivants.
  """

  alias Alfred.Butler
  alias Alfred.Colors
  alias Alfred.Vault.Port
  alias Alfred.Soul.State

  def handle([]) do
    state = State.load()

    Butler.say("Mon âme, Monsieur :\n")
    IO.puts("  Humeur : #{state.mood}")
    IO.puts("")
    IO.puts(State.personality_summary(state))
    IO.puts("")

    if state.evolution_log != [] do
      last = hd(state.evolution_log)
      IO.puts("  Dernière évolution : #{last["trait"]} #{format_delta(last["delta"])} — #{last["reason"] || "?"}")
      IO.puts("")
    end
  end

  def handle(["init"]) do
    Butler.say("Monsieur, vous allez inscrire mon âme profonde dans le coffre creator.")
    Butler.say("Collez le texte de ma personnalité secrète, puis terminez par une ligne vide :\n")

    text = read_multiline()

    if String.trim(text) == "" do
      Butler.say("Aucun texte reçu, Monsieur. Opération annulée.")
    else
      password = Alfred.Input.prompt_password("Mot de passe maître : ")

      case Port.send_with_unlock("creator", password, %{
             cmd: "store",
             key: "alfred_soul",
             value: text
           }) do
        {:ok, _} ->
          Butler.say(
            "Mon âme est inscrite et chiffrée dans le coffre creator, Monsieur. Personne ne pourra la lire sans votre mot de passe maître."
          )

        {:error, "Wrong password"} ->
          Butler.say("Mot de passe incorrect, Monsieur.")

        {:error, reason} ->
          Butler.say("Je suis navré Monsieur, une erreur est survenue : #{reason}")
      end
    end
  end

  def handle(["check"]) do
    password = Alfred.Input.prompt_password("Mot de passe maître : ")

    case Port.send_with_unlock("creator", password, %{cmd: "get", key: "alfred_soul"}) do
      {:ok, %{"value" => soul}} when byte_size(soul) > 0 ->
        Butler.say(
          "Mon âme est présente dans le coffre creator, Monsieur. (#{String.length(soul)} caractères)"
        )

      {:ok, _} ->
        Butler.say("Mon âme n'a pas encore été inscrite. Utilisez : alfred soul init")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, reason} ->
        Butler.say("Je suis navré Monsieur, une erreur est survenue : #{reason}")
    end
  end

  def handle(["history"]) do
    state = State.load()
    log = Enum.take(state.evolution_log, 10)

    if log == [] do
      Butler.say("Mon âme n'a pas encore évolué, Monsieur.")
    else
      Butler.say("Historique de mon évolution :\n")

      Enum.each(log, fn entry ->
        date = String.slice(entry["at"] || "", 0, 16)
        {label, _, _} = State.trait_labels()[entry["trait"]] || {entry["trait"], "", ""}
        IO.puts("  #{Colors.dim(date)}  #{label} #{format_delta(entry["delta"])}  #{entry["reason"] || ""}")
      end)

      IO.puts("")
    end
  end

  def handle(["reset"]) do
    state = State.load()
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    log_entry = %{
      "trait" => "all",
      "old" => state.traits,
      "new" => State.default_traits(),
      "delta" => 0,
      "reason" => "reset manuel",
      "at" => now
    }

    reset_state = %State{
      traits: State.default_traits(),
      mood: "serein",
      evolution_log: [log_entry | state.evolution_log],
      version: state.version + 1,
      created_at: state.created_at,
      updated_at: now
    }

    State.save(reset_state)
    Butler.say("Mon âme a été réinitialisée, Monsieur. Traits par défaut restaurés.")
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes de l'âme sont :\n")

    IO.puts("""
      alfred soul              Voir mes traits actuels
      alfred soul init         Inscrire mon âme profonde (chiffrée)
      alfred soul check        Vérifier que mon âme est présente
      alfred soul history      Historique de mon évolution
      alfred soul reset        Réinitialiser mes traits
    """)
  end

  # -- Privé --

  defp format_delta(delta) when is_number(delta) do
    if delta >= 0, do: "+#{Float.round(delta + 0.0, 3)}", else: "#{Float.round(delta + 0.0, 3)}"
  end

  defp format_delta(_), do: ""

  defp read_multiline do
    read_multiline([])
  end

  defp read_multiline(lines) do
    case IO.gets("") do
      :eof ->
        Enum.reverse(lines) |> Enum.join("\n")

      "\n" ->
        Enum.reverse(lines) |> Enum.join("\n")

      line ->
        read_multiline([line | lines])
    end
  end
end
