defmodule Alfred.Culture.Commands do
  @moduledoc """
  Commandes CLI pour la culture d'Alfred — gestion des connaissances avec sources.
  """

  alias Alfred.Butler
  alias Alfred.Culture

  def handle(["learn", topic | content_parts]) when content_parts != [] do
    content = Enum.join(content_parts, " ")

    Butler.say("Nouvelle connaissance sur '#{topic}', Monsieur.\n")

    # Interactif : demander la source
    source = ask_source()
    tags = ask_tags()
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    case Culture.learn(topic, content, source, tags, password) do
      {:ok, knowledge} ->
        Butler.say("Connaissance enregistrée (#{knowledge.id}), Monsieur.")
        Butler.say("Sujet : #{topic}")
        Butler.say("Source : #{format_source(source)}")

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["search" | words]) when words != [] do
    query = Enum.join(words, " ")
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    case Culture.search(query, password) do
      {:ok, []} ->
        Butler.say("Aucune connaissance trouvée pour '#{query}', Monsieur.")

      {:ok, results} ->
        Butler.say("Monsieur, voici ce que je sais sur '#{query}' :\n")

        Enum.each(results, fn entry ->
          source_str = format_source(entry["source"])
          tags_str = format_tags(entry["tags"])
          IO.puts("  📚 [#{entry["topic"]}] #{entry["content"]}")
          IO.puts("     Source : #{source_str}#{tags_str}")
          IO.puts("")
        end)

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["list"]) do
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    case Culture.list_all(password) do
      {:ok, []} ->
        Butler.say("Ma culture est encore vierge, Monsieur. Utilisez 'alfred culture learn' pour m'enseigner.")

      {:ok, entries} ->
        Butler.say("Monsieur, voici l'ensemble de ma culture (#{length(entries)} connaissances) :\n")

        entries
        |> Enum.group_by(& &1["topic"])
        |> Enum.each(fn {topic, items} ->
          IO.puts("  ── #{topic} (#{length(items)}) ──")

          Enum.each(items, fn entry ->
            source_str = format_source(entry["source"])
            IO.puts("    📚 #{entry["content"]}")
            IO.puts("       Source : #{source_str}")
          end)

          IO.puts("")
        end)

      {:error, "Wrong password"} ->
        Butler.say("Mot de passe incorrect, Monsieur.")

      {:error, msg} ->
        Butler.say("Erreur : #{msg}")
    end
  end

  def handle(["suggestions"]) do
    pending = Alfred.Culture.Suggestions.list_pending()

    if pending == [] do
      Butler.say("Aucune suggestion de culture en attente, Monsieur.")
    else
      Butler.say("Monsieur, #{length(pending)} suggestion(s) de culture à examiner :\n")

      Enum.each(pending, fn s ->
        id = s["id"]
        topic = s["topic"] || "divers"
        content = s["content"] || ""
        confidence = s["confidence"] || 0
        date = String.slice(s["suggested_at"] || "", 0, 10)

        conf_str = "#{trunc(confidence * 100)}%"
        IO.puts("  ##{id} [#{topic}] #{content}")
        IO.puts("     Confiance : #{conf_str} | Date : #{date}")
        IO.puts("")
      end)

      Butler.say("Utilisez 'alfred culture approve <id>' pour approuver ou 'alfred culture dismiss <id>' pour rejeter.")
    end
  end

  def handle(["approve", id_str]) do
    case Integer.parse(id_str) do
      {id, ""} ->
        case Alfred.Culture.Suggestions.get(id) do
          {:error, :not_found} ->
            Butler.say("Suggestion ##{id} introuvable, Monsieur.")

          {:ok, suggestion} ->
            # Need to store in vault
            topic = suggestion["topic"] || "divers"
            content = suggestion["content"] || ""

            Butler.say("Approbation de : [#{topic}] #{content}\n")

            source_type = suggestion["source_type"] || "conversation"
            source = ask_source_for_approval(source_type)
            tags = ask_tags()
            password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

            case Culture.learn(topic, content, source, tags, password) do
              {:ok, knowledge} ->
                Alfred.Culture.Suggestions.approve(id)
                Butler.say("Connaissance enregistrée (#{knowledge.id}) et suggestion approuvée, Monsieur.")

              {:error, "Wrong password"} ->
                Butler.say("Mot de passe incorrect, Monsieur.")

              {:error, msg} ->
                Butler.say("Erreur : #{msg}")
            end
        end

      _ ->
        Butler.say("Numéro invalide, Monsieur.")
    end
  end

  def handle(["dismiss", id_str]) do
    case Integer.parse(id_str) do
      {id, ""} ->
        case Alfred.Culture.Suggestions.dismiss(id) do
          :ok ->
            Butler.say("Suggestion ##{id} rejetée, Monsieur.")

          {:error, :not_found} ->
            Butler.say("Suggestion ##{id} introuvable, Monsieur.")
        end

      _ ->
        Butler.say("Numéro invalide, Monsieur.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes culturelles sont :\n")

    IO.puts("""
      alfred culture learn <sujet> <contenu>   Enseigner une connaissance
      alfred culture search <mots>             Rechercher dans la culture
      alfred culture list                      Lister toutes les connaissances
      alfred culture suggestions               Suggestions auto-extraites
      alfred culture approve <id>              Approuver une suggestion
      alfred culture dismiss <id>              Rejeter une suggestion
    """)
  end

  defp ask_source_for_approval(source_type) do
    Butler.say("Source suggérée : #{source_type}")
    choice = IO.gets("  Garder cette source ? (O/n) : ") |> String.trim() |> String.downcase()

    if choice in ["", "o", "oui", "y", "yes"] do
      now = DateTime.utc_now() |> DateTime.to_iso8601()
      %{type: source_type, name: nil, ref: nil, date: now}
    else
      ask_source()
    end
  end

  # -- Interactive source input --

  defp ask_source do
    IO.puts("  Type de source :")
    IO.puts("    1. Personne")
    IO.puts("    2. Livre")
    IO.puts("    3. Observation")
    IO.puts("    4. Web")
    IO.puts("    5. Autre\n")

    choice = IO.gets("  Choix (1-5) : ") |> String.trim()

    type =
      case choice do
        "1" -> "person"
        "2" -> "book"
        "3" -> "observation"
        "4" -> "web"
        _ -> "other"
      end

    name =
      case type do
        "person" -> IO.gets("  Nom de la personne : ") |> String.trim()
        "book" -> IO.gets("  Titre du livre : ") |> String.trim()
        "web" -> IO.gets("  URL ou site : ") |> String.trim()
        _ -> nil
      end

    ref = IO.gets("  Référence (optionnel, Entrée pour ignorer) : ") |> String.trim()
    ref = if ref == "", do: nil, else: ref

    now = DateTime.utc_now() |> DateTime.to_iso8601()

    %{
      type: type,
      name: name,
      ref: ref,
      date: now
    }
  end

  defp ask_tags do
    input = IO.gets("  Tags (séparés par des virgules, Entrée pour ignorer) : ") |> String.trim()

    if input == "" do
      []
    else
      input
      |> String.split(",", trim: true)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
    end
  end

  # -- Formatting --

  defp format_source(%{"type" => "person", "name" => name}) when is_binary(name),
    do: "#{name} (personne)"

  defp format_source(%{"type" => "book", "name" => name}) when is_binary(name),
    do: "#{name} (livre)"

  defp format_source(%{"type" => "web", "name" => url}) when is_binary(url),
    do: "#{url} (web)"

  defp format_source(%{"type" => "observation"}),
    do: "Observation"

  defp format_source(%{"type" => type}),
    do: type

  defp format_source(%{type: "person", name: name}) when is_binary(name),
    do: "#{name} (personne)"

  defp format_source(%{type: type}),
    do: to_string(type)

  defp format_source(_), do: "inconnue"

  defp format_tags([]), do: ""
  defp format_tags(tags) when is_list(tags), do: " | Tags : #{Enum.join(tags, ", ")}"
  defp format_tags(_), do: ""
end
