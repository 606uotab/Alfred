defmodule Alfred.Chat.Tools do
  @moduledoc """
  Outils qu'Alfred peut exécuter pendant une conversation.
  Mistral décide quand appeler un outil via function calling.
  """

  alias Alfred.Projects.{Manager, Task, Note}

  # Commands safe to run via capture (no interactive input needed)
  @safe_commands ~w(
    project task note status dashboard health remind
    culture search briefing suggest summarize prioritize
    think cortex arms memory matrix daemon soul
  )

  def definitions do
    [
      # -- Actions précises --
      tool("note_add",
        "Ajouter une note à un projet",
        %{
          project: %{type: "string", description: "Nom du projet"},
          text: %{type: "string", description: "Contenu de la note"}
        },
        ["project", "text"]
      ),
      tool("task_add",
        "Ajouter une tâche à un projet",
        %{
          project: %{type: "string", description: "Nom du projet"},
          description: %{type: "string", description: "Description de la tâche"}
        },
        ["project", "description"]
      ),
      tool("task_done",
        "Marquer une tâche comme accomplie par son numéro",
        %{
          id: %{type: "integer", description: "Numéro de la tâche"}
        },
        ["id"]
      ),
      tool("remind_set",
        "Programmer un rappel pour plus tard",
        %{
          project: %{type: "string", description: "Nom du projet"},
          text: %{type: "string", description: "Description du rappel"},
          delay: %{type: "string", description: "Délai avant le rappel (ex: 30m, 2h, 1d, 1j)"}
        },
        ["project", "text", "delay"]
      ),
      tool("project_create",
        "Créer un nouveau projet",
        %{
          name: %{type: "string", description: "Nom du projet"}
        },
        ["name"]
      ),

      # -- Outil générique pour toute commande Alfred --
      tool("alfred_command",
        "Exécuter une commande Alfred et retourner le résultat. " <>
        "Commandes disponibles : project list, task list [projet], note list [projet], " <>
        "status, dashboard, health, remind list, " <>
        "culture list, culture search <mots>, " <>
        "memory facts, memory search <mots>, memory episodes, " <>
        "search <mots>, briefing, suggest, summarize <projet>, prioritize <projet>, " <>
        "think about <projet>, think culture, " <>
        "cortex trends/stats/analyze/productivity/culture/correlations, " <>
        "arms status/disk/memory/backup",
        %{
          command: %{
            type: "string",
            description: "Commande Alfred sans le préfixe 'alfred' (ex: 'task list', 'status', 'health', 'culture search orchidées')"
          }
        },
        ["command"]
      )
    ]
  end

  # -- Execution --

  def execute("note_add", args) do
    project = args["project"]
    text = args["text"]

    if Manager.exists?(project) do
      {:ok, _} = Note.add(project, text)
      "Note ajoutée au projet \"#{project}\"."
    else
      "Le projet \"#{project}\" n'existe pas. Projets disponibles : #{project_names()}."
    end
  rescue
    e -> "Erreur : #{Exception.message(e)}"
  end

  def execute("task_add", args) do
    project = args["project"]
    description = args["description"]

    if Manager.exists?(project) do
      {:ok, task} = Task.add(project, description)
      "Tâche ##{task.id} ajoutée au projet \"#{project}\"."
    else
      "Le projet \"#{project}\" n'existe pas. Projets disponibles : #{project_names()}."
    end
  rescue
    e -> "Erreur : #{Exception.message(e)}"
  end

  def execute("task_done", args) do
    id = if is_binary(args["id"]), do: String.to_integer(args["id"]), else: args["id"]

    case Task.complete(id) do
      {:ok, task} -> "Tâche ##{id} \"#{task.description}\" marquée comme accomplie."
      {:error, :not_found} -> "Tâche ##{id} introuvable."
      {:error, :already_done} -> "Tâche ##{id} est déjà accomplie."
    end
  rescue
    e -> "Erreur : #{Exception.message(e)}"
  end

  def execute("remind_set", args) do
    project = args["project"]
    text = args["text"]
    delay = args["delay"]

    seconds = parse_delay(delay)

    if seconds > 0 do
      due_at = System.system_time(:second) + seconds
      {:ok, id} = :alfred_scheduler.add_reminder(project, text, due_at)
      "Rappel ##{id} programmé : \"#{text}\" dans #{delay}."
    else
      "Délai invalide : #{delay}. Utilisez 30m, 2h, 1d."
    end
  rescue
    e -> "Erreur : #{Exception.message(e)}"
  end

  def execute("project_create", args) do
    name = args["name"]

    case Manager.create(name) do
      {:ok, _} -> "Projet \"#{name}\" créé."
      {:error, :already_exists} -> "Le projet \"#{name}\" existe déjà."
    end
  rescue
    e -> "Erreur : #{Exception.message(e)}"
  end

  def execute("alfred_command", args) do
    command = args["command"] || ""
    cli_args = String.split(command)

    first = List.first(cli_args) || ""

    if first in @safe_commands do
      run_cli_command(cli_args)
    else
      "Commande non disponible en conversation : #{command}. " <>
        "Commandes accessibles : status, task list, note list, health, remind list, culture list, search, etc."
    end
  rescue
    e -> "Erreur : #{Exception.message(e)}"
  end

  def execute(name, _args), do: "Outil inconnu : #{name}"

  # -- Generic CLI execution --

  defp run_cli_command(args) do
    {:ok, string_io} = StringIO.open("")

    original_gl = Process.group_leader()
    Process.group_leader(self(), string_io)

    try do
      Alfred.CLI.main(args)
    rescue
      _ -> :ok
    after
      Process.group_leader(self(), original_gl)
    end

    {_input, output} = StringIO.contents(string_io)
    StringIO.close(string_io)

    output
    |> strip_ansi()
    |> String.trim()
    |> case do
      "" -> "Commande exécutée (pas de sortie)."
      text -> text
    end
  rescue
    _ -> "Erreur lors de l'exécution de la commande."
  end

  # -- Helpers --

  defp tool(name, description, properties, required) do
    %{
      type: "function",
      function: %{
        name: name,
        description: description,
        parameters: %{
          type: "object",
          properties: properties,
          required: required
        }
      }
    }
  end

  defp project_names do
    Manager.list() |> Enum.map_join(", ", & &1.name)
  end

  defp parse_delay(delay) do
    case Regex.run(~r/^(\d+)\s*(m|min|h|d|j)/, String.downcase(delay)) do
      [_, num, unit] ->
        n = String.to_integer(num)

        case unit do
          u when u in ["m", "min"] -> n * 60
          "h" -> n * 3600
          u when u in ["d", "j"] -> n * 86400
          _ -> 0
        end

      _ ->
        0
    end
  end

  defp strip_ansi(text) do
    Regex.replace(~r/\e\[[0-9;]*m/, text, "")
  end
end
