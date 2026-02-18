defmodule Alfred.Chat.Tools do
  @moduledoc """
  Outils qu'Alfred peut exécuter pendant une conversation.
  Mistral décide quand appeler un outil via function calling.
  """

  alias Alfred.Projects.{Manager, Task, Note}

  def definitions do
    [
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
      tool("project_list",
        "Lister tous les projets existants",
        %{},
        []
      ),
      tool("task_list",
        "Lister les tâches (toutes ou par projet)",
        %{
          project: %{type: "string", description: "Nom du projet (optionnel)"}
        },
        []
      ),
      tool("project_create",
        "Créer un nouveau projet",
        %{
          name: %{type: "string", description: "Nom du projet"}
        },
        ["name"]
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

  def execute("project_list", _args) do
    projects = Manager.list()

    if Enum.empty?(projects) do
      "Aucun projet en cours."
    else
      projects
      |> Enum.map(fn p ->
        tasks = Task.count_pending_for_project(p.name)
        "- #{p.name} (#{tasks} tâches en attente)"
      end)
      |> Enum.join("\n")
    end
  rescue
    e -> "Erreur : #{Exception.message(e)}"
  end

  def execute("task_list", args) do
    project = args["project"]

    tasks =
      if project && project != "" && Manager.exists?(project) do
        Task.list_for_project(project)
      else
        Task.list_all()
      end

    if Enum.empty?(tasks) do
      "Aucune tâche."
    else
      tasks
      |> Enum.map(fn t ->
        status = if t.status == "done", do: "accomplie", else: "en attente"
        "##{t.id} [#{status}] #{t.description} (#{t.project})"
      end)
      |> Enum.join("\n")
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

  def execute(name, _args), do: "Outil inconnu : #{name}"

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
end
