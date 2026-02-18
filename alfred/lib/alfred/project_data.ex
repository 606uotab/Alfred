defmodule Alfred.ProjectData do
  @moduledoc """
  Construction de données projet pour les organes externes (R, Julia).
  Évite la duplication dans cli.ex, brain/commands.ex, cortex/commands.ex.
  """

  alias Alfred.Projects.Manager, as: Projects
  alias Alfred.Projects.Task, as: Tasks
  alias Alfred.Projects.Note, as: Notes

  @doc """
  Version légère pour startup/dashboard/cortex — tâches + rappels, pas de notes.
  """
  def build_for_startup(project) do
    tasks = Tasks.list_for_project(project.name)

    reminders =
      case :alfred_scheduler.list_reminders() do
        {:ok, rs} ->
          rs
          |> Enum.filter(&(&1.project == project.name))
          |> Enum.map(fn r ->
            %{"status" => Atom.to_string(r.status), "due_at" => r.due_at, "text" => r.text}
          end)

        _ ->
          []
      end

    %{
      "name" => project.name,
      "tasks" =>
        Enum.map(tasks, fn t ->
          %{"status" => t.status, "priority" => t.priority, "description" => t.description}
        end),
      "reminders" => reminders
    }
  end

  @doc """
  Version complète pour brain — tâches + notes + rappels.
  """
  def build_for_brain(project) do
    base = build_for_startup(project)
    notes = Notes.list_for_project(project.name)

    Map.put(base, "notes",
      Enum.map(notes, fn n -> %{"text" => n.text, "created_at" => n.created_at} end)
    )
  end

  @doc """
  Collecte tous les projets en version startup.
  """
  def all_for_startup do
    Projects.list() |> Enum.map(&build_for_startup/1)
  end

  @doc """
  Collecte tous les projets en version brain.
  """
  def all_for_brain do
    Projects.list() |> Enum.map(&build_for_brain/1)
  end
end
