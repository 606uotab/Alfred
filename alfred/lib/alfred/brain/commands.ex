defmodule Alfred.Brain.Commands do
  @moduledoc """
  Commandes d'analyse — Le Cerveau de Alfred réfléchit.
  Utilise le moteur Julia pour l'analyse intelligente.
  """

  alias Alfred.Butler
  alias Alfred.Brain.Port, as: Brain
  alias Alfred.Projects.Manager, as: Projects
  alias Alfred.Projects.Task, as: Tasks
  alias Alfred.Projects.Note, as: Notes

  @doc """
  Analyse approfondie d'un projet.
  """
  def handle_analyze(project_name) do
    unless Projects.exists?(project_name) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project_name}\" n'existe pas.")
    else
      project_data = build_project_data(project_name)

      case Brain.send_command(%{cmd: "analyze", project: project_data, now: now()}) do
        {:ok, resp} ->
          insights = Map.get(resp, "insights", [])
          stats = Map.get(resp, "stats", %{})

          Butler.say("Monsieur, voici mon analyse du projet \"#{project_name}\" :\n")

          Enum.each(insights, fn insight ->
            IO.puts("  💡 #{insight}")
          end)

          IO.puts("")
          print_stats(stats)

        {:error, msg} ->
          Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
      end
    end
  end

  @doc """
  Résumé concis d'un projet.
  """
  def handle_summarize(project_name) do
    unless Projects.exists?(project_name) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project_name}\" n'existe pas.")
    else
      project_data = build_project_data(project_name)

      case Brain.send_command(%{cmd: "summarize", project: project_data}) do
        {:ok, resp} ->
          summary = Map.get(resp, "summary", "")
          Butler.say("Monsieur, voici le résumé :\n")
          IO.puts("  #{String.replace(summary, "\n", "\n  ")}")
          IO.puts("")

        {:error, msg} ->
          Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
      end
    end
  end

  @doc """
  Suggestions transversales sur tous les projets.
  """
  def handle_suggest do
    projects = Projects.list()

    projects_data =
      Enum.map(projects, fn p ->
        build_project_data(p.name)
      end)

    case Brain.send_command(%{cmd: "suggest", projects: projects_data, now: now()}) do
      {:ok, resp} ->
        suggestions = Map.get(resp, "suggestions", [])
        Butler.say("Monsieur, permettez-moi quelques suggestions :\n")

        Enum.each(suggestions, fn s ->
          IO.puts("  🧠 #{s}")
        end)

        IO.puts("")

      {:error, msg} ->
        Butler.say("Je suis navré Monsieur, mon cerveau rencontre une difficulté : #{msg}")
    end
  end

  # -- Helpers --

  defp build_project_data(project_name) do
    tasks = Tasks.list_for_project(project_name)
    notes = Notes.list_for_project(project_name)

    tasks_data =
      Enum.map(tasks, fn t ->
        %{
          "description" => t.description,
          "status" => t.status,
          "priority" => t.priority,
          "created_at" => t.created_at
        }
      end)

    notes_data =
      Enum.map(notes, fn n ->
        %{
          "text" => n.text,
          "created_at" => n.created_at
        }
      end)

    # Get reminders for this project
    reminders_data =
      case :alfred_scheduler.list_reminders() do
        {:ok, reminders} ->
          reminders
          |> Enum.filter(&(&1.project == project_name))
          |> Enum.map(fn r ->
            %{
              "status" => Atom.to_string(r.status),
              "due_at" => r.due_at,
              "text" => r.text
            }
          end)

        _ ->
          []
      end

    %{
      "name" => project_name,
      "tasks" => tasks_data,
      "notes" => notes_data,
      "reminders" => reminders_data
    }
  end

  defp print_stats(stats) when map_size(stats) > 0 do
    IO.puts("  Statistiques :")

    if Map.has_key?(stats, "completion_rate") do
      IO.puts("    Accomplissement : #{stats["completion_rate"]}%")
    end

    if Map.has_key?(stats, "avg_priority") do
      IO.puts("    Priorité moyenne : #{stats["avg_priority"]}")
    end

    if Map.has_key?(stats, "oldest_task_days") do
      IO.puts("    Tâche la plus ancienne : #{stats["oldest_task_days"]} jours")
    end

    if Map.has_key?(stats, "keywords") do
      IO.puts("    Mots-clés : #{Enum.join(stats["keywords"], ", ")}")
    end

    IO.puts("")
  end

  defp print_stats(_), do: :ok

  defp now, do: System.system_time(:second)
end
