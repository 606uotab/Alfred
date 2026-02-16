defmodule Alfred.Projects.Task do
  @moduledoc """
  Gestion des tâches par projet.
  """

  alias Alfred.Storage.Local, as: Storage

  @storage_file "tasks.json"

  defstruct [:id, :project, :description, :status, :priority, :created_at]

  def add(project, description) do
    tasks = list_raw()
    next_id = next_id(tasks)

    task = %{
      "id" => next_id,
      "project" => project,
      "description" => description,
      "status" => "pending",
      "priority" => 1,
      "created_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    Storage.write(@storage_file, tasks ++ [task])
    {:ok, to_struct(task)}
  end

  def list_all do
    list_raw() |> Enum.map(&to_struct/1)
  end

  def list_for_project(project) do
    list_raw()
    |> Enum.filter(&(&1["project"] == project))
    |> Enum.map(&to_struct/1)
  end

  def complete(id) do
    tasks = list_raw()

    case Enum.find_index(tasks, &(&1["id"] == id)) do
      nil ->
        {:error, :not_found}

      idx ->
        task = Enum.at(tasks, idx)

        if task["status"] == "done" do
          {:error, :already_done}
        else
          updated_task = Map.put(task, "status", "done")
          updated_tasks = List.replace_at(tasks, idx, updated_task)
          Storage.write(@storage_file, updated_tasks)
          {:ok, to_struct(updated_task)}
        end
    end
  end

  def set_priority(id, priority) do
    tasks = list_raw()

    case Enum.find_index(tasks, &(&1["id"] == id)) do
      nil ->
        {:error, :not_found}

      idx ->
        task = Enum.at(tasks, idx)
        updated_task = Map.put(task, "priority", priority)
        updated_tasks = List.replace_at(tasks, idx, updated_task)
        Storage.write(@storage_file, updated_tasks)
        {:ok, to_struct(updated_task)}
    end
  end

  def count_for_project(project) do
    list_raw() |> Enum.count(&(&1["project"] == project))
  end

  def count_pending_for_project(project) do
    list_raw()
    |> Enum.count(&(&1["project"] == project and &1["status"] == "pending"))
  end

  def delete_for_project(project) do
    tasks = list_raw()
    updated = Enum.reject(tasks, &(&1["project"] == project))
    Storage.write(@storage_file, updated)
  end

  defp list_raw do
    Storage.read(@storage_file)
  end

  defp next_id([]), do: 1

  defp next_id(tasks) do
    tasks
    |> Enum.map(& &1["id"])
    |> Enum.max()
    |> Kernel.+(1)
  end

  defp to_struct(map) do
    %__MODULE__{
      id: map["id"],
      project: map["project"],
      description: map["description"],
      status: map["status"],
      priority: map["priority"] || 1,
      created_at: map["created_at"]
    }
  end
end
