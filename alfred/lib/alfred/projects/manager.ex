defmodule Alfred.Projects.Manager do
  @moduledoc """
  Gestion des projets — création, listing, suppression.
  """

  alias Alfred.Storage.Local, as: Storage

  @storage_file "projects.json"

  defstruct [:name, :created_at]

  def create(name) do
    projects = list_raw()

    if Enum.any?(projects, &(&1["name"] == name)) do
      {:error, :already_exists}
    else
      project = %{
        "name" => name,
        "created_at" => DateTime.utc_now() |> DateTime.to_iso8601()
      }

      Storage.write(@storage_file, projects ++ [project])
      {:ok, to_struct(project)}
    end
  end

  def list do
    list_raw() |> Enum.map(&to_struct/1)
  end

  def exists?(name) do
    list_raw() |> Enum.any?(&(&1["name"] == name))
  end

  def delete(name) do
    projects = list_raw()

    if Enum.any?(projects, &(&1["name"] == name)) do
      updated = Enum.reject(projects, &(&1["name"] == name))
      Storage.write(@storage_file, updated)
      # Also clean up tasks and notes for this project
      Alfred.Projects.Task.delete_for_project(name)
      Alfred.Projects.Note.delete_for_project(name)
      :ok
    else
      {:error, :not_found}
    end
  end

  defp list_raw do
    Storage.read(@storage_file)
  end

  defp to_struct(map) do
    %__MODULE__{
      name: map["name"],
      created_at: map["created_at"]
    }
  end
end
