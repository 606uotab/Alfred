defmodule Alfred.Projects.Note do
  @moduledoc """
  Gestion des notes par projet.
  """

  alias Alfred.Storage.Local, as: Storage

  @storage_file "notes.json"

  defstruct [:id, :project, :text, :created_at]

  def add(project, text) do
    notes = list_raw()
    next_id = next_id(notes)

    note = %{
      "id" => next_id,
      "project" => project,
      "text" => text,
      "created_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    Storage.write(@storage_file, notes ++ [note])
    {:ok, to_struct(note)}
  end

  def list_all do
    list_raw() |> Enum.map(&to_struct/1)
  end

  def list_for_project(project) do
    list_raw()
    |> Enum.filter(&(&1["project"] == project))
    |> Enum.map(&to_struct/1)
  end

  def count_for_project(project) do
    list_raw() |> Enum.count(&(&1["project"] == project))
  end

  def delete_for_project(project) do
    notes = list_raw()
    updated = Enum.reject(notes, &(&1["project"] == project))
    Storage.write(@storage_file, updated)
  end

  defp list_raw do
    Storage.read(@storage_file)
  end

  defp next_id([]), do: 1

  defp next_id(notes) do
    notes
    |> Enum.map(& &1["id"])
    |> Enum.max()
    |> Kernel.+(1)
  end

  defp to_struct(map) do
    %__MODULE__{
      id: map["id"],
      project: map["project"],
      text: map["text"],
      created_at: map["created_at"]
    }
  end
end
