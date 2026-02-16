defmodule AlfredTest do
  use ExUnit.Case

  alias Alfred.Projects.Manager, as: Projects
  alias Alfred.Projects.Task, as: Tasks
  alias Alfred.Projects.Note, as: Notes

  setup do
    data_dir = Alfred.data_dir()
    File.rm_rf!(data_dir)
    Alfred.Storage.Local.ensure_data_dir!()
    :ok
  end

  # -- Projects --

  describe "Projects.Manager" do
    test "create a project" do
      assert {:ok, project} = Projects.create("Test Project")
      assert project.name == "Test Project"
    end

    test "cannot create duplicate project" do
      {:ok, _} = Projects.create("Dup")
      assert {:error, :already_exists} = Projects.create("Dup")
    end

    test "list projects" do
      {:ok, _} = Projects.create("A")
      {:ok, _} = Projects.create("B")
      projects = Projects.list()
      assert length(projects) == 2
      assert Enum.map(projects, & &1.name) == ["A", "B"]
    end

    test "exists?" do
      {:ok, _} = Projects.create("Exists")
      assert Projects.exists?("Exists")
      refute Projects.exists?("Nope")
    end

    test "delete project and its tasks/notes" do
      {:ok, _} = Projects.create("ToDelete")
      Tasks.add("ToDelete", "A task")
      Notes.add("ToDelete", "A note")

      assert :ok = Projects.delete("ToDelete")
      refute Projects.exists?("ToDelete")
      assert Tasks.list_for_project("ToDelete") == []
      assert Notes.list_for_project("ToDelete") == []
    end

    test "delete non-existent project" do
      assert {:error, :not_found} = Projects.delete("Ghost")
    end
  end

  # -- Tasks --

  describe "Projects.Task" do
    setup do
      {:ok, _} = Projects.create("TaskProj")
      :ok
    end

    test "add and list tasks" do
      {:ok, task} = Tasks.add("TaskProj", "Do something")
      assert task.id == 1
      assert task.description == "Do something"
      assert task.status == "pending"
      assert task.priority == 1

      tasks = Tasks.list_for_project("TaskProj")
      assert length(tasks) == 1
    end

    test "complete a task" do
      {:ok, _} = Tasks.add("TaskProj", "Finish me")
      {:ok, done} = Tasks.complete(1)
      assert done.status == "done"
    end

    test "cannot complete already done task" do
      {:ok, _} = Tasks.add("TaskProj", "Already done")
      {:ok, _} = Tasks.complete(1)
      assert {:error, :already_done} = Tasks.complete(1)
    end

    test "complete non-existent task" do
      assert {:error, :not_found} = Tasks.complete(999)
    end

    test "set priority" do
      {:ok, _} = Tasks.add("TaskProj", "Important")
      {:ok, updated} = Tasks.set_priority(1, 5)
      assert updated.priority == 5
    end

    test "count tasks" do
      {:ok, _} = Tasks.add("TaskProj", "A")
      {:ok, _} = Tasks.add("TaskProj", "B")
      {:ok, _} = Tasks.complete(1)

      assert Tasks.count_for_project("TaskProj") == 2
      assert Tasks.count_pending_for_project("TaskProj") == 1
    end

    test "auto-incrementing IDs" do
      {:ok, t1} = Tasks.add("TaskProj", "First")
      {:ok, t2} = Tasks.add("TaskProj", "Second")
      {:ok, t3} = Tasks.add("TaskProj", "Third")
      assert t1.id == 1
      assert t2.id == 2
      assert t3.id == 3
    end
  end

  # -- Notes --

  describe "Projects.Note" do
    setup do
      {:ok, _} = Projects.create("NoteProj")
      :ok
    end

    test "add and list notes" do
      {:ok, note} = Notes.add("NoteProj", "Remember this")
      assert note.text == "Remember this"
      assert note.project == "NoteProj"

      notes = Notes.list_for_project("NoteProj")
      assert length(notes) == 1
    end

    test "list all notes across projects" do
      {:ok, _} = Projects.create("Other")
      {:ok, _} = Notes.add("NoteProj", "Note A")
      {:ok, _} = Notes.add("Other", "Note B")

      all = Notes.list_all()
      assert length(all) == 2
    end

    test "count notes" do
      {:ok, _} = Notes.add("NoteProj", "A")
      {:ok, _} = Notes.add("NoteProj", "B")
      assert Notes.count_for_project("NoteProj") == 2
    end
  end

  # -- Storage --

  describe "Storage.Local" do
    test "read returns empty list for non-existent file" do
      assert Alfred.Storage.Local.read("nonexistent.json") == []
    end

    test "write and read roundtrip" do
      data = [%{"key" => "value"}]
      Alfred.Storage.Local.write("test.json", data)
      assert Alfred.Storage.Local.read("test.json") == data
    end
  end

  # -- CLI --

  describe "CLI" do
    test "greet outputs something" do
      output = ExUnit.CaptureIO.capture_io(fn -> Alfred.CLI.main([]) end)
      assert output =~ "Alfred"
      assert output =~ "Monsieur"
    end

    test "help outputs commands" do
      output = ExUnit.CaptureIO.capture_io(fn -> Alfred.CLI.main(["help"]) end)
      assert output =~ "project new"
      assert output =~ "task add"
      assert output =~ "note add"
    end

    test "unknown command" do
      output = ExUnit.CaptureIO.capture_io(fn -> Alfred.CLI.main(["xyz"]) end)
      assert output =~ "ne comprends pas"
    end

    test "full workflow via CLI" do
      output = ExUnit.CaptureIO.capture_io(fn -> Alfred.CLI.main(["project", "new", "CLI", "Test"]) end)
      assert output =~ "CLI Test"
      assert output =~ "créé"

      output = ExUnit.CaptureIO.capture_io(fn -> Alfred.CLI.main(["task", "add", "CLI Test", "Do", "it"]) end)
      assert output =~ "Tâche #1"
      assert output =~ "CLI Test"

      output = ExUnit.CaptureIO.capture_io(fn -> Alfred.CLI.main(["status"]) end)
      assert output =~ "CLI Test"
      assert output =~ "1 en attente"
    end
  end
end
