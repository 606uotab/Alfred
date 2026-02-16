defmodule Alfred.BrainTest do
  use ExUnit.Case

  setup do
    # Clean project data
    projects_path = Path.join([System.user_home!(), ".alfred", "data", "projects.json"])
    tasks_path = Path.join([System.user_home!(), ".alfred", "data", "tasks.json"])
    notes_path = Path.join([System.user_home!(), ".alfred", "data", "notes.json"])
    reminder_path = Path.join([System.user_home!(), ".alfred", "data", "reminders.dat"])

    File.rm(projects_path)
    File.rm(tasks_path)
    File.rm(notes_path)
    File.rm(reminder_path)

    # Stop supervisor if running (which also stops scheduler)
    case Process.whereis(Alfred.Supervisor) do
      nil -> :ok
      pid -> Supervisor.stop(pid)
    end

    # Stop standalone scheduler if running
    case GenServer.whereis(:alfred_scheduler) do
      nil -> :ok
      pid -> GenServer.stop(pid)
    end

    # Start via Application (like CLI does)
    Alfred.Storage.Local.ensure_data_dir!()
    {:ok, _} = Alfred.Application.start()
    :ok
  end

  describe "Alfred.Brain.Port" do
    test "analyze command works" do
      project = %{
        "name" => "Test",
        "tasks" => [
          %{"status" => "pending", "priority" => 3, "description" => "Task A"},
          %{"status" => "done", "priority" => 1, "description" => "Task B"}
        ],
        "notes" => [],
        "reminders" => []
      }

      result = Alfred.Brain.Port.send_command(%{cmd: "analyze", project: project, now: System.system_time(:second)})

      assert {:ok, resp} = result
      assert resp["status"] == "ok"
      assert is_list(resp["insights"])
      assert is_map(resp["stats"])
      assert resp["stats"]["total_tasks"] == 2
      assert resp["stats"]["completion_rate"] == 50.0
    end

    test "summarize command works" do
      project = %{
        "name" => "Mon Projet",
        "tasks" => [
          %{"status" => "pending", "priority" => 5, "description" => "Urgent"},
          %{"status" => "done", "priority" => 1, "description" => "Fait"}
        ],
        "notes" => [
          %{"text" => "Note importante sur le projet"}
        ]
      }

      result = Alfred.Brain.Port.send_command(%{cmd: "summarize", project: project})

      assert {:ok, resp} = result
      assert resp["status"] == "ok"
      assert is_binary(resp["summary"])
      assert resp["summary"] =~ "Mon Projet"
      assert resp["summary"] =~ "Urgent"
    end

    test "suggest command works" do
      projects = [
        %{
          "name" => "Projet A",
          "tasks" => [
            %{"status" => "pending", "description" => "X", "priority" => 1}
          ],
          "notes" => []
        },
        %{
          "name" => "Projet B",
          "tasks" => [],
          "notes" => []
        }
      ]

      result = Alfred.Brain.Port.send_command(%{cmd: "suggest", projects: projects, now: System.system_time(:second)})

      assert {:ok, resp} = result
      assert resp["status"] == "ok"
      assert is_list(resp["suggestions"])
      assert length(resp["suggestions"]) > 0
    end

    test "analyze with empty project" do
      project = %{"name" => "Vide", "tasks" => [], "notes" => [], "reminders" => []}

      result = Alfred.Brain.Port.send_command(%{cmd: "analyze", project: project, now: System.system_time(:second)})

      assert {:ok, resp} = result
      assert resp["stats"]["total_tasks"] == 0
      assert Enum.any?(resp["insights"], &(&1 =~ "Aucune tâche"))
    end

    test "analyze with keywords in notes" do
      project = %{
        "name" => "Notes",
        "tasks" => [],
        "notes" => [
          %{"text" => "Architecture du systeme modulaire"},
          %{"text" => "Refactoring architecture composants"},
          %{"text" => "Design architecture finale"}
        ],
        "reminders" => []
      }

      result = Alfred.Brain.Port.send_command(%{cmd: "analyze", project: project, now: System.system_time(:second)})

      assert {:ok, resp} = result
      assert "architecture" in resp["stats"]["keywords"]
    end

    test "unknown command returns error" do
      result = Alfred.Brain.Port.send_command(%{cmd: "invalid"})
      assert {:error, _msg} = result
    end
  end

  describe "Alfred.Brain.Commands (CLI integration)" do
    test "think about existing project" do
      Alfred.Projects.Manager.create("BrainTest")
      Alfred.Projects.Task.add("BrainTest", "First task")

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Brain.Commands.handle_analyze("BrainTest")
        end)

      assert output =~ "analyse"
    end

    test "think about non-existent project" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Brain.Commands.handle_analyze("Inexistant")
        end)

      assert output =~ "n'existe pas"
    end

    test "summarize existing project" do
      Alfred.Projects.Manager.create("SumTest")
      Alfred.Projects.Task.add("SumTest", "A task")
      Alfred.Projects.Note.add("SumTest", "A note about the project")

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Brain.Commands.handle_summarize("SumTest")
        end)

      assert output =~ "résumé"
      assert output =~ "SumTest"
    end

    test "suggest with projects" do
      Alfred.Projects.Manager.create("SuggestA")
      Alfred.Projects.Manager.create("SuggestB")
      Alfred.Projects.Task.add("SuggestA", "Task 1")

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Brain.Commands.handle_suggest()
        end)

      assert output =~ "suggestions"
    end

    test "suggest with no projects" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Brain.Commands.handle_suggest()
        end)

      assert output =~ "suggestions" || output =~ "aucun"
    end

    test "CLI routing for think about" do
      Alfred.Projects.Manager.create("ThinkTest")

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.CLI.main(["think", "about", "ThinkTest"])
        end)

      assert output =~ "analyse" || output =~ "ThinkTest"
    end

    test "CLI routing for summarize" do
      Alfred.Projects.Manager.create("SumRoute")

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.CLI.main(["summarize", "SumRoute"])
        end)

      assert output =~ "résumé" || output =~ "SumRoute"
    end

    test "CLI routing for suggest" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.CLI.main(["suggest"])
        end)

      assert output =~ "suggestions" || output =~ "aucun"
    end
  end

  describe "alfred_health brain check" do
    test "brain check returns julia status" do
      info = :alfred_health.check_brain()
      assert is_map(info)
      assert Map.has_key?(info, :julia_found)
      assert Map.has_key?(info, :script_found)
    end

    test "check_all includes brain" do
      checks = :alfred_health.check_all()
      components = Enum.map(checks, fn {name, _} -> name end)
      assert :brain in components
    end
  end
end
