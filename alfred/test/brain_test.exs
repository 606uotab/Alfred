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

  describe "Alfred.Brain.Port briefing" do
    test "briefing command works with empty data" do
      result = Alfred.Brain.Port.send_command(%{
        cmd: "briefing",
        projects: [],
        reminders: [],
        culture: [],
        patterns: [],
        last_episode: nil,
        now: System.system_time(:second)
      })

      assert {:ok, resp} = result
      assert resp["status"] == "ok"
      assert is_list(resp["sections"])
      assert is_binary(resp["conclusion"])
    end

    test "briefing command works with projects and reminders" do
      result = Alfred.Brain.Port.send_command(%{
        cmd: "briefing",
        projects: [
          %{
            "name" => "Projet X",
            "tasks" => [
              %{"status" => "pending", "priority" => 5, "description" => "Urgent task", "created_at" => "2026-02-10T10:00:00Z"},
              %{"status" => "done", "priority" => 1, "description" => "Done task"}
            ],
            "notes" => [],
            "reminders" => []
          }
        ],
        reminders: [
          %{"project" => "Projet X", "text" => "Rappel test", "status" => "pending", "due_at" => 0}
        ],
        culture: [
          %{"topic" => "botanique", "content" => "Les orchidées", "source" => %{"type" => "person", "name" => "Annie"}, "tags" => [], "learned_at" => "2026-02-17T10:00:00Z"}
        ],
        patterns: [
          %{"description" => "Monsieur travaille le matin", "pattern_type" => "behavioral", "confidence" => 0.8}
        ],
        last_episode: %{"summary" => "Discussion sur le projet", "message_count" => 6, "mode" => "chat"},
        now: System.system_time(:second)
      })

      assert {:ok, resp} = result
      assert resp["total_pending"] == 1
      assert resp["urgent_count"] == 1
      assert resp["overdue_reminders"] == 1
      assert length(resp["sections"]) == 5
    end
  end

  describe "Alfred.Brain.Commands briefing CLI" do
    test "handle_briefing runs without crash" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Brain.Commands.handle_briefing()
        end)

      assert output =~ "briefing"
    end

    test "CLI routing for briefing" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.CLI.main(["briefing"])
        end)

      assert output =~ "briefing"
    end
  end

  describe "Alfred.Brain.Port analyze_culture" do
    test "analyze_culture with empty culture" do
      result = Alfred.Brain.Port.send_command(%{
        cmd: "analyze_culture",
        culture: [],
        now: System.system_time(:second)
      })

      assert {:ok, resp} = result
      assert resp["status"] == "ok"
      assert resp["stats"]["total"] == 0
      assert is_list(resp["insights"])
      assert Enum.any?(resp["insights"], &(&1 =~ "vierge"))
    end

    test "analyze_culture with rich data" do
      culture = [
        %{
          "topic" => "botanique",
          "content" => "Les orchidées ont besoin de lumière",
          "source" => %{"type" => "person", "name" => "Annie"},
          "tags" => ["plantes", "entretien"],
          "learned_at" => "2026-02-17T10:00:00Z"
        },
        %{
          "topic" => "botanique",
          "content" => "Les roses aiment le soleil",
          "source" => %{"type" => "person", "name" => "Annie"},
          "tags" => ["plantes", "jardin"],
          "learned_at" => "2026-02-16T10:00:00Z"
        },
        %{
          "topic" => "cuisine",
          "content" => "Le risotto demande de la patience",
          "source" => %{"type" => "book", "name" => "L'Art Culinaire"},
          "tags" => ["recettes"],
          "learned_at" => "2026-01-10T10:00:00Z"
        },
        %{
          "topic" => "histoire",
          "content" => "La Révolution française a commencé en 1789",
          "source" => %{"type" => "observation", "name" => ""},
          "tags" => ["plantes"],
          "learned_at" => "2026-02-15T10:00:00Z"
        }
      ]

      result = Alfred.Brain.Port.send_command(%{
        cmd: "analyze_culture",
        culture: culture,
        now: System.system_time(:second)
      })

      assert {:ok, resp} = result
      assert resp["stats"]["total"] == 4
      assert resp["stats"]["topic_count"] == 3
      assert is_list(resp["topics"])
      assert length(resp["topics"]) == 3
      assert is_list(resp["suggestions"])
      # botanique has 2 entries, should be top topic
      top = Enum.at(resp["topics"], 0)
      assert top["name"] == "botanique"
      assert top["count"] == 2
      # connections via "plantes" tag (botanique + histoire)
      assert length(resp["stats"]["connections"]) > 0
    end

    test "analyze_culture detects source concentration" do
      culture = [
        %{"topic" => "A", "content" => "x", "source" => %{"type" => "person", "name" => "Bob"}, "tags" => [], "learned_at" => "2026-02-17T10:00:00Z"},
        %{"topic" => "B", "content" => "y", "source" => %{"type" => "person", "name" => "Bob"}, "tags" => [], "learned_at" => "2026-02-17T10:00:00Z"},
        %{"topic" => "C", "content" => "z", "source" => %{"type" => "person", "name" => "Bob"}, "tags" => [], "learned_at" => "2026-02-17T10:00:00Z"}
      ]

      result = Alfred.Brain.Port.send_command(%{
        cmd: "analyze_culture",
        culture: culture,
        now: System.system_time(:second)
      })

      assert {:ok, resp} = result
      # All from "person" type → insight about diversification
      assert Enum.any?(resp["insights"], &(&1 =~ "diversifier"))
    end
  end

  describe "Alfred.Brain.Commands analyze_culture CLI" do
    test "handle_analyze_culture with data" do
      culture_data = [
        %{
          "topic" => "test",
          "content" => "Contenu test",
          "source" => %{"type" => "person", "name" => "Alice"},
          "tags" => ["demo"],
          "learned_at" => "2026-02-17T10:00:00Z"
        }
      ]

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Brain.Commands.handle_analyze_culture(culture_data)
        end)

      assert output =~ "analyse"
      assert output =~ "culture"
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
