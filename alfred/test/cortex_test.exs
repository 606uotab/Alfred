defmodule Alfred.CortexTest do
  use ExUnit.Case

  setup do
    memory_dir = Path.join([System.user_home!(), ".alfred", "data", "memory"])
    File.rm_rf(memory_dir)
    Alfred.Storage.Local.ensure_data_dir!()

    # Ensure scheduler is running for CLI tests
    case Process.whereis(Alfred.Supervisor) do
      nil -> :ok
      pid when is_pid(pid) ->
        if Process.alive?(pid), do: Supervisor.stop(pid)
    end

    case GenServer.whereis(:alfred_scheduler) do
      nil -> :ok
      pid when is_pid(pid) ->
        if Process.alive?(pid), do: GenServer.stop(pid)
    end

    {:ok, _} = Alfred.Application.start()
    :ok
  end

  describe "Alfred.Cortex.Port" do
    test "interaction_trends with episodes" do
      episodes = [
        %{"message_count" => 8, "mode" => "chat", "topics" => ["elixir"]},
        %{"message_count" => 4, "mode" => "ask", "topics" => ["tests"]},
        %{"message_count" => 12, "mode" => "chat", "topics" => ["elixir", "design"]}
      ]

      result = Alfred.Cortex.Port.send_command(%{cmd: "interaction_trends", episodes: episodes})
      assert {:ok, resp} = result
      assert resp["trends"]["total_conversations"] == 3
      assert resp["trends"]["avg_messages_per_session"] == 8.0
    end

    test "memory_stats with facts" do
      facts = [
        %{"category" => "preferences", "confidence" => 0.8, "access_count" => 3},
        %{"category" => "knowledge", "confidence" => 0.6, "access_count" => 0}
      ]

      result = Alfred.Cortex.Port.send_command(%{cmd: "memory_stats", facts: facts})
      assert {:ok, resp} = result
      assert resp["stats"]["total_facts"] == 2
    end

    test "behavioral_analysis" do
      episodes = [
        %{"message_count" => 4, "topics" => ["a"]},
        %{"message_count" => 8, "topics" => ["a", "b"]},
        %{"message_count" => 12, "topics" => ["a"]}
      ]

      facts = [%{"category" => "test"}]

      result =
        Alfred.Cortex.Port.send_command(%{
          cmd: "behavioral_analysis",
          episodes: episodes,
          facts: facts
        })

      assert {:ok, resp} = result
      assert is_list(resp["insights"])
      assert length(resp["insights"]) > 0
    end

    test "empty data" do
      result = Alfred.Cortex.Port.send_command(%{cmd: "interaction_trends", episodes: []})
      assert {:ok, resp} = result
      assert resp["trends"]["total_conversations"] == 0
    end
  end

  describe "Alfred.Cortex.Port productivity_stats" do
    test "productivity_stats with projects" do
      projects = [
        %{
          "name" => "Alpha",
          "tasks" => [
            %{"status" => "done", "priority" => 1, "description" => "A"},
            %{"status" => "done", "priority" => 2, "description" => "B"},
            %{"status" => "pending", "priority" => 4, "description" => "C"}
          ],
          "reminders" => [
            %{"status" => "pending", "due_at" => 0, "text" => "Overdue"}
          ]
        },
        %{
          "name" => "Beta",
          "tasks" => [
            %{"status" => "pending", "priority" => 1, "description" => "D"}
          ],
          "reminders" => []
        }
      ]

      result = Alfred.Cortex.Port.send_command(%{cmd: "productivity_stats", projects: projects})
      assert {:ok, resp} = result
      stats = resp["stats"]
      assert stats["total_done"] == 2
      assert stats["total_pending"] == 2
      assert stats["overall_completion"] == 50.0
      assert stats["overdue_count"] == 1
      assert is_binary(stats["one_liner"])
      assert is_list(stats["project_health"])
    end

    test "productivity_stats with empty projects" do
      result = Alfred.Cortex.Port.send_command(%{cmd: "productivity_stats", projects: []})
      assert {:ok, resp} = result
      assert resp["stats"]["overall_completion"] == 0
    end
  end

  describe "Alfred.Cortex.Port culture_trends" do
    test "culture_trends with entries" do
      culture = [
        %{"topic" => "botanique", "content" => "Orchidées", "source" => %{"type" => "person", "name" => "Annie"}, "tags" => ["plantes"], "learned_at" => "2026-02-10T10:00:00Z"},
        %{"topic" => "botanique", "content" => "Roses", "source" => %{"type" => "person", "name" => "Annie"}, "tags" => ["plantes", "jardin"], "learned_at" => "2026-02-15T10:00:00Z"},
        %{"topic" => "cuisine", "content" => "Risotto", "source" => %{"type" => "book", "name" => "Recettes"}, "tags" => ["recettes"], "learned_at" => "2026-02-17T10:00:00Z"}
      ]

      result = Alfred.Cortex.Port.send_command(%{cmd: "culture_trends", culture: culture})
      assert {:ok, resp} = result
      trends = resp["trends"]
      assert trends["total"] == 3
      assert trends["topic_count"] == 2
      assert is_binary(trends["one_liner"])
      assert trends["growth_trend"] in ["stable", "accelerating", "decelerating"]
    end

    test "culture_trends with empty culture" do
      result = Alfred.Cortex.Port.send_command(%{cmd: "culture_trends", culture: []})
      assert {:ok, resp} = result
      assert resp["trends"]["total"] == 0
    end
  end

  describe "Alfred.Cortex.Port correlation_analysis" do
    test "correlation_analysis detects shared topics" do
      episodes = [
        %{"message_count" => 5, "topics" => ["botanique", "jardin"]},
        %{"message_count" => 3, "topics" => ["cuisine"]}
      ]

      projects = [
        %{"name" => "Jardin", "notes" => [%{"text" => "Notes sur le jardin botanique"}]}
      ]

      culture = [
        %{"topic" => "botanique", "content" => "Orchidées", "tags" => ["plantes"]}
      ]

      result = Alfred.Cortex.Port.send_command(%{
        cmd: "correlation_analysis",
        episodes: episodes,
        projects: projects,
        culture: culture
      })

      assert {:ok, resp} = result
      assert is_list(resp["correlations"])
      assert is_list(resp["insights"])
      assert is_binary(resp["one_liner"])
    end

    test "correlation_analysis with empty data" do
      result = Alfred.Cortex.Port.send_command(%{
        cmd: "correlation_analysis",
        episodes: [],
        projects: [],
        culture: []
      })

      assert {:ok, resp} = result
      assert is_list(resp["insights"])
    end
  end

  describe "alfred_health cortex and mistral checks" do
    test "cortex check returns R status" do
      info = :alfred_health.check_cortex()
      assert is_map(info)
      assert Map.has_key?(info, :r_found)
    end

    test "mistral check returns status" do
      info = :alfred_health.check_mistral()
      assert is_map(info)
      assert Map.has_key?(info, :configured)
    end

    test "check_all includes cortex and mistral" do
      checks = :alfred_health.check_all()
      components = Enum.map(checks, fn {name, _} -> name end)
      assert :cortex in components
      assert :mistral in components
    end
  end
end
