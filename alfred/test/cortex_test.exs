defmodule Alfred.CortexTest do
  use ExUnit.Case

  setup do
    memory_dir = Path.join([System.user_home!(), ".alfred", "data", "memory"])
    File.rm_rf(memory_dir)
    Alfred.Storage.Local.ensure_data_dir!()

    # Ensure scheduler is running for CLI tests
    case Process.whereis(Alfred.Supervisor) do
      nil -> :ok
      pid -> Supervisor.stop(pid)
    end

    case GenServer.whereis(:alfred_scheduler) do
      nil -> :ok
      pid -> GenServer.stop(pid)
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
