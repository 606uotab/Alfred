defmodule Alfred.Initiative.SmartTest do
  use ExUnit.Case

  alias Alfred.Initiative.Smart

  setup do
    Alfred.Storage.Local.ensure_data_dir!()
    Alfred.Storage.Local.ensure_subdir!("initiative")

    # Backup and clean
    path = Path.join([System.user_home!(), ".alfred", "data", "initiative", "activity_log.json"])
    backup = File.read(path)
    File.rm(path)

    on_exit(fn ->
      case backup do
        {:ok, content} -> File.write!(path, content)
        _ -> File.rm(path)
      end
    end)

    :ok
  end

  describe "log_interaction/2" do
    test "logs an interaction" do
      assert Smart.log_interaction("message", "test") == :ok
    end

    test "increments interaction count" do
      Smart.log_interaction("message", "test")
      Smart.log_interaction("command", "test")
      assert Smart.interaction_count() == 2
    end
  end

  describe "should_notify_now?/0" do
    test "returns true with no data" do
      assert Smart.should_notify_now?() == true
    end

    test "returns true with few interactions" do
      for _ <- 1..5 do
        Smart.log_interaction("message", "test")
      end

      assert Smart.should_notify_now?() == true
    end
  end

  describe "optimal_windows/0" do
    test "returns empty list with no data" do
      assert Smart.optimal_windows() == []
    end
  end

  describe "hourly_scores/0" do
    test "returns empty map with no data" do
      assert Smart.hourly_scores() == %{}
    end

    test "returns scores after interactions" do
      for _ <- 1..5 do
        Smart.log_interaction("message", "test")
      end

      scores = Smart.hourly_scores()
      assert is_map(scores)
      assert map_size(scores) > 0
    end
  end
end
