defmodule Alfred.Memory.ConsolidatorTest do
  use ExUnit.Case

  alias Alfred.Memory.Consolidator

  setup do
    Alfred.Storage.Local.ensure_data_dir!()
    Alfred.Storage.Local.ensure_subdir!("memory")
    :ok
  end

  describe "stats/0" do
    test "returns memory statistics" do
      stats = Consolidator.stats()
      assert is_integer(stats.episodes)
      assert is_integer(stats.facts)
      assert is_integer(stats.patterns)
      assert is_boolean(stats.synthesis)
    end
  end

  describe "load_synthesis/0" do
    test "returns nil when no synthesis" do
      # Backup and clean
      path = Path.join([System.user_home!(), ".alfred", "data", "memory", "synthesis.json"])
      backup = File.read(path)
      File.rm(path)

      assert Consolidator.load_synthesis() == nil

      case backup do
        {:ok, content} -> File.write!(path, content)
        _ -> :ok
      end
    end
  end

  describe "last_log/0" do
    test "returns nil when no log" do
      path = Path.join([System.user_home!(), ".alfred", "data", "memory", "consolidation_log.json"])
      backup = File.read(path)
      File.rm(path)

      assert Consolidator.last_log() == nil

      case backup do
        {:ok, content} -> File.write!(path, content)
        _ -> :ok
      end
    end
  end
end
