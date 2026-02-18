defmodule Alfred.ArmsTest do
  use ExUnit.Case

  setup do
    memory_dir = Path.join([System.user_home!(), ".alfred", "data", "memory"])
    File.rm_rf(memory_dir)
    Alfred.Storage.Local.ensure_data_dir!()

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

  describe "Alfred.Arms.Port" do
    test "system_info returns hostname, os, uptime, load" do
      result = Alfred.Arms.Port.send_command(%{cmd: "system_info"})
      assert {:ok, resp} = result
      info = resp["info"]
      assert is_binary(info["hostname"])
      assert info["hostname"] != ""
      assert is_binary(info["os"])
      assert is_binary(info["uptime"])
      assert is_binary(info["load"])
    end

    test "disk_usage returns partitions with percent" do
      result = Alfred.Arms.Port.send_command(%{cmd: "disk_usage"})
      assert {:ok, resp} = result
      assert is_list(resp["partitions"])
      assert length(resp["partitions"]) > 0

      root = Enum.find(resp["partitions"], fn p -> p["mount"] == "/" end)
      assert root != nil
      assert is_integer(root["size_mb"])
      assert is_integer(root["percent_used"])
      assert is_boolean(resp["alert"])
    end

    test "memory_usage returns total and available" do
      result = Alfred.Arms.Port.send_command(%{cmd: "memory_usage"})
      assert {:ok, resp} = result
      mem = resp["memory"]
      assert is_integer(mem["total_mb"])
      assert mem["total_mb"] > 0
      assert is_integer(mem["available_mb"])
      assert mem["available_mb"] > 0
      assert is_integer(mem["percent_used"])
      assert is_boolean(mem["alert"])
    end

    test "backup creates a tar.gz file" do
      # Use a temp directory for test backup
      test_dir = Path.join(System.tmp_dir!(), "alfred_arms_test_#{:rand.uniform(100000)}")
      data_subdir = Path.join(test_dir, "data")
      File.mkdir_p!(data_subdir)
      File.write!(Path.join(data_subdir, "test.txt"), "hello alfred")

      result = Alfred.Arms.Port.send_command(%{cmd: "backup", data_dir: test_dir})
      assert {:ok, resp} = result
      backup = resp["backup"]
      assert is_binary(backup["path"])
      assert String.contains?(backup["path"], "alfred_backup_")
      assert String.ends_with?(backup["path"], ".tar.gz")
      assert File.exists?(backup["path"])
      assert is_integer(backup["size_bytes"])

      # Cleanup
      File.rm_rf(test_dir)
    end

    test "unknown command returns error" do
      result = Alfred.Arms.Port.send_command(%{cmd: "unknown_cmd"})
      assert {:error, msg} = result
      assert is_binary(msg)
    end
  end

  describe "alfred_health arms check" do
    test "check_arms returns map with binary_found" do
      info = :alfred_health.check_arms()
      assert is_map(info)
      assert Map.has_key?(info, :binary_found)
    end

    test "check_all includes arms" do
      checks = :alfred_health.check_all()
      components = Enum.map(checks, fn {name, _} -> name end)
      assert :arms in components
    end
  end

  describe "Alfred.Arms.Commands" do
    test "handle help displays commands" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Arms.Commands.handle(["help"])
        end)

      assert output =~ "arms status"
      assert output =~ "arms disk"
      assert output =~ "arms memory"
      assert output =~ "arms backup"
    end

    test "handle status runs without crash" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Arms.Commands.handle(["status"])
        end)

      assert output =~ "Machine" or output =~ "Impossible"
    end
  end
end
