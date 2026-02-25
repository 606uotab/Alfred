defmodule Alfred.MaintenanceTest do
  use ExUnit.Case

  setup do
    memory_dir = Path.join([System.user_home!(), ".alfred", "data", "memory"])
    maintenance_path = Path.join([System.user_home!(), ".alfred", "data", "maintenance.json"])
    File.rm_rf(memory_dir)
    File.rm(maintenance_path)
    Alfred.Storage.Local.ensure_data_dir!()

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

  describe "Alfred.Maintenance" do
    test "run_startup_checks runs without crash" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Maintenance.run_startup_checks()
        end)

      # Should not crash — output may contain backup/consolidation info
      assert is_binary(output)
    end

    test "run_startup_checks creates maintenance.json" do
      ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Maintenance.run_startup_checks()
      end)

      path = Path.join([System.user_home!(), ".alfred", "data", "maintenance.json"])
      assert File.exists?(path)

      {:ok, content} = File.read(path)
      {:ok, state} = Jason.decode(content)
      assert is_map(state)
    end

    test "second run skips backup (already done)" do
      # First run
      ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Maintenance.run_startup_checks()
      end)

      # Second run should skip backup
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Maintenance.run_startup_checks()
        end)

      # No backup message on second run
      refute output =~ "Sauvegarde automatique"
    end
  end

  describe "Alfred.Shell" do
    test "shell module is available" do
      # Ensure the module is loaded and has the expected function
      Code.ensure_loaded!(Alfred.Shell)
      assert {:start, 0} in Alfred.Shell.__info__(:functions)
    end
  end
end
