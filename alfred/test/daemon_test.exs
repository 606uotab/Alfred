defmodule Alfred.DaemonTest do
  use ExUnit.Case

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    for name <- [Alfred.Daemon, :alfred_scheduler, Alfred.Supervisor] do
      case Process.whereis(name) do
        nil -> :ok
        pid when is_pid(pid) ->
          try do
            if Process.alive?(pid) do
              if name == Alfred.Supervisor, do: Supervisor.stop(pid), else: GenServer.stop(pid)
            end
          catch
            :exit, _ -> :ok
          end
      end
    end

    # Remove stale PID file to avoid interfering with stop/status tests
    Alfred.Launcher.delete_pid_file()

    {:ok, _} = Alfred.Application.start()
    :ok
  end

  describe "Alfred.Daemon" do
    test "start_link and running?" do
      refute Alfred.Daemon.running?()
      {:ok, pid} = Alfred.Daemon.start_link()
      assert is_pid(pid)
      assert Alfred.Daemon.running?()
      Alfred.Daemon.stop()
      Process.sleep(50)
      refute Alfred.Daemon.running?()
    end

    test "status returns info when running" do
      {:ok, _pid} = Alfred.Daemon.start_link()
      info = Alfred.Daemon.status()

      assert is_map(info)
      assert info.check_count == 0
      assert info.uptime_seconds >= 0
      assert is_binary(info.uptime_human)

      Alfred.Daemon.stop()
    end

    test "status returns :not_running when stopped" do
      assert Alfred.Daemon.status() == :not_running
    end

    test "check triggers after interval" do
      {:ok, _pid} = Alfred.Daemon.start_link()

      # Force a check by sending the message directly
      send(Process.whereis(Alfred.Daemon), :check)
      Process.sleep(100)

      info = Alfred.Daemon.status()
      assert info.check_count >= 1

      Alfred.Daemon.stop()
    end
  end

  describe "Alfred.Daemon.Commands" do
    test "handle status when not running" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Daemon.Commands.handle(["status"])
      end)

      assert output =~ "pas actif"
    end

    test "handle stop when not running" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Daemon.Commands.handle(["stop"])
      end)

      assert output =~ "pas actif"
    end

    test "handle unknown shows help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Daemon.Commands.handle(["unknown"])
      end)

      assert output =~ "alfred daemon"
      assert output =~ "daemon status"
    end
  end
end
