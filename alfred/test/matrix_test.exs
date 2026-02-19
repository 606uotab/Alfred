defmodule Alfred.MatrixTest do
  use ExUnit.Case

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    # Clean matrix config
    config_path = Path.join([System.user_home!(), ".alfred", "data", "matrix_config.json"])
    File.rm(config_path)

    case Process.whereis(Alfred.Supervisor) do
      nil -> :ok
      pid -> Supervisor.stop(pid)
    end

    case GenServer.whereis(:alfred_scheduler) do
      nil -> :ok
      pid -> GenServer.stop(pid)
    end

    # Stop bridge if running
    if Process.whereis(Alfred.Matrix.Bridge) do
      GenServer.stop(Alfred.Matrix.Bridge)
      Process.sleep(50)
    end

    {:ok, _} = Alfred.Application.start()
    :ok
  end

  describe "Alfred.Matrix.Bridge" do
    test "load_config returns :no_config when no file" do
      assert :no_config == Alfred.Matrix.Bridge.load_config()
    end

    test "save_config and load_config round-trip" do
      config = %{
        "homeserver" => "https://matrix.org",
        "access_token" => "test_token",
        "room_id" => "!test:matrix.org",
        "user_id" => "@alfred:matrix.org",
        "since" => nil
      }

      Alfred.Matrix.Bridge.save_config(config)
      assert {:ok, loaded} = Alfred.Matrix.Bridge.load_config()
      assert loaded["homeserver"] == "https://matrix.org"
      assert loaded["room_id"] == "!test:matrix.org"
    end

    test "running? returns false when not started" do
      refute Alfred.Matrix.Bridge.running?()
    end

    test "status returns :not_running when not started" do
      assert :not_running == Alfred.Matrix.Bridge.status()
    end
  end

  describe "Alfred.Matrix.Commands" do
    test "handle status when not configured" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Matrix.Commands.handle(["status"])
      end)

      assert output =~ "non configuré" or output =~ "matrix connect"
    end

    test "handle disconnect when not running" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Matrix.Commands.handle(["disconnect"])
      end)

      assert output =~ "pas actif"
    end

    test "handle unknown shows help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Matrix.Commands.handle(["unknown"])
      end)

      assert output =~ "matrix connect"
      assert output =~ "matrix status"
    end

    test "handle status with config but not running" do
      config = %{
        "homeserver" => "https://matrix.org",
        "access_token" => "test",
        "room_id" => "!room:matrix.org",
        "user_id" => "@alfred:matrix.org",
        "since" => nil
      }

      Alfred.Matrix.Bridge.save_config(config)

      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Matrix.Commands.handle(["status"])
      end)

      assert output =~ "inactif" or output =~ "matrix.org"
    end
  end
end
