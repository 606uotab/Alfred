defmodule Alfred.SimplexTest do
  use ExUnit.Case

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    config_path = Path.join([System.user_home!(), ".alfred", "data", "simplex_config.json"])
    File.rm(config_path)

    if Process.whereis(Alfred.Simplex.Bridge) do
      try do
        GenServer.stop(Alfred.Simplex.Bridge, :normal, 1_000)
      catch
        :exit, _ -> :ok
      end

      Process.sleep(50)
    end

    case Process.whereis(Alfred.Supervisor) do
      nil -> :ok
      pid ->
        try do
          Supervisor.stop(pid, :normal, 5_000)
        catch
          :exit, _ -> :ok
        end

        Process.sleep(100)
    end

    case GenServer.whereis(:alfred_scheduler) do
      nil -> :ok
      pid ->
        try do
          GenServer.stop(pid, :normal, 5_000)
        catch
          :exit, _ -> :ok
        end
    end

    {:ok, _} = Alfred.Application.start()
    :ok
  end

  # -- WebSocket : fonctions pures --

  describe "Alfred.Simplex.WebSocket" do
    test "encode text frame produces valid binary" do
      # send_text requires a socket, but we can test decode_frames
      # with a manually constructed server frame (unmasked)
      payload = "hello"
      len = byte_size(payload)
      # Server frame: FIN=1, opcode=1 (text), mask=0, len=5
      frame = <<1::1, 0::3, 1::4, 0::1, len::7>> <> payload

      {frames, buffer} = Alfred.Simplex.WebSocket.decode_frames(frame)
      assert [{:text, "hello"}] == frames
      assert buffer == <<>>
    end

    test "decode_frames handles partial data" do
      # Only send the header, no payload
      partial = <<1::1, 0::3, 1::4, 0::1, 10::7>>

      {frames, buffer} = Alfred.Simplex.WebSocket.decode_frames(partial)
      assert frames == []
      assert byte_size(buffer) > 0
    end

    test "decode_frames handles multiple frames" do
      payload1 = "abc"
      payload2 = "xyz"

      frame1 = <<1::1, 0::3, 1::4, 0::1, 3::7>> <> payload1
      frame2 = <<1::1, 0::3, 1::4, 0::1, 3::7>> <> payload2

      {frames, buffer} = Alfred.Simplex.WebSocket.decode_frames(frame1 <> frame2)
      assert [{:text, "abc"}, {:text, "xyz"}] == frames
      assert buffer == <<>>
    end

    test "decode_frames handles ping frame" do
      # Ping: opcode=9, no payload
      frame = <<1::1, 0::3, 9::4, 0::1, 0::7>>

      {frames, _buffer} = Alfred.Simplex.WebSocket.decode_frames(frame)
      assert [:ping] == frames
    end

    test "decode_frames handles close frame" do
      # Close: opcode=8, no payload
      frame = <<1::1, 0::3, 8::4, 0::1, 0::7>>

      {frames, _buffer} = Alfred.Simplex.WebSocket.decode_frames(frame)
      assert [:close] == frames
    end

    test "pong_frame returns valid binary" do
      pong = Alfred.Simplex.WebSocket.pong_frame()
      assert is_binary(pong)
      assert byte_size(pong) > 0
    end

    test "decode_frames with buffer accumulation" do
      payload = "test data here"
      len = byte_size(payload)
      frame = <<1::1, 0::3, 1::4, 0::1, len::7>> <> payload

      # Split frame into two parts
      {part1, part2} = String.split_at(frame, 3)

      {frames1, buffer1} = Alfred.Simplex.WebSocket.decode_frames(part1)
      assert frames1 == []

      {frames2, buffer2} = Alfred.Simplex.WebSocket.decode_frames(part2, buffer1)
      assert [{:text, "test data here"}] == frames2
      assert buffer2 == <<>>
    end
  end

  # -- Client : parse_response --

  describe "Alfred.Simplex.Client" do
    test "parse_response handles correlated response" do
      json = Jason.encode!(%{"corrId" => "42", "resp" => %{"type" => "ok"}})
      assert {:response, "42", %{"type" => "ok"}} = Alfred.Simplex.Client.parse_response(json)
    end

    test "parse_response handles async event" do
      json = Jason.encode!(%{"corrId" => nil, "resp" => %{"type" => "newChatItem"}})
      assert {:event, %{"type" => "newChatItem"}} = Alfred.Simplex.Client.parse_response(json)
    end

    test "parse_response handles invalid JSON" do
      assert {:error, :invalid_json} = Alfred.Simplex.Client.parse_response("not json {{{")
    end

    test "parse_response handles unknown format" do
      json = Jason.encode!(%{"foo" => "bar"})
      assert {:unknown, %{"foo" => "bar"}} = Alfred.Simplex.Client.parse_response(json)
    end
  end

  # -- Bridge : config persistence et status --

  describe "Alfred.Simplex.Bridge" do
    test "load_config returns :no_config when no file" do
      assert :no_config == Alfred.Simplex.Bridge.load_config()
    end

    test "save_config and load_config round-trip" do
      config = %{"host" => "localhost", "port" => 5226, "contact" => "alice"}
      Alfred.Simplex.Bridge.save_config(config)
      assert {:ok, loaded} = Alfred.Simplex.Bridge.load_config()
      assert loaded["host"] == "localhost"
      assert loaded["port"] == 5226
      assert loaded["contact"] == "alice"
    end

    test "running? returns false when not started" do
      refute Alfred.Simplex.Bridge.running?()
    end

    test "status returns :not_running when not started" do
      assert :not_running == Alfred.Simplex.Bridge.status()
    end
  end

  # -- Commands : sorties CLI --

  describe "Alfred.Simplex.Commands" do
    test "handle status when not configured" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Simplex.Commands.handle(["status"])
        end)

      assert output =~ "non configuré" or output =~ "simplex connect"
    end

    test "handle disconnect when not running" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Simplex.Commands.handle(["disconnect"])
        end)

      assert output =~ "pas actif"
    end

    test "handle unknown shows help" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Simplex.Commands.handle(["unknown"])
        end)

      assert output =~ "simplex connect"
      assert output =~ "simplex status"
    end

    test "handle status with config but not running" do
      config = %{"host" => "localhost", "port" => 5226, "contact" => "alice"}
      Alfred.Simplex.Bridge.save_config(config)

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Simplex.Commands.handle(["status"])
        end)

      assert output =~ "inactif" or output =~ "localhost"
    end
  end

  # -- Clock --

  describe "Clock" do
    test "now returns a DateTime" do
      assert %DateTime{} = Alfred.Clock.now()
    end

    test "is running via supervision" do
      assert Process.whereis(Alfred.Clock) != nil
    end

    test "status returns info" do
      status = Alfred.Clock.status()
      assert is_map(status)
      assert Map.has_key?(status, :tick_count)
      assert Map.has_key?(status, :reminders_sent)
      assert Map.has_key?(status, :now)
      assert %DateTime{} = status.now
      assert is_binary(status.uptime_human)
    end

    test "now is approximately current time" do
      now = Alfred.Clock.now()
      utc = DateTime.utc_now()
      # now devrait être UTC+1 (3600s d'avance)
      diff = DateTime.diff(now, utc)
      assert diff >= 3590 and diff <= 3610
    end
  end
end
