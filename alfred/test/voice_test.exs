defmodule Alfred.VoiceTest do
  use ExUnit.Case

  alias Alfred.Voice

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    # Backup voice config
    path = Path.join([System.user_home!(), ".alfred", "data", "voice_config.json"])
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

  describe "configuration" do
    test "enabled? defaults to false" do
      refute Voice.enabled?()
    end

    test "enable and disable" do
      Voice.enable()
      assert Voice.enabled?()

      Voice.disable()
      refute Voice.enabled?()
    end

    test "configure updates settings" do
      Voice.configure(%{"lang" => "en", "rate" => "200"})
      status = Voice.status()
      assert status.lang == "en"
      assert status.rate == "200"
    end

    test "status returns expected keys" do
      status = Voice.status()
      assert Map.has_key?(status, :enabled)
      assert Map.has_key?(status, :available)
      assert Map.has_key?(status, :lang)
      assert Map.has_key?(status, :rate)
    end
  end

  describe "Commands" do
    test "handle shows status" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Voice.Commands.handle([])
      end)

      assert output =~ "voix"
    end

    test "handle unknown shows help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Voice.Commands.handle(["blabla"])
      end)

      assert output =~ "alfred voice"
    end
  end
end
