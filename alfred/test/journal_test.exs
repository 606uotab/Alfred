defmodule Alfred.JournalTest do
  use ExUnit.Case

  alias Alfred.Journal

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    # Backup & clean journal dir
    journal_dir = Path.join([System.user_home!(), ".alfred", "data", "journal"])
    File.mkdir_p!(journal_dir)

    backup =
      if File.dir?(journal_dir) do
        journal_dir
        |> File.ls!()
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.map(fn f ->
          path = Path.join(journal_dir, f)
          {f, File.read!(path)}
        end)
      else
        []
      end

    # Clean for tests
    Enum.each(backup, fn {f, _} -> File.rm(Path.join(journal_dir, f)) end)

    on_exit(fn ->
      # Remove test files
      if File.dir?(journal_dir) do
        journal_dir
        |> File.ls!()
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.each(fn f -> File.rm(Path.join(journal_dir, f)) end)
      end

      # Restore originals
      Enum.each(backup, fn {f, content} ->
        File.write!(Path.join(journal_dir, f), content)
      end)
    end)

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

  describe "storage" do
    test "load returns error for missing date" do
      assert {:error, :not_found} = Journal.load("1999-01-01")
    end

    test "save and load entry via write path" do
      entry = %{
        "date" => "2026-01-15",
        "written_at" => "2026-01-15T22:00:00Z",
        "mood" => "serein",
        "entry" => "Ce soir, je prends un moment pour réfléchir.",
        "highlights" => ["3 conversations", "nouveau livre"],
        "soul_snapshot" => %{"mood" => "serein"},
        "stats" => %{"episodes_today" => 3}
      }

      Alfred.Storage.Local.ensure_subdir!("journal")
      Alfred.Storage.Local.write("journal/2026-01-15.json", entry)

      assert {:ok, loaded} = Journal.load("2026-01-15")
      assert loaded["mood"] == "serein"
      assert loaded["entry"] =~ "réfléchir"
      assert length(loaded["highlights"]) == 2
    end

    test "written_today? returns false when no entry" do
      refute Journal.written_today?()
    end

    test "written_today? returns true when entry exists" do
      today = Date.utc_today() |> Date.to_iso8601()
      entry = %{"date" => today, "mood" => "pensif", "entry" => "Test.", "highlights" => []}
      Alfred.Storage.Local.ensure_subdir!("journal")
      Alfred.Storage.Local.write("journal/#{today}.json", entry)

      assert Journal.written_today?()
    end

    test "load_latest returns today's entry" do
      today = Date.utc_today() |> Date.to_iso8601()
      entry = %{"date" => today, "mood" => "curieux", "entry" => "Aujourd'hui.", "highlights" => []}
      Alfred.Storage.Local.ensure_subdir!("journal")
      Alfred.Storage.Local.write("journal/#{today}.json", entry)

      assert {:ok, loaded} = Journal.load_latest()
      assert loaded["mood"] == "curieux"
    end

    test "load_latest falls back to yesterday" do
      yesterday = Date.utc_today() |> Date.add(-1) |> Date.to_iso8601()
      entry = %{"date" => yesterday, "mood" => "calme", "entry" => "Hier.", "highlights" => []}
      Alfred.Storage.Local.ensure_subdir!("journal")
      Alfred.Storage.Local.write("journal/#{yesterday}.json", entry)

      assert {:ok, loaded} = Journal.load_latest()
      assert loaded["date"] == yesterday
    end

    test "load_latest returns error when nothing" do
      assert {:error, :not_found} = Journal.load_latest()
    end

    test "list_recent returns entries sorted desc" do
      Alfred.Storage.Local.ensure_subdir!("journal")

      for d <- ["2026-01-10", "2026-01-11", "2026-01-12"] do
        Alfred.Storage.Local.write("journal/#{d}.json", %{
          "date" => d, "mood" => "ok", "entry" => "Jour #{d}.", "highlights" => []
        })
      end

      entries = Journal.list_recent(2)
      assert length(entries) == 2
      assert hd(entries)["date"] == "2026-01-12"
    end

    test "list_recent returns empty when no journal" do
      assert Journal.list_recent() == []
    end
  end

  describe "Commands" do
    test "handle list when empty" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Journal.Commands.handle(["list"])
      end)

      assert output =~ "vierge"
    end

    test "handle shows latest entry" do
      today = Date.utc_today() |> Date.to_iso8601()
      entry = %{"date" => today, "mood" => "fier", "entry" => "Belle journée.", "highlights" => ["test"]}
      Alfred.Storage.Local.ensure_subdir!("journal")
      Alfred.Storage.Local.write("journal/#{today}.json", entry)

      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Journal.Commands.handle([])
      end)

      assert output =~ "fier"
      assert output =~ "Belle journée"
    end

    test "handle unknown shows help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Journal.Commands.handle(["blabla"])
      end)

      assert output =~ "alfred journal"
    end

    test "handle show specific date" do
      entry = %{"date" => "2026-01-20", "mood" => "pensif", "entry" => "Réflexion.", "highlights" => []}
      Alfred.Storage.Local.ensure_subdir!("journal")
      Alfred.Storage.Local.write("journal/2026-01-20.json", entry)

      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Journal.Commands.handle(["show", "2026-01-20"])
      end)

      assert output =~ "pensif"
      assert output =~ "Réflexion"
    end
  end
end
