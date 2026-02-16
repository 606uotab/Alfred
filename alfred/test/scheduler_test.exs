defmodule Alfred.SchedulerTest do
  use ExUnit.Case

  setup do
    # Clean reminders
    reminder_path = Path.join([System.user_home!(), ".alfred", "data", "reminders.dat"])
    File.rm(reminder_path)

    # Restart the scheduler to get a clean state
    case GenServer.whereis(:alfred_scheduler) do
      nil -> :ok
      pid -> GenServer.stop(pid)
    end

    {:ok, _} = :alfred_scheduler.start_link()
    :ok
  end

  describe "alfred_scheduler (Erlang gen_server)" do
    test "add and list reminders" do
      future = System.system_time(:second) + 3600

      {:ok, id} = :alfred_scheduler.add_reminder("Project A", "Do something", future)
      assert id == 1

      {:ok, reminders} = :alfred_scheduler.list_reminders()
      assert length(reminders) == 1

      r = hd(reminders)
      assert r.id == 1
      assert r.project == "Project A"
      assert r.text == "Do something"
      assert r.status == :pending
    end

    test "auto-incrementing IDs" do
      future = System.system_time(:second) + 3600

      {:ok, 1} = :alfred_scheduler.add_reminder("P", "A", future)
      {:ok, 2} = :alfred_scheduler.add_reminder("P", "B", future)
      {:ok, 3} = :alfred_scheduler.add_reminder("P", "C", future)
    end

    test "check_due returns only past-due pending reminders" do
      now = System.system_time(:second)

      # Past due reminder
      {:ok, _} = :alfred_scheduler.add_reminder("P", "Past", now - 60)
      # Future reminder
      {:ok, _} = :alfred_scheduler.add_reminder("P", "Future", now + 3600)

      {:ok, due} = :alfred_scheduler.check_due()
      assert length(due) == 1
      assert hd(due).text == "Past"
    end

    test "complete a reminder" do
      now = System.system_time(:second)
      {:ok, id} = :alfred_scheduler.add_reminder("P", "Complete me", now - 10)

      assert :ok = :alfred_scheduler.complete_reminder(id)

      {:ok, reminders} = :alfred_scheduler.list_reminders()
      r = hd(reminders)
      assert r.status == :done
    end

    test "complete non-existent reminder" do
      assert {:error, :not_found} = :alfred_scheduler.complete_reminder(999)
    end

    test "delete a reminder" do
      future = System.system_time(:second) + 3600
      {:ok, id} = :alfred_scheduler.add_reminder("P", "Delete me", future)

      assert :ok = :alfred_scheduler.delete_reminder(id)

      {:ok, reminders} = :alfred_scheduler.list_reminders()
      assert reminders == []
    end

    test "delete non-existent reminder" do
      assert {:error, :not_found} = :alfred_scheduler.delete_reminder(999)
    end

    test "count_pending" do
      now = System.system_time(:second)
      {:ok, _} = :alfred_scheduler.add_reminder("P", "A", now + 100)
      {:ok, _} = :alfred_scheduler.add_reminder("P", "B", now + 200)
      {:ok, id} = :alfred_scheduler.add_reminder("P", "C", now + 300)

      {:ok, 3} = :alfred_scheduler.count_pending()

      :alfred_scheduler.complete_reminder(id)
      {:ok, 2} = :alfred_scheduler.count_pending()
    end

    test "persistence across restarts" do
      future = System.system_time(:second) + 3600
      {:ok, _} = :alfred_scheduler.add_reminder("P", "Persistent", future)

      # Stop and restart
      GenServer.stop(:alfred_scheduler)
      {:ok, _} = :alfred_scheduler.start_link()

      {:ok, reminders} = :alfred_scheduler.list_reminders()
      assert length(reminders) == 1
      assert hd(reminders).text == "Persistent"
    end

    test "completed reminders not in check_due" do
      now = System.system_time(:second)
      {:ok, id} = :alfred_scheduler.add_reminder("P", "Done", now - 60)
      :alfred_scheduler.complete_reminder(id)

      {:ok, due} = :alfred_scheduler.check_due()
      assert due == []
    end
  end

  describe "alfred_health (Erlang module)" do
    test "check_all returns all components" do
      checks = :alfred_health.check_all()
      components = Enum.map(checks, fn {name, _} -> name end)

      assert :beam in components
      assert :vault in components
      assert :storage in components
      assert :scheduler in components
    end

    test "beam check returns ok" do
      info = :alfred_health.check_beam()
      assert info.status == :ok
      assert is_binary(info.otp_release)
      assert is_integer(info.process_count)
      assert is_integer(info.memory_mb)
    end

    test "storage check returns ok" do
      info = :alfred_health.check_storage()
      assert info.status == :ok
      assert info.writable == true
    end

    test "scheduler check returns ok when running" do
      info = :alfred_health.check_scheduler()
      assert info.status == :ok
      assert info.running == true
    end
  end

  describe "CLI remind commands" do
    test "remind help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Remind.Commands.handle([])
      end)

      assert output =~ "rappel"
    end

    test "duration parsing via CLI" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Remind.Commands.handle(["TestProj", "Do", "stuff", "in", "2h"])
      end)

      assert output =~ "Rappel #"
      assert output =~ "2 heures"
    end

    test "invalid duration" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Remind.Commands.handle(["TestProj", "Do", "stuff", "in", "abc"])
      end)

      assert output =~ "ne comprends pas"
    end
  end
end
