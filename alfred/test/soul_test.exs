defmodule Alfred.SoulTest do
  use ExUnit.Case

  alias Alfred.Soul.State

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    # Clean soul state between tests
    soul_path = Path.join([System.user_home!(), ".alfred", "data", "soul.json"])
    File.rm(soul_path)

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

  describe "Alfred.Soul.State" do
    test "load returns default state when no file" do
      state = State.load()
      assert %State{} = state
      assert state.mood == "serein"
      assert state.traits["formality"] == 0.8
      assert state.traits["humor"] == 0.3
      assert state.evolution_log == []
    end

    test "save and load round-trip" do
      state = State.load()
      state = State.set_mood(state, "joyeux")
      State.save(state)

      loaded = State.load()
      assert loaded.mood == "joyeux"
      assert loaded.traits == state.traits
    end

    test "apply_adjustment changes trait value" do
      state = State.load()
      state = State.apply_adjustment(state, "humor", 0.05, "test")

      assert state.traits["humor"] == 0.35
      assert length(state.evolution_log) == 1
      assert hd(state.evolution_log)["trait"] == "humor"
      assert hd(state.evolution_log)["delta"] == 0.05
    end

    test "apply_adjustment clamps to 0.0-1.0" do
      state = State.load()
      state = State.apply_adjustment(state, "formality", 0.5, "max test")
      assert state.traits["formality"] == 1.0

      state = State.apply_adjustment(state, "humor", -1.0, "min test")
      assert state.traits["humor"] == 0.0
    end

    test "apply_adjustment ignores unknown traits" do
      state = State.load()
      result = State.apply_adjustment(state, "nonexistent", 0.1, "test")
      assert result == state
    end

    test "to_prompt_text contains traits and mood" do
      state = State.load()
      text = State.to_prompt_text(state)

      assert text =~ "serein"
      assert text =~ "Formalité"
      assert text =~ "Humour"
      assert text =~ "Empathie"
    end

    test "personality_summary returns formatted bars" do
      state = State.load()
      summary = State.personality_summary(state)

      assert summary =~ "Formalité"
      assert summary =~ "█"
      assert summary =~ "░"
    end

    test "set_mood changes mood" do
      state = State.load()
      state = State.set_mood(state, "enthousiaste")
      assert state.mood == "enthousiaste"
    end

    test "default_traits returns the default map" do
      traits = State.default_traits()
      assert is_map(traits)
      assert Map.has_key?(traits, "formality")
      assert Map.has_key?(traits, "humor")
    end
  end

  describe "Alfred.Soul.Evolver" do
    test "evolve_from_patterns with empty list" do
      assert :ok == Alfred.Soul.Evolver.evolve_from_patterns([])
    end

    test "evolve_from_patterns detects humor pattern" do
      patterns = [%{"description" => "Le maître apprécie l'humour et les blagues", "confidence" => 0.7}]
      Alfred.Soul.Evolver.evolve_from_patterns(patterns)

      state = State.load()
      assert state.traits["humor"] > 0.3
    end

    test "evolve_from_patterns detects concise pattern" do
      patterns = [%{"description" => "Préfère les sessions courtes et concises", "confidence" => 0.7}]
      Alfred.Soul.Evolver.evolve_from_patterns(patterns)

      state = State.load()
      assert state.traits["verbosity"] < 0.5
    end
  end

  describe "Alfred.Soul.Commands" do
    test "handle shows soul traits" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Soul.Commands.handle([])
      end)

      assert output =~ "âme"
      assert output =~ "serein"
      assert output =~ "Formalité"
    end

    test "handle history when empty" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Soul.Commands.handle(["history"])
      end)

      assert output =~ "pas encore évolué"
    end

    test "handle reset resets traits" do
      # First modify a trait
      state = State.load()
      state = State.apply_adjustment(state, "humor", 0.05, "test")
      State.save(state)

      ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Soul.Commands.handle(["reset"])
      end)

      state = State.load()
      assert state.traits["humor"] == 0.3
      assert state.evolution_log != []
    end

    test "handle unknown subcommand shows help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Soul.Commands.handle(["unknown"])
      end)

      assert output =~ "soul init"
      assert output =~ "soul history"
    end
  end
end
