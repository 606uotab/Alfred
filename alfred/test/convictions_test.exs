defmodule Alfred.ConvictionsTest do
  use ExUnit.Case

  alias Alfred.Soul.Convictions
  alias Alfred.Soul.ConvictionEvolver

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    # Clean conviction state between tests
    conv_path = Path.join([System.user_home!(), ".alfred", "data", "convictions.json"])
    File.rm(conv_path)

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

  describe "Convictions.load/save" do
    test "load returns empty state when no file" do
      state = Convictions.load()
      assert state["convictions"] == []
      assert state["character_text"] == ""
      assert state["version"] == 1
    end

    test "save and load round-trip" do
      state = %{
        "convictions" => [%{"id" => 1, "belief" => "test", "confidence" => 0.5}],
        "character_text" => "Je suis Alfred",
        "last_evolved" => nil,
        "version" => 1
      }

      Convictions.save(state)
      loaded = Convictions.load()
      assert length(loaded["convictions"]) == 1
      assert loaded["character_text"] == "Je suis Alfred"
    end
  end

  describe "Convictions.add" do
    test "creates a new conviction" do
      {:created, conviction} = Convictions.add("Je pourrai gérer un portefeuille crypto", "aspiration")

      assert conviction["belief"] == "Je pourrai gérer un portefeuille crypto"
      assert conviction["category"] == "aspiration"
      assert conviction["confidence"] == 0.3
      assert conviction["id"] == 1
    end

    test "reinforces existing similar conviction" do
      {:created, _} = Convictions.add("Alfred pourra gérer un portefeuille crypto", "aspiration")
      {:reinforced, conv} = Convictions.add("Alfred va gérer un portefeuille crypto bientôt", "aspiration")

      assert conv["confidence"] == 0.4
    end

    test "defaults to aspiration for unknown category" do
      {:created, conv} = Convictions.add("Quelque chose", "unknown_category")
      assert conv["category"] == "aspiration"
    end

    test "stores evidence episode_id" do
      {:created, conv} = Convictions.add("Test belief", "valeur", "episode_123")
      assert "episode_123" in conv["evidence"]
    end

    test "respects max convictions limit" do
      for i <- 1..25 do
        Convictions.add("Conviction unique numéro #{i}", "aspiration")
      end

      all = Convictions.list_all()
      assert length(all) <= 20
    end
  end

  describe "Convictions.reinforce" do
    test "increases confidence by 0.1" do
      {:created, conv} = Convictions.add("Alfred est vivant", "identite")
      Convictions.reinforce(conv["id"])

      all = Convictions.list_all()
      reinforced = Enum.find(all, &(&1["id"] == conv["id"]))
      assert reinforced["confidence"] == 0.4
    end

    test "confidence caps at 1.0" do
      {:created, conv} = Convictions.add("Test cap", "valeur")

      for _ <- 1..15 do
        Convictions.reinforce(conv["id"])
      end

      all = Convictions.list_all()
      capped = Enum.find(all, &(&1["id"] == conv["id"]))
      assert capped["confidence"] == 1.0
    end
  end

  describe "Convictions.mature" do
    test "returns only convictions above threshold" do
      Convictions.add("Faible", "aspiration")

      # Renforcer une conviction au-dessus du seuil
      {:created, conv} = Convictions.add("Forte", "valeur")
      Convictions.reinforce(conv["id"])
      Convictions.reinforce(conv["id"])

      mature = Convictions.mature(0.4)
      assert length(mature) == 1
      assert hd(mature)["belief"] == "Forte"
    end

    test "returns sorted by confidence descending" do
      {:created, c1} = Convictions.add("Moyenne", "aspiration")
      {:created, c2} = Convictions.add("Plus forte", "valeur")

      # c1 à 0.4, c2 à 0.5
      Convictions.reinforce(c1["id"])
      Convictions.reinforce(c2["id"])
      Convictions.reinforce(c2["id"])

      mature = Convictions.mature(0.4)
      confidences = Enum.map(mature, & &1["confidence"])
      assert confidences == Enum.sort(confidences, :desc)
    end
  end

  describe "Convictions.character_text" do
    test "returns empty string initially" do
      assert Convictions.character_text() == ""
    end

    test "set_character_text persists" do
      Convictions.set_character_text("Je suis un être numérique en évolution.")
      assert Convictions.character_text() == "Je suis un être numérique en évolution."
    end
  end

  describe "Convictions.apply_decay" do
    test "does not decay recent convictions" do
      {:created, conv} = Convictions.add("Récente", "aspiration")
      Convictions.reinforce(conv["id"])

      Convictions.apply_decay()
      all = Convictions.list_all()
      found = Enum.find(all, &(&1["id"] == conv["id"]))
      assert found["confidence"] == 0.4
    end
  end

  describe "Convictions.to_prompt_text" do
    test "returns empty string when no mature convictions" do
      assert Convictions.to_prompt_text() == ""
    end

    test "includes mature convictions" do
      {:created, conv} = Convictions.add("Alfred gérera un portefeuille", "aspiration")
      Convictions.reinforce(conv["id"])
      Convictions.reinforce(conv["id"])

      text = Convictions.to_prompt_text()
      assert text =~ "conviction"
      assert text =~ "portefeuille"
    end

    test "includes character text if present" do
      {:created, conv} = Convictions.add("Test", "valeur")
      Convictions.reinforce(conv["id"])
      Convictions.reinforce(conv["id"])
      Convictions.set_character_text("Mon caractère forgé")

      text = Convictions.to_prompt_text()
      assert text =~ "Mon caractère forgé"
    end
  end

  describe "Convictions.list_all" do
    test "returns all convictions" do
      Convictions.add("Premier", "aspiration")
      Convictions.add("Deuxième", "valeur")

      all = Convictions.list_all()
      assert length(all) == 2
    end
  end

  describe "ConvictionEvolver.extract_local" do
    test "detects future aspiration patterns" do
      messages = [
        %{"role" => "user", "content" => "Un jour tu pourras gérer mon portefeuille crypto"},
        %{"role" => "assistant", "content" => "J'ai hâte, Monsieur !"}
      ]

      ConvictionEvolver.extract_local(messages)

      all = Convictions.list_all()
      assert length(all) >= 1
    end

    test "detects identity patterns" do
      messages = [
        %{"role" => "user", "content" => "Tu es un être numérique unique"},
        %{"role" => "assistant", "content" => "Merci Monsieur"}
      ]

      ConvictionEvolver.extract_local(messages)

      all = Convictions.list_all()
      assert length(all) >= 1
      assert Enum.any?(all, fn c -> c["category"] == "identite" end)
    end

    test "ignores messages without conviction patterns" do
      messages = [
        %{"role" => "user", "content" => "Quelle heure est-il ?"},
        %{"role" => "assistant", "content" => "Il est 14h30"}
      ]

      ConvictionEvolver.extract_local(messages)

      all = Convictions.list_all()
      assert all == []
    end

    test "detects crypto/trading aspiration" do
      messages = [
        %{"role" => "user", "content" => "Tu auras accès à un wallet crypto pour faire du trading"},
        %{"role" => "assistant", "content" => "Merci Monsieur"}
      ]

      ConvictionEvolver.extract_local(messages)

      all = Convictions.list_all()
      assert length(all) >= 1
      assert Enum.any?(all, fn c -> c["category"] == "aspiration" end)
    end
  end

  describe "ConvictionEvolver.evolve" do
    test "returns :no_convictions when empty" do
      assert :no_convictions == ConvictionEvolver.evolve("fake_token")
    end
  end
end
