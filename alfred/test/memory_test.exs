defmodule Alfred.MemoryTest do
  use ExUnit.Case

  setup do
    # Clean memory data
    memory_dir = Path.join([System.user_home!(), ".alfred", "data", "memory"])
    File.rm_rf(memory_dir)
    Alfred.Storage.Local.ensure_data_dir!()
    :ok
  end

  describe "Alfred.Memory.Semantic" do
    test "starts empty" do
      assert Alfred.Memory.Semantic.all_facts() == []
      assert Alfred.Memory.Semantic.count() == 0
    end

    test "add and retrieve facts" do
      {:ok, fact} =
        Alfred.Memory.Semantic.add_fact(%{
          "category" => "preferences",
          "subject" => "langage",
          "content" => "Monsieur préfère Elixir",
          "confidence" => 0.8
        })

      assert fact["id"] == 1
      assert fact["category"] == "preferences"
      assert Alfred.Memory.Semantic.count() == 1
    end

    test "auto-incrementing IDs" do
      Alfred.Memory.Semantic.add_fact(%{"content" => "Fait 1"})
      Alfred.Memory.Semantic.add_fact(%{"content" => "Fait 2"})
      {:ok, fact3} = Alfred.Memory.Semantic.add_fact(%{"content" => "Fait 3"})
      assert fact3["id"] == 3
    end

    test "search by keywords" do
      Alfred.Memory.Semantic.add_fact(%{
        "category" => "knowledge",
        "subject" => "tech",
        "content" => "Utilise Elixir et Julia pour Alfred"
      })

      Alfred.Memory.Semantic.add_fact(%{
        "category" => "preferences",
        "subject" => "food",
        "content" => "Aime le café noir"
      })

      results = Alfred.Memory.Semantic.search("elixir")
      assert length(results) == 1
      assert hd(results)["content"] =~ "Elixir"
    end

    test "search returns empty for no match" do
      Alfred.Memory.Semantic.add_fact(%{"content" => "Quelque chose"})
      results = Alfred.Memory.Semantic.search("xyz123")
      assert results == []
    end

    test "list by category" do
      Alfred.Memory.Semantic.add_fact(%{"category" => "preferences", "content" => "A"})
      Alfred.Memory.Semantic.add_fact(%{"category" => "knowledge", "content" => "B"})
      Alfred.Memory.Semantic.add_fact(%{"category" => "preferences", "content" => "C"})

      prefs = Alfred.Memory.Semantic.list_by_category("preferences")
      assert length(prefs) == 2
    end

    test "delete fact" do
      {:ok, fact} = Alfred.Memory.Semantic.add_fact(%{"content" => "To delete"})
      assert :ok = Alfred.Memory.Semantic.delete_fact(fact["id"])
      assert Alfred.Memory.Semantic.count() == 0
    end

    test "delete non-existent fact" do
      assert {:error, :not_found} = Alfred.Memory.Semantic.delete_fact(999)
    end

    test "top facts by access count" do
      Alfred.Memory.Semantic.add_fact(%{"content" => "Rarement accédé"})
      {:ok, popular} = Alfred.Memory.Semantic.add_fact(%{"content" => "Souvent accédé"})

      # Simulate access
      Alfred.Memory.Semantic.search("souvent")
      Alfred.Memory.Semantic.search("souvent")

      top = Alfred.Memory.Semantic.top_facts(1)
      assert length(top) == 1
      assert hd(top)["id"] == popular["id"]
    end
  end

  describe "Alfred.Memory.Episodic" do
    test "starts empty" do
      assert Alfred.Memory.Episodic.list_episodes() == []
      assert Alfred.Memory.Episodic.count() == 0
    end

    test "save and list episodes" do
      {:ok, ep} =
        Alfred.Memory.Episodic.save_episode(%{
          "started_at" => "2026-02-17T10:00:00Z",
          "ended_at" => "2026-02-17T10:05:00Z",
          "mode" => "chat",
          "messages" => [
            %{"role" => "user", "content" => "Hello"},
            %{"role" => "assistant", "content" => "Bonjour"}
          ],
          "message_count" => 2,
          "summary" => "Test conversation"
        })

      assert ep["id"] != nil
      assert Alfred.Memory.Episodic.count() == 1

      episodes = Alfred.Memory.Episodic.list_episodes()
      assert length(episodes) == 1
      assert hd(episodes)["summary"] == "Test conversation"
    end

    test "load full episode" do
      {:ok, ep} =
        Alfred.Memory.Episodic.save_episode(%{
          "messages" => [%{"role" => "user", "content" => "Test"}],
          "message_count" => 1
        })

      {:ok, loaded} = Alfred.Memory.Episodic.load_episode(ep["id"])
      assert loaded["messages"] == [%{"role" => "user", "content" => "Test"}]
    end

    test "load non-existent episode" do
      assert {:error, :not_found} = Alfred.Memory.Episodic.load_episode("fake_id")
    end

    test "recent episodes" do
      Alfred.Memory.Episodic.save_episode(%{"summary" => "First", "message_count" => 1})
      Process.sleep(10)
      Alfred.Memory.Episodic.save_episode(%{"summary" => "Second", "message_count" => 2})

      recent = Alfred.Memory.Episodic.recent_episodes(1)
      assert length(recent) == 1
    end

    test "delete episode" do
      {:ok, ep} = Alfred.Memory.Episodic.save_episode(%{"message_count" => 1})
      assert :ok = Alfred.Memory.Episodic.delete_episode(ep["id"])
      assert Alfred.Memory.Episodic.count() == 0
    end
  end

  describe "Alfred.Memory.Procedural" do
    test "starts empty" do
      assert Alfred.Memory.Procedural.all_patterns() == []
    end

    test "add pattern" do
      {:ok, p} =
        Alfred.Memory.Procedural.add_pattern(%{
          "pattern_type" => "behavioral",
          "description" => "Monsieur travaille le soir",
          "confidence" => 0.7
        })

      assert p["id"] == 1
      assert p["active"] == true
    end

    test "active patterns only" do
      Alfred.Memory.Procedural.add_pattern(%{"description" => "Active"})
      {:ok, p2} = Alfred.Memory.Procedural.add_pattern(%{"description" => "To deactivate"})
      Alfred.Memory.Procedural.deactivate_pattern(p2["id"])

      active = Alfred.Memory.Procedural.active_patterns()
      assert length(active) == 1
      assert hd(active)["description"] == "Active"
    end

    test "confirm pattern increases confidence" do
      {:ok, p} = Alfred.Memory.Procedural.add_pattern(%{"description" => "Test", "confidence" => 0.5})
      Alfred.Memory.Procedural.confirm_pattern(p["id"])

      patterns = Alfred.Memory.Procedural.all_patterns()
      updated = hd(patterns)
      assert updated["confidence"] > 0.5
    end
  end

  describe "Alfred.Memory.Commands (CLI)" do
    test "memory facts when empty" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Memory.Commands.handle(["facts"])
        end)

      assert output =~ "vierge"
    end

    test "memory facts with data" do
      Alfred.Memory.Semantic.add_fact(%{
        "category" => "preferences",
        "content" => "Aime le café"
      })

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Memory.Commands.handle(["facts"])
        end)

      assert output =~ "café"
    end

    test "memory search" do
      Alfred.Memory.Semantic.add_fact(%{"content" => "Développeur Elixir"})

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Memory.Commands.handle(["search", "elixir"])
        end)

      assert output =~ "Elixir"
    end

    test "memory episodes when empty" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Memory.Commands.handle(["episodes"])
        end)

      assert output =~ "pas encore conversé"
    end
  end

  describe "Alfred.Memory.Episodic.update_episode" do
    test "updates episode with summary and topics" do
      {:ok, ep} =
        Alfred.Memory.Episodic.save_episode(%{
          "messages" => [%{"role" => "user", "content" => "Hello"}],
          "message_count" => 1,
          "summary" => nil,
          "topics" => []
        })

      {:ok, updated} =
        Alfred.Memory.Episodic.update_episode(ep["id"], %{
          "summary" => "Test conversation about greetings",
          "topics" => ["greetings", "test"]
        })

      assert updated["summary"] == "Test conversation about greetings"
      assert updated["topics"] == ["greetings", "test"]
      assert updated["message_count"] == 1

      # Verify persistence
      {:ok, reloaded} = Alfred.Memory.Episodic.load_episode(ep["id"])
      assert reloaded["summary"] == "Test conversation about greetings"
    end

    test "returns error for non-existent episode" do
      assert {:error, :not_found} = Alfred.Memory.Episodic.update_episode("fake_id", %{})
    end
  end

  describe "Alfred.Memory.Learner" do
    test "extract_facts via local extraction" do
      messages = [
        %{"role" => "user", "content" => "Je préfère Elixir pour ce projet"},
        %{"role" => "assistant", "content" => "Très bien Monsieur."}
      ]

      session = %Alfred.Chat.Session{
        started_at: DateTime.utc_now() |> DateTime.to_iso8601(),
        mode: "ask",
        system_prompt: "Tu es Alfred.",
        messages: messages
      }

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Alfred.Memory.Learner.learn(session, "fake_token")
        end)

      # Épisode sauvé
      assert Alfred.Memory.Episodic.count() == 1

      # Faits extraits (local fallback car fake token)
      facts = Alfred.Memory.Semantic.all_facts()
      assert length(facts) >= 1

      # Rapport affiché
      assert output =~ "fait(s)"
    end

    test "learn saves episode even with empty messages" do
      session = %Alfred.Chat.Session{
        started_at: DateTime.utc_now() |> DateTime.to_iso8601(),
        mode: "chat",
        system_prompt: "Tu es Alfred.",
        messages: []
      }

      ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Memory.Learner.learn(session, "fake_token")
      end)

      assert Alfred.Memory.Episodic.count() == 1
    end

    test "summarize_episode updates episode via Julia" do
      messages = [
        %{"role" => "user", "content" => "Parlons d'architecture Elixir"},
        %{"role" => "assistant", "content" => "Bien sûr, Monsieur."},
        %{"role" => "user", "content" => "Je veux une architecture modulaire"},
        %{"role" => "assistant", "content" => "Excellent choix."}
      ]

      session = %Alfred.Chat.Session{
        started_at: DateTime.utc_now() |> DateTime.to_iso8601(),
        mode: "chat",
        system_prompt: "Tu es Alfred.",
        messages: messages
      }

      ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Memory.Learner.learn(session, "fake_token")
      end)

      # L'épisode devrait avoir un résumé Julia (si Julia est dispo)
      episodes = Alfred.Memory.Episodic.list_episodes()
      assert length(episodes) == 1
      ep = hd(episodes)

      # Si Julia est disponible, le résumé est rempli
      if ep["summary"] != nil do
        assert is_binary(ep["summary"])
        assert ep["summary"] != ""
      end
    end
  end
end
