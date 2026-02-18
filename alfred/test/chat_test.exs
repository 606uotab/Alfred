defmodule Alfred.ChatTest do
  use ExUnit.Case

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

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

  describe "Alfred.Chat.Commands" do
    test "module is available with public functions" do
      Code.ensure_loaded!(Alfred.Chat.Commands)
      functions = Alfred.Chat.Commands.__info__(:functions)

      assert {:handle_chat, 0} in functions
      assert {:handle_ask, 1} in functions
      assert {:authenticate, 0} in functions
      assert {:authenticate_with_password, 1} in functions
      assert {:send_message, 5} in functions
      assert {:build_session, 3} in functions
      assert {:save_conversation, 2} in functions
    end

    test "build_session returns a session struct" do
      session = Alfred.Chat.Commands.build_session("test", nil, [])
      assert %Alfred.Chat.Session{} = session
      assert session.mode == "test"
      assert is_binary(session.system_prompt)
      assert session.system_prompt =~ "Alfred"
    end

    test "build_session with culture" do
      culture = [%{"topic" => "test", "content" => "hello", "source" => %{"type" => "observation"}}]
      session = Alfred.Chat.Commands.build_session("test", nil, culture)
      assert session.system_prompt =~ "culture"
    end

    test "authenticate with env var returns token" do
      original = System.get_env("MISTRAL_API_KEY")
      System.put_env("MISTRAL_API_KEY", "test-token-123")

      result = Alfred.Chat.Commands.authenticate()
      assert {:ok, "test-token-123", nil, []} = result

      # Restore
      if original, do: System.put_env("MISTRAL_API_KEY", original), else: System.delete_env("MISTRAL_API_KEY")
    end
  end

  describe "Alfred.Chat.Session" do
    test "new creates a session" do
      session = Alfred.Chat.Session.new("test prompt")
      assert session.system_prompt == "test prompt"
      assert session.messages == []
      assert session.mode == "chat"
    end

    test "add_message and message_count" do
      session = Alfred.Chat.Session.new("test")
      assert Alfred.Chat.Session.message_count(session) == 0

      session = Alfred.Chat.Session.add_message(session, "user", "hello")
      assert Alfred.Chat.Session.message_count(session) == 1

      session = Alfred.Chat.Session.add_message(session, "assistant", "hi")
      assert Alfred.Chat.Session.message_count(session) == 2
    end

    test "to_api_messages includes system prompt" do
      session = Alfred.Chat.Session.new("system prompt here")
      session = Alfred.Chat.Session.add_message(session, "user", "hello")
      messages = Alfred.Chat.Session.to_api_messages(session)

      assert length(messages) == 2
      assert hd(messages)["role"] == "system"
      assert hd(messages)["content"] == "system prompt here"
    end

    test "to_episode returns episode map" do
      session = Alfred.Chat.Session.new("test")
      session = Alfred.Chat.Session.add_message(session, "user", "hello")
      episode = Alfred.Chat.Session.to_episode(session)

      assert is_map(episode)
      assert episode["message_count"] == 1
      assert is_binary(episode["started_at"])
    end
  end

  describe "Alfred.Shell" do
    test "shell module has start function" do
      Code.ensure_loaded!(Alfred.Shell)
      assert {:start, 0} in Alfred.Shell.__info__(:functions)
    end
  end

  describe "Alfred.Chat.SystemPrompt" do
    test "build returns personality string" do
      prompt = Alfred.Chat.SystemPrompt.build()
      assert is_binary(prompt)
      assert prompt =~ "Alfred"
      assert prompt =~ "majordome"
    end

    test "build with facts" do
      prompt = Alfred.Chat.SystemPrompt.build(facts: [%{"content" => "test fact"}])
      assert prompt =~ "test fact"
    end

    test "minimal returns personality" do
      prompt = Alfred.Chat.SystemPrompt.minimal()
      assert is_binary(prompt)
      assert prompt =~ "Alfred"
    end
  end
end
