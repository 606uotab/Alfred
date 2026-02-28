defmodule Alfred.SessionStoreTest do
  use ExUnit.Case

  @test_password "test_password_123"
  @test_token "sk-test-token-abc123"
  @test_soul %{"traits" => %{"formality" => 0.8}}
  @test_culture [%{"topic" => "elixir", "content" => "functional programming"}]

  setup do
    # Ensure clean state
    Alfred.SessionStore.clear()
    on_exit(fn -> Alfred.SessionStore.clear() end)
    :ok
  end

  test "save and load round-trip" do
    assert :ok = Alfred.SessionStore.save(@test_password, @test_token, @test_soul, @test_culture)
    assert Alfred.SessionStore.exists?()

    assert {:ok, session} = Alfred.SessionStore.load(@test_password)
    assert session.token == @test_token
    assert session.soul == @test_soul
    assert session.culture == @test_culture
  end

  test "load with wrong password returns error" do
    :ok = Alfred.SessionStore.save(@test_password, @test_token, @test_soul, @test_culture)
    {:error, msg} = Alfred.SessionStore.load("wrong_password")
    assert is_binary(msg)
    assert msg =~ "mauvais mot de passe"
  end

  test "load with no session file returns :no_session" do
    assert {:error, :no_session} = Alfred.SessionStore.load(@test_password)
  end

  test "clear removes the session file" do
    :ok = Alfred.SessionStore.save(@test_password, @test_token, @test_soul, @test_culture)
    assert Alfred.SessionStore.exists?()
    Alfred.SessionStore.clear()
    refute Alfred.SessionStore.exists?()
  end

  test "expired session returns :expired" do
    # Save with TTL of 0 seconds (immediately expired)
    :ok = Alfred.SessionStore.save(@test_password, @test_token, @test_soul, @test_culture, ttl: 0)
    Process.sleep(100)
    assert {:error, :expired} = Alfred.SessionStore.load(@test_password)
  end

  test "save overwrites previous session" do
    :ok = Alfred.SessionStore.save(@test_password, "token_1", @test_soul, @test_culture)
    :ok = Alfred.SessionStore.save(@test_password, "token_2", @test_soul, @test_culture)

    {:ok, session} = Alfred.SessionStore.load(@test_password)
    assert session.token == "token_2"
  end
end
