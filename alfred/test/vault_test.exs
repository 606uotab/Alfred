defmodule Alfred.VaultTest do
  use ExUnit.Case

  @test_vault_path "/tmp/alfred_test_vault.enc"
  @test_password "test_master_password"

  setup do
    File.rm_rf!(@test_vault_path)
    :ok
  end

  # Override vault_path for tests
  defp send_cmd(command) do
    binary = vault_binary_path()
    json = Jason.encode!(command)

    port =
      Port.open({:spawn_executable, binary}, [
        :binary,
        :exit_status,
        args: [@test_vault_path],
        line: 65536
      ])

    Port.command(port, json <> "\n")
    result = receive_response(port)

    send(port, {self(), :close})
    flush(port)

    result
  end

  defp send_cmds(commands) do
    binary = vault_binary_path()

    port =
      Port.open({:spawn_executable, binary}, [
        :binary,
        :exit_status,
        args: [@test_vault_path],
        line: 65536
      ])

    result = send_and_receive(port, commands)

    send(port, {self(), :close})
    flush(port)

    result
  end

  defp send_and_receive(_port, []), do: {:ok, %{}}

  defp send_and_receive(port, [cmd]) do
    Port.command(port, Jason.encode!(cmd) <> "\n")
    receive_response(port)
  end

  defp send_and_receive(port, [cmd | rest]) do
    Port.command(port, Jason.encode!(cmd) <> "\n")

    case receive_response(port) do
      {:ok, _} -> send_and_receive(port, rest)
      {:error, _} = err -> err
    end
  end

  defp receive_response(port) do
    receive do
      {^port, {:data, {:eol, line}}} ->
        case Jason.decode(line) do
          {:ok, %{"status" => "ok"} = resp} -> {:ok, resp}
          {:ok, %{"status" => "error", "message" => msg}} -> {:error, msg}
          _ -> {:error, "Invalid response"}
        end

      {^port, {:exit_status, _}} ->
        {:error, "Process exited"}
    after
      30_000 -> {:error, "Timeout"}
    end
  end

  defp flush(port) do
    receive do
      {^port, _} -> flush(port)
    after
      100 -> :ok
    end
  end

  defp vault_binary_path do
    Path.expand("native/vault/zig-out/bin/alfred-vault", File.cwd!())
  end

  # -- Tests --

  describe "vault init" do
    test "creates a new vault" do
      assert {:ok, %{"message" => "Vault initialized"}} =
               send_cmd(%{cmd: "init", password: @test_password})

      assert File.exists?(@test_vault_path)
    end

    test "fails if vault already exists" do
      send_cmd(%{cmd: "init", password: @test_password})
      assert {:error, "Vault already exists"} = send_cmd(%{cmd: "init", password: @test_password})
    end
  end

  describe "unlock" do
    setup do
      send_cmd(%{cmd: "init", password: @test_password})
      :ok
    end

    test "unlocks with correct password" do
      assert {:ok, %{"message" => "Vault unlocked"}} =
               send_cmd(%{cmd: "unlock", password: @test_password})
    end

    test "fails with wrong password" do
      assert {:error, "Wrong password"} = send_cmd(%{cmd: "unlock", password: "wrong"})
    end
  end

  describe "secrets" do
    setup do
      send_cmd(%{cmd: "init", password: @test_password})
      :ok
    end

    test "store and get a secret" do
      assert {:ok, _} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "store", key: "api_key", value: "secret_value_123"}
               ])

      assert {:ok, %{"value" => "secret_value_123"}} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "get", key: "api_key"}
               ])
    end

    test "list keys" do
      send_cmds([
        %{cmd: "unlock", password: @test_password},
        %{cmd: "store", key: "key_a", value: "val_a"}
      ])

      send_cmds([
        %{cmd: "unlock", password: @test_password},
        %{cmd: "store", key: "key_b", value: "val_b"}
      ])

      assert {:ok, %{"keys" => keys}} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "list"}
               ])

      assert "key_a" in keys
      assert "key_b" in keys
    end

    test "update existing secret" do
      send_cmds([
        %{cmd: "unlock", password: @test_password},
        %{cmd: "store", key: "token", value: "old_value"}
      ])

      send_cmds([
        %{cmd: "unlock", password: @test_password},
        %{cmd: "store", key: "token", value: "new_value"}
      ])

      assert {:ok, %{"value" => "new_value"}} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "get", key: "token"}
               ])
    end

    test "delete a secret" do
      send_cmds([
        %{cmd: "unlock", password: @test_password},
        %{cmd: "store", key: "to_delete", value: "bye"}
      ])

      assert {:ok, _} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "delete", key: "to_delete"}
               ])

      assert {:error, "Key not found"} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "get", key: "to_delete"}
               ])
    end

    test "get non-existent key" do
      assert {:error, "Key not found"} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "get", key: "nonexistent"}
               ])
    end
  end

  describe "encrypted notes" do
    setup do
      send_cmd(%{cmd: "init", password: @test_password})
      :ok
    end

    test "add and list notes" do
      assert {:ok, %{"id" => 1}} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "note_add", text: "Secret thought"}
               ])

      assert {:ok, %{"id" => 2}} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "note_add", text: "Another secret"}
               ])

      assert {:ok, %{"notes" => notes}} =
               send_cmds([
                 %{cmd: "unlock", password: @test_password},
                 %{cmd: "notes"}
               ])

      assert length(notes) == 2
      assert Enum.at(notes, 0)["text"] == "Secret thought"
      assert Enum.at(notes, 1)["text"] == "Another secret"
      assert is_integer(Enum.at(notes, 0)["created_at"])
    end
  end

  describe "locked vault" do
    setup do
      send_cmd(%{cmd: "init", password: @test_password})
      :ok
    end

    test "operations fail when locked" do
      # Try to get without unlocking
      assert {:error, "Vault is locked"} = send_cmd(%{cmd: "get", key: "anything"})
      assert {:error, "Vault is locked"} = send_cmd(%{cmd: "list"})
      assert {:error, "Vault is locked"} = send_cmd(%{cmd: "notes"})
    end
  end
end
