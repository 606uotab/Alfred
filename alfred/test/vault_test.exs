defmodule Alfred.VaultTest do
  use ExUnit.Case

  @test_vault_dir "/tmp/alfred_test_vaults"
  @test_password "test_master_password"
  @test_admin_password "test_admin_password"

  setup do
    File.rm_rf!(@test_vault_dir)
    :ok
  end

  # Send a single command to the multi-vault binary
  defp send_cmd(command) do
    binary = vault_binary_path()
    json = Jason.encode!(command)

    port =
      Port.open({:spawn_executable, binary}, [
        :binary,
        :exit_status,
        args: [@test_vault_dir],
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
        args: [@test_vault_dir],
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

  describe "init_all" do
    test "creates all 3 vaults" do
      assert {:ok, %{"message" => "All 3 vaults initialized"}} =
               send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})

      assert File.exists?(Path.join(@test_vault_dir, "creator.enc"))
      assert File.exists?(Path.join(@test_vault_dir, "users.enc"))
      assert File.exists?(Path.join(@test_vault_dir, "culture.enc"))
    end

    test "stores admin password in creator" do
      send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})

      assert {:ok, %{"value" => @test_admin_password}} =
               send_cmds([
                 %{cmd: "unlock", vault: "creator", password: @test_password},
                 %{cmd: "get", vault: "creator", key: "admin_password"}
               ])
    end

    test "fails if vaults already exist" do
      send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})

      assert {:error, "Vault already exists"} =
               send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})
    end
  end

  describe "unlock_all" do
    setup do
      send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})
      :ok
    end

    test "unlocks all vaults with correct password" do
      assert {:ok, %{"message" => "All vaults unlocked"}} =
               send_cmd(%{cmd: "unlock_all", password: @test_password})
    end

    test "fails with wrong password" do
      assert {:error, "Wrong password"} =
               send_cmd(%{cmd: "unlock_all", password: "wrong"})
    end
  end

  describe "status" do
    test "reports all vaults absent when none exist" do
      File.mkdir_p!(@test_vault_dir)

      assert {:ok, %{"vaults" => vaults}} = send_cmd(%{cmd: "status"})
      assert vaults["creator"]["exists"] == false
      assert vaults["users"]["exists"] == false
      assert vaults["culture"]["exists"] == false
    end

    test "reports all vaults present after init" do
      send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})

      assert {:ok, %{"vaults" => vaults}} = send_cmd(%{cmd: "status"})
      assert vaults["creator"]["exists"] == true
      assert vaults["users"]["exists"] == true
      assert vaults["culture"]["exists"] == true
    end
  end

  describe "per-vault secrets" do
    setup do
      send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})
      :ok
    end

    test "store and get in specific vault" do
      assert {:ok, _} =
               send_cmds([
                 %{cmd: "unlock", vault: "creator", password: @test_password},
                 %{cmd: "store", vault: "creator", key: "soul", value: "I am Alfred"}
               ])

      assert {:ok, %{"value" => "I am Alfred"}} =
               send_cmds([
                 %{cmd: "unlock", vault: "creator", password: @test_password},
                 %{cmd: "get", vault: "creator", key: "soul"}
               ])
    end

    test "vaults are isolated" do
      send_cmds([
        %{cmd: "unlock", vault: "creator", password: @test_password},
        %{cmd: "store", vault: "creator", key: "secret", value: "creator_val"}
      ])

      send_cmds([
        %{cmd: "unlock", vault: "users", password: @test_password},
        %{cmd: "store", vault: "users", key: "secret", value: "users_val"}
      ])

      assert {:ok, %{"value" => "creator_val"}} =
               send_cmds([
                 %{cmd: "unlock", vault: "creator", password: @test_password},
                 %{cmd: "get", vault: "creator", key: "secret"}
               ])

      assert {:ok, %{"value" => "users_val"}} =
               send_cmds([
                 %{cmd: "unlock", vault: "users", password: @test_password},
                 %{cmd: "get", vault: "users", key: "secret"}
               ])
    end

    test "list keys per vault" do
      send_cmds([
        %{cmd: "unlock", vault: "culture", password: @test_password},
        %{cmd: "store", vault: "culture", key: "k1", value: "v1"}
      ])

      send_cmds([
        %{cmd: "unlock", vault: "culture", password: @test_password},
        %{cmd: "store", vault: "culture", key: "k2", value: "v2"}
      ])

      assert {:ok, %{"keys" => keys}} =
               send_cmds([
                 %{cmd: "unlock", vault: "culture", password: @test_password},
                 %{cmd: "list", vault: "culture"}
               ])

      assert "k1" in keys
      assert "k2" in keys
    end

    test "delete from specific vault" do
      send_cmds([
        %{cmd: "unlock", vault: "users", password: @test_password},
        %{cmd: "store", vault: "users", key: "to_del", value: "bye"}
      ])

      assert {:ok, _} =
               send_cmds([
                 %{cmd: "unlock", vault: "users", password: @test_password},
                 %{cmd: "delete", vault: "users", key: "to_del"}
               ])

      assert {:error, "Key not found"} =
               send_cmds([
                 %{cmd: "unlock", vault: "users", password: @test_password},
                 %{cmd: "get", vault: "users", key: "to_del"}
               ])
    end

    test "unknown vault name returns error" do
      assert {:error, "unknown vault name"} =
               send_cmds([
                 %{cmd: "unlock", vault: "invalid", password: @test_password}
               ])
    end
  end

  describe "per-vault notes" do
    setup do
      send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})
      :ok
    end

    test "add and list notes in specific vault" do
      assert {:ok, %{"id" => 1}} =
               send_cmds([
                 %{cmd: "unlock", vault: "creator", password: @test_password},
                 %{cmd: "note_add", vault: "creator", text: "Secret thought"}
               ])

      assert {:ok, %{"notes" => notes}} =
               send_cmds([
                 %{cmd: "unlock", vault: "creator", password: @test_password},
                 %{cmd: "notes", vault: "creator"}
               ])

      assert length(notes) == 1
      assert Enum.at(notes, 0)["text"] == "Secret thought"
    end
  end

  describe "locked vault" do
    setup do
      send_cmd(%{cmd: "init_all", password: @test_password, value: @test_admin_password})
      :ok
    end

    test "operations fail when vault is locked" do
      assert {:error, "Vault is locked"} =
               send_cmd(%{cmd: "get", vault: "creator", key: "anything"})

      assert {:error, "Vault is locked"} =
               send_cmd(%{cmd: "list", vault: "creator"})

      assert {:error, "Vault is locked"} =
               send_cmd(%{cmd: "notes", vault: "users"})
    end
  end
end
