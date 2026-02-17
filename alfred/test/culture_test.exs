defmodule Alfred.CultureTest do
  use ExUnit.Case

  alias Alfred.Culture

  @test_vault_dir "/tmp/alfred_culture_test_vaults"
  @test_password "culture_test_pw"
  @test_admin_password "culture_admin_pw"

  setup do
    File.rm_rf!(@test_vault_dir)

    # Initialize vaults for testing
    binary = vault_binary_path()

    port =
      Port.open({:spawn_executable, binary}, [
        :binary,
        :exit_status,
        args: [@test_vault_dir],
        line: 65536
      ])

    json = Jason.encode!(%{cmd: "init_all", password: @test_password, value: @test_admin_password})
    Port.command(port, json <> "\n")

    receive do
      {^port, {:data, {:eol, _}}} -> :ok
    after
      30_000 -> :timeout
    end

    send(port, {self(), :close})
    flush(port)

    :ok
  end

  defp vault_binary_path do
    Path.expand("native/vault/zig-out/bin/alfred-vault", File.cwd!())
  end

  defp flush(port) do
    receive do
      {^port, _} -> flush(port)
    after
      100 -> :ok
    end
  end

  describe "Culture module" do
    test "source_types returns valid types" do
      types = Culture.source_types()
      assert "person" in types
      assert "book" in types
      assert "observation" in types
      assert "web" in types
      assert "other" in types
    end
  end

  describe "Culture.Commands" do
    test "handle/1 with no args shows help" do
      # Should not crash
      assert :ok == Alfred.Culture.Commands.handle([])
    end
  end
end
