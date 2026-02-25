defmodule Alfred.Simplex.CommandParserTest do
  use ExUnit.Case

  alias Alfred.Simplex.CommandParser

  describe "parse/1" do
    test "parses simple command" do
      assert CommandParser.parse("/status") == {:command, "status", []}
    end

    test "parses command with args" do
      assert CommandParser.parse("/library next") == {:command, "library", ["next"]}
    end

    test "parses command with multiple args" do
      assert CommandParser.parse("/library read 5") == {:command, "library", ["read", "5"]}
    end

    test "handles case insensitive" do
      assert CommandParser.parse("/HELP") == {:command, "help", []}
      assert CommandParser.parse("/Status") == {:command, "status", []}
    end

    test "returns not_command for normal text" do
      assert CommandParser.parse("bonjour") == :not_command
      assert CommandParser.parse("salut Alfred") == :not_command
    end

    test "returns not_command for empty string" do
      assert CommandParser.parse("") == :not_command
    end

    test "returns not_command for slash alone" do
      assert CommandParser.parse("/") == :not_command
    end

    test "handles whitespace" do
      assert CommandParser.parse("  /status  ") == {:command, "status", []}
      assert CommandParser.parse("  bonjour  ") == :not_command
    end

    test "all known commands parse correctly" do
      commands = ~w(status report library health help brain cortex soul system journal memory voice dashboard)

      for cmd <- commands do
        assert {:command, ^cmd, []} = CommandParser.parse("/#{cmd}")
      end
    end

    test "parses system subcommands" do
      assert CommandParser.parse("/system disk") == {:command, "system", ["disk"]}
      assert CommandParser.parse("/system memory") == {:command, "system", ["memory"]}
      assert CommandParser.parse("/system backup") == {:command, "system", ["backup"]}
    end
  end
end
