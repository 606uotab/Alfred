defmodule Alfred.Chat.WebSearchTest do
  use ExUnit.Case

  describe "Alfred.Chat.WebSearch" do
    test "search returns ok or error tuple" do
      result = Alfred.Chat.WebSearch.search("elixir programming")
      assert {status, text} = result
      assert status in [:ok, :error]
      assert is_binary(text)
    end

    test "search with empty query returns results or error gracefully" do
      result = Alfred.Chat.WebSearch.search("")
      assert {status, text} = result
      assert status in [:ok, :error]
      assert is_binary(text)
    end

    test "module is loaded with search/1" do
      Code.ensure_loaded!(Alfred.Chat.WebSearch)
      assert {:search, 1} in Alfred.Chat.WebSearch.__info__(:functions)
    end
  end
end
