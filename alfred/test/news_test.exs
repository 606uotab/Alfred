defmodule Alfred.NewsTest do
  use ExUnit.Case

  alias Alfred.News

  setup do
    Alfred.Storage.Local.ensure_data_dir!()
    Alfred.Storage.Local.ensure_subdir!("news")

    # Backup and clean today's briefing
    today = Date.utc_today() |> Date.to_iso8601()
    path = Path.join([System.user_home!(), ".alfred", "data", "news", "#{today}.json"])
    backup = File.read(path)
    File.rm(path)

    on_exit(fn ->
      case backup do
        {:ok, content} -> File.write!(path, content)
        _ -> File.rm(path)
      end
    end)

    :ok
  end

  describe "fetch_news/0" do
    test "fetches articles from API" do
      case News.fetch_news() do
        {:ok, articles} ->
          assert is_list(articles)
          assert length(articles) > 0

          first = hd(articles)
          assert Map.has_key?(first, "title")
          assert Map.has_key?(first, "source")
          assert Map.has_key?(first, "category")

        {:error, _reason} ->
          # API might not be running in CI
          :ok
      end
    end
  end

  describe "load_latest/0" do
    test "returns nil when no briefing" do
      assert News.load_latest() == nil
    end
  end

  describe "done_today?/0" do
    test "returns false when no briefing today" do
      refute News.done_today?()
    end
  end

  describe "list_briefings/0" do
    test "returns a list" do
      assert is_list(News.list_briefings())
    end
  end

  describe "Commands" do
    test "handle shows no briefing message" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.News.Commands.handle([])
      end)

      assert output =~ "briefing" or output =~ "Aucun"
    end

    test "handle unknown shows help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.News.Commands.handle(["blabla"])
      end)

      assert output =~ "alfred news"
    end
  end
end
