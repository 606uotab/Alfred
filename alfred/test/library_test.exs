defmodule Alfred.LibraryTest do
  use ExUnit.Case

  alias Alfred.Library.{Reader, State, Downloader}

  setup do
    Alfred.Storage.Local.ensure_data_dir!()

    # Clean library state
    library_dir = Path.join([System.user_home!(), ".alfred", "data", "library"])
    state_path = Path.join(library_dir, "reading_state.json")
    history_path = Path.join(library_dir, "history.json")
    File.rm(state_path)
    File.rm(history_path)

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

  # -- Reader tests --

  describe "Reader.paginate" do
    test "splits text into pages of ~250 words" do
      # 500 words
      text = 1..500 |> Enum.map(fn i -> "word#{i}" end) |> Enum.join(" ")
      pages = Reader.paginate(text)

      assert length(pages) == 2
      {num1, _} = hd(pages)
      assert num1 == 1
      {num2, _} = List.last(pages)
      assert num2 == 2
    end

    test "handles short text" do
      pages = Reader.paginate("Hello world")
      assert length(pages) == 1
      {1, content} = hd(pages)
      assert content == "Hello world"
    end

    test "handles empty text" do
      pages = Reader.paginate("")
      assert pages == []
    end

    test "custom words per page" do
      text = 1..100 |> Enum.map(fn i -> "mot#{i}" end) |> Enum.join(" ")
      pages = Reader.paginate(text, 50)
      assert length(pages) == 2
    end
  end

  describe "Reader.pages_for_day" do
    test "returns correct pages for day 1" do
      text = 1..1500 |> Enum.map(fn i -> "mot#{i}" end) |> Enum.join(" ")
      pages = Reader.paginate(text)
      # 1500 words / 250 = 6 pages, 6 pages / 6 days = 1 page/day

      {:ok, _text, first, last} = Reader.pages_for_day(pages, 1)
      assert first == 1
      assert last == 1
    end

    test "returns correct pages for last day" do
      text = 1..3000 |> Enum.map(fn i -> "mot#{i}" end) |> Enum.join(" ")
      pages = Reader.paginate(text)
      # 3000 words / 250 = 12 pages, 12 / 6 = 2 pages/day

      {:ok, _text, first, last} = Reader.pages_for_day(pages, 6)
      assert first == 11
      assert last == 12
    end

    test "returns error if day exceeds pages" do
      pages = Reader.paginate("short text")
      assert {:error, :no_more_pages} = Reader.pages_for_day(pages, 6)
    end
  end

  describe "Reader.word_count" do
    test "counts words" do
      assert Reader.word_count("Hello beautiful world") == 3
    end

    test "handles multiple spaces" do
      assert Reader.word_count("  Hello   world  ") == 2
    end
  end

  describe "Reader.read (txt)" do
    test "reads a plain text file" do
      dir = Alfred.Storage.Local.ensure_subdir!("library")
      path = Path.join(dir, "test_book.txt")
      File.write!(path, "This is a test book content.")

      {:ok, text} = Reader.read(path)
      assert text =~ "test book"

      File.rm(path)
    end

    test "strips Gutenberg headers and footers" do
      dir = Alfred.Storage.Local.ensure_subdir!("library")
      path = Path.join(dir, "test_gutenberg.txt")

      content = """
      The Project Gutenberg eBook of Test
      *** START OF THE PROJECT GUTENBERG EBOOK ***

      This is the actual book content.
      Chapter 1: The beginning.

      *** END OF THE PROJECT GUTENBERG EBOOK ***
      Some footer text here.
      """

      File.write!(path, content)

      {:ok, text} = Reader.read(path)
      assert text =~ "actual book content"
      assert text =~ "Chapter 1"
      refute text =~ "Project Gutenberg"
      refute text =~ "footer text"

      File.rm(path)
    end

    test "returns error for missing file" do
      {:error, _} = Reader.read("/nonexistent/file.txt")
    end

    test "returns error for unsupported format" do
      {:error, msg} = Reader.read("book.pdf")
      assert msg =~ "non supporté"
    end
  end

  # -- State tests --

  describe "State" do
    test "load_current returns nil when no reading" do
      assert State.load_current() == nil
    end

    test "reading_in_progress? returns false initially" do
      refute State.reading_in_progress?()
    end

    test "start_reading creates state" do
      book_info = %{
        id: 1342,
        title: "Pride and Prejudice",
        author: "Jane Austen",
        language: "en",
        original_language: "en",
        genre: "Fiction"
      }

      state = State.start_reading(book_info, 100)

      assert state["current_book"]["title"] == "Pride and Prejudice"
      assert state["current_book"]["total_pages"] == 100
      assert state["current_book"]["pages_per_day"] == 17
      assert State.reading_in_progress?()
    end

    test "add_daily_log appends to state" do
      book_info = %{id: 1, title: "Test", author: "A", language: "en", genre: "Fiction"}
      State.start_reading(book_info, 60)

      analysis = %{
        "resume" => "Résumé du jour",
        "jugements" => [%{"idee" => "test", "jugement" => "intéressant"}],
        "valeurs" => ["persévérance"],
        "axes" => ["courage"]
      }

      {:ok, state} = State.add_daily_log(1, 1, 10, analysis)

      assert length(state["daily_logs"]) == 1
      assert hd(state["daily_logs"])["resume"] == "Résumé du jour"
      assert state["accumulated_summary"] =~ "Résumé du jour"
    end

    test "clear_current removes state" do
      book_info = %{id: 1, title: "Test", author: "A", language: "en", genre: "Fiction"}
      State.start_reading(book_info, 60)
      assert State.reading_in_progress?()

      State.clear_current()
      refute State.reading_in_progress?()
    end

    test "current_day returns 0 when no reading" do
      assert State.current_day() == 0
    end

    test "current_day returns 1 on first day" do
      book_info = %{id: 1, title: "Test", author: "A", language: "en", genre: "Fiction"}
      State.start_reading(book_info, 60)

      assert State.current_day() == 1
    end
  end

  describe "State history" do
    test "load_history returns empty list initially" do
      assert State.load_history() == []
    end

    test "add_to_history persists" do
      entry = %{
        "gutenberg_id" => 1342,
        "title" => "Pride and Prejudice",
        "author" => "Jane Austen",
        "read_at" => "2026-02-23"
      }

      State.add_to_history(entry)
      history = State.load_history()

      assert length(history) == 1
      assert hd(history)["title"] == "Pride and Prejudice"
    end

    test "read_book_ids returns IDs from history" do
      State.add_to_history(%{"gutenberg_id" => 100, "title" => "Book A"})
      State.add_to_history(%{"gutenberg_id" => 200, "title" => "Book B"})

      ids = State.read_book_ids()
      assert 100 in ids
      assert 200 in ids
    end

    test "books_read_count" do
      assert State.books_read_count() == 0

      State.add_to_history(%{"gutenberg_id" => 1, "title" => "Test"})
      assert State.books_read_count() == 1
    end
  end

  # -- Downloader tests --

  describe "Downloader.best_available_language" do
    test "prefers original language" do
      assert Downloader.best_available_language(["en", "fr", "de"], "de") == "de"
    end

    test "falls back to french" do
      assert Downloader.best_available_language(["en", "fr"], "ru") == "fr"
    end

    test "falls back to english" do
      assert Downloader.best_available_language(["en", "it"], "ja") == "en"
    end

    test "uses first available if no match" do
      assert Downloader.best_available_language(["it", "es"], "ja") == "it"
    end

    test "defaults to english for empty list" do
      assert Downloader.best_available_language([], "fr") == "en"
    end
  end

  # -- Commands tests --

  describe "Library.Commands" do
    test "handle shows no reading in progress" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Library.Commands.handle([])
      end)

      assert output =~ "Aucune lecture en cours"
    end

    test "handle status with book in progress" do
      book_info = %{id: 1, title: "Test Book", author: "Author", language: "fr", original_language: "fr", genre: "Fiction"}
      State.start_reading(book_info, 120)

      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Library.Commands.handle(["status"])
      end)

      assert output =~ "Test Book"
      assert output =~ "Author"
      assert output =~ "120"
    end

    test "handle history when empty" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Library.Commands.handle(["history"])
      end)

      assert output =~ "aucun livre"
    end

    test "handle history with books" do
      State.add_to_history(%{
        "gutenberg_id" => 1,
        "title" => "Le Petit Prince",
        "author" => "Saint-Exupéry",
        "language" => "fr",
        "read_at" => "2026-02-20"
      })

      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Library.Commands.handle(["history"])
      end)

      assert output =~ "Le Petit Prince"
      assert output =~ "Saint-Exupéry"
    end

    test "handle unknown shows help" do
      output = ExUnit.CaptureIO.capture_io(fn ->
        Alfred.Library.Commands.handle(["unknown"])
      end)

      assert output =~ "library"
      assert output =~ "history"
      assert output =~ "next"
    end
  end
end
