defmodule Alfred.Dashboard.Server do
  @moduledoc """
  Serveur HTTP minimal pour le dashboard d'Alfred.
  Utilise :gen_tcp — zéro dépendance externe.
  Sert une page HTML + API JSON sur le port 4567.
  """

  use GenServer

  @default_port 4567

  def start_link(opts \\ []) do
    port = Keyword.get(opts, :port, @default_port)
    GenServer.start_link(__MODULE__, port, name: __MODULE__)
  end

  def stop do
    if running?() do
      GenServer.stop(__MODULE__, :normal)
    end
  end

  def running? do
    Process.whereis(__MODULE__) != nil
  end

  def port do
    if running?(), do: GenServer.call(__MODULE__, :port), else: nil
  end

  # -- GenServer callbacks --

  @impl true
  def init(port) do
    case :gen_tcp.listen(port, [
      :binary,
      packet: :raw,
      active: false,
      reuseaddr: true
    ]) do
      {:ok, listen_socket} ->
        IO.puts("[Dashboard] Serveur démarré sur http://localhost:#{port}")
        # Start accept loop
        pid = self()
        Task.start(fn -> accept_loop(listen_socket, pid) end)
        {:ok, %{listen_socket: listen_socket, port: port}}

      {:error, reason} ->
        IO.puts("[Dashboard] Erreur démarrage : #{inspect(reason)}")
        {:stop, reason}
    end
  end

  @impl true
  def handle_call(:port, _from, state) do
    {:reply, state.port, state}
  end

  @impl true
  def terminate(_reason, state) do
    :gen_tcp.close(state.listen_socket)
    :ok
  end

  # -- Accept loop --

  defp accept_loop(listen_socket, server_pid) do
    case :gen_tcp.accept(listen_socket, 5000) do
      {:ok, client} ->
        Task.start(fn -> handle_request(client) end)
        accept_loop(listen_socket, server_pid)

      {:error, :timeout} ->
        accept_loop(listen_socket, server_pid)

      {:error, :closed} ->
        :ok

      {:error, _} ->
        :ok
    end
  end

  # -- HTTP request handling --

  defp handle_request(socket) do
    case :gen_tcp.recv(socket, 0, 5000) do
      {:ok, data} ->
        {method, path} = parse_request(data)

        response = if method == "GET" do
          route(path)
        else
          http_response(405, "text/plain", "Method Not Allowed")
        end

        :gen_tcp.send(socket, response)
        :gen_tcp.close(socket)

      {:error, _} ->
        :gen_tcp.close(socket)
    end
  end

  defp parse_request(data) do
    case String.split(data, "\r\n") |> hd() |> String.split(" ") do
      [method, path | _] -> {method, path}
      _ -> {"GET", "/"}
    end
  end

  # -- Routing --

  defp route("/"), do: http_response(200, "text/html; charset=utf-8", Alfred.Dashboard.Html.page())
  defp route("/api/status"), do: json_response(api_status())
  defp route("/api/soul"), do: json_response(api_soul())
  defp route("/api/memory"), do: json_response(api_memory())
  defp route("/api/journal"), do: json_response(api_journal())
  defp route("/api/library"), do: json_response(api_library())
  defp route("/api/report"), do: json_response(api_report())
  defp route(_), do: http_response(404, "text/plain", "Not Found")

  # -- API endpoints --

  defp api_status do
    daemon = if Alfred.Daemon.running?() do
      info = Alfred.Daemon.status()
      %{running: true, uptime: info.uptime_human, checks: info.check_count}
    else
      %{running: false}
    end

    bridge = if Alfred.Simplex.Bridge.running?() do
      info = Alfred.Simplex.Bridge.status()
      %{running: true, connected: info.connected, messages: info.message_count}
    else
      %{running: false}
    end

    %{daemon: daemon, bridge: bridge}
  end

  defp api_soul do
    soul = try do
      s = Alfred.Soul.State.load()
      %{mood: s.mood, traits: s.traits}
    rescue
      _ -> %{mood: "?", traits: %{}}
    end

    convictions = try do
      c = Alfred.Soul.Convictions.load()
      all = c["convictions"] || []
      mature = Enum.count(all, fn c -> (c["confidence"] || 0) >= 0.6 end)
      %{total: length(all), mature: mature}
    rescue
      _ -> %{total: 0, mature: 0}
    end

    %{soul: soul, convictions: convictions}
  end

  defp api_memory do
    Alfred.Memory.Consolidator.stats()
  end

  defp api_journal do
    case Alfred.Journal.load_latest() do
      {:ok, entry} -> entry
      _ -> %{entry: nil}
    end
  end

  defp api_library do
    current = case Alfred.Library.State.load_current() do
      nil -> nil
      state ->
        book = state["current_book"]
        day = Alfred.Library.State.current_day()
        %{title: book["title"], author: book["author"], day: day, total_pages: book["total_pages"]}
    end

    history = Alfred.Library.State.load_history()
      |> Enum.take(5)
      |> Enum.map(fn h -> %{title: h["title"], author: h["author"]} end)

    %{current: current, history: history, books_read: Alfred.Library.State.books_read_count()}
  rescue
    _ -> %{current: nil, history: []}
  end

  defp api_report do
    today = Date.utc_today() |> Date.to_iso8601()
    dir = Path.join([System.user_home!(), ".alfred", "data", "reports"])
    path = Path.join(dir, "#{today}.json")

    if File.exists?(path) do
      path |> File.read!() |> Jason.decode!()
    else
      %{date: nil}
    end
  rescue
    _ -> %{date: nil}
  end

  # -- HTTP helpers --

  defp json_response(data) do
    body = Jason.encode!(data)
    http_response(200, "application/json", body)
  end

  defp http_response(status, content_type, body) do
    status_text = case status do
      200 -> "OK"
      404 -> "Not Found"
      405 -> "Method Not Allowed"
      _ -> "Error"
    end

    "HTTP/1.1 #{status} #{status_text}\r\n" <>
    "Content-Type: #{content_type}\r\n" <>
    "Content-Length: #{byte_size(body)}\r\n" <>
    "Access-Control-Allow-Origin: *\r\n" <>
    "Connection: close\r\n" <>
    "\r\n" <>
    body
  end
end
