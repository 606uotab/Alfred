defmodule Alfred.Clock do
  @moduledoc """
  La Montre d'Alfred — conscience du temps et rappels automatiques.
  Tick toutes les 30 secondes, vérifie les rappels dus, notifie via SimpleX.
  """

  use GenServer

  @tick_interval 30_000

  # -- API publique --

  def start_link(_opts \\ []) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc "Retourne l'heure locale (UTC+1 Paris)."
  def now do
    DateTime.utc_now() |> DateTime.add(3600, :second)
  end

  @doc "Retourne le statut de la montre."
  def status do
    if Process.whereis(__MODULE__) do
      GenServer.call(__MODULE__, :status)
    else
      :not_running
    end
  end

  # -- Callbacks --

  @impl true
  def init(_) do
    schedule_tick()

    {:ok,
     %{
       tick_count: 0,
       reminders_sent: 0,
       started_at: DateTime.utc_now()
     }}
  end

  @impl true
  def handle_call(:status, _from, state) do
    uptime = DateTime.diff(DateTime.utc_now(), state.started_at)

    info = %{
      tick_count: state.tick_count,
      reminders_sent: state.reminders_sent,
      started_at: state.started_at,
      uptime_seconds: uptime,
      uptime_human: format_uptime(uptime),
      now: now()
    }

    {:reply, info, state}
  end

  @impl true
  def handle_info(:tick, state) do
    sent = check_and_notify()
    schedule_tick()

    {:noreply,
     %{
       state
       | tick_count: state.tick_count + 1,
         reminders_sent: state.reminders_sent + sent
     }}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  # -- Logique interne --

  defp check_and_notify do
    case :alfred_scheduler.check_due() do
      {:ok, due} when is_list(due) and due != [] ->
        Enum.each(due, fn r ->
          text = "⏰ Rappel [#{r.project}] #{r.text}"

          Alfred.Simplex.Bridge.send_group_notification(text)

          try do
            :alfred_scheduler.complete_reminder(r.id)
          catch
            _, _ -> :ok
          end
        end)

        length(due)

      _ ->
        0
    end
  rescue
    _ -> 0
  end

  defp schedule_tick do
    Process.send_after(self(), :tick, @tick_interval)
  end

  defp format_uptime(seconds) when seconds < 60, do: "#{seconds}s"
  defp format_uptime(seconds) when seconds < 3600, do: "#{div(seconds, 60)}min"

  defp format_uptime(seconds) do
    h = div(seconds, 3600)
    m = div(rem(seconds, 3600), 60)
    "#{h}h #{m}min"
  end
end
