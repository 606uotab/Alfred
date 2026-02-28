defmodule Alfred.Application do
  @moduledoc """
  Supervision OTP — Alfred veille sur ses organes.
  Le scheduler Erlang est supervisé : s'il tombe, il redémarre.
  Le bridge SimpleX est sous DynamicSupervisor : s'il crash, il redémarre.
  """

  def start do
    case Process.whereis(Alfred.Supervisor) do
      nil ->
        children = [
          %{
            id: :alfred_scheduler,
            start: {:alfred_scheduler, :start_link, []}
          },
          Alfred.Clock,
          {Task.Supervisor, name: Alfred.TaskSupervisor},
          Alfred.Chat.SessionGuard,
          {DynamicSupervisor, name: Alfred.BridgeSupervisor, strategy: :one_for_one}
        ]

        Supervisor.start_link(children, strategy: :one_for_one, name: Alfred.Supervisor)

      _pid ->
        {:ok, Process.whereis(Alfred.Supervisor)}
    end
  end

  @doc "Démarre le bridge sous supervision."
  def start_bridge(config) do
    spec = %{
      id: Alfred.Simplex.Bridge,
      start: {Alfred.Simplex.Bridge, :start_link, [config]},
      restart: :transient,
      shutdown: 5_000
    }

    case DynamicSupervisor.start_child(Alfred.BridgeSupervisor, spec) do
      {:ok, pid} -> {:ok, pid}
      {:error, {:already_started, pid}} -> {:ok, pid}
      {:error, reason} -> {:error, reason}
    end
  end
end
