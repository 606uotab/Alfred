defmodule Alfred.Application do
  @moduledoc """
  Supervision OTP — Alfred veille sur ses organes.
  Le scheduler Erlang est supervisé : s'il tombe, il redémarre.
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
          {Task.Supervisor, name: Alfred.TaskSupervisor}
        ]

        Supervisor.start_link(children, strategy: :one_for_one, name: Alfred.Supervisor)

      _pid ->
        {:ok, Process.whereis(Alfred.Supervisor)}
    end
  end
end
