defmodule Alfred.Chat.SessionGuard do
  @moduledoc """
  Gardien de session — sauvegarde automatique sur arrêt brutal (Ctrl+C).
  GenServer supervisé qui retient la session active et la sauvegarde
  si le système s'arrête de façon inattendue (Ctrl+C → abort).
  """

  use GenServer

  # -- API publique --

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :inactive, name: __MODULE__)
  end

  @doc "Active le gardien avec une session de chat."
  def activate(session, token) do
    if Process.whereis(__MODULE__) do
      GenServer.cast(__MODULE__, {:activate, session, token})
    end
  end

  @doc "Met à jour la session après chaque échange."
  def update(session) do
    if Process.whereis(__MODULE__) do
      GenServer.cast(__MODULE__, {:update_session, session})
    end
  end

  @doc "Désactive le gardien (sortie normale — la sauvegarde est gérée par l'appelant)."
  def deactivate do
    if Process.whereis(__MODULE__) do
      GenServer.cast(__MODULE__, :deactivate)
    end
  end

  @doc "Vérifie si une session est active dans le gardien."
  def active? do
    if Process.whereis(__MODULE__) do
      GenServer.call(__MODULE__, :active?)
    else
      false
    end
  end

  # -- Callbacks --

  @impl true
  def init(:inactive) do
    Process.flag(:trap_exit, true)
    {:ok, %{session: nil, token: nil}}
  end

  @impl true
  def handle_call(:active?, _from, state) do
    {:reply, state.session != nil, state}
  end

  @impl true
  def handle_cast({:activate, session, token}, _state) do
    {:noreply, %{session: session, token: token}}
  end

  @impl true
  def handle_cast({:update_session, session}, state) do
    {:noreply, %{state | session: session}}
  end

  @impl true
  def handle_cast(:deactivate, _state) do
    {:noreply, %{session: nil, token: nil}}
  end

  # Sortie normale (quit/exit) — pas de sauvegarde ici, l'appelant gère
  @impl true
  def terminate(:normal, _state), do: :ok

  # Pas de session active — rien à sauvegarder
  @impl true
  def terminate(_reason, %{session: nil}), do: :ok

  # Arrêt brutal avec session active — sauvegarde d'urgence
  @impl true
  def terminate(_reason, state) do
    count =
      try do
        Alfred.Chat.Session.message_count(state.session)
      catch
        _, _ -> 0
      end

    if count > 0 do
      IO.puts("\n  Sauvegarde conversation en cours...")

      try do
        Alfred.Memory.Episodic.autosave(state.session)
      catch
        _, _ -> :ok
      end

      IO.puts("  Au revoir, Monsieur.")
    end

    :ok
  end
end
