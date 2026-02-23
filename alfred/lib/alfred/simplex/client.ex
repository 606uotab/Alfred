defmodule Alfred.Simplex.Client do
  @moduledoc """
  Client pour l'API SimpleX Chat — communique via WebSocket JSON.
  Protocole : {"corrId": "id", "cmd": "/commande"} → {"corrId": "id", "resp": {...}}
  """

  alias Alfred.Simplex.WebSocket

  @doc "Connecte au serveur WebSocket SimpleX Chat."
  def connect(host \\ ~c"localhost", port \\ 5226) do
    WebSocket.connect(host, port, "/")
  end

  @doc "Déconnecte proprement."
  def disconnect(socket) do
    WebSocket.close(socket)
  end

  @doc "Envoie une commande SimpleX et retourne le corrId."
  def send_command(socket, command) do
    corr_id = generate_corr_id()
    payload = Jason.encode!(%{"corrId" => corr_id, "cmd" => command})
    :ok = WebSocket.send_text(socket, payload)
    {:ok, corr_id}
  end

  @doc "Envoie un message direct à un contact."
  def send_direct_message(socket, contact_name, text) do
    send_command(socket, "@#{contact_name} #{text}")
  end

  @doc "Envoie un message dans un groupe."
  def send_group_message(socket, group_name, text) do
    send_command(socket, "##{group_name} #{text}")
  end

  @doc "Liste les contacts."
  def list_contacts(socket) do
    send_command(socket, "/contacts")
  end

  @doc "Liste les groupes."
  def list_groups(socket) do
    send_command(socket, "/groups")
  end

  @doc """
  Parse une réponse JSON SimpleX.
  Retourne {:response, corr_id, resp} | {:event, resp} | {:error, reason}
  """
  def parse_response(json_text) do
    case Jason.decode(json_text) do
      {:ok, %{"corrId" => corr_id, "resp" => resp}} when is_binary(corr_id) ->
        {:response, corr_id, resp}

      {:ok, %{"resp" => resp}} ->
        {:event, resp}

      {:ok, other} ->
        {:unknown, other}

      {:error, _} ->
        {:error, :invalid_json}
    end
  end

  defp generate_corr_id do
    :erlang.unique_integer([:positive]) |> Integer.to_string()
  end
end
