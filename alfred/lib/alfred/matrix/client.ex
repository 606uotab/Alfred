defmodule Alfred.Matrix.Client do
  @moduledoc """
  Client HTTP pour l'API Matrix Client-Server — Alfred communique avec Element.
  Utilise :httpc (zéro dépendance), même pattern que Chat.Client.
  """

  @timeout 35_000
  @sync_timeout 30_000

  @doc """
  Vérifie le token d'accès.
  """
  def whoami(config) do
    url = "#{config["homeserver"]}/_matrix/client/v3/account/whoami"
    get(url, config["access_token"])
  end

  @doc """
  Synchronise avec le serveur Matrix (long-poll).
  Retourne {:ok, %{"next_batch" => ..., "rooms" => ...}} ou {:error, reason}.
  """
  def sync(config, since \\ nil) do
    params = "timeout=#{@sync_timeout}"
    params = if since, do: "#{params}&since=#{since}", else: params

    url = "#{config["homeserver"]}/_matrix/client/v3/sync?#{params}"
    get(url, config["access_token"])
  end

  @doc """
  Envoie un message texte dans une room.
  """
  def send_message(config, room_id, text) do
    txn_id = :erlang.unique_integer([:positive]) |> Integer.to_string()
    encoded_room = URI.encode(room_id, &URI.char_unreserved?/1)
    url = "#{config["homeserver"]}/_matrix/client/v3/rooms/#{encoded_room}/send/m.room.message/#{txn_id}"

    body = Jason.encode!(%{
      "msgtype" => "m.text",
      "body" => text
    })

    put(url, config["access_token"], body)
  end

  @doc """
  Rejoint une room.
  """
  def join_room(config, room_id) do
    encoded_room = URI.encode(room_id, &URI.char_unreserved?/1)
    url = "#{config["homeserver"]}/_matrix/client/v3/join/#{encoded_room}"
    post(url, config["access_token"], "{}")
  end

  # -- HTTP helpers --

  defp get(url, token) do
    ensure_http_started()
    headers = auth_headers(token)

    case :httpc.request(:get, {String.to_charlist(url), headers}, http_opts(), body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        {:ok, Jason.decode!(body)}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, "HTTP #{status}: #{String.slice(to_string(body), 0, 200)}"}

      {:error, reason} ->
        {:error, "Connexion : #{inspect(reason)}"}
    end
  end

  defp put(url, token, body) do
    ensure_http_started()
    headers = auth_headers(token)

    case :httpc.request(:put, {String.to_charlist(url), headers, ~c"application/json", body},
           http_opts(), body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, resp_body}} ->
        {:ok, Jason.decode!(resp_body)}

      {:ok, {{_, status, _}, _headers, resp_body}} ->
        {:error, "HTTP #{status}: #{String.slice(to_string(resp_body), 0, 200)}"}

      {:error, reason} ->
        {:error, "Connexion : #{inspect(reason)}"}
    end
  end

  defp post(url, token, body) do
    ensure_http_started()
    headers = auth_headers(token)

    case :httpc.request(:post, {String.to_charlist(url), headers, ~c"application/json", body},
           http_opts(), body_format: :binary) do
      {:ok, {{_, code, _}, _headers, resp_body}} when code in [200, 204] ->
        {:ok, Jason.decode!(resp_body)}

      {:ok, {{_, status, _}, _headers, resp_body}} ->
        {:error, "HTTP #{status}: #{String.slice(to_string(resp_body), 0, 200)}"}

      {:error, reason} ->
        {:error, "Connexion : #{inspect(reason)}"}
    end
  end

  defp auth_headers(token) do
    [
      {~c"Authorization", String.to_charlist("Bearer #{token}")},
      {~c"Accept", ~c"application/json"}
    ]
  end

  defp http_opts do
    [
      timeout: @timeout,
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        depth: 3
      ]
    ]
  end

  defp ensure_http_started do
    :inets.start()
    :ssl.start()
  end
end
