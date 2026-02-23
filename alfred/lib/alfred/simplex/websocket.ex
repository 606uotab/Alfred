defmodule Alfred.Simplex.WebSocket do
  @moduledoc """
  Client WebSocket minimal sur :gen_tcp — zéro dépendance.
  Implémente RFC 6455 pour une connexion locale (pas de TLS).
  """

  # -- Public API --

  @doc """
  Connecte à un serveur WebSocket (HTTP upgrade handshake).
  Retourne {:ok, socket} ou {:error, reason}.
  """
  def connect(host \\ ~c"localhost", port \\ 5226, path \\ "/") do
    host_charlist = if is_binary(host), do: String.to_charlist(host), else: host

    case :gen_tcp.connect(host_charlist, port, [:binary, active: false, packet: :raw], 5_000) do
      {:ok, socket} ->
        case do_handshake(socket, host, port, path) do
          :ok ->
            :inet.setopts(socket, active: true)
            {:ok, socket}

          {:error, reason} ->
            :gen_tcp.close(socket)
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Envoie un text frame masqué (opcode 0x1).
  """
  def send_text(socket, text) when is_binary(text) do
    frame = encode_frame(0x1, text)
    :gen_tcp.send(socket, frame)
  end

  @doc """
  Envoie un close frame et ferme le socket.
  """
  def close(socket) do
    frame = encode_frame(0x8, <<>>)

    try do
      :gen_tcp.send(socket, frame)
    catch
      _, _ -> :ok
    end

    :gen_tcp.close(socket)
  end

  @doc """
  Décode les frames WebSocket depuis des données TCP brutes.
  Retourne {frames, remaining_buffer}.
  frames = [{:text, payload} | :ping | {:ping, payload} | :pong | :close]
  """
  def decode_frames(data, buffer \\ <<>>) do
    do_decode(buffer <> data, [])
  end

  @doc """
  Construit un pong frame (réponse à un ping).
  """
  def pong_frame(payload \\ <<>>) do
    encode_frame(0xA, payload)
  end

  # -- Handshake HTTP --

  defp do_handshake(socket, host, port, path) do
    key = :crypto.strong_rand_bytes(16) |> Base.encode64()
    host_str = if is_list(host), do: List.to_string(host), else: host

    request =
      "GET #{path} HTTP/1.1\r\n" <>
        "Host: #{host_str}:#{port}\r\n" <>
        "Upgrade: websocket\r\n" <>
        "Connection: Upgrade\r\n" <>
        "Sec-WebSocket-Key: #{key}\r\n" <>
        "Sec-WebSocket-Version: 13\r\n" <>
        "\r\n"

    :gen_tcp.send(socket, request)

    case :gen_tcp.recv(socket, 0, 5_000) do
      {:ok, response} ->
        response_str = if is_binary(response), do: response, else: IO.iodata_to_binary(response)

        if String.contains?(response_str, "101") and
             String.contains?(response_str, "Upgrade") do
          :ok
        else
          {:error, :upgrade_failed}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # -- Frame encoding (client → server, MUST mask) --

  defp encode_frame(opcode, payload) do
    fin_opcode = <<1::1, 0::3, opcode::4>>
    mask_key = :crypto.strong_rand_bytes(4)
    masked_payload = mask_payload(payload, mask_key)
    len = byte_size(payload)

    length_bytes =
      cond do
        len <= 125 ->
          <<1::1, len::7>>

        len <= 65_535 ->
          <<1::1, 126::7, len::16>>

        true ->
          <<1::1, 127::7, len::64>>
      end

    fin_opcode <> length_bytes <> mask_key <> masked_payload
  end

  defp mask_payload(payload, <<k0, k1, k2, k3>>) do
    mask = [k0, k1, k2, k3]

    payload
    |> :binary.bin_to_list()
    |> Enum.with_index()
    |> Enum.map(fn {byte, i} -> Bitwise.bxor(byte, Enum.at(mask, rem(i, 4))) end)
    |> :binary.list_to_bin()
  end

  # -- Frame decoding (server → client, NOT masked) --

  defp do_decode(<<_fin::1, _rsv::3, opcode::4, mask_flag::1, len::7, rest::binary>> = data, acc) do
    {payload_len, header_rest} =
      case len do
        126 ->
          case rest do
            <<actual_len::16, r::binary>> -> {actual_len, r}
            _ -> {:incomplete, rest}
          end

        127 ->
          case rest do
            <<actual_len::64, r::binary>> -> {actual_len, r}
            _ -> {:incomplete, rest}
          end

        n ->
          {n, rest}
      end

    if payload_len == :incomplete do
      {Enum.reverse(acc), data}
    else
      mask_size = if mask_flag == 1, do: 4, else: 0
      needed = payload_len + mask_size

      if byte_size(header_rest) >= needed do
        {payload, remaining} =
          if mask_flag == 1 do
            <<mask_key::binary-4, masked::binary-size(payload_len), rem::binary>> = header_rest
            {mask_payload(masked, mask_key), rem}
          else
            <<raw::binary-size(payload_len), rem::binary>> = header_rest
            {raw, rem}
          end

        frame = interpret_opcode(opcode, payload)
        do_decode(remaining, [frame | acc])
      else
        {Enum.reverse(acc), data}
      end
    end
  end

  defp do_decode(data, acc), do: {Enum.reverse(acc), data}

  defp interpret_opcode(0x1, payload), do: {:text, payload}
  defp interpret_opcode(0x2, payload), do: {:binary, payload}
  defp interpret_opcode(0x8, _payload), do: :close
  defp interpret_opcode(0x9, <<>>), do: :ping
  defp interpret_opcode(0x9, payload), do: {:ping, payload}
  defp interpret_opcode(0xA, _payload), do: :pong
  defp interpret_opcode(opcode, payload), do: {:unknown, opcode, payload}
end
