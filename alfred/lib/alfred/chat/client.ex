defmodule Alfred.Chat.Client do
  @moduledoc """
  Client HTTP pour l'API Mistral — la faculté de langage d'Alfred.
  Utilise :httpc (Erlang built-in), zéro dépendance externe.
  Supporte le function calling (tools).
  """

  @api_url ~c"https://api.mistral.ai/v1/chat/completions"
  @default_model "mistral-small-latest"
  @default_temperature 0.3
  @default_max_tokens 1024
  @timeout 60_000

  @doc """
  Envoie une requête de chat completion à Mistral.
  Retourne {:ok, text}, {:tool_calls, calls, assistant_msg} ou {:error, reason}.
  """
  def chat_completion(token, messages, opts \\ []) do
    ensure_http_started()
    model = Keyword.get(opts, :model, @default_model)
    temperature = Keyword.get(opts, :temperature, @default_temperature)
    max_tokens = Keyword.get(opts, :max_tokens, @default_max_tokens)
    tools = Keyword.get(opts, :tools, nil)

    payload =
      %{
        model: model,
        messages: messages,
        temperature: temperature,
        max_tokens: max_tokens
      }
      |> maybe_add_tools(tools)
      |> Jason.encode!()

    headers = [
      {~c"Authorization", String.to_charlist("Bearer #{token}")},
      {~c"Accept", ~c"application/json"}
    ]

    http_opts = [
      timeout: @timeout,
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        depth: 3
      ]
    ]

    case :httpc.request(:post, {@api_url, headers, ~c"application/json", payload}, http_opts, body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        parse_response(body)

      {:ok, {{_, 401, _}, _headers, _body}} ->
        {:error, "Token Mistral invalide ou expiré"}

      {:ok, {{_, 429, _}, _headers, _body}} ->
        {:error, "Limite de requêtes Mistral atteinte. Réessayez plus tard."}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, "Erreur Mistral HTTP #{status}: #{String.slice(body, 0, 200)}"}

      {:error, reason} ->
        {:error, "Erreur de connexion : #{inspect(reason)}"}
    end
  end

  # -- Parsing de la réponse --

  defp parse_response(body) do
    response = Jason.decode!(body)
    message = get_in(response, ["choices", Access.at(0), "message"])

    cond do
      # Function calling — tool_calls present
      is_map(message) and is_list(message["tool_calls"]) and message["tool_calls"] != [] ->
        {:tool_calls, message["tool_calls"], message}

      # Regular text response
      is_map(message) and is_binary(message["content"]) ->
        {:ok, message["content"]}

      # Content as list of parts
      is_map(message) and is_list(message["content"]) ->
        text =
          message["content"]
          |> Enum.filter(&(is_map(&1) and &1["type"] == "text"))
          |> Enum.map_join("\n", & &1["text"])

        if text != "", do: {:ok, text}, else: {:error, "Réponse Mistral vide"}

      true ->
        {:error, "Réponse Mistral invalide"}
    end
  end

  defp maybe_add_tools(payload, nil), do: payload
  defp maybe_add_tools(payload, []), do: payload
  defp maybe_add_tools(payload, tools), do: Map.put(payload, :tools, tools)

  defp ensure_http_started do
    :inets.start()
    :ssl.start()
  end
end
