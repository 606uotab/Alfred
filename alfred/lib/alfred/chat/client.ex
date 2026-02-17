defmodule Alfred.Chat.Client do
  @moduledoc """
  Client HTTP pour l'API Mistral — la faculté de langage d'Alfred.
  Utilise :httpc (Erlang built-in), zéro dépendance externe.
  """

  @api_url ~c"https://api.mistral.ai/v1/chat/completions"
  @default_model "mistral-small-latest"
  @default_temperature 0.3
  @default_max_tokens 1024
  @timeout 60_000

  @doc """
  Envoie une requête de chat completion à Mistral.
  Retourne {:ok, response_content} ou {:error, reason}.
  """
  def chat_completion(token, messages, opts \\ []) do
    ensure_http_started()
    model = Keyword.get(opts, :model, @default_model)
    temperature = Keyword.get(opts, :temperature, @default_temperature)
    max_tokens = Keyword.get(opts, :max_tokens, @default_max_tokens)

    payload =
      Jason.encode!(%{
        model: model,
        messages: messages,
        temperature: temperature,
        max_tokens: max_tokens
      })

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
    content = get_in(response, ["choices", Access.at(0), "message", "content"])

    case content do
      nil ->
        {:error, "Réponse Mistral invalide"}

      text when is_binary(text) ->
        {:ok, text}

      parts when is_list(parts) ->
        # Le content peut être une liste d'objets [{type: "text", text: "..."}]
        text =
          parts
          |> Enum.filter(&(is_map(&1) and &1["type"] == "text"))
          |> Enum.map_join("\n", & &1["text"])

        if text != "", do: {:ok, text}, else: {:error, "Réponse Mistral vide"}

      _ ->
        {:error, "Format de réponse Mistral inattendu"}
    end
  end

  defp ensure_http_started do
    :inets.start()
    :ssl.start()
  end
end
