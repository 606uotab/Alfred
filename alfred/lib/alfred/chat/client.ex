defmodule Alfred.Chat.Client do
  @moduledoc """
  Client HTTP pour l'API Mistral — la faculté de langage d'Alfred.
  Utilise :httpc (Erlang built-in), zéro dépendance externe.
  """

  @api_url ~c"https://api.mistral.ai/v1/chat/completions"
  @default_model "mistral-small-latest"
  @timeout 60_000

  @doc """
  Envoie une requête de chat completion à Mistral.
  Retourne {:ok, response_content} ou {:error, reason}.
  """
  def chat_completion(token, messages, opts \\ []) do
    ensure_http_started()
    model = Keyword.get(opts, :model, @default_model)

    payload =
      Jason.encode!(%{
        model: model,
        messages: messages
      })

    headers = [
      {~c"Authorization", String.to_charlist("Bearer #{token}")},
      {~c"Content-Type", ~c"application/json"}
    ]

    http_opts = [
      timeout: @timeout,
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        depth: 3
      ]
    ]

    case :httpc.request(:post, {@api_url, headers, ~c"application/json", String.to_charlist(payload)}, http_opts, []) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        response = Jason.decode!(List.to_string(body))
        content = get_in(response, ["choices", Access.at(0), "message", "content"])

        if content do
          {:ok, content}
        else
          {:error, "Réponse Mistral invalide"}
        end

      {:ok, {{_, 401, _}, _headers, _body}} ->
        {:error, "Token Mistral invalide ou expiré"}

      {:ok, {{_, 429, _}, _headers, _body}} ->
        {:error, "Limite de requêtes Mistral atteinte. Réessayez plus tard."}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, "Erreur Mistral HTTP #{status}: #{List.to_string(body) |> String.slice(0, 200)}"}

      {:error, reason} ->
        {:error, "Erreur de connexion : #{inspect(reason)}"}
    end
  end

  defp ensure_http_started do
    :inets.start()
    :ssl.start()
  end
end
