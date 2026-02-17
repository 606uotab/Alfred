defmodule Alfred.Chat.Session do
  @moduledoc """
  Gestion d'une session de conversation — la mémoire de travail d'Alfred.
  Maintient l'historique des messages et le system prompt.
  """

  defstruct [
    :started_at,
    :mode,
    system_prompt: "",
    messages: [],
    model: "mistral-small-latest"
  ]

  @doc """
  Crée une nouvelle session avec un system prompt.
  """
  def new(system_prompt, opts \\ []) do
    %__MODULE__{
      started_at: DateTime.utc_now() |> DateTime.to_iso8601(),
      mode: Keyword.get(opts, :mode, "chat"),
      system_prompt: system_prompt,
      model: Keyword.get(opts, :model, "mistral-small-latest"),
      messages: []
    }
  end

  @doc """
  Ajoute un message à la session.
  """
  def add_message(%__MODULE__{} = session, role, content) when role in ["user", "assistant"] do
    message = %{
      "role" => role,
      "content" => content,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    %{session | messages: session.messages ++ [message]}
  end

  @doc """
  Construit le payload pour l'API Mistral.
  """
  def to_api_messages(%__MODULE__{} = session) do
    system_msg = %{"role" => "system", "content" => session.system_prompt}

    api_messages =
      Enum.map(session.messages, fn msg ->
        %{"role" => msg["role"], "content" => msg["content"]}
      end)

    [system_msg | api_messages]
  end

  @doc """
  Nombre de messages dans la session.
  """
  def message_count(%__MODULE__{} = session) do
    length(session.messages)
  end

  @doc """
  Convertit la session en données sauvegardables (pour mémoire épisodique).
  """
  def to_episode(%__MODULE__{} = session) do
    %{
      "started_at" => session.started_at,
      "ended_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "mode" => session.mode,
      "messages" => session.messages,
      "message_count" => length(session.messages),
      "summary" => nil,
      "topics" => [],
      "extracted_fact_ids" => []
    }
  end
end
