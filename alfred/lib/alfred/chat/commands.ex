defmodule Alfred.Chat.Commands do
  @moduledoc """
  Commandes de conversation — Alfred converse avec son maître.
  """

  alias Alfred.Butler
  alias Alfred.Colors
  alias Alfred.Chat.{Client, Session, SystemPrompt}
  alias Alfred.Memory.{Episodic, Learner, Semantic, Procedural}
  alias Alfred.Vault.Port

  @doc """
  Mode chat interactif — authentification directe, puis conversation.
  """
  def handle_chat do
    case authenticate() do
      {:ok, token, soul, culture} ->
        session = build_session("chat", soul, culture)

        Butler.say("Je suis prêt à converser, Monsieur.")
        IO.puts("  #{Colors.dim("Tapez 'quit' pour terminer.")}\n")
        chat_loop(session, token, soul, culture)

      {:error, reason} ->
        Butler.say(reason)
    end
  end

  @doc """
  Mode question ponctuelle — une question, une réponse.
  """
  def handle_ask(question) do
    case authenticate() do
      {:ok, token, soul, culture} ->
        session = build_session("ask", soul, culture)
        session = Session.add_message(session, "user", question)
        messages = Session.to_api_messages(session)

        case Client.chat_completion(token, messages) do
          {:ok, response} ->
            Butler.say(response)
            session = Session.add_message(session, "assistant", response)
            save_conversation(session, token)

          {:error, reason} ->
            Butler.say("Je suis navré Monsieur, une erreur est survenue : #{reason}")
        end

      {:error, reason} ->
        Butler.say(reason)
    end
  end

  @doc """
  Authentifie et retourne les credentials. Utilisé par chat et shell.
  """
  def authenticate do
    # Try env first (no password needed)
    case System.get_env("MISTRAL_API_KEY") do
      token when is_binary(token) and byte_size(token) > 0 ->
        {:ok, token, nil, []}

      _ ->
        authenticate_with_vault()
    end
  end

  @doc """
  Authentifie avec un mot de passe donné (pour le shell).
  """
  def authenticate_with_password(password) do
    case Port.send_commands([%{cmd: "unlock_all", password: password}]) do
      {:ok, _} ->
        token = fetch_from_vault("creator", password, "mistral_api_key")
        soul = fetch_from_vault("creator", password, "alfred_soul")
        culture = fetch_culture(password)

        case token do
          nil ->
            case System.get_env("MISTRAL_API_KEY") do
              nil ->
                {:error, "Clé 'mistral_api_key' introuvable dans le coffre."}

              env_token ->
                {:ok, env_token, soul, culture}
            end

          t ->
            {:ok, t, soul, culture}
        end

      {:error, "Wrong password"} ->
        {:error, "Mot de passe incorrect, Monsieur."}

      {:error, reason} ->
        {:error, "Impossible d'ouvrir les coffres : #{reason}"}
    end
  end

  @doc """
  Envoie un message et retourne {réponse, nouvelle_session}.
  Utilisé par le shell conversationnel.
  """
  def send_message(session, token, input, soul, culture) do
    session = Session.add_message(session, "user", input)
    messages = Session.to_api_messages(session)

    case Client.chat_completion(token, messages) do
      {:ok, response} ->
        session = Session.add_message(session, "assistant", response)

        session =
          if rem(Session.message_count(session), 6) == 0 do
            refresh_context(session, input, soul, culture)
          else
            session
          end

        {:ok, response, session}

      {:error, reason} ->
        {:error, reason, session}
    end
  end

  @doc """
  Construit une session de chat avec mémoire contextuelle.
  """
  def build_session(mode, soul, culture) do
    facts = Semantic.top_facts(10)
    summaries = Episodic.recent_summaries(3)
    patterns = Procedural.active_patterns()

    system_prompt =
      SystemPrompt.build(
        facts: facts,
        summaries: summaries,
        patterns: patterns,
        soul: soul,
        culture: culture
      )

    Session.new(system_prompt, mode: mode)
  end

  # -- Vault authentication --

  defp authenticate_with_vault do
    password = Alfred.Input.prompt_password("  Mot de passe : ")

    if password == "" do
      {:error,
       "Monsieur, je ne peux converser sans accès au coffre-fort.\nAstuce : export MISTRAL_API_KEY=votre_clé"}
    else
      authenticate_with_password(password)
    end
  end

  # -- Chat loop --

  defp chat_loop(session, token, soul, culture) do
    case IO.gets("  #{Colors.cyan("Vous")} : ") do
      :eof ->
        end_chat(session, token)

      input ->
        input = String.trim(input)

        if input in ["quit", "exit", "q", "au revoir", ""] do
          end_chat(session, token)
        else
          case send_message(session, token, input, soul, culture) do
            {:ok, response, session} ->
              IO.puts("")
              IO.puts("  🎩 #{Colors.bold("Alfred")} : #{response}")
              IO.puts("")
              chat_loop(session, token, soul, culture)

            {:error, reason, session} ->
              IO.puts("  #{Colors.red("!")} #{reason}")
              IO.puts("")
              chat_loop(session, token, soul, culture)
          end
        end
    end
  end

  defp end_chat(session, token) do
    if Session.message_count(session) > 0 do
      Butler.say("Très bien. Ce fut un plaisir de converser avec vous.")
      save_conversation(session, token)
    else
      Butler.say("Au revoir.")
    end
  end

  # -- Context refresh --

  defp refresh_context(session, latest_query, soul, culture) do
    relevant_facts = Semantic.search(latest_query, limit: 5)

    # Enrichments
    cortex_summary = get_cortex_summary_for_chat()
    suggestion_count = Alfred.Culture.Suggestions.count_pending()

    system_prompt =
      SystemPrompt.build(
        facts: if(relevant_facts != [], do: relevant_facts, else: Semantic.top_facts(5)),
        summaries: Episodic.recent_summaries(2),
        patterns: Procedural.active_patterns(),
        soul: soul,
        culture: culture,
        cortex_summary: cortex_summary,
        suggestion_count: suggestion_count
      )

    %{session | system_prompt: system_prompt}
  end

  defp get_cortex_summary_for_chat do
    cortex_info = :alfred_health.check_cortex()

    if cortex_info.r_found and cortex_info.script_found do
      episodes = Episodic.list_episodes()

      case Alfred.Cortex.Port.send_command(%{cmd: "interaction_trends", episodes: episodes}) do
        {:ok, %{"trends" => %{"avg_messages_per_session" => avg}}} when avg > 0 ->
          "Sessions de #{avg} messages en moyenne"

        _ ->
          nil
      end
    else
      nil
    end
  rescue
    _ -> nil
  end

  @doc """
  Sauvegarde la conversation dans la mémoire épisodique.
  """
  def save_conversation(session, token) do
    Learner.learn(session, token)
  end

  # -- Vault helpers --

  defp fetch_from_vault(vault_name, password, key) do
    case Port.send_with_unlock(vault_name, password, %{cmd: "get", key: key}) do
      {:ok, %{"value" => value}} when byte_size(value) > 0 -> value
      _ -> nil
    end
  end

  defp fetch_culture(password) do
    case Alfred.Culture.list_all(password) do
      {:ok, entries} -> entries
      _ -> []
    end
  rescue
    _ -> []
  end

end
