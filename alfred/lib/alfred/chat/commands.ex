defmodule Alfred.Chat.Commands do
  @moduledoc """
  Commandes de conversation — Alfred converse avec son maître.
  Deux modes : chat interactif et question ponctuelle.
  """

  alias Alfred.Butler
  alias Alfred.Chat.{Client, Session, SystemPrompt}
  alias Alfred.Memory.{Episodic, Extractor, Semantic, Procedural}

  @doc """
  Mode chat interactif — boucle de conversation.
  """
  def handle_chat do
    case retrieve_api_token() do
      {:ok, token} ->
        session = build_session("chat")

        Butler.say("Bonjour Monsieur. Je suis prêt à converser. Tapez 'quit' pour terminer.\n")
        chat_loop(session, token)

      {:error, reason} ->
        Butler.say(reason)
    end
  end

  @doc """
  Mode question ponctuelle — une question, une réponse.
  """
  def handle_ask(question) do
    case retrieve_api_token() do
      {:ok, token} ->
        session = build_session("ask")
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

  # -- Boucle de chat --

  defp chat_loop(session, token) do
    case IO.gets("  Vous : ") do
      :eof ->
        end_chat(session, token)

      input ->
        input = String.trim(input)

        if input in ["quit", "exit", "q", "au revoir", ""] do
          end_chat(session, token)
        else
          session = Session.add_message(session, "user", input)
          messages = Session.to_api_messages(session)

          case Client.chat_completion(token, messages) do
            {:ok, response} ->
              IO.puts("")
              Butler.say(response)
              IO.puts("")
              session = Session.add_message(session, "assistant", response)

              # Rafraîchir le contexte mémoire tous les 6 messages
              session =
                if rem(Session.message_count(session), 6) == 0 do
                  refresh_context(session, input)
                else
                  session
                end

              chat_loop(session, token)

            {:error, reason} ->
              Butler.say("Erreur : #{reason}")
              chat_loop(session, token)
          end
        end
    end
  end

  defp end_chat(session, token) do
    if Session.message_count(session) > 0 do
      Butler.say("Très bien Monsieur. Ce fut un plaisir de converser avec vous.")
      save_conversation(session, token)
    else
      Butler.say("Au revoir, Monsieur.")
    end
  end

  # -- Construction de session avec mémoire --

  defp build_session(mode) do
    facts = Semantic.top_facts(10)
    summaries = Episodic.recent_summaries(3)
    patterns = Procedural.active_patterns()

    system_prompt =
      SystemPrompt.build(
        facts: facts,
        summaries: summaries,
        patterns: patterns
      )

    Session.new(system_prompt, mode: mode)
  end

  defp refresh_context(session, latest_query) do
    relevant_facts = Semantic.search(latest_query, limit: 5)

    if relevant_facts != [] do
      system_prompt =
        SystemPrompt.build(
          facts: relevant_facts,
          summaries: Episodic.recent_summaries(2),
          patterns: Procedural.active_patterns()
        )

      %{session | system_prompt: system_prompt}
    else
      session
    end
  end

  # -- Sauvegarde épisodique + extraction de faits --

  defp save_conversation(session, token) do
    episode = Session.to_episode(session)
    {:ok, saved} = Episodic.save_episode(episode)

    messages = session.messages
    extract_facts(messages, token, saved["id"])
  end

  defp extract_facts([_, _ | _] = messages, token, _episode_id) do
    # Tenter l'extraction via Mistral d'abord, fallback local
    case Extractor.extract_via_mistral(messages, token) do
      {:ok, [_ | _] = facts} ->
        Butler.say("(#{length(facts)} fait(s) mémorisé(s))")

      _ ->
        # Fallback : extraction locale sans API
        case Extractor.extract_local(messages) do
          {:ok, [_ | _] = facts} ->
            Butler.say("(#{length(facts)} fait(s) mémorisé(s))")

          _ ->
            :ok
        end
    end
  end

  defp extract_facts(_messages, _token, _episode_id), do: :ok

  # -- Récupération du token Mistral --

  defp retrieve_api_token do
    # D'abord, essayer la variable d'environnement (pour les tests/dev)
    case System.get_env("MISTRAL_API_KEY") do
      nil ->
        retrieve_from_vault()

      token when byte_size(token) > 0 ->
        {:ok, token}

      _ ->
        retrieve_from_vault()
    end
  end

  defp retrieve_from_vault do
    password = prompt_password("Mot de passe du coffre-fort : ")

    if password == "" do
      {:error, "Monsieur, je ne peux converser sans accès au coffre-fort contenant ma clé Mistral."}
    else
      case Alfred.Vault.Port.send_with_unlock(password, %{cmd: "get", key: "mistral_api_key"}) do
        {:ok, %{"value" => token}} when byte_size(token) > 0 ->
          {:ok, token}

        {:ok, _} ->
          {:error,
           "Monsieur, la clé 'mistral_api_key' est introuvable dans le coffre-fort.\nUtilisez : alfred vault store mistral_api_key"}

        {:error, reason} ->
          {:error, "Impossible d'ouvrir le coffre-fort : #{reason}"}
      end
    end
  end

  defp prompt_password(prompt) do
    case IO.gets(prompt) do
      :eof -> ""
      input -> String.trim(input)
    end
  end
end
