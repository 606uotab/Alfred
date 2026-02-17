defmodule Alfred.Chat.Commands do
  @moduledoc """
  Commandes de conversation — Alfred converse avec son maître ou ses utilisateurs.
  Authentification multi-rôle : Maître, Admin, User.
  """

  alias Alfred.Butler
  alias Alfred.Chat.{Client, Session, SystemPrompt}
  alias Alfred.Memory.{Episodic, Learner, Semantic, Procedural}
  alias Alfred.Vault.Port

  @doc """
  Mode chat interactif — demande le rôle puis lance la boucle de conversation.
  """
  def handle_chat do
    case authenticate_interactive() do
      {:ok, role, token, soul, culture} ->
        session = build_session("chat", soul, culture)
        role_label = role_label(role)

        Butler.say("Bienvenue, #{role_label}. Je suis prêt à converser. Tapez 'quit' pour terminer.\n")
        chat_loop(session, token, soul, culture)

      {:error, reason} ->
        Butler.say(reason)
    end
  end

  @doc """
  Mode question ponctuelle — une question, une réponse.
  """
  def handle_ask(question) do
    case retrieve_credentials() do
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

  # -- Interactive authentication --

  defp authenticate_interactive do
    Butler.say("Qui ai-je le plaisir de servir ?\n")
    IO.puts("  1. Maître (accès complet)")
    IO.puts("  2. Admin (users + culture)")
    IO.puts("  3. Utilisateur\n")

    choice = IO.gets("  Choix (1-3) : ") |> String.trim()

    case choice do
      "1" -> authenticate_master()
      "2" -> authenticate_admin()
      "3" -> authenticate_user()
      _ -> {:error, "Choix invalide, Monsieur."}
    end
  end

  defp authenticate_master do
    password = Alfred.Input.prompt_password("Mot de passe maître : ")

    if password == "" do
      {:error, "Mot de passe requis, Monsieur."}
    else
      case Port.send_commands([%{cmd: "unlock_all", password: password}]) do
        {:ok, _} ->
          # Retrieve API key from creator vault
          token = fetch_from_vault("creator", password, "mistral_api_key")
          soul = fetch_from_vault("creator", password, "alfred_soul")
          culture = fetch_culture(password)

          case token do
            nil ->
              # Fallback to env
              case System.get_env("MISTRAL_API_KEY") do
                nil ->
                  {:error,
                   "Monsieur, la clé 'mistral_api_key' est introuvable.\nStockez-la : alfred vault store creator mistral_api_key"}

                env_token ->
                  {:ok, :master, env_token, soul, culture}
              end

            t ->
              {:ok, :master, t, soul, culture}
          end

        {:error, "Wrong password"} ->
          {:error, "Mot de passe incorrect, Monsieur."}

        {:error, reason} ->
          {:error, "Impossible d'ouvrir les coffres : #{reason}"}
      end
    end
  end

  defp authenticate_admin do
    password = Alfred.Input.prompt_password("Mot de passe admin : ")

    if password == "" do
      {:error, "Mot de passe requis."}
    else
      case Port.send_commands([
             %{cmd: "unlock", vault: "users", password: password},
             %{cmd: "unlock", vault: "culture", password: password}
           ]) do
        {:ok, _} ->
          # API key: try users vault, then env
          token = fetch_from_vault("users", password, "mistral_api_key")
          culture = fetch_culture(password)

          token = token || System.get_env("MISTRAL_API_KEY")

          if token do
            {:ok, :admin, token, nil, culture}
          else
            {:error,
             "Clé API introuvable. Configurez MISTRAL_API_KEY ou stockez-la dans users vault."}
          end

        {:error, "Wrong password"} ->
          {:error, "Mot de passe incorrect."}

        {:error, reason} ->
          {:error, "Impossible d'ouvrir les coffres : #{reason}"}
      end
    end
  end

  defp authenticate_user do
    name = IO.gets("  Votre nom : ") |> String.trim()

    if name == "" do
      {:error, "Nom requis."}
    else
      token = System.get_env("MISTRAL_API_KEY")

      if token do
        Butler.say("Enchanté, #{name}.")
        {:ok, :user, token, nil, []}
      else
        {:error, "#{name}, la conversation nécessite une clé API. Contactez votre administrateur."}
      end
    end
  end

  # -- Credentials retrieval (for ask mode, backward-compatible) --

  defp retrieve_credentials do
    case System.get_env("MISTRAL_API_KEY") do
      nil ->
        retrieve_from_vault()

      token when byte_size(token) > 0 ->
        {:ok, token, nil, []}

      _ ->
        retrieve_from_vault()
    end
  end

  defp retrieve_from_vault do
    password = Alfred.Input.prompt_password("Mot de passe du coffre-fort : ")

    if password == "" do
      {:error,
       "Monsieur, je ne peux converser sans accès au coffre-fort contenant ma clé Mistral.\nAstuce : export MISTRAL_API_KEY=votre_clé pour éviter le coffre-fort."}
    else
      # Try master unlock
      case Port.send_commands([%{cmd: "unlock_all", password: password}]) do
        {:ok, _} ->
          token = fetch_from_vault("creator", password, "mistral_api_key")
          soul = fetch_from_vault("creator", password, "alfred_soul")
          culture = fetch_culture(password)

          if token do
            {:ok, token, soul, culture}
          else
            {:error,
             "Monsieur, la clé 'mistral_api_key' est introuvable.\nUtilisez : alfred vault store creator mistral_api_key"}
          end

        {:error, "Wrong password"} ->
          {:error, "Mot de passe incorrect, Monsieur."}

        {:error, reason} ->
          {:error, "Impossible d'ouvrir le coffre-fort : #{reason}"}
      end
    end
  end

  # -- Chat loop --

  defp chat_loop(session, token, soul, culture) do
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

              session =
                if rem(Session.message_count(session), 6) == 0 do
                  refresh_context(session, input, soul, culture)
                else
                  session
                end

              chat_loop(session, token, soul, culture)

            {:error, reason} ->
              Butler.say("Erreur : #{reason}")
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

  # -- Session building with memory + culture --

  defp build_session(mode, soul, culture) do
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

  defp refresh_context(session, latest_query, soul, culture) do
    relevant_facts = Semantic.search(latest_query, limit: 5)

    if relevant_facts != [] do
      system_prompt =
        SystemPrompt.build(
          facts: relevant_facts,
          summaries: Episodic.recent_summaries(2),
          patterns: Procedural.active_patterns(),
          soul: soul,
          culture: culture
        )

      %{session | system_prompt: system_prompt}
    else
      session
    end
  end

  defp save_conversation(session, token) do
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

  defp role_label(:master), do: "Monsieur"
  defp role_label(:admin), do: "Administrateur"
  defp role_label(:user), do: "cher utilisateur"
end
