defmodule Alfred.Shell do
  @moduledoc """
  Mode shell interactif — Alfred tourne en continu.
  Commandes Alfred + conversation naturelle via Mistral AI.
  """

  alias Alfred.Butler
  alias Alfred.Colors
  alias Alfred.Chat.Commands, as: Chat

  @known_commands ~w(
    project task note vault culture user remind chat ask memory
    briefing search prioritize think summarize suggest cortex arms
    soul dashboard health status help shell quit exit q
  )

  def start do
    Butler.say("Mode shell activé, Monsieur.")
    IO.puts("  #{Colors.dim("Commandes Alfred sans préfixe. Tout autre texte → conversation.")}")
    IO.puts("  #{Colors.dim("Tapez 'quit' pour quitter.")}")
    IO.puts("")

    credentials = try_authenticate()

    case credentials do
      {:ok, token, soul, culture} ->
        session = Chat.build_session("shell", soul, culture)
        IO.puts("  #{Colors.green("✓")} Connexion Mistral établie. Vous pouvez me parler, Monsieur.")
        IO.puts("")
        loop(%{token: token, soul: soul, culture: culture, session: session})

      :skip ->
        IO.puts("  #{Colors.dim("Mode commandes uniquement. Tapez 'chat' pour converser.")}")
        IO.puts("")
        loop(nil)
    end
  end

  defp try_authenticate do
    IO.puts("  #{Colors.dim("Mot de passe pour activer la conversation (Entrée pour ignorer) :")}")
    password = Alfred.Input.prompt_password("  Mot de passe : ")

    if password == "" do
      :skip
    else
      case Chat.authenticate_with_password(password) do
        {:ok, token, soul, culture} ->
          {:ok, token, soul, culture}

        {:error, reason} ->
          IO.puts("  #{Colors.red("!")} #{reason}")
          :skip
      end
    end
  end

  defp loop(state) do
    prompt =
      if state,
        do: "  #{Colors.cyan("alfred")}#{Colors.dim("›")} ",
        else: "  alfred> "

    case IO.gets(prompt) do
      :eof ->
        goodbye(state)

      input ->
        input = String.trim(input)

        cond do
          input in ["quit", "exit", "q", "au revoir"] ->
            goodbye(state)

          input == "" ->
            loop(state)

          is_command?(input) ->
            execute(String.split(input))
            loop(state)

          state != nil ->
            state = handle_conversation(input, state)
            loop(state)

          true ->
            IO.puts("  #{Colors.dim("Conversation désactivée. Tapez 'chat' pour converser.")}")
            IO.puts("")
            loop(state)
        end
    end
  end

  defp is_command?(input) do
    first_word = input |> String.split() |> List.first() |> to_string() |> String.downcase()
    first_word in @known_commands
  end

  defp handle_conversation(input, state) do
    case Chat.send_message(state.session, state.token, input, state.soul, state.culture) do
      {:ok, response, session} ->
        IO.puts("")
        IO.puts("  🎩 #{Colors.bold("Alfred")} : #{response}")
        IO.puts("")
        %{state | session: session}

      {:error, reason, session} ->
        IO.puts("  #{Colors.red("!")} #{reason}")
        IO.puts("")
        %{state | session: session}
    end
  end

  defp execute(args) do
    try do
      case args do
        ["help"] -> Alfred.CLI.main(["help"])
        ["quit"] -> :ok
        ["shell"] -> Butler.say("Vous êtes déjà en mode shell, Monsieur.")
        other -> Alfred.CLI.main(other)
      end
    rescue
      e ->
        Butler.say("Erreur inattendue : #{Exception.message(e)}")
    end
  end

  defp goodbye(state) do
    if state && Alfred.Chat.Session.message_count(state.session) > 0 do
      Chat.save_conversation(state.session, state.token)
    end

    Butler.say("Au revoir, Monsieur. Ce fut un plaisir de vous servir.")
  end
end
