defmodule Alfred.Input do
  @moduledoc """
  Saisie utilisateur — gestion du mot de passe masqué.
  """

  @doc """
  Demande un mot de passe sans l'afficher à l'écran.
  Un processus parallèle efface continuellement la saisie visible.
  """
  def prompt_password(prompt) do
    parent = self()
    ref = make_ref()

    pid = spawn_link(fn -> hide_loop(prompt, parent, ref) end)

    value = IO.gets(prompt)
    send(pid, :stop)

    receive do
      {:hidden, ^ref} -> :ok
    after
      100 -> :ok
    end

    case value do
      :eof -> ""
      data -> String.trim(to_string(data))
    end
  end

  @doc """
  Demande une valeur secrète (même comportement que prompt_password).
  """
  def prompt_secret(prompt), do: prompt_password(prompt)

  # Boucle qui écrase en continu ce que l'utilisateur tape
  defp hide_loop(prompt, parent, ref) do
    receive do
      :stop ->
        IO.write(["\e[2K", "\e[G"])
        send(parent, {:hidden, ref})
    after
      1 ->
        IO.write(["\e[2K", "\e[G", prompt])
        hide_loop(prompt, parent, ref)
    end
  end
end
