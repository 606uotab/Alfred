defmodule Alfred.Launcher do
  @moduledoc """
  Orchestrateur de démarrage complet d'Alfred.
  Lance le sandbox SimpleX, le daemon, et le bridge en une seule commande.
  """

  alias Alfred.Butler
  alias Alfred.Colors

  @pid_file Path.expand("~/.alfred/alfred.pid")
  @log_file Path.expand("~/.alfred/alfred.log")
  @sandbox_port 5227

  # -- API publique --

  def start(opts \\ []) do
    if opts[:background] do
      start_background()
    else
      start_foreground()
    end
  end

  def stop do
    # 1. Tuer le process Alfred (via PID file)
    alfred_stopped =
      case read_pid_file() do
        {:ok, pid} ->
          System.cmd("kill", [pid])
          delete_pid_file()
          Butler.say("Alfred arrêté (PID #{pid}).")
          true

        :error ->
          false
      end

    # 2. Tuer le sandbox SimpleX
    sandbox_stopped =
      case find_sandbox_pid() do
        nil ->
          false

        sandbox_pid ->
          System.cmd("kill", [sandbox_pid])
          IO.puts("  Sandbox SimpleX arrêté (PID #{sandbox_pid}).")
          true
      end

    unless alfred_stopped or sandbox_stopped do
      Butler.say("Alfred n'est pas actif, Monsieur.")
    end
  end

  # -- Foreground --

  defp start_foreground do
    IO.puts("")
    Butler.say("Démarrage complet d'Alfred...\n")

    # 0. Authentification Mistral AVANT tout (prompt visible)
    auth = authenticate_early()

    # 1. Nettoyer les anciens processus
    cleanup()

    # 2. Lancer le sandbox SimpleX
    start_sandbox()

    # 3. Supervision OTP
    Alfred.Storage.Local.ensure_data_dir!()
    {:ok, _} = Alfred.Application.start()

    # 4. Daemon
    unless Alfred.Daemon.running?() do
      {:ok, _} = Alfred.Daemon.start_link()
    end

    # 5. Bridge SimpleX (démarré ici car le sandbox est garanti prêt)
    start_bridge(auth)

    # 7. PID file
    write_pid_file()

    # 8. Résumé
    IO.puts("")
    IO.puts(Colors.header("Alfred actif"))
    IO.puts("  Daemon    : #{status_icon(Alfred.Daemon.running?())}")
    IO.puts("  Bridge    : #{status_icon(Alfred.Simplex.Bridge.running?())}")
    IO.puts("  Sandbox   : #{status_icon(port_open?())}")
    IO.puts("  PID       : #{System.pid()}")
    IO.puts("")
    IO.puts("  #{Colors.dim("Ctrl+C pour tout arrêter.")}\n")

    # Bloquer
    receive do
      :stop -> :ok
    end
  end

  # -- Background --

  defp start_background do
    # Vérifier si déjà actif
    case read_pid_file() do
      {:ok, pid} ->
        if process_alive?(pid) do
          Butler.say("Alfred est déjà actif (PID #{pid}).")
          return_early()
        end

      :error ->
        :ok
    end

    project_root = find_project_root()

    System.cmd("bash", [
      "-c",
      "cd #{project_root} && nohup mix run --no-halt -e 'Alfred.Launcher.start()' > #{@log_file} 2>&1 & echo $!"
    ])
    |> case do
      {output, 0} ->
        bg_pid = String.trim(output)
        Butler.say("Alfred démarré en arrière-plan.")
        IO.puts("  PID  : #{bg_pid}")
        IO.puts("  Log  : #{@log_file}")
        IO.puts("  Stop : alfred stop\n")

      {_, _} ->
        Butler.say("Erreur au lancement en arrière-plan.")
    end
  end

  # -- Bridge --

  defp authenticate_early do
    case System.get_env("MISTRAL_API_KEY") do
      token when is_binary(token) and byte_size(token) > 0 ->
        IO.puts("  #{Colors.icon_ok()} Mistral (env)")
        {:ok, token, nil, []}

      _ ->
        password = Alfred.Input.prompt_password("  Mot de passe du coffre-fort : ")

        if password == "" do
          IO.puts("  #{Colors.icon_warn()} Pas de mot de passe — bridge sans Mistral")
          :no_auth
        else
          case Alfred.Chat.Commands.authenticate_with_password(password) do
            {:ok, token, soul, culture} = result ->
              IO.puts("  #{Colors.icon_ok()} Mistral authentifié")
              Alfred.SessionStore.save(password, token, soul, culture)
              result

            {:error, reason} ->
              IO.puts("  #{Colors.icon_warn()} Auth échouée : #{reason}")
              :no_auth
          end
        end
    end
  end

  defp start_bridge(auth) do
    alias Alfred.Simplex.Bridge

    if Bridge.running?() do
      IO.puts("  #{Colors.icon_ok()} Bridge SimpleX déjà actif")
    else
      case Bridge.load_config() do
        {:ok, config} ->
          # Petit délai pour laisser simplex-chat s'initialiser complètement
          Process.sleep(1_000)

          config_with_auth = Map.put(config, "auth", auth)

          case Alfred.Application.start_bridge(config_with_auth) do
            {:ok, _} ->
              IO.puts("  #{Colors.icon_ok()} Bridge SimpleX démarré (supervisé)")

            {:error, reason} ->
              IO.puts("  #{Colors.icon_warn()} Bridge SimpleX : #{inspect(reason)}")
          end

        {:error, reason} ->
          IO.puts("  #{Colors.icon_warn()} Config SimpleX invalide : #{reason}")

        :no_config ->
          IO.puts("  #{Colors.icon_warn()} Pas de config SimpleX — bridge non démarré")
      end
    end
  rescue
    _ -> IO.puts("  #{Colors.icon_warn()} Erreur au démarrage du bridge")
  end

  # -- Sandbox --

  defp start_sandbox do
    if port_open?() do
      IO.puts("  #{Colors.icon_ok()} simplex-chat déjà actif sur #{@sandbox_port}")
    else
      script = find_sandbox_script()

      if script && File.exists?(script) do
        IO.puts("  Lancement du sandbox SimpleX...")

        Port.open(
          {:spawn_executable, "/bin/bash"},
          [:binary, :exit_status, :stderr_to_stdout, args: [script, "#{@sandbox_port}"]]
        )

        # Attendre que le port soit prêt
        case wait_for_port(@sandbox_port, 30) do
          :ok ->
            IO.puts("  #{Colors.icon_ok()} simplex-chat prêt sur #{@sandbox_port}")

          :timeout ->
            IO.puts("  #{Colors.icon_warn()} simplex-chat pas prêt après 30s — le bridge tentera de se reconnecter")
        end
      else
        IO.puts("  #{Colors.icon_warn()} Script sandbox introuvable — lancez simplex-chat manuellement")
      end
    end
  end

  # -- Nettoyage --

  defp cleanup do
    # Tuer ancien process Alfred
    case read_pid_file() do
      {:ok, pid} ->
        if process_alive?(pid) do
          System.cmd("kill", [pid])
          Process.sleep(500)
        end

        delete_pid_file()

      :error ->
        :ok
    end

    # Tuer ancien sandbox
    case find_sandbox_pid() do
      nil -> :ok
      pid ->
        System.cmd("kill", [pid])
        Process.sleep(1000)
    end
  end

  # -- PID file (public pour Daemon.Commands) --

  def write_pid_file do
    File.mkdir_p!(Path.dirname(@pid_file))
    File.write!(@pid_file, System.pid())
  end

  def read_pid_file do
    case File.read(@pid_file) do
      {:ok, content} -> {:ok, String.trim(content)}
      {:error, _} -> :error
    end
  end

  def delete_pid_file do
    File.rm(@pid_file)
  end

  # -- Helpers --

  defp port_open? do
    case :gen_tcp.connect(~c"localhost", @sandbox_port, [], 1_000) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        true

      {:error, _} ->
        false
    end
  end

  defp wait_for_port(_port, 0), do: :timeout

  defp wait_for_port(port, remaining) do
    if port_open?() do
      :ok
    else
      Process.sleep(1_000)
      wait_for_port(port, remaining - 1)
    end
  end

  defp find_sandbox_pid do
    case System.cmd("bash", ["-c", "ss -tlnp 2>/dev/null | grep ':#{@sandbox_port}' | grep -o 'pid=[0-9]*' | head -1 | cut -d= -f2"]) do
      {output, 0} ->
        pid = String.trim(output)
        if pid != "", do: pid, else: nil

      _ ->
        nil
    end
  end

  def process_alive?(pid) do
    case System.cmd("kill", ["-0", pid], stderr_to_stdout: true) do
      {_, 0} -> true
      _ -> false
    end
  end

  def find_project_root do
    # On est soit dans alfred/ soit dans Alfred/
    cond do
      File.exists?("mix.exs") -> File.cwd!()
      File.exists?("alfred/mix.exs") -> Path.join(File.cwd!(), "alfred")
      true -> Path.expand("~/Alfred/alfred")
    end
  end

  defp find_sandbox_script do
    candidates = [
      Path.join(find_project_root(), "scripts/simplex-sandbox.sh"),
      Path.expand("~/Alfred/alfred/scripts/simplex-sandbox.sh")
    ]

    Enum.find(candidates, &File.exists?/1)
  end

  defp status_icon(true), do: Colors.icon_ok()
  defp status_icon(false), do: Colors.icon_err()

  defp return_early, do: :ok
end
