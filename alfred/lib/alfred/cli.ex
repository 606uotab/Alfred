defmodule Alfred.CLI do
  @moduledoc """
  Point d'entrée CLI — Alfred écoute les commandes de son maître.
  """

  alias Alfred.Butler
  alias Alfred.Colors
  alias Alfred.Projects.Manager, as: Projects
  alias Alfred.Projects.Task, as: Tasks
  alias Alfred.Projects.Note, as: Notes

  def main(args \\ []) do
    :io.setopts(:standard_io, encoding: :unicode)
    Alfred.Storage.Local.ensure_data_dir!()
    {:ok, _} = Alfred.Application.start()

    case args do
      [] ->
        smart_startup()

      ["project", "new" | rest] ->
        name = Enum.join(rest, " ")
        project_new(name)

      ["project", "list"] ->
        project_list()

      ["project", "delete" | rest] ->
        name = Enum.join(rest, " ")
        project_delete(name)

      ["task", "add", project | rest] ->
        description = Enum.join(rest, " ")
        task_add(project, description)

      ["task", "list"] ->
        task_list(nil)

      ["task", "list" | rest] ->
        project = Enum.join(rest, " ")
        task_list(project)

      ["task", "done", id] ->
        task_done(id)

      ["task", "priority", id, priority] ->
        task_priority(id, priority)

      ["note", "add", project | rest] ->
        text = Enum.join(rest, " ")
        note_add(project, text)

      ["note", "list"] ->
        note_list(nil)

      ["note", "list" | rest] ->
        project = Enum.join(rest, " ")
        note_list(project)

      ["vault" | vault_args] ->
        Alfred.Vault.Commands.handle(vault_args)

      ["culture" | culture_args] ->
        Alfred.Culture.Commands.handle(culture_args)

      ["user", "add" | rest] when rest != [] ->
        name = Enum.join(rest, " ")
        user_add(name)

      ["user", "list"] ->
        user_list()

      ["user", "delete" | rest] when rest != [] ->
        name = Enum.join(rest, " ")
        user_delete(name)

      ["user"] ->
        user_help()

      ["remind", "list"] ->
        Alfred.Remind.Commands.handle(["list"])

      ["remind", "done", id] ->
        Alfred.Remind.Commands.handle(["done", id])

      ["remind", "delete", id] ->
        Alfred.Remind.Commands.handle(["delete", id])

      ["remind" | rest] when rest != [] ->
        Alfred.Remind.Commands.handle(rest)

      ["remind"] ->
        Alfred.Remind.Commands.handle([])

      ["chat"] ->
        Alfred.Chat.Commands.handle_chat()

      ["ask" | rest] when rest != [] ->
        question = Enum.join(rest, " ")
        Alfred.Chat.Commands.handle_ask(question)

      ["memory" | memory_args] ->
        Alfred.Memory.Commands.handle(memory_args)

      ["briefing"] ->
        Alfred.Brain.Commands.handle_briefing()

      ["search" | rest] when rest != [] ->
        query = Enum.join(rest, " ")
        Alfred.Brain.Commands.handle_search(query)

      ["prioritize" | rest] when rest != [] ->
        project = Enum.join(rest, " ")
        Alfred.Brain.Commands.handle_prioritize(project)

      ["think", "culture"] ->
        Alfred.Brain.Commands.handle_analyze_culture()

      ["think", "about" | rest] when rest != [] ->
        project = Enum.join(rest, " ")
        Alfred.Brain.Commands.handle_analyze(project)

      ["summarize" | rest] when rest != [] ->
        project = Enum.join(rest, " ")
        Alfred.Brain.Commands.handle_summarize(project)

      ["suggest"] ->
        Alfred.Brain.Commands.handle_suggest()

      ["cortex" | cortex_args] ->
        Alfred.Cortex.Commands.handle(cortex_args)

      ["arms" | arms_args] ->
        Alfred.Arms.Commands.handle(arms_args)

      ["library" | lib_args] ->
        Alfred.Library.Commands.handle(lib_args)

      ["journal" | journal_args] ->
        Alfred.Journal.Commands.handle(journal_args)

      ["voice" | voice_args] ->
        Alfred.Voice.Commands.handle(voice_args)

      ["soul" | soul_args] ->
        Alfred.Soul.Commands.handle(soul_args)

      ["start"] ->
        Alfred.Launcher.start()

      ["start", "--bg"] ->
        Alfred.Launcher.start(background: true)

      ["stop"] ->
        Alfred.Launcher.stop()

      ["report"] ->
        daily_report()

      ["simplex" | simplex_args] ->
        Alfred.Simplex.Commands.handle(simplex_args)

      ["daemon" | daemon_args] ->
        Alfred.Daemon.Commands.handle(daemon_args)

      ["shell"] ->
        Alfred.Shell.start()

      ["dashboard"] ->
        dashboard()

      ["dashboard", "web"] ->
        dashboard_web()

      ["health"] ->
        health()

      ["status"] ->
        status()

      ["help"] ->
        help()

      unknown ->
        Butler.say("Je suis navré Monsieur, je ne comprends pas la commande : #{Enum.join(unknown, " ")}")
        Butler.say("Tapez 'alfred help' pour voir les commandes disponibles.")
    end
  end

  # -- Projets --

  defp project_new("") do
    Butler.say("Monsieur, veuillez indiquer un nom pour le projet.")
  end

  defp project_new(name) do
    case Projects.create(name) do
      {:ok, project} ->
        Butler.say("Très bien Monsieur. Le projet \"#{project.name}\" a été créé avec succès.")

      {:error, :already_exists} ->
        Butler.say("Monsieur, un projet portant le nom \"#{name}\" existe déjà.")
    end
  end

  defp project_list do
    projects = Projects.list()

    if Enum.empty?(projects) do
      Butler.say("Monsieur, vous n'avez aucun projet en cours. Souhaitez-vous en créer un ?")
    else
      Butler.say("Monsieur, voici vos projets :\n")

      projects
      |> Enum.with_index(1)
      |> Enum.each(fn {project, idx} ->
        task_count = Tasks.count_for_project(project.name)
        note_count = Notes.count_for_project(project.name)
        IO.puts("  #{idx}. #{project.name}  (#{task_count} tâches, #{note_count} notes)")
      end)
    end
  end

  defp project_delete("") do
    Butler.say("Monsieur, veuillez indiquer le nom du projet à supprimer.")
  end

  defp project_delete(name) do
    case Projects.delete(name) do
      :ok ->
        Butler.say("Le projet \"#{name}\" a été supprimé, Monsieur.")

      {:error, :not_found} ->
        Butler.say("Je suis navré Monsieur, le projet \"#{name}\" n'existe pas.")
    end
  end

  # -- Tâches --

  defp task_add(_project, "") do
    Butler.say("Monsieur, veuillez décrire la tâche à ajouter.")
  end

  defp task_add(project, description) do
    unless Projects.exists?(project) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project}\" n'existe pas.")
    else
      {:ok, task} = Tasks.add(project, description)
      count = Tasks.count_pending_for_project(project)
      Butler.say("Bien noté, Monsieur. Tâche ##{task.id} ajoutée au projet \"#{project}\". Vous avez #{count} tâches en attente sur ce projet.")
    end
  end

  defp task_list(nil) do
    tasks = Tasks.list_all()

    if Enum.empty?(tasks) do
      Butler.say("Monsieur, vous n'avez aucune tâche en cours. Profitez de ce moment de répit.")
    else
      Butler.say("Monsieur, voici l'ensemble de vos tâches :\n")
      print_tasks_grouped(tasks)
    end
  end

  defp task_list(project) do
    unless Projects.exists?(project) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project}\" n'existe pas.")
    else
      tasks = Tasks.list_for_project(project)

      if Enum.empty?(tasks) do
        Butler.say("Le projet \"#{project}\" n'a aucune tâche, Monsieur.")
      else
        Butler.say("Tâches du projet \"#{project}\" :\n")
        print_tasks(tasks)
      end
    end
  end

  defp task_done(id_str) do
    case Integer.parse(id_str) do
      {id, ""} ->
        case Tasks.complete(id) do
          {:ok, task} ->
            remaining = Tasks.count_pending_for_project(task.project)
            Butler.say("Très bien Monsieur. La tâche \"#{task.description}\" est marquée comme accomplie. Il vous reste #{remaining} tâches en attente sur le projet \"#{task.project}\".")

          {:error, :not_found} ->
            Butler.say("Je suis navré Monsieur, la tâche ##{id} n'existe pas.")

          {:error, :already_done} ->
            Butler.say("Monsieur, cette tâche est déjà accomplie.")
        end

      _ ->
        Butler.say("Monsieur, veuillez indiquer un numéro de tâche valide.")
    end
  end

  defp task_priority(id_str, priority_str) do
    with {id, ""} <- Integer.parse(id_str),
         {priority, ""} <- Integer.parse(priority_str),
         true <- priority in 1..5 do
      case Tasks.set_priority(id, priority) do
        {:ok, task} ->
          Butler.say("La priorité de la tâche \"#{task.description}\" a été définie à #{priority}, Monsieur.")

        {:error, :not_found} ->
          Butler.say("Je suis navré Monsieur, la tâche ##{id} n'existe pas.")
      end
    else
      _ ->
        Butler.say("Monsieur, veuillez indiquer un numéro de tâche et une priorité entre 1 et 5.")
    end
  end

  # -- Notes --

  defp note_add(_project, "") do
    Butler.say("Monsieur, veuillez indiquer le contenu de la note.")
  end

  defp note_add(project, text) do
    unless Projects.exists?(project) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project}\" n'existe pas.")
    else
      {:ok, _note} = Notes.add(project, text)
      Butler.say("Note ajoutée au projet \"#{project}\", Monsieur.")
    end
  end

  defp note_list(nil) do
    notes = Notes.list_all()

    if Enum.empty?(notes) do
      Butler.say("Aucune note pour le moment, Monsieur.")
    else
      Butler.say("Monsieur, voici vos notes :\n")
      print_notes_grouped(notes)
    end
  end

  defp note_list(project) do
    unless Projects.exists?(project) do
      Butler.say("Je suis navré Monsieur, le projet \"#{project}\" n'existe pas.")
    else
      notes = Notes.list_for_project(project)

      if Enum.empty?(notes) do
        Butler.say("Aucune note sur le projet \"#{project}\", Monsieur.")
      else
        Butler.say("Notes du projet \"#{project}\" :\n")
        print_notes(notes)
      end
    end
  end

  # -- User management --

  defp user_add(name) do
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    users = load_users(password)

    case users do
      {:error, reason} ->
        Butler.say(reason)

      {:ok, list} ->
        if Enum.any?(list, &(&1["name"] == name)) do
          Butler.say("Monsieur, l'utilisateur '#{name}' existe déjà.")
        else
          now = DateTime.utc_now() |> DateTime.to_iso8601()
          updated = [%{"name" => name, "created_at" => now} | list]

          case Alfred.Vault.Port.send_with_unlock("users", password, %{
                 cmd: "store",
                 key: "user_list",
                 value: Jason.encode!(updated)
               }) do
            {:ok, _} ->
              Butler.say("Utilisateur '#{name}' ajouté, Monsieur.")

            {:error, msg} ->
              Butler.say("Erreur : #{msg}")
          end
        end
    end
  end

  defp user_list do
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    case load_users(password) do
      {:error, reason} ->
        Butler.say(reason)

      {:ok, []} ->
        Butler.say("Aucun utilisateur enregistré, Monsieur.")

      {:ok, users} ->
        Butler.say("Utilisateurs enregistrés :\n")

        Enum.each(users, fn user ->
          date = String.slice(user["created_at"] || "", 0, 10)
          IO.puts("  👤 #{user["name"]}  (depuis #{date})")
        end)

        IO.puts("")
    end
  end

  defp user_delete(name) do
    password = Alfred.Input.prompt_password("Mot de passe (admin ou maître) : ")

    case load_users(password) do
      {:error, reason} ->
        Butler.say(reason)

      {:ok, list} ->
        if Enum.any?(list, &(&1["name"] == name)) do
          updated = Enum.reject(list, &(&1["name"] == name))

          case Alfred.Vault.Port.send_with_unlock("users", password, %{
                 cmd: "store",
                 key: "user_list",
                 value: Jason.encode!(updated)
               }) do
            {:ok, _} ->
              Butler.say("Utilisateur '#{name}' supprimé, Monsieur.")

            {:error, msg} ->
              Butler.say("Erreur : #{msg}")
          end
        else
          Butler.say("Monsieur, l'utilisateur '#{name}' n'existe pas.")
        end
    end
  end

  defp user_help do
    Butler.say("Monsieur, les commandes utilisateurs sont :\n")

    IO.puts("""
      alfred user add <nom>      Ajouter un utilisateur
      alfred user list            Lister les utilisateurs
      alfred user delete <nom>    Supprimer un utilisateur
    """)
  end

  defp load_users(password) do
    case Alfred.Vault.Port.send_with_unlock("users", password, %{
           cmd: "get",
           key: "user_list"
         }) do
      {:ok, %{"value" => json}} ->
        case Jason.decode(json) do
          {:ok, list} when is_list(list) -> {:ok, list}
          _ -> {:ok, []}
        end

      {:error, "Key not found"} ->
        {:ok, []}

      {:error, "Wrong password"} ->
        {:error, "Mot de passe incorrect, Monsieur."}

      {:error, msg} ->
        {:error, "Erreur : #{msg}"}
    end
  end

  # -- Smart Startup --

  defp smart_startup do
    Alfred.Vault.Migration.check_and_suggest()
    Butler.greet()
    Alfred.Remind.Commands.check_and_notify()
    Alfred.Maintenance.run_startup_checks()

    # Quick summary — pure Elixir, no external calls
    projects = Projects.list()
    all_tasks = Tasks.list_all()
    pending_tasks = Enum.count(all_tasks, &(&1.status == "pending"))
    {:ok, reminder_count} = :alfred_scheduler.count_pending()

    culture_suggestions = Alfred.Culture.Suggestions.count_pending()

    if length(projects) > 0 or reminder_count > 0 or culture_suggestions > 0 do
      IO.puts("")
      IO.puts(Colors.header("Aperçu rapide"))
      IO.puts("  #{length(projects)} projet(s), #{pending_tasks} tâche(s) en attente, #{reminder_count} rappel(s)")

      if culture_suggestions > 0 do
        IO.puts("  📚 #{culture_suggestions} suggestion(s) de culture à examiner")
      end

      maybe_show_cortex_oneliner(projects)
      IO.puts("")
    end
  end

  defp maybe_show_cortex_oneliner(projects) do
    cortex_info = :alfred_health.check_cortex()

    if cortex_info.r_found and cortex_info.script_found and projects != [] do
      projects_data = Enum.map(projects, &Alfred.ProjectData.build_for_startup/1)

      case Alfred.Cortex.Port.send_command(%{cmd: "productivity_stats", projects: projects_data}) do
        {:ok, %{"stats" => %{"one_liner" => line}}} when is_binary(line) and line != "" ->
          IO.puts("  📊 #{line}")

        _ ->
          :ok
      end
    end
  rescue
    _ -> :ok
  end

  # -- Status --

  defp status do
    projects = Projects.list()

    if Enum.empty?(projects) do
      Butler.say("Monsieur, vous n'avez aucun projet. Votre agenda est vierge.")
    else
      total_tasks = Tasks.list_all()
      pending = Enum.count(total_tasks, &(&1.status == "pending"))
      done = Enum.count(total_tasks, &(&1.status == "done"))
      total_notes = Notes.list_all() |> length()

      Butler.say("Monsieur, voici l'état de vos affaires :\n")
      {:ok, reminder_count} = :alfred_scheduler.count_pending()

      IO.puts("  Projets  : #{length(projects)}")
      IO.puts("  Tâches   : #{pending} en attente, #{done} accomplies")
      IO.puts("  Notes    : #{total_notes}")
      IO.puts("  Rappels  : #{reminder_count} en attente\n")

      Enum.each(projects, fn project ->
        p_tasks = Tasks.list_for_project(project.name)
        p_pending = Enum.count(p_tasks, &(&1.status == "pending"))
        p_done = Enum.count(p_tasks, &(&1.status == "done"))
        p_notes = Notes.count_for_project(project.name)

        status_icon = if p_pending == 0 and p_done > 0, do: "✓", else: "▸"
        IO.puts("  #{status_icon} #{project.name}  —  #{p_pending} en attente, #{p_done} accomplies, #{p_notes} notes")
      end)

      # Cross-organ enrichments
      IO.puts("")

      culture_suggestions = Alfred.Culture.Suggestions.count_pending()

      if culture_suggestions > 0 do
        IO.puts("  Suggestions culture : #{culture_suggestions} en attente")
      end

      pattern_count = Alfred.Memory.Procedural.count_active()

      if pattern_count > 0 do
        IO.puts("  Patterns détectés   : #{pattern_count} actif(s)")
      end

      maybe_show_cortex_oneliner(projects)
      IO.puts("")
    end
  end

  # -- Dashboard --

  defp dashboard do
    Butler.say("Monsieur, voici votre tableau de bord complet :\n")

    # Section 1: État des affaires
    projects = Projects.list()
    all_tasks = Tasks.list_all()
    pending = Enum.count(all_tasks, &(&1.status == "pending"))
    done = Enum.count(all_tasks, &(&1.status == "done"))
    {:ok, reminder_count} = :alfred_scheduler.count_pending()

    IO.puts(Colors.header("État des affaires"))
    IO.puts("  #{length(projects)} projet(s), #{pending} tâche(s) en attente, #{done} accomplies")
    IO.puts("  #{reminder_count} rappel(s)")
    IO.puts("")

    # Section 2: Projets
    if projects != [] do
      IO.puts(Colors.header("Projets"))

      Enum.each(projects, fn project ->
        p_tasks = Tasks.list_for_project(project.name)
        p_pending = Enum.count(p_tasks, &(&1.status == "pending"))
        p_done = Enum.count(p_tasks, &(&1.status == "done"))
        icon = if p_pending == 0 and p_done > 0, do: "✓", else: "▸"
        IO.puts("  #{icon} #{project.name}  — #{p_pending} en attente, #{p_done} accomplies")
      end)

      IO.puts("")
    end

    # Section 3: Mémoire
    fact_count = Alfred.Memory.Semantic.count()
    episode_count = Alfred.Memory.Episodic.count()
    pattern_count = Alfred.Memory.Procedural.count_active()

    IO.puts(Colors.header("Mémoire"))
    IO.puts("  #{fact_count} fait(s), #{episode_count} épisode(s), #{pattern_count} pattern(s) actif(s)")
    IO.puts("")

    # Section 4: Culture
    culture_suggestions = Alfred.Culture.Suggestions.count_pending()
    IO.puts(Colors.header("Culture"))
    IO.puts("  #{culture_suggestions} suggestion(s) en attente d'approbation")
    IO.puts("")

    # Section 5: Bras (Ada) — optionnel
    arms_info = :alfred_health.check_arms()

    if arms_info.binary_found do
      dashboard_arms()
    end

    # Section 6: Cortex (R) — optionnel
    cortex_info = :alfred_health.check_cortex()

    if cortex_info.r_found and cortex_info.script_found and projects != [] do
      dashboard_cortex(projects)
    end

    # Section 7: Cerveau (Julia) — optionnel
    brain_info = :alfred_health.check_brain()

    if brain_info.julia_found and brain_info.script_found and projects != [] do
      dashboard_brain(projects)
    end
  end

  defp dashboard_arms do
    IO.puts(Colors.header("Environnement (Bras/Ada)"))

    case Alfred.Arms.Port.send_command(%{cmd: "system_info"}) do
      {:ok, %{"info" => info}} ->
        IO.puts("  #{info["hostname"]} — #{info["os"]} — up #{info["uptime"]}")

      _ ->
        :ok
    end

    case Alfred.Arms.Port.send_command(%{cmd: "memory_usage"}) do
      {:ok, %{"memory" => mem}} ->
        total_gb = Float.round(mem["total_mb"] / 1024, 1)
        avail_gb = Float.round(mem["available_mb"] / 1024, 1)
        IO.puts("  RAM : #{avail_gb} Go disponibles / #{total_gb} Go (#{mem["percent_used"]}%)")

      _ ->
        :ok
    end

    case Alfred.Arms.Port.send_command(%{cmd: "disk_usage"}) do
      {:ok, %{"partitions" => parts}} ->
        root = Enum.find(parts, fn p -> p["mount"] == "/" end)

        if root do
          avail_gb = Float.round(root["available_mb"] / 1024, 1)
          IO.puts("  Disque / : #{avail_gb} Go disponibles (#{root["percent_used"]}% utilise)")
        end

      _ ->
        :ok
    end

    IO.puts("")
  rescue
    _ -> :ok
  end

  defp dashboard_cortex(projects) do
    projects_data = Enum.map(projects, &Alfred.ProjectData.build_for_startup/1)

    case Alfred.Cortex.Port.send_command(%{cmd: "productivity_stats", projects: projects_data}) do
      {:ok, %{"stats" => stats}} ->
        IO.puts(Colors.header("Tendances (Cortex/R)"))

        if line = stats["one_liner"] do
          IO.puts("  #{line}")
        end

        if rate = stats["overall_completion"] do
          IO.puts("  Accomplissement global : #{rate}%")
        end

        IO.puts("")

      _ ->
        :ok
    end
  rescue
    _ -> :ok
  end

  defp dashboard_brain(projects) do
    projects_data = Enum.map(projects, &Alfred.ProjectData.build_for_startup/1)

    case Alfred.Brain.Port.send_command(%{
           cmd: "suggest",
           projects: projects_data,
           now: System.system_time(:second)
         }) do
      {:ok, %{"suggestions" => suggestions}} when suggestions != [] ->
        IO.puts(Colors.header("Suggestions (Cerveau/Julia)"))

        suggestions
        |> Enum.take(3)
        |> Enum.each(fn s -> IO.puts("  #{s}") end)

        IO.puts("")

      _ ->
        :ok
    end
  rescue
    _ -> :ok
  end

  # -- Health --

  defp health do
    Butler.say("Diagnostic des organes, Monsieur :\n")

    checks = :alfred_health.check_all()

    Enum.each(checks, fn {organ, info} ->
      status = info.status
      icon = case status do
        :ok -> Colors.icon_ok()
        :warning -> Colors.icon_warn()
        :error -> Colors.icon_err()
        :down -> Colors.icon_down()
        _ -> "?"
      end

      label = organ_label(organ)
      details = organ_details(organ, info)
      IO.puts("  #{icon} #{Colors.bold(label)}  —  #{details}")
    end)

    IO.puts("")
  end

  defp organ_label(:beam), do: "Coeur (BEAM/Erlang)"
  defp organ_label(:vault), do: "Os (Zig/Vault)"
  defp organ_label(:storage), do: "Mémoire (Stockage)"
  defp organ_label(:scheduler), do: "Muscles (Scheduler)"
  defp organ_label(:brain), do: "Cerveau (Julia)"
  defp organ_label(:cortex), do: "Cortex (R)"
  defp organ_label(:arms), do: "Bras (Ada)"
  defp organ_label(:mistral), do: "Langage (Mistral AI)"
  defp organ_label(other), do: "#{other}"

  defp organ_details(:beam, info) do
    "OTP #{info.otp_release}, #{info.process_count} processus, #{info.memory_mb} Mo"
  end

  defp organ_details(:vault, info) do
    case {info.binary_found, info.vault_exists} do
      {true, true} -> "Binaire OK, #{info.vault_count}/3 coffres présents"
      {true, false} -> "Binaire OK, #{info.vault_count}/3 coffres"
      {false, _} -> "Binaire introuvable"
    end
  end

  defp organ_details(:storage, info) do
    if info.writable, do: "Répertoire accessible", else: "Répertoire inaccessible"
  end

  defp organ_details(:cortex, info) do
    case {info.r_found, info.script_found} do
      {true, true} -> "R OK, script présent"
      {true, false} -> "R OK, script introuvable"
      {false, _} -> "R introuvable"
    end
  end

  defp organ_details(:mistral, info) do
    case {info.configured, info.source} do
      {true, :env} -> "Clé API configurée (env)"
      {:possible, :vault} -> "Clé possible dans le coffre-fort"
      {false, _} -> "Clé API non configurée"
      _ -> "Statut inconnu"
    end
  end

  defp organ_details(:arms, info) do
    if info.binary_found, do: "Binaire OK", else: "Binaire introuvable"
  end

  defp organ_details(:brain, info) do
    case {info.julia_found, info.script_found} do
      {true, true} -> "Julia OK, script présent"
      {true, false} -> "Julia OK, script introuvable"
      {false, _} -> "Julia introuvable"
    end
  end

  defp organ_details(:scheduler, info) do
    if info.running do
      "Actif, #{info.reminders} rappels en attente"
    else
      "Inactif"
    end
  end

  # -- Help --

  defp help do
    Butler.say("Monsieur, voici les commandes à votre disposition :\n")

    IO.puts("""
      alfred start                                Démarrer Alfred (sandbox + daemon + bridge)
      alfred start --bg                            Idem en arrière-plan
      alfred stop                                  Arrêter Alfred
      alfred report                                Rapport quotidien d'activité

      alfred                                     Salutation
      alfred project new <nom>                   Créer un projet
      alfred project list                        Lister les projets
      alfred project delete <nom>                Supprimer un projet
      alfred task add <projet> <desc>            Ajouter une tâche
      alfred task list [projet]                  Lister les tâches
      alfred task done <id>                      Accomplir une tâche
      alfred task priority <id> <1-5>            Définir la priorité
      alfred note add <projet> <texte>           Ajouter une note
      alfred note list [projet]                  Lister les notes
      alfred status                              Vue d'ensemble
      alfred dashboard                            Tableau de bord complet

      alfred vault setup                         Créer les 3 coffres-forts
      alfred vault status                        État des coffres-forts
      alfred vault store <coffre> <clé> [val]    Stocker un secret
      alfred vault get <coffre> <clé>            Récupérer un secret
      alfred vault list <coffre>                 Lister les clés secrètes
      alfred vault delete <coffre> <clé>         Supprimer un secret
      alfred vault note <coffre> <texte>         Note confidentielle chiffrée
      alfred vault notes <coffre>                Lister les notes chiffrées
      alfred vault destroy                       Détruire tous les coffres
      alfred vault migrate                       Migrer depuis l'ancien format

      alfred user add <nom>                      Ajouter un utilisateur
      alfred user list                           Lister les utilisateurs
      alfred user delete <nom>                   Supprimer un utilisateur

      alfred culture learn <sujet> <contenu>     Enseigner une connaissance
      alfred culture search <mots>               Rechercher dans la culture
      alfred culture list                        Lister les connaissances
      alfred culture suggestions                 Suggestions auto-extraites
      alfred culture approve <id>                Approuver une suggestion
      alfred culture dismiss <id>                Rejeter une suggestion

      alfred chat                                Conversation interactive
      alfred ask <question>                      Question ponctuelle
      alfred memory facts                        Faits mémorisés
      alfred memory search <mots>                Rechercher dans la mémoire
      alfred memory episodes                     Historique des conversations
      alfred memory forget <id>                  Oublier un fait

      alfred search <mots>                       Recherche universelle
      alfred briefing                           Synthèse quotidienne
      alfred think culture                       Analyse de la culture
      alfred think about <projet>                Analyse intelligente
      alfred summarize <projet>                  Résumé du projet
      alfred suggest                             Suggestions transversales
      alfred prioritize <projet>                 Priorisation intelligente

      alfred cortex trends                      Tendances des interactions
      alfred cortex stats                       Statistiques de mémoire
      alfred cortex analyze                     Analyse comportementale
      alfred cortex productivity                Productivité des projets
      alfred cortex culture                     Tendances culturelles
      alfred cortex correlations                Analyse croisée

      alfred arms status                        État de la machine
      alfred arms disk                          Utilisation des disques
      alfred arms memory                        État de la mémoire
      alfred arms backup                        Sauvegarder les données

      alfred remind <projet> <texte> in <durée>  Programmer un rappel
      alfred remind list                         Lister les rappels
      alfred remind done <id>                    Accomplir un rappel
      alfred remind delete <id>                  Supprimer un rappel
      alfred simplex connect                      Connecter à SimpleX Chat
      alfred simplex status                       État du bridge SimpleX
      alfred simplex send [contact] <texte>       Envoyer un message
      alfred simplex disconnect                   Déconnecter le bridge
      alfred daemon start                         Démarrer le daemon (veille)
      alfred daemon status                       État du daemon
      alfred daemon stop                         Arrêter le daemon
      alfred health                              Diagnostic des organes
      alfred shell                               Mode interactif (REPL)

      alfred library                              Livre en cours + progression
      alfred library history                      Historique des livres lus
      alfred library report                        Rapports de lecture
      alfred library next                          Commencer un nouveau livre
      alfred library read                          Lire la portion du jour

      alfred journal                              Dernière entrée du journal
      alfred journal list                         Entrées récentes
      alfred journal write                        Écrire manuellement
      alfred journal show YYYY-MM-DD              Entrée d'une date

      alfred voice                              État de la voix
      alfred voice on                           Activer la voix
      alfred voice off                          Désactiver la voix
      alfred voice say <texte>                  Tester la voix

      alfred memory consolidate                 Consolider la mémoire
      alfred memory stats                       Statistiques mémoire

      alfred soul                                 Voir les traits de personnalité
      alfred soul init                            Inscrire l'âme (coffre creator)
      alfred soul check                           Vérifier l'âme
      alfred soul history                         Historique de l'évolution
      alfred soul reset                           Réinitialiser les traits

      alfred dashboard web                       Dashboard web (http://localhost:4567)

      alfred help                                Cette aide
    """)
  end

  defp dashboard_web do
    Butler.say("Démarrage du dashboard web, Monsieur...")

    case Alfred.Dashboard.Server.start_link() do
      {:ok, _pid} ->
        IO.puts("  Dashboard : http://localhost:4567")
        IO.puts("  Ctrl+C pour arrêter.")
        IO.puts("")
        # Garder le process en vie
        Process.sleep(:infinity)

      {:error, {:already_started, _}} ->
        port = Alfred.Dashboard.Server.port()
        IO.puts("  Dashboard déjà actif : http://localhost:#{port}")

      {:error, reason} ->
        Butler.say("Erreur : #{inspect(reason)}")
    end
  end

  # -- Rapport quotidien --

  defp daily_report do
    {:ok, _report, text} = Alfred.DailyReport.generate()
    Butler.say("Rapport du jour :\n")
    IO.puts(text)
    IO.puts("")
  end

  # -- Helpers d'affichage --

  defp print_tasks(tasks) do
    tasks
    |> Enum.sort_by(fn t -> {if(t.status == "done", do: 1, else: 0), -t.priority, t.id} end)
    |> Enum.each(fn task ->
      icon = if task.status == "done", do: "✓", else: "○"
      priority_str = if task.priority > 1, do: " [P#{task.priority}]", else: ""
      IO.puts("  #{icon} ##{task.id} #{task.description}#{priority_str}")
    end)
  end

  defp print_tasks_grouped(tasks) do
    tasks
    |> Enum.group_by(& &1.project)
    |> Enum.each(fn {project, project_tasks} ->
      IO.puts(Colors.header(project))
      print_tasks(project_tasks)
      IO.puts("")
    end)
  end

  defp print_notes(notes) do
    Enum.each(notes, fn note ->
      date = String.slice(note.created_at, 0, 10)
      IO.puts("  [#{date}] #{note.text}")
    end)
  end

  defp print_notes_grouped(notes) do
    notes
    |> Enum.group_by(& &1.project)
    |> Enum.each(fn {project, project_notes} ->
      IO.puts(Colors.header(project))
      print_notes(project_notes)
      IO.puts("")
    end)
  end
end
