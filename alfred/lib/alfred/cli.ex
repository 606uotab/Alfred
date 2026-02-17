defmodule Alfred.CLI do
  @moduledoc """
  Point d'entrée CLI — Alfred écoute les commandes de son maître.
  """

  alias Alfred.Butler
  alias Alfred.Projects.Manager, as: Projects
  alias Alfred.Projects.Task, as: Tasks
  alias Alfred.Projects.Note, as: Notes

  def main(args \\ []) do
    Alfred.Storage.Local.ensure_data_dir!()
    {:ok, _} = Alfred.Application.start()

    case args do
      [] ->
        Butler.greet()
        Alfred.Remind.Commands.check_and_notify()

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
    end
  end

  # -- Health --

  defp health do
    Butler.say("Diagnostic des organes, Monsieur :\n")

    checks = :alfred_health.check_all()

    Enum.each(checks, fn {organ, info} ->
      status = info.status
      icon = case status do
        :ok -> "✓"
        :warning -> "⚠"
        :error -> "✗"
        :down -> "✗"
        _ -> "?"
      end

      label = organ_label(organ)
      details = organ_details(organ, info)
      IO.puts("  #{icon} #{label}  —  #{details}")
    end)

    IO.puts("")
  end

  defp organ_label(:beam), do: "Coeur (BEAM/Erlang)"
  defp organ_label(:vault), do: "Os (Zig/Vault)"
  defp organ_label(:storage), do: "Mémoire (Stockage)"
  defp organ_label(:scheduler), do: "Muscles (Scheduler)"
  defp organ_label(:brain), do: "Cerveau (Julia)"
  defp organ_label(:cortex), do: "Cortex (R)"
  defp organ_label(:mistral), do: "Langage (Mistral AI)"
  defp organ_label(other), do: "#{other}"

  defp organ_details(:beam, info) do
    "OTP #{info.otp_release}, #{info.process_count} processus, #{info.memory_mb} Mo"
  end

  defp organ_details(:vault, info) do
    case {info.binary_found, info.vault_exists} do
      {true, true} -> "Binaire OK, coffre-fort présent"
      {true, false} -> "Binaire OK, pas de coffre-fort"
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

      alfred vault init                          Créer le coffre-fort
      alfred vault store mistral_api_key         Stocker la clé Mistral
      alfred vault store <nom> [valeur]          Stocker un secret
      alfred vault get <nom>                     Récupérer un secret
      alfred vault list                          Lister les clés secrètes
      alfred vault delete <clé>                  Supprimer un secret
      alfred vault note <texte>                  Note confidentielle chiffrée
      alfred vault notes                         Lister les notes chiffrées
      alfred vault destroy                       Détruire le coffre-fort

      alfred chat                                Conversation interactive
      alfred ask <question>                       Question ponctuelle
      alfred memory facts                         Faits mémorisés
      alfred memory search <mots>                 Rechercher dans la mémoire
      alfred memory episodes                      Historique des conversations
      alfred memory forget <id>                   Oublier un fait

      alfred think about <projet>                Analyse intelligente
      alfred summarize <projet>                   Résumé du projet
      alfred suggest                              Suggestions transversales

      alfred remind <projet> <texte> in <durée>  Programmer un rappel
      alfred remind list                         Lister les rappels
      alfred remind done <id>                    Accomplir un rappel
      alfred remind delete <id>                  Supprimer un rappel
      alfred health                              Diagnostic des organes

      alfred help                                Cette aide
    """)
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
      IO.puts("  ── #{project} ──")
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
      IO.puts("  ── #{project} ──")
      print_notes(project_notes)
      IO.puts("")
    end)
  end
end
