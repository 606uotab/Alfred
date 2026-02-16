#!/usr/bin/env julia
#=
Alfred Brain — Le Cerveau (Julia)
Moteur d'analyse et de réflexion.
Lit des commandes JSON sur stdin, écrit des réponses JSON sur stdout.
=#

using JSON3
using Statistics
using Dates

# ============================================================
# Protocol — JSON lines on stdin/stdout
# ============================================================

function respond(data::Dict)
    data["status"] = "ok"
    println(stdout, JSON3.write(data))
    flush(stdout)
end

function respond_error(msg::String)
    println(stdout, JSON3.write(Dict("status" => "error", "message" => msg)))
    flush(stdout)
end

# ============================================================
# Analyze — Deep analysis of a single project
# ============================================================

function cmd_analyze(input)
    project = input["project"]
    tasks = get(project, "tasks", [])
    notes = get(project, "notes", [])
    reminders = get(project, "reminders", [])
    now = get(input, "now", round(Int, time()))

    insights = String[]
    stats = Dict{String,Any}()

    # -- Task statistics --
    total_tasks = length(tasks)
    if total_tasks > 0
        pending = filter(t -> t["status"] == "pending", tasks)
        done = filter(t -> t["status"] == "done", tasks)
        n_pending = length(pending)
        n_done = length(done)
        completion_rate = round(n_done / total_tasks * 100, digits=1)

        stats["total_tasks"] = total_tasks
        stats["pending"] = n_pending
        stats["done"] = n_done
        stats["completion_rate"] = completion_rate

        push!(insights, "Taux d'accomplissement : $(completion_rate)% ($(n_done)/$(total_tasks))")

        if completion_rate >= 80
            push!(insights, "Excellent progrès sur ce projet, Monsieur.")
        elseif completion_rate >= 50
            push!(insights, "Le projet avance à bon rythme.")
        elseif completion_rate < 20 && total_tasks > 3
            push!(insights, "Ce projet nécessite votre attention — peu de tâches accomplies.")
        end

        # Priority distribution
        priorities = [get(t, "priority", 1) for t in pending]
        if !isempty(priorities)
            high_priority = count(p -> p >= 4, priorities)
            if high_priority > 0
                push!(insights, "$(high_priority) tâche(s) en haute priorité (P4-P5) à traiter.")
            end
            stats["avg_priority"] = round(mean(priorities), digits=1)
        end

        # Task age analysis
        if !isempty(pending)
            ages_days = Float64[]
            for t in pending
                created = get(t, "created_at", "")
                if created != ""
                    try
                        dt = DateTime(created[1:min(19, length(created))])
                        age = (now - round(Int, datetime2unix(dt))) / 86400
                        push!(ages_days, age)
                    catch
                    end
                end
            end
            if !isempty(ages_days)
                oldest = round(maximum(ages_days), digits=1)
                avg_age = round(mean(ages_days), digits=1)
                stats["oldest_task_days"] = oldest
                stats["avg_task_age_days"] = avg_age

                if oldest > 30
                    push!(insights, "Attention : la tâche la plus ancienne date de $(round(Int, oldest)) jours.")
                elseif oldest > 7
                    push!(insights, "Certaines tâches attendent depuis $(round(Int, oldest)) jours.")
                end
            end
        end

        # Velocity estimate
        if n_done > 0 && !isempty(pending)
            push!(insights, "Au rythme actuel, il reste environ $(n_pending) tâches à accomplir.")
        end
    else
        push!(insights, "Aucune tâche sur ce projet. C'est le moment d'en créer.")
        stats["total_tasks"] = 0
    end

    # -- Notes analysis --
    total_notes = length(notes)
    stats["total_notes"] = total_notes

    if total_notes > 0
        # Text analysis - word frequency
        all_text = join([get(n, "text", "") for n in notes], " ")
        keywords = extract_keywords(all_text)
        if !isempty(keywords)
            top_kw = join(keywords[1:min(5, length(keywords))], ", ")
            push!(insights, "Thèmes récurrents dans vos notes : $(top_kw).")
            stats["keywords"] = keywords[1:min(10, length(keywords))]
        end

        # Note lengths
        lengths = [length(get(n, "text", "")) for n in notes]
        stats["avg_note_length"] = round(mean(lengths), digits=0)

        if total_notes > 5
            push!(insights, "$(total_notes) notes accumulées — envisagez une synthèse.")
        end
    end

    # -- Reminders analysis --
    if !isempty(reminders)
        pending_reminders = filter(r -> get(r, "status", "pending") == "pending" || get(r, "status", :pending) == :pending, reminders)
        overdue = filter(r -> get(r, "due_at", now + 1) <= now, pending_reminders)
        if !isempty(overdue)
            push!(insights, "$(length(overdue)) rappel(s) en retard sur ce projet.")
        end
        stats["pending_reminders"] = length(pending_reminders)
    end

    respond(Dict{String,Any}("insights" => insights, "stats" => stats))
end

# ============================================================
# Summarize — Concise summary of a project
# ============================================================

function cmd_summarize(input)
    project = input["project"]
    name = get(project, "name", "Projet")
    tasks = get(project, "tasks", [])
    notes = get(project, "notes", [])

    pending = filter(t -> t["status"] == "pending", tasks)
    done = filter(t -> t["status"] == "done", tasks)

    parts = String[]

    push!(parts, "Projet \"$(name)\" : $(length(tasks)) tâches ($(length(done)) accomplies, $(length(pending)) en attente).")

    # High priority pending
    high = filter(t -> get(t, "priority", 1) >= 4, pending)
    if !isempty(high)
        descs = [t["description"] for t in high]
        push!(parts, "Priorités hautes : " * join(descs, ", ") * ".")
    end

    # Recent notes summary
    if !isempty(notes)
        recent = notes[max(1, length(notes)-2):end]
        for n in recent
            text = get(n, "text", "")
            if length(text) > 80
                text = text[1:77] * "..."
            end
            push!(parts, "Note : \"$(text)\"")
        end
    end

    if isempty(tasks) && isempty(notes)
        push!(parts, "Ce projet est vierge — tout reste à construire.")
    end

    summary = join(parts, "\n")
    respond(Dict{String,Any}("summary" => summary))
end

# ============================================================
# Suggest — Cross-project intelligent suggestions
# ============================================================

function cmd_suggest(input)
    projects = get(input, "projects", [])
    now = get(input, "now", round(Int, time()))

    suggestions = String[]

    if isempty(projects)
        push!(suggestions, "Vous n'avez aucun projet. Souhaitez-vous en créer un ?")
        respond(Dict{String,Any}("suggestions" => suggestions))
        return
    end

    total_pending = 0
    stale_projects = String[]
    heavy_projects = String[]
    empty_projects = String[]

    for proj in projects
        name = get(proj, "name", "?")
        tasks = get(proj, "tasks", [])
        notes = get(proj, "notes", [])
        pending = filter(t -> t["status"] == "pending", tasks)
        n_pending = length(pending)
        total_pending += n_pending

        if isempty(tasks) && isempty(notes)
            push!(empty_projects, name)
            continue
        end

        # Check for stale tasks (older than 7 days)
        for t in pending
            created = get(t, "created_at", "")
            if created != ""
                try
                    dt = DateTime(created[1:min(19, length(created))])
                    age_days = (now - round(Int, datetime2unix(dt))) / 86400
                    if age_days > 14
                        push!(suggestions, "La tâche \"$(t["description"])\" du projet \"$(name)\" attend depuis $(round(Int, age_days)) jours.")
                        break  # One per project max
                    end
                catch
                end
            end
        end

        if n_pending > 5
            push!(heavy_projects, name)
        end
    end

    # Workload overview
    if total_pending > 10
        push!(suggestions, "Vous avez $(total_pending) tâches en attente au total. Priorisez les plus importantes.")
    elseif total_pending == 0
        push!(suggestions, "Toutes vos tâches sont accomplies. Félicitations, Monsieur.")
    end

    if !isempty(heavy_projects)
        push!(suggestions, "Projet(s) chargé(s) : " * join(heavy_projects, ", ") * ". Envisagez de déléguer ou découper.")
    end

    if !isempty(empty_projects)
        push!(suggestions, "Projet(s) vide(s) : " * join(empty_projects, ", ") * ". À alimenter ou supprimer.")
    end

    if length(projects) > 1
        # Balance check
        counts = [length(filter(t -> t["status"] == "pending", get(p, "tasks", []))) for p in projects]
        if !isempty(counts) && maximum(counts) > 3 * max(1, minimum(counts))
            push!(suggestions, "La charge est déséquilibrée entre vos projets.")
        end
    end

    if isempty(suggestions)
        push!(suggestions, "Tout semble en ordre, Monsieur. Continuez ainsi.")
    end

    respond(Dict{String,Any}("suggestions" => suggestions))
end

# ============================================================
# Text Analysis Helpers
# ============================================================

const STOP_WORDS = Set([
    "le", "la", "les", "un", "une", "des", "de", "du", "au", "aux",
    "et", "ou", "mais", "donc", "car", "ni", "que", "qui", "quoi",
    "ce", "cette", "ces", "son", "sa", "ses", "mon", "ma", "mes",
    "en", "dans", "sur", "sous", "par", "pour", "avec", "sans",
    "ne", "pas", "plus", "il", "elle", "on", "nous", "vous", "ils",
    "est", "sont", "être", "avoir", "fait", "faire", "dit", "dire",
    "the", "a", "an", "is", "are", "was", "were", "to", "of", "in",
    "and", "or", "but", "for", "with", "not", "this", "that", "it",
    "je", "tu", "se", "si", "tout", "très", "bien", "aussi", "comme"
])

function extract_keywords(text::String; min_length=3, top_n=10)
    # Normalize
    text = lowercase(text)
    # Remove punctuation
    text = replace(text, r"[^\w\sàâäéèêëïîôùûüÿçœæ]" => " ")
    # Split into words
    words = split(text)
    # Filter
    words = filter(w -> length(w) >= min_length && !(w in STOP_WORDS), words)

    if isempty(words)
        return String[]
    end

    # Count frequencies
    freq = Dict{String,Int}()
    for w in words
        freq[w] = get(freq, w, 0) + 1
    end

    # Sort by frequency
    sorted = sort(collect(freq), by=x -> -x[2])
    return [p[1] for p in sorted[1:min(top_n, length(sorted))]]
end

# ============================================================
# Main Loop
# ============================================================

function main()
    for line in eachline(stdin)
        line = strip(line)
        isempty(line) && continue

        try
            input = JSON3.read(line, Dict{String,Any})
            cmd = get(input, "cmd", "")

            if cmd == "analyze"
                cmd_analyze(input)
            elseif cmd == "summarize"
                cmd_summarize(input)
            elseif cmd == "suggest"
                cmd_suggest(input)
            else
                respond_error("Unknown command: $cmd")
            end
        catch e
            respond_error("Error: $(sprint(showerror, e))")
        end
    end
end

main()
