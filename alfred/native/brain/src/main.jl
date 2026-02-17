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
# Extract Facts — Extract facts from conversation messages
# ============================================================

function cmd_extract_facts(input)
    messages = get(input, "messages", [])

    if isempty(messages)
        respond(Dict{String,Any}("facts" => Any[]))
        return
    end

    facts = Dict{String,Any}[]

    # Analyze user messages for patterns
    user_texts = String[]
    for msg in messages
        if get(msg, "role", "") == "user"
            push!(user_texts, get(msg, "content", ""))
        end
    end

    all_user_text = join(user_texts, " ")

    # Extract keywords as topics
    keywords = extract_keywords(all_user_text, top_n=5)
    if !isempty(keywords)
        push!(facts, Dict{String,Any}(
            "category" => "knowledge",
            "subject" => "topics",
            "content" => "Sujets abordés : " * join(keywords, ", "),
            "confidence" => 0.6
        ))
    end

    # Detect preferences (je préfère, j'aime, etc.)
    for text in user_texts
        lt = lowercase(text)
        if occursin(r"je (préfère|prefere|aime|adore|déteste|deteste)", lt)
            push!(facts, Dict{String,Any}(
                "category" => "preferences",
                "subject" => "preference",
                "content" => length(text) > 200 ? text[1:200] : text,
                "confidence" => 0.7
            ))
        end
        if occursin(r"je (suis|travaille|fais|habite)", lt)
            push!(facts, Dict{String,Any}(
                "category" => "personal_info",
                "subject" => "info",
                "content" => length(text) > 200 ? text[1:200] : text,
                "confidence" => 0.6
            ))
        end
    end

    respond(Dict{String,Any}("facts" => facts))
end

# ============================================================
# Summarize Episode — Create a concise episode summary
# ============================================================

function cmd_summarize_episode(input)
    messages = get(input, "messages", [])

    if isempty(messages)
        respond(Dict{String,Any}("summary" => "Conversation vide."))
        return
    end

    # Count messages by role
    user_count = count(m -> get(m, "role", "") == "user", messages)
    assistant_count = count(m -> get(m, "role", "") == "assistant", messages)

    # Extract topics from user messages
    user_texts = [get(m, "content", "") for m in messages if get(m, "role", "") == "user"]
    all_text = join(user_texts, " ")
    keywords = extract_keywords(all_text, top_n=5)

    topic_str = isempty(keywords) ? "divers" : join(keywords, ", ")
    summary = "Échange de $(user_count + assistant_count) messages. Sujets : $(topic_str)."

    respond(Dict{String,Any}("summary" => summary, "topics" => keywords))
end

# ============================================================
# Detect Patterns — Analyze episodes for behavioral patterns
# ============================================================

function cmd_detect_patterns(input)
    episodes = get(input, "episodes", [])
    patterns = Dict{String,Any}[]

    if length(episodes) < 3
        respond(Dict{String,Any}("patterns" => patterns))
        return
    end

    # Analyze conversation frequency
    # Detect recurring topics across episodes
    all_topics = String[]
    message_counts = Int[]

    for ep in episodes
        topics = get(ep, "topics", [])
        for t in topics
            push!(all_topics, string(t))
        end
        push!(message_counts, get(ep, "message_count", 0))
    end

    if !isempty(all_topics)
        topic_freq = Dict{String,Int}()
        for t in all_topics
            topic_freq[t] = get(topic_freq, t, 0) + 1
        end
        sorted = sort(collect(topic_freq), by=x -> -x[2])

        # Topics appearing in > 30% of episodes
        threshold = max(2, round(Int, length(episodes) * 0.3))
        recurring = filter(x -> x[2] >= threshold, sorted)

        if !isempty(recurring)
            topics_str = join([p[1] for p in recurring[1:min(3, length(recurring))]], ", ")
            push!(patterns, Dict{String,Any}(
                "pattern_type" => "recurring_topics",
                "description" => "Monsieur aborde régulièrement : $(topics_str).",
                "confidence" => min(1.0, recurring[1][2] / length(episodes))
            ))
        end
    end

    # Analyze conversation length patterns
    if !isempty(message_counts)
        avg_msgs = round(mean(message_counts), digits=1)
        if avg_msgs > 10
            push!(patterns, Dict{String,Any}(
                "pattern_type" => "behavioral",
                "description" => "Monsieur engage des conversations approfondies ($(avg_msgs) messages en moyenne).",
                "confidence" => 0.7
            ))
        elseif avg_msgs < 4
            push!(patterns, Dict{String,Any}(
                "pattern_type" => "behavioral",
                "description" => "Monsieur préfère des échanges courts et efficaces.",
                "confidence" => 0.7
            ))
        end
    end

    respond(Dict{String,Any}("patterns" => patterns))
end

# ============================================================
# Briefing — Synthèse quotidienne intelligente
# ============================================================

function cmd_briefing(input)
    projects = get(input, "projects", [])
    reminders = get(input, "reminders", [])
    culture = get(input, "culture", [])
    patterns = get(input, "patterns", [])
    last_episode = get(input, "last_episode", nothing)
    now_ts = get(input, "now", round(Int, time()))

    sections = Dict{String,Any}[]

    # -- 1. Tâches urgentes et en retard --
    urgent_items = String[]
    overdue_items = String[]
    total_pending = 0

    for proj in projects
        name = get(proj, "name", "?")
        tasks = get(proj, "tasks", [])
        pending = filter(t -> t["status"] == "pending", tasks)
        total_pending += length(pending)

        # High priority tasks
        for t in pending
            prio = get(t, "priority", 1)
            if prio >= 4
                push!(urgent_items, "$(t["description"]) [$(name), P$(prio)]")
            end
        end

        # Stale tasks (> 7 days)
        for t in pending
            created = get(t, "created_at", "")
            if created != ""
                try
                    dt = DateTime(created[1:min(19, length(created))])
                    age_days = (now_ts - round(Int, datetime2unix(dt))) / 86400
                    if age_days > 7
                        push!(overdue_items, "$(t["description"]) [$(name), $(round(Int, age_days))j]")
                    end
                catch
                end
            end
        end
    end

    tasks_lines = String[]
    if !isempty(urgent_items)
        push!(tasks_lines, "Priorités hautes :")
        for item in urgent_items[1:min(5, length(urgent_items))]
            push!(tasks_lines, "  ⚡ $item")
        end
    end
    if !isempty(overdue_items)
        push!(tasks_lines, "En attente depuis longtemps :")
        for item in overdue_items[1:min(5, length(overdue_items))]
            push!(tasks_lines, "  ⏳ $item")
        end
    end
    if total_pending > 0
        push!(tasks_lines, "Total : $(total_pending) tâche(s) en attente sur $(length(projects)) projet(s).")
    elseif !isempty(projects)
        push!(tasks_lines, "Toutes vos tâches sont accomplies, Monsieur.")
    end

    if !isempty(tasks_lines)
        push!(sections, Dict{String,Any}(
            "title" => "Tâches",
            "icon" => "📋",
            "lines" => tasks_lines
        ))
    end

    # -- 2. Rappels du jour --
    reminder_lines = String[]
    due_today = []
    overdue_reminders = []

    for r in reminders
        status = get(r, "status", "pending")
        if status == "pending" || status == "active"
            due_at = get(r, "due_at", 0)
            if due_at <= now_ts
                push!(overdue_reminders, r)
            elseif due_at <= now_ts + 86400  # within next 24h
                push!(due_today, r)
            end
        end
    end

    if !isempty(overdue_reminders)
        push!(reminder_lines, "En retard :")
        for r in overdue_reminders
            push!(reminder_lines, "  🔴 $(get(r, "text", "?")) [$(get(r, "project", "?"))]")
        end
    end
    if !isempty(due_today)
        push!(reminder_lines, "Aujourd'hui :")
        for r in due_today
            push!(reminder_lines, "  🟡 $(get(r, "text", "?")) [$(get(r, "project", "?"))]")
        end
    end
    if isempty(overdue_reminders) && isempty(due_today)
        push!(reminder_lines, "Aucun rappel urgent.")
    end

    push!(sections, Dict{String,Any}(
        "title" => "Rappels",
        "icon" => "🔔",
        "lines" => reminder_lines
    ))

    # -- 3. Culture récente --
    culture_lines = String[]
    if !isempty(culture)
        # Group by topic
        topics = Dict{String,Int}()
        recent = []
        for entry in culture
            topic = get(entry, "topic", "divers")
            topics[topic] = get(topics, topic, 0) + 1
            # Check if learned recently (last 7 days)
            learned_at = get(entry, "learned_at", "")
            if learned_at != ""
                try
                    dt = DateTime(learned_at[1:min(19, length(learned_at))])
                    age_days = (now_ts - round(Int, datetime2unix(dt))) / 86400
                    if age_days <= 7
                        push!(recent, entry)
                    end
                catch
                end
            end
        end

        push!(culture_lines, "$(length(culture)) connaissances en $(length(topics)) sujet(s).")

        if !isempty(recent)
            push!(culture_lines, "Récemment acquises :")
            for entry in recent[1:min(3, length(recent))]
                source_name = ""
                src = get(entry, "source", nothing)
                if src !== nothing
                    source_name = get(src, "name", get(src, "type", ""))
                end
                suffix = source_name != "" ? " (via $source_name)" : ""
                push!(culture_lines, "  📚 [$(get(entry, "topic", "?"))] $(get(entry, "content", ""))$suffix")
            end
        end

        # Top topics
        if length(topics) > 1
            sorted_topics = sort(collect(topics), by=x -> -x[2])
            top = sorted_topics[1:min(3, length(sorted_topics))]
            top_str = join(["$(t[1]) ($(t[2]))" for t in top], ", ")
            push!(culture_lines, "Sujets principaux : $top_str")
        end
    else
        push!(culture_lines, "Culture encore vierge. Utilisez 'alfred culture learn' pour m'enseigner.")
    end

    push!(sections, Dict{String,Any}(
        "title" => "Culture",
        "icon" => "📖",
        "lines" => culture_lines
    ))

    # -- 4. Patterns et habitudes --
    pattern_lines = String[]
    if !isempty(patterns)
        for p in patterns[1:min(3, length(patterns))]
            desc = get(p, "description", "")
            if desc != ""
                push!(pattern_lines, "  🔄 $desc")
            end
        end
    end
    if isempty(pattern_lines)
        push!(pattern_lines, "Pas encore assez de données pour détecter des habitudes.")
    end

    push!(sections, Dict{String,Any}(
        "title" => "Habitudes",
        "icon" => "🧠",
        "lines" => pattern_lines
    ))

    # -- 5. Dernière conversation --
    episode_lines = String[]
    if last_episode !== nothing
        summary = get(last_episode, "summary", nothing)
        msg_count = get(last_episode, "message_count", 0)
        mode = get(last_episode, "mode", "chat")

        if summary !== nothing && summary != ""
            push!(episode_lines, summary)
        elseif msg_count > 0
            push!(episode_lines, "Dernier échange : $(msg_count) messages (mode $(mode)).")
        end
    end
    if isempty(episode_lines)
        push!(episode_lines, "Pas de conversation récente.")
    end

    push!(sections, Dict{String,Any}(
        "title" => "Dernière conversation",
        "icon" => "💬",
        "lines" => episode_lines
    ))

    # -- Mot de conclusion --
    conclusion = ""
    if total_pending == 0 && isempty(overdue_reminders)
        conclusion = "Votre agenda est dégagé, Monsieur. Belle journée en perspective."
    elseif !isempty(overdue_reminders) || length(urgent_items) > 2
        conclusion = "Journée chargée en perspective, Monsieur. Je vous recommande de traiter les urgences en priorité."
    else
        conclusion = "Bonne journée, Monsieur."
    end

    respond(Dict{String,Any}(
        "sections" => sections,
        "conclusion" => conclusion,
        "total_pending" => total_pending,
        "urgent_count" => length(urgent_items),
        "overdue_reminders" => length(overdue_reminders)
    ))
end

# ============================================================
# Analyze Culture — Intelligence culturelle
# ============================================================

function cmd_analyze_culture(input)
    culture = get(input, "culture", [])
    now_ts = get(input, "now", round(Int, time()))

    insights = String[]
    stats = Dict{String,Any}()

    if isempty(culture)
        push!(insights, "Votre culture est encore vierge, Monsieur. Utilisez 'alfred culture learn' pour m'enseigner.")
        stats["total"] = 0
        respond(Dict{String,Any}("insights" => insights, "stats" => stats, "topics" => Dict{String,Any}[], "suggestions" => String[]))
        return
    end

    total = length(culture)
    stats["total"] = total

    # -- Topic distribution --
    topic_counts = Dict{String,Int}()
    topic_entries = Dict{String,Vector{Any}}()
    for entry in culture
        topic = get(entry, "topic", "divers")
        topic_counts[topic] = get(topic_counts, topic, 0) + 1
        if !haskey(topic_entries, topic)
            topic_entries[topic] = []
        end
        push!(topic_entries[topic], entry)
    end

    stats["topic_count"] = length(topic_counts)
    sorted_topics = sort(collect(topic_counts), by=x -> -x[2])

    topics_data = Dict{String,Any}[]
    for (topic, count) in sorted_topics
        td = Dict{String,Any}("name" => topic, "count" => count)

        # Sources for this topic
        entries = topic_entries[topic]
        sources = Dict{String,Int}()
        for e in entries
            src = get(e, "source", nothing)
            if src !== nothing
                sname = get(src, "name", get(src, "type", "inconnu"))
                sources[sname] = get(sources, sname, 0) + 1
            end
        end
        if !isempty(sources)
            td["sources"] = sources
        end

        # Tags for this topic
        all_tags = String[]
        for e in entries
            tags = get(e, "tags", [])
            for t in tags
                push!(all_tags, string(t))
            end
        end
        if !isempty(all_tags)
            tag_freq = Dict{String,Int}()
            for t in all_tags
                tag_freq[t] = get(tag_freq, t, 0) + 1
            end
            td["top_tags"] = [p[1] for p in sort(collect(tag_freq), by=x -> -x[2])[1:min(5, length(tag_freq))]]
        end

        push!(topics_data, td)
    end

    # -- Source attribution --
    source_counts = Dict{String,Int}()
    source_types = Dict{String,Int}()
    for entry in culture
        src = get(entry, "source", nothing)
        if src !== nothing
            stype = get(src, "type", "other")
            source_types[stype] = get(source_types, stype, 0) + 1
            sname = get(src, "name", "")
            if sname != ""
                source_counts[sname] = get(source_counts, sname, 0) + 1
            end
        end
    end

    stats["source_types"] = source_types
    if !isempty(source_counts)
        sorted_sources = sort(collect(source_counts), by=x -> -x[2])
        top_sources = sorted_sources[1:min(5, length(sorted_sources))]
        stats["top_sources"] = [Dict("name" => s[1], "count" => s[2]) for s in top_sources]
    end

    # -- Tag co-occurrence (connections between topics) --
    tag_topics = Dict{String,Set{String}}()
    for entry in culture
        topic = get(entry, "topic", "divers")
        tags = get(entry, "tags", [])
        for t in tags
            ts = string(t)
            if !haskey(tag_topics, ts)
                tag_topics[ts] = Set{String}()
            end
            push!(tag_topics[ts], topic)
        end
    end

    connections = String[]
    for (tag, topics_set) in tag_topics
        if length(topics_set) > 1
            topics_list = sort(collect(topics_set))
            push!(connections, "Le tag \"$(tag)\" relie : $(join(topics_list, ", "))")
        end
    end
    stats["connections"] = connections

    # -- Temporal analysis --
    recent_count = 0
    old_count = 0
    for entry in culture
        learned_at = get(entry, "learned_at", "")
        if learned_at != ""
            try
                dt = DateTime(learned_at[1:min(19, length(learned_at))])
                age_days = (now_ts - round(Int, datetime2unix(dt))) / 86400
                if age_days <= 7
                    recent_count += 1
                elseif age_days > 30
                    old_count += 1
                end
            catch
            end
        end
    end
    stats["recent_7d"] = recent_count
    stats["older_30d"] = old_count

    # -- Generate insights --
    push!(insights, "$(total) connaissances réparties sur $(length(topic_counts)) sujet(s).")

    if length(sorted_topics) > 0
        top_topic = sorted_topics[1]
        push!(insights, "Sujet le plus riche : \"$(top_topic[1])\" ($(top_topic[2]) entrée(s)).")
    end

    if length(sorted_topics) > 1
        smallest = sorted_topics[end]
        if smallest[2] == 1
            push!(insights, "Sujet à développer : \"$(smallest[1])\" (1 seule entrée).")
        end
    end

    if !isempty(source_counts)
        top_src = sort(collect(source_counts), by=x -> -x[2])[1]
        push!(insights, "Source la plus fréquente : $(top_src[1]) ($(top_src[2]) contributions).")
    end

    person_count = get(source_types, "person", 0)
    book_count = get(source_types, "book", 0)
    observation_count = get(source_types, "observation", 0)
    if person_count > book_count + observation_count
        push!(insights, "Votre culture provient majoritairement de personnes. Pensez à diversifier vos sources.")
    end

    if !isempty(connections)
        push!(insights, "$(length(connections)) connexion(s) thématique(s) détectée(s) via les tags.")
    end

    if recent_count > 0
        push!(insights, "$(recent_count) connaissance(s) acquise(s) cette semaine. Bonne dynamique !")
    elseif total > 5
        push!(insights, "Aucune connaissance récente. N'hésitez pas à m'enseigner de nouvelles choses.")
    end

    # -- Suggestions --
    suggestions = String[]

    # Suggest topics to explore based on gaps
    if length(sorted_topics) >= 2
        avg_count = total / length(sorted_topics)
        for (topic, count) in sorted_topics
            if count < avg_count * 0.5 && count <= 2
                push!(suggestions, "Approfondir le sujet \"$(topic)\" (seulement $(count) entrée(s)).")
            end
        end
    end

    # Suggest source diversification
    if length(source_types) == 1
        only_type = first(keys(source_types))
        push!(suggestions, "Diversifiez vos sources — tout provient de type \"$(only_type)\".")
    end

    # Suggest tag usage
    entries_without_tags = count(e -> isempty(get(e, "tags", [])), culture)
    if entries_without_tags > total * 0.5
        push!(suggestions, "Ajoutez des tags à vos connaissances pour mieux les relier entre elles.")
    end

    if isempty(suggestions)
        push!(suggestions, "Votre culture est bien organisée, Monsieur. Continuez ainsi.")
    end

    respond(Dict{String,Any}(
        "insights" => insights,
        "stats" => stats,
        "topics" => topics_data,
        "suggestions" => suggestions
    ))
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
            elseif cmd == "extract_facts"
                cmd_extract_facts(input)
            elseif cmd == "summarize_episode"
                cmd_summarize_episode(input)
            elseif cmd == "detect_patterns"
                cmd_detect_patterns(input)
            elseif cmd == "briefing"
                cmd_briefing(input)
            elseif cmd == "analyze_culture"
                cmd_analyze_culture(input)
            else
                respond_error("Unknown command: $cmd")
            end
        catch e
            respond_error("Error: $(sprint(showerror, e))")
        end
    end
end

main()
