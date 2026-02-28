#!/usr/bin/env julia
#=
Alfred Brain — Le Cerveau (Julia)
Moteur d'analyse et de réflexion.
Lit des commandes JSON sur stdin, écrit des réponses JSON sur stdout.
=#

using JSON3
using Statistics
using Dates
using LinearAlgebra

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

function tokenize(text::AbstractString; min_length=3)
    lt = lowercase(string(text))
    lt = replace(lt, r"[^\w\sàâäéèêëïîôùûüÿçœæ]" => " ")
    words = split(lt)
    return [string(w) for w in words if length(w) >= min_length && !(string(w) in STOP_WORDS)]
end

function extract_keywords(text::AbstractString; min_length=3, top_n=10)
    words = tokenize(string(text), min_length=min_length)
    isempty(words) && return String[]

    freq = Dict{String,Int}()
    for w in words
        freq[w] = get(freq, w, 0) + 1
    end

    sorted = sort(collect(freq), by=x -> -x[2])
    return [p[1] for p in sorted[1:min(top_n, length(sorted))]]
end

# ============================================================
# TF-IDF Engine — Infrastructure sémantique partagée
# ============================================================

struct TFIDFModel
    matrix::Matrix{Float64}
    vocabulary::Vector{String}
    idf::Vector{Float64}
    doc_norms::Vector{Float64}
end

function build_tfidf(documents::Vector{String}; min_df=1, max_df_ratio=0.95)
    n_docs = length(documents)
    n_docs == 0 && return TFIDFModel(zeros(0,0), String[], Float64[], Float64[])

    tokenized = [tokenize(doc) for doc in documents]

    # Document frequency
    df = Dict{String,Int}()
    for tokens in tokenized
        for w in Set(tokens)
            df[w] = get(df, w, 0) + 1
        end
    end

    max_df = round(Int, n_docs * max_df_ratio)
    vocab = sort([w for (w, count) in df if count >= min_df && count <= max_df])
    n_vocab = length(vocab)

    n_vocab == 0 && return TFIDFModel(zeros(n_docs, 0), String[], Float64[], zeros(n_docs))

    word_to_idx = Dict(w => i for (i, w) in enumerate(vocab))

    # TF matrix
    tf_matrix = zeros(Float64, n_docs, n_vocab)
    for (d, tokens) in enumerate(tokenized)
        n_tokens = length(tokens)
        n_tokens == 0 && continue
        for w in tokens
            idx = get(word_to_idx, w, 0)
            if idx > 0
                tf_matrix[d, idx] += 1.0
            end
        end
        tf_matrix[d, :] ./= n_tokens
    end

    # IDF
    idf_vec = Float64[log(n_docs / max(1, df[w])) for w in vocab]

    # TF-IDF
    tfidf_matrix = tf_matrix .* idf_vec'

    # L2 norms
    doc_norms = Float64[norm(tfidf_matrix[d, :]) for d in 1:n_docs]

    return TFIDFModel(tfidf_matrix, vocab, idf_vec, doc_norms)
end

function query_vector(model::TFIDFModel, query::AbstractString)
    tokens = tokenize(string(query))
    vec = zeros(Float64, length(model.vocabulary))
    isempty(tokens) && return vec

    word_to_idx = Dict(w => i for (i, w) in enumerate(model.vocabulary))
    for w in tokens
        idx = get(word_to_idx, w, 0)
        if idx > 0
            vec[idx] += 1.0
        end
    end
    vec ./= length(tokens)
    vec .*= model.idf
    return vec
end

function cosine_sim(v1::Vector{Float64}, v2::Vector{Float64}, n1::Float64, n2::Float64)
    (n1 == 0.0 || n2 == 0.0) && return 0.0
    return dot(v1, v2) / (n1 * n2)
end

# ============================================================
# K-Means Clustering
# ============================================================

function kmeans_cluster(data::Matrix{Float64}, k::Int; max_iter=100)
    n, d = size(data)
    k = min(k, n)
    k <= 0 && return ones(Int, n)

    # K-means++ init
    centroids = zeros(Float64, k, d)
    idx = rand(1:n)
    centroids[1, :] = data[idx, :]

    for c in 2:k
        dists = Float64[minimum(sum((data[i, :] .- centroids[j, :]).^2) for j in 1:c-1) for i in 1:n]
        total = sum(dists)
        if total == 0.0
            centroids[c, :] = data[rand(1:n), :]
        else
            r = rand() * total
            cum = 0.0
            for i in 1:n
                cum += dists[i]
                if cum >= r
                    centroids[c, :] = data[i, :]
                    break
                end
            end
        end
    end

    assignments = zeros(Int, n)
    for iter in 1:max_iter
        new_assign = zeros(Int, n)
        for i in 1:n
            best_c, best_d = 1, Inf
            for c in 1:k
                d_sq = sum((data[i, :] .- centroids[c, :]).^2)
                if d_sq < best_d
                    best_d = d_sq
                    best_c = c
                end
            end
            new_assign[i] = best_c
        end

        new_assign == assignments && break
        assignments = new_assign

        for c in 1:k
            members = findall(x -> x == c, assignments)
            if !isempty(members)
                centroids[c, :] = mean(data[members, :], dims=1)[1, :]
            end
        end
    end
    return assignments
end

function auto_k(n::Int)
    return clamp(round(Int, sqrt(n / 2)), 2, min(10, n))
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
# Extract Culture — Extraction de connaissances depuis les conversations
# ============================================================

function cmd_extract_culture(input)
    messages = get(input, "messages", [])

    if isempty(messages)
        respond(Dict{String,Any}("candidates" => Any[]))
        return
    end

    candidates = Dict{String,Any}[]

    # Collect user messages
    user_texts = String[]
    for msg in messages
        if get(msg, "role", "") == "user"
            push!(user_texts, get(msg, "content", ""))
        end
    end

    # Also collect assistant messages (they contain reformulated knowledge)
    assistant_texts = String[]
    for msg in messages
        if get(msg, "role", "") == "assistant"
            push!(assistant_texts, get(msg, "content", ""))
        end
    end

    all_text = join(vcat(user_texts, assistant_texts), "\n")

    # Pattern 1: Factual statements with "c'est", "est un", "sont des"
    factual_patterns = [
        r"([\w\s]+)\s+(?:c'est|est un|est une|sont des|consiste à)\s+(.{10,120})"i,
        r"(?:saviez.vous que|en fait|il paraît que|apparemment)\s+(.{15,150})"i,
        r"(?:on m'a dit que|j'ai appris que|il faut savoir que)\s+(.{15,150})"i
    ]

    for text in user_texts
        for pattern in factual_patterns
            for m in eachmatch(pattern, text)
                content = strip(m.match)
                if length(content) > 15 && length(content) < 300
                    # Try to infer topic from keywords
                    topic = infer_topic(content)
                    push!(candidates, Dict{String,Any}(
                        "content" => content,
                        "topic" => topic,
                        "source_type" => "conversation",
                        "confidence" => 0.5
                    ))
                end
            end
        end
    end

    # Pattern 2: Teaching statements (user explicitly teaching Alfred)
    teach_patterns = [
        r"(?:retiens que|souviens.toi que|note que|sache que)\s+(.{10,200})"i,
        r"(?:pour ta culture|pour info|à savoir)\s*[:,]?\s*(.{10,200})"i
    ]

    for text in user_texts
        for pattern in teach_patterns
            for m in eachmatch(pattern, text)
                content = strip(m.captures[1])
                if length(content) > 10
                    topic = infer_topic(content)
                    push!(candidates, Dict{String,Any}(
                        "content" => content,
                        "topic" => topic,
                        "source_type" => "person",
                        "confidence" => 0.8
                    ))
                end
            end
        end
    end

    # Pattern 3: Definitions or explanations in assistant responses
    explain_patterns = [
        r"(?:en résumé|pour résumer|concrètement)\s*[:,]?\s*(.{20,200})"i
    ]

    for text in assistant_texts
        for pattern in explain_patterns
            for m in eachmatch(pattern, text)
                content = strip(m.captures[1])
                if length(content) > 20
                    topic = infer_topic(content)
                    push!(candidates, Dict{String,Any}(
                        "content" => content,
                        "topic" => topic,
                        "source_type" => "conversation",
                        "confidence" => 0.4
                    ))
                end
            end
        end
    end

    # Deduplicate by content similarity
    unique_candidates = Dict{String,Any}[]
    seen_content = Set{String}()
    for c in candidates
        normalized = lowercase(strip(get(c, "content", "")))
        short = normalized[1:min(50, length(normalized))]
        if !(short in seen_content)
            push!(seen_content, short)
            push!(unique_candidates, c)
        end
    end

    # Limit to top 5 by confidence
    sort!(unique_candidates, by=c -> -get(c, "confidence", 0))
    result = unique_candidates[1:min(5, length(unique_candidates))]

    respond(Dict{String,Any}("candidates" => result))
end

function infer_topic(text::AbstractString)
    lt = lowercase(text)
    topic_hints = Dict(
        "cuisine" => ["recette", "cuisin", "manger", "plat", "ingrédient", "cuisson"],
        "botanique" => ["plante", "fleur", "jardin", "arros", "orchidée", "rose", "arbre"],
        "technologie" => ["code", "program", "logiciel", "ordinat", "serveur", "internet"],
        "histoire" => ["histoi", "siècle", "guerre", "révolution", "époque", "ancien"],
        "science" => ["scientif", "physique", "chimie", "biologie", "molécul", "atome"],
        "musique" => ["musiqu", "chans", "mélodie", "instrum", "concert", "note"],
        "santé" => ["santé", "médic", "maladie", "symptôm", "traitement", "médecin"],
        "sport" => ["sport", "match", "équipe", "joueur", "entraîn", "compétit"]
    )

    for (topic, words) in topic_hints
        for w in words
            if occursin(w, lt)
                return topic
            end
        end
    end

    return "divers"
end

# ============================================================
# Prioritize — Priorisation intelligente des tâches
# ============================================================

function cmd_prioritize(input)
    project = get(input, "project", Dict{String,Any}())
    now_ts = get(input, "now", round(Int, time()))
    name = get(project, "name", "?")
    tasks = get(project, "tasks", [])

    pending = filter(t -> get(t, "status", "") == "pending", tasks)

    if isempty(pending)
        respond(Dict{String,Any}(
            "ranked" => Any[],
            "insights" => ["Aucune tâche en attente sur \"$name\"."],
            "actions" => String[]
        ))
        return
    end

    # Score each task
    scored = []
    for t in pending
        desc = get(t, "description", "")
        priority = get(t, "priority", 1)
        created = get(t, "created_at", "")

        # Base score from priority (P5=50, P4=40, P3=30, P2=20, P1=10)
        score = priority * 10.0

        # Age bonus: older tasks get a boost (1 point per day, max 30)
        age_days = 0.0
        if created != ""
            try
                dt = DateTime(created[1:min(19, length(created))])
                age_days = (now_ts - round(Int, datetime2unix(dt))) / 86400
                score += min(30.0, age_days)
            catch
            end
        end

        # Keyword urgency detection
        ld = lowercase(desc)
        urgent_words = ["urgent", "critique", "asap", "immédiat", "bloqu", "bug", "fix", "erreur", "panne"]
        for w in urgent_words
            if occursin(w, ld)
                score += 15.0
                break
            end
        end

        # Short description bonus (likely quick wins)
        if length(desc) < 30
            score += 3.0
        end

        push!(scored, (task=t, score=score, age_days=age_days))
    end

    # Sort by score descending
    sort!(scored, by=x -> -x.score)

    # Build ranked list
    ranked = Dict{String,Any}[]
    insights = String[]
    actions = String[]

    for (i, item) in enumerate(scored)
        t = item.task
        entry = Dict{String,Any}(
            "rank" => i,
            "description" => get(t, "description", ""),
            "priority" => get(t, "priority", 1),
            "score" => round(item.score, digits=1),
            "age_days" => round(item.age_days, digits=0)
        )
        push!(ranked, entry)
    end

    # Generate insights
    top = scored[1]
    push!(insights, "Tâche recommandée en premier : \"$(get(top.task, "description", ""))\" (score $(round(top.score, digits=1))).")

    # Check if priorities don't match recommended order
    reorder_needed = false
    for (i, item) in enumerate(scored)
        if i <= 3 && get(item.task, "priority", 1) < 3
            reorder_needed = true
            break
        end
    end
    if reorder_needed
        push!(insights, "Certaines tâches urgentes ont une priorité basse — envisagez de les remonter.")
    end

    # Check for very old tasks
    old_tasks = filter(x -> x.age_days > 14, scored)
    if !isempty(old_tasks)
        push!(insights, "$(length(old_tasks)) tâche(s) en attente depuis plus de 2 semaines.")
    end

    # Quick wins
    quick_wins = filter(x -> length(get(x.task, "description", "")) < 30 && get(x.task, "priority", 1) <= 2, scored)
    if !isempty(quick_wins)
        push!(insights, "$(length(quick_wins)) tâche(s) rapide(s) à traiter pour libérer l'esprit.")
    end

    # Suggested actions
    if length(scored) > 5
        push!(actions, "Concentrez-vous sur les 3 premières tâches aujourd'hui.")
    end
    if !isempty(old_tasks)
        oldest_desc = get(old_tasks[1].task, "description", "")
        push!(actions, "Traitez ou reportez \"$oldest_desc\" — elle attend depuis $(round(Int, old_tasks[1].age_days)) jours.")
    end
    if isempty(actions)
        push!(actions, "Suivez l'ordre recommandé, Monsieur.")
    end

    respond(Dict{String,Any}(
        "ranked" => ranked,
        "insights" => insights,
        "actions" => actions
    ))
end

# ============================================================
# Semantic Search — Recherche TF-IDF + cosine similarity
# ============================================================

function cmd_search(input)
    query = get(input, "query", "")
    if isempty(strip(query))
        respond_error("Requete vide.")
        return
    end

    documents = String[]
    doc_meta = Dict{String,Any}[]

    for proj in get(input, "projects", [])
        name = get(proj, "name", "")
        push!(documents, name)
        push!(doc_meta, Dict{String,Any}("type" => "project", "title" => name, "excerpt" => "Projet"))
    end

    for t in get(input, "tasks", [])
        desc = get(t, "description", "")
        project = get(t, "project", "")
        push!(documents, "$desc $project")
        status = get(t, "status", "pending")
        icon = status == "done" ? "done" : "o"
        push!(doc_meta, Dict{String,Any}("type" => "task", "title" => desc,
            "excerpt" => "$icon [$project] P$(get(t, "priority", 1))"))
    end

    for n in get(input, "notes", [])
        text = get(n, "text", "")
        project = get(n, "project", "")
        push!(documents, "$text $project")
        excerpt = length(text) > 80 ? text[1:77] * "..." : text
        push!(doc_meta, Dict{String,Any}("type" => "note", "title" => "[$project]", "excerpt" => excerpt))
    end

    for f in get(input, "facts", [])
        content = get(f, "content", "")
        subject = get(f, "subject", "")
        category = get(f, "category", "")
        push!(documents, "$content $subject $category")
        excerpt = length(content) > 80 ? content[1:77] * "..." : content
        push!(doc_meta, Dict{String,Any}("type" => "fact", "title" => "[$category] $subject", "excerpt" => excerpt))
    end

    for ep in get(input, "episodes", [])
        summary = get(ep, "summary", "")
        topics = get(ep, "topics", [])
        topics_str = join([string(t) for t in topics], " ")
        push!(documents, "$summary $topics_str")
        date = get(ep, "started_at", "?")
        date_short = length(date) >= 10 ? date[1:10] : date
        push!(doc_meta, Dict{String,Any}("type" => "episode", "title" => "Conversation du $date_short",
            "excerpt" => length(summary) > 80 ? summary[1:77] * "..." : summary))
    end

    for r in get(input, "reminders", [])
        text = get(r, "text", "")
        project = get(r, "project", "")
        push!(documents, "$text $project")
        push!(doc_meta, Dict{String,Any}("type" => "reminder", "title" => text,
            "excerpt" => "[$project] ($(get(r, "status", "pending")))"))
    end

    for entry in get(input, "culture", [])
        topic = get(entry, "topic", "")
        content = get(entry, "content", "")
        tags = get(entry, "tags", [])
        tags_str = join([string(t) for t in tags], " ")
        push!(documents, "$topic $content $tags_str")
        excerpt = length(content) > 80 ? content[1:77] * "..." : content
        source_info = ""
        src = get(entry, "source", nothing)
        if src !== nothing
            sname = get(src, "name", "")
            if sname != ""
                source_info = " (via $sname)"
            end
        end
        push!(doc_meta, Dict{String,Any}("type" => "culture", "title" => "[$topic]$source_info", "excerpt" => excerpt))
    end

    if isempty(documents)
        respond(Dict{String,Any}("results" => Any[], "total" => 0, "by_type" => Dict{String,Int}()))
        return
    end

    model = build_tfidf(documents)
    qvec = query_vector(model, query)
    qnorm = norm(qvec)

    scored = []
    for i in 1:length(documents)
        sim = cosine_sim(model.matrix[i, :], qvec, model.doc_norms[i], qnorm)
        if sim > 0.01
            push!(scored, (idx=i, score=round(sim, digits=3)))
        end
    end

    sort!(scored, by=x -> -x.score)

    results = Dict{String,Any}[]
    type_counts = Dict{String,Int}()
    for item in scored
        meta = copy(doc_meta[item.idx])
        meta["score"] = item.score
        push!(results, meta)
        t = meta["type"]
        type_counts[t] = get(type_counts, t, 0) + 1
    end

    respond(Dict{String,Any}(
        "results" => results[1:min(20, length(results))],
        "total" => length(scored),
        "by_type" => type_counts
    ))
end

# ============================================================
# Cluster — Regroupement thématique des conversations
# ============================================================

function cmd_cluster(input)
    episodes = get(input, "episodes", [])

    if length(episodes) < 3
        respond(Dict{String,Any}(
            "clusters" => Any[],
            "insights" => ["Pas assez de conversations pour regrouper (minimum 3)."]
        ))
        return
    end

    documents = String[]
    for ep in episodes
        summary = get(ep, "summary", "")
        topics = get(ep, "topics", [])
        topics_str = join([string(t) for t in topics], " ")
        push!(documents, "$summary $topics_str")
    end

    model = build_tfidf(documents)

    if size(model.matrix, 2) == 0
        respond(Dict{String,Any}("clusters" => Any[], "insights" => ["Contenu insuffisant pour l'analyse."]))
        return
    end

    k = auto_k(length(episodes))
    assignments = kmeans_cluster(model.matrix, k)

    cluster_map = Dict{Int, Vector{Int}}()
    for (i, c) in enumerate(assignments)
        if !haskey(cluster_map, c)
            cluster_map[c] = Int[]
        end
        push!(cluster_map[c], i)
    end

    clusters = Dict{String,Any}[]
    for (cluster_id, member_indices) in sort(collect(cluster_map))
        cluster_topics = String[]
        episode_ids = String[]
        for idx in member_indices
            ep = episodes[idx]
            for t in get(ep, "topics", [])
                push!(cluster_topics, string(t))
            end
            push!(episode_ids, string(get(ep, "id", "ep_$idx")))
        end

        topic_freq = Dict{String,Int}()
        for t in cluster_topics
            topic_freq[t] = get(topic_freq, t, 0) + 1
        end
        sorted_topics = sort(collect(topic_freq), by=x -> -x[2])
        top_topics = [p[1] for p in sorted_topics[1:min(5, length(sorted_topics))]]

        label = isempty(top_topics) ? "Groupe $cluster_id" : join(top_topics[1:min(3, length(top_topics))], ", ")

        push!(clusters, Dict{String,Any}(
            "id" => cluster_id,
            "label" => label,
            "episode_count" => length(member_indices),
            "top_topics" => top_topics,
            "episodes" => episode_ids
        ))
    end

    sort!(clusters, by=c -> -c["episode_count"])

    insights = String[]
    push!(insights, "$(length(episodes)) conversations regroupees en $(length(clusters)) themes.")
    if !isempty(clusters)
        push!(insights, "Theme principal : \"$(clusters[1]["label"])\" ($(clusters[1]["episode_count"]) conversations).")
        if clusters[end]["episode_count"] == 1
            push!(insights, "Theme emergent : \"$(clusters[end]["label"])\" (1 seule conversation).")
        end
    end

    respond(Dict{String,Any}("clusters" => clusters, "insights" => insights))
end

# ============================================================
# Trends — Tendances temporelles
# ============================================================

function cmd_trends(input)
    episodes = get(input, "episodes", [])
    journal_entries = get(input, "journal", [])
    activity = get(input, "activity", Dict{String,Any}())

    insights = String[]
    result = Dict{String,Any}()

    # -- 1. Interaction frequency --
    if !isempty(episodes)
        daily_counts = Dict{String,Int}()
        for ep in episodes
            started = string(get(ep, "started_at", ""))
            if length(started) >= 10
                day = started[1:10]
                daily_counts[day] = get(daily_counts, day, 0) + 1
            end
        end

        if !isempty(daily_counts)
            sorted_days = sort(collect(daily_counts), by=x -> x[1])
            avg_per_day = round(mean([v for (_, v) in sorted_days]), digits=1)
            result["avg_interactions_per_day"] = avg_per_day

            if length(sorted_days) >= 14
                recent = sum(v for (_, v) in sorted_days[end-6:end])
                previous = sum(v for (_, v) in sorted_days[end-13:end-7])
                if recent > previous * 1.3
                    result["interaction_trend"] = "hausse"
                    push!(insights, "Interactions en hausse (+$(round(Int, (recent/max(1,previous) - 1) * 100))% cette semaine).")
                elseif recent < previous * 0.7
                    result["interaction_trend"] = "baisse"
                    push!(insights, "Interactions en baisse cette semaine.")
                else
                    result["interaction_trend"] = "stable"
                    push!(insights, "Interactions stables.")
                end
            else
                result["interaction_trend"] = "donnees_insuffisantes"
            end
        end
    end

    # -- 2. Topic evolution --
    if length(episodes) >= 4
        mid = max(1, div(length(episodes), 2))
        recent_eps = episodes[1:mid]
        older_eps = episodes[mid+1:end]

        recent_topics = Dict{String,Int}()
        older_topics = Dict{String,Int}()

        for ep in recent_eps
            for t in get(ep, "topics", [])
                ts = string(t)
                recent_topics[ts] = get(recent_topics, ts, 0) + 1
            end
        end
        for ep in older_eps
            for t in get(ep, "topics", [])
                ts = string(t)
                older_topics[ts] = get(older_topics, ts, 0) + 1
            end
        end

        topic_trends = Dict{String,Any}[]
        all_topics = union(Set(keys(recent_topics)), Set(keys(older_topics)))

        for t in all_topics
            r = get(recent_topics, t, 0)
            o = get(older_topics, t, 0)
            direction = r > o + 1 ? "rising" : (o > r + 1 ? "falling" : "stable")
            push!(topic_trends, Dict{String,Any}("topic" => t, "direction" => direction, "recent" => r, "older" => o))
        end

        sort!(topic_trends, by=x -> (x["direction"] == "rising" ? 0 : x["direction"] == "falling" ? 2 : 1, -x["recent"]))
        result["topic_trends"] = topic_trends[1:min(10, length(topic_trends))]

        rising = filter(x -> x["direction"] == "rising", topic_trends)
        if !isempty(rising)
            push!(insights, "Sujet en hausse : \"$(rising[1]["topic"])\".")
        end
        falling = filter(x -> x["direction"] == "falling", topic_trends)
        if !isempty(falling)
            push!(insights, "Sujet en baisse : \"$(falling[1]["topic"])\".")
        end
    end

    # -- 3. Mood trajectory --
    if !isempty(journal_entries)
        mood_trajectory = Dict{String,Any}[]
        for entry in journal_entries
            date = string(get(entry, "date", ""))
            mood = string(get(entry, "mood", ""))
            if date != "" && mood != ""
                push!(mood_trajectory, Dict{String,Any}("date" => date, "mood" => mood))
            end
        end
        sort!(mood_trajectory, by=x -> x["date"])
        result["mood_trajectory"] = mood_trajectory

        if length(mood_trajectory) >= 3
            recent_moods = [m["mood"] for m in mood_trajectory[max(1, end-2):end]]
            push!(insights, "Humeurs recentes : $(join(recent_moods, ", ")).")
        end
    end

    # -- 4. Activity patterns --
    hourly_scores = get(activity, "hourly_scores", Dict{String,Any}())
    if !isempty(hourly_scores)
        peaks = [(parse(Int, string(h)), get(v, "total", 0)) for (h, v) in hourly_scores if get(v, "total", 0) > 0]
        sort!(peaks, by=x -> -x[2])
        if !isempty(peaks)
            top_hours = peaks[1:min(3, length(peaks))]
            result["peak_hours"] = [Dict("hour" => h, "score" => s) for (h, s) in top_hours]
            push!(insights, "Heures les plus actives : $(join(["$(h)h" for (h, _) in top_hours], ", ")).")
        end
    end

    if isempty(insights)
        push!(insights, "Pas encore assez de donnees pour degager des tendances.")
    end

    result["insights"] = insights
    respond(result)
end

# ============================================================
# Recommend — Moteur de recommandation
# ============================================================

function cmd_recommend(input)
    episodes = get(input, "episodes", [])
    facts = get(input, "facts", [])
    culture = get(input, "culture", [])
    library_history = get(input, "library_history", [])
    journal = get(input, "journal", [])

    insights = String[]
    topics_to_explore = String[]
    culture_gaps = String[]
    book_suggestions = String[]
    connections = Dict{String,Any}[]

    # User interest profile from episodes
    episode_topics = Dict{String,Int}()
    for ep in episodes
        for t in get(ep, "topics", [])
            ts = string(t)
            episode_topics[ts] = get(episode_topics, ts, 0) + 1
        end
    end

    # Culture topics
    culture_topics = Dict{String,Int}()
    for entry in culture
        topic = string(get(entry, "topic", ""))
        if topic != ""
            culture_topics[topic] = get(culture_topics, topic, 0) + 1
        end
    end

    # Gaps: discussed but not in culture
    for (topic, count) in sort(collect(episode_topics), by=x -> -x[2])
        if count >= 2 && !haskey(culture_topics, topic)
            push!(culture_gaps, topic)
        end
    end
    if !isempty(culture_gaps)
        push!(insights, "$(length(culture_gaps)) sujet(s) frequemment discutes mais absents de votre culture.")
    end

    # Underexplored: in culture but rare in episodes
    for (topic, count) in culture_topics
        if count >= 2 && get(episode_topics, topic, 0) <= 1
            push!(topics_to_explore, topic)
        end
    end
    if !isempty(topics_to_explore)
        push!(insights, "$(length(topics_to_explore)) sujet(s) culturels a explorer davantage.")
    end

    # Book genre suggestions
    if !isempty(library_history)
        genre_counts = Dict{String,Int}()
        for book in library_history
            genre = string(get(book, "genre", "divers"))
            genre_counts[genre] = get(genre_counts, genre, 0) + 1
        end
        top_genres = sort(collect(genre_counts), by=x -> -x[2])

        all_genres = ["fiction", "philosophie", "science", "histoire", "poesie", "theatre", "essai", "biographie"]
        read_genres = Set(keys(genre_counts))
        for g in all_genres
            if !(g in read_genres)
                push!(book_suggestions, "Explorez le genre \"$g\" — absent de vos lectures.")
            end
        end

        if !isempty(top_genres)
            push!(insights, "Genre favori : \"$(top_genres[1][1])\" ($(top_genres[1][2]) livre(s)).")
        end
    end

    # Cross-domain connections
    for f in facts
        content_lc = lowercase(string(get(f, "content", "")))
        for (topic, _) in culture_topics
            if occursin(lowercase(topic), content_lc)
                push!(connections, Dict{String,Any}(
                    "from" => "fait: $(get(f, "subject", ""))",
                    "to" => "culture: $topic",
                    "reason" => "Le fait mentionne ce sujet culturel."
                ))
                break
            end
        end
    end

    if isempty(insights)
        push!(insights, "Continuez d'enrichir vos conversations et votre culture pour de meilleures recommandations.")
    end

    respond(Dict{String,Any}(
        "topics_to_explore" => topics_to_explore[1:min(5, length(topics_to_explore))],
        "culture_gaps" => culture_gaps[1:min(5, length(culture_gaps))],
        "book_suggestions" => book_suggestions[1:min(5, length(book_suggestions))],
        "connections" => connections[1:min(10, length(connections))],
        "insights" => insights
    ))
end

# ============================================================
# Smart Prioritize — Scoring prédictif des tâches
# ============================================================

function cmd_smart_prioritize(input)
    project = get(input, "project", Dict{String,Any}())
    now_ts = get(input, "now", round(Int, time()))
    name = get(project, "name", "?")
    tasks = get(project, "tasks", [])

    pending = filter(t -> get(t, "status", "") == "pending", tasks)
    done = filter(t -> get(t, "status", "") == "done", tasks)

    if isempty(pending)
        respond(Dict{String,Any}("ranked" => Any[], "insights" => ["Aucune tache en attente sur \"$name\"."], "velocity" => Dict{String,Any}()))
        return
    end

    # Learn from completed tasks
    completion_times = Float64[]
    priority_times = Dict{Int, Vector{Float64}}()
    quick_keywords = Dict{String,Int}()
    slow_keywords = Dict{String,Int}()

    for t in done
        created = string(get(t, "created_at", ""))
        completed = string(get(t, "completed_at", ""))
        if created != "" && completed != "" && length(created) >= 19 && length(completed) >= 19
            try
                dt_c = DateTime(created[1:19])
                dt_d = DateTime(completed[1:19])
                days = (datetime2unix(dt_d) - datetime2unix(dt_c)) / 86400
                if days >= 0
                    push!(completion_times, days)
                    prio = get(t, "priority", 1)
                    if !haskey(priority_times, prio)
                        priority_times[prio] = Float64[]
                    end
                    push!(priority_times[prio], days)

                    kws = tokenize(string(get(t, "description", "")))
                    for kw in kws
                        if days < 3
                            quick_keywords[kw] = get(quick_keywords, kw, 0) + 1
                        elseif days > 7
                            slow_keywords[kw] = get(slow_keywords, kw, 0) + 1
                        end
                    end
                end
            catch
            end
        end
    end

    # Velocity
    velocity = Dict{String,Any}()
    if !isempty(completion_times)
        velocity["avg_completion_days"] = round(mean(completion_times), digits=1)
        if !isempty(done)
            first_ts = nothing
            for t in done
                c = string(get(t, "created_at", ""))
                if length(c) >= 19
                    try
                        dt = DateTime(c[1:19])
                        ts = round(Int, datetime2unix(dt))
                        if first_ts === nothing || ts < first_ts
                            first_ts = ts
                        end
                    catch
                    end
                end
            end
            if first_ts !== nothing
                span_weeks = max(1, (now_ts - first_ts) / (86400 * 7))
                velocity["tasks_per_week"] = round(length(done) / span_weeks, digits=1)
            end
        end
    end

    # Score pending tasks
    scored = []
    for t in pending
        desc = string(get(t, "description", ""))
        priority = get(t, "priority", 1)
        created = string(get(t, "created_at", ""))

        score = Float64(priority * 10)
        age_days = 0.0

        if length(created) >= 19
            try
                dt = DateTime(created[1:19])
                age_days = (now_ts - round(Int, datetime2unix(dt))) / 86400
                score += min(30.0, age_days)
            catch
            end
        end

        # Urgency keywords
        ld = lowercase(desc)
        urgent_words = ["urgent", "critique", "asap", "immediat", "bloqu", "bug", "fix", "erreur", "panne"]
        for w in urgent_words
            if occursin(w, ld)
                score += 15.0
                break
            end
        end

        # Effort estimation
        task_kws = tokenize(desc)
        qk = sum(get(quick_keywords, kw, 0) for kw in task_kws; init=0)
        sk = sum(get(slow_keywords, kw, 0) for kw in task_kws; init=0)
        effort = qk > sk + 1 ? "rapide" : (sk > qk + 1 ? "long" : "moyen")

        # Procrastination risk
        risk = age_days > 14 ? "eleve" : (age_days > 7 || sk > qk ? "moyen" : "faible")

        # Reason
        reasons = String[]
        priority >= 4 && push!(reasons, "priorite haute")
        age_days > 7 && push!(reasons, "$(round(Int, age_days)) jours d'attente")
        effort == "rapide" && push!(reasons, "tache rapide")
        reason = isempty(reasons) ? "ordre standard" : join(reasons, ", ")

        push!(scored, (task=t, score=score, age_days=age_days, effort=effort, risk=risk, reason=reason))
    end

    sort!(scored, by=x -> -x.score)

    ranked = Dict{String,Any}[]
    for (i, item) in enumerate(scored)
        push!(ranked, Dict{String,Any}(
            "rank" => i,
            "description" => get(item.task, "description", ""),
            "priority" => get(item.task, "priority", 1),
            "score" => round(item.score, digits=1),
            "effort" => item.effort,
            "risk" => item.risk,
            "reason" => item.reason
        ))
    end

    insights = String[]
    if !isempty(scored)
        top = scored[1]
        push!(insights, "Tache recommandee : \"$(get(top.task, "description", ""))\" (score $(round(top.score, digits=1)), effort $(top.effort)).")
    end
    high_risk = count(x -> x.risk == "eleve", scored)
    high_risk > 0 && push!(insights, "$(high_risk) tache(s) a risque de procrastination eleve.")
    quick_wins = count(x -> x.effort == "rapide", scored)
    quick_wins > 0 && push!(insights, "$(quick_wins) tache(s) rapide(s) identifiee(s).")
    if haskey(velocity, "tasks_per_week")
        push!(insights, "Velocite : $(velocity["tasks_per_week"]) taches/semaine, ~$(velocity["avg_completion_days"]) jours par tache.")
    end

    respond(Dict{String,Any}("ranked" => ranked, "insights" => insights, "velocity" => velocity))
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
            elseif cmd == "search"
                cmd_search(input)
            elseif cmd == "prioritize"
                cmd_prioritize(input)
            elseif cmd == "extract_culture"
                cmd_extract_culture(input)
            elseif cmd == "cluster"
                cmd_cluster(input)
            elseif cmd == "trends"
                cmd_trends(input)
            elseif cmd == "recommend"
                cmd_recommend(input)
            elseif cmd == "smart_prioritize"
                cmd_smart_prioritize(input)
            else
                respond_error("Unknown command: $cmd")
            end
        catch e
            respond_error("Error: $(sprint(showerror, e))")
        end
    end
end

main()
