#!/usr/bin/env Rscript
# ============================================================
# Alfred Cortex — Le Cortex (R)
# Analyse statistique et tendances à long terme.
# Lit des commandes JSON sur stdin, écrit des réponses JSON sur stdout.
# ============================================================

library(jsonlite)

# ============================================================
# Protocol
# ============================================================

respond <- function(data) {
  data$status <- "ok"
  cat(toJSON(data, auto_unbox = TRUE, pretty = FALSE), "\n", sep = "")
  flush(stdout())
}

respond_error <- function(msg) {
  cat(toJSON(list(status = "error", message = msg), auto_unbox = TRUE), "\n", sep = "")
  flush(stdout())
}

# ============================================================
# Interaction Trends
# ============================================================

cmd_interaction_trends <- function(input) {
  episodes <- input$episodes

  if (is.null(episodes) || length(episodes) == 0) {
    respond(list(trends = list(
      total_conversations = 0,
      message = "Aucune conversation enregistrée."
    )))
    return(invisible(NULL))
  }

  total <- length(episodes)
  message_counts <- sapply(episodes, function(e) {
    if (is.null(e$message_count)) 0 else e$message_count
  })

  avg_messages <- round(mean(message_counts), 1)
  max_messages <- max(message_counts)
  min_messages <- min(message_counts)
  total_messages <- sum(message_counts)

  # Mode distribution
  modes <- sapply(episodes, function(e) {
    if (is.null(e$mode)) "unknown" else e$mode
  })
  mode_counts <- as.list(table(modes))

  # Topics frequency across all episodes
  all_topics <- unlist(lapply(episodes, function(e) {
    if (is.null(e$topics)) character(0) else unlist(e$topics)
  }))

  topic_freq <- list()
  if (length(all_topics) > 0) {
    tbl <- sort(table(all_topics), decreasing = TRUE)
    topic_freq <- as.list(tbl[1:min(10, length(tbl))])
  }

  respond(list(trends = list(
    total_conversations = total,
    total_messages = total_messages,
    avg_messages_per_session = avg_messages,
    max_messages = max_messages,
    min_messages = min_messages,
    mode_distribution = mode_counts,
    top_topics = topic_freq
  )))
}

# ============================================================
# Memory Stats
# ============================================================

cmd_memory_stats <- function(input) {
  facts <- input$facts

  if (is.null(facts) || length(facts) == 0) {
    respond(list(stats = list(
      total_facts = 0,
      message = "Aucun fait en mémoire."
    )))
    return(invisible(NULL))
  }

  total <- length(facts)

  # Category distribution
  categories <- sapply(facts, function(f) {
    if (is.null(f$category)) "unknown" else f$category
  })
  cat_dist <- as.list(table(categories))

  # Confidence distribution
  confidences <- sapply(facts, function(f) {
    if (is.null(f$confidence)) 0.5 else f$confidence
  })
  avg_confidence <- round(mean(confidences), 2)
  high_confidence <- sum(confidences >= 0.7)
  low_confidence <- sum(confidences < 0.4)

  # Access patterns
  access_counts <- sapply(facts, function(f) {
    if (is.null(f$access_count)) 0 else f$access_count
  })
  avg_access <- round(mean(access_counts), 1)
  never_accessed <- sum(access_counts == 0)

  respond(list(stats = list(
    total_facts = total,
    by_category = cat_dist,
    avg_confidence = avg_confidence,
    high_confidence_count = high_confidence,
    low_confidence_count = low_confidence,
    avg_access_count = avg_access,
    never_accessed = never_accessed
  )))
}

# ============================================================
# Behavioral Analysis
# ============================================================

cmd_behavioral_analysis <- function(input) {
  episodes <- input$episodes
  facts <- input$facts

  insights <- list()

  if (!is.null(episodes) && length(episodes) > 0) {
    # Conversation length trend
    message_counts <- sapply(episodes, function(e) {
      if (is.null(e$message_count)) 0 else e$message_count
    })

    n <- length(message_counts)
    if (n >= 3) {
      first_half <- mean(message_counts[1:floor(n/2)])
      second_half <- mean(message_counts[(floor(n/2)+1):n])

      if (second_half > first_half * 1.3) {
        insights <- c(insights, list("Les conversations deviennent plus longues — Monsieur s'engage davantage."))
      } else if (second_half < first_half * 0.7) {
        insights <- c(insights, list("Les conversations raccourcissent — Monsieur devient plus efficace."))
      } else {
        insights <- c(insights, list("La durée des conversations est stable."))
      }
    }

    # Topic diversity
    all_topics <- unlist(lapply(episodes, function(e) {
      if (is.null(e$topics)) character(0) else unlist(e$topics)
    }))

    if (length(all_topics) > 0) {
      unique_topics <- length(unique(all_topics))
      topic_ratio <- round(unique_topics / length(all_topics), 2)

      if (topic_ratio > 0.7) {
        insights <- c(insights, list(paste0("Grande diversité de sujets (", unique_topics, " sujets uniques) — Monsieur est curieux et polyvalent.")))
      } else if (topic_ratio < 0.3) {
        insights <- c(insights, list("Sujets récurrents — Monsieur est focalisé sur ses priorités."))
      }
    }
  }

  if (!is.null(facts) && length(facts) > 0) {
    # Memory growth insight
    total_facts <- length(facts)
    insights <- c(insights, list(paste0("Alfred a mémorisé ", total_facts, " faits sur Monsieur.")))
  }

  if (length(insights) == 0) {
    insights <- list("Pas assez de données pour une analyse comportementale.")
  }

  respond(list(insights = insights))
}

# ============================================================
# Productivity Stats
# ============================================================

cmd_productivity_stats <- function(input) {
  projects <- input$projects

  if (is.null(projects) || length(projects) == 0) {
    respond(list(stats = list(
      overall_completion = 0,
      one_liner = "Aucun projet en cours.",
      project_health = list(),
      overdue_count = 0
    )))
    return(invisible(NULL))
  }

  total_done <- 0
  total_pending <- 0
  total_reminders <- 0
  overdue_count <- 0
  project_health <- list()
  priority_dist <- list("1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0)

  for (proj in projects) {
    name <- if (is.null(proj$name)) "?" else proj$name
    tasks <- if (is.null(proj$tasks)) list() else proj$tasks
    reminders <- if (is.null(proj$reminders)) list() else proj$reminders

    p_done <- 0
    p_pending <- 0

    for (t in tasks) {
      status <- if (is.null(t$status)) "pending" else t$status
      if (status == "done") {
        p_done <- p_done + 1
      } else {
        p_pending <- p_pending + 1
        prio <- if (is.null(t$priority)) 1 else t$priority
        key <- as.character(prio)
        if (key %in% names(priority_dist)) {
          priority_dist[[key]] <- priority_dist[[key]] + 1
        }
      }
    }

    total_done <- total_done + p_done
    total_pending <- total_pending + p_pending

    p_total <- p_done + p_pending
    completion <- if (p_total > 0) round(p_done / p_total * 100, 1) else 0
    health <- if (p_total == 0) 50 else completion

    project_health <- c(project_health, list(list(
      name = name,
      completion = completion,
      health_score = health,
      pending = p_pending,
      done = p_done
    )))

    # Reminders
    for (r in reminders) {
      total_reminders <- total_reminders + 1
      r_status <- if (is.null(r$status)) "pending" else r$status
      if (r_status == "pending" || r_status == "active") {
        due_at <- if (is.null(r$due_at)) 0 else r$due_at
        now <- as.numeric(Sys.time())
        if (due_at <= now) {
          overdue_count <- overdue_count + 1
        }
      }
    }
  }

  grand_total <- total_done + total_pending
  overall <- if (grand_total > 0) round(total_done / grand_total * 100, 1) else 0

  # Build one-liner
  parts <- c()
  parts <- c(parts, paste0(overall, "% accompli"))
  if (total_pending > 0) parts <- c(parts, paste0(total_pending, " en attente"))
  if (overdue_count > 0) parts <- c(parts, paste0(overdue_count, " rappel(s) en retard"))
  one_liner <- paste(parts, collapse = ", ")

  respond(list(stats = list(
    overall_completion = overall,
    total_done = total_done,
    total_pending = total_pending,
    project_health = project_health,
    overdue_count = overdue_count,
    total_reminders = total_reminders,
    priority_distribution = priority_dist,
    one_liner = one_liner
  )))
}

# ============================================================
# Culture Trends
# ============================================================

cmd_culture_trends <- function(input) {
  culture <- input$culture

  if (is.null(culture) || length(culture) == 0) {
    respond(list(trends = list(
      total = 0,
      one_liner = "Culture encore vierge.",
      topic_count = 0,
      source_count = 0
    )))
    return(invisible(NULL))
  }

  total <- length(culture)

  # Topic distribution
  topics <- sapply(culture, function(e) {
    if (is.null(e$topic)) "divers" else e$topic
  })
  topic_counts <- as.list(sort(table(topics), decreasing = TRUE))
  topic_count <- length(unique(topics))

  # Source distribution
  source_names <- sapply(culture, function(e) {
    if (!is.null(e$source) && !is.null(e$source$name) && nchar(e$source$name) > 0) {
      e$source$name
    } else if (!is.null(e$source) && !is.null(e$source$type)) {
      e$source$type
    } else {
      "inconnu"
    }
  })
  source_counts <- as.list(sort(table(source_names), decreasing = TRUE))
  source_count <- length(unique(source_names))

  # Source types
  source_types <- sapply(culture, function(e) {
    if (!is.null(e$source) && !is.null(e$source$type)) e$source$type else "other"
  })
  type_counts <- as.list(table(source_types))

  # Tag analysis
  all_tags <- unlist(lapply(culture, function(e) {
    if (is.null(e$tags)) character(0) else unlist(e$tags)
  }))
  tag_counts <- list()
  if (length(all_tags) > 0) {
    tbl <- sort(table(all_tags), decreasing = TRUE)
    tag_counts <- as.list(tbl[1:min(10, length(tbl))])
  }

  # Temporal analysis - learning velocity
  dates <- c()
  for (e in culture) {
    if (!is.null(e$learned_at) && nchar(e$learned_at) >= 10) {
      dates <- c(dates, substr(e$learned_at, 1, 10))
    }
  }

  velocity_per_week <- 0
  growth_trend <- "stable"
  if (length(dates) >= 2) {
    parsed_dates <- as.Date(dates, format = "%Y-%m-%d")
    date_range <- as.numeric(difftime(max(parsed_dates), min(parsed_dates), units = "weeks"))
    if (date_range > 0) {
      velocity_per_week <- round(total / date_range, 1)
    }

    # Growth: compare first half vs second half
    sorted_dates <- sort(parsed_dates)
    n <- length(sorted_dates)
    mid <- floor(n / 2)
    if (mid > 0 && mid < n) {
      first_span <- as.numeric(difftime(sorted_dates[mid], sorted_dates[1], units = "days"))
      second_span <- as.numeric(difftime(sorted_dates[n], sorted_dates[mid + 1], units = "days"))
      first_rate <- if (first_span > 0) mid / first_span else 0
      second_rate <- if (second_span > 0) (n - mid) / second_span else 0
      if (second_rate > first_rate * 1.3) {
        growth_trend <- "accelerating"
      } else if (second_rate < first_rate * 0.7) {
        growth_trend <- "decelerating"
      }
    }
  }

  # Domain distribution (percentage)
  domain_distribution <- list()
  for (t in names(topic_counts)) {
    domain_distribution[[t]] <- round(as.numeric(topic_counts[[t]]) / total * 100, 1)
  }

  # Build one-liner
  top_topic <- if (length(names(topic_counts)) > 0) names(topic_counts)[1] else "aucun"
  trend_word <- switch(growth_trend,
    "accelerating" = "en acceleration",
    "decelerating" = "en ralentissement",
    "stable"
  )
  one_liner <- paste0(total, " connaissances en ", topic_count, " sujet(s), sujet principal : ", top_topic, " (", trend_word, ")")

  respond(list(trends = list(
    total = total,
    topic_count = topic_count,
    source_count = source_count,
    velocity_per_week = velocity_per_week,
    growth_trend = growth_trend,
    domain_distribution = domain_distribution,
    topic_counts = topic_counts,
    source_counts = source_counts,
    type_counts = type_counts,
    top_tags = tag_counts,
    one_liner = one_liner
  )))
}

# ============================================================
# Correlation Analysis
# ============================================================

cmd_correlation_analysis <- function(input) {
  episodes <- if (is.null(input$episodes)) list() else input$episodes
  projects <- if (is.null(input$projects)) list() else input$projects
  culture <- if (is.null(input$culture)) list() else input$culture

  correlations <- list()
  insights <- list()

  # Extract project names
  project_names <- sapply(projects, function(p) {
    if (is.null(p$name)) "" else tolower(p$name)
  })

  # Extract conversation topics
  episode_topics <- unlist(lapply(episodes, function(e) {
    if (is.null(e$topics)) character(0) else tolower(unlist(e$topics))
  }))

  # Extract culture topics
  culture_topics <- sapply(culture, function(e) {
    if (is.null(e$topic)) "" else tolower(e$topic)
  })

  # Correlation 1: Project names mentioned in conversation topics
  if (length(project_names) > 0 && length(episode_topics) > 0) {
    for (pname in unique(project_names)) {
      if (nchar(pname) >= 3) {
        matches <- sum(grepl(pname, episode_topics, fixed = TRUE))
        if (matches > 0) {
          correlations <- c(correlations, list(paste0(
            "Le projet \"", pname, "\" apparait dans ", matches, " sujet(s) de conversation."
          )))
        }
      }
    }
  }

  # Correlation 2: Culture topics that overlap with project note keywords
  if (length(culture_topics) > 0 && length(projects) > 0) {
    for (proj in projects) {
      pname <- if (is.null(proj$name)) "" else proj$name
      notes <- if (is.null(proj$notes)) list() else proj$notes
      note_text <- tolower(paste(sapply(notes, function(n) {
        if (is.null(n$text)) "" else n$text
      }), collapse = " "))

      for (ct in unique(culture_topics)) {
        if (nchar(ct) >= 3 && grepl(ct, note_text, fixed = TRUE)) {
          correlations <- c(correlations, list(paste0(
            "Le sujet culturel \"", ct, "\" est lie aux notes du projet \"", pname, "\"."
          )))
        }
      }
    }
  }

  # Correlation 3: Culture topics that match conversation topics
  if (length(culture_topics) > 0 && length(episode_topics) > 0) {
    shared <- intersect(unique(culture_topics), unique(episode_topics))
    if (length(shared) > 0) {
      correlations <- c(correlations, list(paste0(
        "Sujets communs entre culture et conversations : ", paste(shared, collapse = ", "), "."
      )))
    }
  }

  # Generate insights
  if (length(correlations) > 0) {
    insights <- c(insights, list(paste0(length(correlations), " correlation(s) detectee(s) entre vos organes.")))
  }

  if (length(culture_topics) > 0 && length(episode_topics) > 0) {
    overlap <- length(intersect(unique(culture_topics), unique(episode_topics)))
    culture_only <- length(setdiff(unique(culture_topics), unique(episode_topics)))
    if (culture_only > 0) {
      insights <- c(insights, list(paste0(
        culture_only, " sujet(s) culturel(s) jamais aborde(s) en conversation."
      )))
    }
    if (overlap > 0) {
      insights <- c(insights, list(paste0(
        "Bonne coherence : ", overlap, " sujet(s) culturel(s) actifs en conversation."
      )))
    }
  }

  if (length(insights) == 0) {
    insights <- list("Pas assez de donnees croisees pour une analyse de correlations.")
  }

  one_liner <- if (length(correlations) > 0) {
    paste0(length(correlations), " lien(s) entre vos organes detecte(s)")
  } else {
    "Aucune correlation detectee pour le moment."
  }

  respond(list(
    correlations = correlations,
    insights = insights,
    one_liner = one_liner
  ))
}

# ============================================================
# Main Loop
# ============================================================

con <- file("stdin", "r")
while (TRUE) {
  line <- readLines(con, n = 1)
  if (length(line) == 0) break
  if (nchar(trimws(line)) == 0) next

  tryCatch({
    input <- fromJSON(line, simplifyVector = FALSE)
    cmd <- input$cmd

    if (cmd == "interaction_trends") {
      cmd_interaction_trends(input)
    } else if (cmd == "memory_stats") {
      cmd_memory_stats(input)
    } else if (cmd == "behavioral_analysis") {
      cmd_behavioral_analysis(input)
    } else if (cmd == "productivity_stats") {
      cmd_productivity_stats(input)
    } else if (cmd == "culture_trends") {
      cmd_culture_trends(input)
    } else if (cmd == "correlation_analysis") {
      cmd_correlation_analysis(input)
    } else {
      respond_error(paste("Unknown command:", cmd))
    }
  }, error = function(e) {
    respond_error(paste("Error:", e$message))
  })
}
close(con)
