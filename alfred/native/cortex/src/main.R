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
    } else {
      respond_error(paste("Unknown command:", cmd))
    }
  }, error = function(e) {
    respond_error(paste("Error:", e$message))
  })
}
close(con)
