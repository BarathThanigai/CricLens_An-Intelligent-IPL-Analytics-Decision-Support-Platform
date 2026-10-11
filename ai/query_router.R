# ------------------------------------------------------------------
# CricLens - Natural Language Query Router
# ------------------------------------------------------------------
#
# Flow:
#
# User query
#     ↓
# NVIDIA NIM intent parsing
#     ↓
# Deterministic validation / parameter extraction
#     ↓
# R analytics functions
#     ↓
# Validated R result
#     ↓
# NVIDIA NIM explanation
#     ↓
# User-facing response
#
# IMPORTANT:
# R is the single source of truth for all statistics.
# NIM must never calculate, invent, or modify analytical values.
# ------------------------------------------------------------------

suppressMessages({
  library(dplyr)
  library(stringr)
  library(jsonlite)
})

# ------------------------------------------------------------------
# Load analytics and AI dependencies
# ------------------------------------------------------------------

source("R/player_analysis.R")
source("R/team_analysis.R")
source("R/venue_analysis.R")
source("ai/nim_client.R")
source("ai/prompt_templates.R")

# ------------------------------------------------------------------
# Utility helpers
# ------------------------------------------------------------------

safe_character <- function(x, default = NULL) {

  if (is.null(x)) {
    return(default)
  }

  x <- as.character(x)

  if (length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) {
    return(default)
  }

  trimws(x[1])
}


safe_integer <- function(x, default = NULL) {

  if (is.null(x)) {
    return(default)
  }

  value <- suppressWarnings(
    as.integer(x[1])
  )

  if (is.na(value)) {
    return(default)
  }

  value
}


escape_html <- function(x) {

  x <- as.character(x)

  x <- gsub(
    "&",
    "&amp;",
    x,
    fixed = TRUE
  )

  x <- gsub(
    "<",
    "&lt;",
    x,
    fixed = TRUE
  )

  x <- gsub(
    ">",
    "&gt;",
    x,
    fixed = TRUE
  )

  x <- gsub(
    "\"",
    "&quot;",
    x,
    fixed = TRUE
  )

  x
}


render_markdown_to_html <- function(text) {

  if (is.null(text) || !nzchar(trimws(text))) {
    return("")
  }

  text <- as.character(text)

  # Escape HTML first.
  text <- escape_html(text)

  # Bold.
  text <- gsub(
    "\\*\\*(.*?)\\*\\*",
    "<strong>\\1</strong>",
    text
  )

  # Headings.
  text <- gsub(
    "^### (.*)$",
    "<h5>\\1</h5>",
    text
  )

  text <- gsub(
    "^## (.*)$",
    "<h4>\\1</h4>",
    text
  )

  text <- gsub(
    "^# (.*)$",
    "<h3>\\1</h3>",
    text
  )

  # Bullet points.
  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1]]

  output <- character(0)

  in_list <- FALSE

  for (line in lines) {

    trimmed <- trimws(line)

    if (str_detect(trimmed, "^[-*] ")) {

      if (!in_list) {
        output <- c(
          output,
          "<ul>"
        )

        in_list <- TRUE
      }

      item <- str_remove(
        trimmed,
        "^[-*] "
      )

      output <- c(
        output,
        paste0(
          "<li>",
          item,
          "</li>"
        )
      )

    } else {

      if (in_list) {

        output <- c(
          output,
          "</ul>"
        )

        in_list <- FALSE
      }

      if (nzchar(trimmed)) {

        output <- c(
          output,
          paste0(
            "<p>",
            trimmed,
            "</p>"
          )
        )
      }
    }
  }

  if (in_list) {
    output <- c(
      output,
      "</ul>"
    )
  }

  paste(
    output,
    collapse = "\n"
  )
}

# ------------------------------------------------------------------
# Dataset value helpers
# ------------------------------------------------------------------

get_dataset_players <- function(df) {

  values <- unique(
    c(
      as.character(df$batter),
      as.character(df$bowler),
      as.character(df$player_out)
    )
  )

  values <- values[
    !is.na(values) &
      nzchar(trimws(values))
  ]

  sort(
    unique(values)
  )
}


get_dataset_teams <- function(df) {

  values <- unique(
    c(
      as.character(df$batting_team),
      as.character(df$bowling_team),
      as.character(df$match_won_by)
    )
  )

  values <- values[
    !is.na(values) &
      nzchar(trimws(values))
  ]

  sort(
    unique(values)
  )
}


get_dataset_venues <- function(df) {

  if ("canonical_venue" %in% names(df)) {

    values <- as.character(
      df$canonical_venue
    )

  } else {

    values <- as.character(
      df$venue
    )
  }

  values <- values[
    !is.na(values) &
      nzchar(trimws(values))
  ]

  sort(
    unique(values)
  )
}


resolve_dataset_value <- function(
  requested,
  available
) {

  requested <- safe_character(
    requested
  )

  if (is.null(requested)) {
    return(NULL)
  }

  if (length(available) == 0) {
    return(NULL)
  }

  available_lower <- tolower(
    trimws(available)
  )

  requested_lower <- tolower(
    trimws(requested)
  )

  # Exact match.
  exact <- which(
    available_lower == requested_lower
  )

  if (length(exact) > 0) {
    return(
      available[exact[1]]
    )
  }

  # Unique substring match.
  matches <- which(
    str_detect(
      available_lower,
      fixed(requested_lower)
    ) |
      str_detect(
        requested_lower,
        fixed(available_lower)
      )
  )

  if (length(matches) == 1) {
    return(
      available[matches[1]]
    )
  }

  NULL
}

# ------------------------------------------------------------------
# Deterministic season extraction
# ------------------------------------------------------------------
#
# NIM is responsible for understanding the user's intent.
#
# Explicit season/year constraints are extracted directly from
# the user's original query and validated against the actual
# dataset.
#
# This prevents a query such as:
#
#   "Who was the top run scorer in IPL 2024?"
#
# from accidentally becoming an all-time query when NIM returns:
#
#   season = NULL
#
# No IPL seasons are hardcoded here.
# ------------------------------------------------------------------

extract_query_season <- function(
  query,
  df
) {

  query <- as.character(
    query
  )

  # Extract four-digit year candidates from the user's query.
  year_matches <- str_extract_all(
    query,
    "\\b20[0-9]{2}\\b"
  )[[1]]

  if (length(year_matches) == 0) {
    return(NULL)
  }

  # Seasons are obtained directly from the dataset.
  available_seasons <- unique(
    as.integer(
      df[["season_year"]][
        !is.na(df[["season_year"]])
      ]
    )
  )

  # Keep only years actually represented in the dataset.
  valid_matches <- as.integer(
    year_matches
  )

  valid_matches <- valid_matches[
    valid_matches %in% available_seasons
  ]

  if (length(valid_matches) == 0) {
    return(NULL)
  }

  # Return the first valid season mentioned.
  as.character(
    valid_matches[1]
  )
}

# ------------------------------------------------------------------
# JSON extraction
# ------------------------------------------------------------------

extract_json_object <- function(text) {

  if (is.null(text)) {
    return(NULL)
  }

  text <- as.character(text)

  # Remove markdown code fences if NIM returns them.
  text <- str_replace_all(
    text,
    "```json",
    ""
  )

  text <- str_replace_all(
    text,
    "```",
    ""
  )

  text <- trimws(
    text
  )

  # Locate first JSON object.
  start <- regexpr(
    "\\{",
    text
  )

  end <- regexpr(
    "\\}[^{]*$",
    text
  )

  if (start[1] == -1 || end[1] == -1) {
    return(NULL)
  }

  json_text <- substr(
    text,
    start[1],
    start[1] + attr(start, "match.length") - 1
  )

  # If simple extraction fails, try the complete string.
  parsed <- tryCatch(
    fromJSON(
      json_text,
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(parsed)) {
    return(parsed)
  }

  # Fallback: find the largest JSON-looking object.
  open_positions <- gregexpr(
    "\\{",
    text
  )[[1]]

  close_positions <- gregexpr(
    "\\}",
    text
  )[[1]]

  if (
    length(open_positions) == 1 &&
    open_positions[1] == -1
  ) {
    return(NULL)
  }

  if (
    length(close_positions) == 1 &&
    close_positions[1] == -1
  ) {
    return(NULL)
  }

  for (s in open_positions) {

    for (e in rev(close_positions)) {

      if (e <= s) {
        next
      }

      candidate <- substr(
        text,
        s,
        e
      )

      parsed <- tryCatch(
        fromJSON(
          candidate,
          simplifyVector = FALSE
        ),
        error = function(e) NULL
      )

      if (!is.null(parsed)) {
        return(parsed)
      }
    }
  }

  NULL
}

# ------------------------------------------------------------------
# NIM intent parser
# ------------------------------------------------------------------

parse_query_intent <- function(
  query,
  df
) {

  prompt <- paste0(
    INTENT_PARSER_SYSTEM_PROMPT,
    "\n\nUSER QUERY:\n",
    query
  )

  response <- call_nvidia_nim(
    prompt = prompt,
    system_prompt = INTENT_PARSER_SYSTEM_PROMPT
  )

  parsed <- extract_json_object(
    response
  )

  if (is.null(parsed)) {
    return(NULL)
  }

  intent <- safe_character(
    parsed$intent
  )

  if (is.null(intent)) {
    return(NULL)
  }

  supported_intents <- c(
    "top_batters",
    "top_bowlers",
    "player_profile",
    "team_summary",
    "venue_stats",
    "head_to_head",
    "unsupported"
  )

  if (!(intent %in% supported_intents)) {
    return(NULL)
  }

  parameters <- parsed$parameters

  if (is.null(parameters)) {
    parameters <- list()
  }

  # ---------------------------------------------------------------
  # Deterministic season extraction
  # ---------------------------------------------------------------
  #
  # NIM identifies the intent.
  # The original user query determines explicit season scope.
  #
  # This value is validated against the actual dataset.
  # ---------------------------------------------------------------

  query_season <- extract_query_season(
    query,
    df
  )

  if (!is.null(query_season)) {
    parameters$season <- query_season
  }

  list(
    intent = intent,
    parameters = parameters
  )
}

# ------------------------------------------------------------------
# Deterministic fallback intent parser
# ------------------------------------------------------------------

fallback_query_intent <- function(
  query,
  df
) {

  q_lower <- tolower(
    trimws(query)
  )

  query_season <- extract_query_season(
    query,
    df
  )

  # ---------------------------------------------------------------
  # Dataset-derived values
  # ---------------------------------------------------------------

  players <- get_dataset_players(
    df
  )

  teams <- get_dataset_teams(
    df
  )

  venues <- get_dataset_venues(
    df
  )

  # ---------------------------------------------------------------
  # Unsupported / prediction questions
  # ---------------------------------------------------------------

  unsupported_patterns <- c(
    "predict.*winner",
    "who.*will.*win",
    "who.*win.*next",
    "future",
    "forecast.*winner",
    "predict.*future"
  )

  if (
    any(
      sapply(
        unsupported_patterns,
        function(pattern) {
          str_detect(
            q_lower,
            regex(
              pattern,
              ignore_case = TRUE
            )
          )
        }
      )
    )
  ) {

    return(
      list(
        intent = "unsupported",
        parameters = list()
      )
    )
  }

  # ---------------------------------------------------------------
  # Head-to-head
  # ---------------------------------------------------------------

  matched_teams <- teams[
    sapply(
      teams,
      function(team) {

        str_detect(
          q_lower,
          fixed(
            tolower(team)
          )
        )
      }
    )
  ]

  if (
    length(matched_teams) >= 2 &&
    (
      str_detect(
        q_lower,
        "vs"
      ) ||
      str_detect(
        q_lower,
        "versus"
      ) ||
      str_detect(
        q_lower,
        "head.to.head"
      ) ||
      str_detect(
        q_lower,
        "compare"
      )
    )
  ) {

    return(
      list(
        intent = "head_to_head",
        parameters = list(
          team1 = matched_teams[1],
          team2 = matched_teams[2],
          season = query_season
        )
      )
    )
  }

  # ---------------------------------------------------------------
  # Player profile
  # ---------------------------------------------------------------

  # First prefer an exact dataset player-name phrase.
  exact_player_matches <- players[
    sapply(
      players,
      function(player) {

        str_detect(
          q_lower,
          fixed(
            tolower(
              trimws(player)
            )
          )
        )
      }
    )
  ]

  if (length(exact_player_matches) > 0) {

    exact_player_matches <- exact_player_matches[
      order(
        nchar(exact_player_matches),
        decreasing = TRUE
      )
    ]

    return(
      list(
        intent = "player_profile",
        parameters = list(
          player_name = exact_player_matches[1],
          season = query_season
        )
      )
    )
  }

  # If an exact name isn't present, attempt a unique meaningful
  # player token match from the dataset.
  player_candidates <- list()

  for (player in players) {

    player_lower <- tolower(
      trimws(player)
    )

    parts <- str_split(
      player_lower,
      "\\s+"
    )[[1]]

    meaningful_parts <- parts[
      nchar(parts) >= 3
    ]

    if (length(meaningful_parts) == 0) {
      next
    }

    matching_parts <- meaningful_parts[
      sapply(
        meaningful_parts,
        function(part) {
          str_detect(
            q_lower,
            fixed(part)
          )
        }
      )
    ]

    if (length(matching_parts) > 0) {

      player_candidates[[length(player_candidates) + 1]] <- list(
        list(
          player = player,
          matches = matching_parts
        )
      )
    }
  }

  if (length(player_candidates) > 0) {

    # Only use a surname/token match when it identifies
    # exactly one dataset player.
    candidate_names <- unique(
      vapply(
        player_candidates,
        function(x) x$player,
        character(1)
      )
    )

    if (length(candidate_names) == 1) {

      return(
        list(
          intent = "player_profile",
          parameters = list(
            player_name = candidate_names[1],
            season = query_season
          )
        )
      )
    }
  }

  # ---------------------------------------------------------------
  # Top bowlers
  # ---------------------------------------------------------------

  if (
    str_detect(
      q_lower,
      "wicket"
    ) ||
    str_detect(
      q_lower,
      "bowler"
    )
  ) {

    return(
      list(
        intent = "top_bowlers",
        parameters = list(
          season = query_season,
          top_n = 5,
          metric = "wickets"
        )
      )
    )
  }

  # ---------------------------------------------------------------
  # Top batters
  # ---------------------------------------------------------------

  if (
    str_detect(
      q_lower,
      "run"
    ) ||
    str_detect(
      q_lower,
      "batter"
    ) ||
    str_detect(
      q_lower,
      "batsman"
    ) ||
    str_detect(
      q_lower,
      "batting"
    )
  ) {

    return(
      list(
        intent = "top_batters",
        parameters = list(
          season = query_season,
          top_n = 5,
          metric = "runs"
        )
      )
    )
  }

  # ---------------------------------------------------------------
  # Team summary
  # ---------------------------------------------------------------

  if (
    str_detect(
      q_lower,
      "team"
    ) ||
    str_detect(
      q_lower,
      "franchise"
    ) ||
    str_detect(
      q_lower,
      "wins"
    ) ||
    str_detect(
      q_lower,
      "won"
    )
  ) {

    return(
      list(
        intent = "team_summary",
        parameters = list(
          team_name = NULL,
          season = query_season
        )
      )
    )
  }

  # ---------------------------------------------------------------
  # Venue statistics
  # ---------------------------------------------------------------

  matched_venues <- venues[
    sapply(
      venues,
      function(venue) {

        str_detect(
          q_lower,
          fixed(
            tolower(venue)
          )
        )
      }
    )
  ]

  if (length(matched_venues) > 0) {

    return(
      list(
        intent = "venue_stats",
        parameters = list(
          venue_name = matched_venues[1],
          season = query_season
        )
      )
    )
  }

  # ---------------------------------------------------------------
  # Final fallback
  # ---------------------------------------------------------------

  list(
    intent = "unsupported",
    parameters = list()
  )
}

# ------------------------------------------------------------------
# Validate intent
# ------------------------------------------------------------------

validate_intent <- function(
  parsed,
  df
) {

  if (is.null(parsed)) {
    return(NULL)
  }

  intent <- safe_character(
    parsed$intent
  )

  parameters <- parsed$parameters

  if (is.null(parameters)) {
    parameters <- list()
  }

  if (intent == "unsupported") {

    return(
      list(
        intent = "unsupported",
        parameters = list()
      )
    )
  }

  # ---------------------------------------------------------------
  # Validate season
  # ---------------------------------------------------------------

  season <- safe_character(
    parameters$season
  )

  if (!is.null(season)) {

    available_seasons <- unique(
      as.character(
        as.integer(
          df[["season_year"]][
            !is.na(df[["season_year"]])
          ]
        )
      )
    )

    if (!(season %in% available_seasons)) {
      season <- NULL
    }
  }

  # ---------------------------------------------------------------
  # Top N
  # ---------------------------------------------------------------

  top_n <- safe_integer(
    parameters$top_n,
    default = 5
  )

  top_n <- max(
    1,
    min(
      top_n,
      20
    )
  )

  # ---------------------------------------------------------------
  # Player
  # ---------------------------------------------------------------

  player_name <- NULL

  if (intent == "player_profile") {

    player_name <- resolve_dataset_value(
      parameters$player_name,
      get_dataset_players(df)
    )

    if (is.null(player_name)) {

      return(
        list(
          intent = "unsupported",
          parameters = list()
        )
      )
    }
  }

  # ---------------------------------------------------------------
  # Team
  # ---------------------------------------------------------------

  team_name <- NULL

  if (
    intent == "team_summary" &&
    !is.null(parameters$team_name)
  ) {

    team_name <- resolve_dataset_value(
      parameters$team_name,
      get_dataset_teams(df)
    )
  }

  # ---------------------------------------------------------------
  # Venue
  # ---------------------------------------------------------------

  venue_name <- NULL

  if (intent == "venue_stats") {

    venue_name <- resolve_dataset_value(
      parameters$venue_name,
      get_dataset_venues(df)
    )

    if (is.null(venue_name)) {

      return(
        list(
          intent = "unsupported",
          parameters = list()
        )
      )
    }
  }

  # ---------------------------------------------------------------
  # Head-to-head
  # ---------------------------------------------------------------

  team1 <- NULL
  team2 <- NULL

  if (intent == "head_to_head") {

    team1 <- resolve_dataset_value(
      parameters$team1,
      get_dataset_teams(df)
    )

    team2 <- resolve_dataset_value(
      parameters$team2,
      get_dataset_teams(df)
    )

    if (
      is.null(team1) ||
      is.null(team2) ||
      identical(
        team1,
        team2
      )
    ) {

      return(
        list(
          intent = "unsupported",
          parameters = list()
        )
      )
    }
  }

  list(
    intent = intent,
    parameters = list(
      season = season,
      top_n = top_n,
      metric = safe_character(
        parameters$metric
      ),
      player_name = player_name,
      team_name = team_name,
      venue_name = venue_name,
      team1 = team1,
      team2 = team2
    )
  )
}

# ------------------------------------------------------------------
# Execute R analytics
# ------------------------------------------------------------------

execute_r_analytics <- function(
  validated,
  df
) {

  if (is.null(validated)) {
    return(NULL)
  }

  intent <- validated$intent
  p <- validated$parameters

  season <- p$season

  # ---------------------------------------------------------------
  # Unsupported
  # ---------------------------------------------------------------

  if (intent == "unsupported") {

    return(
      list(
        intent = "unsupported",
        result = NULL
      )
    )
  }

  # ---------------------------------------------------------------
  # Top batters
  # ---------------------------------------------------------------

  if (intent == "top_batters") {

    result <- get_top_batters(
      df,
      season_filter = season,
      top_n = p$top_n
    )

    return(
      list(
        intent = intent,
        result = result
      )
    )
  }

  # ---------------------------------------------------------------
  # Top bowlers
  # ---------------------------------------------------------------

  if (intent == "top_bowlers") {

    result <- get_top_bowlers(
      df,
      season_filter = season,
      top_n = p$top_n
    )

    return(
      list(
        intent = intent,
        result = result
      )
    )
  }

  # ---------------------------------------------------------------
  # Player profile
  # ---------------------------------------------------------------

  if (intent == "player_profile") {

    result <- get_player_batting_profile(
      df,
      p$player_name
    )

    return(
      list(
        intent = intent,
        result = result
      )
    )
  }

  # ---------------------------------------------------------------
  # Team summary
  # ---------------------------------------------------------------

  if (intent == "team_summary") {

    result <- get_team_performance_summary(
      df,
      season_filter = season
    )

    if (!is.null(p$team_name)) {

      result <- result %>%
        filter(
          .data$team == p$team_name
        )
    }

    return(
      list(
        intent = intent,
        result = result
      )
    )
  }

  # ---------------------------------------------------------------
  # Venue statistics
  # ---------------------------------------------------------------

  if (intent == "venue_stats") {

    result <- get_venue_profile(
      df,
      p$venue_name
    )

    return(
      list(
        intent = intent,
        result = result
      )
    )
  }

  # ---------------------------------------------------------------
  # Head-to-head
  # ---------------------------------------------------------------

  if (intent == "head_to_head") {

    result <- get_head_to_head(
      df,
      p$team1,
      p$team2
    )

    return(
      list(
        intent = intent,
        result = result
      )
    )
  }

  list(
    intent = "unsupported",
    result = NULL
  )
}

# ------------------------------------------------------------------
# Prepare NIM explanation payload
# ------------------------------------------------------------------

prepare_explainer_payload <- function(
  analytics_result
) {

  if (is.null(analytics_result)) {
    return("{}")
  }

  tryCatch(

    toJSON(
      analytics_result,
      auto_unbox = TRUE,
      dataframe = "rows",
      na = "null",
      null = "null",
      pretty = TRUE
    ),

    error = function(e) {

      "{}"
    }
  )
}

# ------------------------------------------------------------------
# Explain validated R result using NIM
# ------------------------------------------------------------------

explain_validated_result <- function(
  query,
  analytics_result
) {

  if (is.null(analytics_result)) {
    return(NULL)
  }

  if (
    identical(
      analytics_result$intent,
      "unsupported"
    )
  ) {

    return(
      paste(
        "I can't answer that from the current CricLens dataset.",
        "I can answer questions about IPL batting, bowling,",
        "teams, venues, matches, and the supported analytics."
      )
    )
  }

  result_json <- prepare_explainer_payload(
    analytics_result$result
  )

  prompt <- paste0(
    RESULT_EXPLAINER_SYSTEM_PROMPT,
    "\n\nUSER QUERY:\n",
    query,
    "\n\nVALIDATED R ANALYTICS RESULT:\n",
    result_json
  )

  call_nvidia_nim(
    prompt = prompt,
    system_prompt = RESULT_EXPLAINER_SYSTEM_PROMPT
  )
}

# ------------------------------------------------------------------
# Deterministic R result formatter
# ------------------------------------------------------------------

format_r_result <- function(
  analytics_result
) {

  if (is.null(analytics_result)) {

    return(
      "I couldn't process that query."
    )
  }

  intent <- analytics_result$intent
  result <- analytics_result$result

  if (intent == "unsupported") {

    return(
      paste(
        "I can't answer that from the current CricLens dataset.",
        "Please ask about supported IPL analytics."
      )
    )
  }

  # ---------------------------------------------------------------
  # Top batters
  # ---------------------------------------------------------------

  if (intent == "top_batters") {

    if (
      is.null(result) ||
      nrow(result) == 0
    ) {

      return(
        "No batting data was found for the requested criteria."
      )
    }

    lines <- c(
      "### 🏏 Top Run Scorers",
      "",
      "| Batter | Runs | Innings | Average | Strike Rate | 4s | 6s |",
      "| :--- | ---: | ---: | ---: | ---: | ---: | ---: |"
    )

    for (i in seq_len(nrow(result))) {

      row <- result[i, ]

      lines <- c(
        lines,
        paste0(
          "| **",
          row$batter,
          "** | ",
          row$total_runs,
          " | ",
          row$innings,
          " | ",
          row$average,
          " | ",
          row$strike_rate,
          " | ",
          row$fours,
          " | ",
          row$sixes,
          " |"
        )
      )
    }

    return(
      paste(
        lines,
        collapse = "\n"
      )
    )
  }

  # ---------------------------------------------------------------
  # Top bowlers
  # ---------------------------------------------------------------

  if (intent == "top_bowlers") {

    if (
      is.null(result) ||
      nrow(result) == 0
    ) {

      return(
        "No bowling data was found for the requested criteria."
      )
    }

    lines <- c(
      "### 🎯 Top Wicket Takers",
      "",
      "| Bowler | Wickets | Economy | Average | Overs | Dot Balls |",
      "| :--- | ---: | ---: | ---: | ---: | ---: |"
    )

    for (i in seq_len(nrow(result))) {

      row <- result[i, ]

      lines <- c(
        lines,
        paste0(
          "| **",
          row$bowler,
          "** | ",
          row$wickets,
          " | ",
          row$economy,
          " | ",
          row$average,
          " | ",
          row$overs_bowled,
          " | ",
          row$dot_balls,
          " |"
        )
      )
    }

    return(
      paste(
        lines,
        collapse = "\n"
      )
    )
  }

  # ---------------------------------------------------------------
  # Player profile
  # ---------------------------------------------------------------

  if (intent == "player_profile") {

    if (
      is.null(result) ||
      nrow(result) == 0
    ) {

      return(
        "No player data was found."
      )
    }

    row <- result[1, ]

    return(
      paste0(
        "### 📊 Player Profile: **",
        row$player,
        "**\n\n",
        "- **Matches Played:** ",
        row$matches,
        "\n",
        "- **Innings Batted:** ",
        row$innings,
        "\n",
        "- **Total Runs:** ",
        row$total_runs,
        "\n",
        "- **Batting Average:** ",
        row$average,
        "\n",
        "- **Strike Rate:** ",
        row$strike_rate,
        "\n",
        "- **Fours:** ",
        row$fours,
        "\n",
        "- **Sixes:** ",
        row$sixes,
        "\n",
        "- **Fifties:** ",
        row$fifties,
        "\n",
        "- **Hundreds:** ",
        row$hundreds
      )
    )
  }

  # ---------------------------------------------------------------
  # Team summary
  # ---------------------------------------------------------------

  if (intent == "team_summary") {

    if (
      is.null(result) ||
      nrow(result) == 0
    ) {

      return(
        "No team data was found."
      )
    }

    lines <- c(
      "### 🏆 Team Performance",
      ""
    )

    for (i in seq_len(nrow(result))) {

      row <- result[i, ]

      lines <- c(
        lines,
        paste0(
          "- **",
          row$team,
          "** — ",
          row$wins,
          " wins in ",
          row$matches_played,
          " matches (",
          row$win_pct,
          "% win rate)"
        )
      )
    }

    return(
      paste(
        lines,
        collapse = "\n"
      )
    )
  }

  # ---------------------------------------------------------------
  # Venue statistics
  # ---------------------------------------------------------------

  if (intent == "venue_stats") {

    if (is.null(result)) {

      return(
        "No venue data was found."
      )
    }

    return(
      paste0(
        "### 🏟️ Venue Statistics\n\n",
        toString(result)
      )
    )
  }

  # ---------------------------------------------------------------
  # Head-to-head
  # ---------------------------------------------------------------

  if (intent == "head_to_head") {

    if (is.null(result)) {

      return(
        "No head-to-head data was found."
      )
    }

    return(
      paste0(
        "### ⚔️ Head-to-Head\n\n",
        "- **",
        result$team1,
        " wins:** ",
        result$team1_wins,
        "\n",
        "- **",
        result$team2,
        " wins:** ",
        result$team2_wins,
        "\n",
        "- **No result:** ",
        result$no_result,
        "\n",
        "- **Total matches:** ",
        result$total_matches
      )
    )
  }

  "No result available."
}

# ------------------------------------------------------------------
# Main CricLens query processor
# ------------------------------------------------------------------

process_criclens_query <- function(
  query,
  df
) {

  query <- safe_character(
    query
  )

  if (is.null(query)) {

    return(
      "Please enter a question."
    )
  }

  # ---------------------------------------------------------------
  # 1. Ask NIM to identify intent
  # ---------------------------------------------------------------

  parsed <- parse_query_intent(
    query,
    df
  )

  # ---------------------------------------------------------------
  # 2. Deterministic fallback if NIM parsing fails
  # ---------------------------------------------------------------

  if (is.null(parsed)) {

    parsed <- fallback_query_intent(
      query,
      df
    )
  }

  # ---------------------------------------------------------------
  # 3. Validate against actual dataset
  # ---------------------------------------------------------------

  validated <- validate_intent(
    parsed,
    df
  )

  if (is.null(validated)) {

    return(
      "I couldn't validate that query against the CricLens dataset."
    )
  }

  # ---------------------------------------------------------------
  # 4. Execute actual R analytics
  # ---------------------------------------------------------------

  analytics_result <- execute_r_analytics(
    validated,
    df
  )

  if (is.null(analytics_result)) {

    return(
      "I couldn't calculate a result from the CricLens dataset."
    )
  }

  # ---------------------------------------------------------------
  # 5. Handle unsupported questions
  # ---------------------------------------------------------------

  if (
    identical(
      analytics_result$intent,
      "unsupported"
    )
  ) {

    return(
      format_r_result(
        analytics_result
      )
    )
  }

  # ---------------------------------------------------------------
  # 6. Ask NIM to explain ONLY the validated R result
  # ---------------------------------------------------------------

  explanation <- explain_validated_result(
    query,
    analytics_result
  )

  # ---------------------------------------------------------------
  # 7. If NIM explanation succeeds, return it
  # ---------------------------------------------------------------

  if (
    !is.null(explanation) &&
    nzchar(trimws(explanation))
  ) {

    # Do not return NIM's API error/fallback notice as the
    # analytical answer.
    if (
      !str_detect(
        explanation,
        regex(
          "API key|NVIDIA_API_KEY|NIM request failed|NIM unavailable",
          ignore_case = TRUE
        )
      )
    ) {

      return(
        explanation
      )
    }
  }

  # ---------------------------------------------------------------
  # 8. Deterministic R-only fallback
  # ---------------------------------------------------------------

  format_r_result(
    analytics_result
  )
}
