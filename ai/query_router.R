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
# Utility helpers
# ------------------------------------------------------------------

safe_character <- function(x, default = NULL) {

  if (
    is.null(x) ||
      !is.atomic(x) ||
      length(x) == 0
  ) {
    return(default)
  }

  x <- as.character(x)

  if (length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) {
    return(default)
  }

  trimws(x[1])
}


safe_integer <- function(x, default = NULL) {

  if (
    is.null(x) ||
      !is.atomic(x) ||
      length(x) == 0
  ) {
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

report_nim_status <- function(status_callback, status, message) {
  if (is.function(status_callback)) {
    status_callback(status, message)
  }
}

call_nim_for_query <- function(prompt, system_prompt, status_callback = NULL) {
  nim_arguments <- list(
    prompt = prompt,
    system_prompt = system_prompt
  )
  nim_formals <- names(formals(call_nvidia_nim))
  if ("status_callback" %in% nim_formals || "..." %in% nim_formals) {
    nim_arguments$status_callback <- status_callback
  }
  do.call(call_nvidia_nim, nim_arguments)
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

  # Headings, paragraphs, lists, and simple pipe tables.
  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1]]

  output <- character(0)

  in_list <- FALSE
  is_table_row <- function(line) {
    startsWith(trimws(line), "|") && endsWith(trimws(line), "|")
  }
  table_cells <- function(line) {
    line <- trimws(line)
    line <- substring(line, 2, nchar(line) - 1)
    trimws(strsplit(line, "|", fixed = TRUE)[[1]])
  }
  is_table_separator <- function(cells) {
    length(cells) > 0 && all(grepl("^:?-{3,}:?$", cells))
  }

  line_index <- 1L
  while (line_index <= length(lines)) {
    trimmed <- trimws(lines[line_index])

    if (is_table_row(trimmed)) {
      if (in_list) {
        output <- c(output, "</ul>")
        in_list <- FALSE
      }

      rows <- list(table_cells(trimmed))
      line_index <- line_index + 1L
      if (line_index <= length(lines) && is_table_row(lines[line_index]) &&
          is_table_separator(table_cells(lines[line_index]))) {
        line_index <- line_index + 1L
      }

      while (line_index <= length(lines) && is_table_row(lines[line_index])) {
        cells <- table_cells(lines[line_index])
        if (!is_table_separator(cells)) {
          rows[[length(rows) + 1L]] <- cells
        }
        line_index <- line_index + 1L
      }

      header <- paste0("<th>", rows[[1]], "</th>", collapse = "")
      body <- if (length(rows) > 1L) {
        paste0(
          "<tr>",
          vapply(rows[-1], function(cells) {
            paste0("<td>", cells, "</td>", collapse = "")
          }, character(1)),
          "</tr>",
          collapse = ""
        )
      } else {
        ""
      }
      output <- c(
        output,
        paste0(
          "<table class=\"table table-sm\"><thead><tr>",
          header,
          "</tr></thead><tbody>",
          body,
          "</tbody></table>"
        )
      )
      next
    }

    if (str_detect(trimmed, "^[-*] ")) {
      if (!in_list) {
        output <- c(output, "<ul>")
        in_list <- TRUE
      }
      item <- str_remove(trimmed, "^[-*] ")
      output <- c(output, paste0("<li>", item, "</li>"))
    } else {
      if (in_list) {
        output <- c(output, "</ul>")
        in_list <- FALSE
      }
      if (nzchar(trimmed)) {
        heading_level <- if (startsWith(trimmed, "### ")) {
          5L
        } else if (startsWith(trimmed, "## ")) {
          4L
        } else if (startsWith(trimmed, "# ")) {
          3L
        } else {
          NULL
        }
        if (is.null(heading_level)) {
          output <- c(output, paste0("<p>", trimmed, "</p>"))
        } else {
          heading_text <- substring(trimmed, heading_level)
          output <- c(
            output,
            paste0("<h", heading_level, ">", heading_text, "</h", heading_level, ">")
          )
        }
      }
    }

    line_index <- line_index + 1L
  }

  if (in_list) {
    output <- c(output, "</ul>")
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


resolve_player_name <- function(
  requested,
  available
) {

  requested <- safe_character(
    requested
  )

  if (is.null(requested)) {
    return(NULL)
  }

  aliases <- get_player_aliases()
  alias_name <- unname(aliases[tolower(requested)])
  if (length(alias_name) > 0 && alias_name %in% available) {
    return(alias_name)
  }

  exact <- available[tolower(trimws(available)) == tolower(requested)]
  if (length(exact) == 1) exact else NULL
}

get_player_aliases <- function() {
  c(
    "virat kohli" = "V Kohli",
    "kohli" = "V Kohli",
    "rohit sharma" = "RG Sharma",
    "rohit" = "RG Sharma",
    "jasprit bumrah" = "JJ Bumrah",
    "bumrah" = "JJ Bumrah",
    "bhuvneshwar kumar" = "B Kumar",
    "bhuvi" = "B Kumar"
  )
}

player_alias_from_query <- function(
  query,
  available
) {
  matches <- extract_player_matches(query, available)
  if (length(matches) == 1) matches[1] else NULL
}

extract_player_matches <- function(
  query,
  available
) {
  query <- safe_character(query, default = "")
  if (!nzchar(query) || !length(available)) {
    return(character())
  }
  query_lower <- tolower(query)
  matches_phrase <- function(phrase) {
    phrase_pattern <- paste0(
      "\\b",
      str_replace_all(tolower(trimws(phrase)), "\\s+", "\\\\s+"),
      "\\b"
    )
    str_detect(query_lower, regex(phrase_pattern))
  }

  exact_hits <- available[vapply(
    available,
    matches_phrase,
    logical(1)
  )]
  aliases <- get_player_aliases()
  alias_hits <- names(aliases)[vapply(
    names(aliases),
    matches_phrase,
    logical(1)
  )]
  conflicting_aliases <- alias_hits[vapply(
    alias_hits,
    function(alias) {
      any(vapply(
        exact_hits,
        function(player) {
          alias_pattern <- paste0(
            "\\b",
            str_replace_all(tolower(alias), "\\s+", "\\\\s+"),
            "\\b"
          )
          str_detect(tolower(player), regex(alias_pattern)) &&
            !identical(unname(aliases[alias]), player)
        },
        logical(1)
      ))
    },
    logical(1)
  )]
  alias_players <- unname(aliases[alias_hits])
  alias_players <- alias_players[
    !(alias_hits %in% conflicting_aliases)
  ]
  alias_players <- alias_players[alias_players %in% available]
  unique(c(exact_hits, alias_players))
}


resolve_team_name_from_query <- function(
  query,
  available
) {

  query <- safe_character(query, default = "")
  query_lower <- tolower(trimws(query))
  if (!length(available)) {
    return(NULL)
  }

  available_lower <- tolower(
    trimws(available)
  )

  team_aliases <- c(
    "csk" = "Chennai Super Kings",
    "mi" = "Mumbai Indians",
    "rcb" = "Royal Challengers Bengaluru",
    "kkr" = "Kolkata Knight Riders",
    "rr" = "Rajasthan Royals",
    "srh" = "Sunrisers Hyderabad",
    "dc" = "Delhi Capitals",
    "pbks" = "Punjab Kings",
    "gt" = "Gujarat Titans",
    "lsg" = "Lucknow Super Giants"
  )
  query_words <- str_split(query_lower, "[^[:alnum:]]+")[[1]]
  alias_matches <- names(team_aliases)[names(team_aliases) %in% query_words]
  alias_teams <- unname(team_aliases[alias_matches])
  alias_teams <- alias_teams[
    tolower(alias_teams) %in% available_lower
  ]
  if (length(alias_teams) == 1) {
    return(available[match(tolower(alias_teams), available_lower)])
  }

  matches <- which(
    vapply(
      available_lower,
      function(team) {
        str_detect(
          query_lower,
          fixed(team)
        )
      },
      logical(1)
    )
  )

  if (length(matches) == 0) {
    return(NULL)
  }

  match_lengths <- nchar(
    available_lower[matches]
  )
  longest_matches <- matches[
    match_lengths == max(match_lengths)
  ]

  if (length(longest_matches) != 1) {
    return(NULL)
  }

  available[longest_matches]
}

extract_team_matches <- function(
  query,
  available
) {
  query <- safe_character(query, default = "")
  query_lower <- tolower(trimws(query))
  full_matches <- available[
    vapply(
      available,
      function(team) {
        str_detect(query_lower, fixed(tolower(team)))
      },
      logical(1)
    )
  ]

  aliases <- c(
    "csk" = "Chennai Super Kings",
    "mi" = "Mumbai Indians",
    "rcb" = "Royal Challengers Bengaluru",
    "kkr" = "Kolkata Knight Riders",
    "rr" = "Rajasthan Royals",
    "srh" = "Sunrisers Hyderabad",
    "dc" = "Delhi Capitals",
    "pbks" = "Punjab Kings",
    "gt" = "Gujarat Titans",
    "lsg" = "Lucknow Super Giants"
  )
  query_words <- str_split(query_lower, "[^[:alnum:]]+")[[1]]
  alias_matches <- unname(aliases[names(aliases) %in% query_words])
  alias_matches <- alias_matches[alias_matches %in% available]
  unique(c(alias_matches, full_matches))
}


is_all_teams_summary_query <- function(query) {

  query_lower <- tolower(
    trimws(query)
  )
  patterns <- c(
    "\\b(all|every)\\s+(the\\s+)?teams?\\b",
    "\\bleague[- ]wide\\b",
    "\\b(entire|whole)\\s+league\\b",
    "\\bacross\\s+the\\s+league\\b"
  )

  any(
    vapply(
      patterns,
      function(pattern) {
        str_detect(
          query_lower,
          regex(pattern)
        )
      },
      logical(1)
    )
  )
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

  text <- safe_character(text)
  if (is.null(text)) {
    return(NULL)
  }

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
  df,
  status_callback = NULL
) {

  query <- safe_character(query)
  if (is.null(query)) {
    return(NULL)
  }

  prompt <- paste0(
    INTENT_PARSER_SYSTEM_PROMPT,
    "\n\nUSER QUERY:\n",
    query
  )

  response <- call_nim_for_query(
    prompt = prompt,
    system_prompt = INTENT_PARSER_SYSTEM_PROMPT,
    status_callback = status_callback
  )

  parsed <- extract_json_object(
    response
  )

  if (is.null(parsed)) {
    if (!is.null(response)) {
      report_nim_status(
        status_callback,
        "degraded",
        "NVIDIA NIM returned an unusable intent. The query is being interpreted with deterministic R rules."
      )
    }
    return(NULL)
  }

  if (!is.list(parsed) || is.null(names(parsed))) {
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
    "player_bowling_profile",
    "player_comparison",
    "player_team_history",
    "team_summary",
    "team_trend",
    "venue_stats",
    "head_to_head",
    "unsupported"
  )

  if (!(intent %in% supported_intents)) {
    return(NULL)
  }

  parameters <- parsed$parameters

  if (!is.list(parameters)) {
    parameters <- list()
  }

  if (intent %in% c(
    "player_profile",
    "player_bowling_profile",
    "player_team_history"
  )) {
    player_alias <- player_alias_from_query(
      query,
      get_dataset_players(df)
    )

    if (!is.null(player_alias)) {
      parameters$player_name <- player_alias
    }
  }

  if (intent == "player_comparison") {
    matched_players <- extract_player_matches(
      query,
      get_dataset_players(df)
    )
    if (length(matched_players) >= 2) {
      parameters$player1 <- matched_players[1]
      parameters$player2 <- matched_players[2]
    }
  }

  if (intent == "team_summary") {
    if (is_all_teams_summary_query(query)) {
      parameters$team_name <- NULL
    } else {
      query_team <- resolve_team_name_from_query(
        query,
        get_dataset_teams(df)
      )

      if (!is.null(query_team)) {
        parameters$team_name <- query_team
      }
    }
  }

  if (intent %in% c("head_to_head", "team_trend")) {
    query_teams <- extract_team_matches(query, get_dataset_teams(df))
    if (intent == "head_to_head" && length(query_teams) >= 2) {
      parameters$team1 <- query_teams[1]
      parameters$team2 <- query_teams[2]
    }
    if (intent == "team_trend" && length(query_teams) >= 1) {
      parameters$team_name <- query_teams[1]
    }
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

  paging_match <- str_match(
    tolower(query),
    "\\b(?:top|next)\\s+([0-9]+|one|two|three|four|five|six|seven|eight|nine|ten)\\b"
  )
  if (!is.na(paging_match[1, 2])) {
    count_words <- c(
      one = 1L, two = 2L, three = 3L, four = 4L, five = 5L,
      six = 6L, seven = 7L, eight = 8L, nine = 9L, ten = 10L
    )
    count_text <- paging_match[1, 2]
    parameters$top_n <- if (count_text %in% names(count_words)) {
      unname(count_words[count_text])
    } else {
      safe_integer(count_text, default = 5L)
    }
  }
  offset_match <- str_match(tolower(query), "\\boffset\\s+([0-9]+)\\b")
  if (!is.na(offset_match[1, 2])) {
    parameters$offset <- safe_integer(offset_match[1, 2], default = 0L)
  }

  query_lower <- tolower(query)
  history_phrase <- str_detect(
    query_lower,
    regex("\\b(played for|teams? did .* represent|represented|teams? by season)\\b")
  )
  comparison_phrase <- str_detect(
    query_lower,
    regex("\\b(compare|versus|vs|against|difference|better than|worse than|outperform)\\b")
  )
  player_question_phrase <- str_detect(
    query_lower,
    regex("\\b(stats?|statistics|profile|career|performance|how many|played|represent)\\b")
  )
  if (history_phrase || comparison_phrase || player_question_phrase) {
    deterministic_intent <- fallback_query_intent(query, df)
    if (
      deterministic_intent$intent %in%
        c(
          "player_profile",
          "player_bowling_profile",
          "player_comparison",
          "player_team_history"
        ) ||
        (
          deterministic_intent$intent == "unsupported" &&
            (history_phrase || comparison_phrase || player_question_phrase)
        )
    ) {
      return(deterministic_intent)
    }
  }

  list(
    intent = intent,
    parameters = parameters
  )
}

# ------------------------------------------------------------------
# Deterministic fallback intent parser
# ------------------------------------------------------------------

extract_requested_count <- function(query, default = 5L) {
  count_match <- str_match(
    tolower(query),
    "\\b(?:top|next)\\s+([0-9]+|one|two|three|four|five|six|seven|eight|nine|ten)\\b"
  )
  if (is.na(count_match[1, 2])) {
    return(as.integer(default))
  }

  count_words <- c(
    one = 1L, two = 2L, three = 3L, four = 4L, five = 5L,
    six = 6L, seven = 7L, eight = 8L, nine = 9L, ten = 10L
  )
  value <- count_match[1, 2]
  if (value %in% names(count_words)) {
    return(unname(count_words[value]))
  }

  safe_integer(value, default = default)
}

fallback_query_intent <- function(
  query,
  df
) {

  query <- safe_character(query)
  if (is.null(query)) {
    return(
      list(
        intent = "unsupported",
        parameters = list()
      )
    )
  }

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

  if (
    str_detect(q_lower, regex("\\b(explain|meaning|means|what does)\\b")) &&
      str_detect(q_lower, regex("\\b(win|winning) probability\\b"))
  ) {
    return(list(
      intent = "unsupported",
      parameters = list(
        reason = paste(
          "CricLens's win probability is the saved model's estimate that the",
          "team chasing the target will win from the current match state.",
          "It uses runs required, balls and wickets remaining, required run",
          "rate, and current run rate; the defending team's probability is",
          "the complement of the chasing team's estimate. It is an estimate,",
          "not a guarantee."
        )
      )
    ))
  }

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

  matched_teams <- extract_team_matches(query, teams)
  matched_players <- extract_player_matches(query, players)
  compare_requested <- str_detect(
    q_lower,
    regex("\\b(compare|versus|vs|against|difference|better than|worse than|outperform)\\b")
  )
  player_history_requested <- str_detect(
    q_lower,
    regex("\\b(played for|teams? did .* represent|represented|teams? by season)\\b")
  )
  wicket_requested <- str_detect(
    q_lower,
    regex("\\b(wickets?|bowling|bowler|dismissals?)\\b")
  )
  runs_requested <- str_detect(
    q_lower,
    regex("\\b(runs?|batting|batters?|batsmen|scorers?)\\b")
  )

  if (player_history_requested) {
    if (length(matched_players) == 1) {
      return(list(
        intent = "player_team_history",
        parameters = list(
          player_name = matched_players[1],
          season = query_season
        )
      ))
    }

    return(list(
      intent = "unsupported",
      parameters = list(
        reason = "Which IPL player do you mean? I couldn't confidently resolve the name to a player in the dataset."
      )
    ))
  }

  if (compare_requested && length(matched_players) >= 2) {
    return(list(
      intent = "player_comparison",
      parameters = list(
        player1 = matched_players[1],
        player2 = matched_players[2],
        season = query_season
      )
    ))
  }

  if (
    length(matched_teams) >= 2 &&
    compare_requested
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

  if (
    compare_requested &&
      length(matched_players) < 2 &&
      length(matched_teams) < 2
  ) {
    clarification <- if (length(matched_players) == 1) {
      paste0(
        "I identified ",
        matched_players[1],
        " but couldn't resolve the other player. Which player should I compare them with?"
      )
    } else {
      "I couldn't resolve both sides as IPL players or teams. Please provide both names so I can compare them."
    }
    return(list(
      intent = "unsupported",
      parameters = list(
        reason = clarification
      )
    ))
  }

  individual_player_question <- length(matched_players) == 1 &&
    str_detect(
      q_lower,
      regex("\\b(stats?|statistics|profile|career|performance|how many|did|has|have|show|tell)\\b")
    )
  if (individual_player_question && wicket_requested) {
    return(
      list(
        intent = "player_bowling_profile",
        parameters = list(
          player_name = matched_players[1],
          season = query_season
        )
      )
    )
  }
  if (individual_player_question) {
    return(
      list(
        intent = "player_profile",
        parameters = list(
          player_name = matched_players[1],
          season = query_season
        )
      )
    )
  }

  aliased_player <- player_alias_from_query(
    query,
    players
  )

  if (!is.null(aliased_player)) {
    return(
      list(
        intent = "player_profile",
        parameters = list(
          player_name = aliased_player,
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
  query_words <- str_split(
  q_lower,
  "[^[:alnum:]]+"
  )[[1]]

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
      meaningful_parts %in% query_words
    ]

    if (length(matching_parts) > 0) {

      player_candidates[[length(player_candidates) + 1]] <- list(
        player = player,
        matches = matching_parts
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
          top_n = extract_requested_count(query),
          metric = "wickets",
          offset = safe_integer(
            str_match(q_lower, "\\boffset\\s+([0-9]+)\\b")[1, 2],
            default = 0L
          )
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

    requested_top_n <- extract_requested_count(query)

    return(
      list(
        intent = "top_batters",
        parameters = list(
          season = query_season,
          top_n = requested_top_n,
          metric = "runs",
          offset = safe_integer(
            str_match(q_lower, "\\boffset\\s+([0-9]+)\\b")[1, 2],
            default = 0L
          )
        )
      )
    )
  }

  if (
    str_detect(
      q_lower,
      regex("\\b(across|over)\\b.*\\b(seasons?|years?)\\b|\\b(seasonal|trend)\\b")
    )
  ) {
    return(
      list(
        intent = "team_trend",
        parameters = list(
          team_name = resolve_team_name_from_query(query, teams),
          season = query_season
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
          team_name = if (is_all_teams_summary_query(query)) {
            NULL
          } else {
            resolve_team_name_from_query(
              query,
              teams
            )
          },
          season = query_season,
          metric = if (
            str_detect(
              q_lower,
              regex("\\b(win(ning)?\\s*(percentage|rate)|best win|highest win)\\b")
            )
          ) "win_pct" else "wins"
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

  supported_intents <- c(
    "top_batters",
    "top_bowlers",
    "player_profile",
    "player_bowling_profile",
    "player_comparison",
    "player_team_history",
    "team_summary",
    "team_trend",
    "venue_stats",
    "head_to_head",
    "unsupported"
  )
  if (!is.list(parsed)) {
    return(list(intent = "unsupported", parameters = list()))
  }

  intent <- safe_character(
    parsed$intent
  )
  if (is.null(intent) || !intent %in% supported_intents) {
    return(
      list(
        intent = "unsupported",
        parameters = list(
          reason = "I couldn't identify a supported IPL analysis for that question."
        )
      )
    )
  }

  parameters <- parsed$parameters

  if (!is.list(parameters)) {
    parameters <- list()
  }

  if (intent == "unsupported") {

    return(
      list(
        intent = "unsupported",
        parameters = list(
          reason = safe_character(
            parameters$reason,
            default = "I can't answer that from the available CricLens IPL dataset."
          )
        )
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

  if (intent %in% c(
    "player_profile",
    "player_bowling_profile",
    "player_team_history"
  )) {

    player_name <- resolve_player_name(
      parameters$player_name,
      get_dataset_players(df)
    )

    if (is.null(player_name)) {

      return(
        list(
          intent = "unsupported",
          parameters = list(
            reason = "Which IPL player do you mean? I couldn't confidently resolve that name to a player in the dataset."
          )
        )
      )
    }
  }

  player1 <- NULL
  player2 <- NULL
  if (intent == "player_comparison") {
    available_players <- get_dataset_players(df)
    player1 <- resolve_player_name(parameters$player1, available_players)
    player2 <- resolve_player_name(parameters$player2, available_players)
    if (is.null(player1) || is.null(player2) || identical(player1, player2)) {
      return(
        list(
          intent = "unsupported",
          parameters = list(
            reason = "Please name two different IPL players to compare their batting statistics."
          )
        )
      )
    }
  }

  # ---------------------------------------------------------------
  # Team
  # ---------------------------------------------------------------

  team_name <- NULL

  if (
    intent %in% c("team_summary", "team_trend") &&
    !is.null(parameters$team_name)
  ) {

    team_name <- resolve_dataset_value(
      parameters$team_name,
      get_dataset_teams(df)
    )

    if (is.null(team_name)) {
      return(
        list(
          intent = "unsupported",
          parameters = list()
        )
      )
    }
  }

  if (intent == "team_trend" && is.null(team_name)) {
    return(
      list(
        intent = "unsupported",
        parameters = list(
          reason = "Please name the IPL team whose season-by-season performance you want to compare."
        )
      )
    )
  }

  metric <- safe_character(parameters$metric)
  if (intent == "team_summary" && !is.null(metric) &&
      !metric %in% c("wins", "win_pct")) {
    metric <- "wins"
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
        metric
      ),
      player_name = player_name,
      player1 = player1,
      player2 = player2,
      team_name = team_name,
      venue_name = venue_name,
      team1 = team1,
      team2 = team2,
      offset = max(0, safe_integer(parameters$offset, default = 0))
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
        result = NULL,
        reason = safe_character(
          p$reason,
          default = "I can't answer that from the available CricLens IPL dataset."
        )
      )
    )
  }

  # ---------------------------------------------------------------
  # Top batters
  # ---------------------------------------------------------------

  if (intent == "top_batters") {

    result <- get_top_batters(
      df,
      top_n = p$top_n + p$offset,
      season_filter = season
    )

    if (p$offset > 0 && nrow(result) > p$offset) {
      last_row <- min(nrow(result), p$offset + p$top_n)
      result <- result[seq.int(p$offset + 1, last_row), , drop = FALSE]
    }

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
      top_n = p$top_n + p$offset,
      season_filter = season
    )

    if (p$offset > 0 && nrow(result) > p$offset) {
      last_row <- min(nrow(result), p$offset + p$top_n)
      result <- result[seq.int(p$offset + 1, last_row), , drop = FALSE]
    }

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

    player_df <- df
    if (!is.null(season)) {
      player_df <- player_df %>%
        filter(.data$season_year == as.numeric(.env$season))
    }
    result <- get_player_batting_profile(
      player_df,
      p$player_name
    )

    return(
      list(
        intent = intent,
        result = result
      )
    )
  }

  if (intent == "player_bowling_profile") {
    bowling_df <- df
    if (!is.null(season)) {
      bowling_df <- bowling_df %>%
        filter(.data$season_year == as.numeric(.env$season))
    }
    result <- get_top_bowlers(
      bowling_df,
      top_n = dplyr::n_distinct(bowling_df$bowler)
    ) %>%
      filter(.data$bowler == p$player_name)

    return(list(intent = intent, result = result))
  }

  if (intent == "player_team_history") {
    required_columns <- c(
      "season_year", "batting_team", "bowling_team",
      "batter", "non_striker", "player_out", "bowler"
    )
    if (!all(required_columns %in% names(df))) {
      return(list(
        intent = intent,
        result = NULL,
        reason = "The available dataset does not include the player and team fields needed to establish team history."
      ))
    }

    batting_history <- df %>%
      filter(
        .data$batter == .env$p$player_name |
          .data$non_striker == .env$p$player_name |
          .data$player_out == .env$p$player_name
      ) %>%
      transmute(
        season = .data$season_year,
        team = .data$batting_team
      )
    bowling_history <- df %>%
      filter(.data$bowler == .env$p$player_name) %>%
      transmute(
        season = .data$season_year,
        team = .data$bowling_team
      )
    result <- bind_rows(batting_history, bowling_history) %>%
      filter(
        !is.na(.data$season),
        !is.na(.data$team),
        nzchar(.data$team)
      ) %>%
      distinct(.data$season, .data$team) %>%
      arrange(.data$season, .data$team)

    if (!is.null(season)) {
      result <- result %>%
        filter(.data$season == as.numeric(.env$season))
    }

    return(list(
      intent = intent,
      result = result,
      player_name = p$player_name
    ))
  }

  if (intent == "player_comparison") {
    comparison_df <- df
    if (!is.null(season)) {
      comparison_df <- comparison_df %>%
        filter(.data$season_year == as.numeric(.env$season))
    }
    result <- bind_rows(
      get_player_batting_profile(comparison_df, p$player1),
      get_player_batting_profile(comparison_df, p$player2)
    )

    return(list(intent = intent, result = result))
  }

  # ---------------------------------------------------------------
  # Team summary
  # ---------------------------------------------------------------

  if (intent == "team_summary") {

    result <- get_team_performance_summary(
      df,
      season_filter = season
    )

    if (identical(p$metric, "win_pct")) {
      result <- result %>%
        arrange(desc(.data$win_pct), desc(.data$wins))
      if (is.null(p$team_name)) {
        result <- result %>% slice_head(n = 1)
      }
    }

    if (!is.null(p$team_name)) {

      result <- result %>%
        filter(
          .data$team == p$team_name
        )
    }

    return(
      list(
        intent = intent,
        result = result,
        metric = p$metric,
        ranking = identical(p$metric, "win_pct") &&
          is.null(p$team_name)
      )
    )
  }

  if (intent == "team_trend") {
    available_seasons <- sort(unique(
      as.integer(df$season_year[!is.na(df$season_year)])
    ))
    result <- lapply(
      available_seasons,
      function(year) {
        get_team_performance_summary(
          df,
          season_filter = as.character(year)
        ) %>%
          filter(.data$team == p$team_name) %>%
          mutate(season = year)
      }
    ) %>%
      bind_rows() %>%
        select(season, dplyr::everything())

    return(list(intent = intent, result = result))
  }

  # ---------------------------------------------------------------
  # Venue statistics
  # ---------------------------------------------------------------

  if (intent == "venue_stats") {

    venue_df <- df
    if (!is.null(season)) {
      venue_df <- venue_df %>%
        filter(.data$season_year == as.numeric(season))
    }
    result <- get_venue_profile(
      venue_df,
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

    head_to_head_df <- df
    if (!is.null(season)) {
      head_to_head_df <- head_to_head_df %>%
        filter(.data$season_year == as.numeric(season))
    }
    result <- get_head_to_head(
      head_to_head_df,
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

explanation_numeric_values_are_supported <- function(
  explanation,
  query,
  analytics_result
) {
  extract_numbers <- function(text) {
    matches <- unlist(stringr::str_extract_all(
      text,
      "(?<![[:alnum:]_])[-+]?(?:[0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?|\\.[0-9]+)%?"
    ))
    suppressWarnings(as.numeric(gsub("[,+%]", "", matches)))
  }

  explanation_numbers <- extract_numbers(explanation)
  if (!length(explanation_numbers)) {
    return(TRUE)
  }

  verified_numbers <- extract_numbers(paste(
    query,
    prepare_explainer_payload(analytics_result$result)
  ))
  all(explanation_numbers %in% verified_numbers)
}

# ------------------------------------------------------------------
# Explain validated R result using NIM
# ------------------------------------------------------------------

explain_validated_result <- function(
  query,
  analytics_result,
  status_callback = NULL
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

  call_nim_for_query(
    prompt = prompt,
    system_prompt = RESULT_EXPLAINER_SYSTEM_PROMPT,
    status_callback = status_callback
  )
}

# ------------------------------------------------------------------
# Deterministic R result formatter
# ------------------------------------------------------------------

format_r_result <- function(
  analytics_result
) {

  if (
    is.null(analytics_result) ||
      !is.list(analytics_result)
  ) {

    return(
      "I couldn't process that query."
    )
  }

  intent <- safe_character(
    analytics_result$intent,
    default = ""
  )
  result <- analytics_result$result

  if (intent == "unsupported") {
    parameters <- analytics_result$parameters
    parameter_reason <- if (is.list(parameters)) {
      parameters$reason
    } else {
      NULL
    }
    reason <- safe_character(
      analytics_result$reason,
      default = safe_character(
        parameter_reason,
        default = "I can't answer that from the current CricLens dataset. I can answer IPL questions about run and wicket leaderboards, player profiles, team records and comparisons, season trends, and venue statistics."
      )
    )
    return(reason)
  }

  if (intent == "player_team_history") {
    if (is.null(result)) {
      return(safe_character(
        analytics_result$reason,
        default = "The available dataset does not include enough player and team information to establish team history."
      ))
    }
    if (
      !is.data.frame(result) ||
        !all(c("season", "team") %in% names(result))
    ) {
      return("The player team-history result could not be formatted.")
    }
    if (nrow(result) == 0) {
      return(paste0(
        "No team history for **",
        safe_character(analytics_result$player_name, "this player"),
        "** was found in the available IPL dataset."
      ))
    }
    rows <- vapply(
      seq_len(nrow(result)),
      function(i) {
        paste0("| ", result$season[i], " | ", result$team[i], " |")
      },
      character(1)
    )
    return(paste(c(
      paste0(
        "### 🏏 Teams Represented by **",
        safe_character(analytics_result$player_name, "this player"),
        "**"
      ),
      "",
      "| Season | Team |",
      "| ---: | :--- |",
      rows,
      "",
      "Based on the player and team records present in the CricLens IPL dataset."
    ), collapse = "\n"))
  }

  # ---------------------------------------------------------------
  # Top batters
  # ---------------------------------------------------------------

  if (intent == "top_batters") {

    if (
      !is.data.frame(result) ||
      nrow(result) == 0
    ) {

      return(
        "No batting data was found for the requested criteria."
      )
    }

    required_columns <- c(
      "batter",
      "total_runs",
      "innings",
      "average",
      "strike_rate",
      "fours",
      "sixes"
    )

    if (!all(required_columns %in% names(result))) {
      return("The batting result could not be formatted.")
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

  if (intent %in% c("top_bowlers", "player_bowling_profile")) {

    if (
      !is.data.frame(result) ||
      nrow(result) == 0
    ) {
      return(
        "No bowling data was found for the requested criteria."
      )
    }

    required_columns <- c(
      "bowler",
      "wickets",
      "matches",
      "innings",
      "overs_bowled",
      "runs_given",
      "economy",
      "average",
      "strike_rate"
    )

    if (!all(required_columns %in% names(result))) {
      return("The bowling result could not be formatted.")
    }

    lines <- c(
      if (intent == "player_bowling_profile") {
        paste0("### 🎯 Bowling Profile: **", result$bowler[1], "**")
      } else {
        "### 🏏 Top Wicket Takers"
      },
      "",
      paste0(
        "| Bowler | Wickets | Matches | Innings | Overs | Runs | Economy | ",
        "Average | Strike Rate |"
      ),
      "| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"
    )

    for (i in seq_len(nrow(result))) {

      row <- result[i, ]

      lines <- c(
        lines,
        paste0(
          "| **", row$bowler,
          "** | ", row$wickets,
          " | ", row$matches,
          " | ", row$innings,
          " | ", row$overs_bowled,
          " | ", row$runs_given,
          " | ", row$economy,
          " | ", row$average,
          " | ", row$strike_rate,
          " |"
        )
      )
    }

    return(
      paste(lines, collapse = "\n")
    )
  }

  # ---------------------------------------------------------------
  # Player profile
  # ---------------------------------------------------------------

  if (intent == "player_comparison") {
    required_columns <- c(
      "player", "matches", "innings", "total_runs",
      "average", "strike_rate", "fours", "sixes"
    )
    if (
      !is.data.frame(result) ||
        nrow(result) == 0 ||
        !all(required_columns %in% names(result))
    ) {
      return("I couldn't find batting records for both players to compare.")
    }

    rows <- vapply(
      seq_len(nrow(result)),
      function(i) {
        row <- result[i, ]
        paste0(
          "| **", row$player, "** | ",
          row$matches, " | ", row$innings, " | ",
          row$total_runs, " | ", row$average, " | ",
          row$strike_rate, " | ", row$fours, " | ", row$sixes, " |"
        )
      },
      character(1)
    )
    return(paste(c(
      "### 🏏 Player Batting Comparison",
      "",
      "| Player | Matches | Innings | Runs | Average | Strike Rate | 4s | 6s |",
      "| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |",
      rows
    ), collapse = "\n"))
  }

  if (intent == "player_profile") {

    if (
      !is.data.frame(result) ||
      nrow(result) == 0
    ) {

      return(
        "No player data was found."
      )
    }

    required_columns <- c(
      "player",
      "matches",
      "innings",
      "total_runs",
      "average",
      "strike_rate",
      "fours",
      "sixes",
      "fifties",
      "hundreds"
    )

    if (!all(required_columns %in% names(result))) {
      return("The player profile could not be formatted.")
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

  if (intent == "team_trend") {
    required_columns <- c(
      "season", "team", "wins", "matches_played", "win_pct"
    )
    if (
      !is.data.frame(result) ||
        nrow(result) == 0 ||
        !all(required_columns %in% names(result))
    ) {
      return("No season-by-season team data was found.")
    }
    rows <- vapply(
      seq_len(nrow(result)),
      function(i) {
        row <- result[i, ]
        paste0(
          "| ", row$season, " | ", row$wins, " | ",
          row$matches_played, " | ", row$win_pct, "% |"
        )
      },
      character(1)
    )
    return(paste(c(
      paste0("### 📈 ", result$team[1], " Season-by-Season Performance"),
      "",
      "| Season | Wins | Matches | Win Rate |",
      "| ---: | ---: | ---: | ---: |",
      rows
    ), collapse = "\n"))
  }

  if (intent == "team_summary") {

    if (
      !is.data.frame(result) ||
      nrow(result) == 0
    ) {

      return(
        "No team data was found."
      )
    }

    required_columns <- c(
      "team",
      "wins",
      "matches_played",
      "win_pct"
    )

    if (!all(required_columns %in% names(result))) {
      return("The team summary could not be formatted.")
    }

    lines <- c(
      if (isTRUE(analytics_result$ranking)) {
        "### 🏆 Highest Team Win Percentage"
      } else {
        "### 🏆 Team Performance"
      },
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

    if (
      is.null(result) ||
        !is.list(result) ||
        is.data.frame(result) ||
        length(result) == 0
    ) {

      return(
        "No venue data was found."
      )
    }

    required_fields <- c(
      "venue",
      "city",
      "total_matches",
      "avg_1st_inn",
      "highest_1st_inn",
      "lowest_1st_inn",
      "bat_1st_wins",
      "chase_wins",
      "chase_win_pct",
      "phase_stats"
    )

    if (!all(required_fields %in% names(result))) {
      return("The venue result could not be formatted.")
    }

    phase_stats <- result$phase_stats
    phase_columns <- c(
      "match_phase",
      "runs",
      "balls",
      "wickets",
      "run_rate"
    )

    if (
      !is.data.frame(phase_stats) ||
        (nrow(phase_stats) > 0 &&
           !all(phase_columns %in% names(phase_stats)))
    ) {
      return("The venue result could not be formatted.")
    }

    lines <- c(
      paste0(
        "### 🏟️ Venue Statistics: **",
        result$venue,
        "**"
      ),
      "",
      paste0("- **City:** ", result$city),
      paste0("- **Matches:** ", result$total_matches),
      paste0("- **Average first-innings score:** ", result$avg_1st_inn),
      paste0("- **Highest first-innings score:** ", result$highest_1st_inn),
      paste0("- **Lowest first-innings score:** ", result$lowest_1st_inn),
      paste0("- **Wins batting first:** ", result$bat_1st_wins),
      paste0(
        "- **Wins chasing:** ",
        result$chase_wins,
        " (",
        result$chase_win_pct,
        "%)"
      ),
      "",
      "#### Phase Statistics",
      ""
    )

    if (nrow(phase_stats) == 0) {
      lines <- c(lines, "No phase statistics were found.")
    } else {
      lines <- c(
        lines,
        "| Phase | Runs | Balls | Wickets | Run Rate |",
        "| :--- | ---: | ---: | ---: | ---: |"
      )

      for (i in seq_len(nrow(phase_stats))) {
        row <- phase_stats[i, ]
        lines <- c(
          lines,
          paste0(
            "| ",
            row$match_phase,
            " | ",
            row$runs,
            " | ",
            row$balls,
            " | ",
            row$wickets,
            " | ",
            row$run_rate,
            " |"
          )
        )
      }
    }

    return(
      paste(
        lines,
        collapse = "\n"
      )
    )
  }

  # ---------------------------------------------------------------
  # Head-to-head
  # ---------------------------------------------------------------

  if (intent == "head_to_head") {

    if (
      is.null(result) ||
        !is.list(result) ||
        is.data.frame(result) ||
        length(result) == 0
    ) {

      return(
        "No head-to-head data was found."
      )
    }

    required_fields <- c(
      "team1",
      "team1_wins",
      "team2",
      "team2_wins",
      "no_result",
      "total_matches"
    )

    if (!all(required_fields %in% names(result))) {
      return("The head-to-head result could not be formatted.")
    }

    if (
      length(result$total_matches) != 1 ||
        is.null(result$total_matches) ||
        is.na(result$total_matches) ||
        result$total_matches == 0
    ) {
      return("No head-to-head data was found.")
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

resolve_criclens_followup <- function(
  query,
  previous_query,
  df
) {
  query <- safe_character(query)
  previous_query <- safe_character(previous_query)
  if (is.null(query) || is.null(previous_query)) {
    return(query)
  }

  query_lower <- tolower(query)
  previous_intent <- fallback_query_intent(previous_query, df)
  previous_parameters <- previous_intent$parameters

  season_match <- str_match(query_lower, "\\b(20[0-9]{2})\\b")
  if (
    !is.na(season_match[1, 2]) &&
      str_detect(query_lower, regex("\\b(what about|how about|and)\\b"))
  ) {
    if (str_detect(previous_query, "\\b20[0-9]{2}\\b")) {
      return(sub(
        "\\b20[0-9]{2}\\b",
        season_match[1, 2],
        previous_query
      ))
    }
    return(paste(previous_query, "in", season_match[1, 2]))
  }

  if (str_detect(query_lower, regex("\\b(next|more)\\b"))) {
    previous_type <- previous_intent$intent
    if (previous_type %in% c("top_batters", "top_bowlers")) {
      next_count <- extract_requested_count(query)
      offset <- safe_integer(previous_parameters$offset, 0L) +
        safe_integer(previous_parameters$top_n, 5L)
      category <- if (previous_type == "top_bowlers") {
        "wicket takers"
      } else {
        "run scorers"
      }
      season <- safe_character(previous_parameters$season)
      return(paste(
        "Show the next", next_count, category,
        if (!is.null(season)) paste("in IPL", season) else "in IPL",
        "offset", offset
      ))
    }
  }

  mentioned_teams <- extract_team_matches(query, get_dataset_teams(df))
  if (
    length(mentioned_teams) == 1 &&
      previous_intent$intent == "team_summary" &&
      str_detect(query_lower, regex("\\b(what about|how about|and)\\b"))
  ) {
    season <- safe_character(previous_parameters$season)
    return(paste(
      "Show the team performance summary for",
      mentioned_teams[1],
      if (!is.null(season)) paste("in IPL", season) else ""
    ))
  }

  mentioned_players <- extract_player_matches(query, get_dataset_players(df))
  previous_players <- extract_player_matches(
    previous_query,
    get_dataset_players(df)
  )
  if (
    str_detect(query_lower, regex("\\b(compare|that with|him with|her with)\\b")) &&
      length(mentioned_players) == 1 &&
      length(previous_players) == 1
  ) {
    return(paste(
      "Compare batting performances of",
      previous_players[1],
      "and",
      mentioned_players[1]
    ))
  }

  query
}

process_criclens_query <- function(
  query,
  df,
  status_callback = NULL
) {

  query <- safe_character(
    query
  )

  if (is.null(query)) {

    return(
      "Please enter a question."
    )
  }

  formatted_result <- tryCatch(
    {
      process_criclens_query_result(query, df, status_callback)
    },
    error = function(error) {
      message(
        "CricLens query processing failed: ",
        conditionMessage(error)
      )
      "I couldn't complete that analysis. Please check the question and try again."
    }
  )
  formatted_result
}

process_criclens_query_result <- function(query, df, status_callback = NULL) {

  # ---------------------------------------------------------------
  # 1. Ask NIM to identify intent
  # ---------------------------------------------------------------

  parsed <- parse_query_intent(
    query,
    df,
    status_callback
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

  deterministic_result <- format_r_result(analytics_result)
  explanation <- explain_validated_result(
    query,
    analytics_result,
    status_callback
  )

  # ---------------------------------------------------------------
  # 7. If NIM explanation succeeds, return it
  # ---------------------------------------------------------------

  if (
    is.character(explanation) &&
      length(explanation) == 1L &&
      !is.na(explanation) &&
      nzchar(trimws(explanation))
  ) {

    if (!explanation_numeric_values_are_supported(
      explanation,
      query,
      analytics_result
    )) {
      report_nim_status(
        status_callback,
        "degraded",
        "NVIDIA NIM returned numerical claims not present in the verified R result. The unverified explanation was omitted."
      )
      return(deterministic_result)
    }

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

      return(paste(
        deterministic_result,
        "\n\n#### Explanation\n\n",
        explanation
      ))
    }
  }

  # ---------------------------------------------------------------
  # 8. Deterministic R-only fallback
  # ---------------------------------------------------------------

  deterministic_result
}
