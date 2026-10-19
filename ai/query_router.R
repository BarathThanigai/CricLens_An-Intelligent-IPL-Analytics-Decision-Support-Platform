# ------------------------------------------------------------------
# CricLens - Natural Language Query Router
# ------------------------------------------------------------------
#
# Flow:
#
# User query
#     ↓
# Deterministic intent parsing / parameter extraction
#     ↓
# NVIDIA NIM intent parsing only when deterministic routing cannot resolve
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
    "\\b20[0-9]{2}\\b(?!-[0-9]{2}-[0-9]{2})"
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

query_constraint <- function(
  value = NULL,
  status = NULL,
  source = NULL,
  validation = NULL,
  raw = NULL,
  reason = NULL
) {
  if (length(value) == 0L) {
    value <- NULL
  }
  if (is.null(status)) {
    status <- if (is.null(value)) "not_requested" else "inferred"
  }
  if (is.null(source)) {
    source <- if (identical(status, "not_requested")) "none" else "model"
  }
  if (is.null(validation)) {
    validation <- if (status %in% c("explicit", "inferred")) {
      "pending"
    } else {
      "not_applicable"
    }
  }

  list(
    value = value,
    status = status,
    source = source,
    validation = validation,
    raw = raw,
    reason = reason
  )
}

query_parameter_values <- function(parameters, names) {
  values <- unlist(
    lapply(names, function(name) parameters[[name]]),
    use.names = FALSE
  )
  values <- as.character(values)
  values[!is.na(values) & nzchar(trimws(values))]
}

extract_query_venues <- function(query, df) {
  query <- safe_character(query, default = "")
  if (!nzchar(query)) {
    return(character())
  }

  available <- get_dataset_venues(df)
  query_lower <- tolower(query)
  query_words <- str_split(query_lower, "[^[:alnum:]]+")[[1]]
  entity_names <- c(
    extract_player_matches(query, get_dataset_players(df)),
    extract_team_matches(query, get_dataset_teams(df))
  )
  entity_words <- str_split(
    tolower(paste(entity_names, collapse = " ")),
    "[^[:alnum:]]+"
  )[[1]]
  query_words <- setdiff(query_words, entity_words)
  generic_venue_words <- c(
    "stadium", "cricket", "international", "ground", "stadia",
    "sports", "academy", "park", "oval", "arena"
  )

  explicit_full_matches <- available[vapply(available, function(venue) {
    str_detect(query_lower, fixed(tolower(venue)))
  }, logical(1))]
  if (length(explicit_full_matches)) {
    return(explicit_full_matches)
  }

  if ("city" %in% names(df) && "venue" %in% names(df)) {
    city_values <- unique(as.character(df$city[!is.na(df$city)]))
    mentioned_cities <- city_values[vapply(city_values, function(city) {
      city_words <- str_split(tolower(city), "[^[:alnum:]]+")[[1]]
      city_words <- city_words[nchar(city_words) >= 3]
      any(city_words %in% query_words)
    }, logical(1))]
    if (length(mentioned_cities)) {
      city_venues <- unique(as.character(
        df$venue[df$city %in% mentioned_cities]
      ))
      city_venues <- city_venues[
        !is.na(city_venues) & nzchar(city_venues)
      ]
      if (length(city_venues)) {
        return(city_venues)
      }
    }
  }

  if (!str_detect(
    query_lower,
    regex("\\b(at|venue|stadium|ground)\\b|\\bin\\s+(?!ipl\\b|20[0-9]{2}\\b)")
  )) {
    return(character())
  }

  available[vapply(available, function(venue) {
    venue_lower <- tolower(venue)
    venue_words <- str_split(venue_lower, "[^[:alnum:]]+")[[1]]
    venue_words <- venue_words[
      nchar(venue_words) >= 4 &
        !venue_words %in% generic_venue_words
    ]
    any(venue_words %in% query_words)
  }, logical(1))]
}

extract_query_innings <- function(query) {
  query_lower <- tolower(query)
  if (str_detect(query_lower, regex("\\bsuper[- ]overs?\\b"))) {
    return(list(value = "super_over", raw = "Super Over"))
  }

  innings_match <- str_match(
    query_lower,
    "\\binnings?\\s*(?:no\\.?\\s*)?([1-6])\\b|\\b([1-6])(st|nd|rd|th)?\\s+innings?\\b|\\b(first|second|third|fourth|fifth|sixth)\\s+innings?\\b"
  )
  if (!is.na(innings_match[1, 2])) {
    return(list(value = as.integer(innings_match[1, 2]), raw = innings_match[1]))
  }
  if (!is.na(innings_match[1, 3])) {
    return(list(value = as.integer(innings_match[1, 3]), raw = innings_match[1]))
  }
  if (!is.na(innings_match[1, 5])) {
    ordinal <- c(
      first = 1L,
      second = 2L,
      third = 3L,
      fourth = 4L,
      fifth = 5L,
      sixth = 6L
    )
    value <- unname(ordinal[innings_match[1, 5]])
    return(list(value = value, raw = innings_match[1]))
  }

  if (str_detect(query_lower, regex("\\binnings?\\b"))) {
    return(list(value = NULL, raw = "innings"))
  }
  NULL
}

new_query_representation <- function(
  parsed,
  query = NULL,
  df = NULL,
  intent_source = "model"
) {
  if (!is.list(parsed)) {
    parsed <- list()
  }
  parameters <- parsed$parameters
  if (!is.list(parameters)) {
    parameters <- list()
  }

  query <- safe_character(query, default = "")
  query_lower <- tolower(query)
  explicit_players <- explicit_teams <- explicit_venues <- character()
  explicit_years <- integer()

  if (!is.null(df) && nzchar(query)) {
    explicit_players <- extract_player_matches(query, get_dataset_players(df))
    explicit_teams <- extract_team_matches(query, get_dataset_teams(df))
    explicit_venues <- extract_query_venues(query, df)
    explicit_years <- as.integer(str_extract_all(
      query,
      "\\b20[0-9]{2}\\b(?!-[0-9]{2}-[0-9]{2})"
    )[[1]])
  }

  available_years <- if (!is.null(df) && "season_year" %in% names(df)) {
    unique(as.integer(df$season_year[!is.na(df$season_year)]))
  } else {
    integer()
  }
  invalid_years <- setdiff(explicit_years, available_years)

  parameter_players <- query_parameter_values(
    parameters,
    c("player1", "player2", "player_name")
  )
  parameter_teams <- query_parameter_values(
    parameters,
    c("team1", "team2", "team_name")
  )
  players <- if (length(explicit_players)) {
    explicit_players
  } else {
    unique(parameter_players)
  }
  teams <- if (length(explicit_teams)) {
    explicit_teams
  } else {
    unique(parameter_teams)
  }
  model_venue <- safe_character(parameters$venue_name)

  make_slot <- function(
    explicit_value = NULL,
    inferred_value = NULL,
    requested = FALSE,
    raw = NULL,
    unsupported_reason = NULL
  ) {
    if (!is.null(unsupported_reason)) {
      return(query_constraint(
        value = explicit_value,
        status = "unsupported",
        source = "user_query",
        raw = raw,
        reason = unsupported_reason
      ))
    }
    if (!is.null(explicit_value)) {
      return(query_constraint(
        value = explicit_value,
        status = "explicit",
        source = "user_query",
        raw = raw
      ))
    }
    if (requested) {
      return(query_constraint(
        value = NULL,
        status = "ambiguous",
        source = "user_query",
        raw = raw,
        reason = "The requested constraint could not be resolved unambiguously."
      ))
    }
    if (!is.null(inferred_value)) {
      return(query_constraint(
        value = inferred_value,
        status = "inferred",
        source = intent_source
      ))
    }
    query_constraint()
  }

  season_value <- if (length(explicit_years) == 1L &&
    !length(invalid_years)) {
    as.character(explicit_years)
  } else {
    NULL
  }
  all_seasons_requested <- str_detect(
    query_lower,
    regex("\\b(across|over)\\s+(all\\s+)?seasons?\\b|\\bseason[- ]by[- ]season\\b")
  )
  season_range_value <- if (length(explicit_years) >= 2L &&
    !length(invalid_years)) {
    as.character(explicit_years)
  } else if (
    all_seasons_requested &&
      length(available_years)
  ) {
    as.character(range(available_years))
  } else {
    NULL
  }
  relative_season_requested <- str_detect(
    query_lower,
    regex("\\b(this|current|last|previous|prior)\\s+season\\b")
  )
  season_requested <- length(explicit_years) == 1L ||
    (relative_season_requested && !all_seasons_requested)
  season_range_requested <- length(explicit_years) >= 2L ||
    all_seasons_requested
  season_reason <- if (length(invalid_years)) {
    paste(
      "The requested season is not present in the dataset:",
      paste(invalid_years, collapse = ", ")
    )
  } else if (relative_season_requested && !length(explicit_years)) {
    "Relative season wording cannot be resolved to a specific dataset season."
  } else {
    NULL
  }

  innings_match <- if (nzchar(query)) extract_query_innings(query) else NULL
  innings_requested <- !is.null(innings_match)
  innings_reason <- if (
    innings_requested && is.null(innings_match$value)
  ) {
    "An innings constraint was requested but its value could not be resolved."
  } else {
    NULL
  }

  date_tokens <- if (nzchar(query)) {
    str_extract_all(query, "\\b20[0-9]{2}-[0-9]{2}-[0-9]{2}\\b")[[1]]
  } else {
    character()
  }
  date_language_requested <- str_detect(
    query_lower,
    regex(
      "\\b(date|dates|period|last\\s+\\d+\\s+(days?|weeks?|months?|years?)|(?:from|between|since|before|after|during)\\s+(jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?))\\b"
    )
  )
  date_range_value <- NULL
  date_range_reason <- NULL
  if (length(date_tokens) >= 2L) {
    parsed_dates <- suppressWarnings(as.Date(date_tokens[1:2]))
    if (!anyNA(parsed_dates) && parsed_dates[1] <= parsed_dates[2]) {
      date_range_value <- as.character(parsed_dates)
    } else {
      date_range_reason <- "The requested date range is invalid."
    }
  } else if (
    length(date_tokens) > 0L ||
      date_language_requested
  ) {
    date_range_reason <- "The requested date range could not be parsed as a complete start and end date."
  }

  phase_aliases <- c(
    powerplay = "Powerplay",
    "middle overs" = "Middle Overs",
    "death overs" = "Death Overs",
    "super over" = "Super Over"
  )
  explicit_phases <- names(phase_aliases)[vapply(
    names(phase_aliases),
    function(phase) str_detect(query_lower, fixed(phase)),
    logical(1)
  )]
  phase_value <- if (length(explicit_phases)) {
    unname(phase_aliases[explicit_phases])
  } else {
    NULL
  }
  phase_requested <- str_detect(
    query_lower,
    regex("\\b(match\\s+)?phases?\\b")
  )

  bowling_style_match <- str_match(
    query_lower,
    "\\b(left[- ]arm(?:ed)?\\s+(?:orthodox|wrist\\s+spin|spin|spinner|spinners)|right[- ]arm(?:ed)?\\s+(?:orthodox|wrist\\s+spin|spin|spinner|spinners)|(?:off[- ]spin|leg[- ]spin|left[- ]arm\\s+spin|right[- ]arm\\s+spin)(?:ners?)?)\\b"
  )
  bowling_style_requested <- !is.na(bowling_style_match[1, 1])

  metrics <- c(
    runs = "\\b(runs?|scoring)\\b",
    wickets = "\\bwickets?\\b",
    average = "\\b(averages?|mean)\\b",
    strike_rate = "\\bstrike\\s+rates?\\b",
    economy = "\\beconomy\\b",
    boundaries = "\\b(boundaries|fours|sixes)\\b",
    wins = "\\bwins?\\b"
  )
  metrics <- c(
    metrics,
    win_pct = "\\bwin\\s*(percentage|percent|rate|pct)\\b"
  )
  requested_metrics <- names(metrics)[vapply(
    metrics,
    function(pattern) str_detect(query_lower, regex(pattern)),
    logical(1)
  )]
  unsupported_metrics <- str_extract_all(
    query_lower,
    "\\b(dot[- ]balls?|maiden overs?|catches|fielding|partnerships?|run rates?)\\b"
  )[[1]]

  aggregation <- if (str_detect(query_lower, regex("\\b(average|mean)\\b"))) {
    "average"
  } else if (str_detect(query_lower, regex("\\b(total|most|highest|top)\\b"))) {
    "total_or_ranked"
  } else {
    NULL
  }

  comparison_options <- character()
  if (str_detect(query_lower, regex("\\b(batting|batting performance)\\b"))) {
    comparison_options <- c(comparison_options, "batting")
  }
  if (str_detect(query_lower, regex("\\b(bowling|bowling performance)\\b"))) {
    comparison_options <- c(comparison_options, "bowling")
  }

  paired_comparison_requested <- str_detect(
    query_lower,
    regex("\\b(compare|versus|vs|difference|better than|worse than|outperform)\\b")
  )
  against_two_entities <- str_detect(query_lower, regex("\\bagainst\\b")) &&
    (
      length(explicit_players) >= 2L ||
        length(explicit_teams) >= 2L
    )
  comparison_requested <- paired_comparison_requested || against_two_entities
  players_requested <- length(players) > 0L ||
    str_detect(
      query_lower,
      regex("\\b(?:stats?|statistics|profile|career)\\s+(?:for|of|about)\\s+(?:the\\s+)?(?:player|batter|bowler)\\b|\\b(?:player|batter|bowler)\\s+(?:named|called)\\b")
    ) ||
    (comparison_requested && length(explicit_teams) < 2L)
  teams_requested <- length(teams) > 0L ||
    str_detect(
      query_lower,
      regex("\\b(?:team|franchise)\\s+(?:named|called)\\b|\\b(?:stats?|performance|record)\\s+(?:for|of)\\s+(?:the\\s+)?(?:team|franchise)\\b")
    ) ||
    (comparison_requested && length(explicit_players) < 2L)
  venue_requested <- str_detect(
    query_lower,
    regex("\\b(at|venue|stadium|ground)\\b")
  ) ||
    (
      str_detect(query_lower, regex("\\bin\\s+(?!ipl\\b|20[0-9]{2}\\b)")) &&
        (length(explicit_teams) == 0L || length(explicit_venues) > 0L)
    )

  players_slot <- make_slot(
    explicit_value = if (length(explicit_players)) explicit_players else NULL,
    inferred_value = if (length(players)) players else NULL,
    requested = players_requested && length(players) == 0L,
    raw = query
  )
  teams_slot <- make_slot(
    explicit_value = if (length(explicit_teams)) explicit_teams else NULL,
    inferred_value = if (length(teams)) teams else NULL,
    requested = teams_requested && length(teams) == 0L,
    raw = query
  )
  if (
    comparison_requested &&
      length(explicit_players) == 1L &&
      length(explicit_teams) < 2L
  ) {
    players_slot <- query_constraint(
      value = explicit_players,
      status = "ambiguous",
      source = "user_query",
      raw = query,
      reason = "Only one player could be resolved from the requested comparison."
    )
  } else if (
    comparison_requested &&
      length(explicit_teams) == 1L &&
      length(explicit_players) < 2L
  ) {
    teams_slot <- query_constraint(
      value = explicit_teams,
      status = "ambiguous",
      source = "user_query",
      raw = query,
      reason = "Only one team could be resolved from the requested comparison."
    )
  }

  venue_slot <- if (length(explicit_venues) > 1L) {
    query_constraint(
      value = explicit_venues,
      status = "ambiguous",
      source = "user_query",
      raw = query,
      reason = "The query mentions multiple possible venues for a single-venue filter."
    )
  } else if (
    venue_requested &&
      !length(explicit_venues) &&
      is.null(model_venue)
  ) {
    query_constraint(
      value = NULL,
      status = "ambiguous",
      source = "user_query",
      raw = query,
      reason = "The requested venue could not be matched to an available dataset venue. Please name one specific venue."
    )
  } else {
    make_slot(
      explicit_value = if (length(explicit_venues)) explicit_venues else NULL,
      inferred_value = model_venue,
      requested = venue_requested && length(explicit_venues) == 0L,
      raw = query
    )
  }

  list(
    schema_version = 1L,
    intent = query_constraint(
      value = safe_character(parsed$intent),
      status = if (is.null(parsed$intent)) "not_requested" else {
        if (identical(intent_source, "user_query")) "explicit" else "inferred"
      },
      source = intent_source
    ),
    players = players_slot,
    teams = teams_slot,
    venue = venue_slot,
    season = make_slot(
      explicit_value = season_value,
      inferred_value = if (!season_requested && !season_range_requested) {
        if (length(date_tokens) || date_language_requested) NULL else {
          parameters$season
        }
      } else {
        NULL
      },
      requested = season_requested,
      raw = if (season_requested) paste(explicit_years, collapse = ", ") else NULL,
      unsupported_reason = season_reason
    ),
    season_range = make_slot(
      explicit_value = season_range_value,
      requested = season_range_requested,
      raw = if (season_range_requested) {
        if (length(explicit_years)) {
          paste(explicit_years, collapse = ", ")
        } else {
          "all seasons"
        }
      } else {
        NULL
      },
      unsupported_reason = season_reason
    ),
    innings = make_slot(
      explicit_value = if (!is.null(innings_match)) innings_match$value else NULL,
      requested = innings_requested,
      raw = if (innings_requested) innings_match$raw else NULL,
      unsupported_reason = innings_reason
    ),
    date_range = make_slot(
      explicit_value = date_range_value,
      requested = length(date_tokens) > 0L ||
        date_language_requested,
      raw = if (length(date_tokens)) paste(date_tokens, collapse = " to ") else NULL,
      unsupported_reason = date_range_reason
    ),
    match_phase = make_slot(
      explicit_value = phase_value,
      requested = phase_requested,
      raw = if (phase_requested) query else NULL,
      unsupported_reason = if (phase_requested && is.null(phase_value)) {
        "The requested match phase is not supported or could not be resolved."
      } else {
        NULL
      }
    ),
    bowling_style = if (bowling_style_requested) {
      query_constraint(
        value = bowling_style_match[1, 1],
        status = "unsupported",
        source = "user_query",
        raw = bowling_style_match[1, 1],
        reason = paste(
          "CricLens cannot calculate player performance against this bowling style:",
          "the available delivery dataset has no verified bowler handedness or bowling-style field."
        )
      )
    } else {
      query_constraint()
    },
    aggregation = make_slot(
      explicit_value = aggregation,
      requested = str_detect(
        query_lower,
        regex("\\b(average|mean|total|most|highest|top)\\b")
      ),
      raw = if (!is.null(aggregation)) aggregation else NULL
    ),
    requested_metrics = make_slot(
      explicit_value = requested_metrics,
      requested = length(requested_metrics) > 0L,
      raw = if (length(requested_metrics)) paste(requested_metrics, collapse = ", ") else NULL,
      unsupported_reason = if (length(unsupported_metrics)) {
        paste(
          "The requested metric is not supported by the current query pipeline:",
          paste(unique(unsupported_metrics), collapse = ", ")
        )
      } else {
        NULL
      }
    ),
    comparison_options = make_slot(
      explicit_value = unique(comparison_options),
      requested = length(comparison_options) > 0L,
      raw = if (length(comparison_options)) paste(comparison_options, collapse = ", ") else NULL
    )
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
    parameters = parameters,
    query_spec = new_query_representation(
      list(intent = intent, parameters = parameters),
      query = query,
      df = df,
      intent_source = "model"
    )
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

fallback_query_intent_legacy <- function(
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
  paired_comparison_requested <- str_detect(
    q_lower,
    regex("\\b(compare|versus|vs|difference|better than|worse than|outperform)\\b")
  )
  against_two_entities <- str_detect(q_lower, regex("\\bagainst\\b")) &&
    (
      length(matched_players) >= 2L ||
        length(matched_teams) >= 2L
    )
  compare_requested <- paired_comparison_requested || against_two_entities
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

fallback_query_intent <- function(query, df) {
  parsed <- fallback_query_intent_legacy(query, df)
  parsed$query_spec <- new_query_representation(
    parsed,
    query = query,
    df = df,
    intent_source = "deterministic_parser"
  )
  parsed
}

# ------------------------------------------------------------------
# Validate intent
# ------------------------------------------------------------------

validate_intent_legacy <- function(
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

validate_query_representation <- function(query_spec, df) {
  if (!is.list(query_spec)) {
    return(query_spec)
  }

  validate_slot <- function(slot, resolve) {
    if (
      !is.list(slot) ||
        !slot$status %in% c("explicit", "inferred")
    ) {
      return(slot)
    }

    resolved <- resolve(slot$value)
    if (is.null(resolved) || anyNA(resolved)) {
      slot$status <- "unsupported"
      slot$validation <- "invalid"
      slot$reason <- "The requested value could not be validated against the dataset."
      return(slot)
    }

    slot$value <- resolved
    slot$validation <- "valid"
    slot
  }

  query_spec$players <- validate_slot(
    query_spec$players,
    function(values) {
      values <- as.character(unlist(values, use.names = FALSE))
      resolved <- vapply(values, function(value) {
        result <- resolve_player_name(value, get_dataset_players(df))
        if (is.null(result)) NA_character_ else result
      }, character(1), USE.NAMES = FALSE)
      if (anyNA(resolved)) NULL else resolved
    }
  )
  query_spec$teams <- validate_slot(
    query_spec$teams,
    function(values) {
      values <- as.character(unlist(values, use.names = FALSE))
      resolved <- vapply(values, function(value) {
        result <- resolve_dataset_value(value, get_dataset_teams(df))
        if (is.null(result)) NA_character_ else result
      }, character(1), USE.NAMES = FALSE)
      if (anyNA(resolved)) NULL else resolved
    }
  )
  query_spec$venue <- validate_slot(
    query_spec$venue,
    function(values) {
      values <- as.character(unlist(values, use.names = FALSE))
      resolved <- vapply(values, function(value) {
        result <- resolve_dataset_value(value, get_dataset_venues(df))
        if (is.null(result)) NA_character_ else result
      }, character(1), USE.NAMES = FALSE)
      if (anyNA(resolved)) NULL else resolved
    }
  )
  available_seasons <- unique(as.character(
    as.integer(df$season_year[!is.na(df$season_year)])
  ))
  query_spec$season <- validate_slot(
    query_spec$season,
    function(value) {
      value <- safe_character(value)
      if (is.null(value) || !value %in% available_seasons) NULL else value
    }
  )
  query_spec$season_range <- validate_slot(
    query_spec$season_range,
    function(values) {
      values <- as.character(unlist(values, use.names = FALSE))
      if (!length(values) || !all(values %in% available_seasons)) NULL else values
    }
  )
  query_spec$innings <- validate_slot(
    query_spec$innings,
    function(value) {
      if (identical(value, "super_over")) {
        if (any(df$is_super_over %in% TRUE, na.rm = TRUE)) value else NULL
      } else if (
        length(value) == 1L &&
          is.numeric(value) &&
          value %in% unique(df$innings)
      ) {
        as.integer(value)
      } else {
        NULL
      }
    }
  )
  query_spec$date_range <- validate_slot(
    query_spec$date_range,
    function(value) {
      dates <- suppressWarnings(as.Date(unlist(value, use.names = FALSE)))
      if (
        length(dates) != 2L ||
          anyNA(dates) ||
          dates[1] > dates[2] ||
          dates[1] < min(df$date, na.rm = TRUE) ||
          dates[2] > max(df$date, na.rm = TRUE)
      ) {
        NULL
      } else {
        as.character(dates)
      }
    }
  )
  if (
    identical(query_spec$date_range$status, "unsupported") &&
      identical(query_spec$date_range$validation, "invalid")
  ) {
    query_spec$date_range$reason <- paste(
      "The requested date range is outside the date coverage",
      "available in the dataset."
    )
  }
  query_spec$match_phase <- validate_slot(
    query_spec$match_phase,
    function(values) {
      values <- as.character(unlist(values, use.names = FALSE))
      available <- unique(as.character(df$match_phase))
      resolved <- vapply(values, function(value) {
        result <- resolve_dataset_value(value, available)
        if (is.null(result)) NA_character_ else result
      }, character(1), USE.NAMES = FALSE)
      if (anyNA(resolved)) NULL else resolved
    }
  )

  query_spec
}

validate_intent <- function(parsed, df) {
  validated <- validate_intent_legacy(parsed, df)
  query_spec <- parsed$query_spec
  if (is.null(query_spec)) {
    query_spec <- new_query_representation(parsed)
  }
  validated$query_spec <- validate_query_representation(query_spec, df)
  if (is.list(validated$parameters)) {
    parameters <- validated$parameters
    players <- validated$query_spec$players
    if (
      players$status %in% c("explicit", "inferred") &&
        players$validation == "valid"
    ) {
      if (validated$intent %in% c("player_comparison") &&
        length(players$value) == 2L) {
        parameters$player1 <- players$value[1]
        parameters$player2 <- players$value[2]
      } else if (
        validated$intent %in% c(
          "player_profile", "player_bowling_profile",
          "player_team_history"
        ) &&
          length(players$value) == 1L
      ) {
        parameters$player_name <- players$value[1]
      }
    }
    teams <- validated$query_spec$teams
    if (
      teams$status %in% c("explicit", "inferred") &&
        teams$validation == "valid"
    ) {
      if (validated$intent == "head_to_head" && length(teams$value) == 2L) {
        parameters$team1 <- teams$value[1]
        parameters$team2 <- teams$value[2]
      } else if (
        validated$intent %in% c("team_summary", "team_trend") &&
          length(teams$value) == 1L
      ) {
        parameters$team_name <- teams$value[1]
      }
    }
    venue <- validated$query_spec$venue
    if (
      validated$intent == "venue_stats" &&
        venue$status %in% c("explicit", "inferred") &&
        venue$validation == "valid" &&
        length(venue$value) == 1L
    ) {
      parameters$venue_name <- venue$value[1]
    }
    season <- validated$query_spec$season
    if (
      season$status %in% c("explicit", "inferred") &&
        season$validation == "valid"
    ) {
      parameters$season <- season$value
    }
    validated$parameters <- parameters
  }
  validated
}

query_spec_limitation <- function(query_spec, intent) {
  constrained_fields <- c(
    "players", "teams", "venue", "season", "season_range",
    "innings", "date_range", "match_phase", "bowling_style", "requested_metrics",
    "comparison_options"
  )
  for (field in constrained_fields) {
    constraint <- query_spec[[field]]
    if (!is.list(constraint)) {
      next
    }
    if (constraint$status %in% c("ambiguous", "unsupported")) {
      limitation <- safe_character(
        constraint$reason,
        paste0(
          "The requested ",
          gsub("_", " ", field, fixed = TRUE),
          " constraint cannot be supported safely."
        )
      )
      if (
        field == "bowling_style" &&
          query_spec$venue$status %in% c("explicit", "inferred") &&
          query_spec$venue$validation == "valid" &&
          length(query_spec$venue$value) == 1L
      ) {
        limitation <- paste0(
          "The requested venue resolves to ",
          query_spec$venue$value,
          ". ",
          limitation
        )
      }
      return(limitation)
    }
    if (
      constraint$status %in% c("explicit", "inferred") &&
        identical(constraint$validation, "invalid")
    ) {
      return(safe_character(
        constraint$reason,
        paste0(
          "The requested ",
          gsub("_", " ", field, fixed = TRUE),
          " constraint is not available in the dataset."
        )
      ))
    }
  }

  requested_metrics <- query_spec$requested_metrics$value
  if (length(requested_metrics)) {
    supported_metrics <- switch(
      intent,
      top_batters = c("runs", "average", "strike_rate", "boundaries"),
      player_profile = c("runs", "average", "strike_rate", "boundaries"),
      player_comparison = c("runs", "average", "strike_rate", "boundaries"),
      top_bowlers = c("wickets", "average", "strike_rate", "economy"),
      player_bowling_profile = c("wickets", "average", "strike_rate", "economy"),
      team_summary = c("wins", "win_pct"),
      team_trend = c("wins"),
      head_to_head = c("wins"),
      venue_stats = c("runs", "average", "wickets", "wins"),
      character()
    )
    unsupported_metrics <- setdiff(requested_metrics, supported_metrics)
    if (length(unsupported_metrics)) {
      return(paste(
        "The current CricLens analytics do not support",
        paste(unsupported_metrics, collapse = ", "),
        "for this query type."
      ))
    }
  }

  if (
    identical(query_spec$aggregation$status, "explicit") &&
      identical(query_spec$aggregation$value, "average") &&
      intent %in% c("top_batters", "top_bowlers")
  ) {
    return(
      "The available leaderboard does not rank players by the requested average; it ranks batters by runs and bowlers by wickets."
    )
  }

  comparison_metrics <- query_spec$comparison_options$value
  if (
    intent == "player_comparison" &&
      length(comparison_metrics) &&
      any(comparison_metrics != "batting")
  ) {
    return(
      "Player comparisons currently support batting metrics only; a verified bowling comparison is not available."
    )
  }

  if (
    intent %in% c("player_comparison", "head_to_head") &&
      query_spec$players$status %in% c("explicit", "inferred") &&
      length(query_spec$players$value) != 2L
  ) {
    return("Please provide exactly two players for this comparison.")
  }
  if (
    intent == "head_to_head" &&
      query_spec$teams$status %in% c("explicit", "inferred") &&
      length(query_spec$teams$value) != 2L
  ) {
    return("Please provide exactly two teams for this head-to-head comparison.")
  }

  if (
    intent == "venue_stats" &&
      query_spec$venue$status %in% c("explicit", "inferred") &&
      length(query_spec$venue$value) != 1L
  ) {
    return("Venue statistics require one unambiguous venue; the requested venue could not be isolated.")
  }

  player_scoped_intents <- c(
    "top_batters", "top_bowlers", "player_profile",
    "player_bowling_profile", "player_comparison",
    "player_team_history"
  )
  if (
    query_spec$players$status %in% c("explicit", "inferred") &&
      !intent %in% player_scoped_intents
  ) {
    return(
      "The current analytics cannot apply a player filter to this query type without changing its metric definition."
    )
  }

  match_level_intents <- c(
    "team_summary", "team_trend", "head_to_head", "venue_stats"
  )
  if (
    intent %in% match_level_intents &&
      query_spec$innings$status %in% c("explicit", "inferred")
  ) {
    return(
      "This match-level analysis requires complete innings records; an innings-only filter would make its match totals or outcomes misleading."
    )
  }
  if (
    intent == "venue_stats" &&
      query_spec$match_phase$status %in% c("explicit", "inferred")
  ) {
    return(
      "Venue profiles calculate whole-innings scores and match outcomes; the current metric definitions cannot apply a match-phase-only filter safely."
    )
  }

  if (
    intent %in% c(
      "top_batters", "top_bowlers", "player_profile",
      "player_bowling_profile", "player_comparison"
    ) &&
      query_spec$innings$status %in% c("explicit", "inferred") &&
      !identical(query_spec$innings$value, "super_over") &&
      !(query_spec$innings$value %in% c(1L, 2L))
  ) {
    return(paste(
      "The existing verified batting and bowling metrics exclude innings",
      "outside innings 1 and 2, so this innings-specific request cannot",
      "be answered using their current definitions."
    ))
  }

  if (
    intent %in% c("top_batters", "top_bowlers", "player_profile",
      "player_bowling_profile", "player_comparison") &&
      identical(query_spec$innings$value, "super_over")
  ) {
    return(
      "The existing verified batting and bowling metrics exclude Super Overs; that innings-specific request is not supported."
    )
  }
  if (
    intent %in% c("top_batters", "top_bowlers", "player_profile",
      "player_bowling_profile", "player_comparison") &&
      identical(query_spec$match_phase$value, "Super Over")
  ) {
    return(
      "The existing verified batting and bowling metrics exclude Super Overs; that match-phase request is not supported."
    )
  }

  NULL
}

apply_query_spec_filters <- function(df, query_spec, intent) {
  filtered <- df
  applied <- character()

  apply_filter <- function(field, column, values, operator = "in") {
    if (!length(values)) {
      return(NULL)
    }
    if (!column %in% names(filtered)) {
      return(paste0(
        "The dataset does not contain the ",
        gsub("_", " ", field, fixed = TRUE),
        " field required by this query."
      ))
    }
    if (operator == "range") {
      filtered <<- filtered[
        !is.na(filtered[[column]]) &
          filtered[[column]] >= values[1] &
          filtered[[column]] <= values[2],
        ,
        drop = FALSE
      ]
    } else {
      filtered <<- filtered[
        !is.na(filtered[[column]]) &
          filtered[[column]] %in% values,
        ,
        drop = FALSE
      ]
    }
    applied <<- c(applied, field)
    NULL
  }

  venue <- query_spec$venue
  if (venue$status %in% c("explicit", "inferred")) {
    venue_column <- if ("canonical_venue" %in% names(filtered)) {
      "canonical_venue"
    } else {
      "venue"
    }
    issue <- apply_filter("venue", venue_column, venue$value)
    if (!is.null(issue)) return(list(data = filtered, applied = applied, reason = issue))
  }

  season <- query_spec$season
  if (season$status %in% c("explicit", "inferred")) {
    issue <- apply_filter(
      "season",
      "season_year",
      as.numeric(season$value)
    )
    if (!is.null(issue)) return(list(data = filtered, applied = applied, reason = issue))
  }

  season_range <- query_spec$season_range
  if (season_range$status %in% c("explicit", "inferred")) {
    values <- as.numeric(season_range$value)
    issue <- apply_filter("season range", "season_year", range(values), "range")
    if (!is.null(issue)) return(list(data = filtered, applied = applied, reason = issue))
  }

  date_range <- query_spec$date_range
  if (date_range$status %in% c("explicit", "inferred")) {
    values <- as.Date(date_range$value)
    issue <- apply_filter("date range", "date", values, "range")
    if (!is.null(issue)) return(list(data = filtered, applied = applied, reason = issue))
  }

  innings <- query_spec$innings
  if (innings$status %in% c("explicit", "inferred")) {
    if (identical(innings$value, "super_over")) {
      if (!"is_super_over" %in% names(filtered)) {
        return(list(
          data = filtered,
          applied = applied,
          reason = "The dataset does not contain the Super Over flag required by this query."
        ))
      }
      filtered <- filtered[filtered$is_super_over %in% TRUE, , drop = FALSE]
      applied <- c(applied, "innings")
    } else {
      issue <- apply_filter("innings", "innings", innings$value)
      if (!is.null(issue)) return(list(data = filtered, applied = applied, reason = issue))
    }
  }

  phase <- query_spec$match_phase
  if (phase$status %in% c("explicit", "inferred")) {
    issue <- apply_filter("match phase", "match_phase", phase$value)
    if (!is.null(issue)) return(list(data = filtered, applied = applied, reason = issue))
  }

  teams <- query_spec$teams
  if (
    teams$status %in% c("explicit", "inferred") &&
      length(teams$value)
  ) {
    required <- c("batting_team", "bowling_team")
    if (!all(required %in% names(filtered))) {
      return(list(
        data = filtered,
        applied = applied,
        reason = "The dataset does not contain the team fields required by this query."
      ))
    }
    team_match <- if (intent %in% c(
      "top_batters", "player_profile", "player_comparison"
    )) {
      !is.na(filtered$batting_team) &
        filtered$batting_team %in% teams$value
    } else if (intent %in% c("top_bowlers", "player_bowling_profile")) {
      !is.na(filtered$bowling_team) &
        filtered$bowling_team %in% teams$value
    } else {
      (!is.na(filtered$batting_team) & filtered$batting_team %in% teams$value) |
        (!is.na(filtered$bowling_team) & filtered$bowling_team %in% teams$value)
    }
    filtered <- filtered[team_match, , drop = FALSE]
    applied <- c(applied, "teams")
  }

  list(data = filtered, applied = applied, reason = NULL)
}

validate_query_result_scope <- function(intent, result, query_spec) {
  if (is.null(result)) {
    return(NULL)
  }

  expected_players <- if (
    query_spec$players$status %in% c("explicit", "inferred") &&
      query_spec$players$validation == "valid"
  ) {
    query_spec$players$value
  } else {
    character()
  }
  if (is.data.frame(result) && length(expected_players)) {
    player_column <- switch(
      intent,
      top_batters = "batter",
      top_bowlers = "bowler",
      player_bowling_profile = "bowler",
      player_profile = "player",
      player_comparison = "player",
      NULL
    )
    if (
      !is.null(player_column) &&
        player_column %in% names(result) &&
        any(!result[[player_column]] %in% expected_players)
    ) {
      return("The calculated player results do not match the resolved player scope.")
    }
    if (
      intent == "player_comparison" &&
        !is.null(player_column) &&
        player_column %in% names(result) &&
        !setequal(result[[player_column]], expected_players)
    ) {
      return("The available filtered records did not produce results for both requested players.")
    }
  }

  expected_teams <- if (
    query_spec$teams$status %in% c("explicit", "inferred") &&
      query_spec$teams$validation == "valid"
  ) {
    query_spec$teams$value
  } else {
    character()
  }
  if (intent == "head_to_head" && is.list(result)) {
    if (
      length(expected_teams) == 2L &&
        !setequal(c(result$team1, result$team2), expected_teams)
    ) {
      return("The calculated head-to-head result does not match the resolved team scope.")
    }
    if (identical(result$total_matches, 0L) || identical(result$total_matches, 0)) {
      return("No head-to-head matches were found for the requested filters.")
    }
  }
  if (
    is.data.frame(result) &&
      length(expected_teams) == 1L &&
      intent %in% c("team_summary", "team_trend") &&
      "team" %in% names(result) &&
      any(result$team != expected_teams)
  ) {
    return("The calculated team results do not match the resolved team scope.")
  }

  expected_venue <- if (
    query_spec$venue$status %in% c("explicit", "inferred") &&
      query_spec$venue$validation == "valid" &&
      length(query_spec$venue$value) == 1L
  ) {
    query_spec$venue$value
  } else {
    NULL
  }
  if (
    intent == "venue_stats" &&
      !is.null(expected_venue) &&
      is.list(result) &&
      !identical(result$venue, expected_venue)
  ) {
    return("The calculated venue result does not match the resolved venue scope.")
  }

  NULL
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
  query_spec <- validated$query_spec
  if (!is.list(query_spec)) {
    query_spec <- new_query_representation(validated)
    query_spec <- validate_query_representation(query_spec, df)
  }

  # ---------------------------------------------------------------
  # Unsupported
  # ---------------------------------------------------------------

  if (intent == "unsupported") {
    if (identical(query_spec$requested_metrics$status, "unsupported")) {
      return(list(
        intent = "unsupported",
        result = NULL,
        reason = query_spec$requested_metrics$reason
      ))
    }

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

  limitation <- query_spec_limitation(query_spec, intent)
  if (!is.null(limitation)) {
    return(list(intent = "unsupported", result = NULL, reason = limitation))
  }
  filtered_data <- apply_query_spec_filters(df, query_spec, intent)
  if (!is.null(filtered_data$reason)) {
    return(list(
      intent = "unsupported",
      result = NULL,
      reason = filtered_data$reason
    ))
  }
  if (!nrow(filtered_data$data)) {
    scope <- unique(c(
      filtered_data$applied,
      if (length(query_spec$players$value)) "player" else character()
    ))
    return(list(
      intent = "unsupported",
      result = NULL,
      reason = paste0(
        "No dataset records match the requested ",
        paste(scope, collapse = ", "),
        " filters."
      )
    ))
  }
  df <- filtered_data$data
  scope_applied <- filtered_data$applied
  scope <- list(
    filters = scope_applied,
    record_count = nrow(df),
    records = "delivery_rows",
    constraints = lapply(
      query_spec[c(
        "players", "teams", "venue", "season", "season_range",
        "innings", "date_range", "match_phase"
      )],
      function(constraint) {
        if (
          is.list(constraint) &&
            constraint$status %in% c("explicit", "inferred") &&
            constraint$validation == "valid"
        ) {
          constraint$value
        } else {
          NULL
        }
      }
    )
  )
  finish_result <- function(result, additional = list()) {
    if (is.null(result)) {
      return(list(
        intent = "unsupported",
        result = NULL,
        reason = paste(
          "No analytical result was available for the requested filters:",
          paste(scope$filters, collapse = ", "),
          "."
        )
      ))
    }
    if (is.data.frame(result) && !nrow(result)) {
      return(list(
        intent = "unsupported",
        result = NULL,
        reason = paste(
          "No analytical records match the requested filters:",
          paste(scope$filters, collapse = ", "),
          "."
        )
      ))
    }
    issue <- validate_query_result_scope(intent, result, query_spec)
    if (!is.null(issue)) {
      return(list(intent = "unsupported", result = NULL, reason = issue))
    }
    c(
      list(intent = intent, result = result, scope = scope),
      additional
    )
  }

  # ---------------------------------------------------------------
  # Top batters
  # ---------------------------------------------------------------

  if (intent == "top_batters") {

    result <- get_top_batters(
      df,
      top_n = if (
        query_spec$players$status %in% c("explicit", "inferred") &&
          length(query_spec$players$value)
      ) {
        dplyr::n_distinct(df$batter)
      } else {
        p$top_n + p$offset
      },
      season_filter = season
    )

    if (
      query_spec$players$status %in% c("explicit", "inferred") &&
        length(query_spec$players$value) &&
        "batter" %in% names(result)
    ) {
      result <- result[result$batter %in% query_spec$players$value, , drop = FALSE]
    }
    if (p$offset > 0 && nrow(result) > p$offset) {
      last_row <- min(nrow(result), p$offset + p$top_n)
      result <- result[seq.int(p$offset + 1, last_row), , drop = FALSE]
    }

    return(finish_result(result))
  }

  # ---------------------------------------------------------------
  # Top bowlers
  # ---------------------------------------------------------------

  if (intent == "top_bowlers") {

    result <- get_top_bowlers(
      df,
      top_n = if (
        query_spec$players$status %in% c("explicit", "inferred") &&
          length(query_spec$players$value)
      ) {
        dplyr::n_distinct(df$bowler)
      } else {
        p$top_n + p$offset
      },
      season_filter = season
    )

    if (
      query_spec$players$status %in% c("explicit", "inferred") &&
        length(query_spec$players$value) &&
        "bowler" %in% names(result)
    ) {
      result <- result[result$bowler %in% query_spec$players$value, , drop = FALSE]
    }
    if (p$offset > 0 && nrow(result) > p$offset) {
      last_row <- min(nrow(result), p$offset + p$top_n)
      result <- result[seq.int(p$offset + 1, last_row), , drop = FALSE]
    }

    return(finish_result(result))
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

    return(finish_result(result))
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

    return(finish_result(result, list(player_name = p$player_name)))
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

    return(finish_result(result, list(player_name = p$player_name)))
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
    if (nrow(result) != 2L) {
      available_players <- if (is.data.frame(result) && "player" %in% names(result)) {
        result$player
      } else {
        character()
      }
      missing_players <- setdiff(c(p$player1, p$player2), available_players)
      requested_scope <- format_query_scope(query_spec)
      return(list(
        intent = "unsupported",
        result = NULL,
        scope = scope,
        reason = paste(
          paste0(
            "No batting records were found for ",
            paste(missing_players, collapse = " and "),
            " under the requested filters."
          ),
          requested_scope
        )
      ))
    }

    return(finish_result(result, list(player_name = p$player_name)))
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

    return(finish_result(result, list(
      metric = p$metric,
      ranking = identical(p$metric, "win_pct") &&
        is.null(p$team_name)
    )))
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

    return(finish_result(result))
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

    return(finish_result(result))
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

    return(finish_result(result))
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

  verified_result_text <- format_r_result(analytics_result)
  verified_result_lines <- strsplit(
    verified_result_text,
    "\n",
    fixed = TRUE
  )[[1]]
  verified_result_lines <- verified_result_lines[
    !stringr::str_detect(
      verified_result_lines,
      regex("^\\|\\s*(player|batter|bowler|team|season|venue)\\b", ignore_case = TRUE)
    ) &
      !stringr::str_detect(verified_result_lines, "^\\|[-: |]+\\|$")
  ]
  verified_numbers <- extract_numbers(paste(
    query,
    verified_result_lines
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
      "average", "strike_rate", "fours", "sixes",
      "highest_score", "fifties", "hundreds"
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
          row$strike_rate, " | ", row$fours, " | ", row$sixes, " | ",
          row$highest_score, " | ", row$fifties, " | ", row$hundreds, " |"
        )
      },
      character(1)
    )
    return(paste(c(
      "### 🏏 Player Batting Comparison",
      "",
      "| Player | Matches | Innings | Runs | Average | Strike Rate | 4s | 6s | Highest Score | 50s | 100s |",
      "| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |",
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
      "highest_score",
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
        "- **Highest Score:** ",
        row$highest_score,
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

format_query_scope <- function(query_spec) {
  if (!is.list(query_spec)) {
    return("")
  }

  scope_fields <- c(
    players = "Players",
    teams = "Teams",
    venue = "Venue",
    season = "Season",
    season_range = "Seasons",
    innings = "Innings",
    date_range = "Dates",
    match_phase = "Match phase"
  )
  descriptions <- vapply(names(scope_fields), function(field) {
    constraint <- query_spec[[field]]
    if (
      !is.list(constraint) ||
        !constraint$status %in% c("explicit", "inferred") ||
        !identical(constraint$validation, "valid")
    ) {
      return("")
    }
    value <- constraint$value
    if (field == "innings" && identical(value, "super_over")) {
      value <- "Super Over"
    } else if (field == "date_range") {
      value <- paste(value, collapse = " to ")
    } else {
      value <- paste(value, collapse = ", ")
    }
    paste0(scope_fields[[field]], ": ", value)
  }, character(1), USE.NAMES = FALSE)
  descriptions <- descriptions[nzchar(descriptions)]
  if (!length(descriptions)) {
    return("")
  }
  paste0("\n\n_Applied filters: ", paste(descriptions, collapse = "; "), "_")
}

is_cricket_analytics_query <- function(query) {
  str_detect(
    tolower(safe_character(query, default = "")),
    regex(
      "\\b(ipl|cricket|runs?|wickets?|batting|bowlers?|batters?|players?|teams?|franchises?|venues?|stadiums?|grounds?|matches?|seasons?|innings|strike\\s+rate|economy|head[- ]to[- ]head|stats?|statistics|performance|profiles?|careers?|compare|versus|\\bvs\\b|sixes|fours|boundaries|scores?|dot[- ]balls?|maiden overs?|catches|fielding|partnerships?|run rates?)\\b"
    )
  )
}

general_conversation_response <- function(query, df = NULL) {
  normalized <- tolower(trimws(safe_character(query, default = "")))
  if (!nzchar(normalized)) {
    return(NULL)
  }

  if (
    str_detect(normalized, regex("\\b(win|winning)\\s+probabilit(y|ies)\\b")) &&
      str_detect(normalized, regex("\\b(why|change|changes|changing|shift|shifts|during|chase|chasing)\\b"))
  ) {
    return(paste(
      "During a chase, the estimate changes as the match state changes: runs required,",
      "balls remaining, and wickets remaining all affect the chasing side's prospects.",
      "CricLens's saved model uses those state values plus required run rate and current run rate.",
      "Factors such as batter-bowler matchups, pitch conditions, and weather may matter to cricket",
      "in general, but they are not explicit input features in this model. The output is a",
      "historical-data-based estimate, not a certainty."
    ))
  }

  if (str_detect(normalized, regex("^(hi|hello|hey|good morning|good afternoon|good evening)[!.?,\\s]*$"))) {
    return("Hi! I'm CricLens. I can help explore IPL player, team, venue, and match analytics.")
  }
  if (str_detect(normalized, regex("^(thanks|thank you|thx)[!.?,\\s]*$"))) {
    return("You're welcome! Ask me whenever you'd like to explore IPL analytics.")
  }
  if (str_detect(
    normalized,
    regex("\\b(what can you do|how can you help|who are you|what are you|\\bhelp\\b)\\b")
  )) {
    return(paste(
      "I'm CricLens, an IPL analytics assistant.",
      "I can report verified player and team statistics, compare players or teams,",
      "show venue profiles, and analyze supported seasons and match filters."
    ))
  }
  if (
    !is.null(df) &&
      str_detect(normalized, regex("\\b(what|how)\\s+about\\b")) &&
      (
        length(extract_team_matches(query, get_dataset_teams(df))) > 0L ||
          length(extract_player_matches(query, get_dataset_players(df))) > 0L
      )
  ) {
    return(paste(
      "What would you like to know about that IPL team or player?",
      "I can show verified statistics, seasonal performance, or a comparison."
    ))
  }
  if (!is_cricket_analytics_query(normalized)) {
    return(paste(
      "I'm CricLens, focused on IPL analytics.",
      "I can help with verified player, team, venue, and match statistics."
    ))
  }
  NULL
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

  general_response <- general_conversation_response(query, df)
  if (!is.null(general_response)) {
    return(general_response)
  }

  parsed <- fallback_query_intent(query, df)
  used_nim_for_intent <- FALSE
  if (
    identical(parsed$intent, "unsupported") &&
      is_cricket_analytics_query(query)
  ) {
    nim_parsed <- tryCatch(
      parse_query_intent(query, df, status_callback),
      error = function(error) {
        report_nim_status(
          status_callback,
          "degraded",
          "NVIDIA NIM could not interpret the query. Deterministic R routing is being used."
        )
        message("NVIDIA NIM intent interpretation failed; using deterministic R routing.")
        NULL
      }
    )
    if (
      is.list(nim_parsed) &&
        !identical(nim_parsed$intent, "unsupported")
    ) {
      parsed <- nim_parsed
      used_nim_for_intent <- TRUE
    }
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
  # 6. Ask NIM to explain ONLY the validated R result.
  # If NIM already interpreted this query, keep this response deterministic
  # rather than making a second potentially blocking request.
  # ---------------------------------------------------------------

  deterministic_result <- format_r_result(analytics_result)
  query_scope <- format_query_scope(validated$query_spec)
  if (nzchar(query_scope)) {
    deterministic_result <- paste0(deterministic_result, query_scope)
  }
  deterministic_fallback <- paste(
    deterministic_result,
    "\n\n#### Explanation\n\n",
    "The statistics above are calculated by CricLens R analytics from the IPL dataset.",
    if (length(analytics_result$scope$filters)) {
      "The requested filters shown above were applied to source records before aggregation."
    } else {
      "No additional query filters were applied."
    }
  )
  explanation <- if (used_nim_for_intent) {
    NULL
  } else {
    tryCatch(
      explain_validated_result(
        query,
        analytics_result,
        status_callback
      ),
      error = function(error) {
        report_nim_status(
          status_callback,
          "degraded",
          "NVIDIA NIM explanation failed. The verified result is being returned with a deterministic explanation."
        )
        message("NVIDIA NIM explanation failed; returning the verified R result.")
        NULL
      }
    )
  }

  # ---------------------------------------------------------------
  # 7. If NIM explanation succeeds, return it
  # ---------------------------------------------------------------

  if (
    is.character(explanation) &&
      length(explanation) == 1L &&
      !is.na(explanation) &&
      nzchar(trimws(explanation)) &&
      !substr(trimws(explanation), 1L, 1L) %in% c("{", "[")
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
      return(deterministic_fallback)
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

  deterministic_fallback
}
