# R/venue_analysis.R
# CricLens — Venue Analytics Module
# Source of truth for venue statistics, stadium pitch profiles,
# toss bias, and scoring trends

suppressMessages({
  library(dplyr)
  library(tidyr)
})


#' Get Searchable Venue List
get_venue_list <- function(df) {

  venues <- df %>%
    group_by(
      venue,
      city
    ) %>%
    summarise(
      matches = n_distinct(match_id),
      .groups = "drop"
    ) %>%
    arrange(
      desc(matches),
      venue
    )

  return(venues)
}


#' Get Comprehensive Venue Profile & Pitch Analytics
#'
#' @param df Featured IPL dataframe
#' @param selected_venue Venue string name
get_venue_profile <- function(
    df,
    selected_venue
) {

  v_balls <- df %>%
    filter(
      venue == selected_venue
    )

  if (nrow(v_balls) == 0) {
    return(NULL)
  }


  # One row per match
  v_matches <- v_balls %>%
    distinct(
      match_id,
      .keep_all = TRUE
    )


  total_matches <- nrow(
    v_matches
  )


  # ---------------------------------------------------------
  # 1st Innings Run Distribution
  # ---------------------------------------------------------

  inn1_scores <- v_balls %>%
    filter(
      innings == 1
    ) %>%
    group_by(
      match_id
    ) %>%
    summarise(
      total_runs = sum(
        runs_total,
        na.rm = TRUE
      ),
      .groups = "drop"
    )


  avg_1st_inn <- ifelse(
    nrow(inn1_scores) > 0,
    round(
      mean(
        inn1_scores$total_runs,
        na.rm = TRUE
      ),
      1
    ),
    0
  )


  highest_1st_inn <- ifelse(
    nrow(inn1_scores) > 0,
    max(
      inn1_scores$total_runs,
      na.rm = TRUE
    ),
    0
  )


  lowest_1st_inn <- ifelse(
    nrow(inn1_scores) > 0,
    min(
      inn1_scores$total_runs,
      na.rm = TRUE
    ),
    0
  )


  # ---------------------------------------------------------
  # Match Outcome by Innings
  # ---------------------------------------------------------

  match_outcomes <- v_balls %>%
    filter(
      innings %in% c(1, 2)
    ) %>%
    group_by(
      match_id
    ) %>%
    summarise(
      first_innings_team = first(
        batting_team[innings == 1]
      ),

      second_innings_team = first(
        batting_team[innings == 2]
      ),

      match_winner = first(
        match_won_by
      ),

      .groups = "drop"
    )


  completed_matches <- match_outcomes %>%
    filter(
      !is.na(match_winner),
      match_winner != "Unknown",
      match_winner != ""
    )


  bat_1st_wins <- sum(
    completed_matches$match_winner ==
      completed_matches$first_innings_team,
    na.rm = TRUE
  )


  chase_wins <- sum(
    completed_matches$match_winner ==
      completed_matches$second_innings_team,
    na.rm = TRUE
  )


  chase_win_pct <- ifelse(
    nrow(completed_matches) > 0,
    round(
      chase_wins /
        nrow(completed_matches) *
        100,
      1
    ),
    0
  )


  # ---------------------------------------------------------
  # Phase Run Rates at this Venue
  # ---------------------------------------------------------

  phase_stats <- v_balls %>%
    group_by(
      match_phase
    ) %>%
    summarise(

      runs = sum(
        runs_total,
        na.rm = TRUE
      ),

      balls = sum(
        valid_ball == 1,
        na.rm = TRUE
      ),

      wickets = sum(
        is_wicket,
        na.rm = TRUE
      ),

      run_rate = ifelse(
        balls > 0,
        round(
          runs /
            (balls / 6),
          2
        ),
        0
      ),

      .groups = "drop"
    )


  return(
    list(
      venue = selected_venue,

      city = first(
        v_balls$city
      ),

      total_matches = total_matches,

      avg_1st_inn = avg_1st_inn,

      highest_1st_inn = highest_1st_inn,

      lowest_1st_inn = lowest_1st_inn,

      bat_1st_wins = bat_1st_wins,

      chase_wins = chase_wins,

      chase_win_pct = chase_win_pct,

      phase_stats = phase_stats
    )
  )
}


#' Get Venue Comparative Leaderboard
get_venue_leaderboard <- function(
    df,
    min_matches = 10
) {

  # Build one analytical record per match.
  # This prevents ball-level rows from being counted
  # as separate matches.

  match_data <- df %>%
    filter(
      innings %in% c(1, 2)
    ) %>%
    group_by(
      venue,
      match_id
    ) %>%
    summarise(

      inn1_runs = sum(
        runs_total[
          innings == 1
        ],
        na.rm = TRUE
      ),

      inn2_runs = sum(
        runs_total[
          innings == 2
        ],
        na.rm = TRUE
      ),

      first_innings_team = first(
        batting_team[
          innings == 1
        ]
      ),

      second_innings_team = first(
        batting_team[
          innings == 2
        ]
      ),

      match_winner = first(
        match_won_by
      ),

      .groups = "drop"
    )


  # Explicitly identify completed matches and chasing wins.
  match_data <- match_data %>%
    mutate(
      completed = (
        !is.na(match_winner) &
          match_winner != "Unknown" &
          match_winner != ""
      ),

      chased_won = case_when(

        !completed ~ NA,

        match_winner ==
          second_innings_team ~ TRUE,

        TRUE ~ FALSE
      )
    )


  leaderboard <- match_data %>%
    group_by(
      venue
    ) %>%
    summarise(

      matches = n(),

      avg_1st_inn_score = ifelse(
        sum(inn1_runs > 0) > 0,
        round(
          mean(
            inn1_runs[
              inn1_runs > 0
            ],
            na.rm = TRUE
          ),
          1
        ),
        0
      ),

      avg_2nd_inn_score = ifelse(
        sum(inn2_runs > 0) > 0,
        round(
          mean(
            inn2_runs[
              inn2_runs > 0
            ],
            na.rm = TRUE
          ),
          1
        ),
        0
      ),

      completed_matches = sum(
        completed
      ),

      chase_wins = sum(
        chased_won == TRUE,
        na.rm = TRUE
      ),

      chasing_win_pct = ifelse(
        completed_matches > 0,
        round(
          chase_wins /
            completed_matches *
            100,
          1
        ),
        0
      ),

      .groups = "drop"
    ) %>%
    filter(
      matches >= min_matches
    ) %>%
    arrange(
      desc(avg_1st_inn_score),
      venue
    )


  return(leaderboard)
}