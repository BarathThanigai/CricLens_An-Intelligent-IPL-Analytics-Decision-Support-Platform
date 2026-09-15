# R/team_analysis.R
# CricLens — Team Analytics Module
# Source of truth for team performance, head-to-head metrics,
# and toss impact calculations

suppressMessages({
  library(dplyr)
  library(tidyr)
})


#' Get Team Performance Summary Table
#'
#' Calculates matches played, wins, losses and win percentage
#' directly from the featured IPL dataset.
#'
#' @param df Featured IPL dataframe
#' @param season_filter Optional season year filter
get_team_performance_summary <- function(
    df,
    season_filter = NULL
) {

  data_filtered <- df

  if (!is.null(season_filter) && season_filter != "All") {
    data_filtered <- data_filtered %>%
      filter(
        season_year == as.numeric(season_filter)
      )
  }

  # One row per match
  match_distinct <- data_filtered %>%
    distinct(
      match_id,
      .keep_all = TRUE
    )

  # Identify every team that participated in each match
  teams_played <- match_distinct %>%
    select(
      match_id,
      batting_team,
      bowling_team
    ) %>%
    pivot_longer(
      cols = c(
        batting_team,
        bowling_team
      ),
      names_to = "role",
      values_to = "team"
    ) %>%
    filter(
      !is.na(team),
      team != ""
    ) %>%
    distinct(
      match_id,
      team
    ) %>%
    group_by(team) %>%
    summarise(
      matches_played = n(),
      .groups = "drop"
    )

  # One winner per completed match
  team_wins <- match_distinct %>%
    filter(
      !is.na(match_won_by),
      match_won_by != "Unknown"
    ) %>%
    group_by(
      team = match_won_by
    ) %>%
    summarise(
      wins = n(),
      .groups = "drop"
    )

  summary_table <- teams_played %>%
    left_join(
      team_wins,
      by = "team"
    ) %>%
    mutate(
      wins = coalesce(
        wins,
        0L
      ),

      losses = pmax(
        matches_played - wins,
        0L
      ),

      win_pct = ifelse(
        matches_played > 0,
        round(
          wins / matches_played * 100,
          1
        ),
        0
      )
    ) %>%
    arrange(
      desc(wins),
      desc(win_pct)
    )

  return(summary_table)
}


#' Get Head-to-Head Record between Team 1 and Team 2
#'
#' @param df Featured IPL dataframe
#' @param team1 First team name
#' @param team2 Second team name
get_head_to_head <- function(
    df,
    team1,
    team2
) {

  matches_h2h <- df %>%
    filter(
      (
        batting_team == team1 &
          bowling_team == team2
      ) |
      (
        batting_team == team2 &
          bowling_team == team1
      )
    ) %>%
    distinct(
      match_id,
      .keep_all = TRUE
    ) %>%
    select(
      match_id,
      date,
      season_display,
      venue,
      toss_winner,
      toss_decision,
      match_won_by
    )

  total_matches <- nrow(
    matches_h2h
  )

  team1_wins <- sum(
    matches_h2h$match_won_by == team1,
    na.rm = TRUE
  )

  team2_wins <- sum(
    matches_h2h$match_won_by == team2,
    na.rm = TRUE
  )

  completed_matches <- team1_wins + team2_wins

  no_result <- max(
    total_matches - completed_matches,
    0
  )

  return(
    list(
      total_matches = total_matches,
      team1 = team1,
      team1_wins = team1_wins,
      team2 = team2,
      team2_wins = team2_wins,
      no_result = no_result,
      match_history = matches_h2h %>%
        arrange(
          desc(date),
          desc(match_id)
        )
    )
  )
}


#' Get Head-to-Head Pairwise Matrix for All Teams
get_head_to_head_matrix <- function(df) {

  # One row per match
  matches <- df %>%
    distinct(
      match_id,
      .keep_all = TRUE
    ) %>%
    filter(
      !is.na(match_won_by),
      match_won_by != "Unknown",
      !is.na(batting_team),
      !is.na(bowling_team)
    ) %>%
    mutate(
      opponent = ifelse(
        match_won_by == batting_team,
        bowling_team,
        ifelse(
          match_won_by == bowling_team,
          batting_team,
          NA_character_
        )
      )
    ) %>%
    filter(
      !is.na(opponent)
    )

  matrix_data <- matches %>%
    group_by(
      winner = match_won_by,
      opponent = opponent
    ) %>%
    summarise(
      wins = n(),
      .groups = "drop"
    ) %>%
    arrange(
      desc(wins),
      winner,
      opponent
    )

  return(matrix_data)
}


#' Get Toss Decision & Impact Analytics
#'
#' If team_name is supplied, only matches where that team won
#' the toss are included.
get_toss_impact <- function(
    df,
    team_name = NULL
) {

  data_filtered <- df

  if (!is.null(team_name) && team_name != "All") {
    data_filtered <- data_filtered %>%
      filter(
        toss_winner == team_name
      )
  }

  match_distinct <- data_filtered %>%
    distinct(
      match_id,
      toss_winner,
      toss_decision,
      match_won_by
    ) %>%
    filter(
      !is.na(toss_winner),
      !is.na(toss_decision),
      !is.na(match_won_by),
      match_won_by != "Unknown"
    )

  toss_summary <- match_distinct %>%
    group_by(
      toss_decision
    ) %>%
    summarise(
      total_tosses = n(),

      won_match = sum(
        toss_winner == match_won_by,
        na.rm = TRUE
      ),

      win_pct_after_toss = round(
        won_match / total_tosses * 100,
        1
      ),

      .groups = "drop"
    )

  return(toss_summary)
}


#' Get Batting 1st vs Chasing Performance Split
#'
#' Calculates team performance when batting first versus
#' batting second.
get_batting_1st_vs_chasing <- function(
    df,
    team_name = NULL
) {

  # One row per match + innings
  match_distinct <- df %>%
    distinct(
      match_id,
      innings,
      batting_team,
      bowling_team,
      match_won_by
    ) %>%
    filter(
      innings %in% c(1, 2)
    )

  if (!is.null(team_name) && team_name != "All") {
    match_distinct <- match_distinct %>%
      filter(
        batting_team == team_name |
          bowling_team == team_name
      )
  }

  # Batting first
  bat_1st <- match_distinct %>%
    filter(
      innings == 1
    ) %>%
    mutate(
      team = batting_team,

      win_bat1st = (
        match_won_by == batting_team
      )
    ) %>%
    group_by(
      team
    ) %>%
    summarise(
      bat_1st_matches = n(),

      bat_1st_wins = sum(
        win_bat1st,
        na.rm = TRUE
      ),

      bat_1st_win_pct = ifelse(
        bat_1st_matches > 0,
        round(
          bat_1st_wins /
            bat_1st_matches *
            100,
          1
        ),
        0
      ),

      .groups = "drop"
    )

  # Batting second / chasing
  bat_2nd <- match_distinct %>%
    filter(
      innings == 2
    ) %>%
    mutate(
      team = batting_team,

      win_chasing = (
        match_won_by == batting_team
      )
    ) %>%
    group_by(
      team
    ) %>%
    summarise(
      chase_matches = n(),

      chase_wins = sum(
        win_chasing,
        na.rm = TRUE
      ),

      chase_win_pct = ifelse(
        chase_matches > 0,
        round(
          chase_wins /
            chase_matches *
            100,
          1
        ),
        0
      ),

      .groups = "drop"
    )

  split_summary <- full_join(
    bat_1st,
    bat_2nd,
    by = "team"
  ) %>%
    mutate(
      bat_1st_matches = coalesce(
        bat_1st_matches,
        0L
      ),

      bat_1st_wins = coalesce(
        bat_1st_wins,
        0L
      ),

      chase_matches = coalesce(
        chase_matches,
        0L
      ),

      chase_wins = coalesce(
        chase_wins,
        0L
      ),

      bat_1st_win_pct = coalesce(
        bat_1st_win_pct,
        0
      ),

      chase_win_pct = coalesce(
        chase_win_pct,
        0
      )
    ) %>%
    arrange(
      desc(
        bat_1st_wins +
          chase_wins
      )
    )

  return(split_summary)
}