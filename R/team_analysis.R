# R/team_analysis.R
# CricLens — Team Analytics Module
# Source of truth for team performance, head-to-head metrics, and toss impact calculations

suppressMessages({
  library(dplyr)
  library(tidyr)
})

#' Get Team Performance Summary Table
#' @param df Featured IPL dataframe
#' @param season_filter Optional season year filter
get_team_performance_summary <- function(df, season_filter = NULL) {
  data_filtered <- df
  if (!is.null(season_filter) && season_filter != "All") {
    data_filtered <- data_filtered %>% filter(season_year == as.numeric(season_filter))
  }

  match_distinct <- data_filtered %>%
    distinct(match_id, .keep_all = TRUE)

  # Teams played list
  teams_played <- match_distinct %>%
    select(match_id, batting_team, bowling_team) %>%
    pivot_longer(cols = c(batting_team, bowling_team), names_to = "role", values_to = "team") %>%
    group_by(team) %>%
    summarise(matches_played = length(unique(match_id)), .groups = "drop")

  # Team wins list
  team_wins <- match_distinct %>%
    filter(!is.na(match_won_by) & match_won_by != "Unknown") %>%
    group_by(team = match_won_by) %>%
    summarise(wins = n(), .groups = "drop")

  summary_table <- teams_played %>%
    left_join(team_wins, by = "team") %>%
    mutate(
      wins = coalesce(wins, 0L),
      losses = matches_played - wins,
      win_pct = ifelse(matches_played > 0, round(wins / matches_played * 100, 1), 0)
    ) %>%
    arrange(desc(wins), desc(win_pct))

  return(summary_table)
}

#' Get Head-to-Head Record between Team 1 and Team 2
#' @param df Featured IPL dataframe
#' @param team1 First team name
#' @param team2 Second team name
get_head_to_head <- function(df, team1, team2) {
  matches_h2h <- df %>%
    filter(
      (batting_team == team1 & bowling_team == team2) |
      (batting_team == team2 & bowling_team == team1)
    ) %>%
    distinct(match_id, date, season_display, venue, toss_winner, toss_decision, match_won_by)

  total_matches <- nrow(matches_h2h)
  team1_wins <- sum(matches_h2h$match_won_by == team1, na.rm = TRUE)
  team2_wins <- sum(matches_h2h$match_won_by == team2, na.rm = TRUE)
  no_result <- total_matches - team1_wins - team2_wins

  return(list(
    total_matches = total_matches,
    team1 = team1,
    team1_wins = team1_wins,
    team2 = team2,
    team2_wins = team2_wins,
    no_result = no_result,
    match_history = matches_h2h %>% arrange(desc(date))
  ))
}

#' Get Head-to-Head Pairwise Matrix for All Teams
get_head_to_head_matrix <- function(df) {
  matches <- df %>%
    distinct(match_id, batting_team, bowling_team, match_won_by) %>%
    filter(!is.na(match_won_by) & match_won_by != "Unknown")

  matrix_data <- matches %>%
    group_by(winner = match_won_by, opponent = ifelse(match_won_by == batting_team, bowling_team, batting_team)) %>%
    summarise(wins = n(), .groups = "drop")

  return(matrix_data)
}

#' Get Toss Decision & Impact Analytics
get_toss_impact <- function(df, team_name = NULL) {
  data_filtered <- df
  if (!is.null(team_name) && team_name != "All") {
    data_filtered <- data_filtered %>% filter(toss_winner == team_name)
  }

  match_distinct <- data_filtered %>%
    distinct(match_id, toss_winner, toss_decision, match_won_by) %>%
    filter(!is.na(match_won_by) & match_won_by != "Unknown")

  toss_summary <- match_distinct %>%
    group_by(toss_decision) %>%
    summarise(
      total_tosses = n(),
      won_match = sum(toss_winner == match_won_by, na.rm = TRUE),
      win_pct_after_toss = round(won_match / total_tosses * 100, 1),
      .groups = "drop"
    )

  return(toss_summary)
}

#' Get Batting 1st vs Chasing Performance Split
get_batting_1st_vs_chasing <- function(df, team_name = NULL) {
  match_distinct <- df %>%
    distinct(match_id, batting_team, bowling_team, match_won_by, innings)

  if (!is.null(team_name) && team_name != "All") {
    match_distinct <- match_distinct %>%
      filter(batting_team == team_name | bowling_team == team_name)
  }

  # Batting 1st matches
  bat_1st <- match_distinct %>%
    filter(innings == 1) %>%
    mutate(
      team = batting_team,
      win_bat1st = (match_won_by == batting_team)
    ) %>%
    group_by(team) %>%
    summarise(
      bat_1st_matches = n(),
      bat_1st_wins = sum(win_bat1st, na.rm = TRUE),
      bat_1st_win_pct = round(bat_1st_wins / bat_1st_matches * 100, 1),
      .groups = "drop"
    )

  # Chasing (Batting 2nd) matches
  bat_2nd <- match_distinct %>%
    filter(innings == 2) %>%
    mutate(
      team = batting_team,
      win_chasing = (match_won_by == batting_team)
    ) %>%
    group_by(team) %>%
    summarise(
      chase_matches = n(),
      chase_wins = sum(win_chasing, na.rm = TRUE),
      chase_win_pct = round(chase_wins / chase_matches * 100, 1),
      .groups = "drop"
    )

  split_summary <- bat_1st %>%
    inner_join(bat_2nd, by = "team") %>%
    arrange(desc(bat_1st_wins + chase_wins))

  return(split_summary)
}
