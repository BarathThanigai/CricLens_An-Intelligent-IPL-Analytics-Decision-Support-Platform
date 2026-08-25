# R/match_analysis.R
# CricLens — Match Analytics Module
# Source of truth for match scorecards, worm charts, manhattan plots, and partnership analytics

suppressMessages({
  library(dplyr)
  library(tidyr)
})

#' Get Searchable Match List for Navigation & Selection
get_match_list <- function(df, season_filter = NULL) {
  match_data <- df
  if (!is.null(season_filter) && season_filter != "All") {
    match_data <- match_data %>% filter(season_year == as.numeric(season_filter))
  }

  match_list <- match_data %>%
    distinct(match_id, date, season_display, batting_team, bowling_team, venue, match_won_by) %>%
    arrange(desc(date), match_id) %>%
    mutate(
      label = paste0(date, " | ", batting_team, " vs ", bowling_team, " (", match_won_by, " won)")
    )

  return(match_list)
}

#' Get Detailed Overview Header for Selected Match
get_match_summary_info <- function(df, selected_match_id) {
  match_balls <- df %>% filter(match_id == selected_match_id)
  if (nrow(match_balls) == 0) return(NULL)

  match_info <- match_balls %>%
    summarise(
      match_id = first(match_id),
      date = first(date),
      season = first(season_display),
      venue = first(venue),
      city = first(city),
      toss_winner = first(toss_winner),
      toss_decision = first(toss_decision),
      match_won_by = first(match_won_by),
      player_of_match = first(player_of_match),
      inn1_team = first(batting_team[innings == 1]),
      inn1_runs = sum(runs_total[innings == 1], na.rm = TRUE),
      inn1_wickets = sum(is_wicket[innings == 1], na.rm = TRUE),
      inn1_overs = max(over[innings == 1], na.rm = TRUE) + 1,
      inn2_team = first(batting_team[innings == 2]),
      inn2_runs = sum(runs_total[innings == 2], na.rm = TRUE),
      inn2_wickets = sum(is_wicket[innings == 2], na.rm = TRUE),
      inn2_overs = ifelse(any(innings == 2), max(over[innings == 2], na.rm = TRUE) + 1, 0)
    )

  return(match_info)
}

#' Get Match Worm Chart Data (Cumulative Runs per Over)
get_match_worm_data <- function(df, selected_match_id) {
  match_balls <- df %>% filter(match_id == selected_match_id & innings %in% c(1, 2))

  worm_data <- match_balls %>%
    group_by(innings, batting_team, over_display) %>%
    summarise(
      over_runs = sum(runs_total, na.rm = TRUE),
      wickets_in_over = sum(is_wicket, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(innings) %>%
    mutate(
      cum_runs = cumsum(over_runs),
      cum_wickets = cumsum(wickets_in_over)
    ) %>%
    ungroup()

  return(worm_data)
}

#' Get Match Manhattan Chart Data (Runs Per Over)
get_match_manhattan_data <- function(df, selected_match_id) {
  manhattan_data <- df %>%
    filter(match_id == selected_match_id & innings %in% c(1, 2)) %>%
    group_by(innings, batting_team, over_display) %>%
    summarise(
      over_runs = sum(runs_total, na.rm = TRUE),
      wickets = sum(is_wicket, na.rm = TRUE),
      .groups = "drop"
    )

  return(manhattan_data)
}

#' Get Batting and Bowling Scorecards for Selected Match
get_match_scorecard <- function(df, selected_match_id) {
  match_balls <- df %>% filter(match_id == selected_match_id)

  # Innings 1 & 2 Batting
  batting_scorecard <- match_balls %>%
    group_by(innings, batting_team, batter) %>%
    summarise(
      runs = sum(runs_batter, na.rm = TRUE),
      balls = sum(balls_faced == 1, na.rm = TRUE),
      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      strike_rate = ifelse(balls > 0, round(runs / balls * 100, 1), 0),
      wicket_kind = first(na.omit(wicket_kind)),
      bowler_out = first(na.omit(bowler)),
      is_out = any(player_out == batter, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dismissal = case_when(
        !is_out ~ "not out",
        !is.na(wicket_kind) ~ paste(wicket_kind, ifelse(!is.na(bowler_out), paste("b", bowler_out), "")),
        TRUE ~ "out"
      )
    ) %>%
    arrange(innings, desc(runs))

  # Innings 1 & 2 Bowling
  bowling_scorecard <- match_balls %>%
    group_by(innings, bowling_team, bowler) %>%
    summarise(
      valid_balls = sum(valid_ball == 1, na.rm = TRUE),
      overs = paste0(valid_balls %/% 6, ".", valid_balls %% 6),
      maidens = 0, # Dot overs calculation
      runs_given = sum(runs_bowler, na.rm = TRUE),
      wickets = sum(is_bowler_wicket, na.rm = TRUE),
      economy = ifelse(valid_balls > 0, round(runs_given / (valid_balls / 6), 2), 0),
      dots = sum(is_dot_ball, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(innings, desc(wickets), economy)

  return(list(batting = batting_scorecard, bowling = bowling_scorecard))
}

#' Get Match Partnership Breakdown
get_match_partnerships <- function(df, selected_match_id, inn = 1) {
  inn_balls <- df %>% filter(match_id == selected_match_id & innings == inn)
  if (nrow(inn_balls) == 0) return(NULL)

  partnerships <- inn_balls %>%
    group_by(batting_partners) %>%
    summarise(
      runs = sum(runs_total, na.rm = TRUE),
      balls = n(),
      batter_runs = sum(runs_batter, na.rm = TRUE),
      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(batting_partners)) %>%
    arrange(desc(runs))

  return(partnerships)
}
