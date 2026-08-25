# R/player_analysis.R
# CricLens — Player Analytics Module
# Source of truth for all individual player (batting & bowling) analytical calculations

suppressMessages({
  library(dplyr)
  library(tidyr)
})

#' Get Top Batters with Filtering
#' @param df Featured IPL dataframe
#' @param top_n Number of batters to return (default 10)
#' @param season_filter Optional season year filter
#' @param min_balls Minimum balls faced threshold
get_top_batters <- function(df, top_n = 10, season_filter = NULL, min_balls = 0) {
  data_filtered <- df
  if (!is.null(season_filter) && season_filter != "All") {
    data_filtered <- data_filtered %>% filter(season_year == as.numeric(season_filter))
  }

  top_batters <- data_filtered %>%
    group_by(batter) %>%
    summarise(
      matches = length(unique(match_id)),
      innings = length(unique(paste(match_id, innings))),
      total_runs = sum(runs_batter, na.rm = TRUE),
      balls_faced = sum(balls_faced == 1, na.rm = TRUE),
      outs = sum(player_out == batter, na.rm = TRUE),
      average = ifelse(outs > 0, round(total_runs / outs, 2), total_runs),
      strike_rate = ifelse(balls_faced > 0, round(total_runs / balls_faced * 100, 2), 0),
      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      dot_balls = sum(is_dot_ball, na.rm = TRUE),
      dot_percent = ifelse(balls_faced > 0, round(dot_balls / balls_faced * 100, 1), 0),
      .groups = "drop"
    ) %>%
    filter(balls_faced >= min_balls) %>%
    arrange(desc(total_runs)) %>%
    head(top_n)

  return(top_batters)
}

#' Get Top Bowlers with Filtering
#' @param df Featured IPL dataframe
#' @param top_n Number of bowlers to return (default 10)
#' @param season_filter Optional season year filter
#' @param min_balls Minimum valid balls bowled threshold
get_top_bowlers <- function(df, top_n = 10, season_filter = NULL, min_balls = 0) {
  data_filtered <- df
  if (!is.null(season_filter) && season_filter != "All") {
    data_filtered <- data_filtered %>% filter(season_year == as.numeric(season_filter))
  }

  top_bowlers <- data_filtered %>%
    group_by(bowler) %>%
    summarise(
      matches = length(unique(match_id)),
      innings = length(unique(paste(match_id, innings))),
      wickets = sum(is_bowler_wicket, na.rm = TRUE),
      runs_given = sum(runs_bowler, na.rm = TRUE),
      valid_balls = sum(valid_ball == 1, na.rm = TRUE),
      overs_bowled = round(valid_balls / 6, 1),
      economy = ifelse(valid_balls > 0, round(runs_given / (valid_balls / 6), 2), 0),
      average = ifelse(wickets > 0, round(runs_given / wickets, 2), NA_real_),
      strike_rate = ifelse(wickets > 0, round(valid_balls / wickets, 2), NA_real_),
      dot_balls = sum(is_dot_ball, na.rm = TRUE),
      dot_percent = ifelse(valid_balls > 0, round(dot_balls / valid_balls * 100, 1), 0),
      .groups = "drop"
    ) %>%
    filter(valid_balls >= min_balls) %>%
    arrange(desc(wickets)) %>%
    head(top_n)

  return(top_bowlers)
}

#' Get Specific Player Batting Deep Dive
get_player_batting_profile <- function(df, player_name) {
  p_data <- df %>% filter(batter == player_name)
  if (nrow(p_data) == 0) return(NULL)

  profile <- p_data %>%
    summarise(
      player = player_name,
      matches = length(unique(match_id)),
      innings = length(unique(paste(match_id, innings))),
      total_runs = sum(runs_batter, na.rm = TRUE),
      balls_faced = sum(balls_faced == 1, na.rm = TRUE),
      outs = sum(player_out == player_name, na.rm = TRUE),
      average = ifelse(outs > 0, round(total_runs / outs, 2), total_runs),
      strike_rate = ifelse(balls_faced > 0, round(total_runs / balls_faced * 100, 2), 0),
      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      highest_score = max(tapply(runs_batter, match_id, sum, na.rm = TRUE), na.rm = TRUE),
      fifties = sum(tapply(runs_batter, match_id, function(x) sum(x) >= 50 & sum(x) < 100), na.rm = TRUE),
      hundreds = sum(tapply(runs_batter, match_id, function(x) sum(x) >= 100), na.rm = TRUE)
    )

  return(profile)
}

#' Get Player Performance Breakdown by Phase (Powerplay, Middle, Death)
get_player_phase_breakdown <- function(df, player_name) {
  bat_phase <- df %>%
    filter(batter == player_name) %>%
    group_by(match_phase) %>%
    summarise(
      runs = sum(runs_batter, na.rm = TRUE),
      balls = sum(balls_faced == 1, na.rm = TRUE),
      strike_rate = ifelse(balls > 0, round(runs / balls * 100, 2), 0),
      outs = sum(player_out == player_name, na.rm = TRUE),
      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      .groups = "drop"
    )

  bowl_phase <- df %>%
    filter(bowler == player_name) %>%
    group_by(match_phase) %>%
    summarise(
      wickets = sum(is_bowler_wicket, na.rm = TRUE),
      runs_given = sum(runs_bowler, na.rm = TRUE),
      balls = sum(valid_ball == 1, na.rm = TRUE),
      economy = ifelse(balls > 0, round(runs_given / (balls / 6), 2), 0),
      .groups = "drop"
    )

  return(list(batting = bat_phase, bowling = bowl_phase))
}

#' Head-to-head matchup: Batter vs Bowler
get_head_to_head_player <- function(df, batter_name, bowler_name) {
  matchup <- df %>%
    filter(batter == batter_name & bowler == bowler_name) %>%
    summarise(
      batter = batter_name,
      bowler = bowler_name,
      runs = sum(runs_batter, na.rm = TRUE),
      balls = sum(balls_faced == 1, na.rm = TRUE),
      strike_rate = ifelse(balls > 0, round(runs / balls * 100, 2), 0),
      outs = sum(player_out == batter_name, na.rm = TRUE),
      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      dot_balls = sum(is_dot_ball, na.rm = TRUE)
    )

  return(matchup)
}
