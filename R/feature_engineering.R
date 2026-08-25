# R/feature_engineering.R
# CricLens — Feature Engineering Module
# Constructs analytical features, match phase tags, cumulative metrics, and cached summaries

suppressMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# Source cleaning module if functions aren't loaded
if (!exists("clean_ipl_data")) {
  source("R/data_cleaning.R")
}

#' Classify match phase based on over index (0-19)
classify_phase <- function(over_vec) {
  case_when(
    over_vec >= 0 & over_vec <= 5 ~ "Powerplay",
    over_vec >= 6 & over_vec <= 14 ~ "Middle Overs",
    over_vec >= 15 & over_vec <= 19 ~ "Death Overs",
    TRUE ~ "Super Over"
  )
}

#' Main Function: Engineer Features for IPL Dataset
#' @param df Cleaned IPL data frame
#' @return Data frame with enhanced analytical features
engineer_features <- function(df) {
  cat("Engineering match features, phase tags, and cumulative tracking...\n")

  df_feat <- df %>%
    # Add match phase tag
    mutate(
      match_phase = classify_phase(over),
      over_display = over + 1,
      is_dot_ball = (runs_total == 0 & valid_ball == 1),
      is_four = (runs_batter == 4 & runs_not_boundary == FALSE),
      is_six = (runs_batter == 6 & runs_not_boundary == FALSE)
    ) %>%
    # Sort chronologically by match, innings, over, ball
    arrange(match_id, innings, over, ball) %>%
    # Group by match and innings for cumulative metrics
    group_by(match_id, innings) %>%
    mutate(
      cum_valid_balls = cumsum(valid_ball),
      cum_team_runs = cumsum(runs_total),
      cum_team_wickets = cumsum(as.integer(is_wicket)),
      current_rr = ifelse(cum_valid_balls > 0, round(cum_team_runs / (cum_valid_balls / 6), 2), 0)
    ) %>%
    # Add Chasing / Target metrics for 2nd innings
    mutate(
      balls_remaining = pmax(0, 120 - cum_valid_balls),
      wickets_remaining = pmax(0, 10 - cum_team_wickets),
      runs_required = ifelse(!is.na(runs_target) & innings == 2, pmax(0, runs_target - cum_team_runs), NA_real_),
      req_rr = ifelse(innings == 2 & !is.na(runs_required) & balls_remaining > 0,
                      round(runs_required / (balls_remaining / 6), 2), NA_real_)
    ) %>%
    ungroup()

  cat("Feature engineering complete.\n")
  return(df_feat)
}

#' Generate and Cache Pre-computed Aggregations for High Performance Shiny UI
generate_analytics_caches <- function(df_feat, out_dir = "data/processed") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  cat("Generating pre-computed analytical summaries...\n")

  # 1. Player Batting Summary
  cat("  - Computing player batting summaries...\n")
  batting_summary <- df_feat %>%
    group_by(batter) %>%
    summarise(
      matches = length(unique(match_id)),
      innings = length(unique(paste(match_id, innings))),
      total_runs = sum(runs_batter, na.rm = TRUE),
      balls_faced = sum(balls_faced == 1, na.rm = TRUE),
      strike_rate = ifelse(balls_faced > 0, round(total_runs / balls_faced * 100, 2), 0),
      outs = sum(player_out == batter, na.rm = TRUE),
      average = ifelse(outs > 0, round(total_runs / outs, 2), total_runs),
      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      highest_score = max(tapply(runs_batter, match_id, sum, na.rm = TRUE), na.rm = TRUE),
      fifties = sum(tapply(runs_batter, match_id, function(x) sum(x) >= 50 & sum(x) < 100), na.rm = TRUE),
      hundreds = sum(tapply(runs_batter, match_id, function(x) sum(x) >= 100), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_runs))
  saveRDS(batting_summary, file.path(out_dir, "player_batting_summary.rds"))

  # 2. Player Bowling Summary
  cat("  - Computing player bowling summaries...\n")
  bowling_summary <- df_feat %>%
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
      .groups = "drop"
    ) %>%
    arrange(desc(wickets))
  saveRDS(bowling_summary, file.path(out_dir, "player_bowling_summary.rds"))

  # 3. Team Performance Summary
  cat("  - Computing team summaries...\n")
  match_distinct <- df_feat %>%
    distinct(match_id, .keep_all = TRUE)

  team_summary <- match_distinct %>%
    filter(!is.na(match_won_by) & match_won_by != "Unknown") %>%
    group_by(team = match_won_by) %>%
    summarise(
      total_wins = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(total_wins))
  saveRDS(team_summary, file.path(out_dir, "team_summary.rds"))

  # 4. Venue Summary
  cat("  - Computing venue summaries...\n")
  venue_summary <- df_feat %>%
    group_by(venue, match_id) %>%
    summarise(
      first_inn_runs = sum(runs_total[innings == 1], na.rm = TRUE),
      match_winner = first(match_won_by),
      toss_win = first(toss_winner),
      toss_dec = first(toss_decision),
      .groups = "drop_last"
    ) %>%
    summarise(
      total_matches = n(),
      avg_1st_inn_score = round(mean(first_inn_runs[first_inn_runs > 0], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(total_matches))
  saveRDS(venue_summary, file.path(out_dir, "venue_summary.rds"))

  cat("All analytical summary caches saved to:", out_dir, "\n")
}

# Run feature engineering if executed directly
if (sys.nframe() == 0) {
  cleaned_file <- "data/processed/ipl_cleaned.rds"
  if (!file.exists(cleaned_file)) {
    df_clean <- clean_ipl_data()
  } else {
    df_clean <- readRDS(cleaned_file)
  }

  df_feat <- engineer_features(df_clean)
  saveRDS(df_feat, "data/processed/ipl_featured.rds")
  cat("Saved featured dataset to data/processed/ipl_featured.rds\n")

  generate_analytics_caches(df_feat)
}
