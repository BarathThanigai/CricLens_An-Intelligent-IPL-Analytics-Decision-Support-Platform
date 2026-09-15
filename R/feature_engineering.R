# R/feature_engineering.R
# CricLens — Feature Engineering Module
# Constructs analytical features, match phase tags,
# cumulative metrics, and cached analytical summaries.

suppressMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# Source cleaning module if functions aren't loaded
if (!exists("clean_ipl_data")) {
  source("R/data_cleaning.R")
}


# =========================================================
# 1. MATCH PHASE CLASSIFICATION
# =========================================================

#' Classify match phase
#'
#' Regular IPL innings:
#'   Overs 0-5   -> Powerplay
#'   Overs 6-14  -> Middle Overs
#'   Overs 15-19 -> Death Overs
#'
#' Super Overs are identified using innings > 2.
#'
#' @param over_vec Over number
#' @param innings_vec Innings number
#' @return Character vector of phase labels
classify_phase <- function(over_vec, innings_vec) {

  case_when(
    innings_vec > 2 ~ "Super Over",
    over_vec >= 0 & over_vec <= 5 ~ "Powerplay",
    over_vec >= 6 & over_vec <= 14 ~ "Middle Overs",
    over_vec >= 15 & over_vec <= 19 ~ "Death Overs",
    TRUE ~ "Unknown"
  )
}


# =========================================================
# 2. FEATURE ENGINEERING
# =========================================================

#' Engineer analytical features for the IPL dataset
#'
#' Adds delivery-level indicators, match phases,
#' cumulative innings metrics, and chasing metrics.
#'
#' @param df Cleaned IPL data frame
#' @return Feature-engineered data frame
engineer_features <- function(df) {

  cat("Engineering match features, phase tags, and cumulative tracking...\n")

  df_feat <- df %>%

    mutate(
      # Explicit Super Over identification
      is_super_over = innings > 2,

      # Match phase
      match_phase = classify_phase(
        over,
        innings
      ),

      # Human-readable over number
      over_display = over + 1,

      # Delivery-level indicators
      is_dot_ball = (
        runs_total == 0 &
          valid_ball == 1
      ),

      is_four = (
        runs_batter == 4 &
          runs_not_boundary == FALSE
      ),

      is_six = (
        runs_batter == 6 &
          runs_not_boundary == FALSE
      )
    ) %>%

    # Maintain chronological delivery order
    arrange(
      match_id,
      innings,
      over,
      ball
    ) %>%

    # Cumulative metrics within each innings
    group_by(
      match_id,
      innings
    ) %>%

    mutate(
      cum_valid_balls = cumsum(valid_ball),

      cum_team_runs = cumsum(
        runs_total
      ),

      cum_team_wickets = cumsum(
        as.integer(is_wicket)
      ),

      current_rr = ifelse(
        cum_valid_balls > 0,
        round(
          cum_team_runs /
            (cum_valid_balls / 6),
          2
        ),
        0
      )
    ) %>%

    # Chasing-specific metrics
    mutate(
      # Standard IPL regulation innings are 20 overs.
      # These fields are only meaningful for the second innings.
      balls_remaining = ifelse(
        innings == 2,
        pmax(
          0,
          120 - cum_valid_balls
        ),
        NA_real_
      ),

      wickets_remaining = ifelse(
        innings == 2,
        pmax(
          0,
          10 - cum_team_wickets
        ),
        NA_real_
      ),

      runs_required = ifelse(
        innings == 2 &
          !is.na(runs_target),
        pmax(
          0,
          runs_target - cum_team_runs
        ),
        NA_real_
      ),

      req_rr = ifelse(
        innings == 2 &
          !is.na(runs_required) &
          balls_remaining > 0,
        round(
          runs_required /
            (balls_remaining / 6),
          2
        ),
        NA_real_
      )
    ) %>%

    ungroup()

  cat("Feature engineering complete.\n")

  return(df_feat)
}


# =========================================================
# 3. ANALYTICS CACHE GENERATION
# =========================================================

#' Generate pre-computed analytical summaries
#'
#' These RDS files are used by the Shiny application
#' for fast analytical queries and visualizations.
generate_analytics_caches <- function(
    df_feat,
    out_dir = "data/processed"
) {

  if (!dir.exists(out_dir)) {
    dir.create(
      out_dir,
      recursive = TRUE
    )
  }

  cat(
    "Generating pre-computed analytical summaries...\n"
  )


  # =======================================================
  # 3.1 PLAYER BATTING SUMMARY
  # =======================================================

  cat(
    "  - Computing player batting summaries...\n"
  )

  # Only standard IPL innings contribute to
  # standard batting statistics.
  standard_batting <- df_feat %>%
    filter(
      innings %in% c(1, 2)
    )

  # First calculate one row per player per innings.
  # This is required for correct highest score,
  # fifties, and hundreds.
  player_innings <- standard_batting %>%
    group_by(
      match_id,
      innings,
      batter
    ) %>%
    summarise(
      innings_runs = sum(
        runs_batter,
        na.rm = TRUE
      ),

      innings_balls = sum(
        balls_faced == 1,
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  batting_summary <- standard_batting %>%

    group_by(batter) %>%

    summarise(
      matches = n_distinct(match_id),

      innings = n_distinct(
        paste(
          match_id,
          innings
        )
      ),

      total_runs = sum(
        runs_batter,
        na.rm = TRUE
      ),

      balls_faced = sum(
        balls_faced == 1,
        na.rm = TRUE
      ),

      strike_rate = ifelse(
        balls_faced > 0,
        round(
          total_runs /
            balls_faced *
            100,
          2
        ),
        0
      ),

      fours = sum(
        is_four,
        na.rm = TRUE
      ),

      sixes = sum(
        is_six,
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  # Correct innings-based scoring statistics
  innings_score_summary <- player_innings %>%
    group_by(batter) %>%
    summarise(

      highest_score = max(
        innings_runs,
        na.rm = TRUE
      ),

      fifties = sum(
        innings_runs >= 50 &
          innings_runs < 100,
        na.rm = TRUE
      ),

      hundreds = sum(
        innings_runs >= 100,
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  # Correct dismissal calculation.
  #
  # IMPORTANT:
  # A player can be dismissed while being the non-striker.
  #
  # Therefore dismissal information must be calculated
  # from ALL delivery rows.
  dismissal_summary <- df_feat %>%

    filter(
      innings %in% c(1, 2),
      !is.na(player_out),
      player_out != ""
    ) %>%

    group_by(player_out) %>%

    summarise(
      outs = n(),
      .groups = "drop"
    ) %>%

    rename(
      batter = player_out
    )

  batting_summary <- batting_summary %>%

    left_join(
      innings_score_summary,
      by = "batter"
    ) %>%

    left_join(
      dismissal_summary,
      by = "batter"
    ) %>%

    mutate(
      outs = coalesce(
        outs,
        0L
      ),

      highest_score = coalesce(
        highest_score,
        0
      ),

      fifties = coalesce(
        fifties,
        0L
      ),

      hundreds = coalesce(
        hundreds,
        0L
      ),

      average = ifelse(
        outs > 0,
        round(
          total_runs / outs,
          2
        ),
        total_runs
      )
    ) %>%

    arrange(
      desc(total_runs)
    )

  saveRDS(
    batting_summary,
    file.path(
      out_dir,
      "player_batting_summary.rds"
    )
  )


  # =======================================================
  # 3.2 PLAYER BOWLING SUMMARY
  # =======================================================

  cat(
    "  - Computing player bowling summaries...\n"
  )

  bowling_summary <- df_feat %>%

    filter(
      innings %in% c(1, 2)
    ) %>%

    group_by(bowler) %>%

    summarise(

      matches = n_distinct(
        match_id
      ),

      innings = n_distinct(
        paste(
          match_id,
          innings
        )
      ),

      wickets = sum(
        is_bowler_wicket,
        na.rm = TRUE
      ),

      runs_given = sum(
        runs_bowler,
        na.rm = TRUE
      ),

      valid_balls = sum(
        valid_ball == 1,
        na.rm = TRUE
      ),

      overs_bowled = round(
        valid_balls / 6,
        1
      ),

      economy = ifelse(
        valid_balls > 0,
        round(
          runs_given /
            (valid_balls / 6),
          2
        ),
        0
      ),

      average = ifelse(
        wickets > 0,
        round(
          runs_given /
            wickets,
          2
        ),
        NA_real_
      ),

      strike_rate = ifelse(
        wickets > 0,
        round(
          valid_balls /
            wickets,
          2
        ),
        NA_real_
      ),

      dot_balls = sum(
        is_dot_ball,
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    arrange(
      desc(wickets)
    )

  saveRDS(
    bowling_summary,
    file.path(
      out_dir,
      "player_bowling_summary.rds"
    )
  )


  # =======================================================
  # 3.3 TEAM PERFORMANCE SUMMARY
  # =======================================================

  cat(
    "  - Computing team summaries...\n"
  )

  match_distinct <- df_feat %>%
    distinct(
      match_id,
      .keep_all = TRUE
    )

  team_summary <- match_distinct %>%

    filter(
      !is.na(match_won_by),
      match_won_by != "Unknown"
    ) %>%

    group_by(
      team = match_won_by
    ) %>%

    summarise(
      total_wins = n(),
      .groups = "drop"
    ) %>%

    arrange(
      desc(total_wins)
    )

  saveRDS(
    team_summary,
    file.path(
      out_dir,
      "team_summary.rds"
    )
  )


  # =======================================================
  # 3.4 VENUE SUMMARY
  # =======================================================

  cat(
    "  - Computing venue summaries...\n"
  )

  venue_match_summary <- df_feat %>%

    filter(
      innings %in% c(1, 2)
    ) %>%

    group_by(
      venue,
      match_id
    ) %>%

    summarise(

      first_inn_runs = sum(
        runs_total[
          innings == 1
        ],
        na.rm = TRUE
      ),

      match_winner = first(
        match_won_by
      ),

      toss_win = first(
        toss_winner
      ),

      toss_dec = first(
        toss_decision
      ),

      .groups = "drop"
    )

  venue_summary <- venue_match_summary %>%

    group_by(venue) %>%

    summarise(

      total_matches = n(),

      avg_1st_inn_score = ifelse(
        sum(
          first_inn_runs > 0,
          na.rm = TRUE
        ) > 0,

        round(
          mean(
            first_inn_runs[
              first_inn_runs > 0
            ],
            na.rm = TRUE
          ),
          1
        ),

        NA_real_
      ),

      .groups = "drop"
    ) %>%

    arrange(
      desc(total_matches)
    )

  saveRDS(
    venue_summary,
    file.path(
      out_dir,
      "venue_summary.rds"
    )
  )


  # =======================================================
  # COMPLETION
  # =======================================================

  cat(
    "All analytical summary caches saved to:",
    out_dir,
    "\n"
  )
}


# =========================================================
# 4. DIRECT EXECUTION
# =========================================================

if (sys.nframe() == 0) {

  cleaned_file <- "data/processed/ipl_cleaned.rds"

  if (!file.exists(cleaned_file)) {

    df_clean <- clean_ipl_data()

  } else {

    df_clean <- readRDS(
      cleaned_file
    )
  }

  df_feat <- engineer_features(
    df_clean
  )

  saveRDS(
    df_feat,
    "data/processed/ipl_featured.rds"
  )

  cat(
    "Saved featured dataset to data/processed/ipl_featured.rds\n"
  )

  generate_analytics_caches(
    df_feat
  )
}