# R/venue_analysis.R
# CricLens — Venue Analytics Module
# Source of truth for venue statistics, stadium pitch profiles, toss bias, and scoring trends

suppressMessages({
  library(dplyr)
  library(tidyr)
})

#' Get Searchable Venue List
get_venue_list <- function(df) {
  venues <- df %>%
    group_by(venue, city) %>%
    summarise(
      matches = length(unique(match_id)),
      .groups = "drop"
    ) %>%
    arrange(desc(matches))

  return(venues)
}

#' Get Comprehensive Venue Profile & Pitch Analytics
#' @param df Featured IPL dataframe
#' @param selected_venue Venue string name
get_venue_profile <- function(df, selected_venue) {
  v_balls <- df %>% filter(venue == selected_venue)
  if (nrow(v_balls) == 0) return(NULL)

  v_matches <- v_balls %>%
    distinct(match_id, season_year, date, toss_winner, toss_decision, match_won_by)

  total_matches <- nrow(v_matches)

  # 1st Innings Runs distribution
  inn1_scores <- v_balls %>%
    filter(innings == 1) %>%
    group_by(match_id) %>%
    summarise(total_runs = sum(runs_total, na.rm = TRUE), .groups = "drop")

  avg_1st_inn <- round(mean(inn1_scores$total_runs, na.rm = TRUE), 1)
  highest_1st_inn <- max(inn1_scores$total_runs, na.rm = TRUE)
  lowest_1st_inn <- min(inn1_scores$total_runs, na.rm = TRUE)

  # Toss & Match Outcome Alignment
  chase_wins <- v_balls %>%
    filter(innings == 2) %>%
    distinct(match_id, batting_team, match_won_by) %>%
    summarise(chase_wins = sum(batting_team == match_won_by, na.rm = TRUE)) %>%
    pull(chase_wins)

  bat_1st_wins <- total_matches - chase_wins
  chase_win_pct <- ifelse(total_matches > 0, round(chase_wins / total_matches * 100, 1), 0)

  # Phase Run Rates at this venue
  phase_stats <- v_balls %>%
    group_by(match_phase) %>%
    summarise(
      runs = sum(runs_total, na.rm = TRUE),
      balls = sum(valid_ball == 1, na.rm = TRUE),
      wickets = sum(is_wicket, na.rm = TRUE),
      run_rate = ifelse(balls > 0, round(runs / (balls / 6), 2), 0),
      .groups = "drop"
    )

  return(list(
    venue = selected_venue,
    city = first(v_balls$city),
    total_matches = total_matches,
    avg_1st_inn = avg_1st_inn,
    highest_1st_inn = highest_1st_inn,
    lowest_1st_inn = lowest_1st_inn,
    bat_1st_wins = bat_1st_wins,
    chase_wins = chase_wins,
    chase_win_pct = chase_win_pct,
    phase_stats = phase_stats
  ))
}

#' Get Venue Comparative Leaderboard
get_venue_leaderboard <- function(df, min_matches = 10) {
  leaderboard <- df %>%
    group_by(venue, match_id) %>%
    summarise(
      inn1_runs = sum(runs_total[innings == 1], na.rm = TRUE),
      inn2_runs = sum(runs_total[innings == 2], na.rm = TRUE),
      chased_won = (first(match_won_by) == first(batting_team[innings == 2])),
      .groups = "drop_last"
    ) %>%
    summarise(
      matches = n(),
      avg_1st_inn_score = round(mean(inn1_runs[inn1_runs > 0], na.rm = TRUE), 1),
      avg_2nd_inn_score = round(mean(inn2_runs[inn2_runs > 0], na.rm = TRUE), 1),
      chasing_win_pct = round(mean(chased_won, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(matches >= min_matches) %>%
    arrange(desc(avg_1st_inn_score))

  return(leaderboard)
}
