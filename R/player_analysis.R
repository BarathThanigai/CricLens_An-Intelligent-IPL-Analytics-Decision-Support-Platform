# R/player_analysis.R
# CricLens — Player Analytics Module
# Source of truth for all individual player (batting & bowling) analytical calculations

suppressMessages({
  library(dplyr)
  library(tidyr)
})


#' Get Top Batters with Filtering
get_top_batters <- function(
    df,
    top_n = 10,
    season_filter = NULL,
    min_balls = 0
) {

  data_filtered <- df

  if (!is.null(season_filter) && season_filter != "All") {
    data_filtered <- data_filtered %>%
      filter(
        season_year == as.numeric(season_filter)
      )
  }

  # Standard batting statistics exclude Super Overs
  batting_data <- data_filtered %>%
    filter(innings %in% c(1, 2))

  top_batters <- batting_data %>%
    group_by(batter) %>%
    summarise(
      matches = length(unique(match_id)),
      innings = length(unique(paste(match_id, innings))),

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
        round(total_runs / balls_faced * 100, 2),
        0
      ),

      fours = sum(is_four, na.rm = TRUE),
      sixes = sum(is_six, na.rm = TRUE),
      dot_balls = sum(is_dot_ball, na.rm = TRUE),

      dot_percent = ifelse(
        balls_faced > 0,
        round(dot_balls / balls_faced * 100, 1),
        0
      ),

      .groups = "drop"
    )

  # Calculate dismissals independently from batter rows.
  # This correctly includes cases where a batter is dismissed
  # as the non-striker in a run-out.
  dismissal_summary <- batting_data %>%
    filter(
      !is.na(player_out),
      player_out != ""
    ) %>%
    group_by(player_out) %>%
    summarise(
      outs = n(),
      .groups = "drop"
    ) %>%
    rename(batter = player_out)

  top_batters <- top_batters %>%
    left_join(
      dismissal_summary,
      by = "batter"
    ) %>%
    mutate(
      outs = coalesce(outs, 0L),

      average = ifelse(
        outs > 0,
        round(total_runs / outs, 2),
        total_runs
      )
    ) %>%
    filter(
      balls_faced >= min_balls
    ) %>%
    arrange(desc(total_runs)) %>%
    head(top_n)

  return(top_batters)
}


#' Get Top Bowlers with Filtering
get_top_bowlers <- function(
    df,
    top_n = 10,
    season_filter = NULL,
    min_balls = 0
) {

  data_filtered <- df

  if (!is.null(season_filter) && season_filter != "All") {
    data_filtered <- data_filtered %>%
      filter(
        season_year == as.numeric(season_filter)
      )
  }

  # Standard bowling statistics exclude Super Overs
  data_filtered <- data_filtered %>%
    filter(innings %in% c(1, 2))

  top_bowlers <- data_filtered %>%
    group_by(bowler) %>%
    summarise(
      matches = length(unique(match_id)),
      innings = length(unique(paste(match_id, innings))),

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
          runs_given / (valid_balls / 6),
          2
        ),
        0
      ),

      average = ifelse(
        wickets > 0,
        round(
          runs_given / wickets,
          2
        ),
        NA_real_
      ),

      strike_rate = ifelse(
        wickets > 0,
        round(
          valid_balls / wickets,
          2
        ),
        NA_real_
      ),

      dot_balls = sum(
        is_dot_ball,
        na.rm = TRUE
      ),

      dot_percent = ifelse(
        valid_balls > 0,
        round(
          dot_balls / valid_balls * 100,
          1
        ),
        0
      ),

      .groups = "drop"
    ) %>%
    filter(
      valid_balls >= min_balls
    ) %>%
    arrange(desc(wickets)) %>%
    head(top_n)

  return(top_bowlers)
}


#' Get Specific Player Batting Deep Dive
get_player_batting_profile <- function(
    df,
    player_name
) {

  # Standard batting statistics exclude Super Overs
  p_data <- df %>%
    filter(
      batter == player_name,
      innings %in% c(1, 2)
    )

  if (nrow(p_data) == 0) {
    return(NULL)
  }

  profile <- p_data %>%
    summarise(
      player = player_name,

      matches = length(unique(match_id)),

      innings = length(
        unique(paste(match_id, innings))
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
          total_runs / balls_faced * 100,
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

      highest_score = max(
        tapply(
          runs_batter,
          match_id,
          sum,
          na.rm = TRUE
        ),
        na.rm = TRUE
      ),

      fifties = sum(
        tapply(
          runs_batter,
          match_id,
          function(x)
            sum(x) >= 50 &&
            sum(x) < 100
        ),
        na.rm = TRUE
      ),

      hundreds = sum(
        tapply(
          runs_batter,
          match_id,
          function(x)
            sum(x) >= 100
        ),
        na.rm = TRUE
      )
    )

  # Calculate dismissals from the full dataframe.
  #
  # This is important because a batter can be dismissed while
  # appearing in the non_striker column of a run-out delivery.
  #
  # We therefore use player_out directly instead of restricting
  # the dismissal search to rows where batter == player_name.
  player_outs <- df %>%
    filter(
      innings %in% c(1, 2),
      player_out == player_name
    ) %>%
    nrow()

  profile$outs <- player_outs

  profile$average <- ifelse(
    player_outs > 0,
    round(
      profile$total_runs / player_outs,
      2
    ),
    profile$total_runs
  )

  return(profile)
}


#' Get Player Performance Breakdown by Phase
get_player_phase_breakdown <- function(
    df,
    player_name
) {

  # Standard batting statistics exclude Super Overs
  bat_phase <- df %>%
    filter(
      batter == player_name,
      innings %in% c(1, 2)
    ) %>%
    group_by(match_phase) %>%
    summarise(
      runs = sum(
        runs_batter,
        na.rm = TRUE
      ),

      balls = sum(
        balls_faced == 1,
        na.rm = TRUE
      ),

      strike_rate = ifelse(
        balls > 0,
        round(
          runs / balls * 100,
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

  # Calculate dismissals by phase using player_out directly.
  # This includes non-striker run-outs.
  phase_outs <- df %>%
    filter(
      player_out == player_name,
      innings %in% c(1, 2)
    ) %>%
    group_by(match_phase) %>%
    summarise(
      outs = n(),
      .groups = "drop"
    )

  bat_phase <- bat_phase %>%
    left_join(
      phase_outs,
      by = "match_phase"
    ) %>%
    mutate(
      outs = coalesce(
        outs,
        0L
      )
    )

  # Standard bowling statistics exclude Super Overs
  bowl_phase <- df %>%
    filter(
      bowler == player_name,
      innings %in% c(1, 2)
    ) %>%
    group_by(match_phase) %>%
    summarise(
      wickets = sum(
        is_bowler_wicket,
        na.rm = TRUE
      ),

      runs_given = sum(
        runs_bowler,
        na.rm = TRUE
      ),

      balls = sum(
        valid_ball == 1,
        na.rm = TRUE
      ),

      economy = ifelse(
        balls > 0,
        round(
          runs_given / (balls / 6),
          2
        ),
        0
      ),

      .groups = "drop"
    )

  return(
    list(
      batting = bat_phase,
      bowling = bowl_phase
    )
  )
}


#' Head-to-head matchup: Batter vs Bowler
get_head_to_head_player <- function(
    df,
    batter_name,
    bowler_name
) {

  # Standard IPL innings only
  matchup_data <- df %>%
    filter(
      batter == batter_name,
      bowler == bowler_name,
      innings %in% c(1, 2)
    )

  matchup <- matchup_data %>%
    summarise(
      batter = batter_name,
      bowler = bowler_name,

      runs = sum(
        runs_batter,
        na.rm = TRUE
      ),

      balls = sum(
        balls_faced == 1,
        na.rm = TRUE
      ),

      strike_rate = ifelse(
        balls > 0,
        round(
          runs / balls * 100,
          2
        ),
        0
      ),

      # For batter-vs-bowler analysis, count dismissals
      # occurring in the actual matchup rows.
      #
      # This avoids attributing a non-striker run-out to the
      # bowler when the dismissal was not caused by the bowler.
      outs = sum(
        player_out == batter_name,
        na.rm = TRUE
      ),

      fours = sum(
        is_four,
        na.rm = TRUE
      ),

      sixes = sum(
        is_six,
        na.rm = TRUE
      ),

      dot_balls = sum(
        is_dot_ball,
        na.rm = TRUE
      )
    )

  return(matchup)
}


#' Score a player's batting and/or bowling impact within the supplied cohort.
#' The score is a relative index (50 = cohort average), not an official rating.
get_player_impact_scores <- function(df, batting_weight = 0.6, bowling_weight = 0.4,
                                     min_batting_balls = 30, min_bowling_balls = 12) {
  batting_weight <- max(0, min(1, as.numeric(batting_weight)))
  bowling_weight <- max(0, min(1, as.numeric(bowling_weight)))
  if (batting_weight + bowling_weight <= 0) {
    return(data.frame(player = character(), impact = numeric(), role = character(),
                      batting_component = numeric(), bowling_component = numeric(),
                      batting_balls = integer(), bowling_balls = integer()))
  }

  d <- df %>% filter(innings %in% c(1, 2))
  bat <- d %>% group_by(player = batter) %>% summarise(
    batting_balls = sum(balls_faced == 1, na.rm = TRUE),
    runs = sum(runs_batter, na.rm = TRUE),
    outs = sum(player_out == first(player), na.rm = TRUE),
    average = ifelse(outs > 0, runs / outs, NA_real_),
    strike_rate = ifelse(batting_balls > 0, 100 * runs / batting_balls, NA_real_),
    .groups = "drop"
  ) %>% filter(!is.na(player), player != "", batting_balls >= min_batting_balls)

  bowl <- d %>% group_by(player = bowler) %>% summarise(
    bowling_balls = sum(valid_ball == 1, na.rm = TRUE),
    runs_conceded = sum(runs_bowler, na.rm = TRUE),
    wickets = sum(is_bowler_wicket, na.rm = TRUE),
    economy = ifelse(bowling_balls > 0, 6 * runs_conceded / bowling_balls, NA_real_),
    wicket_rate = ifelse(bowling_balls > 0, 6 * wickets / bowling_balls, NA_real_),
    .groups = "drop"
  ) %>% filter(!is.na(player), player != "", bowling_balls >= min_bowling_balls)

  z <- function(x, reverse = FALSE) {
    if (reverse) x <- -x
    s <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(rep(0, length(x)))
    as.numeric((x - mean(x, na.rm = TRUE)) / s)
  }
  if (nrow(bat) >= 2) {
    bat$batting_component <- rowMeans(cbind(z(bat$average), z(bat$strike_rate)), na.rm = TRUE)
    bat$batting_component[!is.finite(bat$batting_component)] <- NA_real_
    bat$batting_component <- bat$batting_component * pmin(1, sqrt(bat$batting_balls / 120))
  } else bat$batting_component <- NA_real_
  if (nrow(bowl) >= 2) {
    bowl$bowling_component <- rowMeans(cbind(z(bowl$wicket_rate), z(bowl$economy, reverse = TRUE)), na.rm = TRUE)
    bowl$bowling_component[!is.finite(bowl$bowling_component)] <- NA_real_
    bowl$bowling_component <- bowl$bowling_component * pmin(1, sqrt(bowl$bowling_balls / 48))
  } else bowl$bowling_component <- NA_real_

  players <- sort(unique(c(bat$player, bowl$player)))
  out <- data.frame(player = players, stringsAsFactors = FALSE) %>%
    left_join(bat %>% select(player, batting_balls, batting_component), by = "player") %>%
    left_join(bowl %>% select(player, bowling_balls, bowling_component), by = "player") %>%
    mutate(
      has_bat = !is.na(batting_component), has_bowl = !is.na(bowling_component),
      used_bat_weight = ifelse(has_bat, batting_weight, 0),
      used_bowl_weight = ifelse(has_bowl, bowling_weight, 0),
      weight_total = used_bat_weight + used_bowl_weight,
      impact = ifelse(weight_total > 0,
                      50 + 10 * (coalesce(batting_component, 0) * used_bat_weight +
                                   coalesce(bowling_component, 0) * used_bowl_weight) / weight_total,
                      NA_real_),
      impact = pmax(0, pmin(100, impact)),
      role = case_when(has_bat & has_bowl ~ "All-rounder", has_bat ~ "Batter",
                       has_bowl ~ "Bowler", TRUE ~ "Insufficient sample")
    ) %>% select(player, role, impact, batting_component, bowling_component,
                 batting_balls, bowling_balls)
  out
}

weight_player_impact_scores <- function(scores, batting_weight = 0.6, bowling_weight = 0.4) {
  if (!nrow(scores)) return(scores)
  batting_weight <- max(0, min(1, as.numeric(batting_weight)))
  bowling_weight <- max(0, min(1, as.numeric(bowling_weight)))
  has_bat <- !is.na(scores$batting_component)
  has_bowl <- !is.na(scores$bowling_component)
  wb <- ifelse(has_bat, batting_weight, 0)
  ww <- ifelse(has_bowl, bowling_weight, 0)
  total <- wb + ww
  component <- (ifelse(has_bat, scores$batting_component, 0) * wb +
                  ifelse(has_bowl, scores$bowling_component, 0) * ww) / ifelse(total > 0, total, NA_real_)
  scores$impact <- pmax(0, pmin(100, 50 + 10 * component))
  scores$impact[total <= 0] <- NA_real_
  scores
}


#' Return role-appropriate profile rows for two players for a H2H comparison.
get_player_role_profiles <- function(df, player_names, venue = NULL) {
  d <- df %>% filter(innings %in% c(1, 2))
  if (!is.null(venue) && length(venue) == 1 && !is.na(venue) && nzchar(venue)) {
    d <- d %>% filter(.data$venue == .env$venue)
  }
  profiles <- list()
  for (p in unique(player_names)) {
    b <- d %>% filter(batter == p)
    if (sum(b$balls_faced == 1, na.rm = TRUE) > 0) {
      runs <- sum(b$runs_batter, na.rm = TRUE)
      balls <- sum(b$balls_faced == 1, na.rm = TRUE)
      outs <- sum(d$player_out == p, na.rm = TRUE)
      profiles[[length(profiles) + 1L]] <- data.frame(
        player = p, role = "Batting", matches = dplyr::n_distinct(b$match_id),
        innings = dplyr::n_distinct(paste(b$match_id, b$innings)), balls = balls,
        runs_or_conceded = runs, wickets = NA_integer_,
        average = if (outs > 0) runs / outs else NA_real_,
        rate = if (balls > 0) 100 * runs / balls else NA_real_,
        fours = sum(b$is_four, na.rm = TRUE), sixes = sum(b$is_six, na.rm = TRUE),
        boundary_pct = if (balls > 0) 100 * (sum(b$is_four, na.rm = TRUE) + sum(b$is_six, na.rm = TRUE)) / balls else NA_real_,
        stringsAsFactors = FALSE)
    }
    w <- d %>% filter(bowler == p)
    balls <- sum(w$valid_ball == 1, na.rm = TRUE)
    wickets <- sum(w$is_bowler_wicket, na.rm = TRUE)
    if (balls > 0) {
      conceded <- sum(w$runs_bowler, na.rm = TRUE)
      profiles[[length(profiles) + 1L]] <- data.frame(
        player = p, role = "Bowling", matches = dplyr::n_distinct(w$match_id),
        innings = dplyr::n_distinct(paste(w$match_id, w$innings)), balls = balls,
        runs_or_conceded = conceded, wickets = wickets,
        average = if (wickets > 0) conceded / wickets else NA_real_,
        rate = 6 * conceded / balls, fours = NA_integer_, sixes = NA_integer_,
        boundary_pct = if (wickets > 0) balls / wickets else NA_real_,
        stringsAsFactors = FALSE)
    }
  }
  if (!length(profiles)) return(data.frame())
  do.call(rbind, profiles)
}

#' Summarize direct batter-versus-bowler encounters for a two-player matchup.
get_player_batter_bowler_matchup <- function(df, player_names) {
  players <- unique(as.character(player_names))
  players <- players[!is.na(players) & nzchar(players)]
  if (length(players) != 2L) return(data.frame())

  d <- df %>% filter(innings %in% c(1, 2))

  rows <- lapply(seq_len(2L), function(i) {
    batter <- players[[i]]
    bowler <- players[[3L - i]]
    # Use .env explicitly: bare `batter` / `bowler` names are also dataframe
    # columns, so data-mask lookup would otherwise compare each column to itself.
    balls <- d %>% filter(.data$batter == .env$batter, .data$bowler == .env$bowler)
    faced <- sum(balls$balls_faced == 1, na.rm = TRUE)
    legal <- sum(balls$valid_ball == 1, na.rm = TRUE)
    runs <- sum(balls$runs_batter, na.rm = TRUE)
    conceded <- sum(balls$runs_bowler, na.rm = TRUE)
    wickets <- sum(balls$is_bowler_wicket == 1 & balls$player_out == batter, na.rm = TRUE)

    data.frame(
      Batter = batter,
      Bowler = bowler,
      Matchup = paste(batter, "vs", bowler),
      Matches = dplyr::n_distinct(balls$match_id),
      Runs = runs,
      `Balls faced` = faced,
      `Strike rate` = if (faced > 0) round(100 * runs / faced, 1) else NA_real_,
      `Dismissals by bowler` = wickets,
      `Bowler economy` = if (legal > 0) round(6 * conceded / legal, 2) else NA_real_,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
