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
    match_data <- match_data %>%
      filter(
        season_year == as.numeric(season_filter)
      )
  }

  match_list <- match_data %>%
    distinct(
      match_id,
      date,
      season_display,
      batting_team,
      bowling_team,
      venue,
      match_won_by
    ) %>%
    arrange(desc(date), match_id) %>%
    mutate(
      label = paste0(
        date,
        " | ",
        batting_team,
        " vs ",
        bowling_team,
        " (",
        match_won_by,
        " won)"
      )
    )

  return(match_list)
}


#' Get Detailed Overview Header for Selected Match
get_match_summary_info <- function(
    df,
    selected_match_id
) {

  match_balls <- df %>%
    filter(match_id == selected_match_id)

  if (nrow(match_balls) == 0) {
    return(NULL)
  }

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

      inn1_team = first(
        batting_team[innings == 1]
      ),

      inn1_runs = sum(
        runs_total[innings == 1],
        na.rm = TRUE
      ),

      inn1_wickets = sum(
        is_wicket[innings == 1],
        na.rm = TRUE
      ),

      inn1_overs = max(
        over[innings == 1],
        na.rm = TRUE
      ) + 1,

      inn2_team = first(
        batting_team[innings == 2]
      ),

      inn2_runs = sum(
        runs_total[innings == 2],
        na.rm = TRUE
      ),

      inn2_wickets = sum(
        is_wicket[innings == 2],
        na.rm = TRUE
      ),

      inn2_overs = ifelse(
        any(innings == 2),
        max(
          over[innings == 2],
          na.rm = TRUE
        ) + 1,
        0
      )
    )

  return(match_info)
}


#' Get Match Worm Chart Data
get_match_worm_data <- function(
    df,
    selected_match_id
) {

  match_balls <- df %>%
    filter(
      match_id == selected_match_id,
      innings %in% c(1, 2)
    )

  worm_data <- match_balls %>%

    group_by(
      innings,
      batting_team,
      over_display
    ) %>%

    summarise(
      over_runs = sum(
        runs_total,
        na.rm = TRUE
      ),

      wickets_in_over = sum(
        is_wicket,
        na.rm = TRUE
      ),

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


#' Get Match Manhattan Chart Data
get_match_manhattan_data <- function(
    df,
    selected_match_id
) {

  manhattan_data <- df %>%

    filter(
      match_id == selected_match_id,
      innings %in% c(1, 2)
    ) %>%

    group_by(
      innings,
      batting_team,
      over_display
    ) %>%

    summarise(
      over_runs = sum(
        runs_total,
        na.rm = TRUE
      ),

      wickets = sum(
        is_wicket,
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  return(manhattan_data)
}


#' Get Batting and Bowling Scorecards for Selected Match
get_match_scorecard <- function(
    df,
    selected_match_id
) {

  match_balls <- df %>%
    filter(
      match_id == selected_match_id,
      innings %in% c(1, 2)
    )


  # ---------------------------------------------------------
  # Correct dismissal information
  # ---------------------------------------------------------

  dismissal_info <- match_balls %>%

    filter(
      !is.na(player_out),
      player_out != ""
    ) %>%

    group_by(
      innings,
      player_out
    ) %>%

    summarise(
      is_out = TRUE,

      # A player can only be dismissed once per innings.
      dismissal_ball = first(
        paste(match_id, ball_no)
      ),

      wicket_kind = first(
        na.omit(wicket_kind)
      ),

      bowler_out = first(
        na.omit(bowler)
      ),

      .groups = "drop"
    ) %>%

    rename(
      batter = player_out
    )


  # ---------------------------------------------------------
  # Batting Scorecard
  # ---------------------------------------------------------

  batting_scorecard <- match_balls %>%

    group_by(
      innings,
      batting_team,
      batter
    ) %>%

    summarise(
      runs = sum(
        runs_batter,
        na.rm = TRUE
      ),

      balls = sum(
        balls_faced == 1,
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

      strike_rate = ifelse(
        balls > 0,
        round(runs / balls * 100, 1),
        0
      ),

      .groups = "drop"
    ) %>%

    left_join(
      dismissal_info %>%
        select(
          innings,
          batter,
          is_out,
          wicket_kind,
          bowler_out
        ),
      by = c(
        "innings",
        "batter"
      )
    ) %>%

    mutate(
      is_out = coalesce(
        is_out,
        FALSE
      ),

      dismissal = case_when(

        !is_out ~ "not out",

        !is.na(wicket_kind) &
          !is.na(bowler_out) ~
          paste(
            wicket_kind,
            "b",
            bowler_out
          ),

        !is.na(wicket_kind) ~
          wicket_kind,

        TRUE ~ "out"
      )
    ) %>%

    arrange(
      innings,
      desc(runs)
    )


  # ---------------------------------------------------------
  # Bowling Scorecard
  # ---------------------------------------------------------

  bowling_scorecard <- match_balls %>%

    group_by(
      innings,
      bowling_team,
      bowler
    ) %>%

    summarise(
      valid_balls = sum(
        valid_ball == 1,
        na.rm = TRUE
      ),

      overs = paste0(
        valid_balls %/% 6,
        ".",
        valid_balls %% 6
      ),

      maidens = 0,

      runs_given = sum(
        runs_bowler,
        na.rm = TRUE
      ),

      wickets = sum(
        is_bowler_wicket,
        na.rm = TRUE
      ),

      economy = ifelse(
        valid_balls > 0,
        round(
          runs_given / (valid_balls / 6),
          2
        ),
        0
      ),

      dots = sum(
        is_dot_ball,
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    arrange(
      innings,
      desc(wickets),
      economy
    )


  return(
    list(
      batting = batting_scorecard,
      bowling = bowling_scorecard
    )
  )
}


#' Get Match Partnership Breakdown
get_match_partnerships <- function(
    df,
    selected_match_id,
    inn = 1
) {

  inn_balls <- df %>%
    filter(
      match_id == selected_match_id,
      innings == inn
    )

  if (nrow(inn_balls) == 0) {
    return(NULL)
  }

  partnerships <- inn_balls %>%

    group_by(
      batting_partners
    ) %>%

    summarise(
      runs = sum(
        runs_total,
        na.rm = TRUE
      ),

      balls = n(),

      batter_runs = sum(
        runs_batter,
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

      .groups = "drop"
    ) %>%

    filter(
      !is.na(batting_partners)
    ) %>%

    arrange(
      desc(runs)
    )

  return(partnerships)
}