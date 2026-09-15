# R/match_analysis.R
# CricLens — Match Analytics Module
# Source of truth for match scorecards, worm charts,
# Manhattan plots, and partnership analytics

suppressMessages({
  library(dplyr)
  library(tidyr)
})


# =========================================================
# 1. MATCH LIST
# =========================================================

#' Get searchable match list for navigation and selection
get_match_list <- function(
    df,
    season_filter = NULL
) {

  match_data <- df

  if (!is.null(season_filter) &&
      season_filter != "All") {

    match_data <- match_data %>%
      filter(
        season_year ==
          as.numeric(season_filter)
      )
  }

  # Build exactly one metadata record per match.
  match_list <- match_data %>%

    group_by(match_id) %>%

    summarise(

      date = first(date),

      season_display =
        first(season_display),

      venue =
        first(venue),

      match_won_by =
        first(match_won_by),

      # Teams are obtained from the actual
      # first two standard innings.
      batting_team =
        first(
          batting_team[
            innings == 1
          ]
        ),

      bowling_team =
        first(
          bowling_team[
            innings == 1
          ]
        ),

      .groups = "drop"
    ) %>%

    arrange(
      desc(date),
      match_id
    ) %>%

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


# =========================================================
# 2. MATCH SUMMARY
# =========================================================

#' Get detailed overview information for a selected match
get_match_summary_info <- function(
    df,
    selected_match_id
) {

  match_balls <- df %>%
    filter(
      match_id ==
        selected_match_id
    )

  if (nrow(match_balls) == 0) {
    return(NULL)
  }

  # Work only with standard IPL innings
  # for the main scorecard.
  standard_balls <- match_balls %>%
    filter(
      innings %in% c(1, 2)
    )

  if (nrow(standard_balls) == 0) {
    return(NULL)
  }

  # One record containing match-level metadata.
  metadata <- standard_balls %>%
    summarise(

      match_id =
        first(match_id),

      date =
        first(date),

      season =
        first(season_display),

      venue =
        first(venue),

      city =
        first(city),

      toss_winner =
        first(toss_winner),

      toss_decision =
        first(toss_decision),

      match_won_by =
        first(match_won_by),

      player_of_match =
        first(player_of_match)
    )


  # Calculate each standard innings independently.
  innings_summary <- standard_balls %>%

    group_by(innings) %>%

    summarise(

      team =
        first(batting_team),

      runs =
        sum(
          runs_total,
          na.rm = TRUE
        ),

      wickets =
        sum(
          is_wicket,
          na.rm = TRUE
        ),

      valid_balls =
        sum(
          valid_ball == 1,
          na.rm = TRUE
        ),

      .groups = "drop"
    ) %>%

    mutate(

      overs = paste0(
        valid_balls %/% 6,
        ".",
        valid_balls %% 6
      )
    )


  # Extract first and second innings safely.
  get_innings_value <- function(
      data,
      inn,
      column
  ) {

    rows <- data %>%
      filter(
        innings == inn
      )

    if (nrow(rows) == 0) {
      return(NA)
    }

    rows[[column]][1]
  }


  match_info <- metadata %>%
    mutate(

      inn1_team =
        get_innings_value(
          innings_summary,
          1,
          "team"
        ),

      inn1_runs =
        get_innings_value(
          innings_summary,
          1,
          "runs"
        ),

      inn1_wickets =
        get_innings_value(
          innings_summary,
          1,
          "wickets"
        ),

      inn1_overs =
        get_innings_value(
          innings_summary,
          1,
          "overs"
        ),

      inn2_team =
        get_innings_value(
          innings_summary,
          2,
          "team"
        ),

      inn2_runs =
        get_innings_value(
          innings_summary,
          2,
          "runs"
        ),

      inn2_wickets =
        get_innings_value(
          innings_summary,
          2,
          "wickets"
        ),

      inn2_overs =
        get_innings_value(
          innings_summary,
          2,
          "overs"
        )
    )

  return(match_info)
}


# =========================================================
# 3. WORM CHART
# =========================================================

#' Get cumulative runs and wickets by over
get_match_worm_data <- function(
    df,
    selected_match_id
) {

  match_balls <- df %>%

    filter(
      match_id ==
        selected_match_id,
      innings %in% c(1, 2)
    )

  worm_data <- match_balls %>%

    group_by(
      innings,
      batting_team,
      over_display
    ) %>%

    summarise(

      over_runs =
        sum(
          runs_total,
          na.rm = TRUE
        ),

      wickets_in_over =
        sum(
          is_wicket,
          na.rm = TRUE
        ),

      .groups = "drop"
    ) %>%

    arrange(
      innings,
      over_display
    ) %>%

    group_by(innings) %>%

    mutate(

      cum_runs =
        cumsum(over_runs),

      cum_wickets =
        cumsum(wickets_in_over)
    ) %>%

    ungroup()

  return(worm_data)
}


# =========================================================
# 4. MANHATTAN CHART
# =========================================================

#' Get runs and wickets by over
get_match_manhattan_data <- function(
    df,
    selected_match_id
) {

  manhattan_data <- df %>%

    filter(
      match_id ==
        selected_match_id,
      innings %in% c(1, 2)
    ) %>%

    group_by(
      innings,
      batting_team,
      over_display
    ) %>%

    summarise(

      over_runs =
        sum(
          runs_total,
          na.rm = TRUE
        ),

      wickets =
        sum(
          is_wicket,
          na.rm = TRUE
        ),

      .groups = "drop"
    ) %>%

    arrange(
      innings,
      over_display
    )

  return(manhattan_data)
}


# =========================================================
# 5. SCORECARD
# =========================================================

#' Get batting and bowling scorecards for a selected match
get_match_scorecard <- function(
    df,
    selected_match_id
) {

  match_balls <- df %>%

    filter(
      match_id ==
        selected_match_id,
      innings %in% c(1, 2)
    )


  # -------------------------------------------------------
  # Dismissal information
  # -------------------------------------------------------

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

      wicket_kind = {

        values <- wicket_kind[
          !is.na(wicket_kind) &
            wicket_kind != ""
        ]

        if (length(values) > 0) {
          values[1]
        } else {
          NA_character_
        }
      },

      bowler_out = {

        values <- bowler[
          !is.na(bowler) &
            bowler != ""
        ]

        if (length(values) > 0) {
          values[1]
        } else {
          NA_character_
        }
      },

      .groups = "drop"
    ) %>%

    rename(
      batter = player_out
    )


  # -------------------------------------------------------
  # Batting scorecard
  # -------------------------------------------------------

  batting_scorecard <- match_balls %>%

    group_by(
      innings,
      batting_team,
      batter
    ) %>%

    summarise(

      runs =
        sum(
          runs_batter,
          na.rm = TRUE
        ),

      balls =
        sum(
          balls_faced == 1,
          na.rm = TRUE
        ),

      fours =
        sum(
          is_four,
          na.rm = TRUE
        ),

      sixes =
        sum(
          is_six,
          na.rm = TRUE
        ),

      strike_rate =
        ifelse(
          balls > 0,
          round(
            runs / balls * 100,
            1
          ),
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

      is_out =
        coalesce(
          is_out,
          FALSE
        ),

      dismissal = case_when(

        !is_out ~
          "not out",

        !is.na(wicket_kind) &
          !is.na(bowler_out) ~
          paste(
            wicket_kind,
            "b",
            bowler_out
          ),

        !is.na(wicket_kind) ~
          wicket_kind,

        TRUE ~
          "out"
      )
    ) %>%

    arrange(
      innings,
      desc(runs)
    )


  # -------------------------------------------------------
  # Bowling scorecard
  # -------------------------------------------------------

  bowling_scorecard <- match_balls %>%

    group_by(
      innings,
      bowling_team,
      bowler
    ) %>%

    summarise(

      valid_balls =
        sum(
          valid_ball == 1,
          na.rm = TRUE
        ),

      overs =
        paste0(
          valid_balls %/% 6,
          ".",
          valid_balls %% 6
        ),

      # Maiden calculation is intentionally
      # not fabricated. The dataset does not
      # provide a direct maiden-over field here.
      maidens = NA_integer_,

      runs_given =
        sum(
          runs_bowler,
          na.rm = TRUE
        ),

      wickets =
        sum(
          is_bowler_wicket,
          na.rm = TRUE
        ),

      economy =
        ifelse(
          valid_balls > 0,
          round(
            runs_given /
              (valid_balls / 6),
            2
          ),
          0
        ),

      dots =
        sum(
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


# =========================================================
# 6. PARTNERSHIPS
# =========================================================

#' Get partnership breakdown for an innings
get_match_partnerships <- function(
    df,
    selected_match_id,
    inn = 1
) {

  inn_balls <- df %>%

    filter(
      match_id ==
        selected_match_id,
      innings == inn
    )

  if (nrow(inn_balls) == 0) {
    return(NULL)
  }

  partnerships <- inn_balls %>%

    filter(
      !is.na(batting_partners),
      batting_partners != ""
    ) %>%

    group_by(
      batting_partners
    ) %>%

    summarise(

      runs =
        sum(
          runs_total,
          na.rm = TRUE
        ),

      # Partnership ball count should use
      # legal deliveries rather than raw rows.
      balls =
        sum(
          valid_ball == 1,
          na.rm = TRUE
        ),

      batter_runs =
        sum(
          runs_batter,
          na.rm = TRUE
        ),

      fours =
        sum(
          is_four,
          na.rm = TRUE
        ),

      sixes =
        sum(
          is_six,
          na.rm = TRUE
        ),

      .groups = "drop"
    ) %>%

    arrange(
      desc(runs)
    )

  return(partnerships)
}