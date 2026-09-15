# R/visualizations.R
# CricLens — Visualizations Engine Module
# Generates interactive Plotly analytical charts.
#
# IMPORTANT:
# This module ONLY visualizes analytical results supplied by
# the R analytics engine. It does not calculate cricket
# statistics or modify analytical values.

suppressMessages({
  library(plotly)
})


# =========================================================
# CRICLENS VISUAL THEME
# =========================================================

CRICLENS_BG <- "#0B0F19"
CRICLENS_CARD_BG <- "#161F33"
CRICLENS_TEXT <- "#E2E8F0"
CRICLENS_CYAN <- "#00D2FF"
CRICLENS_ORANGE <- "#FF5E00"
CRICLENS_GOLD <- "#FFD700"
CRICLENS_PURPLE <- "#9D4EDD"
CRICLENS_GRID <- "#2D3748"


# =========================================================
# COMMON PLOTLY LAYOUT
# =========================================================

#' Apply CricLens dark Plotly layout
apply_dark_plotly_layout <- function(
    p,
    title_text = "",
    x_title = "",
    y_title = ""
) {

  p %>%
    layout(

      title = list(
        text = paste0(
          "<b>",
          title_text,
          "</b>"
        ),
        font = list(
          color = "#FFFFFF",
          size = 18
        )
      ),

      paper_bgcolor =
        CRICLENS_CARD_BG,

      plot_bgcolor =
        CRICLENS_CARD_BG,

      font = list(
        color = CRICLENS_TEXT,
        family = "Inter, sans-serif"
      ),

      xaxis = list(
        title = x_title,
        gridcolor = CRICLENS_GRID,
        zerolinecolor = CRICLENS_GRID,
        tickfont = list(
          color = CRICLENS_TEXT
        )
      ),

      yaxis = list(
        title = y_title,
        gridcolor = CRICLENS_GRID,
        zerolinecolor = CRICLENS_GRID,
        tickfont = list(
          color = CRICLENS_TEXT
        )
      ),

      margin = list(
        l = 50,
        r = 30,
        t = 50,
        b = 50
      ),

      legend = list(
        font = list(
          color = CRICLENS_TEXT
        )
      )
    )
}


# =========================================================
# 1. TOP BATTERS
# =========================================================

#' Plot top batters by total runs
#'
#' Expected analytical columns:
#' batter, total_runs, average, strike_rate
plot_top_batters_chart <- function(
    batting_df,
    title = "Top IPL Run Scorers"
) {

  if (
    is.null(batting_df) ||
    nrow(batting_df) == 0
  ) {
    return(plotly_empty())
  }

  required_columns <- c(
    "batter",
    "total_runs",
    "average",
    "strike_rate"
  )

  if (
    !all(
      required_columns %in%
        names(batting_df)
    )
  ) {
    stop(
      "plot_top_batters_chart: missing required analytical columns."
    )
  }

  chart_data <- batting_df %>%
    arrange(total_runs)

  p <- plot_ly(
    data = chart_data,
    x = ~total_runs,
    y = ~reorder(
      batter,
      total_runs
    ),
    type = "bar",
    orientation = "h",

    marker = list(
      color = CRICLENS_CYAN,
      line = list(
        color = "#FFFFFF",
        width = 1
      )
    ),

    text = ~paste0(
      "Runs: ",
      total_runs,
      "<br>Average: ",
      average,
      "<br>Strike Rate: ",
      strike_rate
    ),

    hoverinfo = "text"
  )

  apply_dark_plotly_layout(
    p,
    title_text = title,
    x_title = "Total Runs",
    y_title = "Batter"
  )
}


# =========================================================
# 2. TOP BOWLERS
# =========================================================

#' Plot top bowlers by wickets
#'
#' Expected analytical columns:
#' bowler, wickets, economy, average
plot_top_bowlers_chart <- function(
    bowling_df,
    title = "Top IPL Wicket Takers"
) {

  if (
    is.null(bowling_df) ||
    nrow(bowling_df) == 0
  ) {
    return(plotly_empty())
  }

  required_columns <- c(
    "bowler",
    "wickets",
    "economy",
    "average"
  )

  if (
    !all(
      required_columns %in%
        names(bowling_df)
    )
  ) {
    stop(
      "plot_top_bowlers_chart: missing required analytical columns."
    )
  }

  chart_data <- bowling_df %>%
    arrange(wickets)

  p <- plot_ly(
    data = chart_data,
    x = ~wickets,
    y = ~reorder(
      bowler,
      wickets
    ),
    type = "bar",
    orientation = "h",

    marker = list(
      color = CRICLENS_ORANGE,
      line = list(
        color = "#FFFFFF",
        width = 1
      )
    ),

    text = ~paste0(
      "Wickets: ",
      wickets,
      "<br>Economy: ",
      economy,
      "<br>Average: ",
      average
    ),

    hoverinfo = "text"
  )

  apply_dark_plotly_layout(
    p,
    title_text = title,
    x_title = "Total Wickets",
    y_title = "Bowler"
  )
}


# =========================================================
# 3. MATCH WORM CHART
# =========================================================

#' Plot cumulative score progression for both innings
plot_match_worm_chart <- function(
    worm_data,
    title = "Match Progression (Worm Chart)"
) {

  if (
    is.null(worm_data) ||
    nrow(worm_data) == 0
  ) {
    return(plotly_empty())
  }

  required_columns <- c(
    "innings",
    "batting_team",
    "over_display",
    "cum_runs",
    "cum_wickets"
  )

  if (
    !all(
      required_columns %in%
        names(worm_data)
    )
  ) {
    stop(
      "plot_match_worm_chart: missing required analytical columns."
    )
  }

  p <- plot_ly()

  inn1_data <- worm_data %>%
    filter(innings == 1)

  inn2_data <- worm_data %>%
    filter(innings == 2)


  if (nrow(inn1_data) > 0) {

    team_name <- unique(
      inn1_data$batting_team
    )[1]

    p <- p %>%

      add_trace(
        data = inn1_data,

        x = ~over_display,

        y = ~cum_runs,

        type = "scatter",

        mode = "lines+markers",

        name = paste(
          "Innings 1:",
          team_name
        ),

        line = list(
          color = CRICLENS_CYAN,
          width = 3
        ),

        marker = list(
          size = 6,
          color = CRICLENS_CYAN
        ),

        text = ~paste0(
          "Over: ",
          over_display,
          "<br>Score: ",
          cum_runs,
          "/",
          cum_wickets
        ),

        hoverinfo = "text"
      )
  }


  if (nrow(inn2_data) > 0) {

    team_name <- unique(
      inn2_data$batting_team
    )[1]

    p <- p %>%

      add_trace(
        data = inn2_data,

        x = ~over_display,

        y = ~cum_runs,

        type = "scatter",

        mode = "lines+markers",

        name = paste(
          "Innings 2:",
          team_name
        ),

        line = list(
          color = CRICLENS_ORANGE,
          width = 3
        ),

        marker = list(
          size = 6,
          color = CRICLENS_ORANGE
        ),

        text = ~paste0(
          "Over: ",
          over_display,
          "<br>Score: ",
          cum_runs,
          "/",
          cum_wickets
        ),

        hoverinfo = "text"
      )
  }


  apply_dark_plotly_layout(
    p,
    title_text = title,
    x_title = "Over",
    y_title = "Cumulative Runs"
  )
}


# =========================================================
# 4. MATCH MANHATTAN CHART
# =========================================================

#' Plot runs scored per over
plot_match_manhattan_chart <- function(
    manhattan_data,
    title = "Runs Per Over (Manhattan Chart)"
) {

  if (
    is.null(manhattan_data) ||
    nrow(manhattan_data) == 0
  ) {
    return(plotly_empty())
  }

  required_columns <- c(
    "innings",
    "batting_team",
    "over_display",
    "over_runs",
    "wickets"
  )

  if (
    !all(
      required_columns %in%
        names(manhattan_data)
    )
  ) {
    stop(
      "plot_match_manhattan_chart: missing required analytical columns."
    )
  }

  p <- plot_ly(

    data = manhattan_data,

    x = ~over_display,

    y = ~over_runs,

    color = ~as.factor(
      batting_team
    ),

    colors = c(
      CRICLENS_CYAN,
      CRICLENS_ORANGE
    ),

    type = "bar",

    text = ~paste0(
      "Over ",
      over_display,
      ": ",
      over_runs,
      " runs",
      "<br>Wickets: ",
      wickets
    ),

    hoverinfo = "text"
  )

  apply_dark_plotly_layout(
    p,
    title_text = title,
    x_title = "Over Number",
    y_title = "Runs Scored"
  )
}


# =========================================================
# 5. TEAM WINS
# =========================================================

#' Plot team wins leaderboard
#'
#' Supports the analytical output from
#' get_team_performance_summary(), which uses
#' `wins` as the canonical field.
plot_team_wins_chart <- function(
    team_summary_df,
    title = "Total IPL Matches Won by Franchise"
) {

  if (
    is.null(team_summary_df) ||
    nrow(team_summary_df) == 0
  ) {
    return(plotly_empty())
  }

  required_columns <- c(
    "team",
    "wins"
  )

  if (
    !all(
      required_columns %in%
        names(team_summary_df)
    )
  ) {
    stop(
      "plot_team_wins_chart: expected `team` and `wins` from team analytics."
    )
  }

  chart_data <- team_summary_df %>%
    arrange(wins)

  hover_text <- if (
    "win_pct" %in%
      names(chart_data)
  ) {

    paste0(
      chart_data$team,
      ": ",
      chart_data$wins,
      " wins (",
      chart_data$win_pct,
      "% Win Rate)"
    )

  } else {

    paste0(
      chart_data$team,
      ": ",
      chart_data$wins,
      " wins"
    )
  }


  p <- plot_ly(

    data = chart_data,

    x = ~wins,

    y = ~reorder(
      team,
      wins
    ),

    type = "bar",

    orientation = "h",

    marker = list(
      color = CRICLENS_GOLD,
      line = list(
        color = "#FFFFFF",
        width = 1
      )
    ),

    text = hover_text,

    hoverinfo = "text"
  )

  apply_dark_plotly_layout(
    p,
    title_text = title,
    x_title = "Total Wins",
    y_title = "Franchise"
  )
}


# =========================================================
# 6. VENUE SCORES
# =========================================================

#' Plot average first-innings score by venue
#'
#' Supports the analytical output from venue analysis
#' using `total_matches` as the canonical match count.
plot_venue_scores_chart <- function(
    venue_df,
    title = "Average 1st Innings Score by Venue"
) {

  if (
    is.null(venue_df) ||
    nrow(venue_df) == 0
  ) {
    return(plotly_empty())
  }

  required_columns <- c(
    "venue",
    "avg_1st_inn_score"
  )

  if (
    !all(
      required_columns %in%
        names(venue_df)
    )
  ) {
    stop(
      "plot_venue_scores_chart: missing required venue analytical columns."
    )
  }

  chart_data <- venue_df %>%

    arrange(
      avg_1st_inn_score
    ) %>%

    head(15)


  match_count_label <- if (
    "total_matches" %in%
      names(chart_data)
  ) {

    paste0(
      "<br>Matches: ",
      chart_data$total_matches
    )

  } else if (
    "matches" %in%
      names(chart_data)
  ) {

    paste0(
      "<br>Matches: ",
      chart_data$matches
    )

  } else {

    ""
  }


  hover_text <- paste0(
    "Avg 1st Innings Score: ",
    chart_data$avg_1st_inn_score,
    match_count_label
  )


  p <- plot_ly(

    data = chart_data,

    x = ~avg_1st_inn_score,

    y = ~reorder(
      venue,
      avg_1st_inn_score
    ),

    type = "bar",

    orientation = "h",

    marker = list(
      color = CRICLENS_PURPLE
    ),

    text = hover_text,

    hoverinfo = "text"
  )

  apply_dark_plotly_layout(
    p,
    title_text = title,
    x_title = "Average 1st Innings Score",
    y_title = "Venue"
  )
}