# R/visualizations.R
# CricLens — Visualizations Engine Module
# Generates rich, interactive Plotly analytical charts with IPL Dark Mode styling

suppressMessages({
  library(plotly)
  library(dplyr)
  library(ggplot2)
})

# CricLens Theme Colors
CRICLENS_BG <- "#0B0F19"
CRICLENS_CARD_BG <- "#161F33"
CRICLENS_TEXT <- "#E2E8F0"
CRICLENS_CYAN <- "#00D2FF"
CRICLENS_ORANGE <- "#FF5E00"
CRICLENS_GOLD <- "#FFD700"
CRICLENS_PURPLE <- "#9D4EDD"
CRICLENS_GRID <- "#2D3748"

#' Helper to apply dark theme layout to Plotly figures
apply_dark_plotly_layout <- function(p, title_text = "", x_title = "", y_title = "") {
  p %>% layout(
    title = list(text = paste0("<b>", title_text, "</b>"), font = list(color = "#FFFFFF", size = 18)),
    paper_bgcolor = CRICLENS_CARD_BG,
    plot_bgcolor = CRICLENS_CARD_BG,
    font = list(color = CRICLENS_TEXT, family = "Inter, sans-serif"),
    xaxis = list(
      title = x_title,
      gridcolor = CRICLENS_GRID,
      zerolinecolor = CRICLENS_GRID,
      tickfont = list(color = CRICLENS_TEXT)
    ),
    yaxis = list(
      title = y_title,
      gridcolor = CRICLENS_GRID,
      zerolinecolor = CRICLENS_GRID,
      tickfont = list(color = CRICLENS_TEXT)
    ),
    margin = list(l = 50, r = 30, t = 50, b = 50),
    legend = list(font = list(color = CRICLENS_TEXT))
  )
}

#' Plot Top Batters (Horizontal Bar Chart)
plot_top_batters_chart <- function(batting_df, title = "Top IPL Run Scorers") {
  if (nrow(batting_df) == 0) return(plotly_empty())

  batting_df <- batting_df %>% arrange(total_runs)

  p <- plot_ly(
    data = batting_df,
    x = ~total_runs,
    y = ~reorder(batter, total_runs),
    type = "bar",
    orientation = "h",
    marker = list(
      color = CRICLENS_CYAN,
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~paste0("Runs: ", total_runs, "<br>Avg: ", average, "<br>SR: ", strike_rate),
    hoverinfo = "text"
  )

  apply_dark_plotly_layout(p, title_text = title, x_title = "Total Runs", y_title = "Batter")
}

#' Plot Top Bowlers (Horizontal Bar Chart)
plot_top_bowlers_chart <- function(bowling_df, title = "Top IPL Wicket Takers") {
  if (nrow(bowling_df) == 0) return(plotly_empty())

  bowling_df <- bowling_df %>% arrange(wickets)

  p <- plot_ly(
    data = bowling_df,
    x = ~wickets,
    y = ~reorder(bowler, wickets),
    type = "bar",
    orientation = "h",
    marker = list(
      color = CRICLENS_ORANGE,
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~paste0("Wickets: ", wickets, "<br>Econ: ", economy, "<br>Avg: ", average),
    hoverinfo = "text"
  )

  apply_dark_plotly_layout(p, title_text = title, x_title = "Total Wickets", y_title = "Bowler")
}

#' Plot Match Worm Chart (Score Progression)
plot_match_worm_chart <- function(worm_data, title = "Match Progression (Worm Chart)") {
  if (nrow(worm_data) == 0) return(plotly_empty())

  p <- plot_ly()

  inn1_data <- worm_data %>% filter(innings == 1)
  inn2_data <- worm_data %>% filter(innings == 2)

  if (nrow(inn1_data) > 0) {
    p <- p %>% add_trace(
      data = inn1_data,
      x = ~over_display,
      y = ~cum_runs,
      type = "scatter",
      mode = "lines+markers",
      name = paste("Innings 1:", first(inn1_data$batting_team)),
      line = list(color = CRICLENS_CYAN, width = 3),
      marker = list(size = 6, color = CRICLENS_CYAN),
      text = ~paste0("Over: ", over_display, "<br>Score: ", cum_runs, "/", cum_wickets),
      hoverinfo = "text"
    )
  }

  if (nrow(inn2_data) > 0) {
    p <- p %>% add_trace(
      data = inn2_data,
      x = ~over_display,
      y = ~cum_runs,
      type = "scatter",
      mode = "lines+markers",
      name = paste("Innings 2:", first(inn2_data$batting_team)),
      line = list(color = CRICLENS_ORANGE, width = 3),
      marker = list(size = 6, color = CRICLENS_ORANGE),
      text = ~paste0("Over: ", over_display, "<br>Score: ", cum_runs, "/", cum_wickets),
      hoverinfo = "text"
    )
  }

  apply_dark_plotly_layout(p, title_text = title, x_title = "Overs", y_title = "Cumulative Runs")
}

#' Plot Match Manhattan Chart (Runs Per Over)
plot_match_manhattan_chart <- function(manhattan_data, title = "Runs Per Over (Manhattan Chart)") {
  if (nrow(manhattan_data) == 0) return(plotly_empty())

  p <- plot_ly(
    data = manhattan_data,
    x = ~over_display,
    y = ~over_runs,
    color = ~as.factor(batting_team),
    colors = c(CRICLENS_CYAN, CRICLENS_ORANGE),
    type = "bar",
    text = ~paste0("Over ", over_display, ": ", over_runs, " runs (Wickets: ", wickets, ")"),
    hoverinfo = "text"
  )

  apply_dark_plotly_layout(p, title_text = title, x_title = "Over Number", y_title = "Runs Conceded/Scored")
}

#' Plot Team Wins Leaderboard Bar Chart
plot_team_wins_chart <- function(team_summary_df, title = "Total IPL Matches Won by Franchise") {
  if (nrow(team_summary_df) == 0) return(plotly_empty())

  p <- plot_ly(
    data = team_summary_df,
    x = ~wins,
    y = ~reorder(team, wins),
    type = "bar",
    orientation = "h",
    marker = list(
      color = CRICLENS_GOLD,
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~paste0(team, ": ", wins, " wins (", win_pct, "% Win Rate)"),
    hoverinfo = "text"
  )

  apply_dark_plotly_layout(p, title_text = title, x_title = "Total Wins", y_title = "Franchise")
}

#' Plot Venue Average 1st Innings Score Chart
plot_venue_scores_chart <- function(venue_df, title = "Average 1st Innings Score by Venue") {
  if (nrow(venue_df) == 0) return(plotly_empty())

  v_top <- venue_df %>% head(15) %>% arrange(avg_1st_inn_score)

  p <- plot_ly(
    data = v_top,
    x = ~avg_1st_inn_score,
    y = ~reorder(venue, avg_1st_inn_score),
    type = "bar",
    orientation = "h",
    marker = list(color = CRICLENS_PURPLE),
    text = ~paste0("Avg 1st Inn Score: ", avg_1st_inn_score, "<br>Matches: ", matches),
    hoverinfo = "text"
  )

  apply_dark_plotly_layout(p, title_text = title, x_title = "Avg 1st Innings Score", y_title = "Venue")
}
