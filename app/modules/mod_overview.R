# app/modules/mod_overview.R
# Overview Module for CricLens Shiny App

suppressMessages({
  library(shiny)
  library(plotly)
  library(DT)
})

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, div(class = "kpi-card",
                    div(class = "kpi-label", "Total IPL Matches"),
                    div(class = "kpi-value", textOutput(ns("kpi_matches"))))),
      column(3, div(class = "kpi-card orange",
                    div(class = "kpi-label", "Total Runs Scored"),
                    div(class = "kpi-value", textOutput(ns("kpi_runs"))))),
      column(3, div(class = "kpi-card gold",
                    div(class = "kpi-label", "Total Wickets Fallen"),
                    div(class = "kpi-value", textOutput(ns("kpi_wickets"))))),
      column(3, div(class = "kpi-card purple",
                    div(class = "kpi-label", "Seasons Covered"),
                    div(class = "kpi-value", textOutput(ns("kpi_seasons")))))
    ),
    fluidRow(
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("trophy"), "Most Successful IPL Franchises"), style = "color: #00d2ff; font-weight: 700;"),
                 plotlyOutput(ns("plot_team_overview"), height = paste0(cl_plot_height(8, 34, 110, 320), "px"))
             )
      ),
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("bat"), "All-Time Leading Run Scorers"), style = "color: #ff5e00; font-weight: 700;"),
                 plotlyOutput(ns("plot_top_batters"), height = paste0(cl_plot_height(8, 34, 110, 320), "px"))
             )
      )
    )
  )
}

mod_overview_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    # Dynamic KPI Calculations
    output$kpi_matches <- renderText({
      d <- df_feat()
      format(length(unique(d$match_id)), big.mark = ",")
    })

    output$kpi_runs <- renderText({
      d <- df_feat()
      format(sum(d$runs_total, na.rm = TRUE), big.mark = ",")
    })

    output$kpi_wickets <- renderText({
      d <- df_feat()
      format(sum(d$is_wicket, na.rm = TRUE), big.mark = ",")
    })

    output$kpi_seasons <- renderText({
      d <- df_feat()
      seasons <- sort(unique(d$season_year))
      seasons <- seasons[!is.na(seasons)]
      if (!length(seasons)) return("0")
      paste0(length(seasons), " (", min(seasons), "-", max(seasons), ")")
    })

    output$plot_team_overview <- renderPlotly({
      franchise <- get_team_performance_summary(df_feat())
      cl_leaderboard_chart(
        franchise,
        label = "team",
        value = "wins",
        value_name = "Total wins",
        details = c(Matches = "matches_played", `Win rate` = "win_pct"),
        color = cl_colors[["navy"]],
        top_n = 8
      )
    })

    output$plot_top_batters <- renderPlotly({
      batters <- get_top_batters(df_feat(), top_n = 8)
      cl_leaderboard_chart(
        batters,
        label = "batter",
        value = "total_runs",
        value_name = "Total runs",
        details = c(Avg = "average", SR = "strike_rate"),
        top_n = 8
      )
    })
  })
}
