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
                 h4("🏆 Most Successful IPL Franchises", style = "color: #00d2ff; font-weight: 700;"),
                 plotlyOutput(ns("plot_team_overview"), height = "360px")
             )
      ),
      column(6,
             div(class = "criclens-card",
                 h4("🏏 All-Time Leading Run Scorers", style = "color: #ff5e00; font-weight: 700;"),
                 plotlyOutput(ns("plot_top_batters"), height = "360px")
             )
      )
    )
  )
}

mod_overview_server <- function(id, df_feat, team_summary, batting_summary) {
  moduleServer(id, function(input, output, session) {
    # Dynamic KPI Calculations
    output$kpi_matches <- renderText({
      format(length(unique(df_feat$match_id)), big.mark = ",")
    })

    output$kpi_runs <- renderText({
      format(sum(df_feat$runs_total, na.rm = TRUE), big.mark = ",")
    })

    output$kpi_wickets <- renderText({
      format(sum(df_feat$is_wicket, na.rm = TRUE), big.mark = ",")
    })

    output$kpi_seasons <- renderText({
      seasons <- sort(unique(df_feat$season_year))
      paste0(length(seasons), " (", min(seasons), "-", max(seasons), ")")
    })

    output$plot_team_overview <- renderPlotly({
      plot_team_wins_chart(team_summary %>% head(8), title = "Top Franchises by Match Victories")
    })

    output$plot_top_batters <- renderPlotly({
      plot_top_batters_chart(batting_summary %>% head(8), title = "Top IPL Run Scorers")
    })
  })
}
