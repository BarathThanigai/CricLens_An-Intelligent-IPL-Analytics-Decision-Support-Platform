# app/modules/mod_venues.R
# Venue Analytics Module for CricLens Shiny App

mod_venues_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             div(class = "criclens-card",
                 h4("🏟️ Stadium Selector", style = "color: #00d2ff; font-weight: 700;"),
                 selectInput(ns("venue_select"), "Select IPL Venue:", choices = NULL, selectize = TRUE)
             )
      ),
      column(8,
             div(class = "criclens-card",
                 uiOutput(ns("venue_profile_card"))
             )
      )
    ),
    fluidRow(
      column(12,
             div(class = "criclens-card",
                 h4("📊 Average 1st Innings Score Across IPL Grounds", style = "color: #9d4edd; font-weight: 700;"),
                 plotlyOutput(ns("plot_venue_leaderboard"), height = "420px")
             )
      )
    )
  )
}

mod_venues_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    observe({
      v_list <- get_venue_list(df_feat)
      updateSelectInput(session, "venue_select", choices = v_list$venue, selected = "Eden Gardens")
    })

    output$venue_profile_card <- renderUI({
      req(input$venue_select)
      prof <- get_venue_profile(df_feat, input$venue_select)
      if (is.null(prof)) return(div("No venue details found."))

      tagList(
        h3(prof$venue, style = "color: #ffd700; font-weight: 800;"),
        p(style = "color: #94a3b8;", paste("City:", prof$city, "| Total Matches Hosted:", prof$total_matches)),
        fluidRow(
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Avg 1st Inn Score"), div(class = "kpi-value", prof$avg_1st_inn))),
          column(3, div(class = "kpi-card orange", div(class = "kpi-label", "Highest 1st Inn Score"), div(class = "kpi-value", prof$highest_1st_inn))),
          column(3, div(class = "kpi-card gold", div(class = "kpi-label", "Chasing Win %"), div(class = "kpi-value", paste0(prof$chase_win_pct, "%")))),
          column(3, div(class = "kpi-card purple", div(class = "kpi-label", "Bat 1st Wins"), div(class = "kpi-value", prof$bat_1st_wins)))
        )
      )
    })

    output$plot_venue_leaderboard <- renderPlotly({
      v_lead <- get_venue_leaderboard(df_feat, min_matches = 10)
      plot_venue_scores_chart(v_lead)
    })
  })
}
