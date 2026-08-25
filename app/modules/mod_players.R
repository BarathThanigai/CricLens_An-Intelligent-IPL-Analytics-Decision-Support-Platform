# app/modules/mod_players.R
# Player Analytics Module for CricLens Shiny App

mod_players_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             div(class = "criclens-card",
                 h4("🔍 Player Filter Controls", style = "color: #00d2ff; font-weight: 700;"),
                 selectInput(ns("season_select"), "Select Season:",
                             choices = c("All", 2008:2026), selected = "All"),
                 numericInput(ns("top_n"), "Show Top N Players:", value = 10, min = 5, max = 25),
                 selectInput(ns("search_player"), "Search Individual Player:",
                             choices = NULL, selectize = TRUE)
             )
      ),
      column(8,
             div(class = "criclens-card",
                 uiOutput(ns("player_profile_card"))
             )
      )
    ),
    fluidRow(
      column(6,
             div(class = "criclens-card",
                 h4("🏏 Top Batting Performers", style = "color: #00d2ff; font-weight: 700;"),
                 plotlyOutput(ns("plot_batters"), height = "400px")
             )
      ),
      column(6,
             div(class = "criclens-card",
                 h4("🎯 Top Bowling Performers", style = "color: #ff5e00; font-weight: 700;"),
                 plotlyOutput(ns("plot_bowlers"), height = "400px")
             )
      )
    )
  )
}

mod_players_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    # Populate player list in selectize
    observe({
      players <- sort(unique(c(df_feat$batter, df_feat$bowler)))
      updateSelectInput(session, "search_player", choices = players, selected = "V Kohli")
    })

    # Render Player Profile Summary Card
    output$player_profile_card <- renderUI({
      req(input$search_player)
      p_name <- input$search_player

      prof <- get_player_batting_profile(df_feat, p_name)
      if (is.null(prof)) {
        return(div(h4(paste("No batting records found for", p_name))))
      }

      tagList(
        h3(prof$player, style = "color: #ffd700; font-weight: 800; margin-bottom: 15px;"),
        fluidRow(
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Matches / Innings"), div(class = "kpi-value", paste(prof$matches, "/", prof$innings)))),
          column(3, div(class = "kpi-card orange", div(class = "kpi-label", "Total Runs"), div(class = "kpi-value", prof$total_runs))),
          column(3, div(class = "kpi-card gold", div(class = "kpi-label", "Batting Avg"), div(class = "kpi-value", prof$average))),
          column(3, div(class = "kpi-card purple", div(class = "kpi-label", "Strike Rate"), div(class = "kpi-value", prof$strike_rate)))
        ),
        p(style = "color: #94a3b8; font-size: 0.95rem; margin-top: 10px;",
          paste("Boundaries:", prof$fours, "Fours (4s) |", prof$sixes, "Sixes (6s) | Fifties (50s):", prof$fifties, "| Hundreds (100s):", prof$hundreds)
        )
      )
    })

    output$plot_batters <- renderPlotly({
      top_b <- get_top_batters(df_feat, top_n = input$top_n, season_filter = input$season_select)
      plot_top_batters_chart(top_b)
    })

    output$plot_bowlers <- renderPlotly({
      top_bw <- get_top_bowlers(df_feat, top_n = input$top_n, season_filter = input$season_select)
      plot_top_bowlers_chart(top_bw)
    })
  })
}
