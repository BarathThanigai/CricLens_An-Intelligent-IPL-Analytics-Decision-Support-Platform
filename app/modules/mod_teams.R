# app/modules/mod_teams.R
# Team Analytics Module for CricLens Shiny App

mod_teams_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             div(class = "criclens-card",
                 h4("⚔️ Head-to-Head Comparison", style = "color: #00d2ff; font-weight: 700;"),
                 selectInput(ns("team1"), "Select Team 1:", choices = NULL),
                 selectInput(ns("team2"), "Select Team 2:", choices = NULL),
                 hr(style = "border-color: #26334d;"),
                 uiOutput(ns("h2h_result_card"))
             )
      ),
      column(8,
             div(class = "criclens-card",
                 h4("🏆 Franchise Performance & Win Rates", style = "color: #ffd700; font-weight: 700;"),
                 DTOutput(ns("team_summary_table"))
             )
      )
    ),
    fluidRow(
      column(12,
             div(class = "criclens-card",
                 h4("⚖️ Batting 1st vs Chasing Win Comparison", style = "color: #ff5e00; font-weight: 700;"),
                 DTOutput(ns("bat1st_vs_chasing_table"))
             )
      )
    )
  )
}

mod_teams_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    observe({
      teams <- sort(unique(c(df_feat$batting_team, df_feat$bowling_team)))
      updateSelectInput(session, "team1", choices = teams, selected = "Mumbai Indians")
      updateSelectInput(session, "team2", choices = teams, selected = "Chennai Super Kings")
    })

    output$h2h_result_card <- renderUI({
      req(input$team1, input$team2)
      if (input$team1 == input$team2) {
        return(div(p(style = "color: #ff5e00;", "Please select two different teams.")))
      }

      h2h <- get_head_to_head(df_feat, input$team1, input$team2)

      tagList(
        h5(paste(h2h$team1, "vs", h2h$team2), style = "color: #ffffff; font-weight: 700;"),
        p(style = "font-size: 1.1rem; color: #00d2ff;", paste("Total Matches Played:", h2h$total_matches)),
        div(class = "kpi-card", div(class = "kpi-label", h2h$team1), div(class = "kpi-value", paste(h2h$team1_wins, "Wins"))),
        div(class = "kpi-card orange", div(class = "kpi-label", h2h$team2), div(class = "kpi-value", paste(h2h$team2_wins, "Wins")))
      )
    })

    output$team_summary_table <- renderDT({
      t_sum <- get_team_performance_summary(df_feat)
      datatable(t_sum, options = list(pageLength = 8, dom = 't'), rownames = FALSE)
    })

    output$bat1st_vs_chasing_table <- renderDT({
      split_sum <- get_batting_1st_vs_chasing(df_feat)
      datatable(split_sum, options = list(pageLength = 8, dom = 't'), rownames = FALSE)
    })
  })
}
