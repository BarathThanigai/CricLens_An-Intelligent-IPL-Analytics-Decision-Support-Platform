# app/modules/mod_predictions.R
# Predictive Analytics Module for CricLens Shiny App

mod_predictions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5,
             div(class = "criclens-card",
                 h4("⚡ Live Match State Simulator", style = "color: #00d2ff; font-weight: 700;"),
                 numericInput(ns("target"), "Target Score (Innings 2 Target):", value = 180, min = 50, max = 300),
                 sliderInput(ns("current_runs"), "Current Runs Scored:", min = 0, max = 280, value = 110, step = 1),
                 sliderInput(ns("overs_bowled"), "Overs Completed (0.1 - 19.5):", min = 1, max = 19.5, value = 12, step = 0.5),
                 sliderInput(ns("wickets_down"), "Wickets Lost:", min = 0, max = 9, value = 3, step = 1)
             )
      ),
      column(7,
             div(class = "criclens-card",
                 h4("🤖 Machine Learning Win Probability Model", style = "color: #ffd700; font-weight: 700;"),
                 uiOutput(ns("win_prob_results_card"))
             )
      )
    )
  )
}

mod_predictions_server <- function(id, win_model) {
  moduleServer(id, function(input, output, session) {
    output$win_prob_results_card <- renderUI({
      req(input$target, input$current_runs, input$overs_bowled, input$wickets_down)

      res <- predict_live_win_prob(
        target = input$target,
        current_runs = input$current_runs,
        overs_bowled = input$overs_bowled,
        wickets_down = input$wickets_down,
        model = win_model
      )

      tagList(
        fluidRow(
          column(6, div(class = "kpi-card",
                        div(class = "kpi-label", "Chasing Team Win Prob"),
                        div(class = "kpi-value", paste0(res$chase_win_prob, "%")))),
          column(6, div(class = "kpi-card orange",
                        div(class = "kpi-label", "Defending Team Win Prob"),
                        div(class = "kpi-value", paste0(res$defend_win_prob, "%"))))
        ),
        hr(style = "border-color: #26334d;"),
        h5("Match Context Breakdown", style = "color: #00d2ff; font-weight: 700;"),
        ul(style = "color: #e2e8f0; font-size: 1.05rem; line-height: 1.8;",
           tags$li(paste("Runs Required:", res$runs_required, "runs")),
           tags$li(paste("Balls Remaining:", res$balls_remaining, "balls")),
           tags$li(paste("Required Run Rate (RRR):", res$req_rr, "runs/over")),
           tags$li(paste("Current Run Rate (CRR):", res$current_rr, "runs/over"))
        )
      )
    })
  })
}
