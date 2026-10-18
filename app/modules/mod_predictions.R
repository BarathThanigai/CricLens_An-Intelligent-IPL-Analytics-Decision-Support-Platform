# app/modules/mod_predictions.R
# Predictive Analytics Module for CricLens Shiny App

mod_predictions_ui <- function(id) {
  ns <- NS(id)
  overs_values <- sprintf(
    "%d.%d",
    rep(0:19, each = 6),
    rep(0:5, times = 20)
  )
  tagList(
    fluidRow(
      column(5,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("predictions"), "Live Match State Simulator"), style = "color: #00d2ff; font-weight: 700;"),
                 numericInput(ns("target"), "Target Score (Innings 2 Target):", value = 180, min = 1, max = 300),
                 sliderInput(ns("current_runs"), "Current Runs Scored:", min = 0, max = 300, value = 110, step = 1),
                 selectInput(
                   ns("overs_bowled"),
                   "Overs Completed (legal balls only):",
                   choices = stats::setNames(overs_values, paste(overs_values, "overs")),
                   selected = "12.0"
                 ),
                 sliderInput(ns("wickets_down"), "Wickets Lost:", min = 0, max = 9, value = 3, step = 1)
             )
      ),
      column(7,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("ai"), "Machine Learning Win Probability Model"), style = "color: #ffd700; font-weight: 700;"),
                 p("Probability that the second-innings batting side wins; the defending probability is its complement.",
                   style = "color: #94a3b8;"),
                 uiOutput(ns("win_prob_results_card"))
             )
      )
    )
  )
}

mod_predictions_server <- function(id, win_model) {
  moduleServer(id, function(input, output, session) {
    output$win_prob_results_card <- renderUI({
      if (inherits(win_model, "criclens_model_load_error")) {
        return(div(
          class = "alert alert-warning",
          role = "alert",
          tags$strong("Win probability model unavailable. "),
          win_model$message
        ))
      }

      req(input$target, input$current_runs, input$overs_bowled, input$wickets_down)

      res <- tryCatch(
        withProgress(
          message = "Updating win probabilities...",
          value = 0.5,
          {
            predict_live_win_prob(
              target = input$target,
              current_runs = input$current_runs,
              overs_bowled = input$overs_bowled,
              wickets_down = input$wickets_down,
              model = win_model
            )
          }
        ),
        error = function(error) error
      )
      if (inherits(res, "error")) {
        return(div(
          class = "alert alert-danger",
          role = "alert",
          tags$strong("Prediction unavailable. "),
          conditionMessage(res)
        ))
      }

      tagList(
        fluidRow(
          column(6, div(class = "kpi-card",
                        div(class = "kpi-label", "Chasing Team Win Prob"),
                        div(class = "kpi-value", sprintf("%.1f%%", res$chase_win_prob * 100)))),
          column(6, div(class = "kpi-card orange",
                        div(class = "kpi-label", "Defending Team Win Prob"),
                        div(class = "kpi-value", sprintf("%.1f%%", res$defend_win_prob * 100))))
        ),
        hr(style = "border-color: #26334d;"),
        h5("Match Context Breakdown", style = "color: #00d2ff; font-weight: 700;"),
        tags$ul(style = "color: #e2e8f0; font-size: 1.05rem; line-height: 1.8;",
           tags$li(paste("Runs Required:", res$runs_required, "runs")),
           tags$li(paste("Balls Remaining:", res$balls_remaining, "balls")),
           tags$li(paste("Required Run Rate (RRR):", res$req_rr, "runs/over")),
           tags$li(paste("Current Run Rate (CRR):", res$current_rr, "runs/over"))
        )
      )
    })
  })
}
