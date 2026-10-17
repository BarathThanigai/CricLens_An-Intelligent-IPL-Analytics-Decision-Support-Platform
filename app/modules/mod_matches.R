# app/modules/mod_matches.R
# Match Center Analytics Module for CricLens Shiny App

mod_matches_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("matches"), "Match Selector"), style = "color: #00d2ff; font-weight: 700;"),
                 selectInput(ns("season_filter"), "Filter Season:", choices = c("All", 2008:2026), selected = "All"),
                 selectizeInput(
                   ns("match_select"), "Select IPL Match:", choices = NULL,
                   options = list(placeholder = "Type a team or date to find a match...", maxOptions = 1000)
                 )
             )
      ),
      column(8,
             div(class = "criclens-card",
                 uiOutput(ns("match_header_card"))
             )
      )
    ),
    fluidRow(
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("overview"), "Score Progression (Worm Chart)"), style = "color: #00d2ff; font-weight: 700;"),
                 plotlyOutput(ns("plot_worm"), height = "360px")
             )
      ),
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("chart"), "Runs Per Over (Manhattan Chart)"), style = "color: #ff5e00; font-weight: 700;"),
                 plotlyOutput(ns("plot_manhattan"), height = "360px")
             )
      )
    ),
    fluidRow(
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("bat"), "Batting Scorecards"), style = "color: #ffd700; font-weight: 700;"),
                 DTOutput(ns("batting_scorecard_table"))
             )
      ),
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("bowl"), "Bowling Scorecards"), style = "color: #9d4edd; font-weight: 700;"),
                 DTOutput(ns("bowling_scorecard_table"))
             )
      )
    )
  )
}

mod_matches_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    observe({
      m_list <- get_match_list(df_feat(), season_filter = input$season_filter)
      choices_vec <- m_list$match_id
      names(choices_vec) <- m_list$label
      updateSelectInput(session, "match_select", choices = choices_vec, selected = choices_vec[1])
    })

    output$match_header_card <- renderUI({
      req(input$match_select)
      m_info <- get_match_summary_info(df_feat(), as.numeric(input$match_select))
      if (is.null(m_info)) return(div("No match details found."))

      tagList(
        h3(paste(m_info$inn1_team, "vs", m_info$inn2_team), style = "color: #ffd700; font-weight: 800;"),
        p(style = "color: #94a3b8; margin-bottom: 15px;", paste("Date:", m_info$date, "| Season:", m_info$season, "| Venue:", m_info$venue)),
        fluidRow(
          column(6, div(class = "kpi-card",
                        div(class = "kpi-label", m_info$inn1_team),
                        div(class = "kpi-value", paste0(m_info$inn1_runs, "/", m_info$inn1_wickets, " (", m_info$inn1_overs, " ov)")))),
          column(6, div(class = "kpi-card orange",
                        div(class = "kpi-label", m_info$inn2_team),
                        div(class = "kpi-value", paste0(m_info$inn2_runs, "/", m_info$inn2_wickets, " (", m_info$inn2_overs, " ov)"))))
        ),
        p(style = "color: #00d2ff; font-weight: 700; margin-top: 10px;",
          tagList(cl_icon("trophy"), "Winner: ", m_info$match_won_by, " | Player of the Match: ", m_info$player_of_match))
      )
    })

    output$plot_worm <- renderPlotly({
      req(input$match_select)
      w_data <- get_match_worm_data(df_feat(), as.numeric(input$match_select))
      if (is.null(w_data) || nrow(w_data) == 0) return(plotly_empty())
      chart_data <- w_data %>%
        transmute(
          over = as.numeric(over_display),
          cumulative_runs = cum_runs,
          team = paste0("Innings ", innings, ": ", batting_team),
          wickets = wickets_in_over
        )
      chart_overs <- max(20, ceiling(max(chart_data$over, na.rm = TRUE)))
      cl_worm_chart(
        chart_data,
        over = "over",
        runs = "cumulative_runs",
        team = "team",
        wickets = "wickets",
        overs = chart_overs
      )
    })

    output$plot_manhattan <- renderPlotly({
      req(input$match_select)
      m_data <- get_match_manhattan_data(df_feat(), as.numeric(input$match_select))
      if (is.null(m_data) || nrow(m_data) == 0) return(plotly_empty())
      chart_data <- m_data %>%
        transmute(
          over = as.numeric(over_display),
          runs = over_runs,
          team = paste0("Innings ", innings, ": ", batting_team),
          wickets = wickets
        )
      chart_overs <- max(20, ceiling(max(chart_data$over, na.rm = TRUE)))
      cl_manhattan_chart(
        chart_data,
        over = "over",
        runs = "runs",
        team = "team",
        wickets = "wickets",
        overs = chart_overs
      )
    })

    output$batting_scorecard_table <- renderDT({
      req(input$match_select)
      sc <- get_match_scorecard(df_feat(), as.numeric(input$match_select))
      cl_batting_scorecard(sc$batting)
    })

    output$bowling_scorecard_table <- renderDT({
      req(input$match_select)
      sc <- get_match_scorecard(df_feat(), as.numeric(input$match_select))
      cl_bowling_scorecard(sc$bowling)
    })
  })
}
