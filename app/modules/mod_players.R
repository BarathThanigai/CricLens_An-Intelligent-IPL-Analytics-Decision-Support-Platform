# app/modules/mod_players.R
# Player Analytics Module for CricLens Shiny App

mod_players_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("filter"), "Player Filter Controls"), style = "color: #00d2ff; font-weight: 700;"),
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
                 h4(tagList(cl_icon("bat"), "Top Batting Performers"), style = "color: #00d2ff; font-weight: 700;"),
                 uiOutput(ns("plot_batters_ui"))
             )
      ),
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("bowl"), "Top Bowling Performers"), style = "color: #ff5e00; font-weight: 700;"),
                 uiOutput(ns("plot_bowlers_ui"))
             )
      )
    ),
    fluidRow(
      column(6,
             div(class = "criclens-card analytics-table-card",
                 h4(tagList(cl_icon("bowl"), "Selected Player Bowling Profile"), style = "color: #ff5e00; font-weight: 700;"),
                 DTOutput(ns("bowling_profile_table"))
             )
      ),
      column(6,
             div(class = "criclens-card analytics-table-card",
                 h4(tagList(cl_icon("scale"), "Custom Impact Score"), style = "color: #00d2ff; font-weight: 700;"),
                 sliderInput(ns("bat_weight"), "Impact score batting weight", min = 0, max = 1, value = 0.6, step = 0.1),
                 sliderInput(ns("bowl_weight"), "Impact score bowling weight", min = 0, max = 1, value = 0.4, step = 0.1),
                 textOutput(ns("impact_explanation")),
                 DTOutput(ns("impact_table"))
             )
      )
    )
  )
}

mod_players_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    player_data <- reactive({
      d <- df_feat()
      if (!is.null(input$season_select) && input$season_select != "All") {
        d <- d[d$season_year == as.numeric(input$season_select), , drop = FALSE]
      }
      d
    })
    impact_components <- reactive(get_player_impact_scores(player_data(), 1, 1))
    # Populate player list in selectize
    observe({
      d <- player_data()
      players <- sort(unique(c(as.character(d$batter), as.character(d$bowler))))
      players <- players[!is.na(players) & nzchar(players)]
      selected <- if ("V Kohli" %in% players) "V Kohli" else if (length(players)) players[1] else character()
      updateSelectInput(session, "search_player", choices = players, selected = selected)
    })

    # Render Player Profile Summary Card
    output$player_profile_card <- renderUI({
      req(input$search_player)
      p_name <- input$search_player

      d <- player_data()
      prof <- get_player_batting_profile(d, p_name)
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

    output$plot_batters_ui <- renderUI({
      rows <- suppressWarnings(as.integer(input$top_n))
      if (!length(rows) || is.na(rows)) rows <- 10L
      rows <- min(max(rows, 5L), 25L)
      plotlyOutput(session$ns("plot_batters"), height = paste0(cl_plot_height(rows, 34, 110, 320), "px"))
    })

    output$plot_bowlers_ui <- renderUI({
      rows <- suppressWarnings(as.integer(input$top_n))
      if (!length(rows) || is.na(rows)) rows <- 10L
      rows <- min(max(rows, 5L), 25L)
      plotlyOutput(session$ns("plot_bowlers"), height = paste0(cl_plot_height(rows, 34, 110, 320), "px"))
    })

    output$plot_batters <- renderPlotly({
      top_b <- get_top_batters(player_data(), top_n = input$top_n)
      cl_leaderboard_chart(
        top_b,
        label = "batter",
        value = "total_runs",
        value_name = "Total runs",
        details = c(Avg = "average", SR = "strike_rate"),
        top_n = input$top_n,
        selected = input$search_player
      )
    })

    output$plot_bowlers <- renderPlotly({
      top_bw <- get_top_bowlers(player_data(), top_n = input$top_n)
      cl_leaderboard_chart(
        top_bw,
        label = "bowler",
        value = "wickets",
        value_name = "Total wickets",
        details = c(Econ = "economy", Avg = "average"),
        color = cl_colors[["orange"]],
        top_n = input$top_n,
        selected = input$search_player
      )
    })

    output$bowling_profile_table <- renderDT({
      req(input$search_player)
      d <- player_data()
      profile <- get_top_bowlers(d, top_n = nrow(d), min_balls = 0)
      profile <- profile[profile$bowler == input$search_player, , drop = FALSE]
      if (!nrow(profile)) profile <- data.frame(Note = "No bowling records for this player in the selected data.")
      if ("bowler" %in% names(profile)) {
        profile <- profile[, c("bowler", "matches", "valid_balls", "overs_bowled", "runs_given", "wickets", "economy", "average", "strike_rate")]
        names(profile) <- c("Player", "Matches", "Legal balls", "Overs", "Runs conceded", "Wickets", "Economy", "Average", "Bowling SR")
        numeric_cols <- vapply(profile, is.numeric, logical(1))
        profile[numeric_cols] <- lapply(profile[numeric_cols], round, 2)
      }
      datatable(profile, options = list(pageLength = 5, scrollX = TRUE, autoWidth = FALSE, dom = "tip"), rownames = FALSE)
    })

    output$impact_explanation <- renderText({
      paste0("Relative score within the currently filtered players (50 = cohort average). It blends standardized batting average/strike rate and bowling economy/wicket rate, then reduces scores for small samples. Current weights: batting ",
             round(input$bat_weight * 100), "% and bowling ", round(input$bowl_weight * 100), "%. Minimum samples: 30 batting balls and 12 legal bowling balls.")
    })

    output$impact_table <- renderDT({
      req(input$search_player)
      scores <- weight_player_impact_scores(impact_components(), input$bat_weight, input$bowl_weight)
      row <- scores[scores$player == input$search_player, , drop = FALSE]
      if (!nrow(row) || !is.finite(row$impact[1])) {
        row <- data.frame(Note = "Insufficient eligible peer or role sample for a reliable score.")
      } else {
        row$impact <- round(row$impact, 2)
        row$batting_component <- round(row$batting_component, 2)
        row$bowling_component <- round(row$bowling_component, 2)
        names(row) <- c("Player", "Role", "Impact score", "Batting component", "Bowling component", "Batting balls", "Bowling balls")
      }
      datatable(row, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), rownames = FALSE)
    })
  })
}
