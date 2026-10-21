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
                 h4(tagList(cl_icon("bat"), "Selected Player Batting Trend"), style = "color: #00d2ff; font-weight: 700;"),
                 plotlyOutput(ns("player_batting_chart"), height = "320px")
             )
      ),
      column(6,
             div(class = "criclens-card",
                 h4(tagList(cl_icon("bowl"), "Selected Player Bowling Trend"), style = "color: #ff5e00; font-weight: 700;"),
                 plotlyOutput(ns("player_bowling_chart"), height = "320px")
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
                 h4(tagList(cl_icon("scale"), "Impact Score Breakdown"), style = "color: #00d2ff; font-weight: 700;"),
                 plotlyOutput(ns("impact_chart"), height = "390px")
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
      bowl <- get_player_bowling_profile(d, p_name)
      value_or_dash <- function(x, digits = 2) {
        if (length(x) != 1L || is.na(x) || !is.finite(x)) return("N/A")
        format(round(x, digits), nsmall = digits, trim = TRUE)
      }
      batting_section <- if (is.null(prof)) {
        div(class = "player-empty-state", "No batting records found for this player in the selected season.")
      } else {
        tagList(
          fluidRow(
            column(3, div(class = "kpi-card", div(class = "kpi-label", "Matches / Innings"), div(class = "kpi-value", paste(prof$matches, "/", prof$innings)))),
            column(3, div(class = "kpi-card orange", div(class = "kpi-label", "Total Runs"), div(class = "kpi-value", prof$total_runs))),
            column(3, div(class = "kpi-card gold", div(class = "kpi-label", "Batting Avg"), div(class = "kpi-value", value_or_dash(prof$average)))),
            column(3, div(class = "kpi-card purple", div(class = "kpi-label", "Strike Rate"), div(class = "kpi-value", value_or_dash(prof$strike_rate))))
          ),
          p(style = "color: #94a3b8; font-size: 0.95rem; margin-top: 10px;",
            paste("Boundaries:", prof$fours, "Fours (4s) |", prof$sixes, "Sixes (6s) | Fifties (50s):", prof$fifties, "| Hundreds (100s):", prof$hundreds)
          )
        )
      }
      bowling_section <- if (is.null(bowl)) {
        div(class = "player-empty-state", "No bowling records found for this player in the selected season.")
      } else {
        tagList(
          h4(tagList(cl_icon("bowl"), "Bowling summary"), style = "color: #ff5e00; font-weight: 700; margin-top: 22px;"),
          fluidRow(
            column(3, div(class = "kpi-card orange", div(class = "kpi-label", "Matches / Innings"), div(class = "kpi-value", paste(bowl$matches, "/", bowl$innings)))),
            column(3, div(class = "kpi-card", div(class = "kpi-label", "Legal balls / Overs"), div(class = "kpi-value", paste(bowl$valid_balls, "/", value_or_dash(bowl$overs_bowled, 1))))),
            column(2, div(class = "kpi-card gold", div(class = "kpi-label", "Runs conceded"), div(class = "kpi-value", bowl$runs_given))),
            column(2, div(class = "kpi-card purple", div(class = "kpi-label", "Wickets"), div(class = "kpi-value", bowl$wickets))),
            column(2, div(class = "kpi-card", div(class = "kpi-label", "Economy"), div(class = "kpi-value", value_or_dash(bowl$economy))))
          ),
          p(style = "color: #94a3b8; font-size: 0.95rem; margin-top: 10px;",
            paste("Bowling average:", value_or_dash(bowl$average), "| Bowling strike rate:", value_or_dash(bowl$strike_rate))
          )
        )
      }

      tagList(
        h3(p_name, style = "color: #ffd700; font-weight: 800; margin-bottom: 15px;"),
        h4("Batting summary", style = "color: #00d2ff; font-weight: 700;"),
        batting_section,
        bowling_section
      )
    })

    output$player_batting_chart <- renderPlotly({
      req(input$search_player)
      d <- if (identical(input$season_select, "All")) {
        get_player_batting_season_breakdown(player_data(), input$search_player)
      } else {
        get_player_batting_breakdown(player_data(), input$search_player)
      }
      if (!nrow(d)) return(plotly_empty() %>% layout(title = "No batting records for the selected season"))
      if (identical(input$season_select, "All")) {
        d$label <- paste("Season", d$season_year)
        plot_ly(d, x = ~season_year, y = ~runs, type = "bar", name = "Runs",
                text = ~label, hovertemplate = "%{text}<br>Total runs: %{y}<extra></extra>",
                marker = list(color = cl_colors[["teal"]])) %>%
          add_lines(x = ~season_year, y = ~strike_rate, name = "Strike rate", yaxis = "y2",
                    text = ~label, hovertemplate = "%{text}<br>Strike rate: %{y:.2f}<extra></extra>",
                    line = list(color = cl_colors[["gold"]]), connectgaps = FALSE) %>%
          layout(title = "Season-level batting overview",
                 xaxis = list(title = "IPL season", dtick = 1),
                 yaxis = list(title = "Total runs", rangemode = "tozero"),
                 yaxis2 = list(title = "Strike rate", overlaying = "y", side = "right"),
                 hovermode = "x unified")
      } else {
        d$label <- paste(d$season_year, "-", d$match_id, "- innings", d$innings)
        plot_ly(d, x = ~match_number, y = ~runs, type = "scatter", mode = "lines+markers",
                text = ~label, hovertemplate = "%{text}<br>Runs: %{y}<extra></extra>",
                line = list(color = cl_colors[["teal"]]), marker = list(size = 7)) %>%
          layout(title = "Match/innings batting trend",
                 xaxis = list(title = "Matches / innings (chronological)"),
                 yaxis = list(title = "Runs scored"), hovermode = "x unified")
      }
    })

    output$player_bowling_chart <- renderPlotly({
      req(input$search_player)
      d <- if (identical(input$season_select, "All")) {
        get_player_bowling_season_breakdown(player_data(), input$search_player)
      } else {
        get_player_bowling_breakdown(player_data(), input$search_player)
      }
      if (!nrow(d)) return(plotly_empty() %>% layout(title = "No bowling records for the selected season"))
      if (identical(input$season_select, "All")) {
        d$label <- paste("Season", d$season_year)
        plot_ly(d, x = ~season_year, y = ~wickets, type = "bar", name = "Wickets",
                text = ~label, hovertemplate = "%{text}<br>Wickets: %{y}<extra></extra>",
                marker = list(color = cl_colors[["orange"]])) %>%
          add_lines(x = ~season_year, y = ~economy, name = "Economy", yaxis = "y2",
                    text = ~label, hovertemplate = "%{text}<br>Economy: %{y:.2f}<extra></extra>",
                    line = list(color = cl_colors[["gold"]]), connectgaps = FALSE) %>%
          layout(title = "Season-level bowling overview",
                 xaxis = list(title = "IPL season", dtick = 1),
                 yaxis = list(title = "Wickets", rangemode = "tozero"),
                 yaxis2 = list(title = "Economy", overlaying = "y", side = "right"),
                 hovermode = "x unified")
      } else {
        d$label <- paste(d$season_year, "-", d$match_id, "- innings", d$innings)
        plot_ly(d, x = ~match_number, y = ~wickets, type = "bar", name = "Wickets",
                text = ~label, hovertemplate = "%{text}<br>Wickets: %{y}<extra></extra>",
                marker = list(color = cl_colors[["orange"]])) %>%
          add_lines(y = ~economy, name = "Economy", yaxis = "y2",
                    text = ~label, hovertemplate = "%{text}<br>Economy: %{y:.2f}<extra></extra>",
                    line = list(color = cl_colors[["gold"]]), connectgaps = FALSE) %>%
          layout(title = "Match/innings bowling trend",
                 xaxis = list(title = "Matches / innings (chronological)"),
                 yaxis = list(title = "Wickets", rangemode = "tozero"),
                 yaxis2 = list(title = "Economy", overlaying = "y", side = "right"),
                 barmode = "group", hovermode = "x unified")
      }
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

    output$impact_chart <- renderPlotly({
      req(input$search_player)
      scores <- weight_player_impact_scores(impact_components(), input$bat_weight, input$bowl_weight)
      row <- scores[scores$player == input$search_player, , drop = FALSE]
      if (!nrow(row) || !is.finite(row$impact[1])) {
        return(plotly_empty() %>% layout(title = "Impact score unavailable for this player"))
      }
      raw <- c(row$batting_component[1], row$bowling_component[1])
      used_weights <- c(ifelse(is.finite(raw[1]), input$bat_weight, 0),
                        ifelse(is.finite(raw[2]), input$bowl_weight, 0))
      weight_total <- sum(used_weights)
      weighted <- if (weight_total > 0) raw * used_weights / weight_total else rep(NA_real_, 2)
      labels <- c("Batting component", "Bowling component")
      raw[!is.finite(raw)] <- NA_real_
      weighted[!is.finite(weighted)] <- NA_real_
      chart <- data.frame(
        component = rep(labels, 2),
        value = c(raw, weighted),
        kind = rep(c("Raw component", "Weighted contribution"), each = 2),
        stringsAsFactors = FALSE
      )
      chart$hover <- paste0(chart$kind, ": ", ifelse(is.na(chart$value), "N/A", round(chart$value, 3)))
      plot_ly() %>%
        add_trace(type = "indicator", mode = "gauge+number",
                  value = row$impact[1], domain = list(x = c(0, 1), y = c(0.70, 1)),
                  title = list(text = "Overall impact<br><sup>Reference average: 50</sup>"),
                  gauge = list(axis = list(range = c(0, 100)),
                               threshold = list(line = list(color = cl_colors[["gold"]], width = 4),
                                                thickness = 0.8, value = 50),
                               bar = list(color = cl_colors[["teal"]]),
                               steps = list(list(range = c(0, 50), color = "#EEF3F7"),
                                             list(range = c(50, 100), color = "#D9F0EE")))) %>%
        add_bars(data = chart, x = ~value, y = ~component, color = ~kind,
                 colors = c("Raw component" = cl_colors[["navy"]],
                            "Weighted contribution" = cl_colors[["orange"]]),
                 text = ~hover, hovertemplate = "%{text}<extra></extra>",
                 orientation = "h") %>%
        layout(title = paste("Relative score for", input$search_player),
               xaxis = list(title = "Component value", zeroline = TRUE, zerolinecolor = "#17243A"),
               yaxis = list(title = NULL, domain = c(0, 0.58)),
               legend = list(orientation = "h", y = 0.62),
               annotations = list(list(x = 0.5, y = 0.65, xref = "paper", yref = "paper",
                                       text = paste0("Weights: batting ", round(input$bat_weight * 100),
                                                     "% | bowling ", round(input$bowl_weight * 100), "%"),
                                       showarrow = FALSE, font = list(color = "#526176"))))
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
