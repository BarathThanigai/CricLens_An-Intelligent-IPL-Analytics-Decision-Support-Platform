# Direct batter-versus-bowler comparison.
mod_player_h2h_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, div(class = "criclens-card",
        h4("Player matchup", style = "font-weight:700;"),
        selectInput(ns("player1"), "Player 1", choices = NULL),
        selectInput(ns("player2"), "Player 2", choices = NULL),
        p(class = "cl-help", "The graph compares runs scored in each direction: Player 1 batting against Player 2, then Player 2 against Player 1.")
      )),
      column(8, div(class = "criclens-card",
        h4("Direct batter vs bowler", style = "font-weight:700;"),
        uiOutput(ns("matchup_caption")),
        plotlyOutput(ns("matchup_chart"), height = "340px")
      ))
    ),
    fluidRow(
      column(12, div(class = "criclens-card analytics-table-card",
        h4("Ball-by-ball matchup stats", style = "font-weight:700;"),
        DTOutput(ns("matchup"))
      ))
    ),
    fluidRow(
      column(12, div(class = "criclens-card analytics-table-card",
        h4("Overall player profiles", style = "font-weight:700;"),
        p("Career batting and bowling records across all opponents, for context alongside the direct matchup above."),
        DTOutput(ns("profiles"))
      ))
    )
  )
}

mod_player_h2h_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    observeEvent(df_feat(), {
      d <- df_feat()
      players <- sort(unique(c(as.character(d$batter), as.character(d$bowler))))
      players <- players[!is.na(players) & nzchar(players)]
      player1 <- if ("V Kohli" %in% players) "V Kohli" else if (length(players)) players[[1]] else character()
      player2 <- if ("JJ Bumrah" %in% players) "JJ Bumrah" else if (length(players) > 1L) players[[2]] else character()
      updateSelectInput(session, "player1", choices = players, selected = player1)
      updateSelectInput(session, "player2", choices = players, selected = player2)
    }, ignoreInit = FALSE)

    matchup_data <- reactive({
      req(input$player1, input$player2)
      get_player_batter_bowler_matchup(df_feat(), c(input$player1, input$player2))
    })

    profile_data <- reactive({
      req(input$player1, input$player2)
      get_player_role_profiles(df_feat(), c(input$player1, input$player2))
    })

    output$matchup_caption <- renderUI({
      req(input$player1, input$player2)
      div(class = "cl-help", paste(
        input$player1, "batting vs", input$player2,
        "and the reverse · regular innings only"
      ))
    })

    output$matchup_chart <- renderPlotly({
      req(input$player1, input$player2)
      shiny::validate(shiny::need(input$player1 != input$player2, "Choose two different players to compare."))
      matchup <- matchup_data()
      shiny::validate(shiny::need(nrow(matchup) && any(matchup$Matches > 0),
                                  "No direct encounters between these players in the available ball-by-ball data."))

      chart_data <- matchup
      chart_data$Hover <- paste0(
        "<b>", chart_data$Matchup, "</b>",
        "<br>Runs: ", chart_data$Runs,
        "<br>Matches: ", chart_data$Matches,
        "<br>Balls faced: ", chart_data$`Balls faced`,
        "<br>Strike rate: ", ifelse(is.na(chart_data$`Strike rate`), "—", chart_data$`Strike rate`),
        "<br>Dismissals by bowler: ", chart_data$`Dismissals by bowler`,
        "<br>Bowler economy: ", ifelse(is.na(chart_data$`Bowler economy`), "—", chart_data$`Bowler economy`)
      )
      chart_data$Bar_label <- ifelse(
        is.na(chart_data$`Strike rate`),
        paste0(chart_data$Runs, " runs"),
        paste0(chart_data$Runs, " runs · SR ", chart_data$`Strike rate`)
      )
      x_limit <- max(1, max(chart_data$Runs, na.rm = TRUE)) * 1.45

      plotly::plot_ly(
        data = chart_data,
        x = ~Runs,
        y = ~Batter,
        type = "bar",
        orientation = "h",
        marker = list(color = c("#087e8b", "#c85b21")),
        text = ~Bar_label,
        textposition = "outside",
        cliponaxis = FALSE,
        hovertext = ~Hover,
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis = list(title = "Runs scored against the opposing player", range = c(0, x_limit), rangemode = "tozero", gridcolor = "#dce4ee"),
          yaxis = list(title = "Batter", automargin = TRUE, categoryorder = "array", categoryarray = rev(chart_data$Batter)),
          margin = list(l = 115, r = 30, t = 16, b = 52),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$matchup <- renderDT({
      req(input$player1, input$player2)
      if (input$player1 == input$player2) {
        return(datatable(data.frame(Note = "Choose two different players to compare."),
                         options = list(dom = "t"), rownames = FALSE))
      }
      matchup <- matchup_data()
      if (!nrow(matchup) || all(matchup$Matches == 0)) {
        return(datatable(data.frame(Note = "No direct encounters between these players in the available ball-by-ball data."),
                         options = list(dom = "t"), rownames = FALSE))
      }
      display <- matchup[, c("Matchup", "Matches", "Runs", "Balls faced", "Strike rate",
                             "Dismissals by bowler", "Bowler economy")]
      datatable(display, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), rownames = FALSE)
    })

    output$profiles <- renderDT({
      req(input$player1, input$player2)
      if (input$player1 == input$player2) {
        return(datatable(data.frame(Note = "Choose two different players to compare."),
                         options = list(dom = "t"), rownames = FALSE))
      }
      profiles <- profile_data()
      if (!nrow(profiles)) {
        return(datatable(data.frame(Note = "No batting or bowling records match these players."),
                         options = list(dom = "t"), rownames = FALSE))
      }

      profiles$sample <- paste0(profiles$matches, " matches / ", profiles$innings,
                                " innings / ", profiles$balls, " balls")
      profiles$rate_name <- ifelse(profiles$role == "Batting", "Strike rate", "Economy")
      profiles$rate <- round(profiles$rate, 2)
      profiles$average <- round(profiles$average, 2)
      profiles$boundary_pct <- round(profiles$boundary_pct, 1)
      profiles$runs_or_conceded <- round(profiles$runs_or_conceded, 1)
      profiles <- profiles[, c("player", "role", "sample", "runs_or_conceded", "wickets",
                               "average", "rate_name", "rate", "fours", "sixes", "boundary_pct")]
      names(profiles) <- c("Player", "Profile", "Sample", "Runs / conceded", "Wickets", "Average",
                           "Rate type", "Rate", "Fours", "Sixes", "Boundary % / bowling SR")
      datatable(profiles, options = list(pageLength = 8, scrollX = TRUE, autoWidth = FALSE), rownames = FALSE)
    })
  })
}
