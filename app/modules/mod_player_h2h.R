# Role-aware player head-to-head comparison.
mod_player_h2h_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, div(class = "criclens-card",
        h4("Player matchup", style = "color:#00d2ff;font-weight:700;"),
        selectInput(ns("player1"), "Player 1", choices = NULL),
        selectInput(ns("player2"), "Player 2", choices = NULL),
        selectInput(ns("venue"), "Venue (optional)", choices = c("All venues" = ""))
      )),
      column(8, div(class = "criclens-card analytics-table-card",
        h4("Role-based batting and bowling profiles", style = "color:#ffd700;font-weight:700;"),
        p("Each player is shown with the batting and/or bowling profile supported by their records. Samples are limited to regular innings and the selected venue."),
        DTOutput(ns("profiles"))
      ))
    )
  )
}

mod_player_h2h_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    observe({
      d <- df_feat()
      players <- sort(unique(c(as.character(d$batter), as.character(d$bowler))))
      players <- players[!is.na(players) & nzchar(players)]
      updateSelectInput(session, "player1", choices = players,
                        selected = if ("V Kohli" %in% players) "V Kohli" else if (length(players)) players[1] else character())
      updateSelectInput(session, "player2", choices = players,
                        selected = if ("JJ Bumrah" %in% players) "JJ Bumrah" else if (length(players) > 1) players[2] else character())
      venues <- sort(unique(as.character(d$venue)))
      venues <- venues[!is.na(venues) & nzchar(venues)]
      updateSelectInput(session, "venue", choices = c("All venues" = "", venues), selected = "")
    })

    output$profiles <- renderDT({
      req(input$player1, input$player2)
      if (input$player1 == input$player2) {
        return(datatable(data.frame(Note = "Choose two different players to compare."), options = list(dom = "t"), rownames = FALSE))
      }
      profiles <- get_player_role_profiles(df_feat(), c(input$player1, input$player2), input$venue)
      if (!nrow(profiles)) {
        profiles <- data.frame(Note = "No batting or bowling records match these players and filters.")
      } else {
        profiles$sample <- paste0(profiles$matches, " matches / ", profiles$innings,
                                  " innings / ", profiles$balls, " balls")
        profiles$rate_name <- ifelse(profiles$role == "Batting", "Strike rate", "Economy")
        profiles$rate <- round(profiles$rate, 2)
        profiles$average <- round(profiles$average, 2)
        profiles$boundary_pct <- round(profiles$boundary_pct, 1)
        profiles$runs_or_conceded <- round(profiles$runs_or_conceded, 1)
        profiles <- profiles[, c("player", "role", "sample", "runs_or_conceded", "wickets", "average", "rate_name", "rate", "fours", "sixes", "boundary_pct")]
        names(profiles) <- c("Player", "Profile", "Sample", "Runs / conceded", "Wickets", "Average", "Rate type", "Rate", "Fours", "Sixes", "Boundary % / bowling SR")
      }
      datatable(profiles, options = list(pageLength = 8, scrollX = TRUE, autoWidth = FALSE), rownames = FALSE)
    })
  })
}
