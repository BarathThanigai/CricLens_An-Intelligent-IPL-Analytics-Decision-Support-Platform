# app/server.R
# CricLens Shiny Application Server Logic

suppressMessages({
  library(shiny)
  library(dplyr)
  library(plotly)
  library(DT)
})

# Robust path helper for root vs app directory execution
get_root_path <- function(rel_path) {
  if (file.exists(rel_path)) {
    return(rel_path)
  } else if (file.exists(file.path("..", rel_path))) {
    return(file.path("..", rel_path))
  } else {
    return(rel_path)
  }
}

# Source R analytics engine modules
source(get_root_path("R/player_analysis.R"), local = TRUE)
source(get_root_path("R/team_analysis.R"), local = TRUE)
source(get_root_path("R/match_analysis.R"), local = TRUE)
source(get_root_path("R/venue_analysis.R"), local = TRUE)
source(get_root_path("R/visualizations.R"), local = TRUE)
source(get_root_path("R/predictive_models.R"), local = TRUE)
source(get_root_path("R/data_filters.R"), local = TRUE)
source(get_root_path("R/data_explorer.R"), local = TRUE)
source(get_root_path("ai/nim_client.R"), local = TRUE)
source(get_root_path("ai/prompt_templates.R"), local = TRUE)
source(get_root_path("ai/query_router.R"), local = TRUE)

# Source Shiny server modules
source(get_root_path("app/modules/mod_overview.R"))
source(get_root_path("app/modules/mod_players.R"))
source(get_root_path("app/modules/mod_teams.R"))
source(get_root_path("app/modules/mod_matches.R"))
source(get_root_path("app/modules/mod_venues.R"))
source(get_root_path("app/modules/mod_predictions.R"))
source(get_root_path("app/modules/mod_ai_chat.R"))
source(get_root_path("app/modules/mod_player_h2h.R"))
source(get_root_path("app/modules/mod_data_explorer.R"))

server <- function(input, output, session) {
  # Load pre-processed binary datasets & caches
  df_feat <- readRDS(get_root_path("data/processed/ipl_featured.rds"))
  win_model <- readRDS(get_root_path("data/processed/win_prob_model.rds"))

  output$global_filter_bar <- renderUI({
    seasons <- sort(unique(df_feat$season_year[!is.na(df_feat$season_year)]))
    teams <- sort(unique(c(df_feat$batting_team, df_feat$bowling_team)))
    teams <- teams[!is.na(teams) & nzchar(teams)]
    venues <- sort(unique(df_feat$venue[!is.na(df_feat$venue) & nzchar(df_feat$venue)]))
    div(class = "global-filter-bar",
      selectInput("global_season", "Season", choices = c("All" = "All", seasons), selected = "All"),
      selectInput("global_team", "Team", choices = c("All teams" = "All", teams), selected = "All"),
      selectInput("global_venue", "Venue", choices = c("All venues" = "All", venues), selected = "All")
    )
  })

  filtered_data <- reactive({
    filter_global_data(df_feat, input$global_season, input$global_team, input$global_venue)
  })

  # Wire Module Servers
  mod_overview_server("overview", filtered_data)
  mod_players_server("players", filtered_data)
  mod_teams_server("teams", filtered_data)
  mod_matches_server("matches", filtered_data)
  mod_venues_server("venues", filtered_data)
  mod_player_h2h_server("player_h2h", filtered_data)
  mod_predictions_server("predictions", win_model)
  mod_data_explorer_server("data_explorer", filtered_data)
  mod_ai_chat_server("ai_chat", filtered_data)
}
