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

server <- function(input, output, session) {
  # Load pre-processed binary datasets & caches
  df_feat <- readRDS(get_root_path("data/processed/ipl_featured.rds"))
  batting_summary <- readRDS(get_root_path("data/processed/player_batting_summary.rds"))
  bowling_summary <- readRDS(get_root_path("data/processed/player_bowling_summary.rds"))
  team_summary <- readRDS(get_root_path("data/processed/team_summary.rds"))
  venue_summary <- readRDS(get_root_path("data/processed/venue_summary.rds"))
  win_model <- readRDS(get_root_path("data/processed/win_prob_model.rds"))

  # Wire Module Servers
  mod_overview_server("overview", df_feat, team_summary, batting_summary)
  mod_players_server("players", df_feat)
  mod_teams_server("teams", df_feat)
  mod_matches_server("matches", df_feat)
  mod_venues_server("venues", df_feat)
  mod_predictions_server("predictions", win_model)
  mod_ai_chat_server("ai_chat", df_feat)
}
