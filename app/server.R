# app/server.R
# CricLens Shiny Application Server Logic

suppressMessages({
  library(shiny)
  library(dplyr)
  library(plotly)
  library(DT)
})

# Find the project root whether Shiny starts with the project folder or app/ as
# its working directory. Shiny can change the working directory before it
# initializes a session, so resolve and retain the root when this file is read.
find_project_root <- function() {
  start <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    start,
    file.path(start, ".."),
    file.path(start, "../.."),
    file.path(start, "../../..")
  ))

  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[file.exists(file.path(candidates, "R", "player_analysis.R"))]

  if (!length(matches)) {
    stop("Could not find the CricLens project root containing R/player_analysis.R. ",
         "Run the app from the project folder or its app/ folder.")
  }

  matches[[1L]]
}

project_root <- find_project_root()

get_root_path <- function(rel_path) {
  path <- file.path(project_root, rel_path)
  if (!file.exists(path)) stop("Required CricLens file not found: ", path)
  path
}

# Source R analytics engine modules
# The modules below are sourced into .GlobalEnv so the separately sourced
# Shiny module functions can resolve their analytics helpers at render time.
source(get_root_path("R/player_analysis.R"), local = .GlobalEnv)
source(get_root_path("R/team_analysis.R"), local = .GlobalEnv)
source(get_root_path("R/match_analysis.R"), local = .GlobalEnv)
source(get_root_path("R/venue_analysis.R"), local = .GlobalEnv)
source(get_root_path("R/visualizations.R"), local = .GlobalEnv)
source(get_root_path("R/predictive_models.R"), local = .GlobalEnv)
source(get_root_path("R/data_filters.R"), local = .GlobalEnv)
source(get_root_path("R/data_explorer.R"), local = .GlobalEnv)
source(get_root_path("ai/nim_client.R"), local = .GlobalEnv)
source(get_root_path("ai/prompt_templates.R"), local = .GlobalEnv)
source(get_root_path("ai/query_router.R"), local = .GlobalEnv)

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
  win_model <- load_win_prob_model(
    get_root_path("data/processed/win_prob_model.rds")
  )

  # Keep module-level filters (for example, Player and Data Explorer) local to
  # their pages; the shared Season / Team / Venue bar has been removed.
  filtered_data <- reactive(df_feat)

  # Wire Module Servers
  mod_overview_server("overview", filtered_data)
  mod_players_server("players", filtered_data)
  mod_teams_server("teams", filtered_data)
  mod_matches_server("matches", filtered_data)
  mod_venues_server("venues", filtered_data)
  mod_player_h2h_server("player_h2h", filtered_data)
  mod_predictions_server("predictions", win_model)
  mod_data_explorer_server("data_explorer", filtered_data)
  mod_ai_chat_server("ai_chat", df_feat)
}
