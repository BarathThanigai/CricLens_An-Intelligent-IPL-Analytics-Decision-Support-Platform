# app/ui.R
# CricLens Shiny Application User Interface

suppressMessages({
  library(shiny)
  library(bslib)
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

# Shared visual helpers from the presentation layer.
source(get_root_path("R/criclens_ui.R"), local = .GlobalEnv)
source(get_root_path("R/criclens_charts.R"), local = .GlobalEnv)

# Source module UI definitions
source(get_root_path("app/modules/mod_overview.R"))
source(get_root_path("app/modules/mod_players.R"))
source(get_root_path("app/modules/mod_teams.R"))
source(get_root_path("app/modules/mod_matches.R"))
source(get_root_path("app/modules/mod_venues.R"))
source(get_root_path("app/modules/mod_predictions.R"))
source(get_root_path("app/modules/mod_ai_chat.R"))
source(get_root_path("app/modules/mod_player_h2h.R"))
source(get_root_path("app/modules/mod_data_explorer.R"))

ui <- page_navbar(
  title = cl_brand("CricLens", "IPL Analytics"),
  theme = criclens_theme(),
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), tags$link(rel = "stylesheet", type = "text/css", href = "criclens.css"), tags$script(src = "criclens.js"))
  ),
  nav_panel(tagList(cl_icon("overview"), "Overview"), value = "overview", mod_overview_ui("overview")),
  nav_panel(tagList(cl_icon("players"), "Players"), value = "players", mod_players_ui("players")),
  nav_panel(tagList(cl_icon("teams"), "Teams"), value = "teams", mod_teams_ui("teams")),
  nav_panel(tagList(cl_icon("matches"), "Matches"), value = "matches", mod_matches_ui("matches")),
  nav_panel(tagList(cl_icon("venues"), "Venues"), value = "venues", mod_venues_ui("venues")),
  nav_panel(tagList(cl_icon("h2h"), "Player H2H"), value = "player_h2h", mod_player_h2h_ui("player_h2h")),
  nav_panel(tagList(cl_icon("predictions"), "Predictions"), value = "predictions", mod_predictions_ui("predictions")),
  nav_panel(tagList(cl_icon("explorer"), "Data Explorer"), value = "data_explorer", mod_data_explorer_ui("data_explorer")),
  nav_panel(tagList(cl_icon("ai"), "Ask CricLens"), value = "ai_chat", mod_ai_chat_ui("ai_chat"))
)

