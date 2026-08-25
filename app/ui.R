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

# Source module UI definitions
source(get_root_path("app/modules/mod_overview.R"))
source(get_root_path("app/modules/mod_players.R"))
source(get_root_path("app/modules/mod_teams.R"))
source(get_root_path("app/modules/mod_matches.R"))
source(get_root_path("app/modules/mod_venues.R"))
source(get_root_path("app/modules/mod_predictions.R"))
source(get_root_path("app/modules/mod_ai_chat.R"))

ui <- page_navbar(
  title = span(
    "CricLens", style = "font-family: 'Outfit', sans-serif; font-weight: 800; color: #00d2ff;"
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#00d2ff",
    secondary = "#ff5e00",
    bg = "#0b0f19",
    fg = "#f8fafc"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  nav_panel("🏠 Overview", mod_overview_ui("overview")),
  nav_panel("🏏 Players", mod_players_ui("players")),
  nav_panel("🏆 Teams", mod_teams_ui("teams")),
  nav_panel("🏟️ Matches", mod_matches_ui("matches")),
  nav_panel("📊 Venues", mod_venues_ui("venues")),
  nav_panel("⚡ Predictions", mod_predictions_ui("predictions")),
  nav_panel("🤖 Ask CricLens", mod_ai_chat_ui("ai_chat"))
)
