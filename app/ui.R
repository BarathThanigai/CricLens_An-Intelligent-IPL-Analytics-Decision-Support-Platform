# app/ui.R
# CricLens Shiny Application User Interface

suppressMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
})

app_dir <- getOption("criclens.app_dir")
if (is.null(app_dir) || !file.exists(file.path(app_dir, "modules", "mod_overview.R"))) {
  app_candidates <- normalizePath(
    c(getwd(), dirname(getwd())),
    winslash = "/",
    mustWork = FALSE
  )
  app_dir <- app_candidates[
    file.exists(file.path(app_candidates, "modules", "mod_overview.R"))
  ][1]
}
if (is.na(app_dir) || !length(app_dir)) {
  stop("Unable to locate the CricLens app modules.")
}

source(file.path(app_dir, "modules", "mod_overview.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_players.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_teams.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_matches.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_venues.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_predictions.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_ai_chat.R"), local = TRUE)

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
