# app/server.R
# CricLens Shiny Application Server Logic

suppressMessages({
  library(shiny)
  library(dplyr)
  library(plotly)
  library(DT)
})

project_root <- getOption("criclens.project_root")
if (is.null(project_root) || !file.exists(file.path(project_root, "R", "player_analysis.R"))) {
  root_candidates <- normalizePath(
    c(getwd(), dirname(getwd())),
    winslash = "/",
    mustWork = FALSE
  )
  project_root <- root_candidates[
    file.exists(file.path(root_candidates, "R", "player_analysis.R"))
  ][1]
}
if (is.na(project_root) || !length(project_root)) {
  stop("Unable to locate the CricLens project root.")
}

module_files <- file.path(
  project_root,
  "app",
  "modules",
  paste0(
    "mod_",
    c("overview", "players", "teams", "matches", "venues", "predictions", "ai_chat"),
    ".R"
  )
)
for (module_file in module_files) {
  source(module_file, local = TRUE)
}

# Source R analytics engine modules
source(file.path(project_root, "R", "player_analysis.R"), local = TRUE)
source(file.path(project_root, "R", "team_analysis.R"), local = TRUE)
source(file.path(project_root, "R", "match_analysis.R"), local = TRUE)
source(file.path(project_root, "R", "venue_analysis.R"), local = TRUE)
source(file.path(project_root, "R", "visualizations.R"), local = TRUE)
source(file.path(project_root, "R", "predictive_models.R"), local = TRUE)
source(file.path(project_root, "ai", "nim_client.R"), local = TRUE)
source(file.path(project_root, "ai", "prompt_templates.R"), local = TRUE)
source(file.path(project_root, "ai", "query_router.R"), local = TRUE)

server <- function(input, output, session) {
  # Load pre-processed binary datasets & caches
  df_feat <- readRDS(
    file.path(project_root, "data", "processed", "ipl_featured.rds")
  )
  batting_summary <- readRDS(
    file.path(project_root, "data", "processed", "player_batting_summary.rds")
  )
  bowling_summary <- readRDS(
    file.path(project_root, "data", "processed", "player_bowling_summary.rds")
  )
  team_summary <- get_team_performance_summary(df_feat)
  venue_summary <- readRDS(
    file.path(project_root, "data", "processed", "venue_summary.rds")
  )
  win_model <- readRDS(
    file.path(project_root, "data", "processed", "win_prob_model.rds")
  )

  # Wire Module Servers
  mod_overview_server("overview", df_feat, team_summary, batting_summary)
  mod_players_server("players", df_feat)
  mod_teams_server("teams", df_feat)
  mod_matches_server("matches", df_feat)
  mod_venues_server("venues", df_feat)
  mod_predictions_server("predictions", win_model)
  mod_ai_chat_server("ai_chat", df_feat)
}
