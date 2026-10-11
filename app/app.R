# app/app.R
# CricLens — Main Shiny Application Entry Point

suppressMessages({
  library(shiny)
})

app_file <- sys.frame(1)$ofile

if (is.null(app_file)) {
  stop("Unable to determine the CricLens app.R location.")
}

app_dir <- dirname(
  normalizePath(
    app_file,
    winslash = "/",
    mustWork = TRUE
  )
)
project_root <- dirname(app_dir)
options(
  criclens.app_dir = app_dir,
  criclens.project_root = project_root
)
app_env <- new.env(
  parent = globalenv()
)
app_env$app_dir <- app_dir
app_env$project_root <- project_root

source(
  file.path(app_dir, "ui.R"),
  local = app_env
)
source(
  file.path(app_dir, "server.R"),
  local = app_env
)

shinyApp(
  ui = app_env$ui,
  server = app_env$server
)
