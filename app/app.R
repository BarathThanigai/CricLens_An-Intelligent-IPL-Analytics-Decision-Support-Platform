# app/app.R
# CricLens — Main Shiny Application Entry Point

suppressMessages({
  library(shiny)
})

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)
