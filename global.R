# Shared setup and data loading for CricLens.
options(shiny.maxRequestSize = 100 * 1024^2)
source("R/data_ingestion.R", local = TRUE)
source("R/analytics.R", local = TRUE)
source("R/predictive_models.R", local = TRUE)
source("R/ai_assistant.R", local = TRUE)

criclens_data <- load_criclens_data("data/raw", "data/processed")
