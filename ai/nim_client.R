# ai/nim_client.R
# CricLens — NVIDIA NIM Client Integration Module
# Interface for natural-language query processing using NVIDIA NIM API

suppressMessages({
  library(httr)
  library(jsonlite)
})

#' Call NVIDIA NIM API Endpoint
#' @param prompt User prompt / message
#' @param system_prompt System prompt providing domain context and instructions
#' @param model Model identifier (default: meta/llama-3.1-70b-instruct)
#' @return Natural language text response from NVIDIA NIM
call_nvidia_nim <- function(prompt, system_prompt = "You are CricLens AI assistant.", model = "meta/llama-3.1-70b-instruct") {
  api_key <- Sys.getenv("NVIDIA_API_KEY")

  # Fallback gracefully if API key is not configured
  if (nchar(api_key) == 0) {
    return(paste(
      "[NVIDIA NIM API Key Notice]: NVIDIA_API_KEY environment variable is not set.",
      "The query was processed using CricLens local R analytics engine directly."
    ))
  }

  url <- "https://integrate.api.nvidia.com/v1/chat/completions"

  headers <- c(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = prompt)
    ),
    temperature = 0.2,
    max_tokens = 800
  )

  res <- tryCatch({
    POST(url, add_headers(.headers = headers), body = body, encode = "json", timeout(15))
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(res) || status_code(res) != 200) {
    return(NULL)
  }

  parsed <- content(res, as = "parsed", encoding = "UTF-8")
  if (!is.null(parsed$choices[[1]]$message$content)) {
    return(parsed$choices[[1]]$message$content)
  }

  return(NULL)
}
