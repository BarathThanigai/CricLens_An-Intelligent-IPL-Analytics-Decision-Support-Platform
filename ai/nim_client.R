# ai/nim_client.R
# CricLens — NVIDIA NIM Client Integration Module
# Interface for natural-language query processing using NVIDIA NIM API

suppressMessages({
  library(httr)
  library(jsonlite)
})

.nim_client_state <- new.env(parent = emptyenv())
.nim_client_state$missing_key_reported <- FALSE

nim_client_status <- function() {
  if (!nzchar(Sys.getenv("NVIDIA_API_KEY"))) {
    return(
      "NVIDIA NIM is disabled because NVIDIA_API_KEY is not set; deterministic R analytics remain available."
    )
  }

  "NVIDIA NIM is configured."
}

#' Call NVIDIA NIM API Endpoint
#' @param prompt User prompt / messages to process
#' @param system_prompt System prompt providing domain context and instructions
#' @param model Model identifier
#' @return Natural language text response, or NULL when NIM is unavailable
call_nvidia_nim <- function(
  prompt,
  system_prompt = "You are CricLens AI assistant.",
  model = "meta/llama-3.1-70b-instruct"
) {
  api_key <- Sys.getenv("NVIDIA_API_KEY")

  if (!nzchar(api_key)) {
    if (!isTRUE(.nim_client_state$missing_key_reported)) {
      message(nim_client_status())
      .nim_client_state$missing_key_reported <- TRUE
    }
    return(NULL)
  }

  prompt <- if (is.character(prompt) && length(prompt) == 1L) {
    trimws(prompt)
  } else {
    ""
  }
  if (!nzchar(prompt)) {
    message("NVIDIA NIM request skipped: prompt is empty or invalid.")
    return(NULL)
  }

  url <- "https://integrate.api.nvidia.com/v1/chat/completions"
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = prompt)
    ),
    temperature = 0.2,
    max_tokens = 800
  )

  response <- tryCatch(
    POST(
      url,
      add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = body,
      encode = "json",
      timeout(15)
    ),
    error = function(error) {
      message("NVIDIA NIM request failed; using deterministic R fallback.")
      NULL
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (status_code(response) != 200) {
    message(
      "NVIDIA NIM returned HTTP ",
      status_code(response),
      "; using deterministic R fallback."
    )
    return(NULL)
  }

  parsed <- tryCatch(
    content(response, as = "parsed", encoding = "UTF-8"),
    error = function(error) {
      message("NVIDIA NIM returned an unreadable response; using deterministic R fallback.")
      NULL
    }
  )

  if (
    !is.list(parsed) ||
      !is.list(parsed$choices) ||
      length(parsed$choices) == 0 ||
      !is.list(parsed$choices[[1]])
  ) {
    message("NVIDIA NIM response had no choices; using deterministic R fallback.")
    return(NULL)
  }

  choice <- parsed$choices[[1]]
  message_object <- choice$message
  if (!is.list(message_object)) {
    message("NVIDIA NIM response had no message object; using deterministic R fallback.")
    return(NULL)
  }

  message_content <- message_object$content
  if (
    !is.character(message_content) ||
      length(message_content) != 1L ||
      is.na(message_content) ||
      !nzchar(trimws(message_content))
  ) {
    message("NVIDIA NIM response contained no usable message; using deterministic R fallback.")
    return(NULL)
  }

  message_content
}
