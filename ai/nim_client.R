# ai/nim_client.R
# CricLens — NVIDIA NIM Client Integration Module
# Interface for natural-language query processing using NVIDIA NIM API

suppressMessages({
  library(httr)
  library(jsonlite)
})

.nim_client_state <- new.env(parent = emptyenv())
.nim_client_state$missing_key_reported <- FALSE

sanitize_nim_error_body <- function(body, api_key, max_chars = 1200L) {
  if (!is.character(body) || length(body) != 1L || is.na(body)) {
    return("")
  }

  body <- gsub("[[:cntrl:]]+", " ", body)
  body <- trimws(body)
  if (nzchar(api_key)) {
    body <- gsub(api_key, "[REDACTED]", body, fixed = TRUE)
  }
  body <- gsub(
    "(?i)(\"?(?:authorization|api[_ -]?key|access[_ -]?token|token)\"?\\s*[:=]\\s*\"?)(?:bearer\\s+)?[^\\s,\"'}]+",
    "\\1[REDACTED]",
    body,
    perl = TRUE
  )
  body <- gsub(
    "(?i)Bearer\\s+[A-Za-z0-9._~+/-]+=*",
    "Bearer [REDACTED]",
    body,
    perl = TRUE
  )

  if (nchar(body, type = "chars") > max_chars) {
    body <- paste0(substr(body, 1L, max_chars), "...")
  }

  body
}

nim_client_status <- function() {
  if (!nzchar(Sys.getenv("NVIDIA_API_KEY"))) {
    return("Set NVIDIA_API_KEY in the server environment to enable NVIDIA NIM. Deterministic R analytics remain available.")
  }

  "Configured: NIM connectivity will be verified when you ask a question."
}

#' Call NVIDIA NIM API Endpoint
#' @param prompt User prompt / messages to process
#' @param system_prompt System prompt providing domain context and instructions
#' @param model Model identifier
#' @param status_callback Optional function called with status and user-safe message
#' @return Natural language text response, or NULL when NIM is unavailable
call_nvidia_nim <- function(
  prompt,
  system_prompt = "You are CricLens AI assistant.",
  model = "nvidia/nemotron-3.5-lightning-30b-a3b",
  status_callback = NULL
) {
  report_status <- function(status, message) {
    if (is.function(status_callback)) {
      status_callback(status, message)
    }
  }

  api_key <- Sys.getenv("NVIDIA_API_KEY")

  if (!nzchar(api_key)) {
    report_status(
      "not_configured",
      "NVIDIA NIM is not configured. Set NVIDIA_API_KEY in the server environment; deterministic R analytics are still available."
    )
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
    report_status(
      "unavailable",
      "NVIDIA NIM was not called because the request prompt was empty or invalid."
    )
    return(NULL)
  }

  report_status("checking", "Connecting to NVIDIA NIM...")
  url <- "https://integrate.api.nvidia.com/v1/chat/completions"
  timeout_seconds <- 60
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = prompt)
    ),
    temperature = 0.2,
    max_tokens = 512,
    chat_template_kwargs = list(enable_thinking = FALSE)
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
      timeout(timeout_seconds)
    ),
    error = function(error) {
      error_message <- conditionMessage(error)
      request_error <- sanitize_nim_error_body(
        error_message,
        api_key
      )
      error_kind <- if (grepl(
        "timed out|timeout|time-out",
        error_message,
        ignore.case = TRUE
      )) {
        "transport timeout"
      } else {
        "transport error"
      }
      diagnostic_detail <- if (nzchar(request_error)) {
        request_error
      } else {
        "The connection error was empty or unreadable."
      }
      diagnostic <- paste0("NVIDIA NIM ", error_kind, ": ", diagnostic_detail)
      diagnostic <- paste0(
        diagnostic,
        " Deterministic R parsing and analytics are being used."
      )
      report_status(
        "unavailable",
        diagnostic
      )
      message(diagnostic)
      NULL
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (status_code(response) != 200) {
    http_status <- status_code(response)
    response_body <- tryCatch(
      content(response, as = "text", encoding = "UTF-8"),
      error = function(error) ""
    )
    diagnostic_body <- sanitize_nim_error_body(response_body, api_key)
    diagnostic <- paste0("NVIDIA NIM returned HTTP ", http_status)
    if (nzchar(diagnostic_body)) {
      diagnostic <- paste0(diagnostic, ": ", diagnostic_body)
    } else {
      diagnostic <- paste0(diagnostic, ". The response body was empty or unreadable.")
    }
    diagnostic <- paste0(diagnostic, " Deterministic R parsing and analytics are being used.")

    report_status(
      "unavailable",
      diagnostic
    )
    message(diagnostic)
    return(NULL)
  }

  parsed <- tryCatch(
    content(response, as = "parsed", encoding = "UTF-8"),
    error = function(error) {
      report_status(
        "unavailable",
        "NVIDIA NIM returned an unreadable response. Deterministic R parsing and analytics are being used."
      )
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
    report_status(
      "unavailable",
      "NVIDIA NIM returned an invalid response. Deterministic R parsing and analytics are being used."
    )
    message("NVIDIA NIM response had no choices; using deterministic R fallback.")
    return(NULL)
  }

  choice <- parsed$choices[[1]]
  message_object <- choice$message
  if (!is.list(message_object)) {
    report_status(
      "unavailable",
      "NVIDIA NIM returned an invalid response. Deterministic R parsing and analytics are being used."
    )
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
    report_status(
      "unavailable",
      paste(
        "NVIDIA NIM returned no usable assistant content;",
        "internal reasoning is not shown.",
        "Deterministic R parsing and analytics are being used."
      )
    )
    message("NVIDIA NIM response contained no usable assistant content; using deterministic R fallback.")
    return(NULL)
  }

  report_status(
    "connected",
    "Connected: the latest NVIDIA NIM request succeeded. R remains the source of all statistics."
  )
  message_content
}
