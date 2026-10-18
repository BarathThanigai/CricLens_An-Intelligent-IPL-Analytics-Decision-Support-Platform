# app/modules/mod_ai_chat.R
# Ask CricLens AI Chat Module for CricLens Shiny App

suppressMessages({
  library(shiny)
})

mod_ai_chat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             div(class = "criclens-card",
                 h3(tagList(cl_icon("ai"), "Ask CricLens — Conversational Sports Analytics Interface"), style = "color: #00d2ff; font-weight: 800;"),
                 p(style = "color: #526176;", "Ask natural language questions about IPL players, teams, matches, or venues. R computes all statistics; NVIDIA NIM is optional for intent parsing and explanations."),
                 uiOutput(ns("nim_status")),
                 actionButton(
                   ns("refresh_nim_status"),
                   "Refresh connection status",
                   class = "btn btn-outline-secondary btn-sm",
                   style = "margin-bottom: 12px;"
                 ),
                 div(style = "margin-bottom: 15px;",
                     actionButton(ns("prompt_btn1"), tagList(cl_icon("bat"), "Who has the most runs in IPL?"), class = "btn btn-outline-info btn-sm", style = "margin-right: 5px;"),
                     actionButton(ns("prompt_btn2"), tagList(cl_icon("bowl"), "Who has the most wickets in IPL?"), class = "btn btn-outline-warning btn-sm", style = "margin-right: 5px;"),
                     actionButton(ns("prompt_btn3"), tagList(cl_icon("trophy"), "Which team has the most wins?"), class = "btn btn-outline-success btn-sm", style = "margin-right: 5px;"),
                     actionButton(ns("prompt_btn4"), tagList(cl_icon("chart"), "Show V Kohli stats"), class = "btn btn-outline-primary btn-sm")
                 ),
                 textInput(ns("user_query"), "Your Question:", placeholder = "e.g., Who has the highest strike rate?", width = "100%"),
                 actionButton(ns("submit_btn"), "Ask CricLens AI", class = "btn btn-primary", style = "margin-top: 10px;"),
                 hr(style = "border-color: #26334d; margin-top: 20px;"),
                 uiOutput(ns("chat_response_area"))
             )
      )
    )
  )
}

mod_ai_chat_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    turns <- reactiveVal(list())
    previous_query <- reactiveVal(NULL)
    nim_status <- reactiveVal({
      if (nzchar(Sys.getenv("NVIDIA_API_KEY"))) {
        list(
          status = "configured",
          message = "Configured. NVIDIA NIM connectivity will be checked when you ask a question."
        )
      } else {
        list(
          status = "not_configured",
          message = nim_client_status()
        )
      }
    })

    output$nim_status <- renderUI({
      status <- nim_status()
      status_label <- switch(
        status$status,
        connected = "Connected",
        checking = "Connecting",
        unavailable = "Temporarily unavailable",
        degraded = "Using deterministic fallback",
        not_configured = "Not configured",
        "Configured"
      )
      status_class <- switch(
        status$status,
        connected = "alert-success",
        not_configured = "alert-info",
        configured = "alert-info",
        checking = "alert-warning",
        unavailable = "alert-warning",
        degraded = "alert-warning",
        "alert-info"
      )
      div(
        class = paste("alert", status_class),
        role = "status",
        tags$strong(paste0(status_label, ": ")),
        status$message
      )
    })

    observeEvent(input$refresh_nim_status, {
      if (nzchar(Sys.getenv("NVIDIA_API_KEY"))) {
        nim_status(list(
          status = "configured",
          message = "Configured. NVIDIA NIM connectivity will be checked when you ask a question."
        ))
      } else {
        nim_status(list(
          status = "not_configured",
          message = nim_client_status()
        ))
      }
    })

    submit_query <- function(query) {
      query <- trimws(as.character(query))
      if (!length(query) || is.na(query[1]) || !nzchar(query[1])) {
        return(invisible(NULL))
      }

      query <- query[1]
      resolved_query <- resolve_criclens_followup(
        query,
        previous_query(),
        df_feat
      )

      nim_status(if (nzchar(Sys.getenv("NVIDIA_API_KEY"))) {
        list(
          status = "checking",
          message = "Checking NVIDIA NIM while R prepares the verified analytics."
        )
      } else {
        list(
          status = "not_configured",
          message = nim_client_status()
        )
      })
      nim_events <- list()
      update_nim_status <- function(status, message) {
        nim_events[[length(nim_events) + 1L]] <<- list(
          status = status,
          message = message
        )
        nim_status(list(status = status, message = message))
      }

      response <- withProgress(
        message = "Checking NVIDIA NIM and running verified R analytics...",
        value = 0.5,
        {
          process_criclens_query(
            resolved_query,
            df_feat,
            status_callback = update_nim_status
          )
        }
      )

      fallback_events <- Filter(
        function(event) {
          event$status %in% c("not_configured", "unavailable", "degraded")
        },
        nim_events
      )
      if (length(fallback_events)) {
        response <- paste(
          paste0(
            "**NVIDIA NIM:** ",
            fallback_events[[length(fallback_events)]]$message
          ),
          response,
          sep = "\n\n"
        )
      }

      previous_query(resolved_query)
      turns(c(
        turns(),
        list(list(
          question = query,
          response = render_markdown_to_html(response)
        ))
      ))
      invisible(NULL)
    }

    observeEvent(input$prompt_btn1, {
      submit_query("Who has the most runs in IPL?")
    })
    observeEvent(input$prompt_btn2, {
      submit_query("Who has the most wickets in IPL?")
    })
    observeEvent(input$prompt_btn3, {
      submit_query("Which team has the most wins?")
    })
    observeEvent(input$prompt_btn4, {
      submit_query("Show V Kohli stats")
    })

    observeEvent(input$submit_btn, {
      submit_query(input$user_query)
    })

    output$chat_response_area <- renderUI({
      chat_turns <- turns()
      if (length(chat_turns) == 0) {
        return(div(style = "color: #94a3b8; font-style: italic;", "Type a question or select a prompt chip above to query CricLens AI."))
      }

      tagList(lapply(chat_turns, function(turn) {
        div(
          class = "criclens-card",
          style = "background-color: #fff; border-color: #dce4ee;",
          tags$p(
            style = "color: #526176;",
            tags$strong("You: "),
            htmltools::htmlEscape(turn$question)
          ),
          HTML(turn$response)
        )
      }))
    })
  })
}
