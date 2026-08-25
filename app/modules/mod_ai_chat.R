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
                 h3("🤖 Ask CricLens — Conversational Sports Analytics Interface", style = "color: #00d2ff; font-weight: 800;"),
                 p(style = "color: #94a3b8;", "Ask natural language questions about IPL players, teams, matches, or venues. Powered by R Analytics & NVIDIA NIM API."),
                 div(style = "margin-bottom: 15px;",
                     actionButton(ns("prompt_btn1"), "🏏 Who has the most runs in IPL?", class = "btn btn-outline-info btn-sm", style = "margin-right: 5px;"),
                     actionButton(ns("prompt_btn2"), "🎯 Who has the most wickets in IPL?", class = "btn btn-outline-warning btn-sm", style = "margin-right: 5px;"),
                     actionButton(ns("prompt_btn3"), "🏆 Which team has the most wins?", class = "btn btn-outline-success btn-sm", style = "margin-right: 5px;"),
                     actionButton(ns("prompt_btn4"), "📊 Show V Kohli stats", class = "btn btn-outline-primary btn-sm")
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
    ns <- session$ns
    query_val <- reactiveVal(NULL)

    observeEvent(input$prompt_btn1, { query_val("Who has the most runs in IPL?") })
    observeEvent(input$prompt_btn2, { query_val("Who has the most wickets in IPL?") })
    observeEvent(input$prompt_btn3, { query_val("Which team has the most wins?") })
    observeEvent(input$prompt_btn4, { query_val("Show V Kohli stats") })

    observeEvent(input$submit_btn, {
      req(input$user_query)
      query_val(input$user_query)
    })

    output$chat_response_area <- renderUI({
      q <- query_val()
      if (is.null(q) || nchar(q) == 0) {
        return(div(style = "color: #94a3b8; font-style: italic;", "Type a question or select a prompt chip above to query CricLens AI."))
      }

      withProgress(message = 'Processing query with R & NVIDIA NIM...', value = 0.5, {
        res_markdown <- process_criclens_query(q, df_feat)
        res_html <- render_markdown_to_html(res_markdown)
      })

      div(
        class = "criclens-card",
        style = "background-color: #1e2942; border-color: #00d2ff;",
        HTML(res_html)
      )
    })
  })
}
