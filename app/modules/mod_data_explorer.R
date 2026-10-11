mod_data_explorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "criclens-card", h3("IPL Data Explorer", class = "section-title"),
        uiOutput(ns("filter_controls")),
        actionButton(ns("reset"), "Reset filters", class = "btn btn-outline-info"),
        downloadButton(ns("download"), "Download filtered CSV", class = "btn btn-primary"),
        textOutput(ns("row_count"))),
    div(class = "criclens-card analytics-table-card", h4("Filtered data preview"), DTOutput(ns("table"))),
    div(class = "criclens-card analytics-table-card", h4("Schema and missing values"), DTOutput(ns("schema")))
  )
}

mod_data_explorer_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    full_data <- reactive(df_feat())

    output$filter_controls <- renderUI({
      d <- isolate(full_data())
      categorical <- names(d)[vapply(d, function(x) (is.character(x) || is.factor(x)) && length(unique(x)) > 1, logical(1))]
      numeric_cols <- names(d)[vapply(d, is.numeric, logical(1))]
      date_cols <- names(d)[vapply(d, inherits, logical(1), "Date")]
      tagList(
        textInput(session$ns("global_search"), "Search all columns", value = "", placeholder = "Search the full dataset"),
        h5("Categorical filters"),
        lapply(1:3, function(i) fluidRow(
          column(5, selectInput(session$ns(paste0("cat_col_", i)), paste("Category", i), choices = c("Choose a column" = "", categorical))),
          column(7, selectizeInput(session$ns(paste0("cat_values_", i)), "Values", choices = NULL, multiple = TRUE,
                                   options = list(plugins = list("remove_button"))))
        )),
        h5("Text filters"),
        lapply(1:2, function(i) fluidRow(
          column(4, selectInput(session$ns(paste0("text_col_", i)), paste("Text column", i), choices = c("Choose a column" = "", categorical))),
          column(8, textInput(session$ns(paste0("text_query_", i)), "Contains", value = ""))
        )),
        h5("Numeric ranges"),
        lapply(1:3, function(i) fluidRow(
          column(4, selectInput(session$ns(paste0("num_col_", i)), paste("Numeric column", i), choices = c("Choose a column" = "", numeric_cols))),
          column(4, textInput(session$ns(paste0("num_min_", i)), "Minimum", value = "")),
          column(4, textInput(session$ns(paste0("num_max_", i)), "Maximum", value = ""))
        )),
        if (length(date_cols)) tagList(
          h5("Date range"),
          selectInput(session$ns("date_col"), "Date column", choices = c("Choose a column" = "", date_cols)),
          dateRangeInput(session$ns("date_range"), "Between", start = min(d[[date_cols[1]]], na.rm = TRUE),
                         end = max(d[[date_cols[1]]], na.rm = TRUE), min = min(d[[date_cols[1]]], na.rm = TRUE),
                         max = max(d[[date_cols[1]]], na.rm = TRUE))
        )
      )
    })

    lapply(1:3, function(i) observeEvent(input[[paste0("cat_col_", i)]], {
      d <- full_data(); col <- input[[paste0("cat_col_", i)]]
      vals <- if (!is.null(col) && nzchar(col) && col %in% names(d)) sort(unique(as.character(d[[col]]))) else character()
      vals <- vals[!is.na(vals)]
      updateSelectizeInput(session, paste0("cat_values_", i), choices = vals, selected = character(), server = TRUE)
    }, ignoreInit = TRUE))

    observeEvent(input$reset, {
      updateTextInput(session, "global_search", value = "")
      for (i in 1:3) {
        updateSelectInput(session, paste0("cat_col_", i), selected = "")
        updateSelectizeInput(session, paste0("cat_values_", i), selected = character())
        updateTextInput(session, paste0("num_min_", i), value = "")
        updateTextInput(session, paste0("num_max_", i), value = "")
      }
      for (i in 1:2) updateTextInput(session, paste0("text_query_", i), value = "")
      if ("date_col" %in% names(input)) updateSelectInput(session, "date_col", selected = "")
    })

    filtered_data <- shiny::debounce(reactive({
      d <- full_data()
      cats <- lapply(1:3, function(i) list(column = input[[paste0("cat_col_", i)]], values = input[[paste0("cat_values_", i)]]))
      texts <- lapply(1:2, function(i) list(column = input[[paste0("text_col_", i)]], query = input[[paste0("text_query_", i)]]))
      nums <- lapply(1:3, function(i) {
        lo <- suppressWarnings(as.numeric(input[[paste0("num_min_", i)]]))
        hi <- suppressWarnings(as.numeric(input[[paste0("num_max_", i)]]))
        list(column = input[[paste0("num_col_", i)]], min = lo, max = hi)
      })
      dr <- if (!is.null(input$date_range)) input$date_range else NULL
      date_filter <- list(column = input$date_col, range = dr)
      apply_explorer_filters(d, input$global_search, cats, texts, nums, date_filter)
    }), millis = 250)

    output$row_count <- renderText({
      d <- filtered_data()
      paste0(format(nrow(d), big.mark = ","), " matching rows. The table previews up to 50,000 rows; download includes every match.")
    })
    output$table <- renderDT({
      d <- filtered_data()
      if (!nrow(d)) return(datatable(data.frame(Message = "No rows match these filters."), options = list(dom = "t"), rownames = FALSE))
      preview <- utils::head(d, 50000L)
      datatable(preview, options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE), rownames = FALSE)
    })
    output$schema <- renderDT({
      d <- full_data()
      summary <- data.frame(Column = names(d), Type = vapply(d, function(x) paste(class(x), collapse = "/"), character(1)),
                            Missing = vapply(d, function(x) sum(is.na(x)), integer(1)), stringsAsFactors = FALSE)
      datatable(summary, options = list(pageLength = 15, scrollX = TRUE, autoWidth = FALSE), rownames = FALSE)
    })
    output$download <- downloadHandler(
      filename = function() paste0("criclens-filtered-", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(filtered_data(), file, row.names = FALSE, na = "")
    )
    invisible(filtered_data)
  })
}
