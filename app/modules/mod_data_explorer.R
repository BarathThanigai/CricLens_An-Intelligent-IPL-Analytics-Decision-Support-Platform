mod_data_explorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "criclens-card", h3("IPL Data Explorer", class = "section-title"),
        uiOutput(ns("filter_controls")),
        fluidRow(
          column(6, actionButton(ns("reset"), "Reset filters", class = "btn btn-outline-info")),
          column(6, downloadButton(ns("download"), "Download filtered CSV", class = "btn btn-primary"))
        ),
        textOutput(ns("row_count"))),
    div(class = "criclens-card analytics-table-card",
        h4("Filtered data preview"),
        textInput(ns("global_search"), "Search values or jump to a column", value = "",
                  placeholder = "Search rows or columns"),
        div(class = "cl-explorer-table", DTOutput(ns("table"))))
  )
}

mod_data_explorer_server <- function(id, df_feat) {
  moduleServer(id, function(input, output, session) {
    full_data <- reactive(df_feat())
    ui_refresh <- reactiveVal(0L)
    `%||%` <- function(x, fallback) if (is.null(x)) fallback else x

    column_info <- reactive({
      d <- full_data()
      list(
        categorical = names(d)[vapply(d, function(x) (is.character(x) || is.factor(x)) &&
          length(unique(x[!is.na(x)])) > 1, logical(1))],
        numeric = names(d)[vapply(d, is.numeric, logical(1))],
        dates = names(d)[vapply(d, inherits, logical(1), "Date")]
      )
    })

    output$filter_controls <- renderUI({
      ui_refresh()
      d <- full_data()
      info <- column_info()
      cat_row <- function(i) {
        col <- isolate(input[[paste0("cat_col_", i)]]) %||% ""
        values <- if (nzchar(col) && col %in% names(d)) sort(unique(as.character(d[[col]]))) else character()
        values <- values[!is.na(values) & nzchar(values)]
        current <- isolate(input[[paste0("cat_value_", i)]]) %||% character()
        is_player_column <- col %in% c("batter", "bowler")
        value_ui <- if (length(values) <= 100L || is_player_column) {
          selectizeInput(session$ns(paste0("cat_value_", i)), "Value",
                         choices = values, selected = intersect(current, values),
                         multiple = TRUE, options = list(plugins = list("remove_button"),
                                                        maxOptions = 1000L))
        } else {
          textInput(session$ns(paste0("cat_value_", i)), "Value contains",
                    value = if (length(current)) current[[1L]] else "")
        }
        fluidRow(
          column(4, selectInput(session$ns(paste0("cat_col_", i)), paste("Category", i),
                                choices = c("Choose a column" = "", info$categorical),
                                selected = col)),
          column(8, value_ui)
        )
      }
      num_row <- function(i) {
        col <- isolate(input[[paste0("num_col_", i)]]) %||% ""
        fluidRow(
          column(4, selectInput(session$ns(paste0("num_col_", i)), paste("Numeric column", i),
                                choices = c("Choose a column" = "", info$numeric),
                                selected = col)),
          column(4, textInput(session$ns(paste0("num_min_", i)), "Minimum",
                              value = isolate(input[[paste0("num_min_", i)]]) %||% "")),
          column(4, textInput(session$ns(paste0("num_max_", i)), "Maximum",
                              value = isolate(input[[paste0("num_max_", i)]]) %||% ""))
        )
      }
      date_ui <- if (length(info$dates)) tagList(
        h5("Date range"),
        selectInput(session$ns("date_col"), "Date column",
                    choices = c("Choose a column" = "", info$dates),
                    selected = isolate(input$date_col) %||% ""),
        dateRangeInput(session$ns("date_range"), "Between")
      )
      tagList(
        h5("Categorical filters"),
        div(class = "cl-explorer-filter-list", lapply(seq_len(5L), cat_row)),
        h5("Numeric filters"),
        div(class = "cl-explorer-filter-list", lapply(seq_len(5L), num_row)),
        date_ui
      )
    })

    observe({
      lapply(seq_len(5L), function(i) {
        input[[paste0("cat_col_", i)]]
        input[[paste0("num_col_", i)]]
      })
      ui_refresh(isolate(ui_refresh()) + 1L)
    })

    observeEvent(input$reset, {
      updateTextInput(session, "global_search", value = "")
      for (i in seq_len(5L)) {
        updateSelectInput(session, paste0("cat_col_", i), selected = "")
        updateSelectizeInput(session, paste0("cat_value_", i), selected = character())
        updateTextInput(session, paste0("cat_value_", i), value = "")
        updateSelectInput(session, paste0("num_col_", i), selected = "")
        updateTextInput(session, paste0("num_min_", i), value = "")
        updateTextInput(session, paste0("num_max_", i), value = "")
      }
      if ("date_col" %in% names(input)) updateSelectInput(session, "date_col", selected = "")
    })

    filtered_data <- shiny::debounce(reactive({
      d <- full_data()
      cats <- lapply(seq_len(5L), function(i) {
        col <- input[[paste0("cat_col_", i)]]
        value <- input[[paste0("cat_value_", i)]]
        unique_values <- if (length(col) && col %in% names(d)) unique(d[[col]]) else character()
        is_player_column <- identical(as.character(col), "batter") ||
          identical(as.character(col), "bowler")
        if (length(unique_values) <= 100L || is_player_column) {
          list(column = col, values = value)
        } else {
          list(column = col, query = value)
        }
      })
      nums <- lapply(seq_len(5L), function(i) list(
        column = input[[paste0("num_col_", i)]],
        min = suppressWarnings(as.numeric(input[[paste0("num_min_", i)]])),
        max = suppressWarnings(as.numeric(input[[paste0("num_max_", i)]]))
      ))
      query <- trimws(input$global_search %||% "")
      is_column_query <- nchar(query) >= 2L &&
        any(grepl(tolower(query), tolower(names(d)), fixed = TRUE))
      apply_explorer_filters(
        d,
        global_search = if (is_column_query) "" else query,
        category_filters = cats,
        numeric_filters = nums,
        date_filter = list(column = input$date_col, range = input$date_range)
      )
    }), millis = 250)

    output$row_count <- renderText({
      d <- filtered_data()
      query <- trimws(input$global_search %||% "")
      matching_columns <- if (nchar(query) >= 2L) {
        names(full_data())[grepl(tolower(query), tolower(names(full_data())), fixed = TRUE)]
      } else character()
      if (length(matching_columns)) {
        paste0("Column match: ", paste(matching_columns, collapse = ", "),
               ". Highlighted and scrolled into view. ",
               format(nrow(d), big.mark = ","), " rows match the filters.")
      } else {
        paste0(format(nrow(d), big.mark = ","), " matching rows. ",
               "The table previews up to 50,000 rows; download includes every match.")
      }
    })

    output$table <- renderDT({
      d <- filtered_data()
      if (!nrow(d)) {
        return(datatable(data.frame(Message = "No rows match these filters."),
                         options = list(dom = "t"), rownames = FALSE))
      }
      preview <- utils::head(d, 50000L)
      column_search_id <- session$ns("global_search")
      callback <- htmlwidgets::JS(sprintf(
        "var input = document.getElementById('%s');
         var container = $(table.table().container());
         function highlightColumnName() {
           var term = input ? input.value.trim().toLowerCase() : '';
           container.find('thead th, tbody td').removeClass('cl-column-target');
           if (!term) return;
           var matches = [];
           table.columns().every(function(index) {
             var header = this.header();
             var name = (header.textContent || '').trim().toLowerCase();
             if (name.indexOf(term) !== -1) {
               $(header).addClass('cl-column-target');
               $(this.nodes()).addClass('cl-column-target');
               matches.push({index:index, name:name, exact:name === term,
                             starts:name.indexOf(term) === 0});
             }
           });
           if (!matches.length) return;
           matches.sort(function(a,b) {
             return (b.exact-a.exact) || (b.starts-a.starts) || (a.index-b.index);
           });
           var body = container.closest('.cl-explorer-table').get(0) ||
             container.find('.dataTables_scrollBody').get(0);
           if (!body) return;
           var target = table.column(matches[0].index).header();
           var left = target.getBoundingClientRect().left -
             body.getBoundingClientRect().left + body.scrollLeft;
           $(body).stop(true).animate({scrollLeft:Math.max(0, left-12)}, 180);
         }
         if (input) $(input).off('input.clColumnJump')
           .on('input.clColumnJump', highlightColumnName);
         table.off('draw.dt.clColumnJump')
           .on('draw.dt.clColumnJump', highlightColumnName);
         table.columns.adjust();
         setTimeout(function(){table.columns.adjust(); highlightColumnName();}, 100);",
        column_search_id
      ))
      datatable(preview,
                options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE, dom = "ltip"),
                callback = callback, rownames = FALSE)
    })

    output$download <- downloadHandler(
      filename = function() paste0("criclens-filtered-", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(filtered_data(), file, row.names = FALSE, na = "")
    )
    invisible(filtered_data)
  })
}
