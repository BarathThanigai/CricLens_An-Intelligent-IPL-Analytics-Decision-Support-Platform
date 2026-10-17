mod_data_explorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "criclens-card", h3("IPL Data Explorer", class = "section-title"),
        uiOutput(ns("filter_controls")),
        actionButton(ns("reset"), "Reset filters", class = "btn btn-outline-info"),
        downloadButton(ns("download"), "Download filtered CSV", class = "btn btn-primary"),
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

    output$filter_controls <- renderUI({
      d <- isolate(full_data())
      categorical <- names(d)[vapply(d, function(x) (is.character(x) || is.factor(x)) && length(unique(x)) > 1, logical(1))]
      numeric_cols <- names(d)[vapply(d, is.numeric, logical(1))]
      date_cols <- names(d)[vapply(d, inherits, logical(1), "Date")]
      tagList(
        h5("Categorical filters"),
        lapply(1:3, function(i) fluidRow(
          column(5, selectInput(session$ns(paste0("cat_col_", i)), paste("Category", i), choices = c("Choose a column" = "", categorical))),
          column(7, selectizeInput(session$ns(paste0("cat_values_", i)), "Values", choices = NULL, multiple = TRUE,
                                   options = list(plugins = list("remove_button"))))
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
      if ("date_col" %in% names(input)) updateSelectInput(session, "date_col", selected = "")
    })

    filtered_data <- shiny::debounce(reactive({
      d <- full_data()
      cats <- lapply(1:3, function(i) list(column = input[[paste0("cat_col_", i)]], values = input[[paste0("cat_values_", i)]]))
      nums <- lapply(1:3, function(i) {
        lo <- suppressWarnings(as.numeric(input[[paste0("num_min_", i)]]))
        hi <- suppressWarnings(as.numeric(input[[paste0("num_max_", i)]]))
        list(column = input[[paste0("num_col_", i)]], min = lo, max = hi)
      })
      dr <- if (!is.null(input$date_range)) input$date_range else NULL
      date_filter <- list(column = input$date_col, range = dr)
      query <- if (is.null(input$global_search)) "" else trimws(input$global_search)
      # A column-name query navigates to that column instead of filtering out
      # all rows because the column name is not itself a cell value.
      is_column_query <- nchar(query) >= 2L && any(grepl(tolower(query), tolower(names(d)), fixed = TRUE))
      value_query <- if (is_column_query) "" else query
      apply_explorer_filters(
        d, global_search = value_query, category_filters = cats,
        numeric_filters = nums, date_filter = date_filter
      )
    }), millis = 250)

    output$row_count <- renderText({
      d <- filtered_data()
      query <- if (is.null(input$global_search)) "" else trimws(input$global_search)
      matching_columns <- if (nchar(query) >= 2L) names(full_data())[grepl(tolower(query), tolower(names(full_data())), fixed = TRUE)] else character()
      if (length(matching_columns)) {
        paste0("Column match: ", paste(matching_columns, collapse = ", "), ". Highlighted and scrolled into view. ",
               format(nrow(d), big.mark = ","), " rows match the other filters.")
      } else {
        paste0(format(nrow(d), big.mark = ","), " matching rows. The table previews up to 50,000 rows; download includes every match.")
      }
    })
    output$table <- renderDT({
      d <- filtered_data()
      if (!nrow(d)) return(datatable(data.frame(Message = "No rows match these filters."), options = list(dom = "t"), rownames = FALSE))
      preview <- utils::head(d, 50000L)
      column_search_id <- session$ns("global_search")
      jump_to_column <- htmlwidgets::JS(sprintf(
        "var input = document.getElementById('%s');
          var $container = $(table.table().container());
          function highlightColumnName() {
            var term = input ? input.value.trim().toLowerCase() : '';
            $container.find('thead th, tbody td').removeClass('cl-column-target');
            if (!term) return;
            var matches = [];
            table.columns().every(function(index) {
              var column = this;
              var header = column.header();
              var name = (header.textContent || '').trim().toLowerCase();
              if (name.indexOf(term) !== -1) {
                $(header).addClass('cl-column-target');
                $(column.nodes()).addClass('cl-column-target');
                matches.push({ index: index, name: name, exact: name === term, starts: name.indexOf(term) === 0 });
              }
            });
            if (!matches.length) return;
            matches.sort(function(a, b) {
              return (b.exact - a.exact) || (b.starts - a.starts) || (a.index - b.index);
            });
            var body = $container.closest('.cl-explorer-table').get(0) || $container.find('.dataTables_scrollBody').get(0);
            if (!body) return;
            var target = table.column(matches[0].index).header();
            var left = target.getBoundingClientRect().left - body.getBoundingClientRect().left + body.scrollLeft;
            $(body).stop(true).animate({ scrollLeft: Math.max(0, left - 12) }, 180);
          }
          if (input) $(input).off('input.clColumnJump').on('input.clColumnJump', highlightColumnName);
          table.off('draw.dt.clColumnJump').on('draw.dt.clColumnJump', highlightColumnName);
          table.columns.adjust();
          setTimeout(function() { table.columns.adjust(); highlightColumnName(); }, 100);
          $(window).off('resize.clColumnJump').on('resize.clColumnJump', function() { table.columns.adjust(); });
          $(document).off('shown.bs.tab.clColumnJump').on('shown.bs.tab.clColumnJump', function() {
            setTimeout(function() { table.columns.adjust(); highlightColumnName(); }, 80);
          });",
        column_search_id
      ))
      datatable(
        preview,
        # Use the Explorer's main search field for both value filtering and
        # column-name navigation; hide DT's separate row-only search box.
        # Keep header and body in the same table. The surrounding wrapper
        # provides horizontal scrolling without DataTables' split scroll panes.
        options = list(pageLength = 25, scrollX = FALSE, autoWidth = TRUE, dom = "ltip"),
        callback = jump_to_column,
        rownames = FALSE
      )
    })
    output$download <- downloadHandler(
      filename = function() paste0("criclens-filtered-", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(filtered_data(), file, row.names = FALSE, na = "")
    )
    invisible(filtered_data)
  })
}
