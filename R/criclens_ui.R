# CricLens visual layer for Shiny ---------------------------------------------
# Theme and small UI helpers. Presentation only: wrap your existing outputs
# with these. Server logic, data, input IDs and output IDs stay as they are.
#
# Needs: shiny (>= 1.8.1 for useBusyIndicators), bslib (>= 0.9 for
# navbar_options), plotly, DT. Shiny loads files in R/ automatically.

library(shiny)
library(bslib)

cl_colors <- c(teal = "#087E8B", orange = "#C85B21", navy = "#2F4466", muted = "#526176", gold = "#C89B24")

# Small inline Lucide-style SVGs keep the Shiny UI crisp without a React or
# JavaScript icon dependency. Names are intentionally shared across screens.
cl_icon <- function(name, size = 18, class = "cl-ui-icon", title = NULL) {
  paths <- switch(name,
    overview = '<path d="M3 3v18h18"/><path d="m19 9-5 5-4-4-5 5"/>',
    players = '<path d="M16 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"/><circle cx="10" cy="7" r="4"/><path d="M20 21v-2a4 4 0 0 0-3-3.87M16 3.13a4 4 0 0 1 0 7.75"/>',
    teams = '<path d="M8 21h8m-4-4v4"/><path d="M7 4h10v5a5 5 0 0 1-10 0V4Z"/><path d="M7 6H4v2a4 4 0 0 0 4 4m9-6h3v2a4 4 0 0 1-4 4"/>',
    matches = '<rect x="3" y="5" width="18" height="16" rx="2"/><path d="M16 3v4M8 3v4M3 11h18"/>',
    venues = '<path d="M20 10c0 5-8 11-8 11S4 15 4 10a8 8 0 1 1 16 0Z"/><circle cx="12" cy="10" r="2.5"/>',
    h2h = '<circle cx="6" cy="12" r="3"/><circle cx="18" cy="12" r="3"/><path d="M9 12h6m-3-3 3 3-3 3"/>',
    predictions = '<path d="m12 3 1.9 5.8L20 11l-6.1 2.2L12 19l-1.9-5.8L4 11l6.1-2.2L12 3Z"/><path d="m19 14 1 2 2 1-2 1-1 2-1-2-2-1 2-1 1-2Z"/>',
    explorer = '<ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M3 5v14c0 1.7 4 3 9 3s9-1.3 9-3V5M3 12c0 1.7 4 3 9 3s9-1.3 9-3"/>',
    ai = '<path d="M12 8V4H8"/><rect x="4" y="8" width="16" height="12" rx="2"/><path d="M2 14h2m16 0h2M9 13h.01M15 13h.01M9 17h6"/>',
    search = '<circle cx="11" cy="11" r="7"/><path d="m20 20-4-4"/>',
    filter = '<path d="M4 21v-7m0-4V3m8 18v-9m0-4V3m8 18v-5m0-4V3M2 14h4m4-6h4m4 8h4"/>',
    bat = '<path d="M4 20 17 7m-2-2 4 4M3 21l3-1-2-2-1 3Z"/><path d="m14 8 2 2"/>',
    bowl = '<circle cx="12" cy="12" r="9"/><circle cx="12" cy="12" r="5"/><circle cx="12" cy="12" r="1"/>',
    scale = '<path d="m16 16 3-8 3 8a5 5 0 0 1-6 0ZM2 16l3-8 3 8a5 5 0 0 1-6 0ZM12 3v18m-4 0h8M5 8h14"/>',
    chart = '<path d="M3 3v18h18"/><rect x="7" y="12" width="3" height="6" rx="1"/><rect x="13" y="8" width="3" height="10" rx="1"/><rect x="19" y="5" width="3" height="13" rx="1"/>',
    trophy = '<path d="M8 21h8m-4-4v4"/><path d="M7 4h10v5a5 5 0 0 1-10 0V4Zm0 2H4v2a4 4 0 0 0 4 4m9-6h3v2a4 4 0 0 1-4 4"/>',
    stop("Unknown CricLens icon: ", name)
  )
  tags$svg(
    class = class, width = size, height = size, viewBox = "0 0 24 24",
    fill = "none", stroke = "currentColor", `stroke-width` = "1.8",
    `stroke-linecap` = "round", `stroke-linejoin` = "round",
    `aria-hidden` = if (is.null(title)) "true" else NULL,
    role = if (!is.null(title)) "img" else NULL,
    tags$title(title), HTML(paths)
  )
}

# Theme --------------------------------------------------------------------------
criclens_theme <- function() {
  bs_theme(
    version      = 5,
    bg           = "#F4F7FB",
    fg           = "#17243A",
    primary      = "#087E8B",
    secondary    = "#526176",
    success      = "#1B6B45",
    info         = "#087E8B",
    warning      = "#7A5200",
    danger       = "#A8231A",
    base_font    = font_google("DM Sans", wght = c(400, 500, 600, 700)),
    heading_font = font_google("Manrope", wght = c(600, 700, 800)),
    font_scale   = 0.9375, # 15px body text
    "border-color"         = "#DCE4EE",
    "border-radius"        = "0.625rem",
    "card-border-radius"   = "0.875rem",
    "input-border-color"   = "#C3CFDD",
    "link-color"           = "#066A75",
    "headings-font-weight" = 800
  )
}

# Loads www/criclens.css and www/criclens.js. Put it in page_navbar(header = ).
cl_head <- function() {
  tags$head(
    tags$link(rel = "stylesheet", href = "criclens.css"),
    tags$script(src = "criclens.js")
  )
}

cl_brand <- function(name = "CricLens", sub = "IPL Intelligence") {
  tags$span(
    class = "cl-brand",
    tags$img(src = "criclens-mark.svg", width = 34, height = 34, alt = ""),
    name,
    tags$span(class = "cl-brand-sub", sub)
  )
}

# Page layout ------------------------------------------------------------------
# Centered content area (max 1360px). Wrap each nav_panel's content in this.
cl_page <- function(..., class = NULL) {
  div(class = paste(c("cl-container", class), collapse = " "), ...)
}

cl_page_header <- function(eyebrow, title, description, actions = NULL) {
  div(
    class = "cl-page-header",
    div(
      div(class = "cl-eyebrow", eyebrow),
      h1(class = "cl-page-title", title),
      p(class = "cl-page-lede", description)
    ),
    if (!is.null(actions)) div(class = "cl-page-actions", actions)
  )
}

# Card with an icon badge, title and subtitle. tone: teal, orange, navy or gold.
cl_card <- function(title, ..., subtitle = NULL, icon = NULL, tone = "teal",
                    full_screen = FALSE, class = NULL) {
  card(
    full_screen = full_screen,
    class = class,
    card_header(
      if (!is.null(icon)) span(class = paste("cl-icon-badge", tone), icon),
      div(class = "cl-card-title", title, if (!is.null(subtitle)) span(class = "cl-card-sub", subtitle))
    ),
    card_body(..., fillable = FALSE)
  )
}

# KPI card. value can be a literal or textOutput("...").
cl_kpi <- function(title, value, note, tone = "teal", icon = NULL) {
  value_box(title = title, value = value, p(note), showcase = icon, class = paste0("cl-", tone))
}

cl_filter_group <- function(title, hint, ..., icon = NULL) {
  tags$fieldset(
    class = "cl-filter-group",
    tags$legend(class = "visually-hidden", title),
    div(
      class = "cl-filter-group-head",
      if (!is.null(icon)) span(class = "cl-icon-badge", icon),
      div(div(class = "cl-filter-group-title", title), div(class = "cl-filter-group-hint", hint))
    ),
    ...
  )
}

# Tags, empty states -------------------------------------------------------------
# tone: NULL (neutral), "teal", "orange" or "success".
cl_tag <- function(..., tone = NULL, small = FALSE) {
  span(class = paste(c("cl-tag", tone, if (small) "cl-tag-sm"), collapse = " "), ...)
}

# A × button that sends `value` to input[[input_id]] when clicked.
cl_remove_button <- function(input_id, value, label) {
  tags$button(
    type = "button",
    class = "cl-tag-remove",
    `aria-label` = label,
    onclick = sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'})",
      input_id, jsonlite::toJSON(value, auto_unbox = TRUE)
    ),
    HTML("&times;")
  )
}

cl_filter_tag <- function(field, value, remove_input, remove_value) {
  cl_tag(
    tone = "teal",
    span(class = "cl-tag-field", paste0(field, ":")),
    value,
    cl_remove_button(remove_input, remove_value, paste("Remove filter", field, value))
  )
}

cl_empty <- function(title, body = NULL, icon = shiny::icon("inbox")) {
  div(
    class = "cl-empty",
    span(class = "cl-empty-icon", icon),
    strong(title),
    if (!is.null(body)) span(body)
  )
}

# Shared Season / Team / Venue tags ------------------------------------------------
# Reads the existing filter inputs. Each × resets that one input to its default.
# Pair with uiOutput("active_filter_tags", class = "cl-active-filters").
cl_active_filters_server <- function(input, output, session,
                                     defaults = list(season = "All", team = "All teams", venue = "All venues"),
                                     labels = c(season = "Season", team = "Team", venue = "Venue"),
                                     output_id = "active_filter_tags") {
  output[[output_id]] <- renderUI({
    active <- Filter(function(id) !is.null(input[[id]]) && !identical(input[[id]], defaults[[id]]), names(defaults))
    tagList(
      span(class = "cl-active-label", "Active filters"),
      if (!length(active)) cl_tag("None \u00b7 all seasons, teams and venues"),
      lapply(active, function(id) cl_filter_tag(labels[[id]], input[[id]], "cl_clear_filter", id)),
      if (length(active)) actionLink("cl_reset_filters", "Reset all", class = "cl-reset-link")
    )
  })
  observeEvent(input$cl_clear_filter, {
    id <- input$cl_clear_filter
    updateSelectInput(session, id, selected = defaults[[id]])
  })
  observeEvent(input$cl_reset_filters, {
    for (id in names(defaults)) updateSelectInput(session, id, selected = defaults[[id]])
  })
}

# Plotly ---------------------------------------------------------------------------
cl_plotly_layout <- function(p, x_title = NULL, y_title = NULL, legend = TRUE, x_range = NULL) {
  xaxis <- list(
    title = list(text = x_title, font = list(size = 14, color = "#526176"), standoff = 12),
    tickfont = list(size = 13, color = "#526176"),
    gridcolor = "#E8EDF3", zeroline = FALSE, automargin = TRUE, separatethousands = TRUE
  )
  if (!is.null(x_range)) xaxis$range <- x_range
  p |>
    plotly::layout(
      font          = list(family = "DM Sans, sans-serif", size = 14, color = "#17243A"),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      margin        = list(l = 8, r = 24, t = 8, b = 8),
      bargap        = 0.32,
      xaxis         = xaxis,
      yaxis = list(
        title = list(text = y_title, font = list(size = 14, color = "#526176"), standoff = 12),
        tickfont = list(size = 14, color = "#17243A"),
        automargin = TRUE
      ),
      showlegend = legend,
      legend = list(orientation = "h", x = 0, y = 1.08, font = list(size = 14)),
      hoverlabel = list(
        bgcolor = "#FFFFFF", bordercolor = "#DCE4EE", align = "left",
        font = list(family = "DM Sans, sans-serif", size = 14, color = "#17243A")
      )
    ) |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")
    )
}

# Horizontal bar chart, largest value at the top. Value labels go outside the
# bars; extra details belong in hovertemplate, not tiny text inside the bars.
cl_hbar <- function(labels, values, color = cl_colors[["teal"]], x_title = NULL, y_title = NULL,
                    text = prettyNum(values, big.mark = ","),
                    hovertemplate = "<b>%{y}</b><br>%{x:,}<extra></extra>", ...) {
  ord <- order(values)
  plotly::plot_ly(
    x = values[ord], y = factor(labels[ord], levels = labels[ord]),
    type = "bar", orientation = "h",
    marker = list(color = if (length(color) > 1) color[ord] else color),
    text = text[ord], textposition = "outside", cliponaxis = FALSE,
    textfont = list(size = 14, color = "#17243A"),
    hovertemplate = hovertemplate, ...
  ) |>
    cl_plotly_layout(x_title, y_title, legend = FALSE, x_range = c(0, max(values) * 1.22))
}

# DT -------------------------------------------------------------------------------
# Shared table defaults: contained horizontal scroll, pinned first column,
# optional column chooser, right-aligned numbers. Sorting, search, paging unchanged.
#   hidden: columns hidden at first (still available from the Columns button)
#   right:  extra columns to right-align, e.g. HTML-formatted numbers
#   more_options / more_extensions: extra DT options and extensions, e.g. RowGroup
cl_datatable <- function(df, pin_first = TRUE, colvis = FALSE, page_length = 10,
                         hidden = NULL, right = NULL, dom = NULL,
                         more_options = list(), more_extensions = NULL, ...) {
  right_cols <- union(names(df)[vapply(df, is.numeric, logical(1))], right)
  col_defs <- list(list(className = "dt-right", targets = match(right_cols, names(df)) - 1))
  if (length(hidden)) col_defs <- c(col_defs, list(list(visible = FALSE, targets = match(hidden, names(df)) - 1)))
  opts <- list(
    scrollX    = TRUE,
    autoWidth  = FALSE,
    pageLength = page_length,
    lengthMenu = c(10, 25, 50),
    dom        = if (!is.null(dom)) dom else if (colvis) "Blfrtip" else "lfrtip",
    columnDefs = col_defs,
    language   = list(search = "Search table:", zeroRecords = "No matching records")
  )
  if (colvis) opts$buttons <- list(list(extend = "colvis", text = "Columns", columns = ":not(:first-child)"))
  if (pin_first) opts$fixedColumns <- list(leftColumns = 1)
  opts <- utils::modifyList(opts, more_options)
  ext <- c(if (pin_first) "FixedColumns", if (colvis) "Buttons", more_extensions)
  args <- list(data = df, rownames = FALSE, class = "stripe hover", options = opts, ...)
  if (length(ext)) args$extensions <- ext
  do.call(DT::datatable, args)
}

# Inline percentage bar for DT cells (use escape = FALSE). The number is always shown.
cl_pct_bar <- function(x, max = 100) {
  sprintf(
    '<span class="cl-pct"><span class="cl-pct-track" aria-hidden="true"><i style="width:%.1f%%"></i></span><span class="cl-pct-val">%.1f</span></span>',
    x / max * 100, x
  )
}
