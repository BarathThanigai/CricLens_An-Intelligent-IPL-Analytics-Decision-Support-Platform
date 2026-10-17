# CricLens charts and scorecards -------------------------------------------------
# Drop-in replacements for the venue score chart, worm chart, Manhattan chart
# and the batting/bowling scorecards. Each function takes the summary data
# frame your app already builds. Pass your own column names if they differ.
# Nothing is recalculated here, except the reference average on the venue chart
# (a matches-weighted mean of the values you pass in), which you can turn off.
#
# Needs R/criclens_ui.R (cl_colors, cl_plotly_layout, cl_datatable, cl_tag).

cl_team_colors <- c("#087E8B", "#C85B21")      # first team teal, second orange
cl_team_text <- c("#05535B", "#A84A18")        # darker versions for text labels

# Height (px) for horizontal charts, so 3 venues don't turn into giant blocks
# and 30 venues still get room. Use with renderUI + plotlyOutput(height = ).
cl_plot_height <- function(n_rows, per_row = 38, extra = 120, min = 240) {
  max(min, n_rows * per_row + extra)
}

# Shared ranked bar chart for overview and player leaderboards. This formats
# and displays the analytics already calculated by the R analysis functions.
cl_leaderboard_chart <- function(df, label, value, value_name = "Value", details = NULL,
                                 color = cl_colors[["teal"]], top_n = 10, selected = NULL) {
  if (is.null(df) || !nrow(df)) return(plotly::plotly_empty())

  detail_cols <- if (!is.null(details)) unname(details) else character()
  required <- unique(c(label, value, detail_cols))
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("cl_leaderboard_chart: missing columns: ", paste(missing, collapse = ", "))
  }
  if (length(color) != 1L || is.na(color)) stop("color must be one valid CSS color.")

  d <- data.frame(
    label = as.character(df[[label]]),
    value = suppressWarnings(as.numeric(as.character(df[[value]]))),
    stringsAsFactors = FALSE
  )
  if (!is.null(details)) {
    for (i in seq_along(detail_cols)) d[[paste0("detail", i)]] <- df[[detail_cols[i]]]
  }
  keep <- !is.na(d$label) & nzchar(trimws(d$label)) & is.finite(d$value)
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(plotly::plotly_empty())

  d <- d[order(-d$value, d$label), , drop = FALSE]
  if (length(top_n) == 1L && is.finite(top_n) && top_n > 0) {
    d <- utils::head(d, as.integer(top_n))
  }
  d$rank_label <- sprintf("%d. %s", seq_len(nrow(d)), d$label)

  format_number <- function(x, digits = 0L) {
    if (!is.finite(suppressWarnings(as.numeric(x)))) return("—")
    format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE,
           scientific = FALSE, nsmall = digits)
  }
  details_for_row <- function(i) {
    if (!length(detail_cols)) return("")
    labels <- names(details)
    if (is.null(labels) || any(!nzchar(labels))) labels <- detail_cols
    parts <- vapply(seq_along(detail_cols), function(j) {
      col <- detail_cols[j]
      digits <- if (grepl("matches|wins", labels[j], ignore.case = TRUE)) 0L else 1L
      suffix <- if (grepl("(_pct|percent|percentage)$", col, ignore.case = TRUE)) "%" else ""
      paste0(labels[j], " ", format_number(d[[paste0("detail", j)]][i], digits), suffix)
    }, character(1))
    paste(parts, collapse = " · ")
  }

  value_labels <- vapply(d$value, format_number, character(1), digits = 0L)
  detail_labels <- vapply(seq_len(nrow(d)), details_for_row, character(1))
  hover_labels <- paste0(
    "<b>", htmltools::htmlEscape(d$label), "</b><br>",
    htmltools::htmlEscape(value_name), ": ", value_labels,
    ifelse(nzchar(detail_labels), paste0("<br>", htmltools::htmlEscape(detail_labels)), "")
  )

  darken <- function(hex) {
    rgb <- grDevices::col2rgb(hex) * 0.72
    grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)
  }
  bar_colors <- rep(color, nrow(d))
  if (!is.null(selected) && length(selected)) {
    bar_colors[d$label %in% selected] <- darken(color)
  }

  max_value <- max(d$value, na.rm = TRUE)
  if (!is.finite(max_value) || max_value <= 0) max_value <- 1
  annotations <- lapply(seq_len(nrow(d)), function(i) {
    is_selected <- !is.null(selected) && d$label[i] %in% selected
    name <- htmltools::htmlEscape(d$rank_label[i])
    name_text <- if (is_selected) paste0("<b>", name, "</b>") else name
    detail_text <- if (nzchar(detail_labels[i])) {
      paste0(" <span style='color:#526176;font-weight:400'>", htmltools::htmlEscape(detail_labels[i]), "</span>")
    } else ""
    list(
      list(xref = "paper", yref = "y", x = 0.325, y = d$rank_label[i],
           xanchor = "right", showarrow = FALSE, align = "right", text = name_text,
           font = list(size = 14, color = "#17243A")),
      list(xref = "x", yref = "y", x = d$value[i] + max_value * 0.025, y = d$rank_label[i],
           xanchor = "left", showarrow = FALSE, text = paste0("<b>", value_labels[i], "</b>", detail_text),
           font = list(size = 13, color = "#17243A"))
    )
  })
  annotations <- unlist(annotations, recursive = FALSE)

  p <- plotly::plot_ly(
    x = d$value,
    y = factor(d$rank_label, levels = rev(d$rank_label)),
    type = "bar", orientation = "h",
    marker = list(color = bar_colors, line = list(width = 0)),
    text = hover_labels, hoverinfo = "text", textposition = "none",
    cliponaxis = FALSE
  )
  p |>
    cl_plotly_layout(x_title = NULL, y_title = NULL, legend = FALSE,
                     x_range = c(0, max_value * 2.3)) |>
    plotly::layout(
      title = NULL,
      xaxis = list(domain = c(0.34, 1), rangemode = "tozero", zeroline = TRUE,
                   zerolinecolor = "#C3CFDD", gridcolor = "#E8EDF3"),
      yaxis = list(showticklabels = FALSE, showgrid = FALSE, ticks = "", automargin = TRUE),
      margin = list(l = 4, r = 18, t = 12, b = 42),
      bargap = 0.32,
      annotations = annotations,
      hovermode = "closest"
    )
}

# Light shading for powerplay (overs 1-6) and death overs (16-20).
# bars = TRUE for bar charts centred on whole overs, FALSE for line charts.
cl_phase_layout <- function(overs = 20, bars = FALSE) {
  o <- if (bars) 0.5 else 0
  pp <- c(o, 6 + o)
  death <- c(15 + o, overs + o)
  rect <- function(x) {
    list(type = "rect", xref = "x", yref = "paper", x0 = x[1], x1 = x[2], y0 = 0, y1 = 1,
         fillcolor = "#F4F7FB", line = list(width = 0), layer = "below")
  }
  label <- function(x, text) {
    list(x = mean(x), y = 1, xref = "x", yref = "paper", yanchor = "bottom", showarrow = FALSE,
         text = text, font = list(size = 12, color = "#526176"))
  }
  list(
    shapes = list(rect(pp), rect(death)),
    annotations = list(label(pp, "POWERPLAY"), label(death, "DEATH OVERS"))
  )
}

# 1. Average 1st-innings score by venue ------------------------------------------
# A dot plot instead of bars: the averages sit close together (about 150-180),
# and bars that start at 0 make them all look the same. Highest venue on top.
#   selected:    venue(s) to highlight, e.g. input$venue
#   min_matches: venues with fewer matches get a hollow marker (small sample)
cl_venue_score_chart <- function(df, venue = "venue", score = "avg_first_innings", matches = "matches",
                                 selected = NULL, min_matches = 15, show_average = TRUE,
                                 score_label = "Average 1st-innings score") {
  d <- data.frame(
    venue = as.character(df[[venue]]),
    score = as.numeric(df[[score]]),
    matches = as.numeric(df[[matches]])
  )
  d <- d[order(d$score), , drop = FALSE]
  levels_y <- d$venue
  d$y <- factor(d$venue, levels = levels_y)
  d$group <- ifelse(d$venue %in% selected, "Selected venue",
                    ifelse(d$matches < min_matches, paste("Fewer than", min_matches, "matches"),
                           paste0(min_matches, "+ matches")))

  rng <- range(d$score)
  pad <- max(4, diff(rng) * 0.12)
  x_range <- c(floor(rng[1] - pad), ceiling(rng[2] + pad * 1.8))

  styles <- list(
    "Selected venue" = list(symbol = "diamond", color = cl_colors[["orange"]], size = 17),
    list(symbol = "circle-open", color = cl_colors[["teal"]], size = 14),
    list(symbol = "circle", color = cl_colors[["teal"]], size = 14)
  )
  names(styles)[2:3] <- c(paste("Fewer than", min_matches, "matches"), paste0(min_matches, "+ matches"))

  d$x0 <- x_range[1]
  p <- plotly::plot_ly(height = cl_plot_height(nrow(d))) |>
    plotly::add_segments(
      data = d, x = ~x0, xend = ~score, y = ~y, yend = ~y,
      line = list(color = "#DCE4EE", width = 3), hoverinfo = "skip", showlegend = FALSE
    )
  for (g in intersect(names(styles), unique(d$group))) {
    s <- styles[[g]]
    dg <- d[d$group == g, , drop = FALSE]
    p <- p |> plotly::add_markers(
      data = dg, x = ~score, y = ~y, name = g, customdata = ~matches,
      marker = list(symbol = s$symbol, size = s$size, color = s$color, line = list(color = s$color, width = 3)),
      hovertemplate = paste0("<b>%{y}</b><br>", score_label, " <b>%{x:.1f}</b><br>Matches %{customdata}<extra></extra>")
    )
  }
  p <- p |> plotly::add_text(
    data = d, x = ~score, y = ~y, text = ~sprintf("   %.1f", score), textposition = "middle right",
    textfont = list(size = 14, color = "#17243A"), hoverinfo = "skip", showlegend = FALSE
  )

  shapes <- list()
  annotations <- list()
  if (show_average && nrow(d) > 1) {
    avg <- stats::weighted.mean(d$score, d$matches)
    shapes <- list(list(type = "line", xref = "x", yref = "paper", x0 = avg, x1 = avg, y0 = 0, y1 = 1,
                        line = list(color = "#526176", width = 1.5, dash = "dot")))
    annotations <- list(list(x = avg, y = 1, xref = "x", yref = "paper", yanchor = "bottom", showarrow = FALSE,
                             text = sprintf("Average %.1f", avg), font = list(size = 13, color = "#526176")))
  }

  p |>
    cl_plotly_layout(x_title = score_label, y_title = NULL, legend = TRUE, x_range = x_range) |>
    plotly::layout(
      yaxis = list(categoryorder = "array", categoryarray = levels_y, showgrid = FALSE, ticksuffix = "  "),
      legend = list(orientation = "h", x = 0, y = 1.12, yanchor = "bottom"),
      margin = list(t = 64),
      shapes = shapes, annotations = annotations, hovermode = "closest"
    )
}

# 2. Worm chart (cumulative runs by over) -------------------------------------------
#   df: one row per team per over, with cumulative runs
#   wickets: optional column with wickets fallen in that over (shown as markers)
cl_worm_chart <- function(df, over = "over", runs = "cumulative_runs", team = "team",
                          wickets = NULL, overs = 20, phases = TRUE) {
  d <- data.frame(over = as.numeric(df[[over]]), runs = as.numeric(df[[runs]]), team = as.character(df[[team]]))
  if (!is.null(wickets)) d$wkts <- as.numeric(df[[wickets]])
  teams <- unique(d$team)
  p <- plotly::plot_ly()
  end_labels <- list()
  for (i in seq_along(teams)) {
    k <- (i - 1) %% 2 + 1
    t <- d[d$team == teams[i], , drop = FALSE]
    t <- t[order(t$over), , drop = FALSE]
    # start each line at 0 runs after 0 overs
    p <- p |> plotly::add_lines(
      x = c(0, t$over), y = c(0, t$runs), name = teams[i],
      line = list(color = cl_team_colors[k], width = 3.5),
      hovertemplate = paste0(teams[i], ": <b>%{y}</b><extra></extra>")
    )
    if (!is.null(wickets) && any(t$wkts > 0, na.rm = TRUE)) {
      w <- t[!is.na(t$wkts) & t$wkts > 0, , drop = FALSE]
      p <- p |> plotly::add_markers(
        x = w$over, y = w$runs, name = paste(teams[i], "wickets"), showlegend = FALSE,
        customdata = w$wkts,
        marker = list(symbol = "circle", size = 11, color = "#FFFFFF", line = list(color = cl_team_colors[k], width = 3)),
        hovertemplate = paste0(teams[i], ": %{customdata} wicket(s) in this over<extra></extra>")
      )
    }
    last <- t[nrow(t), ]
    end_labels[[i]] <- list(
      x = last$over, y = last$runs, xref = "x", yref = "y", xanchor = "left", xshift = 10, showarrow = FALSE,
      text = sprintf("<b>%s</b>", last$runs), font = list(size = 15, color = cl_team_text[k])
    )
  }
  ph <- if (phases) cl_phase_layout(overs, bars = FALSE) else list(shapes = list(), annotations = list())
  p |>
    cl_plotly_layout(x_title = "Over", y_title = "Cumulative runs", legend = TRUE, x_range = c(0, overs + 1.6)) |>
    plotly::layout(
      xaxis = list(dtick = 2, showgrid = FALSE),
      yaxis = list(rangemode = "tozero"),
      hovermode = "x unified",
      margin = list(t = 64),
      legend = list(orientation = "h", x = 0, y = 1.12, yanchor = "bottom"),
      shapes = ph$shapes,
      annotations = c(ph$annotations, end_labels)
    )
}

# 3. Manhattan chart (runs per over) ------------------------------------------------
#   df: one row per team per over
#   wickets: optional column; overs with wickets get a "1W" / "2W" label on the bar
cl_manhattan_chart <- function(df, over = "over", runs = "runs", team = "team",
                               wickets = NULL, overs = 20, phases = TRUE) {
  d <- data.frame(over = as.numeric(df[[over]]), runs = as.numeric(df[[runs]]), team = as.character(df[[team]]))
  d$wkts <- if (!is.null(wickets)) as.numeric(df[[wickets]]) else 0
  teams <- unique(d$team)
  p <- plotly::plot_ly()
  for (i in seq_along(teams)) {
    k <- (i - 1) %% 2 + 1
    t <- d[d$team == teams[i], , drop = FALSE]
    p <- p |> plotly::add_bars(
      x = t$over, y = t$runs, name = teams[i], customdata = t$wkts,
      marker = list(color = cl_team_colors[k], line = list(width = 0)),
      text = ifelse(!is.na(t$wkts) & t$wkts > 0, paste0(t$wkts, "W"), ""),
      textposition = "outside", cliponaxis = FALSE,
      textfont = list(size = 12, color = cl_team_text[k]),
      hovertemplate = if (is.null(wickets)) {
        paste0(teams[i], ": <b>%{y}</b> runs<extra></extra>")
      } else {
        paste0(teams[i], ": <b>%{y}</b> runs, %{customdata} wkt<extra></extra>")
      }
    )
  }
  ph <- if (phases) cl_phase_layout(overs, bars = TRUE) else list(shapes = list(), annotations = list())
  p |>
    cl_plotly_layout(x_title = "Over", y_title = "Runs in the over", legend = TRUE, x_range = c(0.4, overs + 0.6)) |>
    plotly::layout(
      barmode = "group", bargap = 0.3, bargroupgap = 0.06,
      xaxis = list(dtick = 1, showgrid = FALSE),
      yaxis = list(rangemode = "tozero"),
      hovermode = "x unified",
      margin = list(t = 64),
      legend = list(orientation = "h", x = 0, y = 1.12, yanchor = "bottom"),
      shapes = ph$shapes,
      annotations = ph$annotations
    )
}

# 4. Scorecards -------------------------------------------------------------------
# Rows grouped under a team heading instead of repeating the team on every row.
# Readable headers via the display copy only; your data frame keeps its names.
cl_batting_scorecard <- function(df, team = "batting_team", batter = "batter", runs = "runs",
                                 balls = "balls", strike_rate = "strike_rate", dismissal = "dismissal") {
  not_out <- tolower(trimws(df[[dismissal]])) == "not out"
  disp <- data.frame(
    Team = df[[team]],
    Batter = sprintf("<b>%s</b>", htmltools::htmlEscape(df[[batter]])),
    Dismissal = ifelse(not_out,
                       as.character(cl_tag("not out", tone = "teal", small = TRUE)),
                       sprintf('<span class="cl-muted-cell">%s</span>', htmltools::htmlEscape(df[[dismissal]]))),
    Runs = df[[runs]],
    Balls = df[[balls]],
    `Strike rate` = round(as.numeric(df[[strike_rate]]), 1),
    check.names = FALSE
  )
  cl_datatable(
    disp, pin_first = FALSE, escape = FALSE, dom = "t", page_length = 50, hidden = "Team",
    more_extensions = "RowGroup", more_options = list(rowGroup = list(dataSrc = 0), ordering = FALSE)
  ) |>
    DT::formatStyle("Runs", fontWeight = DT::styleInterval(49.5, c("400", "800")))
}

cl_bowling_scorecard <- function(df, team = "bowling_team", bowler = "bowler", overs = "overs",
                                 runs_given = "runs_given", wickets = "wickets", economy = "economy") {
  disp <- data.frame(
    Team = df[[team]],
    Bowler = sprintf("<b>%s</b>", htmltools::htmlEscape(df[[bowler]])),
    Overs = sprintf("%.1f", as.numeric(df[[overs]])),
    Runs = df[[runs_given]],
    Wickets = df[[wickets]],
    Economy = round(as.numeric(df[[economy]]), 2),
    check.names = FALSE
  )
  best <- max(disp$Wickets, na.rm = TRUE)
  cl_datatable(
    disp, pin_first = FALSE, escape = FALSE, dom = "t", page_length = 50, hidden = "Team", right = "Overs",
    more_extensions = "RowGroup", more_options = list(rowGroup = list(dataSrc = 0), ordering = FALSE)
  ) |>
    DT::formatStyle("Wickets", fontWeight = DT::styleEqual(best, "800"),
                    color = DT::styleEqual(best, "#05535B"))
}
