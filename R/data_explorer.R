# Apply Data Explorer filters to the full dataset before limiting the table preview.
apply_explorer_filters <- function(df, global_search = "", category_filters = list(),
                                   text_filters = list(), numeric_filters = list(),
                                   date_filter = NULL) {
  out <- df
  if (length(global_search) && nzchar(trimws(global_search))) {
    term <- tolower(trimws(global_search))
    keep <- Reduce(`|`, lapply(out, function(col) grepl(term, tolower(as.character(col)), fixed = TRUE)))
    keep[is.na(keep)] <- FALSE
    out <- out[keep, , drop = FALSE]
  }
  for (f in category_filters) {
    if (is.list(f) && length(f$column) && nzchar(f$column) && f$column %in% names(out) && length(f$values)) {
      out <- out[!is.na(out[[f$column]]) & as.character(out[[f$column]]) %in% as.character(f$values), , drop = FALSE]
    }
  }
  for (f in text_filters) {
    if (is.list(f) && length(f$column) && nzchar(f$column) && f$column %in% names(out) && length(f$query) && nzchar(trimws(f$query))) {
      keep <- grepl(tolower(trimws(f$query)), tolower(as.character(out[[f$column]])), fixed = TRUE)
      keep[is.na(keep)] <- FALSE
      out <- out[keep, , drop = FALSE]
    }
  }
  for (f in numeric_filters) {
    if (is.list(f) && length(f$column) && nzchar(f$column) && f$column %in% names(out) && is.numeric(out[[f$column]])) {
      x <- out[[f$column]]
      keep <- !is.na(x)
      if (length(f$min) && is.finite(f$min)) keep <- keep & x >= f$min
      if (length(f$max) && is.finite(f$max)) keep <- keep & x <= f$max
      out <- out[keep, , drop = FALSE]
    }
  }
  if (is.list(date_filter) && length(date_filter$column) && date_filter$column %in% names(out) &&
      inherits(out[[date_filter$column]], "Date") && length(date_filter$range) == 2 && !anyNA(date_filter$range)) {
    out <- out[!is.na(out[[date_filter$column]]) & out[[date_filter$column]] >= as.Date(date_filter$range[1]) &
                 out[[date_filter$column]] <= as.Date(date_filter$range[2]), , drop = FALSE]
  }
  out
}
