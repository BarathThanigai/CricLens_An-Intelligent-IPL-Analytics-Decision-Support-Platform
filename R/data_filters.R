# Shared filters for all analysis pages. Team means a match involving that side.
filter_global_data <- function(df, season = "All", team = "All", venue = "All") {
  out <- df
  if (!is.null(season) && length(season) && season != "All" && "season_year" %in% names(out)) {
    out <- out[!is.na(out$season_year) & out$season_year == as.numeric(season), , drop = FALSE]
  }
  if (!is.null(team) && length(team) && team != "All") {
    keep <- (!is.na(out$batting_team) & out$batting_team == team) |
      (!is.na(out$bowling_team) & out$bowling_team == team)
    keep[is.na(keep)] <- FALSE
    out <- out[keep, , drop = FALSE]
  }
  if (!is.null(venue) && length(venue) && venue != "All" && "venue" %in% names(out)) {
    out <- out[!is.na(out$venue) & out$venue == venue, , drop = FALSE]
  }
  out
}
