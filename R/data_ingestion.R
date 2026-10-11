# IPL schema normalization. All calculations use this canonical schema.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

find_col <- function(nms, aliases, required = FALSE) {
  key <- tolower(gsub("[^a-z0-9]", "", nms))
  wanted <- tolower(gsub("[^a-z0-9]", "", aliases))
  hit <- match(wanted, key, nomatch = 0L); hit <- hit[hit > 0L]
  if (length(hit)) nms[hit[1L]] else if (required) stop("Required field not found. Recognized names: ", paste(aliases, collapse=", ")) else NA_character_
}

read_ipl_csv <- function(path) {
  raw <- if (requireNamespace("readr", quietly=TRUE)) {
    readr::read_csv(path, show_col_types=FALSE, progress=FALSE, name_repair="unique")
  } else {
    utils::read.csv(path, stringsAsFactors=FALSE, check.names=FALSE, na.strings=c("","NA"))
  }
  n <- names(raw)
  map <- list(match_id=c("match_id","id","game_id"), season=c("season","year"), innings=c("innings","inning"),
    over=c("over","overs"), ball=c("ball","delivery"), batter=c("batter","batsman","striker"),
    bowler=c("bowler"), batting_team=c("batting_team","team_batting"), bowling_team=c("bowling_team","team_bowling"),
    runs_batter=c("runs_batter","batter_runs","batsman_runs","runs_off_bat"), balls_faced=c("balls_faced","batter_balls"), runs_total=c("runs_total","total_runs"), runs_bowler=c("runs_bowler","bowler_runs"),
    runs_extras=c("runs_extras","extras"), extras_type=c("extra_type","extras_type","extra"), runs_target=c("runs_target","target","target_score"), valid_ball=c("valid_ball","legal_ball"), wicket_kind=c("wicket_kind","dismissal_kind"),
    player_out=c("player_out","batter_out"), venue=c("venue","ground","stadium"), date=c("date","start_date"),
    toss_winner=c("toss_winner"), toss_decision=c("toss_decision"), match_winner=c("match_winner","winner","match_won_by"),
    result_type=c("result_type","result"), player_of_match=c("player_of_match","pom"))
  out <- as.data.frame(lapply(map, function(a) { col <- find_col(n,a); if (is.na(col)) rep(NA, nrow(raw)) else raw[[col]] }), stringsAsFactors=FALSE)
  if (all(is.na(out$match_id))) stop("Could not identify a match ID column.")
  if (all(is.na(out$innings))) stop("Could not identify innings (aliases: innings, inning).")
  if (all(is.na(out$batting_team))) stop("Could not identify batting team (aliases: batting_team, team_batting).")
  if (all(is.na(out$batter))) stop("Could not identify batter (aliases: batter, batsman, striker).")
  if (all(is.na(out$runs_total)) && all(is.na(out$runs_batter))) stop("Could not identify runs (runs_total/total_runs or runs_batter/batter_runs/batsman_runs).")
  numeric_cols <- c("innings","over","ball","runs_batter","balls_faced","runs_total","runs_extras","runs_bowler","runs_target","valid_ball")
  for (nm in numeric_cols) out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  out$match_id <- as.character(out$match_id)
  out$date <- as.Date(out$date, tryFormats=c("%Y-%m-%d","%d/%m/%Y","%m/%d/%Y"))
  if (any(out$over == 0,na.rm=TRUE)) out$over <- out$over + 1 # source is 0-based; canonical phase buckets are 1-based
  season_text <- as.character(out$season)
  season_num <- suppressWarnings(as.numeric(season_text))
  slash_season <- grepl("^[0-9]{4}/[0-9]{2}$",season_text)
  season_num[slash_season] <- as.numeric(substr(season_text[slash_season],1,4))+1
  season_num[is.na(season_num) & !is.na(out$date)] <- as.integer(format(out$date[is.na(season_num) & !is.na(out$date)], "%Y"))
  out$season <- season_num
  if (requireNamespace("tidyr",quietly=TRUE)) out <- tidyr::replace_na(out,list(runs_batter=0,valid_ball=1))
  out$runs_batter[is.na(out$runs_batter)] <- 0
  out$runs_extras[is.na(out$runs_extras)] <- ifelse(!is.na(out$runs_total), out$runs_total-out$runs_batter, 0)
  out$runs_total[is.na(out$runs_total)] <- out$runs_batter[is.na(out$runs_total)] + out$runs_extras[is.na(out$runs_total)]
  out$valid_ball[is.na(out$valid_ball)] <- 1
  missing_balls_faced<-is.na(out$balls_faced); wide<-!is.na(out$extras_type)&grepl("wide",tolower(as.character(out$extras_type)),fixed=TRUE)
  out$balls_faced[missing_balls_faced] <- as.integer(!wide[missing_balls_faced])
  miss_bowler_runs <- is.na(out$runs_bowler)
  uncharged <- !is.na(out$extras_type) & tolower(out$extras_type) %in% c("byes","bye","legbyes","legbye","penalty")
  out$runs_bowler[miss_bowler_runs] <- pmax(0,out$runs_total[miss_bowler_runs]-ifelse(uncharged[miss_bowler_runs],out$runs_extras[miss_bowler_runs],0))
  out$phase <- ifelse(out$over <= 6, "Powerplay", ifelse(out$over <= 15, "Middle", "Death"))
  out$team <- trimws(as.character(out$batting_team)); out$opposition <- trimws(as.character(out$bowling_team))
  out$player <- trimws(as.character(out$batter)); out$bowler <- trimws(as.character(out$bowler))
  out$venue <- trimws(as.character(out$venue))
  dismissal_kind<-tolower(trimws(as.character(out$wicket_kind)))
  out$dismissal <- !is.na(out$wicket_kind) & nzchar(trimws(out$wicket_kind)) & dismissal_kind!="retired hurt"
  out$wicket <- out$dismissal
  out$match_winner <- trimws(as.character(out$match_winner))
  attr(out,"source_columns") <- names(raw)
  attr(out,"duplicate_rows") <- sum(duplicated(raw))
  attr(out,"missing_match_id") <- sum(is.na(out$match_id))
  attr(out,"missing_core_rows") <- sum(is.na(out$match_id)|is.na(out$innings)|is.na(out$team)|is.na(out$player))
  out
}

load_criclens_data <- function(raw_dir="data/raw", processed_dir="data/processed") {
  cache <- file.path(processed_dir,"deliveries.rds")
  csv <- list.files(raw_dir, pattern="\\.csv$", full.names=TRUE, ignore.case=TRUE)
  if (!length(csv)) return(list(data=NULL, message="No CSV dataset found. Add a ball-by-ball IPL CSV to data/raw."))
  src <- csv[1L]
  if (file.exists(cache) && file.info(cache)$mtime >= file.info(src)$mtime) {
    d <- tryCatch(readRDS(cache), error=function(e) NULL)
    if (!is.null(d)) return(list(data=d, message=paste("Loaded processed cache from", basename(src))))
  }
  d <- read_ipl_csv(src)
  dir.create(processed_dir, recursive=TRUE, showWarnings=FALSE)
  saveRDS(d, cache)
  list(data=d, message=paste("Processed", nrow(d), "deliveries from", basename(src)))
}
