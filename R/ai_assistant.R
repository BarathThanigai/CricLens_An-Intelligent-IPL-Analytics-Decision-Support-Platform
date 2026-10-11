# The router only emits allowlisted operations. No model-generated code is evaluated.
route_question <- function(question) {
  q <- tolower(question)
  if(grepl("death|powerplay|middle over|phase",q)) return(list(operation="phase_leaders",phase=if(grepl("death",q)) "Death" else if(grepl("powerplay",q)) "Powerplay" else "Middle"))
  if(grepl("venue|stadium|ground",q)) return(list(operation="venue_rankings"))
  if(grepl("team|chase|toss|win",q)) return(list(operation="team_records"))
  if(grepl("player|batter|runs|bowler|wicket",q)) return(list(operation="player_rankings"))
  list(operation="help")
}
validate_ai_request <- function(request) {
  allowed <- c("phase_leaders","venue_rankings","team_records","player_rankings","help")
  is.list(request)&&length(request$operation)==1&&request$operation%in%allowed&&
    (is.null(request$phase)||request$phase%in%c("Powerplay","Middle","Death"))
}
execute_ai_request <- function(req,d) {
  if(!validate_ai_request(req)) return(list(error="That query did not map to a supported analysis."))
  switch(req$operation,
    phase_leaders={x<-d[d$phase==req$phase,,drop=FALSE]; a<-player_batting(x, min_balls=30); list(title=paste(req$phase,"over leaders"),data=head(a[order(-a$strike_rate),],10),note="Ranked by batting strike rate; minimum 30 valid balls.")},
    venue_rankings=list(title="Venue scoring",data=head(venue_summary(d)[order(-venue_summary(d)$first_avg),],10),note="Observed first-innings scoring; venue sample sizes shown."),
    team_records=list(title="Team records",data=team_records(d),note="Win percentage uses decided matches only."),
    player_rankings=list(title="Run leaders",data=head(player_batting(d)[order(-player_batting(d)$runs),],10),note="Ranked by batter runs."),
    help=list(title="Try an analytics question",data=data.frame(example=c("Death-over performers","Venue scoring","Team win records","Top run scorers")),note="Questions are mapped to predefined R analyses."))
}
explain_with_nim <- function(question,result) {
  key <- Sys.getenv("NVIDIA_API_KEY",""); if(!nzchar(key)) return("NVIDIA NIM is not configured. The table below contains the R-calculated result.")
  if(!requireNamespace("httr",quietly=TRUE)||!requireNamespace("jsonlite",quietly=TRUE)) return("NIM explanation unavailable because httr/jsonlite are not installed. R results are shown below.")
  payload <- list(model=Sys.getenv("NVIDIA_NIM_MODEL","meta/llama-3.1-70b-instruct"),temperature=0.2,max_tokens=160,
    messages=list(list(role="system",content="Summarize the supplied R result only. Do not add statistics or make new calculations."),
      list(role="user",content=paste("Question:",question,"\nR result:",paste(capture.output(print(result$data)),collapse="\n"),"\nContext:",result$note))))
  ans <- tryCatch({r<-httr::POST("https://integrate.api.nvidia.com/v1/chat/completions",httr::add_headers(Authorization=paste("Bearer",key)),body=payload,encode="json",httr::timeout(15)); if(httr::status_code(r)>=300) stop("NIM returned an error"); httr::content(r,as="parsed")$choices[[1]]$message$content},error=function(e) "NIM is temporarily unavailable. The table below contains the R-calculated result.")
  ans
}
