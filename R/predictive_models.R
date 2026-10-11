# Chronological, match-grouped chase model using only the current chase state.
build_chase_snapshots <- function(d) {
  chase <- d[!is.na(d$innings) & d$innings == 2,,drop=FALSE]
  if (!nrow(chase)) return(data.frame())
  match_groups<-split(chase,chase$match_id,drop=TRUE)
  rows <- lapply(match_groups,function(x) {
    target <- suppressWarnings(max(x$runs_target,na.rm=TRUE)); if(!is.finite(target)||target<=0) return(NULL)
    winner <- x$match_winner[1]; if(is.na(winner)||!nzchar(winner)) return(NULL)
    by_over<-stats::aggregate(x[c("runs_total","dismissal","valid_ball")],list(over=x$over),sum,na.rm=TRUE)
    by_over<-by_over[order(by_over$over),,drop=FALSE]
    data.frame(match_id=x$match_id[1],date=x$date[1],season=x$season[1],
      run_diff=cumsum(by_over$runs_total)-target,wickets_left=10-cumsum(by_over$dismissal),
      balls_left=pmax(120-cumsum(by_over$valid_ball),0),won=as.integer(winner==x$team[1]))
  })
  rows <- Filter(Negate(is.null),rows); if(length(rows)) do.call(rbind,rows) else data.frame()
}
evaluate_chase_model <- function(d) {
  s <- build_chase_snapshots(d)
  if(nrow(s)<100 || length(unique(s$won))<2 || all(is.na(s$date))) return(list(available=FALSE,message="Not enough dated chase snapshots to evaluate a chronological model."))
  cut <- as.Date(stats::quantile(as.numeric(s$date),.8,na.rm=TRUE),origin="1970-01-01")
  train <- s[s$date<cut,,drop=FALSE]; test <- s[s$date>=cut,,drop=FALSE]
  if(nrow(train)<50||nrow(test)<20||length(unique(train$won))<2) return(list(available=FALSE,message="Chronological split leaves too few training or test records."))
  fit <- stats::glm(won~run_diff+wickets_left+balls_left,data=train,family=stats::binomial())
  prob <- stats::predict(fit,newdata=test,type="response")
  list(available=TRUE,model=fit,accuracy=mean((prob>=.5)==test$won),brier=mean((prob-test$won)^2),
    train_matches=length(unique(train$match_id)),test_matches=length(unique(test$match_id)),cutoff=cut,test_rows=nrow(test))
}
predict_chase <- function(model, score, target, wickets_lost, balls_used) {
  if(is.null(model)||!inherits(model,"glm")) return(NA_real_)
  as.numeric(stats::predict(model,newdata=data.frame(run_diff=score-target,wickets_left=10-wickets_lost,balls_left=max(120-balls_used,0)),type="response"))
}
