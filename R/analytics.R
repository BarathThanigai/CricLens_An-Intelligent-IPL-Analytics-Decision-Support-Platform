safe_div <- function(a,b) ifelse(is.na(b) | b == 0, NA_real_, a/b)
safe_mean <- function(x) if(!length(x)||!any(is.finite(x))) NA_real_ else mean(x,na.rm=TRUE)
matches_table <- function(d) {
  if (is.null(d) || !nrow(d)) return(data.frame())
  ids <- unique(d$match_id)
  groups <- split(d,factor(d$match_id,levels=ids),drop=TRUE)
  rows <- lapply(groups, function(allx) { id<-as.character(allx$match_id[1]); first_rows<-allx[allx$innings==min(allx$innings,na.rm=TRUE),,drop=FALSE]; x <- first_rows[1,,drop=FALSE]
    second_rows<-allx[allx$innings==2,,drop=FALSE]; second_team<-if(nrow(second_rows)) second_rows$team[1] else x$opposition
    score1 <- sum(allx$runs_total[allx$innings==1],na.rm=TRUE); score2 <- sum(allx$runs_total[allx$innings==2],na.rm=TRUE)
    data.frame(match_id=id, season=x$season, date=x$date, venue=x$venue, team1=x$team, team2=second_team,
      winner=x$match_winner, toss_winner=x$toss_winner, toss_decision=x$toss_decision, result_type=x$result_type,
      first_score=score1, second_score=score2,
      stringsAsFactors=FALSE) })
  do.call(rbind,rows)
}
team_season_records <- function(d) {
  if(!nrow(d)) return(data.frame())
  ss<-sort(unique(d$season[!is.na(d$season)])); out<-lapply(ss,function(s){x<-team_records(d[d$season==s,,drop=FALSE]); if(nrow(x)) {x$season<-s;x} else NULL})
  out<-Filter(Negate(is.null),out); if(length(out)) do.call(rbind,out) else data.frame()
}
match_condition_summary <- function(d, group=c("toss_decision","venue")) {
  group<-match.arg(group); m<-matches_table(d); if(!nrow(m)) return(data.frame())
  if(group=="toss_decision") {g<-as.character(m$toss_decision); split_by<-"toss_decision"} else {g<-as.character(m$venue);split_by<-"venue"}
  u<-unique(g[!is.na(g)&nzchar(g)]); do.call(rbind,lapply(u,function(v){a<-m[g==v,,drop=FALSE]; decided<-!is.na(a$winner)&nzchar(a$winner)
    batting_first_wins<-sum(decided&a$winner==a$team1,na.rm=TRUE); chasing_wins<-sum(decided&a$winner==a$team2,na.rm=TRUE)
    data.frame(category=v,matches=nrow(a),decided=sum(decided),batting_first_wins=batting_first_wins,chasing_wins=chasing_wins,
      batting_first_pct=100*safe_div(batting_first_wins,sum(decided)),chasing_pct=100*safe_div(chasing_wins,sum(decided)),toss_winner_wins=sum(decided&a$winner==a$toss_winner,na.rm=TRUE),stringsAsFactors=FALSE)
  }))
}
team_head_to_head <- function(d, team_a, team_b) {
  m<-matches_table(d); m<-m[(m$team1==team_a&m$team2==team_b)|(m$team1==team_b&m$team2==team_a),,drop=FALSE]
  if(!nrow(m)) return(data.frame(team_a=team_a,team_b=team_b,matches=0,team_a_wins=0,team_b_wins=0,team_a_win_pct=NA_real_,team_b_win_pct=NA_real_,avg_first_score=NA_real_,avg_second_score=NA_real_))
  decided<-!is.na(m$winner)&nzchar(m$winner); aw<-sum(decided&m$winner==team_a,na.rm=TRUE); bw<-sum(decided&m$winner==team_b,na.rm=TRUE)
  data.frame(team_a=team_a,team_b=team_b,matches=nrow(m),decided=sum(decided),team_a_wins=aw,team_b_wins=bw,
      team_a_win_pct=100*safe_div(aw,sum(decided)),team_b_win_pct=100*safe_div(bw,sum(decided)),avg_first_score=safe_mean(m$first_score),avg_second_score=safe_mean(m$second_score))
}
filter_deliveries <- function(d, season=NULL, team=NULL, venue=NULL) {
  if (is.null(d)) return(d)
  if(requireNamespace("dplyr",quietly=TRUE)) {
    out<-dplyr::as_tibble(d)
    if(!is.null(season)&&length(season)&&!anyNA(season)) out<-dplyr::filter(out,.data$season %in% .env$season)
    if(!is.null(team)&&length(team)&&!identical(team,"All teams")) out<-dplyr::filter(out,.data$team %in% .env$team | .data$opposition %in% .env$team)
    if(!is.null(venue)&&length(venue)&&!identical(venue,"All venues")) out<-dplyr::filter(out,.data$venue %in% .env$venue)
    return(as.data.frame(out))
  }
  if (!is.null(season) && length(season) && !anyNA(season)) d <- d[d$season %in% season,,drop=FALSE]
  if (!is.null(team) && length(team) && !identical(team,"All teams")) d <- d[d$team %in% team | d$opposition %in% team,,drop=FALSE]
  if (!is.null(venue) && length(venue) && !identical(venue,"All venues")) d <- d[d$venue %in% venue,,drop=FALSE]
  d
}
group_sum <- function(d, key, value) {
  if (!nrow(d)) return(data.frame())
  a <- aggregate(d[[value]], list(d[[key]]), sum, na.rm=TRUE); names(a) <- c(key,value); a
}
player_batting <- function(d, min_balls=0) {
  x <- d[!is.na(d$player) & nzchar(d$player),,drop=FALSE]
  if (!nrow(x)) return(data.frame())
  dismissed<-if("dismissal"%in%names(x))x$dismissal else x$wicket
  x$outs <- as.integer(dismissed & !is.na(x$player_out) & x$player_out==x$player)
  x$fours <- as.integer(x$runs_batter==4); x$sixes <- as.integer(x$runs_batter==6)
  x$boundaries <- as.integer(x$runs_batter %in% c(4,6))
  sums <- stats::aggregate(x[c("runs_batter","balls_faced","outs","fours","sixes","boundaries")],list(player=x$player),sum,na.rm=TRUE)
  names(sums)[2:7] <- c("runs","balls","dismissals","fours","sixes","boundaries")
  innings <- unique(x[c("player","match_id","innings")]); inn <- aggregate(rep(1,nrow(innings)),list(player=innings$player),sum); names(inn)[2] <- "innings"
  games <- unique(x[c("player","match_id")]); mat <- aggregate(rep(1,nrow(games)),list(player=games$player),sum); names(mat)[2] <- "matches"
  ans <- merge(merge(sums,inn,by="player"),mat,by="player")
  ans$average <- safe_div(ans$runs,ans$dismissals); ans$strike_rate <- 100*safe_div(ans$runs,ans$balls)
  ans$boundary_pct <- 100*safe_div(ans$boundaries,ans$runs)
  ans[ans$balls>=min_balls,c("player","runs","balls","innings","average","strike_rate","fours","sixes","boundary_pct","matches"),drop=FALSE]
}
player_bowling <- function(d) {
  x<-d[!is.na(d$bowler)&nzchar(d$bowler),,drop=FALSE]; if(!nrow(x)) return(data.frame())
  bowling_groups<-split(x,x$bowler,drop=TRUE); do.call(rbind,lapply(bowling_groups,function(z){p<-z$bowler[1]; balls<-sum(z$valid_ball==1,na.rm=TRUE); credited<-tolower(trimws(as.character(z$wicket_kind))); wk<-sum(credited%in%c("bowled","caught","caught and bowled","lbw","stumped","hit wicket"),na.rm=TRUE); conceded<-sum(z$runs_bowler,na.rm=TRUE)
    data.frame(player=p,balls=balls,overs=balls/6,runs_conceded=conceded,wickets=wk,economy=6*safe_div(conceded,balls),bowling_average=safe_div(conceded,wk),bowling_sr=safe_div(balls,wk),matches=length(unique(z$match_id))) }))
}
team_records <- function(d) {
  m<-matches_table(d); if(!nrow(m)) return(data.frame())
  teams<-unique(c(m$team1,m$team2)); do.call(rbind,lapply(teams,function(t){played<-m$team1==t|m$team2==t; win<-!is.na(m$winner)&m$winner==t; decided<-played&!is.na(m$winner)&nzchar(m$winner)
    chase<-m$team2==t; first<-m$team1==t; chase_decided<-chase&decided; first_decided<-first&decided
    scored<-ifelse(m$team1==t,m$first_score,m$second_score); conceded<-ifelse(m$team1==t,m$second_score,m$first_score)
    data.frame(team=t,played=sum(played),wins=sum(win),losses=sum(decided&!win),no_result=sum(played&!decided),
      win_pct=100*safe_div(sum(win),sum(decided)),avg_runs_scored=safe_mean(scored[played]),avg_runs_conceded=safe_mean(conceded[played]),
      chase_matches=sum(chase),chase_win_pct=100*safe_div(sum(chase_decided&m$winner==t,na.rm=TRUE),sum(chase_decided)),
      batting_first_win_pct=100*safe_div(sum(first_decided&m$winner==t,na.rm=TRUE),sum(first_decided))) }))
}
venue_summary <- function(d) {
  if(!nrow(d)) return(data.frame())
  i<-stats::aggregate(d["runs_total"],list(match_id=d$match_id,innings=d$innings,venue=d$venue),sum,na.rm=TRUE); names(i)[4]<-"score"
  v<-unique(i$venue); do.call(rbind,lapply(v,function(n){a<-i[i$venue==n,,drop=FALSE]; data.frame(venue=n,matches=length(unique(a$match_id)),avg_score=safe_mean(a$score),first_avg=safe_mean(a$score[a$innings==1]),second_avg=safe_mean(a$score[a$innings==2]),score_sd=sd(a$score,na.rm=TRUE))}))
}
season_summary <- function(d) {
  if(!nrow(d)) return(data.frame()); s<-sort(unique(d$season[!is.na(d$season)])); do.call(rbind,lapply(s,function(y){x<-d[d$season==y,,drop=FALSE]; data.frame(season=y,matches=length(unique(x$match_id)),runs=sum(x$runs_total,na.rm=TRUE),deliveries=nrow(x),run_rate=6*sum(x$runs_total,na.rm=TRUE)/sum(x$valid_ball==1,na.rm=TRUE))}))
}
phase_summary <- function(d, player=NULL) {
  if(!is.null(player)) d<-d[d$player==player,,drop=FALSE]
  if(!nrow(d)) return(data.frame()); p<-unique(d$phase); do.call(rbind,lapply(p,function(q){x<-d[d$phase==q,,drop=FALSE]; balls<-sum(x$valid_ball==1); runs<-sum(x$runs_batter); data.frame(phase=q,runs=runs,balls=balls,strike_rate=100*safe_div(runs,balls),wickets=sum(x$wicket),deliveries=nrow(x))}))
}
player_match_batting <- function(d, player) {
  x<-d[d$player==player,,drop=FALSE]; if(!nrow(x)) return(data.frame())
  groups<-split(x,x$match_id,drop=TRUE)
  do.call(rbind,lapply(groups,function(z){runs<-sum(z$runs_batter,na.rm=TRUE); balls<-sum(z$balls_faced,na.rm=TRUE)
    data.frame(match_id=z$match_id[1],season=z$season[1],date=z$date[1],opposition=z$opposition[1],venue=z$venue[1],runs=runs,balls=balls,strike_rate=100*safe_div(runs,balls))}))
}
impact_score <- function(batting, bowling=NULL, bat_weight=.6, bowl_weight=.4, shrinkage=20) {
  if(is.null(batting)||!nrow(batting)) batting<-data.frame(player=character(),balls=numeric(),average=numeric(),strike_rate=numeric())
  if(is.null(bowling)||!nrow(bowling)) bowling<-data.frame(player=character(),balls=numeric(),wickets=numeric(),overs=numeric(),economy=numeric())
  if(!nrow(batting)&&!nrow(bowling)) return(data.frame())
  bat_weight<-max(0,suppressWarnings(as.numeric(bat_weight)[1])); bowl_weight<-max(0,suppressWarnings(as.numeric(bowl_weight)[1]))
  if(!is.finite(bat_weight)) bat_weight<-0; if(!is.finite(bowl_weight)) bowl_weight<-0
  zscore<-function(v,eligible=rep(TRUE,length(v))) {out<-rep(NA_real_,length(v)); ok<-eligible&is.finite(v); if(sum(ok)>=2L&&is.finite(stats::sd(v[ok]))&&stats::sd(v[ok])>0) out[ok]<-(v[ok]-mean(v[ok]))/stats::sd(v[ok]); out}
  bplayers<-as.character(batting$player); wplayers<-as.character(bowling$player); players<-sort(unique(c(bplayers,wplayers)))
  bidx<-match(players,bplayers); widx<-match(players,wplayers)
  bat_eligible<-!is.na(bidx)&is.finite(batting$balls[bidx])&batting$balls[bidx]>=30
  bat_avg<-rep(NA_real_,length(players)); bat_sr<-rep(NA_real_,length(players)); bat_balls<-rep(0,length(players)); bat_matches<-rep(0,length(players))
  bat_avg[!is.na(bidx)]<-batting$average[bidx[!is.na(bidx)]]; bat_sr[!is.na(bidx)]<-batting$strike_rate[bidx[!is.na(bidx)]]; bat_balls[!is.na(bidx)]<-batting$balls[bidx[!is.na(bidx)]]; if("matches"%in%names(batting))bat_matches[!is.na(bidx)]<-batting$matches[bidx[!is.na(bidx)]]
  bat_avg_z<-zscore(bat_avg,bat_eligible); eff_raw<-rep(NA_real_,length(players)); fit_rows<-which(bat_eligible&is.finite(bat_avg)&is.finite(bat_sr))
  if(length(fit_rows)>=3L&&length(unique(bat_avg[fit_rows]))>=2L) {fit<-stats::lm(bat_sr[fit_rows]~bat_avg[fit_rows]); eff_raw[fit_rows]<-stats::residuals(fit)} else eff_raw[bat_eligible]<-bat_sr[bat_eligible]
  eff_z<-zscore(eff_raw,bat_eligible); bat_component<-rep(NA_real_,length(players));
  for(i in which(bat_eligible)) {parts<-c(bat_avg_z[i],eff_z[i]); parts<-parts[is.finite(parts)]; if(length(parts))bat_component[i]<-mean(parts)*sqrt(bat_balls[i]/(bat_balls[i]+shrinkage))}
  bowl_eligible<-!is.na(widx)&is.finite(bowling$balls[widx])&bowling$balls[widx]>=12
  bowl_balls<-rep(0,length(players)); bowl_matches<-rep(0,length(players)); economy<-rep(NA_real_,length(players)); wickets<-rep(NA_real_,length(players)); overs<-rep(NA_real_,length(players))
  bowl_balls[!is.na(widx)]<-bowling$balls[widx[!is.na(widx)]]; if("matches"%in%names(bowling))bowl_matches[!is.na(widx)]<-bowling$matches[widx[!is.na(widx)]]; economy[!is.na(widx)]<-bowling$economy[widx[!is.na(widx)]]; wickets[!is.na(widx)]<-bowling$wickets[widx[!is.na(widx)]]; overs[!is.na(widx)]<-bowling$overs[widx[!is.na(widx)]]
  wicket_rate<-wickets/pmax(overs,1); economy_z<-zscore(-economy,bowl_eligible); rate_resid<-rep(NA_real_,length(players)); fit_rows<-which(bowl_eligible&is.finite(wicket_rate)&is.finite(economy))
  if(length(fit_rows)>=3L&&length(unique(economy[fit_rows]))>=2L) {fit<-stats::lm(wicket_rate[fit_rows]~economy[fit_rows]); rate_resid[fit_rows]<-stats::residuals(fit)} else rate_resid[bowl_eligible]<-wicket_rate[bowl_eligible]
  rate_z<-zscore(rate_resid,bowl_eligible); bowl_component<-rep(NA_real_,length(players));
  for(i in which(bowl_eligible)) {parts<-c(economy_z[i],rate_z[i]); parts<-parts[is.finite(parts)]; if(length(parts))bowl_component[i]<-mean(parts)*sqrt(bowl_balls[i]/(bowl_balls[i]+shrinkage))}
  bw<-ifelse(is.finite(bowl_component),bowl_weight,0); aw<-ifelse(is.finite(bat_component),bat_weight,0); denom<-aw+bw
  score<-ifelse(denom>0,(ifelse(is.finite(bat_component),bat_component*aw,0)+ifelse(is.finite(bowl_component),bowl_component*bw,0))/ifelse(denom>0,denom,1),NA_real_)
  role<-ifelse(is.finite(bat_component)&is.finite(bowl_component),"All-rounder",ifelse(is.finite(bat_component),"Batter",ifelse(is.finite(bowl_component),"Bowler","Insufficient sample")))
  data.frame(player=players,role=role,impact=score,batting_contribution=bat_component,bowling_contribution=bowl_component,batting_balls=bat_balls,batting_matches=bat_matches,bowling_balls=bowl_balls,bowling_matches=bowl_matches,stringsAsFactors=FALSE)
}

# Apply Data Explorer filters to the complete canonical data, before the display preview limit.
filter_explorer_data <- function(d, global_search="", text_filters=list(), category_filters=list(), numeric_filters=list(), date_range=NULL) {
  if(is.null(d)||!nrow(d)) return(d)
  keep<-rep(TRUE,nrow(d)); contains<-function(v,q) {if(!length(q)||is.na(q)||!nzchar(q))return(rep(TRUE,length(v))); !is.na(v)&grepl(q,as.character(v),ignore.case=TRUE,perl=TRUE)}
  if(length(global_search)&&!is.na(global_search)&&nzchar(global_search)) {cols<-names(d)[vapply(d,function(v)is.character(v)||is.factor(v),logical(1))]; hit<-rep(FALSE,nrow(d)); for(nm in cols)hit<-hit|contains(d[[nm]],global_search); keep<-keep&hit}
  for(f in text_filters) if(!is.null(f$column)&&f$column%in%names(d)&&length(f$query)&&nzchar(f$query)) keep<-keep&contains(d[[f$column]],f$query)
  for(f in category_filters) if(!is.null(f$column)&&f$column%in%names(d)&&length(f$values)) keep<-keep&(!is.na(d[[f$column]])&as.character(d[[f$column]])%in%as.character(f$values))
  for(f in numeric_filters) if(!is.null(f$column)&&f$column%in%names(d)&&is.numeric(d[[f$column]])) {v<-d[[f$column]]; if(length(f$min)&&is.finite(f$min))keep<-keep&!is.na(v)&v>=f$min; if(length(f$max)&&is.finite(f$max))keep<-keep&!is.na(v)&v<=f$max}
  if(inherits(d$date,"Date")&&length(date_range)>=2L&&!anyNA(date_range))keep<-keep&!is.na(d$date)&d$date>=as.Date(date_range[1])&d$date<=as.Date(date_range[2])
  d[keep,,drop=FALSE]
}

h2h_player_profiles <- function(d, player_a, player_b, min_batting_balls=30, min_bowling_balls=12) {
  empty<-data.frame(profile=character(),metric=character(),player_a=numeric(),player_b=numeric(),sample_a=character(),sample_b=character(),status=character(),stringsAsFactors=FALSE)
  if(is.null(d)||!nrow(d))return(empty)
  ba<-player_batting(d[d$player==player_a,,drop=FALSE]); bb<-player_batting(d[d$player==player_b,,drop=FALSE]); wa<-player_bowling(d[d$bowler==player_a,,drop=FALSE]); wb<-player_bowling(d[d$bowler==player_b,,drop=FALSE])
  getrow<-function(x)if(nrow(x))x[1,,drop=FALSE] else NULL; ba<-getrow(ba);bb<-getrow(bb);wa<-getrow(wa);wb<-getrow(wb)
  bsample<-function(x)if(is.null(x))"No batting data" else paste(x$matches,"matches ·",x$innings,"innings ·",x$balls,"balls")
  wsample<-function(x)if(is.null(x))"No bowling data" else paste(x$matches,"matches ·",x$overs,"overs ·",x$balls,"legal balls")
  rows<-list(); add<-function(profile,metric,a,b,sa,sb,status="Available")rows[[length(rows)+1L]]<<-data.frame(profile=profile,metric=metric,player_a=a,player_b=b,sample_a=sa,sample_b=sb,status=status,stringsAsFactors=FALSE)
  batting_metrics<-c("Runs","Innings","Balls faced","Batting average","Strike rate","Fours","Sixes","Boundary %")
  bat_cols<-c("runs","innings","balls","average","strike_rate","fours","sixes","boundary_pct")
  if(!is.null(ba)||!is.null(bb))for(i in seq_along(bat_cols)){av<-if(is.null(ba))NA_real_ else ba[[bat_cols[i]]]; bv<-if(is.null(bb))NA_real_ else bb[[bat_cols[i]]]; status<-if((is.null(ba)||ba$balls<min_batting_balls)||(is.null(bb)||bb$balls<min_batting_balls))"Low batting sample" else "Available"; add("Batting",batting_metrics[i],av,bv,bsample(ba),bsample(bb),status)}
  bowling_metrics<-c("Balls bowled","Overs bowled","Runs conceded","Wickets","Bowling average","Economy rate","Bowling strike rate")
  bowl_cols<-c("balls","overs","runs_conceded","wickets","bowling_average","economy","bowling_sr")
  if(!is.null(wa)||!is.null(wb))for(i in seq_along(bowl_cols)){av<-if(is.null(wa))NA_real_ else wa[[bowl_cols[i]]]; bv<-if(is.null(wb))NA_real_ else wb[[bowl_cols[i]]]; status<-if((is.null(wa)||wa$balls<min_bowling_balls)||(is.null(wb)||wb$balls<min_bowling_balls))"Low bowling sample" else "Available"; add("Bowling",bowling_metrics[i],av,bv,wsample(wa),wsample(wb),status)}
  if(!length(rows))return(empty); do.call(rbind,rows)
}
