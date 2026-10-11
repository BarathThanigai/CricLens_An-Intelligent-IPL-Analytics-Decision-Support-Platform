# Synthetic unit fixtures only; these records are not historical IPL matches.
source("R/analytics.R")
source("R/data_ingestion.R")
source("R/predictive_models.R")
source("R/ai_assistant.R")
toy <- data.frame(match_id=c("SYN1","SYN1","SYN1","SYN1","SYN2","SYN2"),season=c(2024,2024,2024,2024,2025,2025),
 innings=c(1,1,2,2,1,1),over=c(1,2,1,2,1,2),ball=1,runs_batter=c(4,0,1,6,2,4),balls_faced=1,
 runs_total=c(4,1,1,6,2,4),runs_bowler=c(4,1,1,6,2,4),runs_extras=0,runs_target=c(NA,NA,6,6,NA,NA),valid_ball=1,
 player=c("A","B","C","C","A","A"),bowler=c("X","X","Y","Y","X","X"),
 team=c("North","North","South","South","North","North"),opposition=c("South","South","North","North","East","East"),
 venue="Synthetic Ground",date=as.Date(c("2024-01-01","2024-01-01","2024-01-01","2024-01-01","2025-01-01","2025-01-01")),
 match_winner=c("North","North","North","North","North","North"),toss_winner="North",toss_decision="bat",result_type="runs",
 wicket_kind=c(NA,NA,NA,"bowled",NA,NA),player_out=c(NA,NA,NA,"C",NA,NA),phase=c("Powerplay","Powerplay","Powerplay","Powerplay","Powerplay","Powerplay"),wicket=c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE),dismissal=c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE),stringsAsFactors=FALSE)
stopifnot(find_col(c("match_id","id"),c("id","match_id"))=="id")
bat<-player_batting(toy); stopifnot(bat$runs[bat$player=="A"]==10,bat$balls[bat$player=="A"]==3)
bowl<-player_bowling(toy); stopifnot(bowl$balls[bowl$player=="X"]==4,bowl$wickets[bowl$player=="Y"]==1)
stopifnot(bat$average[bat$player=="C"]==7)
runout<-toy[1,,drop=FALSE]; runout$match_id<-"RUNOUT"; runout$player<-"A"; runout$bowler<-"X"; runout$runs_batter<-0; runout$runs_total<-0; runout$runs_bowler<-0; runout$player_out<-"A"; runout$wicket_kind<-"run out"; runout$wicket<-TRUE; runout$dismissal<-TRUE
runout_bat<-player_batting(runout); runout_bowl<-player_bowling(runout); stopifnot(runout_bat$dismissals==1,runout_bowl$wickets==0)
tr<-team_records(toy); stopifnot(tr$wins[tr$team=="North"]==2,tr$played[tr$team=="North"]==2,tr$win_pct[tr$team=="North"]==100)
h2h<-team_head_to_head(toy,"North","South"); stopifnot(h2h$matches==1,h2h$team_a_wins==1)
vs<-venue_summary(toy); stopifnot(vs$matches==2,vs$first_avg==5.5,vs$second_avg==7)
ph<-phase_summary(toy, "A"); stopifnot(ph$runs==10,ph$balls==3)
stopifnot(nrow(filter_deliveries(toy,season=2025))==2,phase_summary(toy)$deliveries==6)
snap<-build_chase_snapshots(toy); stopifnot(nrow(snap)==2,all(snap$won==0),snap$run_diff[1]==-5)
stopifnot(!evaluate_chase_model(toy)$available)
stopifnot(is.na(safe_div(1,0)),validate_ai_request(list(operation="phase_leaders",phase="Death")),!validate_ai_request(list(operation="eval",code="1+1")))
stopifnot(nrow(impact_score(bat,bowl))==length(unique(c(bat$player,bowl$player))))
profiles<-h2h_player_profiles(toy,"A","X"); stopifnot(all(c("Batting","Bowling")%in%profiles$profile),any(profiles$metric=="Strike rate"),any(profiles$metric=="Economy rate"))
stopifnot(nrow(filter_explorer_data(toy))==nrow(toy))
explorer_filtered<-filter_explorer_data(toy,global_search="A",text_filters=list(list(column="player",query="A")),category_filters=list(list(column="team",values="North")),numeric_filters=list(list(column="runs_batter",min=2,max=4)),date_range=as.Date(c("2024-01-01","2024-12-31")))
stopifnot(nrow(explorer_filtered)==1,explorer_filtered$player=="A",explorer_filtered$runs_batter==4)
csv_export<-tempfile(fileext=".csv"); utils::write.csv(explorer_filtered,csv_export,row.names=FALSE); csv_roundtrip<-utils::read.csv(csv_export,stringsAsFactors=FALSE); unlink(csv_export); stopifnot(nrow(csv_roundtrip)==nrow(explorer_filtered),csv_roundtrip$player=="A")
stopifnot(nrow(filter_explorer_data(toy,global_search="does-not-match"))==0)
impact_fixture<-do.call(rbind,lapply(seq_along(c("P1","P2","P3")),function(i){p<-c("P1","P2","P3")[i]; n<-30; wk<-rep(FALSE,n); wk[30]<-TRUE; data.frame(player=p,bowler=p,match_id=p,innings=1,season=2024,over=rep(1:5,each=6),runs_batter=rep(c(1,2,4)[i],n),balls_faced=1,runs_total=rep(c(1,2,4)[i],n),runs_bowler=rep(c(1,4,3)[i],n),valid_ball=1,wicket=wk,dismissal=wk,player_out=ifelse(wk,p,NA),wicket_kind=ifelse(wk,"bowled",NA),stringsAsFactors=FALSE)}))
impact_bat<-player_batting(impact_fixture); impact_bowl<-player_bowling(impact_fixture); score_bat<-impact_score(impact_bat,impact_bowl,1,0); score_bowl<-impact_score(impact_bat,impact_bowl,0,1)
stopifnot(all(score_bat$role=="All-rounder"),any(abs(score_bat$impact-score_bowl$impact)>.01),all(is.finite(score_bat$impact)),all(is.finite(score_bowl$impact)))
stopifnot(all(is.na(impact_score(impact_bat,impact_bowl,0,0)$impact)))
fixture<-tempfile(fileext=".csv"); utils::write.csv(data.frame(id="SYN",date="2024-03-01",season="2023/24",innings=1,batting_team="North",bowling_team="South",over=0,ball=1,batsman="A",batter_runs=4,total_runs=5,extras=1,legal_ball=1,ground="Synthetic Ground"),fixture,row.names=FALSE)
clean<-read_ipl_csv(fixture); unlink(fixture); stopifnot(clean$season==2024,clean$over==1,clean$phase=="Powerplay",clean$runs_total==5,clean$runs_batter==4,clean$runs_extras==1)
extras_fixture<-tempfile(fileext=".csv"); utils::write.csv(data.frame(id=c("WIDE","NOBALL"),innings=1,batting_team="North",bowling_team="South",batter="A",bowler="X",total_runs=c(1,1),runs_batter=0,extras=c(1,1),extra_type=c("wides","noballs"),legal_ball=0,wicket_kind=c("obstructing the field","retired hurt"),player_out=c("A","A")),extras_fixture,row.names=FALSE)
extras_clean<-read_ipl_csv(extras_fixture); unlink(extras_fixture); stopifnot(identical(extras_clean$balls_faced,c(0,1)),identical(extras_clean$dismissal,c(TRUE,FALSE)),identical(extras_clean$wicket,c(TRUE,FALSE)))
cat("Synthetic analytics checks passed.\n")
