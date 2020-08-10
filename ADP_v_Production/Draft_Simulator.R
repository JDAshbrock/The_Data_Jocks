year_adp <- read.csv('D:/Data_Jocks_Projects/ADP_v_Production/Data/2019_ADP_New.csv',stringsAsFactors = FALSE)
num_picks <- length(adp[,1])
num_teams <-12

rounds <-(num_picks-(num_picks%%num_teams))/num_teams

Draft_Board <-matrix(0,nrow=rounds,ncol=num_teams)
player_picks <- rep(0,num_picks)

for(i in 1:num_players){
  player_adp <- year_adp[i,"Overall"]
  player_sd <- year_adp[i,"Std..Dev"]
  player_picks[i] <- rnorm(1,player_adp,player_sd)
}
