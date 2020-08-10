adp <- read.csv("D:/Data_Jocks_Projects/ADP_v_Production/Data/2019_ADP_New.csv",stringsAsFactors = FALSE)
stats <- read.csv("D:/Data_Jocks_Projects/ADP_v_Production/Data/2019_Stats_Clean.csv",stringsAsFactors = FALSE)

initial_pos_rank <-c()
final_pos_rank <- c()

for(i in 1:length(adp[,1])){
  player_name <- adp[i,"Name"]
  if(length(stats[stats[,"Name"]==player_name,"Pos_Rk"])==1){
    final_pos_rank <- c(final_pos_rank,stats[stats[,"Name"]==player_name,"Pos_Rk"])
    initial_pos_rank <- c(initial_pos_rank,adp[i,"Pos_ADP"])
  }
}
plot(initial_pos_rank,final_pos_rank,main="2019",xlab="Pre-Draft Position Rank",ylab = "Season End Position Rank", col="blue",xlim=c(0,65), ylim=c(0,100))
