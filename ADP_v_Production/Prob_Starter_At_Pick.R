#This script will compute the probability of finding a starter at
#the current pick +/- 5 picks for buffering. That is, 
#at the current pick x, we find percentage of starters from picks x-5,x+5

buffer <- 3
max_adp <- 150
year_range <- 2019:2007
num_years <- length(year_range)
#contains num starters found at pick i over all years
starters_at_pick <- rep(0,max_adp)
smoothed_starters_at_pick <- rep(0,max_adp)
players_checked_at_pick <- (2*buffer+1)*length(year_range)

for(i in year_range){
  adp <- read.csv(paste("D:/Data_Jocks_Projects/ADP_v_Production/Data/", as.character(i),"_ADP_New.csv",sep=""),stringsAsFactors = FALSE)
  stats <- read.csv(paste("D:/Data_Jocks_Projects/ADP_v_Production/Data/", as.character(i),"_Stats_Clean.csv",sep=""),stringsAsFactors = FALSE)
  for(j in 1:max_adp){
    player_name <- adp[j,"Name"]
    player_row <- stats[stats[,"Name"]==player_name,]
    #If we find exactly one player with given name AND VBD>0, count them
    if(!is.na(player_row[1,"VBD"])){
      #we found a starter at current adp in current year
      starters_at_pick[j] <- starters_at_pick[j]+1
    }
  }
}

#now, smooth it out
num_picks_summed<- rep(0,max_adp)
for(i in 1:buffer){
  smoothed_starters_at_pick[i] <- sum(starters_at_pick[1:(i+buffer)])
  num_picks_summed[i] <- length(1:(i+buffer))
}
for(i in (buffer+1):(max_adp-buffer)){
  smoothed_starters_at_pick[i] <- sum(starters_at_pick[(i-buffer):(i+buffer)])
  num_picks_summed[i] <- length((i-buffer):(i+buffer))
}
for(i in (max_adp-buffer+1):max_adp){
  smoothed_starters_at_pick[i] <-sum(starters_at_pick[(i-buffer):max_adp])
  num_picks_summed[i] <- length((i-buffer):max_adp)
}

smoothed_prob_of_starter <- smoothed_starters_at_pick/(num_years*num_picks_summed)
plot(1:max_adp, smoothed_prob_of_starter)