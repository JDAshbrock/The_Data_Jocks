year_adp <- read.csv('D:/Data_Jocks_Projects/ADP_v_Production/Data/2019_ADP_New.csv',stringsAsFactors = FALSE)
year_stats <- read.csv('D:/Data_Jocks_Projects/ADP_v_Production/Data/2019_Stats_Clean.csv',stringsAsFactors = FALSE)
num_picks <- length(year_adp[,1])
num_teams <-12
rounds <-(num_picks-(num_picks%%num_teams))/num_teams
simulations <- 500
special_team <- 13 #set this >12 for regular, randomized drafting

potential_points <- rep(0, num_teams)
for(k in 1:simulations){
Draft_Board <-matrix(0,nrow=rounds,ncol=num_teams)
player_picks <- rep(0,num_picks)

for(i in 1:num_picks){
  player_adp <- year_adp[i,"Overall"]
  player_sd <- year_adp[i,"Std..Dev"]
  player_picks[i] <- rnorm(1,player_adp,player_sd)
}
year_adp$player_rv <- player_picks
year_adp$Available <- rep(TRUE, num_picks)
sorted_adp<-year_adp[order(year_adp[,"player_rv"]),]
for(i in 1:rounds){
  for(j in 1:num_teams){
    if(i%%2 ==1){
      if(j == special_team){
        #Team that drafts BPA
        for(l in 1:num_picks){
          #look through adp list, find first available player
          if(year_adp[l,"Available"]){
            my_pick <-year_adp[l,"Name"]
            year_adp[l,"Available"]<- FALSE
            sorted_adp[sorted_adp[,"Name"]==my_pick,"Available"]<-FALSE
            break#found BPA
          }
        }#End finding player and updating lists
        Draft_Board[i,j]<- my_pick
      }else{
        #team NOT using BPA, find first available in "sorted"
        for(l in 1:num_picks){
          if(sorted_adp[l,"Available"]){
            my_pick <- sorted_adp[l,"Name"]
            sorted_adp[l,"Available"]<-FALSE
            year_adp[year_adp[,"Name"]==my_pick,"Available"]<-FALSE
            break
          }
        }#end searching for player
        Draft_Board[i,j]<-my_pick
      }#end even numbered round logic
    }else{#odd numbered round, go backwards
      if((13-j) == special_team){
        for(l in 1:num_picks){
          #look through adp list, find first available player
          if(year_adp[l,"Available"]){
            my_pick <-year_adp[l,"Name"]
            year_adp[l,"Available"]<- FALSE
            sorted_adp[sorted_adp[,"Name"]==my_pick,"Available"]<-FALSE
            break#found BPA
          }
        }#End finding player and updating lists
        Draft_Board[i,13-j]<-my_pick
      }else{
        #team NOT using BPA
        for(l in 1:num_picks){
          if(sorted_adp[l,"Available"]){
            #Found first available player in sorted_adp list
            my_pick <- sorted_adp[l,"Name"]
            #set this player to unavailable in both lists
            sorted_adp[l,"Available"]<-FALSE
            year_adp[year_adp[,"Name"]==my_pick,"Available"]<-FALSE
            break
          }
        }#end searching for player
        Draft_Board[i,13-j]<- my_pick
      }#END non BPA logic
    }#End odd numbered round
  }#end round
}#end draft
#Now compute potential points of best starting lineup
#1 QB, 2WR, 2RB, 1 TE, 1 FLEX

for(i in 1:num_teams){
  QB <- 0
  WR1 <- 0
  WR2 <- 0
  RB1 <- 0
  RB2 <- 0
  TE <- 0
  FLEX <- 0
  for(j in 1:rounds){
    flex_candidate <-0
    #Find player position
    pos <- year_adp[year_adp[,"Name"]==Draft_Board[j,i],"Pos"]
    player_pts <- year_stats[year_stats[,"Name"]==Draft_Board[j,i],"Fan_Pts"]
    if(length(player_pts)==0){
      next
    }
    if(pos == "QB" && player_pts > QB){
      QB<- player_pts
    }
    if(pos == "WR"){
      if(player_pts > WR1){
        #player is new WR1
        flex_candidate <- WR2
        WR2 <- WR1
        WR1 <- player_pts
      }else if(player_pts >WR2){
        flex_candidate<- WR2
        WR2 <- player_pts
      }else{
        flex_candidate <- player_pts
      }
    }#END IF(WR)
    if(pos =="RB"){
      if(player_pts>RB1){
        flex_candidate <- RB2
        RB2 <- RB1
        RB1 <- player_pts
      }else if(player_pts >RB2){
        flex_candidate <- RB2
        RB2 <- player_pts
      }else{
        flex_candidate <- player_pts
      }
    }#END IF(RB)
    if(pos == "TE"){
      if(player_pts > TE){
        flex_candidate <- TE
        TE <- player_pts
      }else{
        flex_candidate <- TE
      }
    }#END IF(TE)
    FLEX <- max(FLEX, flex_candidate)
  }#End looking at current player
  potential_points[i] <- potential_points[i]+QB+WR1+WR2+RB1+RB2+TE+FLEX
}#End one simulation
}#end all simulations

plot(1:12, potential_points/simulations,col="blue",xlab="Draft Position",ylab="Average Points", ylim=c(1075,1300))