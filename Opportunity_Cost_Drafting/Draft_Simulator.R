first_available <- function(frame){
  #input is a dataframe with Boolean column named "Available"
  #and a column named "Name
  for(i in 1:length(frame[,1])){
    if(frame[i,"Available"]){
      return(frame[i, "Name"])
    }
  }
}

next_avail_pos <- function(frame, depth, pos){
  #frame must have column 'Available', 'Name', and 'Pos'
  #function returns first available player at given position
  #after 'depth' many available players have been encountered
  num_available <- 0
  for(i in 1:length(frame[,1])){
    if(frame[i,"Available"]){
      num_available <- num_available +1
      if(num_available >depth && frame[i,"Pos"]==pos){
        return(frame[i,"Name"])
      }
    }
  }
}

make_pick <- function(round, team, frame){
  #function changes board, list1, list2, based on the player picked
  #also needs to be input who the pick is
  #First, find the correct pick
  pick <- first_available(frame)
  if((round%%2) == 1){
    Draft_Board[round, team] <<- pick
    update_pos_counts(team, pick, AAV)
  }else{
    Draft_Board[round, 13-team]<<- pick
    update_pos_counts(13-team,pick, AAV)
  }
  sorted_adp[sorted_adp[,"Name"]==pick,"Available"]<<- FALSE
  AAV[AAV[,"Name"]==pick,"Available"]<<- FALSE
  update_pos_counts(team, pick, AAV)
}

make_special_pick <- function(round, team, frame){
  #This function changes based on the draft strategy used
  best_RB <- next_avail_pos(frame, 0, "RB")
  best_WR <- next_avail_pos(frame, 0, "WR")
  best_TE <- next_avail_pos(frame, 0, "TE")
  best_QB <- next_avail_pos(frame, 0, "QB")
  #find how long I have to wait until my next pick
  if((round&&0)==1){
    team_index = team
    picks_to_wait <- 2*(12-team_index)+1
  }else{
    team_index = 13-team
    picks_to_wait <- 2*(team_index-1)+1
  }
  next_RB <- next_avail_pos(frame, picks_to_wait, "RB")
  next_WR <- next_avail_pos(frame, picks_to_wait, "WR")
  next_TE <- next_avail_pos(frame, picks_to_wait, "TE")
  next_QB <- next_avail_pos(frame, picks_to_wait, "QB")
  
  RB_opp <- max(0,frame[frame[,"Name"]==best_RB,"AAV"]-frame[frame[,"Name"]==next_RB,"AAV"])
  WR_opp <- max(0,frame[frame[,"Name"]==best_WR,"AAV"]-frame[frame[,"Name"]==next_WR,"AAV"])
  TE_opp <- max(0,frame[frame[,"Name"]==best_TE,"AAV"]-frame[frame[,"Name"]==next_TE,"AAV"])
  QB_opp <- max(0,frame[frame[,"Name"]==best_QB,"AAV"]-frame[frame[,"Name"]==next_QB,"AAV"])
  max_opp <- max(RB_opp, WR_opp, TE_opp, QB_opp)
  if(max_opp==RB_opp){
    my_pick <- best_RB
  }else if(max_opp == WR_opp){
    my_pick <- best_WR
  }else if(max_opp == TE_opp){
    my_pick <- best_TE
  }else{
    my_pick <- best_QB
  }
  if(is.null(my_pick)){
    my_pick <- first_available(AAV)
  }
  if((round%%2) == 1){
    Draft_Board[round, team] <<- my_pick
    update_pos_counts(team, my_pick, AAV)
  }else{
    Draft_Board[round, 13-team]<<- my_pick
    update_pos_counts(13-team, my_pick, AAV)
  }
  sorted_adp[sorted_adp[,"Name"]==my_pick,"Available"]<<- FALSE
  AAV[AAV[,"Name"]==my_pick,"Available"]<<- FALSE
}

make_position_pick <- function(round, team, position, frame){
  pick<-next_avail_pos(frame,0,position)
  if(is.null(pick)){
    pick <- first_available(frame)
  }
  if((round%%2) == 1){
    Draft_Board[round, team] <<- pick
    update_pos_counts(team, pick, AAV)
  }else{
    Draft_Board[round, 13-team]<<- pick
    update_pos_counts(13-team, pick, AAV)
  }
  sorted_adp[sorted_adp[,"Name"]==pick,"Available"]<<- FALSE
  AAV[AAV[,"Name"]==pick,"Available"]<<- FALSE

}

update_pos_counts <- function(team, player, frame){
  pos <- frame[frame[,"Name"]==player,"Pos"]
  if(pos!= "DEF" && pos!="PK"){
    pos_counts[team, pos]<<- pos_counts[team,pos]+1
  }
}

draft_round <- function(round,special_team){
  for(i in 1:12){
    if(i == special_team){
      #if team i has more than 6 RB/WR and no QB, draft QB
      if((pos_counts[i,"RB"]+pos_counts[i,"WR"])>6 && pos_counts[i,"QB"]==0){
        make_position_pick(round, i, "QB", AAV)
      }else if((pos_counts[i,"RB"]+pos_counts[i,"WR"])>6 && pos_counts[i,"TE"]==0){
        make_position_pick(round, i, "TE", AAV)
      }else{
        make_special_pick(round, i, AAV)
      }
    }else{
      #if team i has more than 6 RB/WR and no QB, draft QB
      if((pos_counts[i,"RB"]+pos_counts[i,"WR"])>6 && pos_counts[i,'QB']==0){
        make_position_pick(round, i, "QB", sorted_adp)
      }else if((pos_counts[i,"RB"]+pos_counts[i,"WR"])>6 && pos_counts[i,'TE']==0){
        make_position_pick(round, i, "TE", sorted_adp)
      }else{
        make_pick(round, i, sorted_adp)
      }
    }
  }
}

  

#LOAD DATA
year_adp <- read.csv('D:/Data_Jocks_Projects/Opportunity_Cost_Drafting/2019_ADP.csv',stringsAsFactors = FALSE)
year_stats <- read.csv('D:/Data_Jocks_Projects/ADP_v_Production/Data/2019_Stats_Clean.csv',stringsAsFactors = FALSE)
num_picks <- length(year_adp[,1])
num_teams <-12
rounds <-(num_picks-(num_picks%%num_teams))/num_teams
simulations <- 500
special_team <- 6#set this >12 for regular, randomized drafting
AAV <- year_adp[order(year_adp[,"AAV"],decreasing=TRUE),]


potential_points <- rep(0, num_teams)
for(k in 1:simulations){
  Draft_Board <-matrix(0,nrow=rounds,ncol=num_teams)
  pos_counts =data.frame("Team"=1:num_teams,"RB"=rep(0,num_teams),"WR"=rep(0,num_teams), "TE"=rep(0,num_teams), "QB"=rep(0,num_teams))
  player_picks <- rep(0,num_picks)
  #randomize draft order
  for(i in 1:num_picks){
    player_adp <- year_adp[i,"Overall"]
    player_sd <- year_adp[i,"Std..Dev"]
    player_picks[i] <- rnorm(1,player_adp,player_sd)
  }
  year_adp$player_rv <- player_picks
  year_adp$Available <- rep(TRUE, num_picks)
  AAV$Available <- rep(TRUE, num_picks)
  sorted_adp<-year_adp[order(year_adp[,"player_rv"]),]
  

  for(i in 1:rounds){
    draft_round(i, special_team)
  }#end draft
  #Now compute potential points of best starting lineup
  #1 QB, 2WR, 2RB, 1 TE, 1 FLEX
  
  for(i in 1:num_teams){
    #The baselines here are to guard against code anomalies
    #that would cause a team to not draft enough players at a pos.
    #and to guard against drafting just '1' player at a position
    #that would get hurt. They are chosen to be about 60% of replacement level
    QB <- 200
    WR1 <- 0
    WR2 <- 0
    RB1 <- 0
    RB2 <- 0
    TE <- 140
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

plot(1:12, potential_points/simulations,col="blue",xlab="Draft Position",ylab="Average Points")
