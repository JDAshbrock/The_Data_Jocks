library('MASS')
games<-read.csv(file='D:/Projects/CAVS/Scripts/Data/2019_2020_NBA_Data.csv')
IDs <- read.csv(file='D:/Projects/CAVS/Scripts/Data/Team_IDs.csv')


home_advantage <- 3 #number of points to give to the home team
weight <-3
bonus_games <- 200 #number of games to count double. 100 means 
#roughly each team's previous 6 games count double

System <- matrix(0,nrow=31,ncol=30)
Recent_System <- matrix(0, nrow=31,ncol=30)
x <- matrix(0, nrow=31,ncol=1)
Recent_x <- matrix(0, nrow=31, ncol=1)
num_teams <- length(IDs[,1])
num_games <-0
for(i in 1:length(games[,1])){
  if(is.na(games[i,"PTS"])){
    break
  }else{
    num_games <- num_games+1
  }
}
#initiate each team to "average"
mu <- rep(100,num_teams)
mu_recent<-rep(100,num_teams)


# Vectors which count cumulative prediction accuracy
Prediction_Accuracy <- data.frame("Date"=games[1,"Date"],"Games_Played" =0,"CAVS_Correct"=0,"CAVS_Percent"=0, "Ensemble_Recent_Correct"=0,"Ensemble_Recent_Percent"=0)

cur_day <- games[1,"Date"]
#The final row of the system matrix is constant and ensures 100 average rating
for(i in 1:num_teams){
  System[num_teams+1,i]=1/num_teams
  Recent_System[num_teams+1,i]=1/num_teams
}
x[num_teams+1]=100
Recent_x[num_teams+1]=100
#Now, go through each game and update the matrix "System" given the game data
i<-1
day_num<-1
while(i<= num_games && !is.na(games[i,"PTS"])){
  # We must break the games log into sets of days, short-circuit operator prevents error in 2nd arg
  Prediction_Accuracy[day_num,]=list(cur_day,0,0,0,0,0)
  if(day_num>1){
    Prediction_Accuracy[day_num,"Games_Played"]<-Prediction_Accuracy[day_num-1,"Games_Played"]
    Prediction_Accuracy[day_num,"CAVS_Correct"]<-Prediction_Accuracy[day_num-1,"CAVS_Correct"]
    Prediction_Accuracy[day_num,"Ensemble_Recent_Correct"]<-Prediction_Accuracy[day_num-1,"Ensemble_Recent_Correct"]
  }
  #Reset Recent System to be equal to the non altered system
  while(i<=num_games && games[i,"Date"]==cur_day && !is.na(games[i,"PTS"])){
    #Find HOME and VISITOR ID's
    Prediction_Accuracy[day_num,"Games_Played"]<-Prediction_Accuracy[day_num,"Games_Played"]+1
    for(j in 1:num_teams){
      if(IDs[j,"Name"]==games[i,"Home.Neutral"]){
        home_id <- j
      }
      if(IDs[j,"Name"]==games[i,"Visitor.Neutral"]){
        vis_id <- j
      }
    }#End Finding ID's
    #FIRST, predict winners using Ensemble and recent and check
    winner_id <- home_id
    if(games[i,"PTS"]>games[i,"PTS.1"]){
      winner_id <- vis_id
    }
    #mu is the CAVS rankings
    if(mu[vis_id]>home_advantage+mu[home_id]){
      CAVS_pred <- vis_id
    }else{
      CAVS_pred <- home_id
    }
    #mu_recent is the ensemble recent ranking
    if(mu_recent[vis_id]>home_advantage+mu_recent[home_id]){
      Ensemble_Recent_pred <- vis_id
    }else{
      Ensemble_Recent_pred <- home_id
    }
    #Check if predictions are correct
    if(CAVS_pred == winner_id){
      Prediction_Accuracy[day_num,"CAVS_Correct"]<-Prediction_Accuracy[day_num,"CAVS_Correct"]+1
    }
    if(Ensemble_Recent_pred == winner_id){
      Prediction_Accuracy[day_num, "Ensemble_Recent_Correct"]<-Prediction_Accuracy[day_num,"Ensemble_Recent_Correct"]+1
    }
    #Now, update System Matrix for ensemble/recent and margin vector
    System[home_id,home_id]<-System[home_id,home_id]+1
    System[vis_id,vis_id]<-System[vis_id,vis_id]+1
    System[home_id,vis_id]<-System[home_id,vis_id]-1
    System[vis_id,home_id]<-System[vis_id,home_id]-1
    x[home_id]<-x[home_id]+games[i,"PTS.1"]-games[i,"PTS"]-home_advantage
    x[vis_id]<-x[vis_id]+games[i,"PTS"]-games[i,"PTS.1"]+home_advantage
    #increment row index
    i<- i+1
  }#END CUR DAY LOOP
  #add extra weight to generate recent rankings
  Recent_System <- System
  Recent_x <- x
  for(k in 1:bonus_games){
    #if not a valid game row, pass
    if (i-k+1 <1 || is.na(games[i-k+1,"PTS"])){
      next
    }
    #grab home/away ID's
    for(j in 1:num_teams){
      if(IDs[j,"Name"]==games[i-k+1,"Home.Neutral"]){
        home_id <- j
      }
      if(IDs[j,"Name"]==games[i-k+1,"Visitor.Neutral"]){
        vis_id <- j
      }
    }#END Finding ID's
    Recent_System[home_id,home_id]<-Recent_System[home_id,home_id]+(weight-1)
    Recent_System[vis_id,vis_id]<-Recent_System[vis_id,vis_id]+(weight-1)
    Recent_System[home_id,vis_id]<-Recent_System[home_id,vis_id]-(weight-1)
    Recent_System[vis_id,home_id]<-Recent_System[vis_id,home_id]-(weight-1)
    Recent_x[home_id]<-Recent_x[home_id]+(weight-1)*(games[i-k+1,"PTS.1"]-games[i-k+1,"PTS"]-home_advantage)
    Recent_x[vis_id]<-Recent_x[vis_id]+(weight-1)*(games[i-k+1,"PTS"]-games[i-k+1,"PTS.1"]+home_advantage)
  }#End adding extra weight
  if(i <= num_games){
    #if their are still more games to process
    cur_day <- games[i,"Date"]
  }
  #After each day, we may update our CAVS rating
  mu<-ginv(System)%*%x
  mu_recent <- ginv(Recent_System)%*%Recent_x
  #increment day number and compute cumulative accuracy pcts
  Prediction_Accuracy[day_num,"CAVS_Percent"]<-Prediction_Accuracy[day_num,"CAVS_Correct"]/Prediction_Accuracy[day_num,"Games_Played"]
  Prediction_Accuracy[day_num,"Ensemble_Recent_Percent"]<-Prediction_Accuracy[day_num,"Ensemble_Recent_Correct"]/Prediction_Accuracy[day_num,"Games_Played"]
  day_num<-day_num+1
}# End outer while

Ratings <- data.frame("Team"=IDs$Name)
Ratings$Rating <- mu_recent

Ratings <- Ratings[order(Ratings$Rating,decreasing=TRUE),]
Ratings$Ranking <- 1:30
Ratings <- Ratings[ ,c(3,1,2)]
write.csv(Ratings, file=paste("D:/Data_Jocks_Projects/NBA_Conference_Strength/Ensemble_Recent_",bonus_games,"_",weight,".csv", sep=""),row.names=FALSE)


