library('MASS') #Contains script for Moore-Penrose inversion of non-invertible matrix
data <- read.csv("D:/Projects/CAVS/Scripts/Data/2019_2020_NBA_Data.csv",stringsAsFactors = FALSE)
IDs <- read.csv("D:/PRojects/CAVS/SCripts/Data/Team_IDs.csv",stringsAsFactors = FALSE)
#Find number of games w/ score
for(i in 1:length(data[,1])){
  if(is.na(data[i,"PTS"])){
    num_games <- i-1
    break
  }
}
num_teams <- length(IDs[,1])


System <- matrix(0,nrow=2*(num_teams)+1,ncol=2*num_teams)
rhs <- rep(0, 2*num_teams+1)

#The last row of the System forces average rating = 100
for(i in 1:num_teams){
  System[2*num_teams+1,i]<- 1/num_teams
}
rhs[2*num_teams+1]<-100

for(i in 1:num_games){
  #Get teams id's
  away_id <- IDs[IDs[ ,"Name"]==data[i,"Visitor.Neutral"] ,"ID"]
  home_id <- IDs[IDs[ ,"Name"]==data[i,"Home.Neutral"] ,"ID"]
  home_margin <- data[i,"PTS.1"]-data[i,"PTS"]
  #Update System
  System[home_id,home_id]<-System[home_id,home_id]-1
  System[home_id,away_id]<-System[home_id,away_id]+1
  #home_id+num_teams is col for home team HCA param
  System[home_id,home_id+num_teams]<-System[home_id,home_id+num_teams]-1
  
  System[away_id,away_id]<-System[away_id,away_id]-1
  System[away_id,home_id]<-System[away_id,home_id]+1
  System[away_id,home_id+num_teams]<-System[away_id,home_id+num_teams]+1
  
  System[home_id+num_teams,home_id+num_teams]<-System[home_id+num_teams,home_id+num_teams]-1
  System[home_id+num_teams,home_id]<-System[home_id+num_teams,home_id]-1
  System[home_id+num_teams,away_id]<-System[home_id+num_teams,away_id]+1
  
  rhs[home_id]<- rhs[home_id]-home_margin
  rhs[home_id+num_teams]<-rhs[home_id+num_teams]-home_margin
  rhs[away_id]<-rhs[away_id]+home_margin
}

sol <- ginv(System)%*%rhs
mu <- sol[1:num_teams]
HCA <- sol[(num_teams+1):(2*num_teams)]

HCA_data <- data.frame('Team'=IDs[,"Name"],'HCA'=HCA)
HCA_data <- HCA_data[order(HCA_data[,'HCA']),]

ratings <- data.frame('Team'=IDs[,"Name"],"Rating"=mu)
ratings <- ratings[order(-ratings[,'Rating']),]
write.csv(ratings,'D:/Data_Jocks_Projects/NBA_Champ_Prob/Variable_HCA_Mu.csv',row.names=FALSE)
