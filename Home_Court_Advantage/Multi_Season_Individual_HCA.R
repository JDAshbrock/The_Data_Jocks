library('MASS') #Contains script for Moore-Penrose inversion of non-invertible matrix
data <- read.csv("D:/Projects/CAVS/Scripts/Data/NBA_Games_2012_2018.csv",stringsAsFactors = FALSE)
IDs <- read.csv("D:/Data_Jocks_Projects/Home_Court_Advantage/IDs.csv",stringsAsFactors = FALSE)
#Find number of games w/ score
num_games <- length(data[,1])
for(i in 1:length(data[,1])){
  if(is.na(data[i,"TeamPoints"])){
    num_games <- i-1
    break
  }
}
num_teams <- length(IDs[,1])
num_seasons <-4
margin_counter <- 0
#One rating per season per team and one HCA per team

parameters <- (num_seasons+1)*(num_teams)
#last matrix row corresponding to a mu (mean) parameter
last_mu <- num_seasons*num_teams
System <- matrix(0,nrow=parameters,ncol=parameters)
rhs <- rep(0, parameters)


for(i in 1:num_games){
  #Skip away games else we double count
  if(data[i,"Home"]=="Away"){
    next
  }
  #Get season
  date <- data[i,"Date"]
  month <- substr(date,1,2)
  year <- as.numeric(substr(date,nchar(date)-1,nchar(date)))
  if(month ==10 || month == 11 || month==12){
    year <- year+1
  }
  seas_num <- year - 14 #14-15 season is declared season num 1
  #Get teams id's
  away_id <- IDs[IDs[ ,"Team"]==data[i,"Opponent"] ,"ID"]
  home_id <- IDs[IDs[ ,"Team"]==data[i,"Team"] ,"ID"]
  home_margin <- data[i,"TeamPoints"]-data[i,"OpponentPoints"]
  margin_counter <- margin_counter + 2*home_margin/num_games
  #Update System
  home_mu <- (seas_num-1)*num_teams + home_id
  away_mu <- (seas_num-1)*num_teams + away_id
  HCA_ID <- last_mu+home_id
  System[home_mu,home_mu]<-System[home_mu,home_mu]-1
  System[home_mu,away_mu]<-System[home_mu,away_mu]+1
  System[home_mu,HCA_ID]<-System[home_mu,HCA_ID]-1
  
  System[away_mu,away_mu]<-System[away_mu,away_mu]-1
  System[away_mu,home_mu]<-System[away_mu,home_mu]+1
  System[away_mu,HCA_ID]<-System[away_mu,HCA_ID]+1
  
  System[HCA_ID,HCA_ID]<-System[HCA_ID,HCA_ID]-1
  System[HCA_ID,home_mu]<-System[HCA_ID,home_mu]-1
  System[HCA_ID,away_mu]<-System[HCA_ID,away_mu]+1
  
  rhs[home_mu]<- rhs[home_mu]-home_margin
  rhs[HCA_ID]<-rhs[HCA_ID]-home_margin
  rhs[away_mu]<-rhs[away_mu]+home_margin
}
sol <- ginv(System)%*%rhs
HCA_data <- data.frame('Team'=IDs[,'Team'],'Home_Court_Advantage'=sol[121:150])
HCA_data <- HCA_data[order(HCA_data),]