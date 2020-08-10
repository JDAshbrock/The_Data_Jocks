hitter_outcome <- function(spot,frame){
  #function is input a frame with the lineup and probablity of walk
  #single, double, triple, HR as columns. Returns a string with the outcome
  rv <- runif(1)
  if(rv<frame[spot, "BB"]){
    return("BB")
  }else if(rv <(frame[spot,"BB"]+frame[spot,"Single"])){
    return("Single")
  }else if(rv < (frame[spot,"BB"]+frame[spot,"Single"]+frame[spot,"Double"])){
    return("Double")
  }else if(rv < (frame[spot,"BB"]+frame[spot,"Single"]+frame[spot,"Double"]+frame[spot,"Triple"])){
    return("Triple")
  }else if(rv < (frame[spot,"BB"]+frame[spot,"Single"]+frame[spot,"Double"]+frame[spot,"Triple"]+frame[spot,"HR"])){
    return("HR")
  }else{
    return("Out")
  }
}

advance <- function(base_state, hit_type){
  #function returns the number of runs scored and the resulting base state
  #depending on the current info. The base state is a boolean triple representing runners on 1,2, or 3
  if(hit_type == "HR"){
    runs <- sum(base_state)+1
    base_state <- c(0,0,0)
  }else if(hit_type == "Triple"){
    runs <- sum(base_state)
    base_state <- c(0,0,1)
  }else if(hit_type == "Double"){
    runs <- base_state[2]+base_state[3]
    rv <- runif(1)
    if(rv < 0.4){
      #runner on 1st would score
      runs <- runs + base_state[1]
      base_state <- c(0,1,0)
    }else{
      base_state <- c(0, 1, base_state[1])
    }
  }else if(hit_type == "Single"){
    runs <- base_state[3]
    rv1 <- runif(1)
    if(rv1 <0.6){
      #the runner potentially on 2nd scores
      runs <- runs + base_state[2]
      base_state[2]<-0
      rv2 <- runif(1)
      if(rv2 <0.6){
        #runner potentially on 1st only advances to 2nd
        base_state[2] <- base_state[1]
        base_state[1] <- 0
      }else{
        #runner potentially on 1st advances to 3rd
        base_state[3] <- base_state[1]
        base_state[2] <- 0
      }
    }else{
      #runner who may be on 2nd only advanced to 3rd
      base_state[3] <- base_state[2]
      base_state[2] <- base_state[1]
    }
    base_state[1] <- 1
  }else if(hit_type == "BB"){
    base_state[1] <- base_state[1] +1
    if(base_state[1]>1){
      base_state[1] <- 1
      base_state[2] <- base_state[2]+1
      if(base_state[2]>1){
        base_state[2]<-1
        base_state[3] <- base_state[3]+1
        if(base_state[3] >1){
          base_state[3] <- 1
          runs <- 1
        }
      }
    }
  }
  return(list(runs, base_state))
}

data <- read.csv("D:/Data_Jocks_Projects/Batting_Order_Irrelevant/Angels_Stats.csv",stringsAsFactors = FALSE)
seasons <- 100
#simulations <- 20000
simulations <- seasons*162
total_runs <- 0
RBI <- rep(0, 9) #will count RBIS by position


#start by turning data into probabilities
for(i in 1:length(data[,1])){
  data[i,"BB"]<- (data[i,"BB"]+data[i,"IBB"]+data[i,"HBP"])/data[i,"PA"]
  data[i,"Single"]<-data[i,"Single"]/data[i,"PA"]
  data[i,"Double"]<-data[i,"Double"]/data[i,"PA"]
  data[i,"Triple"]<-data[i,"Triple"]/data[i,"PA"]
  data[i,"HR"]<-data[i,"HR"]/data[i,"PA"]
}

#order <- 1:9
#order <- c(1,2,3,4,5,6,7,8,9)
order <- 9:1
#order <- c(2,3,6,8,7,5,4,1,9) #best to worst
#order <- c(6,3,2,8,7,5,4,1,9) #Pyramid order
data <- data[order,]
season_RBI <- rep(0, seasons)
season_AB <- rep(0, seasons)
for(i in 1:simulations){
  at_bat <- 1
  runs <- 0
  inning <- 1
  cur_season <- floor((i-1)/162)+1
  #order <- sample(1:9,9, FALSE)
  #data <- data[order,]
  while(inning <10){
    bases <- c(0,0,0)
    outs <- 0
    inning_runs <- 0
    while(outs <3){
      if(at_bat ==5){
        season_AB[cur_season]<- season_AB[cur_season]+1
      }
      hit <- hitter_outcome(at_bat, data)
      if(hit == "Out"){
        outs <- outs+1
        batter_RBI <- 0
      }else{
        outcome <- advance(bases, hit) #returns a list with runs scored and new base state
        batter_RBI <- unlist(outcome[1])
        inning_runs <- inning_runs+batter_RBI
        bases <- unlist(outcome[2])
      }
      RBI[at_bat]<- RBI[at_bat]+batter_RBI
      if(at_bat == 5){
        season_RBI[cur_season]<- season_RBI[cur_season]+batter_RBI
      }
      at_bat <- (at_bat)%%9 +1
    }#END inning
    inning <- inning+1
    total_runs <- total_runs + inning_runs
  }
}
total_runs/simulations
RBI <- 162*RBI/simulations