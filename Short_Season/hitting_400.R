data <- read.csv('D:/Data_Jocks_Projects/Short_Season/batting_leaders_19.csv',stringsAsFactors = FALSE)

players <- 100
averages <- data[1:players,"BA."]
simulations <- 1000
season_length <- 55
at_bats <- 4.5*season_length

accumulated_max_average<-0
over_400_count <- 0

for(i in 1:simulations){
  max_average <- 0
  for(j in 1:players){
    average <- mean(rbinom(at_bats,1,averages[j]))
    max_average <- max(average, max_average)
  }
  if(max_average >0.399){
    over_400_count <- over_400_count+1
  }
  accumulated_max_average <- accumulated_max_average + max_average
}
average_max_average <- accumulated_max_average/simulations
over_400_prob <- over_400_count/simulations