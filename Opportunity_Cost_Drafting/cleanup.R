AAV_data <- read.csv("D:/Data_Jocks_Projects/Opportunity_Cost_Drafting/AAv_Data.csv",stringsAsFactors = FALSE)

#Clean up the AAV data
for(i in 1:length(AAV_data[,1])){
  name <- AAV_data[i,"Name"]
  name <- gsub("'","",name)
  name <- gsub("'","",name)
  name <- tolower(name)
  AAV_data[i,"Name"]<-name
}

#add AAV column to the ADP frame 

ADP <- read.csv("D:/Data_Jocks_Projects/ADP_v_Production/Data/2019_ADP_New.csv",stringsAsFactors = FALSE)
AAV <- rep(0, length(ADP[,"Name"]))

for(i in 1:length(ADP[,"Name"])){
  player <- ADP[i,"Name"]
  #Find player in AAV frame
  for(j in 1:length(AAV_data[,"Name"])){
    if(AAV_data[j,"Name"]==player){
      #found player, add auction value to frame
      AAV[i] <- AAV_data[j,"AAV"]
      break
    }
  }
}
ADP$AAV <- AAV
write.csv(ADP,"D:/Data_Jocks_Projects/Opportunity_Cost_Drafting/2019_ADP.csv",row.names=FALSE)