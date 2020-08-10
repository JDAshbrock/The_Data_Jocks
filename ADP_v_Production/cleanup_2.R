#replace inconsistent player names
for(i in 2019:2007){
  handle <- paste('D:/Data_Jocks_Projects/ADP_v_Production/Data/',as.character(i),'_ADP_New.csv',sep="")
  adp <- read.csv(handle, stringsAsFactors = FALSE)
  for(j in 1:length(adp[,1])){
    player_name <- adp[j,"Name"]
    if(player_name=="pat mahomes"){
      adp[j,"Name"]<- "patrick mahomes"
    }
    if(player_name == "cj anderson"){
      adp[j,"Name"]<- "c.j. anderson"
    }
    if(player_name == "mitch trubisky"){
      adp[j,"Name"]<-"mitchell trubisky"
    }
    if(player_name == "ronald jones ii"){
      adp[j,"Name"]<-"ronald jones"
    }
    if(player_name == "odell beckham jr")
      adp[j,"Name"]<-"odell beckham"
  }
  write.csv(adp, handle, row.names=FALSE)
}