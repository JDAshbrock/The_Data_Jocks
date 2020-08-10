col_names <- c("Name","Pos","Age","Games","Pass_Comp","Pass_Att", "Pass_Yards", "Pass_TD", "Pass_INT", "Rush_Att", "Rush_Yards", "Rush_TD","Rec_Tgt","Rec_Comp","Rec_Yards","Rec_TD","Fum_Lost","2PT_Made", "2PT_Pass", "Fan_Pts", "VBD","Pos_Rk")
cols <- c(2,4,5,6,8,9,10,11,12,13,14,16,17,18,19,21,23,25,26,27,31,32)
for(i in 2019:2007){
  stats <- read.csv(paste("D:/Data_Jocks_Projects/ADP_v_Production/Data/",as.character(i),"_Stats.csv",sep=""),stringsAsFactors = FALSE)
  num_rows = length(stats[,1])
  for(j in 2:num_rows){
    raw_name <- as.character(stats[j,2])
    sub_1 <- gsub("\\*","",raw_name)
    sub_2 <- gsub("\\+","",sub_1)
    sub_3 <- gsub("'","",sub_2)
    split <- unlist(strsplit(sub_3,"\\\\"))
    player_name <- split[1]
    player_name <- tolower(player_name)
    stats[j,2] <- player_name
  }
  stats <- stats[-1,cols]
  names(stats)<-col_names
  file_name <- paste(as.character(i),"_Stats_Clean.csv",sep="")
  write.csv(stats,paste("D:/Data_Jocks_Projects/ADP_v_Production/Data/",file_name,sep=""),row.names=FALSE)
}
#cleanup adp thing now by adding column for (#taken at position)
for(j in 2019:2007){
  RBs <-0
  WRs <-0
  TEs <-0
  QBs <-0
  PKs <-0
  DEFs <-0
  adp <- read.csv(paste("D:/Data_Jocks_Projects/ADP_v_Production/Data/",as.character(j),"_ADP.csv",sep=""),stringsAsFactors =FALSE)
  Pos_ADP <- rep(0,length(adp[,1]))
  for(i in 1:length(adp[,1])){
    if(adp[i,"Pos"]=="RB"){
      RBs <- RBs+1
      Pos_ADP[i]<-RBs
    }
    if(adp[i,"Pos"]=="TE"){
      TEs <- TEs+1
      Pos_ADP[i]<-TEs
    }
    if(adp[i,"Pos"]=="QB"){
      QBs <- QBs+1
      Pos_ADP[i]<-QBs
    }
    if(adp[i,"Pos"]=="WR"){
      WRs <- WRs+1
      Pos_ADP[i]<-WRs
    }
    if(adp[i,"Pos"]=="PK"){
      PKs <- PKs+1
      Pos_ADP[i]<-PKs
    }
    if(adp[i,"Pos"]=="DEF"){
      DEFs <- DEFs+1
      Pos_ADP[i]<-DEFs
    }
  }
  adp$Pos_ADP <- Pos_ADP
  adp<-adp[,-1]
  adp$Name <- tolower(adp$Name)
  adp$Name <- gsub("'","",adp$Name)
  write.csv(adp,paste("D:/Data_Jocks_Projects/ADP_v_Production/Data/",as.character(j),"_ADP_New.csv",sep=""),row.names =FALSE)
}
