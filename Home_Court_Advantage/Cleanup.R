#convert the dataframe into a more useable format
#Extract and assign team IDs
data <- read.csv("D:/Projects/CAVS/Scripts/Data/NBA_Games_2012_2018.csv",stringsAsFactors = FALSE)

names <- unique(data[,"Team"])
IDs <- data.frame("ID"=1:30,"Team"=names)

write.csv(IDs,"D:/Data_Jocks_Projects/Home_Court_Advantage/IDs.csv",row.names=FALSE)