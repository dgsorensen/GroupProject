
filePath <- "./Data/Stats/Stats "
df <- read.csv("./Data/Stats/Stats 2007/player.csv", stringsAsFactors = FALSE)
df$Year <- 2007


#combine all of the player stats 
for(i in 2008:2013){
  inFile <- paste(filePath,i,"/player.csv", sep="")
  dfNew <- read.csv(inFile, stringsAsFactors = FALSE)
  dfNew$Year <- i                           #add the year as a new column
  dfTemp <- rbind(df, dfNew)                #combine the datasets
  df <- dfTemp                              #reassign to main df
  dfNew  <- dfNew[0, ]                      #clear out the work areas
  dfTemp <- dfTemp[0, ]
  
}



filePath <- "./Data/PlayerRankings/"
df2 <- read.csv("./Data/PlayerRankings/2007CFBPlayerRankings.csv", stringsAsFactors = FALSE)
df$Year <- 2007


#combine all of the player stats 
for(i in 2008:2013){
  inFile <- paste(filePath,i,"CFBPlayerRankings.csv", sep="")
  dfNew <- read.csv(inFile, stringsAsFactors = FALSE)
  dfNew$Year <- i                           #add the year as a new column
  dfTemp <- rbind(df2, dfNew)                #combine the datasets
  df2 <- dfTemp                              #reassign to main df
  dfNew  <- dfNew[0, ]                      #clear out the work areas
  dfTemp <- dfTemp[0, ]
  
}


