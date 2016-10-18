
#Load libraries
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))
library(ggplot2)
library(scales)

numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)


getRecruitStats<- function(dfRecruits){
  #Read all of the game csv files and combine them
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder=TRUE) %dopar% {
    inFile <- paste("./Data/GameStats/",i,"player-game-statistics.csv", sep="")
    dfHold <- read.csv(inFile, stringsAsFactors = FALSE)
    #-Add the year played as a new column
    dfHold$Year.Played <- i                  
    dfHold                 
  }
  
  
  
  #Remove stats that don't apply to recruits
  filters <- which(df$Player.Code %in% dfRecruits$playerCode)
  df<- subset(df[filters,])
  
  #-Show actual game code number instead of XY*E^Z
  df$Game.Code <- factor(df$Game.Code) 
  
  #-Remove irrelevant columns
  df <- df[, c(1,2,4,5,8,9,10,13,14,16,17,19,20,28,29,35,59)] 
  
  #-Cut game code temporarily from df - factors can't be multiplied
  hold <- df$Game.Code
  
  #-Vector to hold the point per stat
  pointVector <- c( 1,  .1,  #playerCode(ignore), RushYard (1p/10y)
                    6,  .04, #RushTD(6p), PassYard(1p/25y)
                    4,  -2,  #PassTD(6p), Interception(-2p)
                    .1, 6,   #RecYard(1p/10y), RecTD(6p)
                    .2, 6,   #KOReturnYard(1p/5y), KOReturnTD(6p)
                    .2, 6,   #PuntRetYard(1p/5y), PuntRetTD(6p)
                    .2, 6,   #MiscYard(1p/5y), MiscTD(6p)
                    2,  1)   #2PtConv(2p), Year(ignore)
  
  #-Multiply the games by the vectors to weight the points
  df <- floor(data.frame(mapply("*" ,df[-2],pointVector))) 
  #-Add Game Code back in
  df$Game.Code <- hold 
  rm(hold)
  
  #-Group and summarize to sum the points to get points in year
  df <- group_by(df,Player.Code, Year.Played) 
  df <- summarize(df,PointsInYear= sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD,
                                       Pass.Int, Rec.Yards, Rec.TD,
                                       Kickoff.Ret.Yard,Kickoff.Ret.TD,
                                       Punt.Ret.Yard,Misc.Ret.Yard, Misc.Ret.TD,
                                       Off.2XP.Made))
  
  #-Set names and order df by yearPlayed descending
  names(df) <- c("playerCode", "yearPlayed", "pointsInYear")
  df <- df[order(-df$yearPlayed), ]
  
  return(df)
  
}

#-Combine the recruit CSVs into a single dataframe
getCombinedRecruits<- function(){
  filePath <- "./Data/PlayerRankings/"
  #-Fetch recruiting csv for each year and combine into df
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder = TRUE) %dopar% {
    inFile <- paste(filePath, i, "CFBPlayerRankings.csv", sep = "")
    dfHold<- read.csv(inFile, stringsAsFactors = FALSE)
    #add the year ranked as a new column
    dfHold$Year.Ranked <- i                         
    dfHold
  }
  
  dfTemp <- df
  dfTemp$Year <- factor(dfTemp$Year.Ranked)
  
  dfTemp <- group_by(dfTemp, Year)
  dfTemp <- summarize(dfTemp, numPlayers = n())
  
  if(!(file.exists("./Plots/RecruitsCensus.png"))){
    p <- ggplot(dfTemp, aes(x=Year, y=numPlayers, fill = Year))+
      geom_bar(stat = "identity", color = "black")+
      labs(x = "Year", y = "Number of PRecruits", title = "Players per Year")+
      scale_y_continuous(labels = comma,
                         breaks = 1000 * c(8:19))+
      theme(panel.grid = element_blank(), panel.background = element_blank())+
      coord_cartesian(ylim = c(8000, 19000))+
      guides(fill=FALSE)
    
    
    ggsave(filename = "./Plots/RecruitCensus.png", plot = p, 
           width = 6, height = 4, dpi = 600)
    
    rm(dfTemp, p)
  }
  
  #-Grades go as low as 50, so unranked gets assigned 49
  df$Grade[df$Grade == "NA" | df$Grade == "NR"] <- 49
  
  #-Remove duplicates and keep the newest(solves the JUCO problem)
  df <- df[order(-df$Year.Ranked), ]
  df <- df[!duplicated(c(df$Full.Name, df$Home.Town, df$HomeState),
                       fromLast = TRUE), ] 
  
  names(df) <- c("rank", "fullName", "lastName","firstName","position",
                 "recruitingGrade","hometown", "homeState", "yearRanked")
  
  
  return(df)
  
}

#-Combine the player CSVs into a single dataframe
#-Remove player entries that don't correspond to a recruit
getCombinedPlayers <- function() {
  
  #-Fetch recruiting csv for each year and combine into df
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder = TRUE) %dopar% {
    inFile <- paste("./Data/PlayerInfo/", i, "player.csv", sep = "")
    dfHold <- read.csv(inFile, stringsAsFactors = FALSE)
    dfHold$Full.Name <- paste(dfHold$First.Name, dfHold$Last.Name, sep = " ")
    #-Add the rostered year as a new column
    dfHold$Year.Rostered <- i                  
    dfHold
  } 
  
  df <- subset(df,First.Name != "TEAM")
  
  dfTemp <- df
  dfTemp$Year <- factor(dfTemp$Year.Rostered)
  
  dfTemp <- group_by(dfTemp, Year)
  dfTemp <- summarize(dfTemp, numPlayers = n())
  
  if(!(file.exists("./Plots/PlayerCensus.png"))){
    p <- ggplot(dfTemp, aes(x=Year, y=numPlayers, fill = Year))+
      geom_bar(stat = "identity", color = "black")+
      labs(x = "Year", y = "Number of Players", title = "Players per Year")+
      scale_y_continuous(labels = comma,
                         breaks = 1000 * c(17:22))+
      theme(panel.grid = element_blank(), panel.background = element_blank())+
      coord_cartesian(ylim = c(17000, 22000))+
      guides(fill=FALSE)
    
    ggsave(filename = "./Plots/PlayerCensus.png", plot = p, 
           width = 6, height = 4, dpi = 600)
    
    rm(dfTemp, p)
  }
  
  
  #Get rid of duplicate players, and keep the most recent appearance
  df <- df[order(-df$Year.Rostered), ]
  df <- df[!duplicated(df$Player.Code), ]
  
  #Get rid of unnecessary columns
  df <- subset(df[, c(1,10,11,14,15)])
  names(df) <- c("playerCode","hometown","homeState","fullName","yearRostered")
  
  return(df)
  
}

analyzeSeason <- function(df, year) {
  df <- data.frame(class = year, jibber = 1)
  return(df)
}




