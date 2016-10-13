
#Load libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
library(grid)
library(reshape2)
library(scales)


getEligiblePlayers <- function( year ) {
  inFile <- paste("./Data/Active Players/",year,"ActivePlayers.csv", sep="")
  dfPlayers <- read.csv(inFile, stringsAsFactors = FALSE)
  dfPlayers <- subset(dfPlayers, Position %in% c("WR", #Wide Receiver
                                                 "QB", #Quarterback
                                                 "RB", #Runningback
                                                 "TE", #Tight End
                                                 "FB" )) #Fullback

  return(dfPlayers)
  
}

getSeasonStats <- function( year, df ) {

  
  inFile <- paste("./Data/Stats/Stats ", year, "/player-game-statistics.csv", sep = "")
  dfStats <- read.csv(inFile, stringsAsFactors = FALSE )

  dfStats<- dfStats[, c(1,2,4,5,8,9,10,13,14,16,17,19,20,28,29,35)] #keep the columns we care about
  pointVector <- c(1,.1, 6, .04, 4, -2, .1, 6, .2, 6, .2, 6, .2, 6, 2 ) #the points per yard/td, etc
  
  dfStats <- subset(dfStats, Player.Code %in% df$Player.Code)
  
  dfStats$Game.Code <- as.factor(dfStats$Game.Code) #show the actual game code (instead of 12E14)
  
  dfSeasonStats <- floor(data.frame(mapply("*" ,dfStats[-2],pointVector))) #get total points per game
  

  dfSeasonStats <- group_by(dfSeasonStats,Player.Code) #
  pointSummary <- summarize(dfSeasonStats, #add the points to gether to get the total points for each player for the season
                           totalPoints = sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD, Pass.Int,
                                             Rec.Yards, Rec.TD,Kickoff.Ret.Yard,Kickoff.Ret.TD,Punt.Ret.Yard,
                                             Misc.Ret.Yard, Misc.Ret.TD,Off.2XP.Made))
  
  pointSummary <- pointSummary[with(pointSummary,order(-totalPoints)), ]
  
  
  return(pointSummary)
                    
}



getCombinedStats<- function(){
  filePath <- "./Data/PlayerStats/"
  df <- read.csv("./Data/PlayerStats/2007player-game-statistics.csv", stringsAsFactors = FALSE)
  df$Year <- 2007

  
 #combine all of the player stats 
  for(i in 2008:2013){
    inFile <- paste(filePath,i,"player-game-statistics.csv", sep="")
    dfNew <- read.csv(inFile, stringsAsFactors = FALSE)
    dfNew$Year <- i                           #add the year as a new column
    dfTemp <- rbind(df, dfNew)                #combine the datasets
    df <- dfTemp                              #reassign to main df
    dfNew  <- dfNew[0, ]                      #clear out the work areas
    dfTemp <- dfTemp[0, ]
  
  }
  
  df$Game.Code <- factor(df$Game.Code) #show the actual game code (instead of 12E14)
  df <- df[, c(1,4,5,8,9,10,13,14,16,17,19,20,28,29,35,59)] #keep the columns we care about
  
  return(df)
  
}

getCombinedRankings<- function(){
  filePath <- "./Data/PlayerRankings/"
  df <- read.csv("./Data/PlayerRankings/2007CFBPlayerRankings.csv", stringsAsFactors = FALSE)
  df$Year <- 2007
  
  
  #combine all of the player stats 
  for(i in 2008:2013){
    inFile <- paste(filePath,i,"CFBPlayerRankings.csv", sep="")
    dfNew <- read.csv(inFile, stringsAsFactors = FALSE)
    dfNew$Year <- i                           #add the year as a new column
    dfTemp <- rbind(df, dfNew)                #combine the datasets
    df <- dfTemp                              #reassign to main df
    dfNew  <- dfNew[0, ]                      #clear out the work areas
    dfTemp <- dfTemp[0, ]
    
    
  }
  
  #df$Game.Code <- factor(df$Game.Code) #show the actual game code (instead of 12E14)
  #df <- df[, c(1,4,5,8,9,10,13,14,16,17,19,20,28,29,35,59)] #keep the columns we care about
  
  return(df)
  
}






  