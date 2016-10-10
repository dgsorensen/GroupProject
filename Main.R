
rm(list = ls()) 

#Get work directory
getwd()



#Load libraries

#Load libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
library(grid)
library(reshape2)
library(scales)

###############################
#Temporarily placing functions here for easy testing.  They will eventually go into
#  /R/Functions.R
###############################

#' getSeasonStats
#' Return player points for a specific season
#' @param year 
#' @param df - dataframe for filtering players
#'
#' @return PointSummary
#' @export 
#'
#' @examples
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



#' getCombinedStats
#' Combine the total statistics of all players from 2007-2013 by combining
#' the csv files into a dataframe
#' @return df (all of the players who made a play in any season)
#' @export
#'
#' @examples
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

#' getCombinedRankings
#' Combine all player rankings (that were scraped from the espn recruiting site)
#' and return a list of all of the players who were ranked
#' @return df (all of the players)
#' @export
#'
#' @examples
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

#' getCombinedPlayers
#' Combine the player.csv file for each year to get every player from 2007-2013 regardless
#' of whether or not they ever played
#' @return
#' @export
#'
#' @examples
getCombinedPlayers <- function() {
  filePath <- "./Data/Stats/Stats "
  df <- read.csv("./Data/Stats/Stats 2007/player.csv", stringsAsFactors = FALSE)
  df$Position <- NULL #remove position
  df$Full.Name = paste(df$First.Name, df$LastName, sep = " ")
  df$Year <- 2007
  
  
  #combine all of the player stats 
  for(i in 2008:2013){
    inFile <- paste(filePath,i,"/player.csv", sep="")
    dfNew <- read.csv(inFile, stringsAsFactors = FALSE)
    dfNew$Position <- NULL #remove position
    dfNew$Full.Name = paste(dfNew$First.Name, dfNew$LastName, sep = " ")
    dfNew$Year <- i                           #add the year as a new column
    dfTemp <- rbind(df, dfNew)                #combine the datasets
    df <- dfTemp                              #reassign to main df
    dfNew  <- dfNew[0, ]                      #clear out the work areas
    dfTemp <- dfTemp[0, ]
    
  }
  
  return(df)
  
  
  
}
##########################################
#End of functions that will be moved
##########################################


dfCombinedStats <- getCombinedStats()
dfCombinedStats <- dfCombinedStats %>% group_by(Player.Code, Year) %>% summarise_each(funs(sum))

dfCombinedRankings <- getCombinedRankings()  #these are the players we're going to be ranking
dfCombinedPlayers <- getCombinedPlayers()
dfCombinedPlayers <- group_by(dfCombinedPlayers, Player.Code, Full.Name)
dfUniquePlayers <- summarize(dfCombinedPlayers)

df2007 <- getStatsOfClassRecruits(2007)
df2008 <- getStatsOfClassRecruits(2007)
df2009 <- getStatsOfClassRecruits(2007)
df2010 <- getStatsOfClassRecruits(2007)
df2011 <- getStatsOfClassRecruits(2007)
df2012 <- getStatsOfClassRecruits(2007)
df2013 <- getStatsOfClassRecruits(2007)
