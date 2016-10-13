#Clear work area
rm(list = ls()) 

#Get work directory
getwd()

source("./R/Functions.R")
if(!(file.exists("./Data/PlayerRankings/2007CFBPlayerRankings.csv"))){
#This will rescrape the espn pages for recruiting info and will take forever
  #source("./R/RecruitingScraper.R")
 # getRecruits()
print("Data files not found.  Please set working directory to GroupProject")  
}


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
  
  pointSummary <- pointSummary[with(pointSummary,order(-totalPoints)), ] #sort by totalpoints descending
  
  
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
  filePath <- "./Data/GameStats/"
  df <- read.csv("./Data/GameStats/2007player-game-statistics.csv", stringsAsFactors = FALSE)
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
  df <- df[, c(1,2,4,5,8,9,10,13,14,16,17,19,20,28,29,35,59)] #keep the columns we care about 
  hold <- df$Game.Code #Factor can't be multiplied against, so hold keeps it until after the calculations
  
  #vector to hold the point per attribute
  pointVector <-
    c(  1,  .1,  #PlayerCode(ignored*1) , Rushing Yard (1 pt per 10 yards)
        6,  .04, #Rushing TD(6 pts), Passing Yard (1 pt per 25 yards)
        4,  -2,  #Passing TD (4 pts), Passing Int (-2 points)
        .1, 6,   #Receiving Yard (1 pt per 10 yards), Receiving TD(6 pts)
        .2, 6,   #Kickoff Return Yard(1 pt per 5 yards), kickoff return td(6 points)
        .2, 6,   #punt return yard(1 point per 5 years), punt return td(6 pts)
        .2, 6,   #Misc return yard(1 point per 5 yards), misc return td(6 pts)
        2,  1)   #2pt conversion made (2pts), Year (ignore * 1)
    
  df <- floor(data.frame(mapply("*" ,df[-2],pointVector))) #get total points per game, ignore the game code vector
  df$Game.Code <- hold #reassign the vector
  
  df <- group_by(df,Player.Code, Year) #
  pointSummary <- summarize(df, #add the points to gether to get the total points for each player for the season
                            totalPoints = sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD, Pass.Int,
                                              Rec.Yards, Rec.TD,Kickoff.Ret.Yard,Kickoff.Ret.TD,Punt.Ret.Yard,
                                              Misc.Ret.Yard, Misc.Ret.TD,Off.2XP.Made))
  
  pointSummary <- pointSummary[with(pointSummary,order(-totalPoints)), ] #sort by totalpoints descending
  
  
  
  
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
  
  df$Grade[df$Grade == "NA" | df$Grade == "NR"] <- 49
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
  filePath <- "./Data/PlayerInfo/"
  df <- read.csv("./Data/PlayerInfo/2007player.csv", stringsAsFactors = FALSE)
  df$Position <- NULL #remove position
  df$Full.Name <- paste(df$First.Name, df$Last.Name, sep = " ")
  df$Year <- 2007
  
  
  #combine all of the player stats 
  for(i in 2008:2013){
    inFile <- paste(filePath,i,"player.csv", sep="")
    dfNew <- read.csv(inFile, stringsAsFactors = FALSE)
    dfNew$Position <- NULL #remove position
    dfNew$Full.Name <- paste(dfNew$First.Name, dfNew$Last.Name, sep = " ")
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

#Get the points per game for each player in every game/season
dfRawStats <- getCombinedStats()

#Total the points up to get the total points scored in the year
dfRawStats <- group_by(dfRawStats, Player.Code, Year)
dfSortedStats <- summarize(dfRawStats,
                           totalPoints = sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD, Pass.Int,
                                             Rec.Yards, Rec.TD,Kickoff.Ret.Yard,Kickoff.Ret.TD,Punt.Ret.Yard,
                                             Misc.Ret.Yard, Misc.Ret.TD,Off.2XP.Made))
ungroup(dfRawStats)

dfCombinedRankings <- getCombinedRankings() #Get all of the recruits that were scraped from the ESPN site from 2007-2013
dfCombinedPlayers <- getCombinedPlayers() #Get all of the players listed on the stats spreadsheet from the same period

#Summarize the players so we can get the player ID from the stats to match the scraped recruiting data
dfCombinedPlayers <- group_by(dfCombinedPlayers, Player.Code, Full.Name, Home.State)
dfUniquePlayers <- summarize(dfCombinedPlayers)

#Merge using Full name and Home State giving us the ID
dfRecruited <- merge(dfUniquePlayers, dfCombinedRankings, by=c("Full.Name", "Home.State"))

#Some players were listed on the recruiting pages on different years (going to JUCO, taking a year to play in FCS, etc), so we only
#want the latest year that they appeared in the recruiting rankings
dfRecruited <- dfRecruited[order(-dfRecruited$Year), ] #Sort by year descending
dfRecruited <- dfRecruited[!duplicated(dfRecruited$Player.Code), ] #remove the duplicates

#merge the 2 data frames to add the points and years played to each player's line
dfPlayerPoints <- merge(dfRecruited, dfSortedStats, by = ("Player.Code"))
names(dfPlayerPoints) <- c("Player.Code", "Full.Name", "Home.State", "Recruiting.Ranking", "Last.Name", "First.Name", "Position", 
                           "Recruiting.Grade", "Year.Recruited", "Year.Played", "Yearly.Points")

#Get the career summary of the players' info, their points, and how many years they played
dfPlayerPoints <- group_by(dfPlayerPoints,Player.Code, Full.Name, Recruiting.Ranking, Position, Recruiting.Grade, Year.Recruited)
dfPlayerCareer <- summarize(dfPlayerPoints, Total.Points = sum(Yearly.Points), Years.Played = n())



dfClass07 <- subset(dfPlayerCareer, Year.Recruited == 2007)
dfClass08 <- subset(dfPlayerCareer, Year.Recruited == 2008)
dfClass09 <- subset(dfPlayerCareer, Year.Recruited == 2009)
dfClass10 <- subset(dfPlayerCareer, Year.Recruited == 2010)
dfClass11 <- subset(dfPlayerCareer, Year.Recruited == 2011)
dfClass12 <- subset(dfPlayerCareer, Year.Recruited == 2012)
dfClass13 <- subset(dfPlayerCareer, Year.Recruited == 2013)




