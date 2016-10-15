
#Load libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))
library(grid)
library(reshape2)
library(scales)

#Read the game stat files, calculate points per year for each player, return just
#the stats for the recruits
getRecruitStats<- function(dfRecruits){
  
  #Read all of the game csv files and combine them
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder=TRUE) %dopar% {
    inFile <- paste("./Data/GameStats/",i,"player-game-statistics.csv", sep="")
    dfHold <- read.csv(inFile, stringsAsFactors = FALSE)
    dfHold$Year.Played <- i                    #add the year as a new column
    dfHold                 
  }
  #Remove stats that don't apply to recruits
  filters <- which(df$Player.Code %in% dfRecruits$Player.Code)
  df<- subset(df[filters,])
  
  
  df$Game.Code <- factor(df$Game.Code) #show the actual game code (instead of 12E14)
  df <- df[, c(1,2,4,5,8,9,10,13,14,16,17,19,20,28,29,35,59)] #keep the columns we care about 
  hold <- df$Game.Code #Factor can't be multiplied against, so hold keeps it until after the calculations
  
  #vector to hold the point per attribute
  pointVector <- c( 1,  .1,  #PlayerCode(ignored*1) , Rushing Yard (1 pt per 10 yards)
                    6,  .04, #Rushing TD(6 pts), Passing Yard (1 pt per 25 yards)
                    4,  -2,  #Passing TD (4 pts), Passing Int (-2 points)
                    .1, 6,   #Receiving Yard (1 pt per 10 yards), Receiving TD(6 pts)
                    .2, 6,   #Kickoff Return Yard(1 pt per 5 yards), kickoff return td(6 points)
                    .2, 6,   #punt return yard(1 point per 5 years), punt return td(6 pts)
                    .2, 6,   #Misc return yard(1 point per 5 yards), misc return td(6 pts)
                    2,  1)   #2pt conversion made (2pts), Year (ignore * 1)
  
  df <- floor(data.frame(mapply("*" ,df[-2],pointVector))) #get total points per game, ignore the game code vector
  df$Game.Code <- hold #reassign the vector
  rm(hold)
  df <- group_by(df,Player.Code, Year.Played)  #get the points per year of each player
  df <- summarize(df,totalPoints = sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD, Pass.Int,
                                       Rec.Yards, Rec.TD,Kickoff.Ret.Yard,Kickoff.Ret.TD,Punt.Ret.Yard,
                                       Misc.Ret.Yard, Misc.Ret.TD,Off.2XP.Made))
  
  df <- df[with(df,order(-totalPoints)), ] #sort by totalpoints descending
    return(df)
  
}

#Get the recruiting data and remove duplicates, keeping most recent appearance
getCombinedRecruits<- function(){
  
  #Read all of the recruiting csv files and combine them
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder = TRUE) %dopar% {
    inFile <- paste("./Data/PlayerRankings/",i,"CFBPlayerRankings.csv", sep="")
    dfHold<- read.csv(inFile, stringsAsFactors = FALSE)
    dfHold$Year.Ranked <- i                           #add the year as a new column
    dfHold
  }
  
  df$Grade[df$Grade == "NA" | df$Grade == "NR"] <- 49
  
  #Get rid of duplicate recruits, keeping the most recent (the JUCO problem)
  df <- df[order(-df$Year.Ranked), ]
  df <- df[!duplicated(c(df$Full.Name, df$Home.Town,df$HomeState),
                         fromLast = TRUE), ] #remove the duplicates
  
  
  return(df)
  
}

#Get all of the players by reading the player files for each year,
#remove duplicates, and delete unnecessary columns
getCombinedPlayers <- function() {
  
  #Read all of the player csv files and combine them
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder = TRUE) %dopar% {
            inFile <- paste("./Data/PlayerInfo/",i,"player.csv", sep="")
            dfHold <- read.csv(inFile, stringsAsFactors = FALSE)
            dfHold$Full.Name <- paste(dfHold$First.Name, dfHold$Last.Name, sep = " ")
            dfHold$Year.Rostered <- i                     #add the year as a new column
            dfHold
        } 
  
  df <- subset(df,First.Name != "TEAM")

  #Get rid of duplicate players, and keep the most recent appearance
  df <- df[order(-df$Year.Rostered), ]
  df <- df[!duplicated(df$Player.Code), ]
  
  #Get rid of unnecessary columns
  df <- subset(df[, c(1,10,11,14)])
  
  return(df)
  
  
}




