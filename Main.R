#Clear work area
rm(list = ls()) 

#Get work directory
getwd()

source("./R/Functions.R")
if(!(file.exists("./Data/PlayerRankings/2007CFBPlayerRankings.csv"))){
#This will rescrape the espn pages for recruiting info and will take forever
  #source("./R/RecruitingScraper.R")
 # getRecruits()
stop("Data Files not Found", call.=FALSE)
}



#Get the points per game for each player in every game/season
dfRawStats <- getCombinedStats()

#Total the points up to get the total points scored in the year
dfRawStats <- group_by(dfRawStats, Player.Code, Year)
dfSortedStats <- summarize(dfRawStats,
                           totalPoints = sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD, Pass.Int,
                                             Rec.Yards, Rec.TD,Kickoff.Ret.Yard,Kickoff.Ret.TD,Punt.Ret.Yard,
                                             Misc.Ret.Yard, Misc.Ret.TD,Off.2XP.Made))
rm(dfRawStats) #Free Memory


dfCombinedRankings <- getCombinedRankings() #Get all of the recruits that were scraped from the ESPN site from 2007-2013
dfCombinedPlayers <- getCombinedPlayers() #Get all of the players listed on the stats spreadsheet from the same period

#Summarize the players so we can get the player ID from the stats to match the scraped recruiting data
dfCombinedPlayers <- group_by(dfCombinedPlayers, Player.Code, Full.Name, Home.Town, Home.State)
dfUniquePlayers <- summarize(dfCombinedPlayers,
                             First.Year = min(Year))

#Merge using Full name and Home State giving us the ID

dfRecruits<- merge(dfUniquePlayers, dfCombinedRankings, by=c("Full.Name", "Home.Town", "Home.State"), all.x = FALSE) 


#dfRecruits <- merge(dfUniquePlayers, dfCombinedRankings)with(dfRecruits,dfRecruits[First.Year >= Year])

rm(dfUniquePlayers,dfCombinedPlayers) #Free memory

#Some players were listed on the recruiting pages on different years (going to JUCO, taking a year to play in FCS, etc), so we only
#want the latest year that they appeared in the recruiting rankings
dfRecruits <- dfRecruits[order(-dfRecruits$Year), ] #Sort by year descending
dfRecruits <- dfRecruits[!duplicated(dfRecruits$Player.Code), ] #remove the duplicates

#merge the 2 data frames to add the points and years played to each player's line
dfPlayerPoints <- merge(dfRecruits, dfSortedStats, by = ("Player.Code"), all.y=FALSE)
names(dfPlayerPoints) <- c("Player.Code", "Full.Name", "Home.Town", "Home.State", "First.Year", "Recruiting.Ranking", "Last.Name", "First.Name", "Position", 
                           "Recruiting.Grade", "Last.Year", "Year.Recruited", "Yearly.Points")

#Get the career summary of the players' info, their points, and how many years they played
dfPlayerPoints <- group_by(dfPlayerPoints,Player.Code, Full.Name, Recruiting.Ranking, Position, Recruiting.Grade, Year.Recruited)
dfPlayerCareer <- summarize(dfPlayerPoints, Total.Points = sum(Yearly.Points), Years.Played = n(), Last.Year = max(Years.Played))
#rm(dfPlayerPoints, dfSortedStats, dfRecruits)


dfClass07 <- subset(dfPlayerCareer, Year.Recruited == 2007)
dfClass08 <- subset(dfPlayerCareer, Year.Recruited == 2008)
dfClass09 <- subset(dfPlayerCareer, Year.Recruited == 2009)
dfClass10 <- subset(dfPlayerCareer, Year.Recruited == 2010)
dfClass11 <- subset(dfPlayerCareer, Year.Recruited == 2011)
dfClass12 <- subset(dfPlayerCareer, Year.Recruited == 2012)
dfClass13 <- subset(dfPlayerCareer, Year.Recruited == 2013)




