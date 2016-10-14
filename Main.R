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




dfRawStats         <- getCombinedStats()    #Get the points per game for each player in every game/season
dfCombinedRankings <- getCombinedRankings() #Get all of the recruits that were scraped from the ESPN site from 2007-2013
dfCombinedPlayers  <- getCombinedPlayers()  #Get all of the players listed on the stats spreadsheet from the same period

#Delete the players that aren't part of the recruiting class
filters <- which(dfCombinedPlayers$Home.State %in% dfCombinedRankings$Home.State & 
                 dfCombinedPlayers$Home.Town %in% dfCombinedRankings$Home.Town & 
                 dfCombinedPlayers$Full.Name %in% dfCombinedRankings$Full.Name)
dfCombinedPlayers <- subset(dfCombinedPlayers[filters,])
dfCombinedPlayers <-subset(dfCombinedPlayers, !((Year.Active == 2007 | Year.Active == 2008) &
                             Class != "FR"))

#Remove duplicate recruits by sorting by year, and keeping the most recent entry
#Some players go to JUCO or other non FCS school and show up as a recruiter in 
#later years


dfCombinedRankings <- group_by(dfCombinedRankings, Full.Name, Home.State, Home.Town)
#This max Year gives us the last time the recruit showed up in a ranking report.  This is
#not unusual as some recruits attend JUCO or prep schools that don't affect eligibility
dfUniqueRecruits <- summarize(dfCombinedRankings, 
                               Year.Recruited = max(Year.Ranked))


#Total the points up to get the total points scored in the year
dfRawStats <- group_by(dfRawStats, Player.Code, Year.Stats)
dfSortedStats <- summarize(dfRawStats,
                           totalPoints = sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD, Pass.Int,
                                             Rec.Yards, Rec.TD,Kickoff.Ret.Yard,Kickoff.Ret.TD,Punt.Ret.Yard,
                                             Misc.Ret.Yard, Misc.Ret.TD,Off.2XP.Made))
rm(dfRawStats) #Free Memory




#Summarize the players so we can get the player ID from the stats to match the scraped recruiting data
dfCombinedPlayers <- group_by(dfCombinedPlayers, Player.Code, Full.Name, Home.Town, Home.State)
dfUniquePlayers <- summarize(dfCombinedPlayers)
#Merge using Full name and Home State giving us the ID

dfRecruits<- merge(dfUniquePlayers, dfCombinedRankings, by=c("Full.Name", "Home.Town", "Home.State"), all.x = FALSE) 


#dfRecruits <- merge(dfUniquePlayers, dfCombinedRankings)with(dfRecruits,dfRecruits[First.Year >= Year])

rm(dfUniquePlayers,dfCombinedPlayers) #Free memory

#Some players were listed on the recruiting pages on different years (going to JUCO, taking a year to play in FCS, etc), so we only
#want the latest year that they appeared in the recruiting rankings
dfRecruits <- dfRecruits[order(-dfRecruits$Year.Ranked), ] #Sort by year descending
dfRecruits <- dfRecruits[!duplicated(dfRecruits$Player.Code, fromLast = FALSE), ] #remove the duplicates


filters <- which(dfSortedStats$Player.Code %in% dfRecruits$Player.Code)
dfSortedStats <- subset(dfSortedStats[filters,])
dfSortedStats <- group_by(dfSortedStats, Player.Code)
dfPoints <- summarize(dfSortedStats, Total.Points = sum(totalPoints),
                   Year.Started = min(Year.Stats), Year.Ended = max(Year.Stats),
                   Years.Played = n())
#merge the 2 data frames to add the points and years played to each player's line
dfPlayerPoints <- merge(dfRecruits, dfPoints, by = ("Player.Code"))


                  
                                                                                                                     
#rm(dfPlayerPoints, dfSortedStats, dfRecruits)


dfClass07 <- subset(dfPlayerPoints, Year.Ranked == 2007)
dfClass08 <- subset(dfPlayerPoints, Year.Ranked == 2008)
dfClass09 <- subset(dfPlayerPoints, Year.Ranked == 2009)
dfClass10 <- subset(dfPlayerPoints, Year.Ranked == 2010)
dfClass11 <- subset(dfPlayerPoints, Year.Ranked == 2011)
dfClass12 <- subset(dfPlayerPoints, Year.Ranked == 2012)
dfClass13 <- subset(dfPlayerPoints, Year.Ranked == 2013)




