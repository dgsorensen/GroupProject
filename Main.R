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


dfPlayerStats      <- getCombinedStats()    #Get the points per game for each player in every game/season
dfCombinedRankings <- getCombinedRankings() #Get all of the recruits that were scraped from the ESPN site from 2007-2013
dfCombinedPlayers  <- getCombinedPlayers()  #Get all of the players listed on the stats spreadsheet from the same period

#Get rid of duplicate recruits, keeping the most recent (the JUCO problem)
dfCombinedRankings <- dfCombinedRankings[order(-dfCombinedRankings$Year.Ranked), ]
dfCombinedRankings <- dfCombinedRankings[!duplicated(c(dfCombinedRankings$Full.Name,
                                                       dfCombinedRankings$Home.Town,dfCombinedRankings$HomeState),
                                                       fromLast = TRUE), ] #remove the duplicates

#Get rid of duplicate players unnecessary columns
dfCombinedPlayers <- dfCombinedPlayers[order(-dfCombinedPlayers$Year.Rostered), ]
dfCombinedPlayers <- dfCombinedPlayers[!duplicated(dfCombinedPlayers$Player.Code), ]
dfCombinedPlayers <- subset(dfCombinedPlayers[, c(1,10,11,14)])

#Merge using Full name and Home State giving us the ID so we can link the recruit to the stats
dfRecruits<- merge(dfCombinedPlayers , dfCombinedRankings, by=c("Full.Name", "Home.Town", "Home.State")) 


rm(dfCombinedPlayers,dfCombinedRankings) #Free memory

#Remove stats that don't apply to recruits
filters <- which(dfPlayerStats$Player.Code %in% dfRecruits$Player.Code)
dfPlayerStats <- subset(dfPlayerStats[filters,])

#Format stats to merge: create total points and years played
dfPlayerStats<- group_by(dfPlayerStats, Player.Code)
dfPoints <- summarize(dfPlayerStats, Total.Points = sum(totalPoints),
                       Years.Played = n())
                       
#Merge the player with the stats
dfRecruitCareer <- merge(dfRecruits, dfPoints, by = ("Player.Code"))
#Add a new "points per year" column
dfRecruitCareer$Points.Per.Year <- 
    ceiling(dfRecruitCareer$Total.Points / dfRecruitCareer$Years.Played)

#Drop unnecessary columns and make other columns more readable
dfRecruitCareer <- subset(dfRecruitCareer[,-c(3,6,7)])
names(dfRecruitCareer) <- c("PlayerCode", "Name","Home State",
                            "RecruitingRank","Position","RecruitingGrade",
                            "YearRanked", "TotalPoints", "YearsPlayed", "PointsPerYear")
                  
                                                                                                                     
rm(filters,dfRecruits, dfPoints, dfPlayerStats) #free memory


dfClass07 <- subset(dfRecruitCareer, YearRanked == 2007)
dfClass08 <- subset(dfRecruitCareer, YearRanked == 2008)
dfClass09 <- subset(dfRecruitCareer, YearRanked == 2009)
dfClass10 <- subset(dfRecruitCareer, YearRanked == 2010)
dfClass11 <- subset(dfRecruitCareer, YearRanked == 2011)
dfClass12 <- subset(dfRecruitCareer, YearRanked == 2012)
dfClass13 <- subset(dfRecruitCareer, YearRanked == 2013)




