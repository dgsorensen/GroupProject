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

lSummaryStats <- list()


dfCombinedRecruits <- getCombinedRecruits() #Get all of the recruits that were scraped from the ESPN site from 2007-2013
dfCombinedPlayers  <- getCombinedPlayers()  #Get all of the players listed on the stats spreadsheet from the same period

#Merge using Full name and Home State giving us the ID so we can link the recruit to the stats
dfRecruits<- merge(dfCombinedPlayers , dfCombinedRecruits, by=c("Full.Name", "Home.Town", "Home.State")) 


rm(dfCombinedPlayers,dfCombinedRecruits) #Free memory
dfRecruitStats <- getRecruitStats(dfRecruits)    #Get the points per game for each player in every game/season


#Format stats to merge: create total points and years played
dfRecruitStats<- group_by(dfRecruitStats, Player.Code)
dfPoints <- summarize(dfRecruitStats, Total.Points = sum(totalPoints),
                       Years.Played = n())
                       
#Merge the player with the stats
dfRecruitCareer <- merge(dfRecruits, dfPoints, by = ("Player.Code"))
#Add a new "points per year" column
dfRecruitCareer$Points.Per.Year <- 
    ceiling(dfRecruitCareer$Total.Points / dfRecruitCareer$Years.Played)

#Drop unnecessary columns and make other columns more readable
dfRecruitCareer <- subset(dfRecruitCareer[,-c(3,6,7)])
names(dfRecruitCareer) <- c("PlayerCode", "Name","HomeState",
                            "RecruitingRank","Position","RecruitingGrade",
                            "YearRanked", "TotalPoints", "YearsPlayed", "PointsPerYear")
                  
                                                                                                                     
rm(dfRecruits, dfPoints, dfRecruitStats) #free memory


dfClass07 <- subset(dfRecruitCareer, YearRanked == 2007)
dfClass08 <- subset(dfRecruitCareer, YearRanked == 2008)
dfClass09 <- subset(dfRecruitCareer, YearRanked == 2009)
dfClass10 <- subset(dfRecruitCareer, YearRanked == 2010)
dfClass11 <- subset(dfRecruitCareer, YearRanked == 2011)
dfClass12 <- subset(dfRecruitCareer, YearRanked == 2012)
dfClass13 <- subset(dfRecruitCareer, YearRanked == 2013)




