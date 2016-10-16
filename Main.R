#-Clear work area
rm(list = ls()) 

#-Get work directory
getwd()
#-Check for recruit ranking files.  Rescrape files if not found
source("./R/Functions.R")
if(!(file.exists("./Data/PlayerRankings/2007CFBPlayerRankings.csv"))){
  #Commented to prevent scraping
  #source("./R/RecruitingScraper.R")
  # getRecruits()
  stop("Data Files not Found", call.=FALSE)
}

#------------------------------------------------------
#Get the recruit, player, and stat data by reading
# the csv files in the ./Data folder
#------------------------------------------------------


#-Get recruits from recruit files that were created from ESPN scraping
dfCombinedRecruits <- getCombinedRecruits() 

#-Get players from the player stats set that correspond to the recruits
dfCombinedPlayers  <- getCombinedPlayers() 

#-Attach the playerCode to the recruit to connect to stats
dfRecruits <- merge(dfCombinedPlayers , dfCombinedRecruits, 
                    by=c("fullName", "hometown", "homeState")) 


rm(dfCombinedPlayers,dfCombinedRecruits) 

#-Get the points per year for each recruit by reading the game files
dfRecruitStats <- getRecruitStats(dfRecruits)    


#-Format stats to merge: create total points and years played
dfRecruitStats <- group_by(dfRecruitStats, playerCode)
dfPoints <- summarize(dfRecruitStats, totalPoints = sum(pointsInYear),
                      yearsPlayed = n())

#Merge the player with the stats
dfRecruitCareer <- merge(dfRecruits, dfPoints, by = ("playerCode"))
#Add a new "points per year" column
dfRecruitCareer$pointsPerYear<- 
  ceiling(dfRecruitCareer$totalPoints / dfRecruitCareer$yearsPlayed)

#Drop unnecessary columns and make other columns more readable
dfRecruitCareer <- subset(dfRecruitCareer[,-c(3,7,8)])
names(dfRecruitCareer) <- c("playerCode", "name","homeState", "yearRostered",
                            "recruitingRank","dfRecruitCareer$Position","recruitingGrade",
                            "yearRanked", "totalPoints", "yearsPlayed", 
                            "avgPointsPerYear")


rm(dfRecruits, dfPoints, dfRecruitStats) #free memory


dfClass07 <- subset(dfRecruitCareer, yearRanked == 2007)
dfClass08 <- subset(dfRecruitCareer, yearRanked == 2008)
dfClass09 <- subset(dfRecruitCareer, yearRanked == 2009)
dfClass10 <- subset(dfRecruitCareer, yearRanked == 2010)
dfClass11 <- subset(dfRecruitCareer, yearRanked == 2011)
dfClass12 <- subset(dfRecruitCareer, yearRanked == 2012)
dfClass13 <- subset(dfRecruitCareer, yearRanked == 2013)






