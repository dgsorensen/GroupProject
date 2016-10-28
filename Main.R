#-Clear work area
rm(list = ls()) 

#-Get work directory
getwd()


#Load libraries
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))
library(ggplot2)
library(scales)


#-Register Parallel for faster looping
numCores <- detectCores()
cl <- makeCluster(numCores - 1)
registerDoParallel(cl)


#-Check for recruit ranking files.  Rescrape files if not found
source("./R/Functions.R")
if(!(file.exists("./Data/PlayerRankings/2007CFBPlayerRankings.csv"))){
  #Commented to prevent scraping, scraping takes over an hour
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

dfRecruitsTemp <- subset(dfRecruits[,c(1,4,9,12,13)])
rm(dfCombinedPlayers,dfCombinedRecruits) 

#-Get the points per year for each recruit by reading the game files
dfRecruitStats <- getRecruitStats(dfRecruits)    
#-Stop the cluster.  CSV reading is finished
stopCluster(cl)

#-Hold the yearly stats for later use
dfYearlyStats <- dfRecruitStats
dfYearlyStats <- merge(dfRecruitsTemp,dfYearlyStats, by = ("playerCode"))
dfYearlyStats <- subset(dfYearlyStats,position %in% c("RB","WR","QB","TE","ATH","FB"))

dfYearlyStats <- dfYearlyStats %>% arrange(yearPlayed, origOverallRanking) %>% 
  group_by(yearPlayed) %>% mutate(yearlyOrigOverallRank = row_number())

dfYearlyStats <- dfYearlyStats %>% arrange(yearPlayed, origPosRanking) %>% 
  group_by(yearPlayed, position) %>% mutate(yearlyOrigPosRank = row_number())

dfYearlyStats <- dfYearlyStats %>% arrange(yearPlayed, -pointsInYear) %>% 
  group_by(yearPlayed) %>% mutate(yearlyOverallRank = row_number())
dfYearlyStats <- dfYearlyStats %>% arrange(yearPlayed, -pointsInYear) %>% 
  group_by(yearPlayed, position) %>% mutate(yearlyPositionRank = row_number())

dfYearlyStats$positionRankingVariance <- dfYearlyStats$yearlyOrigPosRank -
  dfYearlyStats$yearlyPositionRank

dfYearlyStats$positionRankingDifference<- abs(dfYearlyStats$positionRankingVariance)

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
                            "recruitingRank", "position","recruitingGrade",
                            "yearRanked", "origOverallRanking","origPositionRank", 
                            "totalPoints", "yearsPlayed", "avgPointsPerYear")
dfRecruitCareer$position <- factor(dfRecruitCareer$position)
dfRecruitCareer$yearRostered <- factor(dfRecruitCareer$yearRostered)
dfRecruitCareer$yearRanked <- factor(dfRecruitCareer$yearRanked)

rm(dfRecruits, dfPoints, dfRecruitStats) #free memory

#-Remove recruits that aren't offensive players
dfRecruitCareer <- subset(dfRecruitCareer, position %in% c("RB","WR","QB","TE","ATH","FB"))

#-New column for adjusted overall ranking by year (just offensive skill positions)
dfRecruitCareer <- dfRecruitCareer %>% arrange(yearRanked, origOverallRanking) %>% 
  group_by(yearRanked) %>% mutate(adjOverallRank= row_number())

#-New column for adjusted position ranking by year (just offensive skill positions)
dfRecruitCareer <- dfRecruitCareer %>% arrange(yearRanked, origOverallRanking) %>% 
  group_by(yearRanked, position) %>% mutate(adjPositionRank = row_number())

#-New column for overall ranking (for year)
dfRecruitCareer <- dfRecruitCareer %>% arrange(yearRanked, -avgPointsPerYear) %>% 
  group_by(yearRanked) %>% mutate(newOverallRank = row_number())

#-New column for position ranking (for year)
dfRecruitCareer <- dfRecruitCareer %>% arrange(yearRanked, -avgPointsPerYear) %>% 
  group_by(yearRanked, position) %>% mutate(newPositionRank= row_number())

#-New column for variance between new and original position ranking
dfRecruitCareer$positionRankingVariance <- dfRecruitCareer$adjPositionRank -
                                                    dfRecruitCareer$newPositionRank

#-New column for difference between new and original position ranking
dfRecruitCareer$positionRankingDifference<- abs(dfRecruitCareer$positionRankingVariance)

#create scatter and histogram plots by position and year based on rankings and variance
#createYearlyPlots()

#Create scatter and histogram plots for the overall data set (i.e career)
#createCareerPlots()

plotMeanDifference()



