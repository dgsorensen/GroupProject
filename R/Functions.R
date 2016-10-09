rm(list = ls()) 

#Get work directory
getwd()

#Load libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
library(grid)
library(reshape2)
library(scales)

#formatSeasonStatsDf <- function(df, year ) {fg
  year = 2006
  inFile = paste("./Data/GameData/Stats ", year, "/player-game-statistics.csv", sep = "")
  dfStats <- read.csv(inFile, stringsAsFactors = FALSE )
  #cols <-  c(1,4,5,8,9,10,13,14,16,17,19,20,28,29,35)
  dfStats<- dfStats[, c(1,4,5,8,9,10,13,14,16,17,19,20,28,29,35)]
 names(dfStats) <- c("1", "2", "3", "4", "5", "6", "7", "8",
                      "9", "10", "12", "13", "14", "15")
 #  pointVector <- c(1, .1, 6, .04, 4, -2, .1, 6, .2, 6, .2, 6, .2, 6,)
  
  
  
#}