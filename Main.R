rm(list = ls()) 

#Get work directory
getwd()

source("./R/Functions.R")

#Load libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
library(grid)
library(reshape2)
library(scales)




#el2006 <- getEligiblePlayers(2006)
df2006 <- getSeasonStats(2006)
df2007 <- getSeasonStats(2007)
df2008 <- getSeasonStats(2008)
df2009 <- getSeasonStats(2009)
df2010 <- getSeasonStats(2010)
df2011 <- getSeasonStats(2011)
df2012 <- getSeasonStats(2012)
