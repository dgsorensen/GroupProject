
getRecruitStats<- function(dfRecruits){
  #Read all of the game csv files and combine them
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder=TRUE) %dopar% {
    inFile <- paste("./Data/GameStats/",i,"player-game-statistics.csv", sep="")
    dfHold <- read.csv(inFile, stringsAsFactors = FALSE)
    #-Add the year played as a new column
    dfHold$Year.Played <- i                  
    dfHold                 
  }
  
  #Remove stats that don't apply to recruits
  filters <- which(df$Player.Code %in% dfRecruits$playerCode)
  df<- subset(df[filters,])
  
  #-Show actual game code number instead of XY*E^Z
  df$Game.Code <- factor(df$Game.Code) 
  
  #-Remove irrelevant columns
  df <- df[, c(1,2,4,5,8,9,10,13,14,16,17,19,20,28,29,35,59)] 
  
  #-Cut game code temporarily from df - factors can't be multiplied
  hold <- df$Game.Code
  
  #-Vector to hold the point per stat
  pointVector <- c( 1,  .1,  #playerCode(ignore), RushYard (1p/10y)
                    6,  .04, #RushTD(6p), PassYard(1p/25y)
                    4,  -2,  #PassTD(6p), Interception(-2p)
                    .1, 6,   #RecYard(1p/10y), RecTD(6p)
                    .2, 6,   #KOReturnYard(1p/5y), KOReturnTD(6p)
                    .2, 6,   #PuntRetYard(1p/5y), PuntRetTD(6p)
                    .2, 6,   #MiscYard(1p/5y), MiscTD(6p)
                    2,  1)   #2PtConv(2p), Year(ignore)
  
  #-Multiply the games by the vectors to weight the points
  df <- floor(data.frame(mapply("*" ,df[-2],pointVector))) 
  #-Add Game Code back in
  df$Game.Code <- hold 
  rm(hold)
  
  #-Group and summarize to sum the points to get points in year
  df <- group_by(df,Player.Code, Year.Played) 
  df <- summarize(df,PointsInYear= sum(Rush.Yard, Rush.TD, Pass.Yard, Pass.TD,
                                       Pass.Int, Rec.Yards, Rec.TD,
                                       Kickoff.Ret.Yard,Kickoff.Ret.TD,
                                       Punt.Ret.Yard, Punt.Ret.TD, Misc.Ret.Yard, Misc.Ret.TD,
                                       Off.2XP.Made))
  
  #-Set names and order df by yearPlayed descending
  names(df) <- c("playerCode", "yearPlayed", "pointsInYear")
  df <- df[order(-df$yearPlayed), ]
  
  return(df)
  
}

#-Combine the recruit CSVs into a single dataframe
getCombinedRecruits<- function(){
  
  filePath <- "./Data/PlayerRankings/"
  #-Fetch recruiting csv for each year and combine into df
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder = TRUE) %dopar% {
    inFile <- paste(filePath, i, "CFBPlayerRankings.csv", sep = "")
    dfHold<- read.csv(inFile, stringsAsFactors = FALSE)
    #add the year ranked as a new column
    dfHold$Year.Ranked <- i 
    #-If a recruit isn't graded, we should assign them the ranking of the last graded player
    #-This prevents drastic differences in original vs final rankings
    x <- match(c("NR","NA"), dfHold$Grade) #Get the first instance of unranked
    dfHold$Rank[dfHold$Rank >= x[1]] <- x[1] #Assign the rest the same ranking
    dfHold
  }
  
  
  
  
  
  dfTemp <- df
  dfTemp$Year <- factor(df$Year.Ranked)
  
  
  
  dfTemp <- group_by(dfTemp, Year)
  dfTemp <- summarize(dfTemp, numPlayers = n())
  
  if(!(file.exists("./Plots/RecruitsCensus.png"))){
    p <- ggplot(dfTemp, aes(x=Year, y=numPlayers, fill = Year))+
      geom_bar(stat = "identity", color = "black")+
      labs(x = "Year", y = "Number of PRecruits", title = "Players per Year")+
      scale_y_continuous(labels = comma,
                         breaks = 1000 * c(8:19))+
      theme(panel.grid = element_blank(), panel.background = element_blank())+
      coord_cartesian(ylim = c(8000, 19000))+
      guides(fill=FALSE)
    
    
    ggsave(filename = "./Plots/RecruitCensus.png", plot = p, 
           width = 6, height = 4, dpi = 600)
    
    
    rm(dfTemp, p)
  }
  
  
  
  #-Grades go as low as 50, so unranked gets assigned 49
  df$Grade[df$Grade == "NA" | df$Grade == "NR"] <- 49
  
  #-Remove duplicates and keep the newest(solves the JUCO problem)
  df <- df[order(-df$Year.Ranked), ]
  df <- df[!duplicated(c(df$Full.Name, df$Home.Town, df$HomeState),
                       fromLast = TRUE), ] 
  
  names(df) <- c("rank", "fullName", "lastName","firstName","position",
                 "recruitingGrade","hometown", "homeState", "yearRanked")
  
  df <- df %>% arrange(yearRanked,-as.integer(recruitingGrade)) %>% 
    group_by(yearRanked) %>% mutate(origOverallRanking = row_number())
  
  df <- df %>% arrange(yearRanked, -as.integer(recruitingGrade)) %>% 
    group_by(yearRanked, position) %>% mutate(origPosRanking = row_number())
  
  
  return(df)
  
}

#-Combine the player CSVs into a single dataframe
#-Remove player entries that don't correspond to a recruit
getCombinedPlayers <- function() {
  
  #-Fetch recruiting csv for each year and combine into df
  df <- foreach(i=2007:2013, .combine = 'rbind', .inorder = TRUE) %dopar% {
    inFile <- paste("./Data/PlayerInfo/", i, "player.csv", sep = "")
    dfHold <- read.csv(inFile, stringsAsFactors = FALSE)
    dfHold$Full.Name <- paste(dfHold$First.Name, dfHold$Last.Name, sep = " ")
    #-Add the rostered year as a new column
    dfHold$Year.Rostered <- i                  
    dfHold
  } 
  
  df <- subset(df,First.Name != "TEAM")
  
  dfTemp <- df
  dfTemp$Year <- factor(dfTemp$Year.Rostered)
  
  dfTemp <- group_by(dfTemp, Year)
  dfTemp <- summarize(dfTemp, numPlayers = n())
  
  if(!(file.exists("./Plots/PlayerCensus.png"))){
    p <- ggplot(dfTemp, aes(x=Year, y=numPlayers, fill = Year))+
      geom_bar(stat = "identity", color = "black")+
      labs(x = "Year", y = "Number of Players", title = "Players per Year")+
      scale_y_continuous(labels = comma,
                         breaks = 1000 * c(17:22))+
      theme(panel.grid = element_blank(), panel.background = element_blank())+
      coord_cartesian(ylim = c(17000, 22000))+
      guides(fill=FALSE)
    
    ggsave(filename = "./Plots/PlayerCensus.png", plot = p, 
           width = 6, height = 4, dpi = 600)
    
    rm(dfTemp, p)
  }
  
  
  #Get rid of duplicate players, and keep the most recent appearance
  df <- df[order(-df$Year.Rostered), ]
  df <- df[!duplicated(df$Player.Code), ]
  
  #Get rid of unnecessary columns
  df <- subset(df[, c(1,10,11,14,15)])
  names(df) <- c("playerCode","hometown","homeState","fullName","yearRostered")
  
  return(df)
  
}

#Function to create plots based on yearly and positional statistics
createYearlyPlots <- function(){
  
  for(i in 2007:2013){
    
    dfYear <- subset(dfYearlyStats, yearPlayed == i)
    title = paste("Original Ranking vs Ranking in", i, sep = "")
    p <- qplot(yearlyOrigOverallRank, yearlyOverallRank, data = dfYear, geom = "point",
               color = position, main = title)+
      scale_x_continuous(name = "Original Ranking")+
      scale_y_continuous(name = "Current Yearly Ranking")
    ggsave(filename = paste ("./Plots/", i, "Scatter.png", sep = " "), plot = p, 
           width = 6, height = 4, dpi = 600)
    
    title = paste("Variance of original ranking and current ranking in ", i, sep="")
    p2 <- qplot(positionRankingVariance, data = dfYear, geom = "density", 
                main = title, fill = I("blue"), alpha = I(0.6))+
      theme(panel.grid = element_blank(), panel.background = element_blank())+
      scale_x_continuous(name = "Variance between rankings")+
      scale_y_continuous(name = "Density of Occurence")
    
    ggsave(filename = paste ("./Plots/", i, "PositionVarianceDensity.png", sep = " "), plot = p2, 
           width = 6, height = 4, dpi = 600)
    
    
    p3 <- qplot(positionRankingDifference, data = dfYear, geom = "density",
                main = title,fill = I("green"), alpha = I(0.6))+
      scale_x_continuous(name = "Difference between rankings")+
      scale_y_continuous(name = "Density of Occurence")+
      theme(panel.grid = element_blank(), panel.background = element_blank())
    
    ggsave(filename = paste ("./Plots/", i, "PositionDifferenceDensity.png", sep = " "), plot = p3, 
           width = 6, height = 4, dpi = 600)
    
    
    for(j in  c("RB","WR","QB","TE","ATH","FB")){
      dfPos <- subset(dfYear, position == j)
      p4 <- qplot(yearlyOrigPosRank, yearlyPositionRank, data = dfPos, geom = "point")+
        labs(x="Original Position Rank", y = paste("Position Rank in ",i,sep=""),
             title = paste("Original Rank vs Rank in ",i,sep=""))+
        theme(panel.grid = element_blank(), panel.background = element_blank())
      ggsave(filename = paste ("./Plots/", i, j, "Scatter.png", sep = " "), plot = p4, 
             width = 6, height = 4, dpi = 600)
      
      p5 <- qplot(positionRankingVariance, data = dfPos, geom = "density", main = 
                    paste(i," Variance for position: ",j, sep=""), fill = I("red"), alpha = I(0.6))+
        labs(x = "Variance in Position", y = "Density of Occurence")+
        theme(panel.grid = element_blank(), panel.background = element_blank())
      ggsave(filename = paste ("./Plots/", i, j, "PositionVarianceDensity.png", sep = " "), plot = p5, 
             width = 6, height = 4, dpi = 600)
      
      p6 <- qplot(positionRankingDifference, data = dfPos, geom = "density",main = 
                    paste(i," Difference for position: ",j, sep=""), fill = I("orange"), alpha = I(0.6))+
        theme(panel.grid = element_blank(), panel.background = element_blank())+
        labs(x = "Difference in Position", y = "Density of Occurence") 
      ggsave(filename = paste ("./Plots/", i, j, "PositionDifferenceDensity.png", sep = " "), plot = p6, 
             width = 6, height = 4, dpi = 600)
    }
    
  }
  
}

#Function to create plots based on the overall player pool
createCareerPlots <- function(){
  
  p <- qplot(adjOverallRank, newOverallRank, data = dfRecruitCareer, geom = "point", main = 
               "Original Rank vs New Rank")+
    labs(x = "Original Rank(lower is better)", y = "New Rank (lower is better")+
    theme(panel.grid = element_blank(), panel.background = element_blank())+
    scale_color_hue()
  ggsave(filename = paste ("./Plots/CareerScatter.png", sep = " "), plot = p, 
         width = 6, height = 4, dpi = 600)
  
  p2 <- qplot(positionRankingVariance, data = dfRecruitCareer,geom = "density", fill = I("purple"), alpha = I(0.6))+
    labs(x = "Variance in Position Ranking", y = "Density of Occurence", 
         title = "Variance for Career")
  ggsave(filename = "./Plots/CareerVarianceDensity.png", plot = p2, 
         width = 6, height = 4, dpi = 600)
  
  p3 <- qplot(positionRankingDifference, data = dfRecruitCareer, geom = "density", fill = I("yellow"), alpha = I(0.6))+
    labs(x = "Difference in Position Ranking", y = "Density of Occurence", 
         title = "Difference for Career")
  ggsave(filename = "./Plots/CareerDifferenceDensity.png", plot = p3, 
         width = 6, height = 4, dpi = 600)
  
  for(j in  c("RB","WR","QB","TE","ATH","FB")){
    dfPos <- subset(dfRecruitCareer, position == j)
    title = paste("Position Differences for ",j,sep="")
    p4 <- qplot(origPositionRank, newPositionRank, data = dfPos, 
                main = title, geom = "point")+
      scale_x_continuous(name = "Original Rank")+
      scale_y_continuous(name = "Overall Rank")+
      scale_color_brewer()
    ggsave(filename = paste ("./Plots/", j, "CareerScatter.png", sep = " "), plot = p4, 
           width = 6, height = 4, dpi = 600)
    
    p5 <- qplot(positionRankingVariance, data = dfPos,geom = "density", fill = I("purple"), alpha = I(0.6))+
      labs(x = "Variance in Position Ranking", y = "Density of Occurence", 
           title = paste("Variance for ", j,  sep = ""))
    ggsave(filename = paste ("./Plots/", j, "CareerPositionVarianceDensity.png", sep = " "), plot = p5, 
           width = 6, height = 4, dpi = 600)
    
    p6 <- qplot(positionRankingDifference, data = dfPos, geom = "density", fill = I("yellow"), alpha = I(0.6))+
      labs(x = "Difference in Position Ranking", y = "Density of Occurence", 
           title = paste("Difference for ", j, sep = ""))
    ggsave(filename = paste ("./Plots/",j, "CareerPositionDifferenceDensity.png", sep = " "), plot = p6, 
           width = 6, height = 4, dpi = 600)
  }
}

plotMeanDifference <- function(){
  
  
  #Summarize and plot the mean difference by year
  
  df <- group_by(dfRecruitCareer, yearRanked)
  summ <- summarize(df, avgRankingDifference = mean(positionRankingDifference))
  
  summ$yearRanked <- factor(summ$yearRanked)
  
  p <- ggplot(summ, aes(x=yearRanked, y=avgRankingDifference,fill = yearRanked), stat = "identity")+
    geom_bar(stat = "identity")+
    scale_x_discrete(name = "Year",
                     breaks = c(2007:2013))+
    labs(x = "Year Ranked", y = "Difference in Ranking", title = "Summary of Yearly Differences")+
    guides(fill=FALSE)
  
  ggsave(filename = "./Plots/MeanYearlyDifferenceBar.png", plot = p, 
         width = 6, height = 4, dpi = 600)
  
  #Summarize and plot the mean difference by position
  
  df <- group_by(dfRecruitCareer, position)
  summ <- summarize(df, avgRankingDifference = mean(positionRankingDifference))
  
  p <- ggplot(summ, aes(x=position, y=avgRankingDifference,fill = position), stat = "identity")+
    geom_bar(stat = "identity")+
    scale_x_discrete(name = "Position",
                     breaks = c("RB","WR","QB","TE","ATH","FB"))+
    labs(x = "Position", y = "Difference in Ranking", title = "Summary of Position Differences")+
    guides(fill=FALSE)
  
  ggsave(filename = "./Plots/MeanPositionDifferenceBar.png", plot = p, 
         width = 6, height = 4, dpi = 600)
  
  #Summarize and plot the overall difference with bars by year
  
  df <- group_by(dfRecruitCareer, yearRanked, position)
  summ <- summarize(df, avgRankingDifference = mean(positionRankingDifference))
  
  summ$yearRanked <- factor(summ$yearRanked)
  
  p2 <- ggplot(summ, aes(x=yearRanked, y=avgRankingDifference, color = I("black"),fill = position))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_brewer(palette = "Set1")+
    labs(x = "Year Ranked", y = "Difference in Ranking", title = "Summary of Position Differences")
  
  ggsave(filename = "./Plots/OverallSummary.png", plot = p2, 
         width = 6, height = 4, dpi = 600)
  
  #Summarize and plot the overall difference with stacked bars
  
  df <- group_by(dfRecruitCareer, yearRanked, position)
  summ <- summarize(df, avgRankingDifference = mean(positionRankingDifference))
  
  summ$yearRanked <- factor(summ$yearRanked)
  
  p2 <- ggplot(summ, aes(x=yearRanked, y=avgRankingDifference, color = I("black"),fill = position))+
    geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Set1")+
    labs(x = "Year Ranked", y = "Difference in Ranking", title = "Summary of Position Differences")
  
  ggsave(filename = "./Plots/OverallSummaryStacked.png", plot = p2, 
         width = 6, height = 4, dpi = 600)
  
}