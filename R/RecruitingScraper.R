
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(rvest))

#Scrape the recruiting data from the ESPN recruited pages.
#Each year has a different amount of pages to read, so maxPages controls
#the number of iterations
getRecruits <- function () {
  dfLoop <- data.frame(year = 2007:2013, 
                       maxPages = c(283,258,282,487,508,631,574) )
  
  for (looper in dfLoop) {
    on.exit(close(pg))
    df <- foreach(page = 1:looper$maxPages, .combine = 'rbind', .inorder = TRUE) %dopar% {
      urlP1 <-"http://www.espn.com/college-sports/football/recruiting/
                             databaseresults/_/page/"
      urlP2 <- "/sportid/24/class/"
      urlP3 <- "/sort/grade/order/true"
      
      
      url <- paste(urlP1, page, urlP2, looper$year, urlP3, sep = "")
      #import the website content
      pg <- read_html(url, options=c("RECOVER, NOERROR"))              
      #make sure the site loaded correctly, otherwise skip
      if (is.list(pg) & length(pg) != 0) {   
        
        tb <- html_table(pg, fill = TRUE)    #import tables
        dfHold <- tb[[1]]
        dfHold          
        
      }
      
      names(df) <- c("Name","Hometown","Position","Stars", "Grade","School")
      #Remove the NAME row that comes with each page
      df <- subset(df, Name != "NAME") 
      
      #------------------------------------------------------------
      #-Format the data into usable CSVs from the HTML using regex
      #------------------------------------------------------------
      
      #-Fetch the state by grabbing the 2 chars after ', '
      #-Leave it blank is ', ' doesn't appear
      df$homestate <-ifelse(grepl(".*, (..).*", df$Hometown),
                            gsub(".*, (..).*", "\\1", df$Hometown)," ")
      
      #-Fetch the town by grabbing everything before ', '
      #-Leave it blank is ', ' doesn't appear
      df$hometown  <-ifelse(grepl(".*, (..).*", df$Hometown),
                            gsub(" .*", "", df$Hometown), "")
      
      
      #-All names come as First LastVideo | xxxx, and we only want First Last
      df$Name <- gsub("Video.*", "", df$Name) #Remove Video* from Name
      df$First.Name <- gsub(" (.*?)", "", df$Name) #Grab data before Space (not greedy)
      df$Last.Name <-  sub("(.*?) ", "", df$Name) #Grab data after Space (not greedy)
      
      #Formatting Positions (e.g. QB-DT, etc.  We just want the position)
      df$Position <- gsub("#.* ", "", df$Position) #fixes "#1 DT" issue
      df$Position <- gsub("-.*", "", df$Position)  #fixes QB-PP issue
      df$Position <- gsub("(\\(.*)\\)", "", df$Position) #fixes DT(POST) issue
      
      
      #Get rid of columns we don't want
      df$School <- NULL
      df$Stars <- NULL
      
      outFile <- paste("./Data/PlayerRankings/",looper$year, "CFBPlayerRankings.csv")
      write.csv(df, outFile, row.names = FALSE)
      
    } #end year loop
  }#end outer loop
}#end function