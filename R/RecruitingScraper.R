
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))

#Function to wrap the read_html in a try catch block
#Some of ESPN's later pages don't exist, so this will try to read the URL
#And return NULL if the page doensn't load correctly
readUrl <- function(url) {
  out <- tryCatch({
    read_html(url)
  },
  error = function(cond) {
    #Oh Well
    return(NULL)
  },
  
  warning = function(cond) {
    return(NULL)
  })
  return(out)
}




getRecruits <- function () {
  dfLoop <- data.frame(year = 2007:2013, maxPages = c(283,258,282,487,508,631,574) )
  
  for(looper in dfLoop) {
    df <- foreach(page = 1:looper$maxPages,.combine = 'rbind',inorder = TRUE) %dopar% {
      urlP1 <-
        "http://www.espn.com/college-sports/football/recruiting/databaseresults/_/page/"
      urlP2 <- "/sportid/24/class/"
      urlP3 <- "/sort/grade/order/true"
      
      
      url <- paste(urlP1, page, urlP2, looper$year, urlP3, sep = "")
      
      pg <- readUrl(url)                     #import the website content
      if (is.list(pg) & length(pg) != 0) {   #make sure the site loaded correctly, otherwise skip
        
        
        tb <- html_table(pg, fill = TRUE)    #import tables
        newdf <- tb[[1]]
        dfYear <- rbind(dfYear, newdf)            #combine tables
        dfYear <- tmpdf
        newdf <- newdf[0,]               #clear out the old data
        
      }
      
      names(df) <- c("Name","Hometown","Position","Stars", "Grade","School")
      df <- subset(df, Name != "NAME") #Remove the NAME row that comes with each page
      
      #Format the data
      
      
      #If the homestate contains ", ", then we want the 2 characters following the space 
      #as the state.  If it doesn't, then we leave it blank (e.g. if it just lists a school)
      df$homestate <-ifelse(grepl(".*, (..).*", df$Hometown),
                            gsub(".*, (..).*", "\\1", df$Hometown)," ")
      
      #all names come as First LastVideo | xxxx, and we only want First Last
      df$Name <- gsub("Video.*", "", df$Name) #substitute "Video... with space
      df$First.Name <- gsub(" .*", "", df$Name) #
      df$Last.Name <-  sub("(.*?) ", "", df$Name) #everything after the first space becomes the last name
      
      #Formatting Positions (e.g. QB-DT, etc.  We just want the position)
      df$Position <- gsub("#.* ", "", df$Position) #fixes "#1 DT" issue
      df$Position <- gsub("-.*", "", df$Position)  #fixes QB-PP issue
      df$Position <- gsub("(\\(.*)\\)", "", df$Position) #fixes DT(POST) issue
      
      #Get rid of columns we don't want
      df$School <- NULL
      df$Hometown <- NULL
      df$Stars <- NULL
      
      
      outFile <- paste("./Data/PlayerRankings/",looper$year, "CFBPlayerRankings.csv")
      write.csv(df, outFile, row.names = FALSE)
      
    }
  }
}