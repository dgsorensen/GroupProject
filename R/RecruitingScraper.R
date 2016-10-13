

#Clear Work area
rm(list = ls())

#Get work directory
getwd()
library(foreach)
library(doParallel)
library(iterators)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

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
  foreach(year = 2007:2013,
          maxPages = 283,
          258,
          282,
          487,
          508,
          631,
          574) %:% {
            #Get the first page
            url <-
              "http://www.espn.com/college-sports/football/recruiting/databaseresults/_/page/1/sportid/24/class/2016/sort/grade/order/true"
            
            pg <-
              read_html(url)                        #import the website content
            tb <- html_table(pg, fill = TRUE)           #import tables
            df <- tb[[1]]
            
            foreach(page = 2:maxPages,
                    .combine = 'rbind',
                    .inorder = TRUE) %dopar% {
                      urlP1 <-
                        "http://www.espn.com/college-sports/football/recruiting/databaseresults/_/page/"
                      urlP2 <- "/sportid/24/class/"
                      urlP3 <- "/sort/grade/order/true"
                      
                      
                      
                      
                      
                      url <- paste(urlP1, page, urlP2, year, urlP3, sep = "")
                      
                      pg <-
                        readUrl(url)                     #import the website content
                      if (is.list(pg) &
                          length(pg) != 0)
                        #make sure the site loaded correctly, otherwise skip
                      {
                        tb <- html_table(pg, fill = TRUE)    #import tables
                        newdf <- tb[[1]]
                        tmpdf <- rbind(df, newdf)
                        df <- tmpdf
                        newdf <- newdf[0,]
                        tmpdf <- tmpdf[0,]
                      }
                      
                      if (page == maxpages - 1) {
                        names(df) <-
                          c("Name",
                            "Hometown",
                            "Position",
                            "Stars",
                            "Grade",
                            "School")
                        df <- subset(df, Name != "NAME")
                        
                        #Just get the state from the field
                        df$homestate <-
                          ifelse(grepl(".*, (..).*", df$Hometown),
                                 gsub(".*, (..).*", "\\1", df$Hometown),
                                 " ")
                        
                        df$Name <-
                          gsub("Video.*", "", df$Name)#Remove Video.. from Name, which just gives us the name
                        df$First.Name <-
                          gsub(" .*", "", df$Name) #everything before the space becomes the first name
                        df$Last.Name <-
                          sub("(.*?) ", "", df$Name) #everything after the first space becomes the last name
                        
                        #Formatting Positions (e.g. QB-DT, etc.  We just want the position)
                        df$Position <-
                          gsub("#.* ", "", df$Position) #fixes "#1 DT" issue
                        df$Position <-
                          gsub("-.*", "", df$Position)  #fixes QB-PP issue
                        df$Position <-
                          gsub("(\\(.*)\\)", "", df$Position) #fixes DT(POST) issue
                        
                        #Get rid of columns we don't want
                        df$School <- NULL
                        df$Hometown <- NULL
                        df$Stars <- NULL
                        
                        
                        outFile <- paste("./Data/PlayerRankings/",year, "CFBPlayerRankings.csv")
                        write.csv(df, outFile, row.names = FALSE)
                      }
                    }
            
          }
}