

#Clear Work area
rm(list = ls())

#Get work directory
getwd()
library(rvest)
library(parallel)
library(foreach)
library(doParallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores, type = "FORK")

#Function to wrap the read_html in a try catch block
#Some of ESPN's later pages don't exist, so this will try to read the URL
#And return NULL if the page doensn't load correctly
readUrl <- function(url) {
  library(rvest)
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

parLapply(cl, X= list(2008, 2010, 2013) ,function(x)  {
  maxPages <- c( 258,  487, 575)
  names(maxPages) <- c("2008", "2010", "2013")
  if (x == 2008)
    maxPages <- 258
  else if(x == 2010)
    maxPages <- 487
  else if(x == 2013)
    maxPages <- 575
  


  
  urlP1 <-
    "http://www.espn.com/college-sports/football/recruiting/databaseresults/_/page/"
  urlP2 <- "/sportid/24/class/"
  urlP3 <- "/sort/grade/order/true"





    
    url <-paste(urlP1, 1, urlP2, x, urlP3,  sep = "")
    
    
    pg <-
      read_html(url)                        #import the website content
    
    tb <- html_table(pg, fill = TRUE)           #import tables
    df <- tb[[1]]
    
    currentPage <- 2
    while (currentPage < maxPages)
    {
      
      if (x == 2008 & page == 26)
        continue
    
      url <- paste(urlP1, currentPage, urlP2, x ,urlP3 ,sep = "")
      
      pg <-
        read_html(url)                     #import the website content
      #if(is.null(pg)){continue}
        #make sure the site loaded correctly, otherwise skip
      
        tb <- html_table(pg, fill = TRUE)    #import tables
        newdf <- tb[[1]]
      
        tmpdf <- rbind(df, newdf)
        df <- tmpdf
        newdf <- newdf[0,]
        tmpdf <- tmpdf[0,]
      
      currentPage <- currentPage + 1
      
    }
    
    names(df) <-
      c("Name", "Hometown", "Position", "Stars", "Grade", "School")
    df <- subset(df, Name != "NAME")
    outFile <- paste(x,"Recruits.csv", sep = "")
    write.csv(df, outFile, row.names = FALSE)

})
#clusterExport(cl)

# registerDoParallel(cl)
# foreach(x=list(2008, 2010, 2013))  %dopar%  
# {
#   scrapePlayerRankings(x)
# }
# stopCluster(cl)

