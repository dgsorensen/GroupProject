
#Clear Work area
rm(list = ls())

#Get work directory
getwd()
library(rvest)

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

maxPages <- c(284,258,282,487,508,631,575)

urlP1 <-
  "http://www.espn.com/college-sports/football/recruiting/databaseresults/_/page/"
urlP2 <- "/sportid/24/class/"
urlP3 <- "/sort/grade/order/true"

for(year in 2007:2013){
  i <- 1
  
  url <- paste(urlP1,1,urlP2,year,urlP3, sep="")
    
  
  pg <-
    read_html(url)                        #import the website content
  tb <- html_table(pg, fill = TRUE)           #import tables
  df <- tb[[1]]

currentPage <- 2
while (currentPage < maxPages[i])
{
  

  url <- paste(urlP1,currentPage,urlP2,year,urlP3, sep="")

  pg <- readUrl(url)                     #import the website content
  if (is.list(pg) & length(pg) != 0)     #make sure the site loaded correctly, otherwise skip
  {
    tb <- html_table(pg, fill = TRUE)    #import tables
    newdf <- tb[[1]]
    tmpdf <- rbind(df, newdf)
    df <- tmpdf
    newdf <- newdf[0, ]
    tmpdf <- tmpdf[0, ]
  }
  currentPage <- currentPage + 1

}

names(df) <-
  c("Name", "Hometown", "Position", "Stars", "Grade", "School")
df <- subset(df, Name != "NAME")
outFile <- paste(year,"Recruits.csv")
write.csv(df, outFile, row.names = FALSE)
i <- i + 1.
df <- df[0, ]
}
