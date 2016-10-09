
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



#Get the first page
url <-
  "http://www.espn.com/college-sports/football/recruiting/databaseresults/_/page/1/sportid/24/class/2016/sort/grade/order/true"

pg <-
  read_html(url)                        #import the website content
tb <- html_table(pg, fill = TRUE)           #import tables
df <- tb[[1]]



urlPrefix <-
  "http://www.espn.com/college-sports/football/recruiting/databaseresults/_/page/"
urlSuffix <- "/sportid/24/class/2016/sort/grade/order/true"
currentPage <- 2


while (currentPage < 284)
{
  url <- paste(urlPrefix, currentPage, urlSuffix, sep = "")

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
write.csv(df, "20016Players.csv", row.names = FALSE)

#df_top25 <- tb[[12]]                        #top 25 teams

#df_ratings <- tb[[11]]                      #ranking of 32 conferences
