################################################################################
# R code for 30 Nov 2014 post on www.returnandrisk.com:
# FOMC Dates - Scraping Data From Web Pages
# http://www.returnandrisk.com/2014/11/scraping-data-from-web-pages-fomc-dates.html
################################################################################

install.packages(c("httr", "XML"), repos = "http://cran.us.r-project.org")

library(httr)
library(XML)

# get and parse web page content                                            
webpage <- content(GET("http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"), as="text")
xhtmldoc <- htmlParse(webpage)
# get statement urls and sort them
statements <- xpathSApply(xhtmldoc, "//td[@class='statement2']/a", xmlGetAttr, "href")
statements <- sort(statements)
# get dates from statement urls
fomcdates <- sapply(statements, function(x) substr(x, 28, 35))
fomcdates <- as.Date(fomcdates, format="%Y%m%d")
# save results in working directory
save(list = c("statements", "fomcdates"), file = "fomcdates.RData")

str(statements)
head(statements)
str(fomcdates)
head(fomcdates)

load("fomcdates.RData")
