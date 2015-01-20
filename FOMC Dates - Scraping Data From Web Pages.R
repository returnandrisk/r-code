################################################################################
# FOMC Dates - Scraping Data From Web Pages
# R code for 30 Nov 2014 post on www.returnandrisk.com:
# http://www.returnandrisk.com/2014/11/scraping-data-from-web-pages-fomc-dates.html
# Copyright (C) 2014  Peter Chan (peter-at-return-and-risk-dot-com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
