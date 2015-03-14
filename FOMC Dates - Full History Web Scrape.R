################################################################################
# FOMC Dates - Full History Web Scrape
# R code for 21 Jan 2015 post on www.returnandrisk.com:
# http://www.returnandrisk.com/2015/01/fomc-dates-full-history-web-scrape.html
# Copyright (C) 2015  Peter Chan (peter-at-return-and-risk-dot-com)
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

# load fomc date functions
source("FOMC Dates Functions.R")

# extract data from web pages and parse dates

fomcdatespre2009 <- get.fomc.dates.pre.2009(1936, 2009) # need to manually update second year when the Fed archives it
fomcdatesfrom2009 <- get.fomc.dates.from.2009()

# combine datasets and order chronologically
fomcdatesall <- do.call(rbind, list(fomcdatespre2009, fomcdatesfrom2009))
fomcdatesall <- fomcdatesall[order(fomcdatesall$begdate), ]

# save as RData format
save(fomcdatesall, file = "fomcdatesall.RData")
# save as csv file
write.csv(fomcdatesall, "fomcdatesall.csv", row.names = FALSE)

# check results
head(fomcdatesall)
tail(fomcdatesall, 100)


