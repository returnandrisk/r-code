################################################################################
# FOMC Dates - Full History Web Scrape Update (26 May 2016)
# Updated R functions for 21 Jan 2015 post on www.returnandrisk.com:
# http://www.returnandrisk.com/2014/11/scraping-data-from-web-pages-fomc-dates.html
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

# function to extract fomc data from recent years
get.fomc.dates.recent <- function() {
    print("Getting FOMC dates for recent years...")
    # get and parse web page content                                            
    webpage <- content(GET("http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"), as="text")
    xhtmldoc <- htmlParse(webpage)
    # get nodeset of relevant html tables
    nodeset <- getNodeSet(xhtmldoc, "//table[@id='fomcCurrentCal']")
    # initialise dataframe
    fomcdates <- data.frame(begdate = as.numeric(),
                            enddate = as.numeric(),
                            pressconf = as.numeric(),
                            scheduled = as.numeric(),
                            document = as.character(),
                            url = as.character(),
                            stringsAsFactors = FALSE)
    # iterate through nodeset and parse dates
    for (i in 1:length(nodeset)) {
        days <- xpathApply(nodeset[[i]], ".//td[@class='day']", xmlValue)
        months <- xpathApply(nodeset[[i]], ".//td[@class='month']", xmlValue)
        year <- substr(xpathApply(nodeset[[i]], ".//th", xmlValue), 1, 4)
        statements <- xpathApply(nodeset[[i]], ".//td[@class='statement2']", xmlValue)
        urls <- xpathApply(nodeset[[i]], ".//td[@class='statement2']/a[1]", xmlGetAttr, "href")
        tmp <- parse.dates.recent(days, months, year, statements, urls)
        fomcdates <- rbind(fomcdates, tmp)
    }
    fomcdates
}

# function to parse dates from recent years
parse.dates.recent <- function(days, months, year, statements, urls) {
    len <- length(days)
    # initialise dataframe
    dates <- data.frame(begdate = as.Date(rep(0, len), origin = "1970-01-01"),
                        enddate = as.Date(rep(0, len), origin = "1970-01-01"),
                        pressconf = as.numeric(rep(0, len)),
                        scheduled = as.numeric(rep(1, len)),
                        document = as.character(rep(NA, len)),
                        url = as.character(rep(NA, len)),
                        stringsAsFactors = FALSE)
    for (i in 1:len) {
        day <- days[[i]]
        month <- months[[i]]
        # flags for press conference and unscheduled
        if (grepl("\\*", day)) dates[i, 3] <- 1
        if (grepl("unscheduled", day, ignore.case = TRUE)) dates[i, 4] <- 0
        # parse day
        dmatch <- gregexpr("[^ A-Za-z*()]", day)
        day <- paste(unlist(regmatches(day, dmatch)), sep = "", collapse = "")
        day <- strsplit(day, "-")
        # parse month
        month <- strsplit(months[[i]], "/")
        # parse begin and end dates
        beg <- as.Date(paste0(day[[1]][1], month[[1]][1], year), format = "%d%B%Y")
        if (length(month[[1]]) == 1) {
            if (length(day[[1]]) == 1) {
                end <- beg
            } else {
                end <- as.Date(paste0(day[[1]][2], month[[1]][1], year), format = "%d%B%Y")
            }
        } else {
            end <- as.Date(paste0(day[[1]][2], month[[1]][2], year), format = "%d%B%Y")
        }
        dates[i, 1] <- beg
        dates[i, 2] <- end
    }
    # parse statement urls
    stmtexists <- grepl("Statement", statements)
    if (sum(stmtexists) < length(statements)) statements[!stmtexists] <- NA
    statements[stmtexists] <- urls
    dates$document[stmtexists] <- "Statement"
    dates$url <- unlist(statements)
    dates
}

# function to extract fomc data from 1936 (excluding recent years)
get.fomc.dates.past <- function(begyear = 1936, endyear = 2010) {
    print(paste("Getting FOMC dates from", begyear, "to", endyear))
    # initialise dataframe
    fomcdates <- data.frame(begdate = as.numeric(),
                            enddate = as.numeric(),
                            pressconf = as.numeric(),
                            scheduled = as.numeric(),
                            document = as.character(),
                            url = as.character(),
                            stringsAsFactors = FALSE)
    # iterate through each year                                            
    for (year in seq(begyear, endyear)) { 
        print(year)
        # get and parse web page content                                            
        xhtmldoc = htmlTreeParse(paste0("http://www.federalreserve.gov/monetarypolicy/fomchistorical", year, ".htm"), useInternalNodes = TRUE)
        # get nodeset of relevant html tables
        nodeset <- getNodeSet(xhtmldoc, "//table[@class='alternate']")
        # iterate through nodeset and parse dates
        for (i in 1:length(nodeset)) {
            header <- xpathApply(nodeset[[i]], ".//th[@class='year']", xmlValue)
            document <- xpathApply(nodeset[[i]], ".//td[@class='minutes']/ul[@class='fomc']/li[1]", xmlValue)
            url <- xpathApply(nodeset[[i]], ".//td[@class='minutes']/ul[@class='fomc']/li[1]/a", xmlGetAttr, "href")
            tmp <- parse.dates.past(header, document, url)
            fomcdates <- rbind(fomcdates, tmp)
        }
    }
    fomcdates
}

# function to parse dates from 1936 (excluding recent years)
parse.dates.past <- function(header, document, url) {
    # initialise dataframe
    dates <- data.frame(begdate = as.Date(0, origin = "1970-01-01"),
                        enddate = as.Date(0, origin = "1970-01-01"),
                        pressconf = as.numeric(0),
                        scheduled = as.numeric(0),
                        document = as.character(NA),
                        url = as.character(NA),
                        stringsAsFactors = FALSE)
    header <- unlist(header)
    # parse year
    ymatch <- gregexpr("[0-9]{4}", header)
    year <- paste(unlist(regmatches(header, ymatch)), sep = "", collapse = "")
    # assume scheduled meeting if header contains "Meeting"
    if (grepl("Meeting", header)) dates$scheduled <- 1
    # set document type
    dates$document <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", document[[1]], perl=TRUE)
    # parse date
    header <- substr(header, 1, gregexpr("(Meeting)|(Conference Call)", header)[[1]][1] - 2)
    monthday <- unlist(strsplit(header, "-"))
    mpattern <- "(January)|(February)|(March)|(April)|(May)|(June)|(July)|(August)|(September)|(October)|(November)|(December)"
    if (length(monthday) == 1) {
        # fix for Oct 21-30 1987 stock market crash conference calls
        if (monthday == "October 21, 22, 23, 26, 27, 28, 29, and 30 ") {
            beg <- as.Date(c("21101987", "22101987", "23101987", "26101987", "27101987", "28101987", "29101987", "30101987"), format = "%d%m%Y")
        } else {
            beg <- as.Date(paste(monthday, year), format = "%B %d %Y")
        }
        end <- beg
    } else {
        beg <- as.Date(paste(monthday[1], year), format = "%B %d %Y")
        mmatch <- gregexpr(mpattern, monthday)
        if (mmatch[[2]][1] == -1) {
            # one month matched
            end <- as.Date(paste(regmatches(monthday, mmatch)[[1]], monthday[2], year), format = "%B %d %Y")
        } else {
            # two months matched
            monthday <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", monthday[2], perl=TRUE)
            end <- as.Date(paste(monthday, year), format = "%B %d %Y")
        }  
    }
    # fix for 21-30 Oct 1987 stock market crash conference calls
    if (length(beg) == 8) {
        dates <- data.frame(begdate = beg, enddate = end, pressconf = NA, scheduled = 0, document = "Record of Policy Actions: see September 22, 1987 Record", url = NA, stringsAsFactors = FALSE)
    } else {
        dates$begdate <- beg
        dates$enddate <- end
        if (length(url) > 0) dates$url <- url[[1]]
    }
    # fix for 15 Sep 2003 meeting, set as unscheduled
    ifelse (beg == as.Date("15092003", format = "%d%m%Y"), dates$scheduled <- 0, dates$scheduled <- dates$scheduled)
    dates
}