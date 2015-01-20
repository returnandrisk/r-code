################################################################################
# FOMC Dates - Price Data Exploration
# R code for 14 Dec 2014 post on www.returnandrisk.com:
# http://www.returnandrisk.com/2014/12/fomc-dates-price-data-exploration.html
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

install.packages(c("quantmod", "reshape2", "ggplot2"), repos = "http://cran.us.r-project.org")
library(quantmod)
library(reshape2) # for melt function
library(ggplot2)

# get 2-year and 10-year US Treasury yields
getSymbols(c("DGS2", "DGS10"), src = "FRED")
DGS2 <- DGS2["2009-01-02/"]
DGS10 <- DGS10["2009-01-02/"]
# get S&P500 ETF prices
getSymbols("SPY", from = "2009-01-02")
# get USD Index ETF prices
getSymbols("UUP", from = "2009-01-02")
# load FOMC announcement dates from file previously saved in working directory
load("fomcdates.RData")

# prepare yield data
yields <- data.frame(index(DGS2), DGS2, DGS10)
names(yields) <- c("Date", "2Yr Yield", "10Yr Yield")
yieldsmelt <- melt(yields, id.vars = "Date")
# plot yield chart
gp <- ggplot(yieldsmelt, aes(x = Date, y = value)) +
    geom_line(aes(colour = variable)) +
    labs(list(title = "US Treasury Yields with FOMC Dates", x = "", y = "% p.a.")) +
    scale_colour_manual(name = 'Yield', values = c('darkblue', 'darkred')) +
    geom_vline(xintercept = as.numeric(fomcdates), linetype = "dashed", size = 0.5,
               alpha = 0.5) +
    scale_x_date()
print(gp)   

# plot S&P500 chart
gp1 <- autoplot.zoo(SPY[, "SPY.Adjusted"]) + 
    labs(list(title = "S&P500 ETF (SPY) with FOMC Dates", x = "", y = "USD")) +
    geom_line(colour="darkblue") +
    geom_vline(xintercept = as.numeric(fomcdates), linetype = "dashed", size = 0.5,
               alpha = 0.5) +
    scale_x_date()
print(gp1)   

# plot USD Index chart
gp2 <- autoplot.zoo(UUP[, "UUP.Adjusted"]) + 
    labs(list(title = "USD Index ETF (UUP) with FOMC Dates", x = "", y = "USD")) +
    geom_line(colour="darkblue") +
    geom_vline(xintercept = as.numeric(fomcdates), linetype = "dashed", size = 0.5,
               alpha = 0.5) +
    scale_x_date()
print(gp2)   

save(list = c("DGS2", "DGS10", "SPY", "UUP", "yields", "yieldsmelt"), file = "fomcprices.RData")

load("fomcprices.RData")