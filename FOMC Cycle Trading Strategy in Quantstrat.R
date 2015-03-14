################################################################################
# FOMC Cycle Trading Strategy in QuantStrat
# R code for 14 March 2015 post on www.returnandrisk.com:
# http://www.returnandrisk.com/2015/03/fomc-cycle-trading-strategy-in.html
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

################################################################################
# install packages and load them                                               #
################################################################################
install.packages("RCurl", repos = "http://cran.us.r-project.org")
install.packages("quantstrat", repos="http://R-Forge.R-project.org")
library(RCurl)
library(quantstrat)

################################################################################
# get data - Jan 1994 to Mar 2015                                              #
################################################################################
# download csv file data of FOMC announcement dates from previous post
csvfile = getURLContent(
    "https://docs.google.com/uc?export=download&id=0B4oNodML7SgSckhUUWxTN1p5VlE",
    ssl.verifypeer = FALSE, followlocation = TRUE, binary = FALSE)
fomcdatesall <- read.csv(textConnection(csvfile), colClasses = c(rep("Date", 2),
    rep("numeric", 2), rep("character", 2)), stringsAsFactors = FALSE)
# set begin and end dates
beg.date <- "1994-01-01" 
end.date <- "2015-03-09"
last.fomc.date <- "2015-03-17"
# get S&P500 ETF prices
getSymbols(c("SPY"), from = beg.date, to = end.date)
# subset fomc dates
fomc.dates <- subset(fomcdatesall, begdate > as.Date(beg.date) & 
                         begdate <= as.Date(last.fomc.date) & 
                         scheduled == 1, select = c(begdate, enddate))

################################################################################
# custom indicator function for fomc cycle                                     #
# calculates cycle day, week and phase                                         #
################################################################################
get.fomc.cycle <- function(mktdata, fomcdates, begdate, enddate) {
    # create time series with all weekdays incl. holidays
    indicator <- xts(order.by = seq(as.Date(begdate), as.Date(as.numeric(last(fomc.dates)[2])), by = 1))
    indicator <- merge(indicator, mktdata)
    indicator <- indicator[which(weekdays(index(indicator)) %in% c("Monday", "Tuesday", "Wednesday",
                                                                   "Thursday", "Friday")), ]
    indicator <- na.locf(indicator)
    names(indicator) <- "close"
    indicator$week <- indicator$day <- NA
    indicator$phase <- NA
    # get fomc cycle data
    numdates <- nrow(fomcdates)
    for (i in 1:numdates) {
        cycle.beg <- which(index(indicator) == fomcdates[i, "enddate"]) - 6
        if (i < numdates) {
            cycle.end <- which(index(indicator) == fomcdates[i + 1, "enddate"]) - 6
        } else {
            cycle.end <- nrow(indicator)
        }
        # calculate cycle window, day and week counts
        win <- window(index(indicator), cycle.beg, cycle.end)
        win.len <- length(win)
        day <- seq(-6, win.len - 7)
        week <- rep(-1:7, each = 5, length.out = win.len)
        # identify up and down phases
        phase <- rep(c(-1, 1), each = 5, length.out = win.len)
        # combine data
        indicator[cycle.beg:cycle.end, c("day", "week", "phase")] <- c(day, week, phase)
    }
    # fix for day number > 33 ie keep as week 6 up-phase
    # (only 3 instances 1994-2014, so not material)
    indicator$phase[which(indicator$day > 33)] <- 0 # 1
    # shift phase forward 2 days to force quantstrat trades to be executed on
    # close of correct day ie this is a hack
    indicator$phase.shift <- lag(indicator$phase, -2) 
    return(indicator[paste0(begdate, "::", enddate), ])
}

# get fomc cycle indicator data
fomc.cycle <- get.fomc.cycle(Ad(SPY), fomc.dates, beg.date, end.date)
# calculate 1-day and 5 day returns
fomc.cycle$ret1day <- ROC(fomc.cycle$close, n = 1, type = "discrete")
fomc.cycle$ret5day <- lag(ROC(fomc.cycle$close, n = 5, type = "discrete"), -4)
# calculate average 5-day return based on day in fomc cycle
rets <- tapply(fomc.cycle$ret5day, fomc.cycle$day, mean, na.rm = TRUE)[1:40] * 100
# plot cycle graph
plot(-6:33, rets, type = "l",
     xlab = "Days since FOMC meeting (weekends excluded)", 
     ylab = "Avg 5-day return, t0 to t4 (%)", 
     main = "SPY Average 5-day Return over FOMC Cycle\r\nJan 1994 - Mar 2015",
     xaxt = "n")
axis(1, at = seq(-6, 33, by = 1))
points(-6:33, rets)
abline(h = seq(-0.2, 0.6, 0.2), col = "gray")
points(seq(-6, 33, 10), rets[seq(1, 40, 10)], col = "red", bg = "red", pch = 25)
points(seq(-1, 33, 10), rets[seq(6, 40, 10)], col = "blue", bg = "blue", pch = 24)
text(-6:33, rets, -6:33, adj = c(-0.25, 1.25), cex = 0.7)
# get spy close mktdata for quantstrat
spy <- fomc.cycle$close

################################################################################
# trading strategy using quantsrat                                             #
################################################################################
# workaround to xts date handling, reversed at end of code
ttz <- Sys.getenv('TZ')
Sys.setenv(TZ = 'UTC')
# cleanup
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
suppressWarnings(rm(list = ls(envir = .blotter), envir = .blotter))
suppressWarnings(rm(list = ls(envir = .strategy), envir = .strategy))
# etf instrument setup
etf <- "spy"
currency("USD")
stock(etf, currency = "USD", multiplier = 1)
# required quantstrat variables
initDate <- "1994-01-01"
initEq <- 1e5 
qs.account <- "fomc"
qs.portfolio <- "trading"
qs.strategy <- "longonly"
# initialize quantstrat
initPortf(name = qs.portfolio, symbols = etf, initDate = initDate)
initOrders(portfolio = qs.portfolio, initDate = initDate)
initAcct(name = qs.account, portfolios = qs.portfolio, initDate = initDate, 
         initEq = initEq)

################################################################################
# custom transaction fee function                                              #
################################################################################
# execution costs estimated at 5 basis points, incls brokerage and slippage
ExecutionCost <- 0.0005 
# custom transaction fee function based on value of transaction
AdValoremFee <- function(TxnQty, TxnPrice, Symbol, ...)
{
    abs(TxnQty) * TxnPrice * -ExecutionCost
}

################################################################################
# custom order sizing function to allocate 100% of equity to a trade           #
################################################################################
osAllIn <- function(timestamp, orderqty, portfolio, symbol, ruletype, 
                    roundqty = FALSE, ...) {
    # hack to get correct index for trading on today's close
    idx <- which(index(mktdata) == as.Date(timestamp)) + 1
    close <- as.numeric(Cl(mktdata[idx, ]))
    txns <- getTxns(portfolio, symbol, paste0(initDate, "::", timestamp))
    # calculate unrealised pnl
    tmp <- getPos(portfolio, symbol, timestamp)
    unreal.pl <- (close - as.numeric(tmp$Pos.Avg.Cost)) * as.numeric(tmp$Pos.Qty)
    # round qty down or not
    if (roundqty) {
        orderqty <- floor((initEq + sum(txns$Net.Txn.Realized.PL) + unreal.pl) / 
                              (close * (1 + ExecutionCost))) * sign(orderqty)
    } else {
        orderqty <- (initEq + sum(txns$Net.Txn.Realized.PL) + unreal.pl) / 
            (close * (1 + ExecutionCost)) * sign(orderqty)
    } 
    return(orderqty[1])
}

################################################################################
# define long only strategy                                                    #
################################################################################
strategy(name = qs.strategy, store = TRUE)
# add custom indicator get.fomc.cycle
add.indicator(qs.strategy, name = "get.fomc.cycle", arguments = list(mktdata = 
     quote(Cl(spy)), fomcdates = fomc.dates, begdate = beg.date, enddate = 
     end.date), label = "ind", store = TRUE)
# add signals
add.signal(strategy = qs.strategy, name = "sigThreshold", arguments = 
               list(column = c("phase.shift.ind"), relationship ="gt", threshold = 0.5, 
                    cross = TRUE), label = "long.entry")
add.signal(strategy = qs.strategy, name = "sigThreshold", arguments = 
               list(column = c("phase.shift.ind"), relationship = "lt", threshold = 0.5,
                    cross = TRUE), label = "long.exit")
# add long entry rule
add.rule(strategy = qs.strategy, name="ruleSignal", arguments = list(
    sigcol = "long.entry", sigval = TRUE, orderqty = 1, ordertype = "market", 
    orderside = "long", TxnFees = "AdValoremFee", osFUN = "osAllIn", roundqty = 
        TRUE, replace = FALSE), type = "enter")
# add long exit rule
add.rule(strategy = qs.strategy, name="ruleSignal", arguments = list(sigcol =
    "long.exit", sigval = TRUE, orderqty = "all", ordertype = "market", 
    orderside = "long", TxnFees = "AdValoremFee", replace = FALSE), type = "exit")

################################################################################
# run strategy backtest                                                        #
################################################################################
applyStrategy(strategy = qs.strategy, portfolios = qs.portfolio)
updatePortf(Portfolio = qs.portfolio)
updateAcct(qs.account)
updateEndEq(qs.account)
# get trading data for future use...
book    = getOrderBook(qs.portfolio)
stats   = tradeStats(qs.portfolio, use = "trades", inclZeroDays = TRUE)
ptstats = perTradeStats(qs.portfolio)
txns    = getTxns(qs.portfolio, etf)

################################################################################
# analyze long only performance                                                #
################################################################################
equity.curve <- getAccount(qs.account)$summary$End.Eq
daily.returns <- Return.calculate(equity.curve$End.Eq, "discrete")
names(daily.returns) <- "return"
# get annualized summary
table.AnnualizedReturns(daily.returns, scale = 260.85) # adjusted for weekdays 
# per year of ~ 260.85
# chart performance
charts.PerformanceSummary(daily.returns, main = "FOMC Cycle Strategy Performance")
# get some summary trade statistics
stats[,c("Symbol", "Num.Trades", "Percent.Positive", "Net.Trading.PL",
         "Profit.Factor", "Max.Drawdown")] 
# get table of monthly returns
monthly.returns <-  Return.calculate(to.monthly(equity.curve)[, 4], "discrete")
names(monthly.returns) <- "Total"
table.CalendarReturns(monthly.returns)

################################################################################
# comparison with buy and hold strategy                                        #
################################################################################
# calculate buy and hold summary performance using functions from package
# PerformanceAnalytics - quick but doesn't take into account transaction costs
table.AnnualizedReturns(fomc.cycle$ret1day["1994-02-03::"], scale = 260.85)
# compare long only fomc cyclewith buy and hold
compare.returns <- cbind(daily.returns["1994-02-03::"], 
                         fomc.cycle$ret1day["1994-02-03::"])
names(compare.returns) <- c("Long only FOMC Cycle", "Buy and Hold")
charts.PerformanceSummary(compare.returns, main = "Performance Comparison - 
    Long only FOMC Cycle vs Buy and Hold")

# save data for future use...
save.image("longonly.fomccycle.RData")
# cleanup - remove date workaround
Sys.setenv(TZ = ttz)