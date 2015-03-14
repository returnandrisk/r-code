################################################################################
# Update on The Pre-FOMC Annooncement Drift
# R code for 4 March 2015 post on www.returnandrisk.com:
# http://www.returnandrisk.com/2015/03/update-on-pre-fomc-announcement-drift.html
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
install.packages(c("quantmod", "Quandl", "boot", "eventstudies", "compute.es",
                   "pwr", "stargazer"), repos = "http://cran.us.r-project.org")
library(quantmod) 
library(Quandl)
library(boot)
library(eventstudies)
library(stargazer)
library(compute.es)
library(pwr)

################################################################################
# get data                                                                     #
################################################################################
# download csv file of FOMC announcement dates from previous post into working 
# directory, and read into R
library("curl")
download.file("https://docs.google.com/uc?export=download&id=0B4oNodML7SgSMlZxYW4yWTZabGs",
              "fomcdatesall.csv")
+# for apple mac users, uncomment the following 2 lines and use them to download the file
+# download.file("https://docs.google.com/uc?export=download&id=0B4oNodML7SgSMlZxYW4yWTZabGs",
+#              "fomcdatesall.csv", method = "curl")
fomcdatesall <- read.csv("fomcdatesall.csv", colClasses = c(rep("Date", 2),
                                                            rep("numeric", 2), rep("character", 2)), stringsAsFactors = FALSE)
# get S&P500 ETF prices
getSymbols("SPY", from = "1994-01-01", to = "2015-02-28")
# get fama french factors from quandl
FF <- Quandl("KFRENCH/FACTORS_D", type="xts")
# get daily risk-free rate of return i.e. "The Tbill return is the simple daily
# rate that, over the number of trading days in the month, compounds to 1-month
# TBill rate from Ibbotson and Associates, Inc."
rets.rf <- FF["1994-01-01/2015-02-28", "RF"] / 100
# calculate SPY 1-day simple returns (close-to-close)
rets <- cbind(ROC(SPY[, "SPY.Adjusted"], type = "discrete")[-1], rets.rf[-1])
rets$RF <- na.locf(rets$RF) # no Feb 2015 data at time of writing
# calculate SPY excess return over risk-free tbill return
xs.rets <- rets[, 1] - rets[, 2]
names(xs.rets) <- "xs.ret"

################################################################################
# create in-sample data - Jan 1994 to Mar 2011                                 #
################################################################################
ins.dates <- subset(fomcdatesall, begdate > "1994-01-01" & 
                        begdate < as.Date("2011-03-31") & 
                        scheduled == 1, select = c(begdate, enddate))
# get in-sample SPY 1-day excess simple returns (close-to-close)
ins.xs.rets <- xs.rets["/2011-03-31"]
# get returns on fomc dates
ins.xs.fomcrets <- ins.xs.rets[ins.dates$enddate, ]
# get returns on nonfomc dates
ins.xs.nonfomcrets <- ins.xs.rets[-which(index(ins.xs.rets) 
                                         %in% ins.dates$enddate), ]

################################################################################
# create out-of-sample data - Apr 2011 to Jan 2015                             #
################################################################################
oos.dates <- subset(fomcdatesall, begdate > as.Date("2011-03-31") & 
                        begdate < as.Date("2015-02-28") &
                        scheduled == 1, select = c(begdate, enddate))
# get out-of-sample SPY 1-day excess simple returns (close-to-close)
oos.xs.rets <- xs.rets["2011-04-01/2015-02-28"]
# get returns on fomc dates
oos.xs.fomcrets <- oos.xs.rets[oos.dates$enddate, ]
# get returns on nonfomc dates
oos.xs.nonfomcrets <- oos.xs.rets[-which(index(oos.xs.rets) 
                                         %in% oos.dates$enddate), ]

################################################################################
# in-sample event study                                                        #
################################################################################
# event study function
plot.es <- function(dates, returns, window = 5) {
    # prepare data for event study
    ins.events <- data.frame(unit = names(returns), 
                             when = dates[, "enddate"], stringsAsFactors = FALSE)
    # map returns to event time e.g. event time index = 0 is mapped to calendar
    # end date of fomc meeting
    rets.evt <- phys2eventtime(z = returns, events = ins.events, width = window)
    # get 5-day window of returns either side of end date of fomc meeting
    rets.window <- window(x = rets.evt$z.e, start = -window, end = window)
    # calculate cumulative return over entire window
    rets.cum <- remap.cumprod(rets.window, is.pc = FALSE, is.returns = TRUE, 
                              base = 1) 
    mean.rets.cum <- (rowMeans(rets.cum, na.rm = TRUE) - 1) * 100
    # plot event study chart
    plot(-window:window, mean.rets.cum, type = "l", lwd = 1, 
         xlab = "Days Relative to Announcement Date", 
         ylab = "Cumulative Excess Returns (%)", 
         main = paste("Cumulative SPY Excess Returns Around FOMC Announcements"), 
         xaxt = "n")
    axis(1, at = seq(-window, window, by = 1))
    points(-window:window, mean.rets.cum)
    text(-window:window, mean.rets.cum, round(mean.rets.cum, 2), 
         adj = c(-0.25, 1), cex = 0.7)
    abline(v = 0, h = 0)
    abline(v = -1, lty = 2, col = "blue")    
}

plot.es(ins.dates, ins.xs.rets, 5)

################################################################################
# in-sample summary statistics and plot fomc returns                           #
################################################################################
stargazer(ins.xs.fomcrets, type = "text", summary = TRUE, digits = 4, title =
              "Summary Statistics for In-sample FOMC Dates")
stargazer(ins.xs.nonfomcrets, type = "text", summary = TRUE, digits = 4, title =
              "Summary Statistics for In-sample Non-FOMC Dates")
# plot fomc returns
plot(index(ins.xs.fomcrets), as.numeric(ins.xs.fomcrets), type = "h", main = 
         "SPY Excess Returns on FOMC Dates", ylab = "Excess Return", xlab = "")
abline(h = 0)
abline(h = mean(ins.xs.fomcrets), col = "blue")
legend("topleft", paste("mean = ", round(mean(ins.xs.fomcrets), 4)), lty = 1, 
       col = "blue", cex = 0.8)

################################################################################
# in-sample test for normality                                                 #
################################################################################
shapiro.test(coredata(ins.xs.fomcrets))
qqnorm(coredata(ins.xs.fomcrets), main = "Normal Q-Q Plot for SPY Excess Returns 
       on FOMC Dates")
qqline(coredata(ins.xs.fomcrets))

################################################################################
# in-sample mean confidence interval by bootstrap                              #
################################################################################
mean.fun <- function(d, i) {
    m <- mean(d[i])
    n <- length(i)
    v <- (n-1) * var(d[i]) / n
    c(m, v)
}

# perform bootstrap
set.seed(8)
boot.mean <- boot(coredata(ins.xs.fomcrets), mean.fun, R = 999)
boot.ci(boot.mean, type = c("all"))
# compare with Student's t-Test (for reference)
t.test(ins.xs.fomcrets)

################################################################################
# in-sample difference in means test by bootstrap                              #
################################################################################
# difference in means function for bootstrap
diff.means <- function(d, f) {
    n <- nrow(d)
    idx1 <- 1:table(as.numeric(d$fomc))[2]
    idx2 <- seq(length(idx1) + 1, n)
    m1 <- sum(d[idx1,1] * f[idx1])/sum(f[idx1])
    m2 <- sum(d[idx2,1] * f[idx2])/sum(f[idx2])
    ss1 <- sum(d[idx1,1]^2 * f[idx1]) - (m1^2 * sum(f[idx1]))
    ss2 <- sum(d[idx2,1]^2 * f[idx2]) - (m2^2 * sum(f[idx2]))
    c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}

# create stratified in-sample data for bootstrap
ins.xs.rets.boot <- data.frame(rets = c(coredata(ins.xs.fomcrets), 
                                        coredata(ins.xs.nonfomcrets)), fomc = c(rep(1, nrow(ins.xs.fomcrets)),
                                                                                rep(0, nrow(ins.xs.nonfomcrets))))
# perform bootstrap
set.seed(1)
(boot.diffmean <- boot(ins.xs.rets.boot, diff.means, R = 999, stype = "f", 
                       strata = ins.xs.rets.boot[,2]))
# get 95% confidence interval using the studentized method
boot.ci(boot.diffmean, type = c("stud"))
# compare with Student's t-Test (for reference)
t.test(ins.xs.fomcrets, ins.xs.nonfomcrets)

################################################################################
# out-of-sample event study                                                    #
################################################################################
# using event study function
plot.es(oos.dates, oos.xs.rets, 5)

################################################################################
# out-of-sample summary statistics and plot fomc returns                       #
################################################################################
stargazer(oos.xs.fomcrets, type = "text", summary = TRUE, digits = 4, title =
              "Summary Statistics for Out-of-sample FOMC Dates")
stargazer(oos.xs.nonfomcrets, type = "text", summary = TRUE, digits = 4, title =
              "Summary Statistics for Out-of-sample Non-FOMC Dates")
# plot fomc returns
plot(index(oos.xs.fomcrets), as.numeric(oos.xs.fomcrets), type = "h", main = 
         "SPY Excess Returns on FOMC Dates", ylab = "Excess Return", xlab = "")
abline(h = 0)
abline(h = mean(oos.xs.fomcrets), col = "blue")
legend("topright", paste("mean = ", round(mean(oos.xs.fomcrets), 4)), lty = 1, 
       col = "blue", cex = 0.8)

################################################################################
# out-of-sample mean confidence interval by bootstrap                          #
################################################################################
# perform bootstrap
set.seed(7)
boot.mean1 <- boot(coredata(oos.xs.fomcrets), mean.fun, R = 999)
boot.ci(boot.mean1, type = c("all"))
# compare with Student's t-Test (for reference)
t.test(oos.xs.fomcrets)

################################################################################
# out-of-sample difference in means test by bootstrap                          #
################################################################################
# create stratified out-of-sample data for bootstrap
oos.xs.rets.boot <- data.frame(rets = c(coredata(oos.xs.fomcrets), 
                                        coredata(oos.xs.nonfomcrets)), fomc = c(rep(1, nrow(oos.xs.fomcrets)),
                                                                                rep(0, nrow(oos.xs.nonfomcrets))))
# perform bootstrap
set.seed(2)
(boot.diffmean1 <- boot(oos.xs.rets.boot, diff.means, R = 999, stype = "f", 
                        strata = oos.xs.rets.boot[,2]))
# get 95% confidence interval using the studentized method
boot.ci(boot.diffmean1, type = c("stud"))

################################################################################
# ballpark estimate of out-of-sample size required                             #
################################################################################
# calc t-stat from in-sample assuming normality
t.stat <- as.numeric(t.test(ins.xs.fomcrets, ins.xs.nonfomcrets)$statistic)
# calc effect size of in-sample data
effect.size <- tes(t.stat, nrow(ins.xs.fomcrets), nrow(ins.xs.nonfomcrets))$d
# rough estimate of oos size fomc meetings (by trial and error changing n2 value)
num.fomcs <- pwr.t2n.test(d = effect.size, power = 0.80, n2 = 3660, sig.level = 
                              0.05, alternative = "two.sided")$n1
(oos.num.years <- round(num.fomcs / 8))


