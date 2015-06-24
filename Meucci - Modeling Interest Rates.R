################################################################################
# Modeling Interest Rates Meucci Style
# R code for 24 June 2015 post on www.returnandrisk.com:
# http://www.returnandrisk.com/2015/06/modeling-interest-rates-meucci-style.html
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
# load packaages                                                               #
################################################################################
library(RCurl)
library(xts)
library(matlab)
library(pracma)
library(reshape2)
library(ggplot2)
library(gridExtra)

################################################################################
# get JGB data from file                                                       #
################################################################################
csvfile = getURLContent(
    "https://docs.google.com/uc?export=download&id=0B4oNodML7SgSZGdVQXYxVWRwMW8",
    ssl.verifypeer = FALSE, followlocation = TRUE, binary = FALSE)
JGB <- read.zoo(textConnection(csvfile), format = "%Y-%m-%d", index = 1,
       header = TRUE, tz = "UTC", sep = ",")
JGB <- as.xts(JGB)

# process data
tau <- as.numeric(gsub("Y", "", names(JGB)[c(1, 2, 3, 5, 7, 10, 11, 12)]))

# rates
y <- coredata(JGB[, c(1, 2, 3, 5, 7, 10, 11, 12)])
Date <- index(JGB)
TimeStep <- 5 # daily (1) / weekly (6) observations
date <- Date[seq(1, NROW(Date), TimeStep)]
y <- t(y[seq(1, NROW(Date), TimeStep), ])

################################################################################
# Inverse Call Transformation function
################################################################################
# this function computes the Inverse Call Transformation and returns shadow 
# rates see A. Meucci, A. Loregian - "Neither "Normal" not "Lognormal": Modeling
# Interest Rates Across all Regimes" to appear (2013)
# Last version of code and article available at http://symmys.com/node/601
#
# INPUT
# rates: matrix containing the time series of the rates to be transformed; 
#        size(rates)= lenght(tau) x t_, where t_ is the length of the time series
# tau: vector containing the times to maturity corresponding to the rows of the 
#      rates matrix
# eta, zeta: Inverse-call transformation parameters (the smoothing parameter s 
#            is obtained as s=eta*exp(zeta*tau))
#
# OUTPUT
# x: shadow rates, computed from rates via inverse-call transformation
################################################################################
InverseCallTransformation <- function(rates, tau, eta, zeta) {
    t_=size(rates,2);
    x=zeros(size(rates)[1], size(rates)[2]); 
    s=eta*exp(zeta*tau);
    
    # Bachelier call pricing function
    BachelierCallPrice <- function(x,s) {
        # Call function (zero-strike call option price profile according to the 
        # Bachelier pricing function)
        # s: smoothing parameter 
        C = x * pnorm(x/s) + s * dnorm(x/s);
    }
    
    call_fit <- function(tmpX,y,sigma) {
        c=BachelierCallPrice(tmpX,sigma);
        F= y - c;
    }
    
    for (v in 1:length(tau)) {
        # inverse call transformation
        # Optimization options. 
        opts <- list(tau = 0.01, tolx = 1e-10, tolg = 1e-10, maxeval = 7*500)
        x0=0; #initialization
        for (t in 1:t_) {
            # fitting inverse call
            x[v,t] <- lsqnonlin(call_fit, x0, opts, y = rates[v,t], sigma = s[v])$x;
        }
    }
    return(x)
}

################################################################################

# shadow rates, via Inverse Call Transformation
eta <- 0.005;
zeta <- 0;
icy <- InverseCallTransformation(y, tau, eta, zeta);

# log-rates
lny <- log(y)

# rate changes
dy <- diff(t(y));
dlny <- diff(t(log(y)));
dx <- diff(t(icy));

################################################################################
# plot rates/log-rates/shadow-rates                                            #
################################################################################
fmt <- function(x) {
    format(x, nsmall = 2, scientific = FALSE)
}

get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

y_df <- melt(data.frame(date = as.Date(date), t(y)), id.vars = "date", 
             variable.name = "Maturity")
gp <- ggplot(y_df, aes(x = date, y = value)) +
    geom_line(aes(colour = Maturity)) + 
    labs(list(title = "JGB Rates", x = "", y = "")) + 
    scale_x_date(expand = c(0, 0)) + 
    theme(legend.position = "bottom") +
    theme(plot.margin = unit(c(0, .5, -0.5, .5), "lines"))


lny_df <- melt(data.frame(date = as.Date(date), t(lny)), id.vars = "date",
               variable.name = "Maturity")
gp1 <- ggplot(lny_df, aes(x = date, y = value)) +
    geom_line(aes(colour = Maturity)) + 
    labs(list(title = "JGB Log-Rates", x = "", y = "")) + 
    scale_x_date(expand = c(0, 0)) + 
    theme(legend.position="none") +
    theme(plot.margin = unit(c(0, .5, -0.5, .5), "lines")) +
    scale_y_continuous(labels = fmt)

icy_df <- melt(data.frame(date = as.Date(date), t(icy)), id.vars = "date",
               variable.name = "Maturity")
gp2 <- ggplot(icy_df, aes(x = date, y = value)) +
    geom_line(aes(colour = Maturity)) + 
    labs(list(title = "JGB Shadow Rates", x = "", y = "")) + 
    scale_x_date(expand = c(0, 0)) + 
    theme(legend.position = "none") +
    theme(plot.margin = unit(c(0, .5, -0.5, .5), "lines"))

legend <- get_legend(gp)
gp <- gp + theme(legend.position="none")
grid.arrange(gp, gp1, gp2, legend, nrow=4, heights=c(3, 3, 3, 0.5))
