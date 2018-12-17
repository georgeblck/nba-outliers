# Clear Workspace
rm(list=ls())

# load packages
library(formatR)
library(nbastatR)
library(mvoutlier)

# Read Data
bDat <- bref_players_stats(seasons = 2017, tables = c("per_game","advanced", "per_poss", "per_minute"),
                   only_totals = FALSE, join_data = TRUE, assign_to_environment = FALSE)


# Find multivariate outliers for per minute data
smallDat <- bDat[,c("namePlayer","minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")]
multOut <- pcout(bDat[,c("minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")],
                 makeplot = TRUE)
smallDat <- cbind(smallDat,multOut[1:2])

chisq.plot(bDat[,c("minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")],ask=FALSE,
           quan=1)
aq.plot(bDat[,c("minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")])
