# Clear Workspace
rm(list=ls())

# load packages
library(formatR)
library(nbastatR)
library(mvoutlier)
library(Rtsne)
library(ggplot2)

# Read Data
bDat <- bref_players_stats(seasons = 2017, tables = c("advanced", "per_minute", "totals"),
                   only_totals = FALSE, join_data = TRUE, assign_to_environment = FALSE)
bDat <- na.omit(bDat)
bDatreduc <- bDat[,c("minutes", "countGames", "agePlayer", "ptsPerMinute", "pfPerMinute",
                     "tovPerMinute", "blkPerMinute", "stlPerMinute", "astPerMinute", "trbPerMinute")]
bDatreduc <- bDat[,c("minutes", "countGames", "agePlayer", "ptsTotals", "pfTotals", "tovTotals",
                     "stlTotals", "blkTotals", "astTotals", "trbTotals")]
# Apply t-SNE dimension reduction
set.seed(1234)
rtsneDat <- Rtsne(bDatreduc, theta = 0, perplexity = 10, check_duplicates =TRUE, num_threads=3)

plotDat <- cbind(bDat, rtsneDat$Y)
ggplot(plotDat, aes(x=`1`,y=`2`,col=ratioPER))+geom_point()
ggplot(plotDat, aes(x=`1`,y=`2`,label=namePlayer, col = ratioPER))+geom_text(check_overlap = FALSE)

# Find multivariate outliers for per minute data
smallDat <- bDat[,c("namePlayer","minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")]
multOut <- pcout(bDat[,c("minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")],
                 makeplot = TRUE)
smallDat <- cbind(smallDat,multOut[1:2])

chisq.plot(bDat[,c("minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")],ask=FALSE,
           quan=1)
aq.plot(bDat[,c("minutes","ptsPerMinute","blkPerMinute","stlPerMinute","astPerMinute","trbPerMinute")])
