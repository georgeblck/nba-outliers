# Clear Workspace
rm(list=ls())

# load packages
library(formatR)
library(nbastatR)
library(ggplot2)
library(ggridges)
library(reshape2)

# Get per game data for the 2017 season
boxScores <- game_logs(seasons = 2018, result_types = "player", league = "NBA", assign_to_environment = FALSE,
                 season_types = "Regular Season")

# Category Vector
catVec <- c("pts", "treb", "ast", "blk", "stl")
boxScores$sumCat <- rowSums(boxScores[,catVec], na.rm=TRUE)

# Look at the quantiles of each categories
apply(boxScores[,c(catVec,"sumCat")], 2, FUN = function(x){
  quantile(x, probs = c(seq(0,0.9,0.1),0.95,0.99,0.999,1))
})


############## Visualize distributions 
# Melt data
boxScores_long <- melt(boxScores, id.vars = c("idGame", "namePlayer"), measure.vars = catVec,
                       factorsAsStrings = FALSE, value.name = "value", variable.name = "cat")

# With ECDF
ggplot(boxScores_long, aes(x = value)) + stat_ecdf(aes(color = cat), 
              geom = "step", size = 1) + xlab("Count per Category")+ylab("Percentiles")+
  scale_colour_brewer(palette = "Spectral",name = "Category", labels = c("Points", "Rebounds", "Assists", "Blocks", "Steals"))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0), 
        legend.background = element_rect(fill = "white", size = 1, linetype = "solid", colour = "black"), 
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  geom_vline(xintercept = 10, alpha = 0.8)

# With ridges
ggplot(boxScores_long, aes(x = value, y = cat)) +
  geom_density_ridges(aes(fill = cat)) + geom_vline(xintercept = 10)+
  scale_fill_brewer(palette = "Spectral",name = "Category", labels = c("Points", "Rebounds", "Assists", "Blocks", "Steals"))

# with boxplots
ggplot(boxScores_long, aes(y=value, x=cat, fill = cat)) + 
  geom_boxplot()+
  scale_fill_brewer(palette = "Spectral",name = "Category", labels = c("Points", "Rebounds", "Assists", "Blocks", "Steals"))

########## Häufigkeit triple double
