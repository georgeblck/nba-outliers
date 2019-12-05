# Clear Workspace
rm(list = ls())

# load packages
library(formatR)
library(nbastatR)
library(ggplot2)
library(ggridges)
library(reshape2)
library(questionr)
library(mvoutlier)
library(tidyverse)
library(lubridate)
library(stringi)
library(ggthemes)
library(viridis)
library(ggrepel)

# Remove playoffs and apply Games restriction
# Make merge better (without distinct)
# Make one with general tripDoub


# Get BoxScores
boxScores <- read.table("trippleData.csv", sep = ";", dec = ".", header = TRUE, 
                        stringsAsFactors = FALSE) %>% mutate(ldate = ymd(dateGame), gamemonth = month(ldate))
# Get Old ELO Data
eloAll <- read.table("https://projects.fivethirtyeight.com/nba-model/nba_elo.csv", 
                     header = TRUE, sep = ",", dec = ".", na.strings = "", stringsAsFactors = FALSE) %>% 
  drop_na(score1) %>% mutate(ldate = ymd(date), gamemonth = month(ldate))
#eloNew <- read.table("https://projects.fivethirtyeight.com/nba-model/nba_elo_latest.csv", 
#                     sep = ",", dec = ".", na.strings = "", header = TRUE, stringsAsFactors = FALSE) %>% drop_na(score1)
#eloAll <- bind_rows(eloOld, eloNew) 
# Double the data
eloDatRe <- eloAll
colnames(eloDatRe) <- stri_replace_all_fixed(colnames(eloDatRe),
                                             c(1,2), c(2,1), mode = "first")
eloList <- list(eloAll, eloDatRe)
eloAll <- data.table::rbindlist(eloList, use.names=TRUE, idcol=TRUE)
rm(eloList)

# trip month
tripMonth <- boxScores %>% add_count(idPlayer, slugSeason, gamemonth, name = "numgames") %>%
  group_by(idPlayer, slugSeason, gamemonth) %>%
  summarise_at(c("pts", "ast", "treb", "blk", "stl", "numgames"), mean, na.rm = TRUE) %>% 
  filter(pts >= 30, ast >= 10, treb >= 10) %>%
  arrange(-numgames) %>% filter(numgames >= 5) 

# join important player data
playDat <- boxScores %>% left_join(tripMonth, by = c("idPlayer", "slugSeason", "gamemonth"), suffix = c("", ".mean")) %>%
  drop_na(numgames) %>% select(yearSeason, ldate, gamemonth, namePlayer, 
                               countDaysRestPlayer, numberGamePlayerSeason, slugTeam, plusminus, isWin) %>%
  rename(season = yearSeason, team1 = slugTeam)
print(nrow(playDat) == sum(tripMonth$numgames))


# Transform Elo
eloSmall <- eloAll %>% 
  left_join(playDat, by = c("ldate", "team1"), suffix = c("", ".mean")) %>% drop_na(isWin)

# Combine Elo and Other Data
wat <- eloSmall %>% group_by(namePlayer, season, gamemonth) %>% 
  summarise_at(c("elo1_pre","elo2_pre", "isWin","countDaysRestPlayer", 
                 "numberGamePlayerSeason", "carm.elo1_pre", "carm.elo2_pre",
                 "raptor1_pre", "raptor2_pre"), mean, na.rm = TRUE) %>%
  arrange(elo2_pre) 
print(wat)
wat %>% ggplot(aes(x=elo1_pre, y = elo2_pre, label = namePlayer))+geom_label()+
  xlab("Strength of Team")+ylab("Strength of Opponent")



# Make one for Russell
densDat <- eloAll %>% group_by(season, team1, gamemonth) %>% 
  summarise_at(c("elo1_pre","elo2_pre"), mean, na.rm = TRUE) %>% ungroup() %>%
  left_join(playDat, by = c("season", "team1", "gamemonth")) %>% 
  distinct(season, team1, gamemonth, .keep_all = TRUE) 

yas <- ggplot(densDat, aes(x=elo1_pre,y=elo2_pre)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  xlab("Strength of Team")+ylab("Strength of Opponents")+theme_tufte(base_size = 13)+theme(legend.position="none")+
  geom_point(alpha=0.04)+geom_rangeframe()+
  geom_label_repel(data = subset(densDat, elo2_pre > 1480), 
                   aes(x=elo1_pre, y = elo2_pre,label=namePlayer, colour = namePlayer), 
                   nudge_y = 200, force = 10, direction = "x", segment.size = 1.1)+
  geom_label_repel(data = subset(densDat, elo2_pre <= 1480), 
                   aes(x=elo1_pre, y = elo2_pre,label=namePlayer,colour = namePlayer), 
                   nudge_y = -120, force = 10, direction = "x",
                   segment.size = 1.1)+
  scale_colour_manual(values = c("black", "#5B2B82", "#007DC3"))

ggsave("out.png", width = 108, height = 135, units = "mm")


  ggplot(aes(x=elo1_pre, y=elo2_pre) ) +
  stat_density_2d(aes(fill = stat(level)),
                  geom = "polygon", 
                  n = 100 ,
                  bins = 10) + 
  scale_fill_viridis(option = "A")

  
# Make thingi
eloAll %>% group_by(season, team1, gamemonth) %>% 
  summarise_at(c("elo1_pre", "elo2_pre"), mean, na.rm = TRUE) %>% 
  ungroup() %>% filter(season == 2017) %>% mutate(wat = (gamemonth %in% 2:3) & (team1 == "OKC")) %>%
  ggplot(aes(x=elo1_pre, elo2_pre)) +
  stat_density2d(aes( fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  theme(legend.position="none")+
  geom_point(aes(colour = factor(wat), x=elo1_pre, y = elo2_pre), inherit.aes = FALSE)+
  scale_colour_manual(values = c("grey", "black"))