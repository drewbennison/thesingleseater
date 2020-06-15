library(data.table)
library(tidyverse)
library(ggplot2)
library(gganimate)

data<- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

#Calculate AFP from every starting position
afp <- data %>%
  group_by(st) %>% 
  summarise(xFP = mean(fin))

#Left join data with xFP for every driver's results
data <- data %>%
  left_join(afp, by=c("st" = "st")) %>% 
  mutate(xFPDifference=xFP-fin)

working <- data %>%
  filter(year==2019) %>%
  group_by(driver) %>% 
  mutate(percFavorablePass = 100*(sum(passesFor)/(sum(passesFor)+sum(passesAgainst))),
         favorableStart = ifelse(lapOneChange>=0, 1,
                                 ifelse(lapOneChange<0, 0, NA)),
         StartRetention = 100*mean(favorableStart),
         StartPM = sum(lapOneChange),
         Races = n(),
         PMperStart = StartPM/Races,
         Pts=sum(pts),
         xPoints = sum(xPts),
         AFP = mean(fin),
         DevFP = sd(fin),
         ASP = mean(st),
         DevSP = sd(st),
         ATP = mean(atp),
         DevATP = sd(atp),
         ATP25 = mean(atp25),
         DevATP25 = sd(atp25),
         PassEff = 100*(sum(passesFor)/ (sum(passesFor)+sum(passesAgainst))),
         RunningCheck = ifelse(status=="running",1,0),
         RunPerc = 100*mean(RunningCheck),
         AFS = mean(fastLapRank),
         Top5Perc = 100*(sum(inTopFive)/sum(laps)),
         #Average Surplus Position
         AEP = mean(xFPDifference)) %>% 
  #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
  distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, RunPerc, Top5Perc, AEP, AFS)

#Normalize points and xpoints to a per race level
working$Pts = working$Pts/working$Races
working$xPoints = working$xPoints/working$Races

#Get ranks
working$ATP_Rank=37-(rank(working$ATP))
working$ATP25_Rank=37-(rank(working$ATP25))
working$ASP_Rank = 37-(rank(working$ASP))
working$PassEff_Rank=rank(working$PassEff)
working$Top_5_Perc_Rank=rank(working$Top5Perc)
working$Pts_Rank=rank(working$Pts)
working$xPts_Rank=rank(working$xPoints)
working$AEP_Rank=rank(working$AEP)
working$AFS_Rank=37-(rank(working$AFS))


working <- working %>% 
  select(driver, ATP_Rank, AEP_Rank, PassEff_Rank, Top_5_Perc_Rank, Pts_Rank, xPts_Rank, ASP_Rank, AFS_Rank)

setnames(working, c("ATP_Rank", "AEP_Rank", "PassEff_Rank", "Top_5_Perc_Rank", "Pts_Rank", "xPts_Rank", "ASP_Rank", "AFS_Rank"),
         c("ATP", "AEP", "Pass Eff.", "Top5 %", "Points/Race", "xPts/Race", "ASP", "AFS"))

fwrite(working, "working.csv")
