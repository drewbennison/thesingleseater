library(data.table)
library(tidyverse)

# Read in the data
dt<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/indycar_results.csv")
dt <- dt %>% filter(year == 2019)
dt <- dt %>% select(raceNumber, driver, pts, atp, fin, xPtsATP)

#Calculate first half stats
numFirstHalf <- dt %>% filter(raceNumber < 9) %>% 
  group_by(driver) %>% 
  count()

firstHalf <- dt %>% 
  filter(raceNumber < 9) %>% 
  group_by(driver) %>% 
  summarise(TotalPoints = sum(pts),
            AvgATP = mean(atp),
            AvgFin = mean(fin),
            TotalExpPoints = sum(xPtsATP)) %>% 
  left_join(numFirstHalf) %>% 
  filter(n>5)

#Calculate second half stats
numSecondHalf <- dt %>% filter(raceNumber >8) %>% 
  group_by(driver) %>% 
  count()

secondHalf <- dt %>% 
  filter(raceNumber > 8) %>% 
  group_by(driver) %>% 
  summarise(TotalPoints = sum(pts),
            AvgATP = mean(atp),
            AvgFin = mean(fin),
            TotalExpPoints = sum(xPtsATP)) %>% 
  left_join(numSecondHalf) %>% 
  filter(n>7)

full <- left_join(firstHalf, secondHalf, by="driver")
full <- full %>% filter(!is.na(TotalPoints.y))
full <- full[,-1]

round(cor(full), 2)









