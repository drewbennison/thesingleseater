#First, make Indy worth only regular points - same for final race

library(data.table)
library(tidyverse)
library(ggplot2)

data<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/indycar_results.csv")
ptd<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/part_time_drivers.csv")


num_oval <- 5
num_roadandstreet <- 12

eligible_drivers <- data %>% 
  filter(year==2019) %>% 
  group_by(driver) %>% 
  count() %>% 
  filter(n<17)

driver_season_stats <- data %>%
  filter(year==2019) %>%
  mutate(NewPoints = ifelse(track == "Indianapolis", pts/2, pts)) %>% 
  mutate(NewPoints = ifelse(track == "Laguna Seca", pts/2, NewPoints)) %>%
  left_join(eligible_drivers) %>% 
  filter(!is.na(n)) %>% 
  select(driver, type, NewPoints) %>% 
  mutate(type=ifelse(type=="oval", "oval", "roadandstreet")) %>% 
  group_by(driver, type) %>% 
  summarise(Races = n(),
         Pts=sum(NewPoints))

final_standings <- driver_season_stats %>% 
  mutate(pointsPer = Pts/Races) %>% 
  mutate(PerSeason = ifelse(type=="oval", pointsPer*num_oval, pointsPer*num_roadandstreet)) %>% 
  group_by(driver) %>% 
  summarise(PointsPer17 = sum(PerSeason)) %>% 
  left_join(eligible_drivers)

final_standings <- setnames(final_standings, c("driver", "PointsPer17", "n"), c("Driver", "Theoretical Full Season Points", "Actual # of 2019 Races"))

`%notin%` <- Negate(`%in%`)

final_standings <- final_standings %>% 
  left_join(ptd) %>% 
  filter(Driver %notin% c("Ed Carpenter", "J.R. Hildebrand", "James Davison", "Jordan King", "Max Chilton",
                          "Oriol Servia", "Patricio O'Ward", "Pippa Mann", "R.C. Enerson")) %>% 
  arrange(-`Theoretical Full Season Points`) 

fwrite(final_standings, "article.csv")
