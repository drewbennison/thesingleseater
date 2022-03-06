library(indyscrapR)
library(data.table)
library(tidyverse)

dt <- data.table

dt <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

dt1 <- dt %>% mutate(did_not_finish = ifelse(tolower(status) != "running", 1, 0)) %>% 
  group_by(year, driver) %>% 
  select(year, driver, did_not_finish) %>% 
  add_count() %>% 
  filter(n > 10) %>% 
  summarise(percDNF = mean(did_not_finish))

dt2 <- dt %>% mutate(did_not_finish = ifelse(tolower(status) != "running", 1, 0)) %>% 
  group_by(year, driver) %>% 
  select(year, driver, did_not_finish) %>% 
  add_count() %>% 
  filter(n > 10) %>% 
  summarise(percDNF = mean(did_not_finish)) %>% 
  mutate(year=year-1)

dt3 <- dt1 %>% left_join(dt2, by=c("year", "driver")) %>% 
  filter(!is.na(percDNF.x), !is.na(percDNF.y)) 

sato <- dt3 %>% filter(driver=="Takuma Sato")

cor(sato$percDNF.x, sato$percDNF.y)

dt3 %>% 
  ggplot(aes(x=percDNF.x, y=percDNF.y)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = .55, y = .3, label = "r = 0.32") +
  labs(x="Percentage of races DNF year X", 
        y="Percentage of races DNF year X+1",
        title = "Year to year correlation of the percentage of races a driver does not finish (DNF)",
       caption = "@thesingleseater | thesingleseater.com") +
  theme_bw()




#at the race level
dt4 <- dt %>% mutate(did_not_finish = ifelse(tolower(status) != "running", 1, 0)) %>% 
  select(raceNumber, year, driver, did_not_finish)

dt5 <- dt %>% mutate(did_not_finish = ifelse(tolower(status) != "running", 1, 0)) %>% 
  select(raceNumber, year, driver, did_not_finish) %>% 
  mutate(raceNumber= raceNumber-1)

dt6 <- dt4 %>% left_join(dt5, by=c("year", "driver", "raceNumber")) %>% 
  filter(!is.na(did_not_finish.x), !is.na(did_not_finish.y)) 

cor(dt6$did_not_finish.x, dt6$did_not_finish.y)

#dt6 %>% 
#  ggplot(aes(x=did_not_finish.x, y=did_not_finish.y)) + geom_jitter(width = .1, height = .1) +
#  geom_smooth(method = "lm", se = FALSE) +
#  annotate("text", x = .55, y = .3, label = "r = 0.32") +
#  labs(x="DNF in race X", 
#       y="DNF in race X+1",
#       title = "Race to race correlation of driver did not finish (DNF) status",
#       caption = "@thesingleseater | thesingleseater.com") +
#  theme_bw()

#at race level
dt6 %>% group_by(did_not_finish.x) %>% 
  summarise(race_two_dnf_probability = mean(did_not_finish.y))

dt6 %>% filter(driver == "Takuma Sato") %>% group_by(did_not_finish.x) %>% 
  summarise(race_two_dnf_probability = mean(did_not_finish.y))



#career DNF percenrage
dt3 %>% group_by(driver) %>% 
  add_count() %>% 
  filter(n>5) %>% 
  summarise(career_dnf_percentage = mean(percDNF.x)) %>% 
  arrange(-career_dnf_percentage)
  
