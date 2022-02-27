library(data.table)
library(tidyverse)
library(lubridate)
library(png)
library(ggthemes)

dt <- fread("C:/Users/drewb/Desktop/2022_02_27_r.csv")

#lap speed 
dt %>% select(-time_stamp) %>% 
  select(lastName, LastSpeed, diff, gap, overallRank, startPosition, laps, totalTime, lastLapTime) %>% 
  unique() %>% 
  filter(laps>79) %>% 
  mutate(LastSpeedNew = as.numeric(LastSpeed),
         LapTimeNew = 1.8/LastSpeedNew*60*60) %>% 
  filter(lastName %in% c("McLaughlin", "Herta", "Palou", "Power", "VeeKay")) %>%
  rename(Lap = laps) %>%
  rename(`Lap Time` = LapTimeNew) %>% 
  rename(speed = LastSpeed) %>% 
  rename(Driver = lastName) %>% 
  #filter(lap_time < 120) %>% 
  ggplot(aes(x=Lap, y=`Lap Time`, color=Driver)) + geom_line() +
  labs(title = "Lap time by driver, Firestone GP at St. Petersburg",
       y= "Lap time (seconds)") +
  theme_bw() +
  labs(caption = "@thesingleseater | thesingleseater.com")


ggsave("C:/Users/drewb/Desktop/gap.png", width = 7, height = 4, dpi = 500)

#gap of two drivers - only works if one is the leader
dt %>% 
  select(lastName, LastSpeed, diff, gap, overallRank, startPosition, laps, totalTime) %>% 
  unique() %>% 
  filter(lastName %in% c("McLaughlin", "Herta", "Palou", "Power", "VeeKay")) %>% 
  mutate(diff = -1 * as.numeric(diff)) %>% 
  filter(!is.na(diff), laps>79) %>% 
  rename(Lap = laps) %>%
  rename(Driver = lastName) %>% 
  ggplot(aes(x=Lap, y=diff, color=Driver)) + geom_line() +
  labs(title = "Gap to leader, Firestone GP at St. Petersburg",
       caption = "@thesingleseater | thesingleseater.com",
       y="Gap to leader (seconds)") +
  theme_bw() +
  ylim(-15, 0)
  
ggsave("C:/Users/drewb/Desktop/diff.png", width = 7, height = 4, dpi = 500)


#static gap of all drivers
dt %>% select(lastName, diff, time_stamp) %>% 
  mutate(time_stamp = as_datetime(time_stamp),
         diff = as.numeric(diff)) %>% 
  group_by(lastName) %>% 
  slice(which.max(time_stamp)) %>% 
  filter(diff < 10, diff > -10) %>% 
  ggplot(aes(y=reorder(lastName, -diff), x=diff)) +
  geom_col() +
  labs(x="Gap to fastest driver (seconds)",
       y="",
       title = "Single lap time") +
  theme_bw()
  
ggsave("C:/Users/drewb/Desktop/plot.png", dpi = 800, height = 6, width = 8)



#push to pass remaining

