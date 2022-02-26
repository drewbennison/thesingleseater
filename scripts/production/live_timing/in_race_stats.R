library(data.table)
library(tidyverse)
library(lubridate)
library(png)

dt <- fread("C:/Users/drewb/Desktop/nashville_2021.csv")

#lap speed 
dt %>% select(-time_stamp) %>% 
  select(lastName, LastSpeed, diff, gap, overallRank, startPosition, laps, totalTime) %>% 
  unique() %>% 
  filter(laps>0) %>% 
  mutate(LastSpeedNew = as.numeric(LastSpeed),
         LapTimeNew = 2.17/LastSpeedNew*60*60) %>% 
  filter(lastName %in% c("Dixon", "Herta", "Rossi")) %>%
  rename(lap = laps) %>%
  rename(lap_time = LapTimeNew) %>% 
  rename(speed = LastSpeed) %>% 
  #filter(lap_time < 120) %>% 
  ggplot(aes(x=lap, y=speed, color=lastName)) + geom_line() +
  labs(title = "Lap Time") +
  theme_bw()


ggsave("C:/Users/drewb/Desktop/gap.png", width = 7, height = 4, dpi = 500)

#gap of two drivers - only works if one is the leader
dt %>% 
  select(lastName, LastSpeed, diff, gap, overallRank, startPosition, laps, totalTime) %>% 
  unique() %>% 
  filter(lastName %in% c("Herta", "Ericsson", "Dixon")) %>% 
  mutate(diff = -1 * as.numeric(diff)) %>% 
  filter(!is.na(diff), laps>59) %>% 
  rename(lap = laps) %>%
  ggplot(aes(x=lap, y=diff, color=lastName)) + geom_line() +
  labs(title = "Gap to leader") +
  theme_bw() +
  ylim(-15, 0)
  

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

