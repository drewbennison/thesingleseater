library(data.table)
library(tidyverse)
library(lubridate)
library(png)

dt <- fread("C:/Users/drewb/Desktop/mid_ohio_2021.csv")

#lap speed 
dt %>% select(-time_stamp) %>% 
  select(lastName, LastSpeed, diff, gap, overallRank, startPosition, laps, totalTime) %>% 
  unique() %>% 
  filter(laps>0) %>% 
  mutate(LastSpeedNew = as.numeric(LastSpeed),
         LapTimeNew = 1.44/LastSpeedNew*60*60) %>% 
  filter(lastName %in% c("Dixon", "Rahal", "Newgarden", "O'Ward"), laps > 185) %>% 
  rename(lap = laps) %>%
  rename(lap_time = LapTimeNew) %>% 
  ggplot(aes(x=lap, y=lap_time, color=lastName)) + geom_line() +
  labs(title = "Lap Time") +
  theme_bw() +
  ylim(23.5, 26)


ggsave("./plot.png", width = 7, height = 4, dpi = 500)

#gap of two drivers - only works if one is the leader
dt %>% 
  select(lastName, LastSpeed, diff, gap, overallRank, startPosition, laps, totalTime) %>% 
  unique() %>% 
  filter(lastName %in% c("O'Ward", "Newgarden", "Dixon", "Rahal"), laps>200) %>% 
  mutate(diff = -1 * as.numeric(diff)) %>% 
  filter(!is.na(diff), laps>72) %>% 
  rename(lap = laps) %>%
  ggplot(aes(x=lap, y=diff, color=lastName)) + geom_line() +
  labs(title = "Gap to leader") +
  theme_bw() +
  ylim(-10, 0)
  

#static gap
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

