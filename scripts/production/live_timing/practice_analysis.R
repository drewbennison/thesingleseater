library(tidyverse)
library(data.table)
library(lubridate)
library(ggthemes)

dt <- fread("C:/Users/drewb/Desktop/2022_02_26_p2.csv")

dt2 <- dt %>% select(rank, diff, gap, overallRank, startPosition, bestLapTime, lastLapTime, laps, totalTime,
                     status, pitStops, sincePitLap, flagStatus, no, firstName, lastName, LastSpeed, 
                     time_stamp, BestSpeed, NTBestSpeed) %>% 
  mutate(time_stamp = as_datetime(time_stamp)) %>% 
  group_by(lastName, laps) %>% 
  top_n(1, time_stamp) %>% 
  ungroup()

dt3 <- dt2 %>% group_by(lastName) %>% 
  top_n(1, BestSpeed) %>% 
  select(lastName, BestSpeed) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(sessionRank = rank(BestSpeed))

dt3 <- dt2 %>% 
  left_join(dt3, by="lastName")
  
dt3 %>% 
  filter(LastSpeed >=100) %>% 
  rename("Driver" = "lastName",
         "Speed" = "LastSpeed") %>% 
  ggplot(aes(x=reorder(Driver,sessionRank), y=Speed)) + 
  geom_boxplot() +
  geom_point() +
  #geom_dotplot(binaxis='y', stackdir='center',
     #          position=position_dodge(1))
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=.5) +
  coord_flip() +
  labs(x="", title = "Full speed laps, IndyCar St. Petersburg Practice 2",
       y="Speed (mph)",
       caption = "@thesingleseater") +
  theme_solarized()
  
ggsave("C:/users/drewb/Desktop/plot2.png", width = 6, height = 5)

#diff = total time - leaders total time at same lap
#one lap for each driver
dt3 <- dt2 %>% group_by(lastName, laps) %>% 
  top_n(1, time_stamp) %>% 
  ungroup()

#sincepitlap = 0 is the in lap time
#sincepitlap = 1 is the out lap time
sub <- dt3 %>% filter(laps>=185, laps <=190, lastName %in% c("O'Ward", "Newgarden")) %>% 
  mutate(LastSpeedNew = as.numeric(LastSpeed),
         LapTimeNew = 1.44/LastSpeedNew*60*60,
         totalTime =  (hms(totalTime)),
         racingTime = hour(totalTime)*60*60 + minute(totalTime)*60+second(totalTime))


sub %>% ggplot(aes(x=laps, y=racingTime, color=lastName)) + geom_line()




