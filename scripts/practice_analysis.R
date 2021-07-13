library(tidyverse)
library(data.table)
library(lubridate)

dt <- fread("C:/Users/drewb/Desktop/2021_r4_texas_api.csv")

dt2 <- dt %>% select(rank, diff, gap, overallRank, startPosition, bestLapTime, lastLapTime, laps, totalTime,
                     status, pitStops, sincePitLap, flagStatus, no, firstName, lastName, LastSpeed, 
                     time_stamp, BestSpeed, Passes, Passed, NTBestSpeed) %>% 
  mutate(time_stamp = as_datetime(time_stamp))

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
