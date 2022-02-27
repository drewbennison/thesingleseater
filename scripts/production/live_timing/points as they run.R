library(gt)


#load in data sets
current_data <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
dt <- fread("C:/Users/drewb/Desktop/mid_ohio_2021.csv")
type <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/points_table.csv")

dt2 <- dt %>% select(rank, diff, gap, overallRank, startPosition, bestLapTime, lastLapTime, laps, totalTime,
                     status, pitStops, sincePitLap, flagStatus, no, firstName, lastName, LastSpeed, 
                     time_stamp, BestSpeed, Passes, Passed, NTBestSpeed) %>% 
  mutate(time_stamp = as_datetime(time_stamp))

#diff = total time - leaders total time at same lap
#one lap for each driver
dt3 <- dt2 %>% group_by(lastName, laps) %>% 
  top_n(1, time_stamp) %>% 
  ungroup()

#calculate race stats
atp <- dt3 %>% group_by(lastName) %>% 
  summarise(atp = mean(overallRank))

max_lap <- max(dt3$laps)

atp25 <- dt3 %>% filter(laps > round(.75*max_lap)) %>%
  group_by(lastName) %>% 
  summarise(atp25 = mean(overallRank))

pole <- dt3 %>% filter(laps==0) %>% 
  mutate(polePoints = ifelse(rank==1, 1,0)) %>% 
  ungroup() %>% 
  select(lastName, polePoints)

dt4 <- dt3 %>% filter(rank==1) %>% 
  group_by(lastName) %>% count()

led_most_points <-  dt4 %>% 
  mutate(ledLapPoints = 1,
         ledMostPoints = ifelse(n == max(dt4$n), 2, 0)) %>% 
  select(-n)


in_top_five <- dt3 %>% filter(rank<6) %>% 
  group_by(lastName) %>% count()

#passes
passes <- dt3 %>% filter(!is.na(Passes), !is.na(Passed)) %>% 
  select(lastName, Passes, Passed, time_stamp) %>% 
  group_by(lastName) %>% 
  top_n(1, time_stamp) %>% 
  rename(passesFor= Passes) %>% 
  rename(passesAgainst = Passed) %>% 
  select(-time_stamp)

on_start <- dt3 %>% filter(laps==0) %>% 
  select(lastName, startPosition)

on_lap_two <- dt3 %>% filter(laps==2)  %>% 
  select(lastName, rank)
combined <- on_start %>% left_join(on_lap_two, by="lastName") %>% 
  mutate(lapOneChange = startPosition-rank) %>% 
  select(lastName, lapOneChange)

fast_lap <- dt3 %>% select(lastName,BestSpeed) %>% 
  group_by(lastName) %>% slice(which.max(BestSpeed)) %>%
  ungroup() %>% 
  mutate(fastLapRank = rank(-BestSpeed)) %>% 
  select(-BestSpeed)

xpoints <- atp %>% 
  mutate(atp = round(atp)) %>% 
  left_join(type, by=c("atp"="fin")) %>% 
  select(-atp)

xpoints25 <- atp25 %>% 
  mutate(atp25 = round(atp25)) %>% 
  left_join(type, by=c("atp25"="fin")) %>% 
  select(-atp25)

race_results <- dt3 %>% select(lastName, time_stamp, overallRank) %>% 
  group_by(lastName) %>% 
  slice(which.max(time_stamp))


drivers <- dt3 %>% select(lastName, firstName) %>% unique()

final <- drivers %>% left_join(led_most_points) %>% left_join(pole) %>% 
  mutate(ledLapPoints = replace_na(ledLapPoints, 0),
         ledMostPoints = replace_na(ledMostPoints,0),
         polePoints = replace_na(polePoints,0)) %>% 
  left_join(atp) %>%  left_join(atp25) %>% 
  left_join(xpoints) %>% left_join(xpoints25, by=c("lastName")) %>% 
  mutate(xPoints = ledMostPoints + ledLapPoints +polePoints + points.x) %>% 
  left_join(passes) %>% 
  left_join(combined) %>% 
  mutate(passesFor = replace_na(passesFor, 0),
         passesAgainst = replace_na(passesAgainst,0)) %>% 
  left_join(in_top_five) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  left_join(fast_lap) %>% 
  mutate(driver_name = paste0(firstName, " ", lastName)) %>% 
  left_join(race_results)





final2 <- final %>% left_join(type, by=c("overallRank" = "fin")) %>% 
  mutate(TotalPoints = points+ledLapPoints+ledMostPoints+polePoints) %>% 
  select(driver_name, TotalPoints) %>% 
  mutate(driver_name = ifelse(driver_name=="Pato O'Ward", "Patricio O'Ward", driver_name))

combined <- current_data %>% filter(year==2021) %>% 
  group_by(driver) %>% summarise(pointsOld = sum(pts)) %>% 
  full_join(final2, by=c("driver"="driver_name")) %>% 
  mutate(pointsOld = ifelse(is.na(pointsOld),0,pointsOld),
         TotalPoints = ifelse(is.na(TotalPoints),0,TotalPoints),
         PointsAsTheyRun = pointsOld+TotalPoints) %>% 
  select(driver, PointsAsTheyRun)

max_points = max(combined$PointsAsTheyRun)

combined <- combined %>% mutate(Gap = PointsAsTheyRun-max_points) %>% 
  rename(Driver = driver) %>% 
  arrange(-Gap) %>% gt()

older <- current_data %>% filter(year==2021) %>% 
  group_by(driver) %>% summarise(pointsOld = sum(pts))
