library(data.table)
library(tidyverse)
library(ggplot2)

data<-fread("C:/Users/drewb/Desktop/indycar_results.csv")
cautions<-fread("C:/Users/drewb/Desktop/restartdata.csv")
green_flag<-fread("C:/Users/drewb/Desktop/green_flag_stops.csv")
strategists<-fread("C:/Users/drewb/Desktop/race_strategists.csv")

#Calculate AFP from every starting position
afp <- data %>%
  filter(year!=2019) %>% 
  group_by(st) %>% 
  summarise(xFP = mean(fin))

#Left join data with xFP for every driver's results
data <- data %>%
  filter(year==2019) %>% 
  left_join(afp, by=c("st" = "st")) %>% 
  mutate(xFPDifference=xFP-fin)


driver_season_stats <- data %>%
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
         ASP = mean(st),
         ATP = mean(atp),
         ATP25 = mean(atp25),
         PassEff = 100*(sum(passesFor)/ (sum(passesFor)+sum(passesAgainst))),
         RunningCheck = ifelse(status=="running",1,0),
         RunPerc = 100*mean(RunningCheck),
         AvgFastSpeed = mean(fastLapRank),
         Top5Perc = 100*(sum(inTopFive)/sum(laps)),
         #Average Surplus Position
         ASPos = mean(xFPDifference)) %>% 
  #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
  distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, ASP, ATP, ATP25, PassEff, RunPerc, AvgFastSpeed, Top5Perc, ASPos)


#Process caution data for season stats
caution_stats <- cautions %>%
  group_by(driver) %>%
  mutate(favorableRestart = ifelse(restartPM>=0, 1,
                            ifelse(restartPM<0, 0, NA)),
         RestartRetention = 100*mean(favorableRestart),
         RestartPM = sum(restartPM),
         Attempts = n(),
         PMperRestart = RestartPM/Attempts) %>% 
  select(driver, RestartRetention, RestartPM, PMperRestart, Attempts) %>% 
  distinct()


#Combine tables
combined<-left_join(driver_season_stats, caution_stats, by="driver")

#One table for driver, races, points, xpoints, Difference, avgFin, avgSt, ATP, ATP25, PassEff
season1<- combined %>%
  mutate(Difference = Pts-xPoints) %>% 
  select(driver, Races, Pts, xPoints, Difference, AFP, ASP, ATP, ATP25, PassEff, RunPerc, AvgFastSpeed, Top5Perc, ASPos)

season1<-data.table(season1)  
fwrite(season1, "season1.csv")

season2<- combined %>% 
  select(driver, Races, StartRetention, StartPM, PMperStart, RestartRetention, RestartPM, PMperRestart, Attempts)
season2<-data.table(season2)  
fwrite(season2, "season2.csv")

#GREEN FLAG PITSTOP STATS
green_flag_stats <- green_flag %>% 
  group_by(driver) %>%
  mutate(favorableStop = ifelse(plusminus>=0, 1,
                                   ifelse(plusminus<0, 0, NA)),
         GFRetention = 100*mean(favorableStop),
         GFStopPM = sum(plusminus),
         Stops = n(),
         PMperStop = GFStopPM/Stops) %>% 
  select(driver, GFRetention, GFStopPM, PMperStop, Stops) %>% 
  distinct() %>% 
  left_join(strategists, by = c("driver" = "Driver")) %>% 
  select(driver, Team, Strategist, GFRetention, GFStopPM, PMperStop, Stops)

season3<-data.table(green_flag_stats)  
fwrite(season3, "season3.csv")  


#Extra table for raw passing tables
passes<- data %>% 
  filter(year==2019) %>% 
  group_by(driver) %>% 
  mutate(totalPassesFor=sum(passesFor),
         totalPassesAgainst=sum(passesAgainst),
         passEff = totalPassesFor/(totalPassesAgainst+totalPassesFor)) %>% 
  select(driver, totalPassesFor, totalPassesAgainst, passEff) %>% 
  distinct()

