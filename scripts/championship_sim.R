# No bonus points included yet

# UPDATE 4/24/2019
# BASE DRIVER DISTRIBUTIONS ONLY ON RACES IN WHICH
# THEY DID NOT DNF.

library(data.table)
library(tidyverse)

##########################################################################
#CHANGE THIS AFTER EACH RACE OBVIOUSLY
total_races<-17
numracesleft<-13
##########################################################################
# Read in the data, calculate the current points standings
data<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/indycar_results.csv")
points<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/points_table.csv")

current_points <- data %>%
  filter(year==2019) %>% 
  group_by(driver) %>% 
  summarise(CurrentPoints = sum(pts))

##########################################################################
#FILTER FOR NUMBER OF RACES AS SEASON PROGRESSES TO PREVENT ONE-OFF DRIVERS
current_drivers <- data %>% 
  filter(year==2019) %>% 
  select(driver) %>% 
  distinct()
##########################################################################
#Identify each driver's unique distribution based on their performances
driver_distributions <- data %>%
  filter(year==2019) %>% 
  select(driver, track, status, fin, atp) %>% 
  melt(idvars=c("driver", "track", "status")) %>%
  filter(status =="running" & variable=="fin" | status == "running" & variable=="atp" |
         status!= "running" & variable=="atp") %>% 
  select(driver, value) %>% 
  group_by(driver) %>%
  summarise(mean=mean(value, na.rm = TRUE),
            sd=sd(value, na.rm = TRUE))

#Create driver vector
Drivers = c()
for (i in 1:nrow(current_drivers)) {
  current<-current_drivers[i,]
  Drivers<-c(Drivers, current)
}

#Number of races left in season
Race = c(1:numracesleft)

results <- data.table()

for (i in 1:50000) {
  season <- tibble()
  #Create all combinations of races and drivers
  season <- crossing(Race, Drivers)
  #Season winner
  season_winner <- season %>% 
    #Join with driver distributions, take a random draw, order draws by race for a result
    left_join(driver_distributions, by=c("Drivers"="driver")) %>% 
    mutate(dist_random_number = runif(nrow(season),0,1),
           RaceDraw = if_else(dist_random_number<(numracesleft/total_races), rnorm(nrow(season),12,7),  rnorm(nrow(season), mean, sd))) %>%
    group_by(Race) %>% 
    mutate(RaceResult=order(order(RaceDraw))) %>% 
    ungroup(Race) %>%
    #Bring in points based on race results
    left_join(points, by=c("RaceResult"="fin")) %>%
    ########################################################################################
    #Double points (Race==last race)
    mutate(DoublePoints = if_else(Race==numracesleft | Race==1, points+points, points)) %>% 
    group_by(Drivers) %>% 
    #Sum total points over the season
    summarise(SumPointsEarned = sum(points)) %>% 
    left_join(current_points, by=c("Drivers"="driver")) %>% 
    mutate(Total = SumPointsEarned+CurrentPoints,
           ChampPos = order(order(-Total))) %>% 
    mutate(season = i) %>% 
    arrange(-Total)
  results<-rbind(results, season_winner)
  
  message("Season ",i, " winner: ", season_winner[1,1])
  
}

#Summarize the probabilities for every driver finishing in every position
x<-results %>% 
  filter(ChampPos==1) 
y<- results %>% 
  filter(ChampPos==1) %>% 
  group_by(Drivers) %>% 
  count() %>% 
  mutate(prob=n/nrow(x),
         ChampPos=1)

master_results<-data.table(y)

#Aggregating probability by position and driver
for (i in 2:26) {
  x<-results %>% 
    filter(ChampPos==i) 
  y<- results %>% 
    filter(ChampPos==i) %>% 
    group_by(Drivers) %>% 
    count() %>% 
    mutate(prob=n/nrow(x),
           ChampPos=i)
  y<-data.table(y)
  l=list(master_results,y)
  master_results <- rbindlist(l)
}
  
#Export a CSV of predictions
final<-dcast(master_results, Drivers~ChampPos, sum, value.var = "prob")
fwrite(final, "champredictions.csv")

#Heatmap of the above information
final2<-melt(final, "Drivers")
ggplot(data=final2, aes(x=variable, y=Drivers)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0.1, limits=c(0,1))

master_results %>% 
  ggplot(aes(x=ChampPos, y=prob, color=Drivers)) + geom_col() +
  facet_wrap(~Drivers) +
  theme(legend.position = "false") +
  labs(x="Championship Position", y="Probability")

