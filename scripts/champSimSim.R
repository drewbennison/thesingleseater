library(data.table)
library(tidyverse)

# 20 cars?
# vary SD too?



##########################################################################
# Read in the data, calculate the current points standings
dt<-fread("C:/Users/drewb/Desktop/thesingleseater/temp_scripts/driver_points.csv")
data <-dt
points<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/points_table.csv")

df <- tibble(n=100, prob=100)

for(i in c(13,14,15,16,17)) {

numracesleft<-i

current_points <- data %>%
  filter(year==2019) %>% 
  group_by(driver) %>% 
  summarise(CurrentPoints = sum(pts))

##########################################################################
#FILTER FOR NUMBER OF RACES AS SEASON PROGRESSES TO PREVENT ONE-OFF DRIVERS
current_drivers <- data %>% 
  filter(year==2019) %>% 
  group_by(driver) %>% 
  summarise(x = sum(pts)) %>% 
  arrange(-x) %>% 
  top_n(23) %>% 
  select(driver) %>% 
  distinct()
##########################################################################
#Identify each driver's unique distribution based on their performances
driver_distributions <- data %>%
  select(driver, mean, sd)

#Create driver vector
Drivers = c()
for (i in 1:nrow(current_drivers)) {
  current<-current_drivers[i,]
  Drivers<-c(Drivers, current)
}

#Number of races left in season
Race = c(1:numracesleft)

results <- data.table()

for (i in 1:10000) {
  season <- tibble()
  #Create all combinations of races and drivers
  season <- crossing(Race, Drivers)
  season$Drivers <- as.character(season$Drivers)
  #Season winner
  season_winner <- season %>% 
    #Join with driver distributions, take a random draw, order draws by race for a result
    left_join(driver_distributions, by=c("Drivers"="driver")) %>% 
    mutate(dist_random_number = runif(nrow(season),0,1),
           RaceDraw = if_else(dist_random_number<(-1), rnorm(nrow(season),12,7),  rnorm(nrow(season), mean, sd))) %>%
    group_by(Race) %>% 
    mutate(RaceResult=order(order(RaceDraw))) %>% 
    ungroup(Race) %>%
    #Bring in points based on race results
    left_join(points, by=c("RaceResult"="fin")) %>%
    ########################################################################################
  #Double points (Race==last race)
  mutate(DoublePoints = if_else(Race==numracesleft, points+points, points)) %>% 
    group_by(Drivers) %>% 
    #Sum total points over the season
    summarise(SumPointsEarned = sum(points)) %>% 
    left_join(current_points, by=c("Drivers"="driver")) %>% 
    #Take the top driver of the season and add him to the results table
    mutate(Total = SumPointsEarned+CurrentPoints,
           ChampPos = order(order(-Total))) %>% 
    #top_n(1, Total) %>% 
    mutate(season = i) %>% 
    arrange(-Total)
  results<-rbind(results, season_winner)
  
  #message("Season ",i, " winner: ", season_winner[1,1])
  
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


final<-dcast(master_results, Drivers~ChampPos, sum, value.var = "prob")
p = final[1,2]

df <- add_row(df, n=numracesleft, prob=p)

df$prob <- unlist(df$prob)

}

df %>% filter(n!=100) %>% 
ggplot(aes(x=n, y=prob)) + geom_line() + geom_smooth(method = "lm")
