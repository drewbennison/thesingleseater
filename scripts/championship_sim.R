# No bonus points included yet

library(data.table)
library(tidyverse)

##########################################################################
#CHANGE THIS AFTER EACH RACE OBVIOUSLY
total_races<-17
numracesleft<-17
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
  group_by(driver) %>% 
  summarise(x = sum(pts)) %>% 
  arrange(-x) %>% 
  top_n(23) %>% 
  select(driver) %>% 
  distinct()
##########################################################################
#Identify each driver's unique distribution based on their performances
driver_distributions <- data %>%
  filter(year %in% c(2019)) %>% 
  select(driver, fin, atp) %>% 
  melt("driver") %>% 
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
  season$Drivers <- as.character(season$Drivers)
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
fwrite(final, "champredictions.csv")

final2 <- melt(final, id.vars = c("Drivers"))

final2 %>%
  ggplot(label=value) + geom_col(aes(x=variable, y=value, fill=Drivers)) + facet_wrap(~Drivers) +
  theme(legend.position = "none")+
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20")) +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits = c(0,1)) +
  labs(y="Probability of finishing season in position",
       x="Championship finishing position",
       title="2019 IndyCar Championship Predictions",
       subtitle = "After simulating the remaining races 50,000 times",
       caption = "www.thesingleseater.com")



#Test
results %>% 
  filter(Drivers %in% c("Scott Dixon", "Simon Pagenaud")) %>% 
  group_by(season) %>% 
  arrange(Drivers)

final2$x <- final2$value*as.numeric(final2$variable)

y<-dcast(final2, Drivers ~., sum, value.var = "x")
fwrite(y, "t.csv")
