library(data.table)
library(tidyverse)
library(lubridate)
#Elo for starting positions - same as elo.R except all instances of $driver are replaced with $st
dt <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

#Initialize elo ratings, k
elo_ratings_initial <- dt %>% select(st) %>% unique() %>% mutate(EloRating=1500)
#k = 2.5 previously
k <- 2.5

elo_ratings <- elo_ratings_initial
tracker <- tibble(st=elo_ratings_initial$st, date=ymd("2021-01-01"), year=2000, EloRating=1500)
k_optimization <- tibble(errorXWin=0, errorXWin2=0) 

#Only years we want
dt2 <- dt %>% filter(year %in% c(2008:2020)) %>% select(year, raceNumber, st, fin, date)

yr <- c(2008:2020)
#needed for xaxis plotting since i dont have race dates
for(a in yr) {
  current_year <- dt2 %>% filter(year==a) %>% select(year, raceNumber, st, fin, date)
  
  for(i in 1:max(current_year$raceNumber)) {
    current_race <- current_year %>% filter(raceNumber==i, )
    x <- current_race$st
    y <- current_race$st
    
    current_race_cross <- crossing(x, y) %>% left_join(current_race, by=c("x"="st")) %>% 
      left_join(current_race, by=c("y"="st")) %>% select(-raceNumber.y) %>% filter(x!=y) %>% 
      left_join(elo_ratings, by=c("x"="st")) %>% 
      left_join(elo_ratings, by=c("y"="st")) %>% 
      mutate("xWin" = ifelse(fin.x<fin.y,1,0),
             "XexpectedWin" = (1/(1+10^((EloRating.y-EloRating.x)/400))))
    
    #Update elo ratings for race
    elo <- current_race_cross %>% group_by(x) %>% 
      summarise("oldRating" = mean(EloRating.x),
                "actualScore" = sum(xWin),
                "expectedScore" = sum(XexpectedWin),
                "newRating"= oldRating+k*(actualScore-expectedScore),
                "date" = max(mdy(date.x)))
    
    for(j in 1:nrow(elo)) {
      elo_ratings[elo_ratings$st==elo$x[j],2] <- elo$newRating[j]
      tracker <- add_row(tracker, st=elo$x[j], date=elo$date[j], year=a, EloRating=elo$newRating[j])
    }
    k_optimization <- add_row(k_optimization, errorXWin = current_race_cross$xWin, errorXWin2=current_race_cross$XexpectedWin)
  }
}

k_optimization %>% mutate(error=(errorXWin-errorXWin2)^2) %>% 
  summarise(e=mean(error))

