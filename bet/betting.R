library(data.table)
library(tidyverse)
library(lubridate)

#read in entrants and starting positions
race <- fread("C:/Users/drewb/desktop/elo_drivers.csv")
draftkings <- fread("C:/Users/drewb/desktop/draftkings_odds.csv")
matchups <- fread("C:/Users/drewb/desktop/draftkings_matchup.csv")
elo_ratings <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")
sp_elo <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/4_1_2020_start_pos_elo_ratings.csv")


matchups <- matchups %>% left_join(race, by=c("Driver1"="driver")) %>% 
  left_join(race, by=c("Driver2" = "driver"))
  
#get most recent Elo ratings
elo <- elo_ratings %>% filter(year !=2000) %>% 
  group_by(driver) %>%
  slice(which.max(ymd(date))) %>% 
  mutate(EloRating = round(EloRating)) %>% 
  select(-PreviousEloRating)

if(race[1,2] == 0){
  print("Pre-qualifying")
  
  race_temp <- race %>% left_join(elo) %>% 
    mutate(EloRating = ifelse(is.na(EloRating),1400,EloRating))
  
  r <- tibble(driver="test", WinTSSProb = 0)
  print(race_temp)
  
  for(i in c(1:nrow(race_temp))){
    current_driver <- race_temp[i,1]
    current_q <- 10^((race_temp[i,5])/400)
    sum_opponents_q <- 0
    
    for(j in c(1:nrow(race_temp))){
      if(race_temp[j,1] != current_driver){
        opponents_q <- 10^((race_temp[j,5])/400)
        sum_opponents_q = sum_opponents_q + opponents_q 
      }
    }
    
    r <- r %>% add_row(driver=current_driver$driver, WinTSSProb=(current_q$EloRating/(current_q$EloRating+sum_opponents_q$EloRating)))
  }
  
  
  #matchups
  matchups <- matchups %>% left_join(elo, by=c("Driver1"="driver")) %>% 
    left_join(elo, by=c("Driver2"="driver")) %>% 
    mutate(Driver1TSSProb = (10^(EloRating.x/400)) / ((10^(EloRating.x/400))+(10^(EloRating.y/400))),
           Driver2TSSProb = 1-Driver1TSSProb)
  
} else{
  print("Post qualifying")
  
  race_temp <- race %>% left_join(elo) %>% 
    mutate(EloRating = ifelse(is.na(EloRating),1400,EloRating)) %>% 
    left_join(sp_elo, by=c("start"="st"))
  
  r <- tibble(driver="test", WinTSSProb = 0)
  print(race_temp)
  
  for(i in c(1:nrow(race_temp))){
    current_driver <- race_temp[i,1]
    current_q <- 10^(( .5*race_temp[i,5]+.5*race_temp[i,6] )/400)
    sum_opponents_q <- 0
    
    for(j in c(1:nrow(race_temp))){
      if(race_temp[j,1] != current_driver){
        opponents_q <- 10^(( .5*race_temp[j,5]+.5*race_temp[j,6] )/400)
        sum_opponents_q = sum_opponents_q + opponents_q 
      }
    }
    
    r <- r %>% add_row(driver=current_driver, WinTSSProb=(current_q/(current_q+sum_opponents_q)))
  }
  
  # matchups
  matchups <- matchups %>% left_join(elo, by=c("Driver1"="driver")) %>% 
    left_join(elo, by=c("Driver2"="driver")) %>% 
    left_join(sp_elo, by=c("start.x"="st")) %>% 
    left_join(sp_elo, by=c("start.y"="st")) %>% 
    mutate(Driver1CombinedElo = .5*EloRating.x+.5*EloRating.x.x,
           Driver2CombinedElo = .5*EloRating.y+.5*EloRating.y.y,
           Driver1TSSProb = (10^(Driver1CombinedElo/400)) / ((10^(Driver1CombinedElo/400))+(10^(Driver2CombinedElo/400))),
           Driver2TSSProb = 1-Driver1TSSProb)

  }


#r <- race win probabilities

x <- draftkings %>% left_join(r, by=c("Driver"="driver")) %>% 
  mutate(WinDKProb = 100/(WinDK+100),
         Top3DKProb = 100/(Top3DK+100),
         EV10DollarBet = WinTSSProb*10*(WinDK/100)+(1-WinTSSProb)*-10)

y <- matchups %>% mutate(Driver1DKProb = ifelse(Driver1DK<0, (Driver1DK*-1/(Driver1DK*-1+100)), (100/(Driver1DK+100)) ),
                    Driver2DKProb = ifelse(Driver2DK<0, (Driver2DK*-1/(Driver2DK*-1+100)), (100/(Driver2DK+100))),
                    EV10DollarBetOnDriver1 =  Driver1TSSProb*10*(ifelse(Driver1DK>0,Driver1DK/100,100/(-1*Driver1DK))) + (1-Driver1TSSProb)*-10,
                    EV10DollarBetOnDriver2 = Driver2TSSProb*10*(ifelse(Driver2DK>0,Driver2DK/100,100/(-1*Driver2DK)))+ (1-Driver2TSSProb)*-10)


