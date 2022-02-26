library(data.table)
library(tidyverse)
library(lubridate)

#read in entrants and starting positions
race <- fread("C:/Users/drewb/desktop/projects/thesingleseater/bet/elo_drivers.csv")
draftkings <- fread("C:/Users/drewb/desktop/projects/thesingleseater/bet/draftkings_odds.csv")
matchups <- fread("C:/Users/drewb/desktop/projects/thesingleseater/bet/draftkings_matchup.csv")
elo_ratings <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker_new_test.csv")

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
    mutate(EloRating = ifelse(is.na(EloRating),1400,EloRating))
  
  r <- tibble(driver="test", WinTSSProb = 0)
  print(race_temp)
  
  for(i in c(1:nrow(race_temp))){
    current_driver <- race_temp[i,1]
    current_q <- 10^((race_temp[i,5])/400)
    sum_opponents_q <- 0
    
    current_driver_qualified <- race_temp[i,2] %>% pull()
    
    for(j in c(1:nrow(race_temp))){
      if(race_temp[j,1] != current_driver){
        opponents_q <- 10^((race_temp[j,5])/400)
        
        opponent_driver_qualified <- race_temp[j, 2] %>% pull()
        
        #qualifying modifier
        qual_modifier <- (opponent_driver_qualified - current_driver_qualified)*7.5
        
        sum_opponents_q = sum_opponents_q + (opponents_q - qual_modifier)
      }
    }
    
    r <- r %>% add_row(driver=current_driver$driver, WinTSSProb=(current_q$EloRating/(current_q$EloRating+sum_opponents_q$EloRating)))
  }
  
  # matchups
  matchups <- matchups %>% left_join(elo, by=c("Driver1"="driver")) %>% 
    left_join(elo, by=c("Driver2"="driver")) %>% 
    mutate(EloRating.x = ifelse(is.na(EloRating.x),1400,EloRating.x)) %>% 
    mutate(EloRating.y = ifelse(is.na(EloRating.y),1400,EloRating.y)) %>%
    left_join(race, by=c("Driver1" = "driver")) %>% 
    left_join(race, by=c("Driver2"="driver")) %>% 
    mutate(Driver1TSSProb = (1/(1+10^((EloRating.y - (EloRating.x + 7.5*(start.y-start.x)))/400))),
           Driver2TSSProb = 1-Driver1TSSProb)

  }


#r <- race win probabilities

x <- draftkings %>% left_join(r, by=c("Driver"="driver")) %>% 
  mutate(WinDKProb = 100/(WinDK+100),
         Top3DKProb = ifelse(Top3DK<0, (Top3DK*-1/(Top3DK*-1+100)), 100/(Top3DK+100)),
         EV10DollarBetWin = WinTSSProb*10*(WinDK/100)+(1-WinTSSProb)*-10,
         EV10DollarBetPodium = 3*WinTSSProb*10*(ifelse(Top3DK>0, Top3DK/100, 100/(-1*Top3DK))) + (1-3*WinTSSProb)*-10)

y <- matchups %>% mutate(Driver1DKProb = ifelse(Driver1DK<0, (Driver1DK*-1/(Driver1DK*-1+100)), (100/(Driver1DK+100)) ),
                    Driver2DKProb = ifelse(Driver2DK<0, (Driver2DK*-1/(Driver2DK*-1+100)), (100/(Driver2DK+100))),
                    EV10DollarBetOnDriver1 =  Driver1TSSProb*10*(ifelse(Driver1DK>0, Driver1DK/100, 100/(-1*Driver1DK))) + (1-Driver1TSSProb)*-10,
                    EV10DollarBetOnDriver2 = Driver2TSSProb*10*(ifelse(Driver2DK>0,Driver2DK/100,100/(-1*Driver2DK)))+ (1-Driver2TSSProb)*-10)

  