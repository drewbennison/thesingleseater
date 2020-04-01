library(data.table)
library(tidyverse)
#Elo for starting positions - same as elo.R except all instances of driver are replaced with st
dt <-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/indycar_results.csv")

#Initialize elo ratings, k
elo_ratings_initial <- dt %>% select(st) %>% unique() %>% mutate(EloRating=1500)
k <- 2.5

elo_ratings <- elo_ratings_initial
tracker <- tibble(st=elo_ratings_initial$st, raceNumber=0, year=2018, EloRating=1500)
#k_optimization <- tibble(errorXWin=0, errorXWin2=0) 

#Only years we want
dt2 <- dt %>% filter(year %in% c(2008:2019)) %>% select(year, raceNumber, st, fin)

yr <- c(2008:2019)
#needed for xaxis plotting since i dont have race dates
bb <- c(0,18,35,52,69,84,103,121,137,153,170,187,204)
for(a in 1:length(yr)) {
  b <- bb[a]
  current_year <- dt2 %>% filter(year==yr[a]) %>% select(raceNumber, st, fin)
  
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
                "newRating"= oldRating+k*(actualScore-expectedScore))
    
    for(j in 1:nrow(elo)) {
      elo_ratings[elo_ratings$st==elo$x[j],2] <- elo$newRating[j]
      tracker <- add_row(tracker, st=elo$x[j], raceNumber=b+i, year=a, EloRating=elo$newRating[j])
    }
    k_optimization <- add_row(k_optimization, errorXWin = current_race_cross$xWin, errorXWin2=current_race_cross$XexpectedWin)
  }
}

k_optimization %>% mutate(error=(errorXWin-errorXWin2)^2) %>% 
  summarise(e=mean(error))






#elo rating by time
tracker %>% mutate(st=as.factor(st)) %>%  filter(raceNumber!=0, st %in% c(1,2,3,4,5)) %>%
  ggplot(aes(x=raceNumber, y=EloRating, color=st)) + geom_line()

#elo rating by race number in career
elo_ratings %>% ggplot(aes(x=st,y=EloRating)) + geom_col()

