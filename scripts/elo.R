library(data.table)
library(tidyverse)

dt <-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/indycar_results.csv")
sp_elo <- fread("C:/Users/drewb/Desktop/thesingleseater/datasets/starting_position_elo.csv")

#Initialize elo ratings, k
elo_ratings_initial <- dt %>% select(driver) %>% unique() %>% mutate(EloRating=1500)
k <- 2.5

elo_ratings <- elo_ratings_initial
tracker <- tibble(driver=elo_ratings_initial$driver, date=ymd("2021-01-01"), year=2018, EloRating=1500)
k_optimization <- tibble(errorXWin=0, errorXWin2=0) 

#Select variables we want
dt <- dt %>% select(year, raceNumber, driver, fin, st, date)

#Starting and ending year range
yr <- c(2008:2019)
for(a in c(2008:2019)) {
  current_year <- dt %>% filter(year==a) %>% select(raceNumber, driver, fin, st, date)
  
for(i in 1:max(current_year$raceNumber)) {
  current_race <- current_year %>% filter(raceNumber==i, )
  
  x <- current_race$driver
  y <- current_race$driver
  
  current_race_cross <- crossing(x, y) %>% left_join(current_race, by=c("x"="driver")) %>% 
    left_join(current_race, by=c("y"="driver")) %>% select(-raceNumber.y) %>% filter(x!=y) %>% 
    left_join(elo_ratings, by=c("x"="driver")) %>% 
    left_join(elo_ratings, by=c("y"="driver")) %>% 
    #bring in starting position adjustment
    left_join(sp_elo, by=c("st.x"="st")) %>% 
    left_join(sp_elo, by=c("st.y"="st")) %>% 
    mutate("xWin" = ifelse(fin.x<fin.y,1,0),
           "XexpectedWin" = (1/(1+10^(( (.5*EloRating.y+.5*EloRating.y.y) - (.5*EloRating.x+.5*EloRating.x.x) )/400))))
  
  #Update elo ratings for race
  elo <- current_race_cross %>% group_by(x) %>% 
    summarise("oldRating" = mean(EloRating.x),
              "actualScore" = sum(xWin),
              "expectedScore" = sum(XexpectedWin),
              "newRating"= oldRating+k*(actualScore-expectedScore),
              "date" = max(ymd(date.x)))
  
  for(j in 1:nrow(elo)) {
    elo_ratings[elo_ratings$driver==elo$x[j],2] <- elo$newRating[j]
    tracker <- add_row(tracker, driver=elo$x[j], date=elo$date[j], year=a, EloRating=elo$newRating[j])
  }
  
  k_optimization <- add_row(k_optimization, errorXWin = current_race_cross$xWin, errorXWin2=current_race_cross$XexpectedWin)
  }
}

#Calculates the brier score
k_optimization %>% mutate(error=(errorXWin-errorXWin2)^2) %>% 
  summarise(e=mean(error))

#elo rating by races since 2008
tracker %>% filter(date!="2021-01-01", driver %in% c("Josef Newgarden", "Simon Pagenaud","Scott Dixon",
                                 "Marco Andretti", "Alexander Rossi")) %>%
  ggplot(aes(x=date, y=EloRating, color=driver)) + geom_line()

#elo rating by race number in career
tracker %>%
  filter(date!="2021-01-01") %>% 
  group_by(driver) %>%
  mutate(my_ranks = order(order(date, decreasing=FALSE))) %>% 
  filter(driver %in% c("Josef Newgarden", "Simon Pagenaud","Scott Dixon",
                                      "Colton Herta")) %>%
  ggplot(aes(x=my_ranks, y=EloRating, color=driver)) + geom_line() +
  labs(x="Race number in career starting in 2008")

##################################################################
#Probability of finishing in first place in a single race
r<-tibble(driver="test",winprob=0)
hypothetical_race <- elo_ratings %>% filter(EloRating>1570)
for(i in (1:nrow(hypothetical_race))) {
  current_driver <- hypothetical_race$driver[i]
  
  current_q <- 10^(hypothetical_race$EloRating[i]/400)
  sum_opponents_q <- 0
  for(j in (1:nrow(hypothetical_race))) {
   if(hypothetical_race$driver[j]!=current_driver) {
     opponent_q <- 10^(hypothetical_race$EloRating[j]/400)
     sum_opponents_q <- sum_opponents_q+opponent_q
   }
  }
  r <- add_row(r, driver=hypothetical_race$driver[i], winprob=current_q/(current_q+sum_opponents_q))
}

