library(data.table)
library(tidyverse)
library(lubridate)

dt <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
sp_elo <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/starting_position_elo.csv")


#Initialize elo ratings, k
elo_ratings_initial <- dt %>% select(driver) %>% unique() %>% mutate(EloRating=1500)
k <- 2.5

elo_ratings <- elo_ratings_initial
tracker <- tibble(driver=elo_ratings_initial$driver, date=ymd("2021-01-01"), year=2000, EloRating=1500)
k_optimization <- tibble(errorXWin=0, errorXWin2=0) 

#Select variables we want
dt <- dt %>% select(year, raceNumber, driver, fin, st, date) %>% 
  mutate(date=mdy(date))

#Starting and ending year range
yr <- c(2008:2020)
for(a in c(2008:2020)) {
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
  
  #k_optimization <- add_row(k_optimization, errorXWin = current_race_cross$xWin, errorXWin2=current_race_cross$XexpectedWin)
  }
}

#Calculates the brier score
#k_optimization %>% mutate(error=(errorXWin-errorXWin2)^2) %>% 
#  summarise(e=mean(error))

#############################  GRAPHS  ##########################################

#  elo rating by date since 2008  #
tracker %>% filter(date!="2021-01-01", driver %in% c("Scott Dixon", "Dario Franchitti", "Simon Pagenaud", "Josef Newgarden",
                                                      "Alexander Rossi")) %>%
  ggplot(aes(x=date, y=EloRating, color=driver)) + geom_line() +
  labs(x="Date", title = "Elo Rating Over Time", subtitle = "Minimum 10 starts",
       color="Driver", y="EloRating") +
  theme_bw()

#  elo rating by race number in career since 2008  #
tracker %>%
  filter(date!="2021-01-01") %>% 
  group_by(driver) %>%
  mutate(my_ranks = order(order(date, decreasing=FALSE))) %>% 
  filter(driver %in% c("Scott Dixon", "Dario Franchitti", "Simon Pagenaud", "Josef Newgarden",
                       "Alexander Rossi")) %>%
  ggplot(aes(x=my_ranks, y=EloRating, color=driver)) + geom_line() +
  labs(x="Race number in career",
       title = "Elo Rating by Race Number in Career", subtitle = "Minimum 10 starts, starting in 2008",
       color="Driver") +
  theme_bw()


y <- tracker %>% group_by(year) %>% 
  summarise(x = max(EloRating))

##################################################################
num_races <- dt %>% group_by(driver) %>% count()

#Final elo ratings, min 10 races
article_elo_ratings <- elo_ratings %>% left_join(num_races) %>% filter(n>10)
fwrite(article_elo_ratings, "article_elo_ratings.csv")

#Peak elo ratings
article_max_elo_ratings <- tracker %>% left_join(num_races) %>% filter(n>10) %>% group_by(driver) %>% 
  top_n(wt=EloRating,n=1)
fwrite(article_max_elo_ratings, "article_max_elo_ratings.csv")


