library(data.table)
library(tidyverse)
library(lubridate)

dt <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

#Initialize elo ratings, k
elo_ratings_initial <- dt %>% select(driver) %>% unique() %>% mutate(EloRating=1500)
k <- 2
w <- 1 #season retention
q <- 7.5 #elo points per place difference

elo_ratings <- elo_ratings_initial
tracker <- tibble(driver=elo_ratings_initial$driver, date=ymd("2021-01-01"), year=2000, EloRating=1500, PreviousEloRating=1500)
k_optimization <- tibble(errorXWin=0, errorXWin2=0, season=year("2000-01-01"), type="None")

#Select variables we want
dt <- dt %>% select(year, raceNumber, driver, fin, st, date, type) %>% 
  mutate(date=mdy(date))

#Starting and ending year range
for(a in c(2008:2021)) {
  current_year <- dt %>% filter(year==a) %>% select(raceNumber, driver, fin, st, date, type)
  
for(i in 1:max(current_year$raceNumber)) {
  current_race <- current_year %>% filter(raceNumber==i, )
  
  x <- current_race$driver
  y <- current_race$driver
  
  if(i == 1){
    elo_ratings$EloRating <- w*elo_ratings$EloRating + (w-1)*1500
  }
  
  current_race_cross <- crossing(x, y) %>% left_join(current_race, by=c("x"="driver")) %>% 
    left_join(current_race, by=c("y"="driver")) %>% select(-raceNumber.y) %>% filter(x!=y) %>% 
    left_join(elo_ratings, by=c("x"="driver")) %>% 
    left_join(elo_ratings, by=c("y"="driver")) %>% 
    mutate("xWin" = ifelse(fin.x<fin.y,1,0),
           "XexpectedWin" = (1/(1+10^((EloRating.y - (EloRating.x + q*(st.y-st.x)))/400))))
  
  #Update elo ratings for race
  elo <- current_race_cross %>% group_by(x) %>% 
    summarise("oldRating" = mean(EloRating.x),
              "actualScore" = sum(xWin),
              "expectedScore" = sum(XexpectedWin),
              "newRating"= oldRating+k*(actualScore-expectedScore),
              "date" = max(ymd(date.x)),
              "type" = max(type.x))
  
  for(j in 1:nrow(elo)) {
    elo_ratings[elo_ratings$driver==elo$x[j],2] <- elo$newRating[j]
    tracker <- add_row(tracker, driver=elo$x[j], date=elo$date[j], year=a, EloRating=elo$newRating[j], PreviousEloRating=elo$oldRating[j])
  }
  
  k_optimization <- add_row(k_optimization, errorXWin = current_race_cross$xWin, errorXWin2=current_race_cross$XexpectedWin, season=year(current_race_cross$date.x), type=current_race_cross$type.x)
  }
}

fwrite(tracker,"C:/Users/drewb/Desktop/projects/thesingleseater/datasets/elo_ratings/elo_tracker.csv")


#
error_calc <- k_optimization %>% mutate(error=(errorXWin-errorXWin2)^2) %>% 
  filter(season>2015)

e <- mean(error_calc$error)

#Calculates the brier score by season
k_optimization %>% mutate(error=(errorXWin-errorXWin2)^2) %>% 
  group_by(season) %>% 
  summarise(e=mean(error)) %>% 
  filter(season!=2000) %>% 
  arrange(-e) %>% 
  ggplot() +
  geom_point(aes(x=season, y=e))

#percent of matchups called correctly
k_optimization %>% 
  mutate(xPredicted = ifelse(errorXWin2>=.5,1,0),
         correctCalled = ifelse(xPredicted==errorXWin,1,0)) %>% 
  #filter(errorXWin2>.35, errorXWin2<.65) %>% 
  #filter(season>=2012) %>% 
  group_by(season) %>% 
  summarise(percMatchupCorrect = mean(correctCalled)) %>% 
  arrange(-percMatchupCorrect)



##### calibration plot #############
k_optimization$bins <- cut(k_optimization$errorXWin2, breaks = c(0,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,1))

k_optimization %>% filter(season==2020) %>% 
  group_by(bins) %>% 
  summarise(prob = mean(errorXWin)) %>% 
  ggplot() + geom_point(aes(x=bins, y=prob)) +
  geom_abline(intercept = 0, slope = .05) +
  labs(x="Predicted matchup win probability", y="Actual matchup win probability",
       title = "How Well Calibrated Are The Single Seater's Predictions?") +
  coord_flip() +
  ylim(0,1) +
  theme_bw()
  
ggsave("C:/Users/drewb/Desktop/calibration_plot.png", height = 4, width = 6)

k_optimization

fwrite(tracker, "elo_tracker.csv")








#############################  GRAPHS  ##########################################

#  elo rating by date since 2008  #
tracker %>% filter(date!="2021-01-01", date>"2019-01-01", driver %in% c("Colton Herta", "Marco Andretti")) %>%
  ggplot(aes(x=date, y=EloRating, color=driver)) + geom_line() +
  labs(x="Date", title = "Elo Rating Over Time", subtitle = "Minimum 10 starts",
       color="Driver", y="EloRating") +
  theme_bw()

#  elo rating by race number in career since 2008  #
tracker %>%
  filter(date!="2021-01-01") %>% 
  group_by(driver) %>%
  mutate(my_ranks = order(order(date, decreasing=FALSE))) %>% 
  filter(driver %in% c("Colton Herta", "Marco Andretti")) %>%
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



now <- tracker %>% filter(year== 2020) %>% 
  group_by(driver) %>%
  slice(which.max(as.Date(date, '%m/%d/%Y')))
  

