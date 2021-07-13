library(data.table)
library(tidyverse)
library(lubridate)
#change from 3000 to 4000 simulations has a max difference of around 2%
data<-fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/champPredictions/2021_5_3_champ.csv")
#over_time <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/champPredictions/2020_champ_predictions.csv")

data <- data %>% filter(season!=0) %>% 
  select(driver, totalPoints, chamPos, season)

data <- data %>% group_by(driver, chamPos) %>% 
  add_count() %>% 
  mutate(prob = n/max(season)) %>% 
  select(driver, chamPos, n, prob) %>% 
  distinct()

final<-dcast(data, driver~chamPos, sum, value.var = "prob")

final %>% filter(`1` > 0) %>% select(driver, `1`) %>% 
  ggplot(aes(x=fct_reorder(driver,-`1`), y=`1`)) + geom_col() +
  labs(x="", y="Probability of winning the championship",
       title = "IndyCar Championship Projection: last updated 7/14/2020",
       subtitle = "Results of 10,000 simulatiions of remaining races") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0,1)

data %>%
  arrange(-prob) %>% 
  filter(driver %in% c("Scott Dixon", "Simon Pagenaud", "Josef Newgarden", "Alexander Rossi", "Ryan Hunter-Reay", "Will Power")) %>% 
  ggplot() + geom_col(aes(x=chamPos, y=prob, fill=driver)) + facet_wrap(~driver, ncol=1) +
  theme(legend.position = "none")+
  scale_x_discrete(limits = c(1, 5, 10, 15, 20, 25, 31)) +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits = c(0,1)) +
  labs(y="Probability of finishing season in position",
       x="Championship finishing position",
       title="2020 IndyCar Championship Predictions",
       subtitle = "After simulating the remaining races 4,000 times",
       caption = "www.thesingleseater.com")

over_time <- over_time %>% filter(season!=0) %>% 
  select(driver, totalPoints, chamPos, season, date)

over_time <- over_time %>% group_by(driver, chamPos, date) %>% 
  add_count() %>% 
  mutate(prob = n/max(season)) %>% 
  select(driver, chamPos, n, prob) %>% 
  distinct()

over_time %>% filter(chamPos==1) %>% group_by(driver) %>% select(-n) %>% 
  add_count() %>% ungroup() %>%  filter(n==3) %>% 
  ggplot(aes(x=mdy(date), y=prob, color=driver)) + geom_line()
