library(data.table)
library(tidyverse)
data<-fread("C:/Users/drewb/Downloads/6_8_2020_champ_2.csv")

data <- data %>% filter(season!=0) %>% 
  select(driver, totalPoints, chamPos, season)

data <- data %>% group_by(driver, chamPos) %>% 
  add_count() %>% 
  mutate(prob = n/max(season)) %>% 
  select(driver, chamPos, n, prob) %>% 
  distinct()

final<-dcast(data, driver~chamPos, sum, value.var = "prob")

data %>%
  ggplot() + geom_col(aes(x=chamPos, y=prob, fill=driver)) + facet_wrap(~driver) +
  theme(legend.position = "none")+
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20")) +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits = c(0,1)) +
  labs(y="Probability of finishing season in position",
       x="Championship finishing position",
       title="2020 IndyCar Championship Predictions",
       subtitle = "After simulating the remaining races 2,000 times",
       caption = "www.thesingleseater.com")



final_long <- final %>% pivot_longer(-driver)

compare <- first %>% pivot_longer(-driver) %>% 
  left_join(final_long, by=c("driver", "name"))

compare <- compare %>% mutate(diff = value.y-value.x)

