library(tidyverse)

dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/restartdata.csv")

dt %>% filter(year==2020) %>% 
  group_by(driver) %>% 
  summarise(`Restart Plus Minus` = sum(restartPM)) %>% 
  ggplot() +
  geom_col(aes(x=fct_reorder(driver,-`Restart Plus Minus`), y=`Restart Plus Minus`)) +
  labs(x="", y="Season Restart +/-",
       title = "Who has been best on restarts this season?",
       subtitle = "Net positions gained or lost on caution restarts",
       caption = "thesingleseater.com | @thesingleseater") +
  theme_bw() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
