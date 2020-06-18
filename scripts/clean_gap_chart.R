library(tidyverse)
library(data.table)

#these lines take in a dirty lap chart and make it usable
dt <- read.csv("C:/Users/drewb/Downloads/indycar-sectionresults-race.csv")
dt <- dt %>% select(Event., X, Genesys.300, X.8)
dt <- dt %>% mutate(Genesys.300 = as.character(Genesys.300))
dt <- dt %>% 
  mutate(Genesys.300 = ifelse(Genesys.300 %like% "Section Data for", Genesys.300, as.character(X)))

dt$Genesys.300 <- as.character(dt$Genesys.300)

dt <- dt %>% mutate(
         Driver = ifelse(Genesys.300 %like% "Section Data for", trimws(sub('.*-', '', dt$Genesys.300)),0))

dt <- dt %>% filter(X=="T" | Driver != "0")
dt <- dt %>% mutate(Driver = ifelse(Driver == "0", NA, Driver))
dt <- dt %>% fill(Driver, .direction = "down")
dt <- dt %>% filter(X=="T", X.8!="") 
dt <- dt %>% select(X.8, Driver) %>% 
  rename(LapTime = 1)

#add in lap numbers and convert times to seconds
dt <- dt %>% group_by(Driver) %>% mutate(Lap = row_number()) %>% 
  ungroup()
dt <- dt %>% mutate(LapTime = seconds(as.character(LapTime)))
dt <- dt %>% group_by(Driver) %>% arrange(Lap) %>% mutate(cs = cumsum(LapTime))

#plot of lap times throughout the race
dt %>% filter(Driver %in% c("Dixon, Scott", "Pagenaud, Simon", "Newgarden, Josef")) %>% 
  ggplot(aes(x=Lap, y=LapTime, color=Driver)) + geom_line(size=1) +
  annotate("rect", xmin = 38, xmax = 45, ymin = 20, ymax = 80,
           alpha = .2, color="yellow") +
  annotate("rect", xmin = 77, xmax = 85, ymin = 20, ymax = 80,
           alpha = .2, color="yellow") +
  annotate("rect", xmin = 191, xmax = 196, ymin = 20, ymax = 80,
           alpha = .2, color="yellow") +
  labs(title = "Lap Times of Podium Finishers During the Genesys 300",
       x="Lap", y="Lap time in seconds",  caption = "thesingleseater.com | @thesingleseater") +
  theme_minimal()


#### box plot of lap speeds ####
dt %>% filter(LapTime < 27) %>%  
  ggplot() + geom_boxplot(aes(x=reorder(Driver, Driver), y=LapTime)) +
  labs(title = "Full Speed Lap Times from the Genesys 300 at Texas",
       x="", y="Lap Time", caption = "thesingleseater.com | @thesingleseater") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#### plot of gap from one driver to another ####

dt2 <- dt %>% mutate(key = 0)
dt3 <- dt %>% mutate(key = 0)

dt4 <- dt2 %>% left_join(dt3, by=c("key", "Lap")) %>% 
  filter(Driver.x!=Driver.y) %>% 
  mutate(`GapToDriver.x` = `cs.x`-`cs.y`)


Driver1 = "Herta, Colton"
Driver2 = "Pagenaud, Simon"

Driver1a <- sub(",.*$", "", Driver1)
Driver2a <- sub(",.*$", "", Driver2)

dt4 %>% filter(Lap >0) %>% 
  filter(Driver.x==Driver1, Driver.y==Driver2) %>% 
  ggplot(aes(x=Lap, y=GapToDriver.x)) +
  geom_line() + 
  annotate("rect", xmin = 38, xmax = 45, ymin = -40, ymax = 10,
           alpha = .2, color="yellow") +
  annotate("rect", xmin = 77, xmax = 85, ymin = -40, ymax = 10,
           alpha = .2, color="yellow") +
  annotate("rect", xmin = 191, xmax = 196, ymin = -40, ymax = 10,
           alpha = .2, color="yellow") +
  annotate("segment", x=0, xend = 200, y=0, yend = 0) +
  annotate("text", x = 50, y = 25, label = paste0(Driver2a, " is ahead")) +
  annotate("text", x = 50, y = -25, label = paste0(Driver1a, " is ahead")) +
  labs(title = paste0("Gap from ", Driver1a, " to ", Driver2a, " During the Genesys 300"),
       x = "Lap", y="Gap in seconds",
       caption = "thesingleseater.com | @thesingleseater") + theme_minimal() 
  


