library(tidyverse)
library(data.table)

#these lines take in a dirty lap chart and make it usable
dt <- read.csv("C:/Users/drewb/Downloads/indycar-sectionresults-pf.csv")
# t column, driver name column, time column (they will be called different things possibly)
dt <- dt %>% select(X, X.1, X.11)
dt <- dt %>% mutate(X.1 = as.character(X.1))
dt <- dt %>% 
  mutate(X.1 = ifelse(X.1 %like% "Section Data for", X.1, as.character(X)))

dt$X.1 <- as.character(dt$X.1)

dt <- dt %>% mutate(
         Driver = ifelse(X.1 %like% "Section Data for", trimws(sub('.*-', '', dt$X.1)),0))

dt <- dt %>% filter(X=="T" | Driver != "0")
dt <- dt %>% mutate(Driver = ifelse(Driver == "0", NA, Driver))
dt <- dt %>% fill(Driver, .direction = "down")
dt <- dt %>% filter(X=="T", X.11!="") 
dt <- dt %>% select(X.11, Driver) %>% 
  rename(LapTime = 1)

#add in lap numbers and convert times to seconds
dt <- dt %>% group_by(Driver) %>% mutate(Lap = row_number()) %>% 
  ungroup()
dt <- dt %>% mutate(LapTime = seconds(as.character(LapTime)))
dt <- dt %>% group_by(Driver) %>% arrange(Lap) %>% mutate(cs = cumsum(LapTime))

#plot of lap times throughout the race
dt %>% filter(Driver %in% c("Newgarden, Josef", "Power, Will", "Chilton, Max")) %>% 
  filter(LapTime < 73.5) %>% 
  ggplot(aes(x=Lap, y=LapTime, color=Driver)) + geom_point(size=1) +
  #annotate("rect", xmin = 38, xmax = 45, ymin = 20, ymax = 80,
  #         alpha = .2, color="yellow") +
  geom_smooth(se=FALSE) +
  labs(title = "Lap times on long runs during Warmup for the Indianapolis GP",
       x="Lap", y="Lap time in seconds",  caption = "thesingleseater.com | @thesingleseater") +
  theme_minimal()

#### attempted leader on each lap ####
t <- dt %>% group_by(Lap) %>% 
  summarise(LeaderTime = min(cs))

t2 <- dt %>% left_join(t, by=c("Lap")) %>% 
  mutate(GapToLeader = LeaderTime-cs)


#### box plot of lap speeds ####
dt %>% filter(LapTime < 73.1) %>%  
  ggplot() + geom_boxplot(aes(x=reorder(Driver, Driver), y=LapTime)) +
  labs(title = "Full Speed Lap Times from Warmup at the Indianapolis GP",
       x="", y="Lap Time (seconds)", caption = "thesingleseater.com | @thesingleseater") +
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
  


