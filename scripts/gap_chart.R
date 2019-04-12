library(data.table)
library(tidyverse)

data<-fread("C:/Users/drewb/Downloads/raw.csv")
teams<-fread("C:/Users/drewb/Desktop/race_strategists.csv")

teams<- teams %>% 
  mutate(LastName = sub(".*? (.+)", "\\1", teams$Driver)) %>% 
  select(LastName, Team)

sub(".*? (.+)", "\\1", D$name)

data %>% 
  filter(LapTime<120) %>% 
  ggplot(aes(x=Lap, y=LapTime)) + geom_line() +
  facet_wrap(~LastName)

data %>%
  filter(LastName=="Rossi", Lap %in% c(2:12)) %>% 
  ggplot(aes(x=Lap, y=LapTime)) + geom_line()


# Gap Chart
# t finds the completed time of the quickest car, by lap
# merge t into the data
t<-data[,.SD[which.min(ElapsedTime)], by = Lap]

t<- t %>% 
  select(Lap, ElapsedTime) %>% 
  rename('LeaderElapsed' = ElapsedTime) %>% 
  select(Lap, LeaderElapsed)

#Pace time of winner #################################

pacetime<-tibble(Lap=0:1, PaceTime=c(0,120.0343))
pacetime <-data.table(pacetime)

for (i in 2:60) {
  nextrow<-tibble(Lap=i, PaceTime=120.0343*i)
  nextrow<-data.table(nextrow)
  l=list(pacetime, nextrow)
  pacetime<-rbindlist(l)
}
########################################################

delta<-data %>%
  left_join(t, by="Lap") %>% 
  mutate(delta=ElapsedTime-LeaderElapsed) %>% 
  filter(delta<110, LastName %in% c("Power", "Herta", "Newgarden", "Rossi")) %>% 
  left_join(teams, by="LastName") %>% 
  left_join(pacetime, by="Lap") %>% 
  mutate(deltaPaceTime=ElapsedTime-PaceTime)

pits<- delta %>%
  filter(Pit==TRUE)

ggplot(delta, aes(x=Lap, y=deltaPaceTime)) + geom_line(aes(color=LastName)) +
  scale_y_reverse() + annotate("rect", xmin = 43, xmax = 50, ymin = -300, ymax = 150, alpha = 0.25, 
                               color="yellow", fill="yellow") +
  geom_point(data=pits, aes(x=Lap, y=deltaPaceTime), color="red", size=2, alpha=.8) +
  scale_x_discrete(limits=c(0:60)) +
  ylab("Delta Pace Time") + labs(title="Lap Chart: COTA 2019", 
                                       subtitle = "Red marks indiciate pitstop") +
  annotate("segment", x = 0, xend = 60, y = 0, yend = 0,
           colour = "blue")
  
