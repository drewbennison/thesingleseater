library(data.table)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(magick)

data<-fread("C:/Users/drewb/Desktop/indycar_results.csv")

#Calculate AFP from every starting position
afp <- data %>%
  filter(year!=2019) %>% 
  group_by(st) %>% 
  summarise(xFP = mean(fin))

#Left join data with xFP for every driver's results
data <- data %>%
  filter(year==2019) %>% 
  left_join(afp, by=c("st" = "st")) %>% 
  mutate(xFPDifference=xFP-fin)

working <- data %>%
  filter(year==2019) %>%
  group_by(driver) %>% 
  mutate(percFavorablePass = 100*(sum(passesFor)/(sum(passesFor)+sum(passesAgainst))),
         favorableStart = ifelse(lapOneChange>=0, 1,
                                 ifelse(lapOneChange<0, 0, NA)),
         StartRetention = 100*mean(favorableStart),
         StartPM = sum(lapOneChange),
         Races = n(),
         PMperStart = StartPM/Races,
         Pts=sum(pts),
         xPoints = sum(xPts),
         AFP = mean(fin),
         ASP = mean(st),
         ATP = mean(atp),
         ATP25 = mean(atp25),
         PassEff = 100*(sum(passesFor)/ (sum(passesFor)+sum(passesAgainst))),
         RunningCheck = ifelse(status=="running",1,0),
         RunPerc = 100*mean(RunningCheck),
         AvgFastSpeed = mean(fastLapRank),
         Top5Perc = 100*(sum(inTopFive)/sum(laps)),
         ASPos = mean(xFPDifference)) %>% 
  #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
  distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, ASP, ATP, ATP25, PassEff, RunPerc, AvgFastSpeed, Top5Perc, ASPos)


#Get ranks
working$ATP_Rank=25-(rank(working$ATP))
working$ATP25_Rank=25-(rank(working$ATP25))
working$PassEff_Rank=rank(working$PassEff)
working$Speed_Rank=25-(rank(working$AvgFastSpeed))
working$Pts_Rank=rank(working$Pts)
working$xPts_Rank=rank(working$xPoints)
working$AEP_Rank=rank(working$ASPos)


working <- working %>% 
  select(driver, ATP_Rank, AEP_Rank, PassEff_Rank, Speed_Rank, Pts_Rank, xPts_Rank)

working<-melt(working, id="driver")

working<-data.table(working)
driver<-working[driver=="Simon Pagenaud",]
  
ggplot(working, aes(variable, value, fill=variable)) +
  geom_col(width = 1, position = "identity") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  scale_y_continuous(breaks = 0:nlevels(working$variable)) +
  facet_wrap(~driver) + 
  #transition_states(driver, transition_length = 8) + 
  labs(fill="Category", title = "Driver Performance") +
  ylab("") + xlab("") + coord_polar() + theme_grey() +
  theme(plot.title = element_text(size=22))

ggsave("radar.png", width=10, height = 10, units = "in")

a<-animate(a, duration = 30, fps = 25)
