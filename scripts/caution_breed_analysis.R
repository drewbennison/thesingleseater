library(tidyverse)
library(lubridate)
#### load in the data ####
options(scipen=999)
dt <- read.csv("C:/Users/drewb/OneDrive/Documents/caution_five_laps.csv")
dt <- dt %>% mutate(TrackType = tolower(TrackType),
                    Year = as.Date(ISOdate(Year, 1, 1)))

#### all tracks ####
dt %>% filter(Group!="3000") %>% 
  mutate(TrackType = tolower(TrackType)) %>% 
  group_by(Group) %>% summarise(x = mean(Caution.Within.Five.Laps),
                                           y = n())

#### analysis by track types ####
dt %>% filter(Group!="3000") %>% 
  mutate(TrackType = tolower(TrackType)) %>% 
  group_by(Group, TrackType) %>% summarise(x = mean(Caution.Within.Five.Laps),
                                           y = n())

#### counts ####
freq <- dt %>% filter(Group!= "3000") %>% group_by(Track, TrackType, Group) %>% count()

#### tracks with the biggest difference between caution restart p and green flag p ####
by_track <- dt %>% group_by(Group, Track, TrackType) %>%
  filter(Group!="3000") %>% 
  summarise(x = mean(Caution.Within.Five.Laps,)) %>% 
  left_join(freq) %>% 
  pivot_wider(names_from = Group, values_from = c(x,n)) %>% 
  #implement test for statistical significance
  mutate(Diff = (x_Caution-`x_Green Flag`),
         pvalue = prop.test(x=c(x_Caution*n_Caution, `x_Green Flag`*`n_Green Flag`), n=c(`n_Caution`, `n_Green Flag`), alternative = "two.sided", correct = "FALSE")$p.value,
         StatisticallySignificant = ifelse(pvalue<.05,"Yes", "No")) %>% 
  arrange(-Diff)

#### graph by year ####
dt %>% filter(Group!="3000", Year<"2020-01-01") %>% 
  mutate(TrackType = tolower(TrackType)) %>% 
  group_by(Year, Group, TrackType) %>% summarise(x = mean(Caution.Within.Five.Laps),
                                           y = n()) %>% 
  ggplot() + geom_smooth(aes(x=Year, y=x, color=TrackType),se=FALSE) +
  labs(x="", y="Probability", title = "Probability of seeing a caution within 5 laps of the end\nof another caution period",
       caption = "@thesingleseater | thesingleseater.com", color="Track Type") +
  thesingleseater_theme()

#### fun article questions ####
dt %>% filter(Track=="Indianapolis", Lap>169, Lap<191) %>% 
  group_by(Group) %>% summarise(x = mean(Caution.Within.Five.Laps),
                                y = n())
#### when there is a caution, what lap does it come on? ####
dt %>% filter(Track == "Indianapolis") %>% 
  arrange(Year, Lap)



























