library(data.table)
library(tidyverse)
library(plyr)

data<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/indycar_results.csv")
source("C:/Users/drewb/Desktop/thesingleseater/scripts/thesingleseater_theme.R")

#Standardize track names
data$track[data[,data$track=="Alabama"]] <- "Barber"
data$track[data[,data$track=="Detroit"]] <- "Belle Isle"
data$track[data[,data$track=="Indianapolis GP"]] <- "Indy GP"

num_races <- data %>% filter(fin==1) %>% select(track) %>% group_by(track) %>% count()

xx <- data %>% select(track, st, fin, type) %>% left_join(num_races) %>% filter(freq > 4)

func <- function(xx) {
  return(data.frame(COR = cor(xx$st, xx$fin)))
}

func2 <- function(xx) {
  return(data.frame(model = summary(lm(xx$fin~xx$st))$coefficients[2,1]))
}

result <- ddply(xx, .(track), func)

restult2 <- ddply(xx, .(track), func2)

result <- result %>% 
  arrange(-COR)


result %>% ggplot(aes(x=reorder(track,-COR), y=COR)) + geom_col() + thesingleseater_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x=element_blank()) +
  labs(y="Correlation", title = "Correlation between start/finish position") +
  scale_y_continuous(limits=c(0, .75))


indy <- xx %>% filter(track=="Indianapolis")

x<-lm(indy$fin~indy$st)
summary(x)
