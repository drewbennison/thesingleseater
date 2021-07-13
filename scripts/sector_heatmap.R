library(data.table)
library(tidyverse)
library(here)
library(lubridate)

source("C:/Users/drewb/Desktop/thesingleseater/scripts/thesingleseater_theme.R")

#Read in data and driver results for that session
data<-fread("C:/Users/drewb/Downloads/indycar-topsectiontimes-p2.csv")
#drivers<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/sector_times/drivers.csv")

#Clean the driver's names to remove anything after the comma in IndyCar data
data$Driver<-gsub(",.*", "", data$Driver)

#Clean C/E/T column
data$`C/E/T`<-gsub("D", "", data$`C/E/T`)
data$`C/E/T`<-gsub("/", "", data$`C/E/T`)
data$`C/E/T`<-gsub("F", "", data$`C/E/T`)
setnames(data, "C/E/T", "Manufacturer")

#Calculate the averages for each sector
avg_per_section<-dcast(data, Section~., mean, value.var = "Speed")
setnames(avg_per_section, ".", "avgSecSpeed")
setkey(avg_per_section, Section)
setkey(data, Section)
data<-merge(data, avg_per_section)

data$percRelativeSection<-((data$Speed-data$avgSecSpeed)/data$avgSecSpeed)*100

#Merge in driver results for that session (last names only)
#setkey(drivers, "Driver")
setkey(data, "Driver")
#data<-merge(data, drivers, all.x = TRUE)

sectors<-c("SF to T1", "T1 to SS1", "SS1 to T2", "T2 to BS","BS to T3", "T3 to SS2", "SS2 to T4", "T4 to FS", "FS to SF", "Full Lap")

#Tile plot. Adjust limits depending on the number of sectors at that track
plot<-ggplot(data=data, aes(x=Section, y=reorder(Driver, -Car))) + 
  geom_tile(aes(fill=percRelativeSection)) + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits=c(-3,3)) +
  labs(fill = "% Faster\nThan Average\nSector Speed", title="2020 Indianapolis 500 Practice 2",
       subtitle = "Drivers' top sector times: Above average (purple) means faster",
       caption = "thesingleseater.com") + ylab("Driver") + xlab("Sector") + 
  scale_x_discrete(limits=sectors) +
  thesingleseater_theme() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot



#  Natural language output of who was quickest in each sector. ############################
for (i in sectors) {
  d <- data %>%
    filter(Section==i) %>% 
    top_n(1, Speed)
  message(paste0(d[,1]," was quickest through ",d[,2], " and was ", round(d[,10],2), "% faster than the average car."))
}

#  Get the "ultimate lap" in seconds:  ###################################################
top_times<-data %>% 
  group_by(Section) %>% 
  summarise(val=min(Time)) %>% 
  filter(Section !="Full Lap") %>% 
  mutate(clean=ms(val))

x<-sum(top_times[,3])

#This can be used to see where drivers gain/lose time relative to best possible time, static
#line of the ultimate lap, geom_line sector times in seconds across the lap. Theoretical
#best lap time is the static time. Maybe don't use static time cause all sectors are 
#not of equal length.

ggplot(top_times, aes(x=Section, y=clean)) + geom_point()

#  R^2 between each sector and the full lap time  ###################################

non_lap<-data %>% 
  filter(Section!="Full Lap")
lap<-data %>%
  filter(Section=="Full Lap") %>% 
  select(Driver, Speed)

merged<-non_lap %>% 
  left_join(lap, by="Driver")

for (i in sectors) {
  test<-merged %>% 
    filter(Section==i) %>% 
    select(Speed.x, Speed.y) 
  cor<-cor(test[,1], test[,2])
  message(i, " ", cor)
}
