library(tidyverse)

#make separate for loop if no caution periods
dt <- read.csv("C:/Users/drewb/OneDrive/Documents/caution_five_laps.csv")

race_laps <- 200
caution_starts <- c(36,45,135,166,176)
caution_ends <- c(41,50,150,171,182)
year <- 2010
trck <- "Homestead"
typ <- "Oval"

dt2 <- tibble(Year=2020, Track="Texas", TrackType="Oval", Group="3000",Lap=0, `Caution.Within.Five.Laps` = 0)

if(length(caution_starts) != 0) {

#for each lap
for(lap in c(1:race_laps)){
  #for each caution period, remove laps that are in that period
  count <- 0
  for(period in c(1:length(caution_starts))) {
    if( (lap > caution_starts[period]-1) && (lap < caution_ends[period]+6)){
      count <- count + 1
    } else{
      count <- count
    }
  }
  #check count and find valid laps
  if(count == 0){
    print(lap)
    #if the lap is the end of a caution
    caution <- 0
    if(lap %in% caution_ends){
      for(i in c(1:5)){
        if((lap+i) %in% caution_starts) {
          caution <- caution + 1
        } else{
          caution <- caution
        }
      }
      
      if(caution != 0){
        message(paste0(lap, " caution caused caution"))
        dt2 <- dt2 %>% add_row(Year=year, Track=trck, TrackType=typ, Group="Caution",Lap=lap, `Caution.Within.Five.Laps` = 1)
      } else{
        message(paste0(lap, " caution didn't cause caution"))
        dt2 <- dt2 %>% add_row(Year=year, Track=trck, TrackType=typ, Group="Caution",Lap=lap, `Caution.Within.Five.Laps` = 0)
      }
    } else{
      #green flag lap
      
      for(i in c(1:5)){
        if((lap+i) %in% caution_starts) {
          caution <- caution + 1
        } else {
          caution <- caution
        }
      }
      if(caution!=0) {
        message(paste0(lap, " green flag caused caution"))
        dt2 <- dt2 %>% add_row(Year=year, Track=trck, TrackType=typ, Group="Green Flag",Lap=lap, `Caution.Within.Five.Laps` = 1)
      } else {
        message(paste0(lap, " green flag didn't cause caution"))
        dt2 <- dt2 %>% add_row(Year=year, Track=trck, TrackType=typ, Group="Green Flag",Lap=lap, `Caution.Within.Five.Laps` = 0)
      }
    }
    }
}

#check caution laps
for(lap in c(1:race_laps)){
caution <- 0
if(lap %in% caution_ends){
  for(i in c(1:5)){
    if((lap+i) %in% caution_starts) {
      caution <- caution + 1
    } else{
      caution <- caution
    }
  }
  
  if(caution != 0){
    message(paste0(lap, " caution caused caution"))
    dt2 <- dt2 %>% add_row(Year=year, Track=trck, TrackType=typ, Group="Caution",Lap=lap, `Caution.Within.Five.Laps` = 1)
  } else{
    message(paste0(lap, " caution didn't cause caution"))
    dt2 <- dt2 %>% add_row(Year=year, Track=trck, TrackType=typ, Group="Caution",Lap=lap, `Caution.Within.Five.Laps` = 0)
  }
} 
}
} else {
  for(lap in c(1:race_laps)){
    dt2 <- dt2 %>% add_row(Year=year, Track=trck, TrackType=typ, Group="Green Flag",Lap=lap, `Caution.Within.Five.Laps` = 0)
  }
  
}

dt2 <- dt2 %>% filter(Lap!=0)
dt <- rbind(dt, dt2)
fwrite(dt, "caution_five_laps.csv")
