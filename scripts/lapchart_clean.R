library(tidyverse)
library(readxl)

d <- read_xlsx("C:/Users/drewb/Downloads/indycar-race-lapchart2.xlsx")
d$ID <- seq.int(nrow(d))

start <- which(d$`Event:` == "Drivers in Race:")
stop <- which(d$`Event:` == "Pit Stop")

lapchart <- tibble()

#make one long lap chart
for(i in 1:length(start)) {
  print(i)
  temp_lapchart <- d %>% filter(ID >start[i] & ID < stop[i]-1) %>% 
    select(-ID)
  temp_lapchart <- data.table(temp_lapchart)
  
  #temp_lapchart <- temp_lapchart %>% select_if(colSums(!is.na(.)) > 0)
  temp_lapchart <- temp_lapchart[,c(-1,-2)]
  
  if(i == 1){
    lapchart <- temp_lapchart
  } else{
    lapchart <- cbind(lapchart, temp_lapchart)
  }
  print(temp_lapchart)
  #temp_lapchart <- temp_lapchart %>% select_if(colSums(!is.na(.)) > 0)
  print(temp_lapchart)
}

#clean lapchart
new_names <- as.character(c(1:ncol(lapchart)))
names(lapchart) <- new_names

x <- lapchart %>% select_if(~!is.na(.[1L]))
new_names <- as.character(c(1:ncol(x)))
names(x) <- new_names

