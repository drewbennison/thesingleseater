library(data.table)
library(tidyverse)
library(readxl)
library(stringi)

d <- read_xlsx("C:/Users/drewb/Downloads/indycar-race-lapchart.xlsx")
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
  
  temp_lapchart <- temp_lapchart[,c(-1,-2)]
  
  if(i == 1){
    lapchart <- temp_lapchart
  } else{
    lapchart <- cbind(lapchart, temp_lapchart)
  }
  print(temp_lapchart)
  print(temp_lapchart)
}

#clean lapchart by renaming columns and remove NA columns
new_names <- as.character(c(1:ncol(lapchart)))
names(lapchart) <- new_names
x <- lapchart %>% select_if(~!is.na(.[1L]))
new_names <- as.character(c(1:ncol(x)))
names(x) <- new_names

#Get driver names for stat update
driver_names <- d %>% filter(ID >start[1] & ID < stop[1]-1) %>% 
  select(`Event:`)

driver_names$number <- stri_extract_first_regex(driver_names$`Event:`, "[0-9]+")
driver_names$name <- sub(".*-", "", driver_names$`Event:`)
driver_names$name <- gsub( ",.*$", "", driver_names$name )
driver_names$name <- trimws(driver_names$name)

driver_names <- driver_names %>% select(name, number)


#### Save clean lapchart and driver names ####
fwrite(x, "C:/Users/drewb/Desktop/Projects/thesingleseater/datasets/lap_charts/2021_r12_indy_lapchart.csv")
fwrite(driver_names, "C:/Users/drewb/Desktop/Projects/thesingleseater/datasets/lap_charts/2021_r12_indy_driver_names.csv")
