library(data.table)
library(tidyverse)

# Read in the data
data<-fread("C:/Users/drewb/Desktop/thesingleseater/datasets/master_backup/indycar_results.csv")

data <- data %>% filter(year == 2019)

#start by making a crossing data frame for every driver combo
driver_table <- crossing(data$driver, data$driver)
driver_table <- setnames(driver_table, c("data$driver", "data$driver1"), c("Driver1", "Driver2"))
driver_table$w <- 0

tracks <- c(1:17)

#For each track, for each driver, for all comparisons with other drivers:
#     - if driver finished below comparison driver, add a 1 (edge) to that row

for(race in tracks) {
  print("STARTING")
  df <- data %>% filter(raceNumber == race, ) %>% 
    select(atp, driver)
  print(race)

for(i in 1:nrow(df)) {
  row <- df[i,]
  current_driver = row$driver
  current_fin = row$atp
  
  for(j in 1:nrow(df)) {
    row2 <- df[j,]
    compare_driver = row2$driver
    compare_fin = row2$atp
    
    if(current_fin>compare_fin) {
      driver_table$w[driver_table$Driver1 == current_driver & driver_table$Driver2 == compare_driver] <- driver_table$w[driver_table$Driver1 == current_driver & driver_table$Driver2 == compare_driver] + 1
      }
  }
  
}
print(driver_table)

}

#Write to adjacency matrix. Note: open up csv after and change first column name to 'name'
x <- dcast(driver_table, Driver1 ~ Driver2)
fwrite(x, "am.csv")
