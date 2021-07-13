library(jsonlite)
library(tidyverse)
library(data.table)
library(lubridate)

master_dt <- tibble()

# difftime(now(), lubridate::as_datetime("2021-04-26 12:42:00 EDT", tz = "US/Eastern"), units = "secs")

#wait until race time
#Sys.sleep(800)

#two hours
for(i in c(1:300)) {
  message(i)
tmp <- "C:/Users/drewb/Desktop/temp_test"
url <- "http://racecontrol.indycar.com/xml/timingscoring.json"
download.file(url, destfile = tmp,quiet = FALSE, mode = "wb")

line = readLines(tmp)
line = line[2]

write(line, "C:/Users/drewb/Desktop/test.json")

wb <- jsonlite::fromJSON("C:/Users/drewb/Desktop/test.json")

new_dt <- wb[["timing_results"]][["Item"]]
current_time <- Sys.time()
new_dt$time_stamp <- current_time

master_dt <- rbindlist(list(master_dt, new_dt), use.names = TRUE, fill = TRUE)
fwrite(master_dt, "C:/Users/drewb/Desktop/mid_ohio_2021.csv")
Sys.sleep(35)
}



#analysis
num_laps_at_each_point <- master_dt %>% group_by(lastName, laps) %>% 
  count()

master_dt %>% unique() %>% filter(lastName %in% c("Palou", "Power")) %>% 
  mutate(gap = as.numeric(gap),
         laps = as.numeric(laps)) %>% 
  filter(gap > 0) %>% 
  select(laps, gap) %>% unique() %>% 
  ggplot(aes(x=laps, y=gap)) + geom_line() +
  labs(x="Lap", y="Gap to leader", title = "Will Power seconds behind Alex Palou") +
  ylim(0,2.5) +
  theme_bw()


master_dt %>% mutate(rank2 = as.numeric(rank)) %>%  filter(rank2<=9) %>% 
  mutate(lap1 = as.numeric(lap1QualSpeed),
         lap2 = as.numeric(lap2QualSpeed),
         lap3 = as.numeric(lap3QualSpeed),
         lap4 = as.numeric(lap4QualSpeed)) %>% 
  select(lastName, lap1, lap2, lap3, lap4) %>% 
  mutate(FourLapAverage = (lap1 + lap2 + lap3 + lap4) / 4) %>% 
  select(-FourLapAverage) %>% 
  pivot_longer(cols = c("lap1", "lap2", "lap3", "lap4")) %>%
  ggplot(aes(x=name, y=value, group=lastName,color=lastName)) + geom_line() +
  labs(x="Lap", y="Speed", title = "Fast 9 Qualifying Speeds",
       color="Driver") +
  theme_bw()

ggsave("C:/Users/drewb/Desktop/plot.png", dpi = 800, height = 6, width = 8)
