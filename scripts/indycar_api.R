library(jsonlite)

master_dt <- tibble()

for(i in c(1:40)) {
tmp <- tempfile()
url <- "http://racecontrol.indycar.com/xml/timingscoring.json"
download.file(url, destfile = tmp,quiet = FALSE, mode = "wb")

line = readLines(tmp)
line = line[2]

write(line, "C:/Users/drewb/Desktop/test.json")

wb <- jsonlite::fromJSON("C:/Users/drewb/Desktop/test.json")

new_dt <- wb[["timing_results"]][["Item"]]
current_time <- Sys.time()
new_dt$time_stamp <- current_time

master_dt <- rbind(master_dt, new_dt)
Sys.sleep(30)
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
