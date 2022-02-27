library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(ggraph)
library(tidygraph)

dt <- fread("C:/Users/drewb/Desktop/2022_02_27_r.csv")


dt2 <- dt %>% select(rank, diff, gap, overallRank, startPosition, bestLapTime, lastLapTime, laps, totalTime,
                     status, pitStops, sincePitLap, flagStatus, no, firstName, lastName, LastSpeed, 
                     time_stamp, BestSpeed, Passes, Passed, NTBestSpeed, totalTime) %>% 
  mutate(time_stamp = as_datetime(time_stamp),
         num_colons  = str_count(totalTime, ":"),
         total_time_split = str_split(totalTime, ":"))

dt2$answer <- "-10"

for(i in 1:nrow(dt2)){
  dt2[i,25] <- case_when(dt2[i,23]==0 ~ as.numeric(dt2[i,9]),
                         dt2[i,23]==1 ~ as.numeric(dt2[i,24][[1]][[1]][1])*60 + as.numeric(dt2[i,24][[1]][[1]][2]),
                         dt2[i,23]==2 ~ as.numeric(dt2[i,24][[1]][[1]][1])*60*60 + as.numeric(dt2[i,24][[1]][[1]][2])*60 + as.numeric(dt2[i,24][[1]][[1]][3]))
}

dt2$answer <- as.numeric(dt2$answer)

dt3 <- dt2 %>% group_by(lastName, laps) %>% 
  top_n(1, time_stamp) %>% 
  ungroup() %>% 
  select(laps, lastName, answer)

dt4 <- dt3

dt5 <- dt3 %>% left_join(dt4, by="laps") %>% 
  filter(lastName.x != lastName.y) %>% 
  mutate(gap = answer.x-answer.y) %>% 
  filter(gap>=0, gap<5) %>% 
  mutate(alpha = ifelse(lastName.x < lastName.y, 1, 0),
         alphaname = ifelse(alpha==1, paste0(lastName.x, ":", lastName.y), paste0(lastName.y, ":", lastName.x))) %>% 
  group_by(alphaname) %>% 
  add_count() %>% 
  mutate(gap = (gap*-1)) %>% 
  #filter(n>40) %>%
  select(alphaname, gap, n) %>% 
  summarise(totals2 = sum(`gap`),
            nn = mean(n)) %>% 
  separate(alphaname, c("driver1", "driver2"), sep = "([:])") %>%
  mutate(totals = totals2/nn)  %>% 
  filter(nn>20) %>% 
  select(-nn, -totals2)
  
g <- dt5  %>% mutate(driver1 = as.factor(driver1),
         driver2 = as.factor(driver2)) %>% 
    as_tbl_graph() %>% 
  activate(edges)
  
ggraph(g, layout = "kk") +
    geom_edge_link(aes(edge_alpha = totals), edge_color = "black", show.legend = FALSE) +
    geom_node_point(size = 3, colour = '#69a5d6') +
    geom_node_label(aes(label=name,), repel = TRUE) +
    theme_graph()

ggsave("C:/Users/drewb/Desktop/graph.png", height = 6, width = 9)
  