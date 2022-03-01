library(tidyverse)
library(gt)

#### load in the data ####
dt <- read_csv("C:/Users/drewb/Desktop/stpete.csv") %>% 
  group_by(`Driver Name`) %>% 
  arrange(Lap) %>% 
  #mutate(totalTime = cumsum(LapTime)) %>% 
  mutate(Status = ifelse(is.na(Status), "G", Status))

#### attempted leader on each lap - does not work ####
#t <- dt %>% group_by(Lap) %>% 
#  summarise(LeaderTime = min(totalTime))

#dt <- dt %>% left_join(t, by=c("Lap")) %>% 
#  mutate(GapToLeader = LeaderTime-totalTime)

#### in/out laps analysis ####
pit_laps <- dt %>% filter(Status %in% c("G", "K", "P"),
                          L2_PItoPO != 0 | L3_POtoSF != 0)
pit_laps_summary <- pit_laps %>% 
  group_by(`Driver Name`) %>% 
  summarise("Time in Pits per Stop" = round(sum(L2_PItoPO)/n()*2, 3),
            "Average In-Lap Time" = round(sum(L4_SFtoPI)/n()*2,3),
            "Average Out-Lap Time" = round(sum(L3_POtoSF)/n()*2,3)) %>% 
  mutate("Pit Stop Rank" = rank(`Time in Pits per Stop`),
         "In-Lap Rank" = rank(`Average In-Lap Time`, ties.method="random"),
         "Out-Lap Rank" = rank(`Average Out-Lap Time`)) %>% 
  rename(Driver = `Driver Name`) %>% 
  select(Driver, `Time in Pits per Stop`, `Pit Stop Rank`,
         `Average In-Lap Time`, `In-Lap Rank`,
         `Average Out-Lap Time`, `Out-Lap Rank`) %>% 
  arrange(`Time in Pits per Stop`)


pit_laps_summary %>%
  gt() %>%
  tab_header(
    title = "IndyCar Pit-Stop Rankings, St. Petersburg",
    subtitle = "Green flag pit-stops only"
  ) %>% 
  tab_source_note(
    source_note = "Table: @thesingleseater | thesingleseater.com / Data: IndyCar"
  ) %>% 
  tab_source_note(
    source_note = "Note: In-Lap = Start-Finish Line to Pit-In Line. Out-Lap = Pit-Out Line to Start-Finish Line"
  ) %>% 
  data_color(
    columns = vars(`Pit Stop Rank`, `In-Lap Rank`, `Out-Lap Rank`),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::orange_material", n = 10)),
      domain = NULL
    )) %>% 
gtsave("C:/Users/drewb/Desktop/pits.png")
