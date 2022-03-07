library(tidyverse)
library(gt)

#### load in the data ####
dt <- read_csv("C:/Users/drewb/Desktop/Projects/thesingleseater/datasets/rtools_lapcharts/rtools_2022_r01_r.csv") %>% 
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
  arrange(`Time in Pits per Stop`) %>% 
  mutate(x = (`Pit Stop Rank` + `In-Lap Rank` + `Out-Lap Rank`)/3)


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



race_results <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
race_results <- race_results %>% filter(year==2022, raceNumber==1) %>% 
  select(driver, fin) %>% 
  mutate(driver = sub(".* ", "", driver) )

#tires used throughout the race
dt$end_of_stint <- ifelse(dt$L2_PItoPO >0, 1, 0)
dt$start_of_stint_notice <- ifelse(dt$L3_POtoSF > 0 | dt$Lap == 1 , 1, 0)
dt$start_of_stint <- ifelse(dt$L3_POtoSF > 0 | dt$Lap == 1, dt$TIRE, NA)
dt$current_tire <- dt$start_of_stint
dt <- dt %>% fill(current_tire, .direction = "down")

dt <- dt %>% left_join(race_results,  by=c("Driver Name" = "driver"))


dt %>% 
  ggplot(aes(x=Lap, y=reorder(`Driver Name`, -fin), color=current_tire,
             fill = ifelse(start_of_stint_notice == 1, current_tire, NA))) +
  geom_point(size=1,
             shape=21,
             stroke=1) +
  scale_color_manual(values = c("red", "black")) +
  scale_fill_manual(values=c("red", "black"),na.value = "transparent") +
  guides(fill= FALSE) +
  labs(y="",
       color="Tire",
       title = "Tire chart from the IndyCar Firestone GP at St. Petersburg",
       subtitle = "Filled in circle = first lap on that stint",
       caption = "Chart: @thesingleseater | thesingleseater.com -- Data: IndyCar")

ggsave("C:/Users/drewb/Desktop/tire_chart.png", width = 9, height = 6, dpi = 900)
