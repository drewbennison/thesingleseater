library(tidyverse)
library(ggplot2)
library(gt)
library(lubridate)

dt <- read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/champPredictions/current_champ.csv")

champ_projections <- dt %>% 
  filter(season!=0) %>% 
  select(driver, totalPoints, chamPos, season) %>% 
  group_by(driver, chamPos) %>% 
  add_count() %>% 
  mutate(prob = 100*(round((n/max(season)),3)),
         exp = round(chamPos*.01*prob,2)) %>% 
  select(driver, chamPos, prob, exp) %>% 
  distinct()

champ_projections_exp <- champ_projections %>% 
  group_by(driver) %>% summarise(x=sum(exp))

champ_projections <- champ_projections %>% select(-exp)

champ_drivers <- champ_projections %>% ungroup() %>% select(driver) %>% unique()
places <- tibble(place=1:nrow(champ_drivers))

new_dt <- crossing(champ_drivers, places)
new_dt2 <- new_dt %>% left_join(champ_projections, by=c("driver"="driver","place"="chamPos")) %>% 
  replace(is.na(.), 0)

x <- new_dt2 %>% pivot_wider(names_from = place, values_from =prob) %>% 
  rename("Win Championship %" = `1`) %>% 
  mutate("Top 3" = `Win Championship %` + `2` + `3`,
         "Top 5" = `Win Championship %` + `2` + `3` + `4` + `5`,
         "Top 10" = `Top 5` + `6` + `7` + `8` + `9` + `10`,
         "Top 20" = `Top 10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20`) %>% 
  mutate("Top 3" = ifelse(`Top 3` >100, 100, `Top 3`),
         "Top 5" = ifelse(`Top 5` >100, 100, `Top 5`),
         "Top 10" = ifelse(`Top 10` >100, 100, `Top 10`),
        "Top 20" = ifelse(`Top 20` >100, 100, `Top 20`)) %>% 
          select(driver, `Win Championship %`, `Top 3`, `Top 5`, `Top 10`, `Top 20`)  %>% 
  left_join(champ_projections_exp, by="driver") %>% 
  rename("Expected Finish Position" = "x") %>% 
  arrange(`Expected Finish Position`) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**IndyCar 2022 Championship Simulation**")),
             subtitle = paste0("Simulating the remaining races ", max(dt$season), " times. Results as of ", today())) %>% 
  cols_label(driver = "Driver") %>% 
  tab_style(style = list(cell_borders(
    sides = "left",
    color = "black",
    weight = px(3)
  )
  ),
  locations = list(
    cells_body(
      columns = vars(`Expected Finish Position`)
    )
  )
  )%>%
  data_color(vars(`Expected Finish Position`),
             scales::col_numeric(palette = c("#097511", "white", "#ba4434"),
                                 domain = c(0, 45)))


gtsave(data = x, 
       filename = paste0("indycar_sim", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/drewb/Desktop/")
