devtools::install_github("drewbennison/indyscrapR")

library(indyscrapR)

race_results()

season_stats()

dt2 <- historical_elo_ratings()

dt3 <- dt2 %>% select(-season) %>% 
  group_by(driver) %>% 
  mutate(career_race = row_number()) %>% 
  select(-date) %>% 
  pivot_wider(id_cols = c(2,3), names_from=c(1), values_from =c(2)) %>% 
  arrange(career_race) %>% 
  select(-career_race)

dt3 <- dt2 %>% select(-season) %>% 
  group_by(driver) %>% 
  mutate(career_race = row_number()) %>%
  add_count() %>% 
  filter(n>14) %>% 
  select(-n) %>% 
  select(-date) %>% 
  pivot_wider(id_cols = c(2,3), names_from=c(1), values_from =c(2)) %>% 
  arrange(career_race) %>% 
  select(-career_race) %>% 
  as.matrix()


res <- cor(dt3, use = "pairwise.complete.obs")

res_mat <- matrix(c("Colton Herta"), ncol = 1)

fin_mat <- res[, res_mat[, 1]]


ch <- res %>% select(`Colton Herta`)
