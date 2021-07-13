library(data.table)
library(tidyverse)

stats <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

race <- read.csv("C:/Users/drewb/desktop/fantasy_drivers.csv")

ytd <- stats %>% filter(year==2020) %>% group_by(driver) %>% 
  summarise(pts = mean(pts,na.rm = TRUE),
            xpts = mean(xPts,na.rm = TRUE),
            xpts25 = mean(xPtsATP25,na.rm = TRUE),
            ledPoints = mean(ledPts, na.rm = TRUE),
            polePoints = mean(polePoints, na.rm = TRUE),
            ledMostPts = mean(ledMostPts, na.rm = TRUE))

last_season <- stats %>% filter(year==2019) %>% group_by(driver) %>% 
  summarise(pts = mean(pts,na.rm = TRUE),
            xpts = mean(xPtsATP,na.rm = TRUE),
            xpts25 = mean(xPtsATP25,na.rm = TRUE),
            ledPoints = mean(ledPts, na.rm = TRUE),
            polePoints = mean(polePoints, na.rm = TRUE),
            ledMostPts = mean(ledMostPts, na.rm = TRUE))

track_td <-stats %>% filter(track=="Road America") %>% group_by(driver) %>% 
  summarise(pts = mean(pts,na.rm = TRUE),
            xpts = mean(xPtsATP,na.rm = TRUE),
            xpts25 = mean(xPtsATP25,na.rm = TRUE),
            ledPoints = mean(ledPts, na.rm = TRUE),
            polePoints = mean(polePoints, na.rm = TRUE),
            ledMostPts = mean(ledMostPts, na.rm = TRUE))

track_type <- stats %>% filter(type=="road") %>% group_by(driver) %>% 
  summarise(pts = mean(pts,na.rm = TRUE),
            xpts = mean(xPtsATP,na.rm = TRUE),
            xpts25 = mean(xPtsATP25,na.rm = TRUE),
            ledPoints = mean(ledPts, na.rm = TRUE),
            polePoints = mean(polePoints, na.rm = TRUE),
            ledMostPts = mean(ledMostPts, na.rm = TRUE))

final <- race %>% left_join(ytd) %>% left_join(last_season, by="driver") %>% 
  left_join(track_td, by="driver") %>% left_join(track_type, by="driver")

for(i in 2:ncol(final)){
  final[is.na(final[,i]), i] <- mean(final[,i], na.rm = TRUE)
}

final <- final %>% group_by(driver) %>% 
  summarise(pts = .45*pts.x + .3*pts.y + .1*pts.x.x + .15*pts.y.y,
            xpts = .45*xpts.x + .3*xpts.y + .1*xpts.x.x + .15*xpts.y.y,
            xpts25 = .45*xpts25.x + .3*xpts25.y + .1*xpts25.x.x + .15*xpts25.y.y,
            ledpts = .45*ledPoints.x + .3*ledPoints.y + .1*ledPoints.x.x + .15*ledPoints.y.y,
            polepts = .45*polePoints.x + .3*polePoints.y + .1*polePoints.x.x + .15*polePoints.y.y,
            ledmostpts = .45*ledMostPts.x + .3*ledMostPts.y + .1*ledMostPts.x.x + .15*ledMostPts.y.y,
            cost = mean(cost))

exp <- final %>% group_by(driver) %>% 
  summarise(ExpectedPoints = .6*pts+.2*xpts+.2*xpts25+ledpts+polepts+ledmostpts,
            Cost = mean(cost))
