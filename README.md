# thesingleseater
This respository contains the data and code behind my website, www.thesingleseater.com.

## datasets
The datasets folders contains csv files for historical championship odds, elo ratings, lap charts, and a master backup for all of the race stats shared on the website.

## scripts
The scripts folder contains the code for individual articles and ongoing projects for the website. The most frequently used scripts are:
#### champ_sim_hot.py
* Simulates the remaining races in the IndyCar season using Elo ratings to estimate the probability of every driver winning the IndyCar championship. The 'hot' refers to the fact that Elo ratings are updated after every simulated race.
#### elo.R
* Calculates the current and historical Elo ratings for all drivers in the IndyCar series since 2008. 
#### raceStats.py
* Takes a race lap chart as input and calculates a number of statistics for that race.
#### lapchart_clean.R
* Takes an unclean IndyCar lapchart and cleans it so that it can be used in raceStats.py
#### sector_heatmap.R
* Produces a sector heatmap chart that visually shows every drivers' strong and weak points around the lap.
