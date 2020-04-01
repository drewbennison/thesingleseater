import pandas as pd

'''
Right now, this doesn't take into account starting position, but that
can be changed by using the code from the original R script and 
looking at how the standard Elo Ratings are updated.

Or,
merge in starting position elo and update driver's elo based on that
'''

# The current elo ratings for all drivers
elo_ratings_table = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/4_1_2020_elo_ratings.csv")
# Drivers in this race
season_drivers = pd.read_csv("C:/users/drewb/desktop/elo_Drivers.csv")

race_drivers_elo = season_drivers.merge(elo_ratings_table, on='driver', how='left')

r = pd.DataFrame(data={'driver': ["test"], 'winprob': [0]})

for i in range(0, len(race_drivers_elo)):
    current_driver = race_drivers_elo.iloc[i, 0]
    current_q = 10**((race_drivers_elo.iloc[i, 2])/400)
    sum_opponents_q = 0

    for j in range(0, len(race_drivers_elo)):
        if race_drivers_elo.iloc[j, 0] != current_driver:
            opponent_q = 10**((race_drivers_elo.iloc[j, 2])/400)
            sum_opponents_q += opponent_q
    r_temp = pd.DataFrame(data={'driver': [current_driver],
                                'winprob': [(current_q/(current_q+sum_opponents_q))]})
    r = r.append(r_temp)

print(r[r.driver != "test"])