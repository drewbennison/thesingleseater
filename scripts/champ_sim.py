import pandas as pd
import random
from datetime import datetime
import numpy as np
pd.options.mode.chained_assignment = None

'''
Needed updates:
- update number of races left each time and double points races
- hot elo ratings update after each race
'''
points_table = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/points_table.csv")

# The current elo ratings for all drivers
elo_ratings_table = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")
elo_ratings_table = elo_ratings_table[elo_ratings_table.year != 2000]
elo_ratings_table = elo_ratings_table[elo_ratings_table.groupby('driver').date.transform('max')==elo_ratings_table['date']]
elo_ratings_table = elo_ratings_table[['driver', 'EloRating']]

final_results = pd.DataFrame(data={'driver': ['test'], 'totalPoints': [0], 'chamPos': [0], 'season': [0]})

# read in master stats, select top n drivers by current points
season_drivers_file = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv", encoding = "ISO-8859-1")
season_drivers_file = season_drivers_file[season_drivers_file.year == 2020]
season_drivers_file = season_drivers_file.groupby('driver')['pts'].agg(points='sum')
season_drivers_file = season_drivers_file.reset_index(level=['driver'])

# for each season, create a season race results table
for season in range(1, 1001):
    print(season)
    startTime = datetime.now()
    race_results = pd.DataFrame(data={'season': [0], 'race': [0], 'driver': ["test"], 'fin': [0]})
    # The drivers that will compete in the season and their current points standings
    # This might be read in from master_results in the future/once the season starts
    # Must be in form driver, points
    season_drivers = season_drivers_file

    # for each race of that season
    for race in range(1, 14):
        race_drivers_elo = season_drivers.merge(elo_ratings_table, on='driver', how='left')
        race_drivers_elo = race_drivers_elo.fillna(1400)
        drivers_who_won = []
        # simulate that actual race
        for subRace in range(0, len(race_drivers_elo)):
            r = pd.DataFrame(data={'driver': ["test"], 'winprob': [0]})

            # go through every driver left in the race and keep track of their Elo ratings
            for i in range(0, len(race_drivers_elo)):
                current_driver = race_drivers_elo.iloc[i, 0]
                current_q = 10**((race_drivers_elo.iloc[i, 2])/400)
                sum_opponents_q = 0

                # go through all of the other drivers that aren't the above driver and keep track of their Elo ratings
                for j in range(0, len(race_drivers_elo)):
                    if race_drivers_elo.iloc[j, 0] != current_driver:
                        opponent_q = 10**((race_drivers_elo.iloc[j, 2])/400)
                        sum_opponents_q += opponent_q
                r_temp = pd.DataFrame(data={'driver': [current_driver],
                                            'winprob': [(current_q/(current_q+sum_opponents_q))]})
                r = r.append(r_temp)
            ids = list(range(0, len(race_drivers_elo)))

            # pick a winner based on drivers who have not "won" yet and remove them from the next sub race
            newdt = r[r['driver'] != "test"]
            newdt['ids'] = ids
            newdt['cs'] = 0

            for id_x in ids:
                cs = sum(newdt['winprob'][newdt['ids'] < id_x+1])
                newdt.iloc[id_x, 3] = cs

            random_num = random.uniform(0, 1)

            for id_x in ids:
                if random_num < newdt.iloc[id_x, 3]:
                    winner = newdt.iloc[id_x,0]
                    break

            race_results_temp = pd.DataFrame(data={'season': [season], 'race': [race], 'driver': [winner], 'fin': [subRace+1]})
            race_results = race_results.append(race_results_temp)
            drivers_who_won.append(winner)
            race_drivers_elo = race_drivers_elo[~race_drivers_elo.driver.isin(drivers_who_won)]

    print(datetime.now() - startTime)

    season_results = race_results[race_results.fin != 0]
    season_results = season_results.merge(points_table, on='fin', how='left')
    season_results['points'] = [x*2 if y == 7 or y == 13 else x for x,y in zip(season_results['points'], season_results['race'])]
    season_results = season_results.groupby('driver').agg({'points': 'sum'})
    season_results = season_results.merge(season_drivers, on='driver', how='left')
    season_results['totalPoints'] = season_results['points_x']+season_results['points_y']
    season_results['chamPos'] = season_results['totalPoints'].rank(method='first', ascending=False)
    season_results['season'] = season
    season_results = season_results[['driver', 'totalPoints', 'chamPos', 'season']]
    final_results = final_results.append(season_results)

final_results.to_csv("6_8_2020_champ.csv")

