import pandas as pd
import random
from datetime import datetime
import numpy as np
pd.options.mode.chained_assignment = None
pd.set_option('display.max_columns', 500)

'''
Needed updates:
- update number of races left each time and double points races
'''
points_table = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/points_table.csv")

final_results = pd.DataFrame(data={'driver': ['test'], 'totalPoints': [0], 'chamPos': [0], 'season': [0]})

# read in master stats for driver points
season_drivers_file = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv", encoding = "ISO-8859-1")
season_drivers_file = season_drivers_file[season_drivers_file.year == 2021]
season_drivers_file = season_drivers_file.groupby('driver')['pts'].agg(points='sum')
season_drivers_file = season_drivers_file.reset_index(level=['driver'])

# read in drivers and the races they are competing in this season
drivers_and_races = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/indycar_2021_drivers_races.csv", encoding = "ISO-8859-1")
drivers_and_races = drivers_and_races.merge(season_drivers_file, how='left', left_on="Driver", right_on="driver")
drivers_and_races = drivers_and_races.fillna(0)
drivers_and_races['driver'] = drivers_and_races['Driver']
drivers_and_races = drivers_and_races[['driver', 'round_1', 'round_2', 'round_3', 'round_4', 'round_5', 'round_6', 'round_7', 'round_8',
                                       'round_9', 'round_10', 'round_11', 'round_12', 'round_13', 'round_14', 'round_15', 'round_16', 'round_7','points']]

# elo ratings table file
elo_ratings_file = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")


# for each season, create a season race results table
for season in range(1, 1001):
    print(season)

    startTime = datetime.now()
    race_results = pd.DataFrame(data={'season': [0], 'race': [0], 'driver': ["test"], 'fin': [0]})
    # Must be in form driver, points
    season_drivers = drivers_and_races

    # The current elo ratings for all drivers
    elo_ratings_table = elo_ratings_file
    elo_ratings_table = elo_ratings_table[elo_ratings_table.year != 2000]
    elo_ratings_table = elo_ratings_table[elo_ratings_table.groupby('driver').date.transform('max') == elo_ratings_table['date']]
    elo_ratings_table = elo_ratings_table[['driver', 'EloRating']]

    # for each race of that season
    for race in range(2, 18):
        # season_drivers_file keep rows where race is in their list of races they will compete in
        season_drivers = drivers_and_races

        season_drivers = season_drivers.iloc[:,[0,race, len(drivers_and_races.columns)-1]]
        season_drivers = season_drivers.rename(columns={season_drivers.columns[1]:'round'})
        season_drivers = season_drivers[season_drivers['round'] == "y"]

        race_drivers_elo = season_drivers.merge(elo_ratings_table, on='driver', how='left')
        race_drivers_elo = race_drivers_elo.fillna(1500)

        drivers_who_won = []
        # simulate that actual race
        for subRace in range(0, len(race_drivers_elo)):
            r = pd.DataFrame(data={'driver': ["test"], 'winprob': [0]})

            # go through every driver left in the race and keep track of their Elo ratings
            for i in range(0, len(race_drivers_elo)):
                current_driver = race_drivers_elo.iloc[i, 0]
                current_q = 10**((race_drivers_elo.iloc[i, 3])/400)
                sum_opponents_q = 0

                # go through all of the other drivers that aren't the above driver and keep track of their Elo ratings
                for j in range(0, len(race_drivers_elo)):
                    if race_drivers_elo.iloc[j, 0] != current_driver:
                        opponent_q = 10**((race_drivers_elo.iloc[j, 3])/400)
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

        # update elo ratings after each race (hot)
        # select current race
        update_elo = race_results[race_results.race == race]
        update_elo2 = update_elo
        update_elo['key'] = 0
        update_elo2['key'] = 0
        # merge race results with itself and add in pre race elo ratings for every driver
        update_elo = update_elo.merge(update_elo2, on='key')
        update_elo = update_elo[update_elo.driver_x != update_elo.driver_y]
        update_elo = update_elo.merge(elo_ratings_table, how='left', left_on='driver_x', right_on='driver')
        update_elo = update_elo.merge(elo_ratings_table, how='left', left_on='driver_y', right_on='driver')
        update_elo = update_elo.fillna(1500)
        update_elo['xWin'] = np.where(update_elo['fin_x']<update_elo['fin_y'], 1,0)
        update_elo['xExpectedWin'] = (1/(1+10**((update_elo['EloRating_y']-update_elo['EloRating_x'])/400)))
        update_elo.columns = ['season_x', 'race_x', 'driver_x', 'fin_x', 'key', 'season_y', 'race_y', 'driver_y', 'fin_y',
                              'driver_x_2', 'EloRating.x', 'driver_y_2', 'EloRating_y', 'xWin', 'xExpectedWin']
        update_elo = update_elo.groupby('driver_x').agg(oldRating = ('EloRating.x', 'mean'),
                                                        actualScore=('xWin', 'sum'),
                                                        expectedScore=('xExpectedWin', 'sum'))
        update_elo['EloRating'] = update_elo['oldRating']+2.5*(update_elo['actualScore']-update_elo['expectedScore'])
        update_elo = update_elo.reset_index(level=['driver_x'])
        update_elo.columns = ['driver', 'oldRating', 'actualScore', 'expectedScore', 'EloRating']
        update_elo = update_elo[['driver', 'EloRating']]
        # update elo ratings of drivers who were in that race
        elo_ratings_table = elo_ratings_table.merge(update_elo, how='left', on='driver')
        elo_ratings_table['EloRating_x'] = [x if pd.isna(y) else y for x,y in zip(elo_ratings_table['EloRating_x'], elo_ratings_table['EloRating_y'])]
        elo_ratings_table.columns = ['driver', 'EloRating', 'EloRatingX']
        elo_ratings_table = elo_ratings_table[['driver', 'EloRating']]

    print(datetime.now() - startTime)

    season_results = race_results[race_results.fin != 0]
    season_results = season_results.merge(points_table, on='fin', how='left')
    season_results['points'] = [x*2 if y == 6 else x for x,y in zip(season_results['points'], season_results['race'])]
    season_results = season_results.groupby('driver').agg({'points': 'sum'})
    season_results = season_results.merge(drivers_and_races, on='driver', how='left')
    season_results['totalPoints'] = season_results['points_x']+season_results['points_y']
    season_results['chamPos'] = season_results['totalPoints'].rank(method='first', ascending=False)
    season_results['season'] = season
    season_results = season_results[['driver', 'totalPoints', 'chamPos', 'season']]
    #print(season_results)
    final_results = final_results.append(season_results)
    final_results = final_results[final_results['driver'] != "test"]
    live_results = final_results[final_results.chamPos == 1]
    live_results = live_results.groupby(['driver'])['chamPos'].sum().rename("count")
    live_results = live_results / live_results.groupby(level=0).sum()
    print(live_results)


final_results.to_csv("4_19_2021_champ.csv")

