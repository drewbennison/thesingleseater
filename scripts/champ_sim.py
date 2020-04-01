import pandas as pd
import random
pd.options.mode.chained_assignment = None

points_table = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/points_table.csv")
# The current elo ratings for all drivers
elo_ratings_table = pd.read_csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/4_1_2020_elo_ratings.csv")
final_results = pd.DataFrame(data={'season': [0], 'race': [0], 'champPos': [0]})

for season in range(1, 101):
    print(season)
    race_results = pd.DataFrame(data={'season': [0], 'race': [0], 'driver': ["test"], 'fin': [0]})
    # The drivers that will compete in the season and their current points standings
    # This might be read in from master_results in the future/once the season starts
    # Must be in form driver, points
    season_drivers = pd.read_csv("C:/users/drewb/desktop/elo_Drivers.csv")

    for race in range(1, 18):
        race_drivers_elo = season_drivers.merge(elo_ratings_table, on='driver', how='left')
        drivers_who_won = []

        for subRace in range(0, len(race_drivers_elo)):
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
            ids = list(range(0, len(race_drivers_elo)))

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
            race_results_temp = pd.DataFrame(data={'season': [season], 'race': [race], 'driver': [winner], 'fin': [subRace]})
            race_results = race_results.append(race_results_temp)
            drivers_who_won.append(winner)
            race_drivers_elo = race_drivers_elo[~race_drivers_elo.driver.isin(drivers_who_won)]

    season_results = race_results[race_results.fin != 0]
    season_results = season_results.merge(points_table, on='fin', how='left')
    season_results = season_results.groupby('driver').agg({'points': 'sum'})
    season_results = season_results.merge(season_drivers, on='driver', how='left')
    season_results['totalPoints'] = season_results['points_x']+season_results['points_y']
    season_results['chamPos'] = season_results['totalPoints'].rank(method='first', ascending=False)
    season_results['season'] = season
    season_results = season_results[['season', 'driver', 'chamPos']]
    final_results = final_results.append(season_results)

print(final_results[final_results.chamPos == 1].groupby('driver').agg({'chamPos': 'sum'}))



