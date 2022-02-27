# -*- coding: utf-8 -*-
"""
Created on Tue Feb 22 23:08:33 2022

@author: Kyle
"""

import requests
import pandas as pd

url = 'https://openwheel.com/drivers-teams/'

url2 = 'https://openwheel.com/drivers-teams/2021-indycar-drivers-teams-indylights/'

url3 = 'https://openwheel.com/drivers-teams/2018-2/'

df_final = pd.DataFrame()

df_want = pd.DataFrame()

def get_table(url, year):
    
    html = requests.get(url).content
    
    df_list = pd.read_html(html)
    
    df_want = pd.DataFrame()
    
    # get the table you want
    df_want = df_list[0]
    
    df_want['year'] = year
        
    return(df_want)
    

df_want = get_table(url, 2022)
df_final = pd.concat([df_final, df_want])

# Get 2019-2021
for i in range(2019, 2022, 1):
    url2 = 'https://openwheel.com/drivers-teams/' + str(i) + '-indycar-drivers-teams-indylights/'
    df_want = get_table(url2, i)
    df_final = pd.concat([df_final, df_want])
    
# Repeat for 2018-2016
for i in range(2016, 2019, 1):
    url2 = 'https://openwheel.com/drivers-teams/' + str(i) + '-2/'
    df_want = get_table(url2, i)
    df_final = pd.concat([df_final, df_want])
    
# TODO - Save File to CSV
# os.chdir('datasets')
# df_final.to_csv('drivers_teams_2016-2022.csv', index=False)
