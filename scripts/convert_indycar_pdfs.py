import tabula
import pandas as pd
import requests

#read in IndyCar lapchart
df = tabula.read_pdf("http://www.imscdn.com/INDYCAR/Documents/5800/2021-08-08/indycar-race-lapchart.pdf", pages="all", multiple_tables = True, lattice=True)

#find total number of totals on the PDF document
num_tables = len(df)

#find which tables are the important ones with real data
indicators = []
for i in range(0, num_tables):
	#the table after this one has the real data
	if df[i].columns[0] == "Event:":
		indicators.append(df[i+1])

#make one large lapchart
df2 = pd.concat(indicators, axis=1)

#read in racing reference data
url = 'https://www.racing-reference.info/race-results/2021_Big_Machine_Music_City_Grand_Prix/O/'
html = requests.get(url).content
df_list = pd.read_html(html)
#this could change
df_want = df_list[4]

#append on starting position
lap_zero = df_want[['St', '#']]
lap_zero = lap_zero.sort_values(by=['St'])[['#']].rename(columns={"#": "0"}).reset_index(drop=True)
df3 = pd.concat([lap_zero, df2], axis=1)

#check if the number of drivers in each dataset match - if not, throw error
if len(df2) == len(df_want):
	print("Data sets match")
else:
	print("Data sets do not match")

#select just driver names and numbers
driver_input_data = df_want[['Driver', "#"]]
