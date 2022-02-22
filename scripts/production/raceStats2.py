from pandas import DataFrame
import pandas as pd
import numpy as np
import os
import tabula
import requests
#from tabula import read_pdf

#intro to script
print("Function raceStats(race_length=integer, lapchart_pdf=stringURL, racing_reference_url=stringURL, points_system='single' OR 'double')")

##################################################################################################
#Calculates all of the race stats for a given race
def raceStats(race_length, lapchart_pdf, racing_reference_url, point_system="single", ):
	#read in IndyCar lapchart
	df = tabula.read_pdf(lapchart_pdf, pages="all", multiple_tables = True, lattice=True)

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
	url = racing_reference_url
	html = requests.get(url).content
	df_list = pd.read_html(html)
	#this could change
	df_want = df_list[4]

	#append on starting position
	lap_zero = df_want[['St', '#']]
	lap_zero = lap_zero.sort_values(by=['St'])[['#']].rename(columns={"#": "0"}).reset_index(drop=True)
	df3 = pd.concat([lap_zero, df2], axis=1)

	#return df3

	#check if the number of drivers in each dataset match - if not, throw error
	if len(df3) != len(df_want):
		return "Data sets do not match. There are a different number of drivers."

	#select just driver names and numbers
	driver_input_data = df_want[['Driver', "#"]]

	#lapchart data
	data = df3

	#check number of laps in data is correct
	if race_length != len(data.columns)-1:
		return "Length of race is not equal to lapchart size."

	#bring in correct points table
	if point_system=="single":
		points = pd.read_csv('https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/points_table.csv')
	elif point_system=="double":
		points = pd.read_csv('https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/double_points_table.csv')
	else:
		return "Invalid points system. Please enter single or double in quotes."
		
	race_length = race_length

	drivers = racePosition(0,race_length+1, driver_input_data, data)
	##################################################################################################
	#ATP
	df = pd.DataFrame({"driver":[], "atp":[]})

	for key in drivers:
	    if len(drivers[key]) != 0:
	        df2 = pd.DataFrame({"driver":[key], "atp":[sum(drivers[key])/len(drivers[key])]})
	        df = df.append(df2, ignore_index=True)

	##################################################################################################
	#Did they lead a lap? The most laps? Get pole position?
	laps_led = pd.DataFrame({"driver":[], "ledLaps":[]})

	for key in drivers:
		if len(drivers[key]) != 0:
	 		led_laps = 0
	 		for i in range(0,len(drivers[key])):
	 			if drivers[key][i] == 1:
	 				led_laps += 1
	 		df2=pd.DataFrame({"driver":[key], "ledLaps":[led_laps]})
	 		laps_led = laps_led.append(df2, ignore_index=True)

	#Check who got pole position
	pole = 0
	for key in drivers:
		if len(drivers[key]) !=0:
			if drivers[key][0] == 1:
				pole = key

	laps_led['ledLapPoints'] = np.where(laps_led['ledLaps']>0, 1, 0)
	most_laps = laps_led['ledLaps'].max()
	laps_led['ledMostPoints'] = np.where(laps_led['ledLaps']==most_laps,2,0)
	laps_led['polePoints'] = np.where(laps_led['driver']==pole,1,0)

	##################################################################################################
	#Number of passes measured at the end of each lap
	passes_df = pd.DataFrame({"driver":[], "passesFor":[], "passesAgainst":[], "totalPasses":[]})

	for key in drivers:
	    if len(drivers[key]) != 0:
	        changes = []
	        for i in range(0, len(drivers[key])-1):
	            change=-(drivers[key][i+1]-drivers[key][i])
	            changes.append(change)

	    goodpass = []
	    badpass = []
	    for i in changes:
	        if i > 0:
	            goodpass.append(i)
	        else:
	            badpass.append(i)
	    
	    if len(drivers[key]) != 0:
	        df2 = pd.DataFrame({"driver":[key], "passesFor":[sum(goodpass)], "passesAgainst":[-1*sum(badpass)], "totalPasses":[sum(goodpass)+sum(map(abs, badpass))]})
	        passes_df = passes_df.append(df2, ignore_index=True)
	    
	##################################################################################################
	#In Top Five Laps
	in_top_five_df = pd.DataFrame({"driver":[], "lapsT5":[]})

	for key in drivers:
	    inT5 = 0
	    first_lap = True
	    if len(drivers[key]) !=0:
	        for lap in drivers[key]:
	        	if first_lap == True:
	        		first_lap = False
	        		inT5 += 0
	        	else:
	        		if lap < 6:
	        			inT5 += 1
	        		else:
	        			inT5 += 0
	    
	    df2 = pd.DataFrame({"driver":[key], "lapsT5":[inT5]})
	    in_top_five_df = in_top_five_df.append(df2, ignore_index=True)

	##################################################################################################
	#Race start change from Lap 0 to Lap 2
	start_df = pd.DataFrame({"driver":[], "lapOneChange":[]})

	for key in drivers:
	    if len(drivers[key]) >=2:
	        df2 = pd.DataFrame({"driver":[key], "lapOneChange":[-1*(drivers[key][2]-drivers[key][0])]})
	        start_df = start_df.append(df2, ignore_index=True)

	##################################################################################################
	#ATP25
	drivers = racePosition(round(.75*race_length),race_length+1, driver_input_data, data)

	atp_percentile = pd.DataFrame({"driver":[], "atp25":[]})

	count = 0
	for key in drivers:
	    if len(drivers[key]) != 0:
	        df2 = pd.DataFrame({"driver":[key], "atp25":[sum(drivers[key])/len(drivers[key])]})
	        atp_percentile = atp_percentile.append(df2, ignore_index=True)
	    count += 1

	##################################################################################################
	#Merge everything together
	df = df.merge(atp_percentile, how="left", left_on="driver", right_on="driver")
	df = df.merge(passes_df, how="left", left_on="driver", right_on="driver")
	df = df.merge(start_df, how="left", left_on="driver", right_on="driver")
	df = df.merge(in_top_five_df, how="left", left_on="driver", right_on="driver")
	df = df.merge(laps_led, how="left", left_on="driver", right_on="driver")

	##################################################################################################
	#Calculating xPoints

	df['roundedATP'] = (round(df['atp']))
	df['roundedATP25'] = round(df['atp25'])
	df = df.merge(points, how="left", left_on="roundedATP", right_on="fin")
	df = df.merge(points, how="left", left_on="roundedATP25", right_on="fin")
	df['xPoints'] = df['ledLapPoints']+df['ledMostPoints']+df['polePoints']+1*df['points_x']

	##################################################################################################
	#Final output
	final_stats = df[['driver', 'ledLapPoints', 'ledMostPoints','polePoints','atp', 'atp25', 'points_x','points_y','xPoints','passesFor','passesAgainst','lapOneChange','lapsT5']]
	
	#merge in racing reference data with stats, remove duplicate columns
	final = df_want.merge(final_stats, how="left", left_on="Driver", right_on="driver")
	del final['driver']
	final = final.sort_values(by=['Pos'])

	final.to_csv("C:/Users/drewb/Desktop/advanced_race_stats.csv")
	return final



#################################################################################################
#Helper function - finds every drivers' position on every lap in the given interval with given driver names and data set
def racePosition(start=0, finish=1, driver_input_data="test", data="test"):
	#I am not sure if all of the below is needed anymore
	names = []

	for i in range(0,len(driver_input_data)):
		names.append(driver_input_data.iloc[i,0])
	
	driver_dict = {k:[] for k in names}

	#Driver numbers in order they are listed above
	nums = []
	for i in range(0,len(driver_input_data)):
		nums.append(int(driver_input_data.iloc[i,1]))

	#List of all driver lists in order of numbers above
	
	def getList(dict): 
		list = [] 
		for key in dict.keys(): 
			list.append(key)
		return list
      
	drivers = getList(driver_dict)

	#Driver names in order - not sure if this is still needed
	names = []
	for i in range(0,len(driver_input_data)):
		names.append(driver_input_data.iloc[i,0])

	#Range of 0 (start) to (last lap+1) of race
	for lap in range(start,finish):
	    #Take column of current lap into a list
	    place_list = list(data[str(lap)])
	    
	    count = 0
	    #For each number in nums
	    for num in nums:
	        #If the number was on the track for that lap
	        if num in place_list:
	            #Find it's index in the list, and append it to the correct driver
	            driver = drivers[count]
	            place = place_list.index(num)
	            driver_dict[driver].append(place+1)
	        else:
	            pass
	        count += 1

	return driver_dict 