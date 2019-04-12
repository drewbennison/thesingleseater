#TO DO:
#Sort by race results (probably have to bring in a file)
#Map my names (rhr) to Racing Reference names for easier merging later on

from pandas import DataFrame
import pandas as pd
import numpy as np
import os

print("Function raceStats(lapchart_link='lapcart_location.csv', race_length=integer, points_system='single' OR 'double')")

#Calculates all of the race stats for a given race
def raceStats(lapchart_link, race_length, point_system="single"):
	#Read in lap chart
	data = pd.read_csv(lapchart_link)
	if point_system=="single":
		points = pd.read_csv('https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/points_table.csv')
	elif point_system=="double":
		points = pd.read_csv('https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/double_points_table.csv')
	else:
		return "Invalid points system. Please enter single or double in quotes."
		
	race_length = race_length

	names = ["leist", "kanaan", "veach", "rossi", "rhr", "andretti", "hinchcliffe", "ericsson", "chilton", "dixon", "rosenqvist", "ferrucci", "bourdais", "pigot", "herta", "rahal", "sato", "newgarden", "power", "pagenaud", "castroneves", "harvey", "hanley", "carpjones", "kimballoward", "oward", "karam", "daly", "kaiser", "mann", "king", "alonso", "jonesindy"]

	##################################################################################################
	#Finds every drivers' position on every lap in the given interval
	def racePosition(start=0, finish=race_length+1):

		#Full time drivers
		leist = []
		kanaan = []
		veach = []
		rossi = []
		rhr = []
		andretti = []
		hinchcliffe = []
		ericsson = []
		chilton = []
		dixon = []
		rosenqvist = []
		ferrucci = []
		bourdais = []
		pigot = []
		herta = []
		rahal = []
		sato = []
		newgarden = []
		power = []
		pagenaud = []
		#Part time drivers
		castroneves = [] #5,6
		harvey = [] #1–6, 10, 13, 16, 17
		hanley = [] # 1, 3, 6, 10, 13
		carpjones = [] #alternate road and oval
		kimballoward = [] #not positive yet
		oward = []
		karam = [] #6
		daly = [] #6
		kaiser = [] #2
		mann = [] #6
		king = [] #6
		alonso = [] #6
		jonesindy = [] #indy number only



		#Driver numbers in order they are listed above
		nums = [4.0, 14.0, 26.0, 27.0, 28.0, 98.0, 5.0, 7.0, 59.0, 9.0, 10.0, 19.0, 18.0, 21.0, 88.0, 15.0, 30.0, 2.0, 12.0, 22.0, 3.0, 60.0, 81.0, 20.0, 23.0, 31.0, 24.0, 25.0, 32.0, 39.0, 42.0, 66.0, 64.0]
		#List of all driver lists in order of numbers above
		drivers = [leist, kanaan, veach, rossi, rhr, andretti, hinchcliffe, ericsson, chilton, dixon, rosenqvist, ferrucci, bourdais, pigot, herta, rahal, sato, newgarden, power, pagenaud, castroneves, harvey, hanley, carpjones, kimballoward, oward, karam, daly, kaiser, mann, king, alonso, jonesindy]
		#Driver names in order
		names = ["leist", "kanaan", "veach", "rossi", "rhr", "andretti", "hinchcliffe", "ericsson", "chilton", "dixon", "rosenqvist", "ferrucci", "bourdais", "pigot", "herta", "rahal", "sato", "newgarden", "power", "pagenaud", "castroneves", "harvey", "hanley", "carpjones", "kimballoward", "oward", "karam", "daly", "kaiser", "mann", "king", "alonso", "jonesindy"]

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
		            driver.append(place+1)
		        else:
		            pass
		        count += 1
		return drivers

	drivers = racePosition()
	##################################################################################################
	#ATP
	df = pd.DataFrame({"driver":[], "atp":[]})

	count = 0
	for driver in drivers:
	    if len(driver) != 0:
	        df2 = pd.DataFrame({"driver":[names[count]], "atp":[sum(driver)/len(driver)]})
	        df = df.append(df2, ignore_index=True)
	    count += 1

	##################################################################################################
	#Did they lead a lap? The most laps? Get pole position?
	laps_led = pd.DataFrame({"driver":[], "ledLaps":[]})

	count = 0
	for driver in drivers:
		if len(driver) != 0:
	 		led_laps = 0
	 		for i in range(1,len(driver)):
	 			if driver[i] == 1:
	 				led_laps += 1
	 		df2=pd.DataFrame({"driver":[names[count]], "ledLaps":[led_laps]})
	 		laps_led = laps_led.append(df2, ignore_index=True)
		count+=1

	#Check who got pole position
	count = 0
	pole = 0
	for driver in drivers:
		if len(driver) !=0:
			if driver[0] == 1:
				pole = names[count]
		count += 1


	laps_led['ledLapPoints'] = np.where(laps_led['ledLaps']>0, 1, 0)
	most_laps = laps_led['ledLaps'].max()
	laps_led['ledMostPoints'] = np.where(laps_led['ledLaps']==most_laps,2,0)
	laps_led['polePoints'] = np.where(laps_led['driver']==pole,1,0)

	##################################################################################################
	#Number of passes measured at the end of each lap
	passes_df = pd.DataFrame({"driver":[], "passesFor":[], "passesAgainst":[], "totalPasses":[]})

	count = 0
	for driver in drivers:
	    if len(driver) != 0:
	        changes = []
	        for i in range(0, len(driver)-1):
	            change=-(driver[i+1]-driver[i])
	            changes.append(change)

	    goodpass = []
	    badpass = []
	    for i in changes:
	        if i > 0:
	            goodpass.append(i)
	        else:
	            badpass.append(i)
	    
	    if len(driver) != 0:
	        df2 = pd.DataFrame({"driver":[names[count]], "passesFor":[sum(goodpass)], "passesAgainst":[-1*sum(badpass)], "totalPasses":[sum(goodpass)+sum(map(abs, badpass))]})
	        passes_df = passes_df.append(df2, ignore_index=True)
	    count +=1
	    
	##################################################################################################
	#In Top Five Laps
	in_top_five_df = pd.DataFrame({"driver":[], "lapsT5":[]})

	count = 0
	for driver in drivers:
	    inT5 = 0
	    if len(driver) !=0:
	        for lap in driver:
	            if lap <6:
	                inT5+=1
	    if inT5>0:
	        df2 = pd.DataFrame({"driver":[names[count]], "lapsT5":[inT5-1]})
	        in_top_five_df = in_top_five_df.append(df2, ignore_index=True)
	    count +=1

	##################################################################################################
	#Race start change
	start_df = pd.DataFrame({"driver":[], "lapOneChange":[]})

	count = 0
	for driver in drivers:
	    if len(driver) >=2:
	        df2 = pd.DataFrame({"driver":[names[count]], "lapOneChange":[-1*(driver[2]-driver[0])]})
	        start_df = start_df.append(df2, ignore_index=True)
	    count += 1

	##################################################################################################
	#ATP25
	drivers = racePosition(round(.75*race_length),race_length+1)

	atp_percentile = pd.DataFrame({"driver":[], "atp25":[]})

	count = 0
	for driver in drivers:
	    if len(driver) != 0:
	        df2 = pd.DataFrame({"driver":[names[count]], "atp25":[sum(driver)/len(driver)]})
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
	df['xPoints'] = df['ledLapPoints']+df['ledMostPoints']+df['polePoints']+.75*df['points_x']+.25*df['points_y']

	##################################################################################################
	#Final output
	final = df[['driver', 'ledLapPoints', 'ledMostPoints','polePoints','atp', 'atp25', 'points_x','points_y','xPoints','passesFor','passesAgainst','lapOneChange','lapsT5']]
	return final


def cautionStats(lapchart_link, list_of_cautions):
	#Takes a list as an input for cautions

	names = ["leist", "kanaan", "veach", "rossi", "rhr", "andretti", "hinchcliffe", "ericsson", "chilton", "dixon", "rosenqvist", "ferrucci", "bourdais", "pigot", "herta", "rahal", "sato", "newgarden", "power", "pagenaud", "castroneves", "harvey", "hanley", "carpjones", "kimballoward", "oward", "karam", "daly", "kaiser", "mann", "king", "alonso", "jonesindy"]
	data = pd.read_csv(lapchart_link)
	caution_laps = list_of_cautions

	#Finds every drivers' position on every lap in the given interval
	def racePosition(start=0, finish=1):

		#Full time drivers
		leist = []
		kanaan = []
		veach = []
		rossi = []
		rhr = []
		andretti = []
		hinchcliffe = []
		ericsson = []
		chilton = []
		dixon = []
		rosenqvist = []
		ferrucci = []
		bourdais = []
		pigot = []
		herta = []
		rahal = []
		sato = []
		newgarden = []
		power = []
		pagenaud = []
		#Part time drivers
		castroneves = [] #5,6
		harvey = [] #1–6, 10, 13, 16, 17
		hanley = [] # 1, 3, 6, 10, 13
		carpjones = [] #alternate road and oval
		kimballoward = [] #not positive yet
		oward = []
		karam = [] #6
		daly = [] #6
		kaiser = [] #2
		mann = [] #6
		king = [] #6
		alonso = [] #6
		jonesindy = [] #indy number only



		#Driver numbers in order they are listed above
		nums = [4.0, 14.0, 26.0, 27.0, 28.0, 98.0, 5.0, 7.0, 59.0, 9.0, 10.0, 19.0, 18.0, 21.0, 88.0, 15.0, 30.0, 2.0, 12.0, 22.0, 3.0, 60.0, 81.0, 20.0, 23.0, 31.0, 24.0, 25.0, 32.0, 39.0, 42.0, 66.0, 64.0]
		#List of all driver lists in order of numbers above
		drivers = [leist, kanaan, veach, rossi, rhr, andretti, hinchcliffe, ericsson, chilton, dixon, rosenqvist, ferrucci, bourdais, pigot, herta, rahal, sato, newgarden, power, pagenaud, castroneves, harvey, hanley, carpjones, kimballoward, oward, karam, daly, kaiser, mann, king, alonso, jonesindy]
		#Driver names in order
		names = ["leist", "kanaan", "veach", "rossi", "rhr", "andretti", "hinchcliffe", "ericsson", "chilton", "dixon", "rosenqvist", "ferrucci", "bourdais", "pigot", "herta", "rahal", "sato", "newgarden", "power", "pagenaud", "castroneves", "harvey", "hanley", "carpjones", "kimballoward", "oward", "karam", "daly", "kaiser", "mann", "king", "alonso", "jonesindy"]

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
		            driver.append(place+1)
		        else:
		            pass
		        count += 1
		return drivers

	caution_df = pd.DataFrame({"caution":[], "driver":[], "restartPM":[]})

	caution = 1

	for i in caution_laps:
		drivers = racePosition(i,i+3)
		count = 0
		for driver in drivers:
			if len(driver)>2:
				df2 = pd.DataFrame({"caution":[caution], "driver":[names[count]], "restartPM":[-1*(driver[2]-driver[0])]})
				caution_df = caution_df.append(df2, ignore_index=True)
			count += 1
		caution += 1

	return caution_df