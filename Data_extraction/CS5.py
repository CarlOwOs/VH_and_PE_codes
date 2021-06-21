import pandas as pd
import numpy as np
import datetime
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
def read_dfs():
	'''
	Function that reads the dataframes of interest
	'''
	df = pd.read_csv("./Temp/fixed_CS3.csv")
	events_label = pd.read_csv("./Temp/events_label_CS4.csv")
	# Deleting the previous temporary files
	del_csvs = ["fixed_CS3","events_label_CS4"]
	aux_fun.delete_csvs(del_csvs,"./Temp/")
	return df, events_label
#--------------------------------------------------
def events_so_far(df,df2,type_ev_ic):
	'''
	Function that computes the events that a pacient has had so far.
	Depending on the'type_ev_ic' value, all or only the IC events are taken into account.
	'''
	name = "events_so_far"
	if type_ev_ic:
		name += "_IC"
		df[name] = 0
	for i, row in df.iterrows():
		visit_date = row.data_visita
		if type_ev_ic:
			aux = df2[(df2.nhc == row.nhc) & (df2.target == 1)].data_visita.to_list()
		else:
			aux = df2[(df2.nhc == row.nhc)].data_visita.to_list()
		num_events = 0
		stop = False
		j = 0
		while j < len(aux) and not stop:
			event = aux[j]
			diff = aux_fun.difference_in_days(visit_date,event,2)
			if diff <= 0:
				num_events += 1
			else:
				stop = True
			j+= 1
		df.at[i,name] = num_events
#--------------------------------------------------
def difference_in_months(x,y):
	'''
	Auxiliary function that given two YYYY-MM-DDTHH:mm:ssZ formated
	strings, computes the difference in months between them.
	'''
	# Checking if both provided string contain info.
	if(not pd.isnull(x) and not pd.isnull(y)):
		x,y = aux_fun.string_to_date(x), aux_fun.string_to_date(y)
		if y < x:
			num_months = -1
		else:
			num_months = (y.year - x.year) * 12 + (y.month - x.month)
		return int(num_months)
	return np.nan
#--------------------------------------------------
def generate_targets(df,df2,deltas,idx):
	'''
	Function that evaluates for each visit if an IC event
	happened for each of the time intervals present on 'deltaT'
	'''
	deltaT = deltas[idx]
	low_lim = 0
	if deltaT == 2000:
		name = "target_future"
	else:
		name = "target_"+str(deltaT)
	df[name] = 0
	if idx != 0:
		low_lim = deltas[idx-1]
	for i, row in df.iterrows():
		visit_date = row.data_visita
		aux = df2[(df2.nhc == row.nhc) & (df2.target == 1)].data_visita.to_list()
		for j in range(len(aux)):
			visita = aux[j]
			diff = difference_in_months(visit_date,visita)
			if diff >= 0 and diff <= deltaT:
				if deltaT != 1 and diff > low_lim:
					df.at[i,name] = 1
				elif deltaT == 1 and diff >= low_lim:
					df.at[i,name] = 1
#--------------------------------------------------
def extract_info_from_date(date_visit,var):
	'''
	Auxiliary function that given a YYYY-MM-DDTHH:mm:ssZ formated string, extracts different time variables from it.
	'''
	if not pd.isnull(date_visit):
		date_visit = aux_fun.string_to_date(date_visit)
		if var == "year":
			return date_visit.year
		elif var == "month":
			return date_visit.month
		elif var == "week":
		  	return date_visit.isocalendar()[1]
		elif var == "day":
		  	return date_visit.isocalendar()[2]
#--------------------------------------------------
def increase_visit_date_info(df):
	'''
	Function that creates and fills new columns for the provided dataframe
	related to the visit temporal information.
	'''
	df["data_visita_year"] = np.nan
	df.data_visita_year = df.apply(lambda row : extract_info_from_date(row.data_visita,"year"), axis = 1)
	df["data_visita_month"] = np.nan
	df.data_visita_month = df.apply(lambda row : extract_info_from_date(row.data_visita,"month"), axis = 1)
	df["data_visita_week"] = np.nan
	df.data_visita_week = df.apply(lambda row : extract_info_from_date(row.data_visita,"week"), axis = 1)
	df["data_visita_day"] = np.nan
	df.data_visita_day = df.apply(lambda row : extract_info_from_date(row.data_visita,"day"), axis = 1)
#--------------------------------------------------
def days_between_visit_and_test(df,diagnostic):
	'''
	Function that computes the numer of days that have passed between the test
	(analytic/IC diagnostic) and the visit date.
	'''
	# DAYS SINCE DIAGNOSTIC
	if diagnostic:
		# Converting some wrongly written values on the 'temps_evolucio' column.
		# They don't have the appropiate date format.
		for i,row in df.iterrows():
			if not pd.isnull(row.temps_evolucio) and len(row.temps_evolucio.split("-")) < 3:
				df.at[i,"temps_evolucio"] = np.nan

		df["days_since_diagnostic"] = np.nan
		df.days_since_diagnostic = df.apply(lambda row: aux_fun.difference_in_days(row.temps_evolucio,row.data_visita,2), axis = 1)
	# ANALYTIC
	else:
		df["diff_anal_seguim"] = np.nan
		df.diff_anal_seguim = df.apply(lambda row :aux_fun.difference_in_days(row.fecha_anal_seguim,row.data_visita,2),axis = 1)
#--------------------------------------------------
def reorder_columns(df,var):
	'''
	Auxiliary function that reorders the values of the df taking 'var' as
	reference. The objective of this funcion is ease the reading of the .csv
	file, as column order hasn't effect on pandas dfs.
	'''
	aux_idx = df.columns.get_loc(var)
	columns = df.columns.to_list()
	if var == "data_visita":
		columns = columns[:aux_idx] + columns[-6:-2] + columns[aux_idx:-6] + columns[-2:]
	elif var == "fecha_anal_seguim":
		columns = columns[:aux_idx] + columns[-2:-1] + columns[aux_idx:-2] + columns[-1:]
	elif var == "temps_evolucio":
		columns = columns[:aux_idx] + columns[-1:] + columns[aux_idx:-1]
	return(df[columns])
#--------------------------------------------------
def execute_script():
	df,events_label = read_dfs()
	events_so_far(df,events_label,False)
	events_so_far(df,events_label,True)
	# Change these values to modify the IC prediction intervals.
	# The values must be in months.
	deltas = [1,3,6,12]
	for i in range(len(deltas)):
		generate_targets(df,events_label,deltas,i)
	increase_visit_date_info(df)
	days_between_visit_and_test(df,False)
	days_between_visit_and_test(df,True)
	df = reorder_columns(df,"data_visita")
	df = reorder_columns(df,"fecha_anal_seguim")
	df = reorder_columns(df,"temps_evolucio")
	# Lastly, some useless columns are deleted
	columns_to_delete = ["data_naixem","data_visita","data_eco_seguim","fecha_anal_seguim", "temps_evolucio"]
	df.drop(columns = columns_to_delete, inplace = True)
	# Change this value to modify the file name.
	names = ["continuous_CS5"]
	# Change this variable to modify the saving path.
	saving_path = './Temp/'
	aux_fun.write_csvs([df],saving_path,names)
#--------------------------------------------------
