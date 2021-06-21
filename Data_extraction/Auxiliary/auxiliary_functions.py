'''
This file contains the auxiliary functions that are
shared between the cleaning scripts and thus, aren't
specific to only one of them.
'''
#--------------------------------------------------
import pandas as pd
import numpy as np
import datetime
from datetime import date
from dateutil.relativedelta import relativedelta
import os
#--------------------------------------------------
def write_csvs(list_dfs,path,list_names):
    '''
    Auxiliary function that writes the csvs that recives as input in the provided
    path. The names of the files are also passed in the input.
    '''
    for i,df in enumerate(list_dfs):
        # Checking if the df has at least one observation.
        if df.shape[0] != 0:
            df.to_csv(path+list_names[i]+'.csv', index = False)
#--------------------------------------------------
def delete_csvs(list_csvs,path):
    '''
    Auxiliary function that deletes temporal .csv files.
    '''
    # Making sure that the file that we want to delete exists.
    for csv_file in list_csvs:
        name = path+csv_file+".csv"
        if os.path.exists(name):
            os.remove(name)
#--------------------------------------------------
def string_to_date(string):
    '''
    Auxiliary function that given a YYYY-MM-DDTHH:mm:ssZ formated string
    returns a datetime.date object.
    '''
    sep = string.split('T')[0]
    da = datetime.datetime.strptime(sep, '%Y-%m-%d').date()
    return da
#--------------------------------------------------
def difference_in_days(x,y,mode):
    '''
    Auxiliary function that given two YYYY-MM-DDTHH:mm:ssZ formated
    strings, computes the difference in days between them.
    '''
    # Checking if both provided string contain info.
    if(not pd.isnull(x) and not pd.isnull(y)):
        if mode == 2 and (x == "NM" or y == "NM"):
            return "NM"
        else:
            # Converting the strings to date using the auxiliary function.
            x,y = string_to_date(x), string_to_date(y)
            difference_in_days= (y-x).days
            return int(difference_in_days)
    return np.nan
#--------------------------------------------------
