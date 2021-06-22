import pandas as pd
import numpy as np
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
def read_and_extract_target():
    '''
    This function reads the processed "events" df and computes which
    of the observations correspond to an IC phenomena. After that computation,
    only relevant columns are kept.
    '''
    events_label = pd.read_csv("./Temp/events_CS2.csv")
    # Deleting the previous temporary files
    aux_fun.delete_csvs(["events_CS2"],"./Temp/")
    events_label["target"] = 0
    for i,row in events_label.iterrows():
        if row.tipus_event in ["Urgències per Insuficiència Cardíaca", "Ingrés per Insuficiència Cardíaca"]:
            events_label.at[i,"target"] = 1
        elif row.tipus_event == "Exitus" and row.causa_exitus == "Cardiovascular" and row.causa_exitus_cv=="Insuficiència cardíaca":
            events_label.at[i,"target"] = 1
        elif events_label.loc[i,"tipus_event"] in ["Ingrés per altra causa cardiològica"]:
            events_label.at[i,"target"] = 2

    events_label.drop(columns=['fecha_exitus_event', 'causa_exitus', 'causa_exitus_cv', 'origen_ingres_ic', 'tipus_event'], inplace= True)
    return events_label
#--------------------------------------------------
def execute_script():
    events_label = read_and_extract_target()
    # Change this value to modify the file name.
    names = ["events_label_CS4"]
    # Change this variable to modify the saving path.
    saving_path = './Temp/'
    aux_fun.write_csvs([events_label],saving_path,names)
#--------------------------------------------------
