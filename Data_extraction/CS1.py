#--------------------------------------------------
import pandas as pd
import numpy as np
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
def read_and_rename():
    '''
    Function that reads the original data from the VH DB and renames the
    columns to the names provided by VH in the auxiliary dictionary.
    '''
    # Obtaining the raw data.
    df = pd.read_csv("./Temp/UIC.csv")
    # Merging two columns that reference the same variable, introducing all the values
    # only in of them.
    for i,row in df.iterrows():
        val = np.nan
        if not pd.isnull(row.columna9):
            val = row.columna9
        elif not pd.isnull(row.columna10):
            val = row.columna10
        df.at[i,"columna10"] = val

    # Droping the useless column.
    df.drop(columns = "columna10", inplace = True)
    # Deleting the previous temporary files
    del_csvs = ["UIC"]
    aux_fun.delete_csvs(del_csvs,"./Temp/")
    # Reading the original column names from an auxiliary file.
    column_names = open("./Auxiliary/column_names.txt", "r")
    rename_columns = column_names.read().split(',')
    # Modifiying the dataframe column names with the ones previously read.
    df.columns = rename_columns
    return df
#--------------------------------------------------
def identify_visit_type(df):
    '''
    In the raw dataframe, each row corresponds to an specific visit, eventhough this is not
    explicitly declared. This function identifies each visit type by consulting which
    columns for each row contain values. The columns with values depending on the visit
    type are the following:

    · Generic patient info: Only filled in the first visita basal. [3,4]
    · Visita basal: [6,194]
    · Visita seguiment: [195,320]
    · Events: [321,329]
    · Ultima visita: [330,448]
    · Titulacio: [449,474]
    · Questionari basal: [475,479]
    · Questionari final: [480,484]
    '''
    for i, row in df.iterrows():
        # VISITA BASAL
        if any(not pd.isnull(x) for x in row.values[6:195].tolist()):
            df.at[i,"redcap_repeat_instrument"]= "visita_basal"
        # VISITA SEGUIMENT
        elif any(not pd.isnull(x) for x in row.values[195:321].tolist()):
            df.at[i,"redcap_repeat_instrument"]= "visita_seguiment"
        # EVENTS
        elif any(not pd.isnull(x) for x in row.values[321:330].tolist()):
            df.at[i,"redcap_repeat_instrument"]= "events"
        # ULTIMA VISITA
        elif any(not pd.isnull(x) for x in row.values[330:449].tolist()):
            df.at[i,"redcap_repeat_instrument"]= "ultima_visita"
        # TITULACIO
        elif any(not pd.isnull(x) for x in row.values[449:475].tolist()):
            df.at[i,"redcap_repeat_instrument"]= "titulacio"
        # QUESTIONAL BASAL
        elif any(not pd.isnull(x) for x in row.values[475:480].tolist()):
            df.at[i,"redcap_repeat_instrument"]= "questionari_basal"
        # QUESTIONAL FINAL
        elif any(not pd.isnull(x) for x in row.values[480:485].tolist()):
            df.at[i,"redcap_repeat_instrument"]= "questionari_final"
#--------------------------------------------------
def duplicate(df,cols):
    '''
    Function that given a data frame and a list of columns from that df, duplicates
    the values present on them for each patient (which is identified using the nhc).
    '''
    nhc=""
    data={}
    for i, row in df.iterrows():
        if nhc != row.nhc:
            nhc = row.nhc
            for col in cols:
                data[col] = row[col]
        else:
            for col in cols:
                df.at[i, col] = data[col]
    return df
#--------------------------------------------------
def relative_age(x,y,mod):
    '''
    Auxiliary function that given two dates in string format, which  can consist
    in YYYY-MM-df HH:mm:ss (mod = 1) or YYYY/MM/df HH:mm:ss.SSS (mod = 2), calculates the difference in years.
    '''
    # If both columns contain values.
    if(not pd.isnull(x) and not pd.isnull(y)):
      if mod == 1:
        y = float(y.split("-")[0])
      else:
        y = float(y.split("/")[0])
      difference_in_years = y - x
      return int(difference_in_years)
    return np.nan
#--------------------------------------------------
def age_visit(df):
    '''
    Function that computes how old were the patients at each visit.
    '''
    df["edat_visita"] = np.nan
    # Visita basal
    df.loc[df["redcap_repeat_instrument"] == "visita_basal","edat_visita"] = df.loc[df["redcap_repeat_instrument"] == "visita_basal"].apply(lambda row : relative_age(row.data_naixem,row.data_visita,1), axis = 1)
    # Visita seguiment
    df.loc[df["redcap_repeat_instrument"] == "visita_seguiment","edat_visita"] = df.loc[df["redcap_repeat_instrument"] == "visita_seguiment"].apply(lambda row : relative_age(row.data_naixem,row.data_visita_seguim,2), axis = 1)
    # Events
    df.loc[df["redcap_repeat_instrument"] == "events","edat_visita"] = df.loc[df["redcap_repeat_instrument"] == "events"].apply(lambda row : relative_age(row.data_naixem,row.data_event,2), axis = 1)
    # Ultima visita
    df.loc[df["redcap_repeat_instrument"] == "ultima_visita","edat_visita"] = df.loc[df["redcap_repeat_instrument"] == "ultima_visita"].apply(lambda row : relative_age(row.data_naixem,row.data_visita_final,2), axis = 1)
    # Titulacio
    df.loc[df["redcap_repeat_instrument"] == "titulacio","edat_visita"] = df.loc[df["redcap_repeat_instrument"] == "titulacio"].apply(lambda row : relative_age(row.data_naixem,row.fecha_inicio,2), axis = 1)
    # Questionari basal
    # As this visit type lacks of a date of completion, a -1 value is imputed.
    df.loc[df["redcap_repeat_instrument"] == "questionari_basal","edat_visita"] = -1
    # Questionari final
    # As this visit type lacks of a date of completion, a -1 value is imputed.
    df.loc[df["redcap_repeat_instrument"] == "questionari_final","edat_visita"] = -1
#--------------------------------------------------
def delete_columns_by_type(df):
    '''
    Auxiliary function that deletes the columns that will not contain any data
    (in the og. dataframe) depending on the visit type.
    '''
    # Checking if at least exists one observation of this visit type.
    if not df.shape[0] == 0:
        visit_type = df.iloc[0,1]
        if visit_type == "visita_basal":
            return df.iloc[:, np.r_[:195,485]]
        elif visit_type == "visita_seguiment":
            return df.iloc[:, np.r_[:6, 195:321,485]]
        elif visit_type == "events":
            return df.iloc[:, np.r_[:6, 321:330,485]]
        elif visit_type == "ultima_visita":
            return df.iloc[:, np.r_[:6, 330:449]]
        elif visit_type == "titulacio":
            return df.iloc[:, np.r_[:6, 449:475,485]]
        elif visit_type == "questionari_basal":
            return df.iloc[:, np.r_[:6, 475:480,485]]
        elif visit_type == "questionari_final":
            return df.iloc[:, np.r_[:6, 480:486]]
    return df
  #-------------------------------------------------
def split_in_dfs(df):
    '''
    Function that splits the original dataframe in multiple sub-dfs,
    and perform and initial column removal process, and returns them on a list.
    '''
    # Splitting in sub-dataframes.
    visita_basal = df[(df['redcap_repeat_instrument']=='visita_basal')]
    visita_seguiment = df[df['redcap_repeat_instrument']=='visita_seguiment']
    events = df[df['redcap_repeat_instrument']=='events']
    ultima_visita = df[(df['redcap_repeat_instrument']=='ultima_visita')]
    titulacio = df[df['redcap_repeat_instrument']=='titulacio']
    ques_basal = df[df['redcap_repeat_instrument']=='questionari_basal']
    ques_final = df[df['redcap_repeat_instrument']=='questionari_final']

    #Deleting the columns.
    visita_basal = delete_columns_by_type(visita_basal)
    visita_seguiment = delete_columns_by_type(visita_seguiment)
    events = delete_columns_by_type(events)
    ultima_visita = delete_columns_by_type(ultima_visita)
    titulacio= delete_columns_by_type(titulacio)
    ques_basal = delete_columns_by_type(ques_basal)
    ques_final = delete_columns_by_type(ques_final)

    return [visita_basal, visita_seguiment, events, ultima_visita, titulacio, ques_basal, ques_final]
#--------------------------------------------------
def execute_script():
    df = read_and_rename()
    identify_visit_type(df)
    # Duplicating the general patient info for all visit types.
    generals = ['data_naixem', 'sexe', 'nhc']
    df = duplicate(df,generals)
    age_visit(df)
    dfs = split_in_dfs(df)
    # Change these values to modify the file names.
    names = ['visita_basal_CS1','visita_seguiment_CS1','events_CS1','ultima_visita_CS1','titulacio_CS1','questionari_basal_CS1','questionari_final_CS1']
    # Change this variable to modify the saving path.
    saving_path = './Temp/'
    for i in range(3):
        aux_fun.write_csvs([dfs[i]],saving_path,[names[i]])
#--------------------------------------------------
