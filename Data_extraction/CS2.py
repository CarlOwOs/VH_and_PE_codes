#--------------------------------------------------
import pandas as pd
import numpy as np
import datetime
from datetime import date
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
def read_and_delete():
    '''
    Function that reads the processed dataframes and deltes the
    columns that don't provide any info.
    '''
    visita_basal = pd.read_csv("./Temp/OR_vb.csv")
    visita_seguiment = pd.read_csv("./Temp/OR_vs.csv")
    events = pd.read_csv("./Temp/OR_events.csv")
    # Deleting the previous temporary files
    del_csvs = ["OR_vb","OR_vs","OR_events"]
    aux_fun.delete_csvs(del_csvs,"./Temp/")
    # Deleting the columns
    visita_basal = visita_basal.dropna(how='all', axis=1)
    visita_seguiment = visita_seguiment.dropna(how='all', axis=1)
    events = events.dropna(how='all', axis=1)
    return visita_basal,visita_seguiment,events
#--------------------------------------------------
def extract_season(string):
    '''
    Auxiliary function that given a YYYY-MM-DDTHH:mm:ssZ formated string
    returns the season of the date
    '''
    # Checking if the provided date contains a value.
    if not pd.isnull(string):
        val = aux_fun.string_to_date(string)

        Y = 2000 # Dummy leap year to allow input X-02-29 (leap day)
        seasons = [('hivern', (date(Y,  1,  1),  date(Y,  3, 20))),
                  ('primavera', (date(Y,  3, 21),  date(Y,  6, 20))),
                  ('estiu', (date(Y,  6, 21),  date(Y,  9, 22))),
                  ('tardor', (date(Y,  9, 23),  date(Y, 12, 20))),
                  ('hivern', (date(Y, 12, 21),  date(Y, 12, 31)))]

        val = val.replace(year=Y)
        return next(season for season, (start, end) in seasons
                    if start <= val <= end)
    return np.nan
#--------------------------------------------------
def season_visit(df):
    '''
    Function that given a dataframe, computes the season for each visit contained
    on the df.
    '''
    df["estacio_visita"] = np.nan
    df.loc[:,"estacio_visita"] = df.apply(lambda row: extract_season(row.data_visita), axis= 1)
#--------------------------------------------------
def visit_duration(df):
    '''
    Function that given a dataframe, computes the duration of the visit
    for each observation contained on the df. The function removes those observations
    with negative duration.
    '''
    df["estada"] = np.nan
    df.loc[:,"estada"] = df.apply(lambda row: aux_fun.difference_in_days(row.data_visita,row.alta_event,1), axis= 1)
    df = df[df['estada'] >= 0]
    df.reset_index(drop=True, inplace=True)
    return df
#--------------------------------------------------
def days_since_first_visit(df,base):
    '''
    Auxiliary function that given the df interested and a  base df for comparison,
    computes the number of days that have passed since the first appearence of a
    patient in the base df.
    '''
    df["days_since_start"] = np.nan
    for person in df['nhc'].unique():
        day_ini = base.loc[base['nhc'] == person,'data_visita'].values[0]
        df.loc[df["nhc"] == person,"days_since_start"] = df.loc[df["nhc"] == person].apply(lambda row : aux_fun.difference_in_days(day_ini,row.data_visita,1), axis = 1)
#--------------------------------------------------
def string_to_year(string):
    '''
    Auxiliary function that given a YYYY-MM-DDTHH:mm:ssZ formated
    string, returns it years.
    '''
    if not pd.isnull(string):
        sep = string.split('T')[0]
        da = datetime.datetime.strptime(sep, '%Y-%m-%d').date()
        return da.year
    return np.nan
#--------------------------------------------------
def old_ecos(df,th):
    '''
    Function that imputes as NA all the ecos performed before
    the given threshold.
    '''
    old_data_aux = df.copy()
    old_data_aux["year"] = np.nan
    old_data_aux.loc[:,"year"] = old_data_aux.apply(lambda row: string_to_year(row.eco_basal), axis= 1)
    # Obtaining the nhcs from the patients that have an eco before the th.
    old_nhc = list(old_data_aux.loc[old_data_aux["year"] <= th, "nhc"])
    df.loc[df.nhc.isin(old_nhc),"eco_basal"] = np.nan
#--------------------------------------------------
def old_visits(df_vb,df_vs,th):
    '''
    Function that deletes the observation which initial visit was before the
    given threshold and don't have any "visita_seguiment".
    '''
    old_data_aux = df_vb.copy()
    old_data_aux["year"] = np.nan
    old_data_aux.loc[:,"year"] = old_data_aux.apply(lambda row: string_to_year(row.data_visita), axis= 1)
    # Obtaining the nhcs from the patients that fulfill the conditions.
    old_nhc = list(old_data_aux.loc[old_data_aux["year"] <= th, "nhc"])
    old_nhc_def = []
    for person in old_nhc:
        aux = df_vs.loc[df_vs['nhc'] == person, :]
        if aux.shape[0] == 0:
            old_nhc_def.append(person)
    # Deleting all the observations whose nhc appears in old_nhc_def.
    df_vb.drop(df_vb[df_vb.nhc.isin(old_nhc_def)].index, inplace = True)
    df_vb.reset_index(drop=True, inplace=True)
    return df_vb
#--------------------------------------------------
def visita_basal_outliers(df_vb,df_vs,th_v,th_e):
    '''
    Function that deletes outliers from the visita_basal df.
    '''
    df_vb = old_visits(df_vb,df_vs,th_v)
    old_ecos(df_vb,th_e)
    # Deleting one observation that has 21 UCI visits but any following visit.
    df_vb.drop(df_vb[df_vb.ucies_basal == 21].index, inplace=True)
    df_vb.reset_index(drop=True, inplace=True)
    # Modifying a manuscript error in the height variable.
    df_vb.at[df_vb["talla"] == 716,"talla"] = 176
    return df_vb
#--------------------------------------------------
def fixing_weight_height(df,type):
    '''
    Function that modifies wrong manually-introduced values. These errors fall
    in the following categories:

    · Values introduced in meters instead of centimeters.
    · Values that miss the 1 at the beginning.
    '''
    # VISITA BASAL
    if type == 1:
        df.loc[df["talla"] < 10,"talla"] = df.loc[df["talla"] < 10].apply(lambda row: row.talla *100,axis =1 )
        df.loc[df["talla"] < 100,"talla"] = df.loc[df["talla"] < 100].apply(lambda row: row.talla +100,axis =1 )
    # VISITA SEGUIMENT
    if type == 2:
        df.loc[df["talla_seguim"] < 10,"talla_seguim"] = df.loc[df["talla_seguim"] < 10].apply(lambda row: row.talla_seguim *100,axis =1 )
        df.loc[df["talla_seguim"] < 100,"talla_seguim"] = df.loc[df["talla_seguim"] < 100].apply(lambda row: row.talla_seguim +100,axis =1 )
#--------------------------------------------------
def compute_imc(weight,height):
    '''
    Auxiliary function that given weight (in kg) and height (in cm)
    compute the IMC using the basic formula.
    '''
    # Checking if both provided variables contain info.
    if not pd.isnull(weight) and not pd.isnull(height):
        # Converting the height from cm to m.
        height = height/100
        return weight/(height**2)
    return np.nan
#--------------------------------------------------
def add_imc(df,weight,height):
    '''
    Function that given a dataframe and the weight and height variable
    names, computes the IMC and adds it into a new column.
    '''
    df["IMC"] = np.nan
    df.loc[:,"IMC"] = df.apply(lambda row: compute_imc(row[weight],row[height]), axis= 1)
#--------------------------------------------------
def sort_df(df):
    '''
    Function that sorts the provided df taking into account the patient
    nhc and the visit date.
    '''
    df.sort_values(by=['nhc','data_visita'], inplace= True)
    df.reset_index(drop=True, inplace=True)
#--------------------------------------------------
def renumber_visit(df):
    '''
    Function that enumerates the visits of a patient taking intro
    account the date they were produced.
    '''
    patient_id = 'aux'
    for i, row in df.iterrows():
        aux = row.nhc
        if aux != patient_id: # Primera visita de seguimiento del paciente.
            value = 1
        if int(row.redcap_repeat_instance) !=  value: # Esta mal puesto el valor de la visita.
            df.at[i,"redcap_repeat_instance"] = value
        value += 1
        patient_id = aux
#--------------------------------------------------
def retouching_first_visit(df):
    '''
    Function that performs small modifications to the visit_basal dataframe
    to be consistent with the rest of the df.
    '''
    # Filling the NAs.
    df.loc[pd.isnull(df["procedencia"]) == True, "procedencia"] = "Altres"
    # Modifying the heart-conduction disorders labels as their are wrongly written.
    df.loc[df["trast_cond_basal"] == "BRDFH","trast_cond_basal"] = "BBDFH"
    df.loc[df["trast_cond_basal"] == "BREFH","trast_cond_basal"] = "BBEFH"
    cols = ["auric_esque_basal","creat_basal","mg_beta","mg_antial","albuorin_basal"]
    # Replacing comma values with points.
    for col in cols:
        df.loc[:,col]= df.loc[:,col].apply(lambda x: str(x).replace(",", "."))
#--------------------------------------------------
def introduce_values(df,col_name,val):
    '''
    Auxiliary functions that proving a df and a column, imputes
    the providing value.
    '''
    df.loc[pd.isnull(df[col_name]),col_name] = val
#--------------------------------------------------
def impute_values(df_vb,df_vs,df_ev):
    '''
    Function that imputes NA values in the actual dataframes.
    '''
    introduce_values(df_vb,"neoplasia_estat","Cap")
    introduce_values(df_vb,"neoplasia_qt","Cap")
    introduce_values(df_vb,"tipus_iqprevia","Cap")
    introduce_values(df_vb,"trast_cond_basal","Cap")
    introduce_values(df_vs,"trastorn_conduccio_t_v_1","Cap")
    introduce_values(df_ev,"causa_exitus","No cardiovascular")
    introduce_values(df_vs,"canvi_ttm_seguim","No")
    introduce_values(df_vb,"estim_marcap_basal","No")
    introduce_values(df_vb, "ingresos_basal",0)
    introduce_values(df_vb, "ucies_basal",0)
    introduce_values(df_vb, "ingresuic_basal",0)
    introduce_values(df_vb, "im_basal",0)
    introduce_values(df_vb, "cf_basal",0)
    introduce_values(df_vb,"im_basal",0)
    introduce_values(df_vs,"insuf_mitral_seguim",0)
    introduce_values(df_vs, "classe_funcional_seguim",0)
    introduce_values(df_vs,"marca","No")
#--------------------------------------------------
def drop_columns(df,column_list):
    '''
    Auxiliary function that given a data frame and a column list, delete those columns.
    '''
    df.drop(columns = column_list, inplace = True)
#--------------------------------------------------
def add_zeros_mg(df_list,col_vars,col_modifys,value2check,value):
    '''
    Function that imputes with 0 the dosis columns taking into account
    the values that determine weather a patient is taking (or not) such
    medication.
    '''
    for i in range(2):
        df_list[i].loc[df_list[i][col_vars[i]] == value2check,col_modifys[i]] = value
#--------------------------------------------------
def imputing_no_medication(df_vb,df_vs):
    '''
    Auxiliar function that contains all the calls of "add_zeros_mg",
    to ease code reading.
    '''
    add_zeros_mg([df_vb,df_vs],["ttm_basal___4","ttm_seguim___4"],["mg_diur","mg_diur_segui"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___5","ttm_seguim___5"],["mg_tiaz","mg_tiaz_seguim"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___6","ttm_seguim___6"],["mg_antial","mg_anti_seguim"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___1","ttm_seguim___1"],["mg_ieca","mg_ieca_seguim"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___2","ttm_seguim___2"],["mg_ara2","mg_ara2_seguim"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___7","ttm_seguim___7"],["mg_beta","mg_beta_seguim"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___22","ttm_seguim___22"],["mg_isglt2","mg_islgt2_seguim"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___19","ttm_seguim___19"],["mg_naco","mg_naco_seguim"],"Unchecked",0)
    add_zeros_mg([df_vb,df_vs],["ttm_basal___3","ttm_seguim___3"],["mg_sac","mg_sacu_seguim"],"Unchecked",0)
#--------------------------------------------------
def execute_script():
    visita_basal,visita_seguiment,events = read_and_delete()
    season_visit(visita_basal)
    season_visit(visita_seguiment)
    season_visit(events)
    events = visit_duration(events)
    days_since_first_visit(visita_seguiment,visita_basal)
    days_since_first_visit(events,visita_basal)
    sort_df(visita_seguiment)
    sort_df(events)
    sort_df(visita_basal)
    renumber_visit(visita_seguiment)
    renumber_visit(events)
    retouching_first_visit(visita_basal)
    impute_values(visita_basal,visita_seguiment,events)
    # Modify these values in order to change the outlier treatment.
    threshold_data_visita = 2018
    threshold_data_eco = 2010
    visita_basal = visita_basal_outliers(visita_basal,visita_seguiment,threshold_data_visita,threshold_data_eco)
    fixing_weight_height(visita_basal,1)
    fixing_weight_height(visita_seguiment,2)
    add_imc(visita_basal,"pes","talla")
    add_imc(visita_seguiment,"pes_seguim", "talla_seguim")
    drop_columns(visita_basal,["ttm_ev_previ","ttm_ev_primera_visita"])
    drop_columns(visita_seguiment,"trastorn_conduccio_seguim")
    imputing_no_medication(visita_basal,visita_seguiment)
    # Change these values to modify the file names.
    names = ['visita_basal_CS2','visita_seguiment_CS2','events_CS2']
    # Change this variable to modify the saving path.
    saving_path = './Temp/'
    dfs = [visita_basal,visita_seguiment,events]
    aux_fun.write_csvs(dfs,saving_path,names)
#--------------------------------------------------
