#--------------------------------------------------
import pandas as pd
import numpy as np
import datetime
from datetime import date
from dateutil.relativedelta import relativedelta
import Levenshtein as lev
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
def read_csvs():
    visita_basal = pd.read_csv("./Temp/visita_basal_CS1.csv")
    visita_seguiment = pd.read_csv("./Temp/visita_seguiment_CS1.csv")
    events = pd.read_csv("./Temp/events_CS1.csv")
    # Deleting the previous temporary files
    del_csvs = ["visita_basal_CS1","visita_seguiment_CS1","events_CS1"]
    aux_fun.delete_csvs(del_csvs,"./Temp/")
    return visita_basal,visita_seguiment,events
#--------------------------------------------------
def drop_columns(df,l_name):
    '''
    Auxiliary function that deletes the columns provided as input from
    the desired dataframe.
    '''
    return df.drop(columns = l_name)
#--------------------------------------------------
def reformat_date(val, method):
    '''
    Auxiliary function that modifies the original date format to the
    one obtained after applying a value.toDate() transformation in OR.
    '''
    if not pd.isnull(val) and isinstance(val, str) and len(val.split(" ")) > 1:
        if method == 1:
            date_val = datetime.datetime.strptime(val, '%Y/%m/%d %H:%M:%S.%f')
        else:
            date_val = datetime.datetime.strptime(val, '%Y-%m-%d %H:%M:%S')
        date_val = str(date_val).replace(' ','T')
        return (date_val + 'Z')
    return val
#--------------------------------------------------
def replace_string(val,ini,last):
    '''
    Auxiliary function that given a numeric decimal value, changes the
    integer and decimal separator for a point instead of a comma.
    '''
    if not pd.isnull(val):
        return str(val).replace(ini,last)
    return val
#--------------------------------------------------
def fixing_typos(df,references):
    '''
    Auxiliary function that converts wrong manually introduced
    values to their respective correct form.
    '''
    for i, row in df.iterrows():
        hosp = row.hospital_event
        if not pd.isnull(hosp) and hosp not in references:
            for ref in references:
                # Edit distance
                if lev.distance(ref.lower(),hosp.lower()) < 5:
                    df.at[i,"hospital_event"] = ref
                    break
#--------------------------------------------------
def reorder_columns(df):
    '''
    Auxiliary function that performs a column reordering.
    '''
    cols = df.columns.tolist()
    cols = cols[0:1]+cols[-1:]+cols[1:-1]
    return df[cols]
#--------------------------------------------------
def OR_events(df):
    '''
    Function that performs the cleaning process previously executed in
    Open Refine (OR) using Pandas and Python code for the "events" dataframe.
    '''
    cols_2_del = ['redcap_repeat_instrument','dades_generals_del_p_v_0','events_complete']
    df = drop_columns(df,cols_2_del)
    for col in ["alta_event","fecha_exitus_event","data_event"]:
        idx = df.columns.get_loc(col)
        df[col] = df.apply(lambda row: reformat_date(row[idx],1), axis = 1)
    df["redcap_repeat_instance"] = df.apply(lambda row: replace_string(row.redcap_repeat_instance,",","."),axis = 1)
    df = df.rename(columns = {'data_event':'data_visita'})
    df = reorder_columns(df)
    fixing_typos(df,["Parc Sanitari Pere i Virgili","Hospital de Vall d'Hebron","Hospital Dos de Maig"])
    return df
#--------------------------------------------------
def OR_vb(df):
    '''
    Function that performs the cleaning process previously executed in
    Open Refine (OR) using Pandas and Python code for the "visita_basal" dataframe.
    '''
    cols_2_del = ['motiu_altres','redcap_repeat_instance','tto_ev_tipo_primera_otro',
    'tto_ev_tipo_otro','dades_generals_del_p_v_0','redcap_repeat_instrument']
    df = drop_columns(df,cols_2_del)
    df = reorder_columns(df)
    for col in ["im_basal","cf_basal"]:
            idx = df.columns.get_loc(col)
            for i,val in enumerate(["IV","III","II","I"]):
                df[col] = df.apply(lambda row: replace_string(row[idx],val,str(4-i)), axis = 1)
    for col in ["auric_esque_basal","im_basal","temps_desacceleracio","qrs_basal",
    "fc","pad","talla","pes","ferritina_basal","sat_transferr_basal","ha1c_basal",
    "creat_basal","fge_basal","sodi_basal","cloro_basal","tni_basal","probnp_basal",
    "colehdl_basal","coleldl_basal","albu_cr_basal","mg_antial","mg_ieca","dias","ona_e",
    "ona_a","ona_e_prima","potassi_basal","albu_basal","mg_isglt2","relacio_ones_e",
    "albuorin_basal","hb_basal","ca125_basal","temps_evolucio","urat_basal","prot_basal",
    "st2_basal","protorin_basal","mg_sac","mg_tiaz"]:
        idx = df.columns.get_loc(col)
        df[col] = df.apply(lambda row: replace_string(row[idx],",","."), axis = 1)
    date_columns = ["data_visita","eco_basal","fecha_anal","temps_evolucio"]
    date_methods = [2,2,2,1]
    for col,method in zip(date_columns,date_methods):
        idx = df.columns.get_loc(col)
        df[col] = df.apply(lambda row: reformat_date(row[idx],method), axis = 1)
    # Fixing relacio_ones_e
    df["relacio_ones_e"] = df.apply(lambda row: float(row.ona_e)/float(row.ona_e_prima), axis= 1)
    # Modifying wrongly introduced values on 'mg_sac'
    df.at[df.mg_sac == "Entresto 97/103 mg/12h","mg_sac"] = "97/103"
    df["mg_sac"] = df.apply(lambda row: str(row.mg_sac).split(" ")[0]
                            if not pd.isnull(row.mg_sac) else row.mg_sac, axis = 1)
    return df
#--------------------------------------------------
def OR_vs(df):
    '''
    Function that performs the cleaning process previously executed in
    Open Refine (OR) using Pandas and Python code for the "visita_seguiment" dataframe.
    '''
    cols_2_del = ['redcap_repeat_instrument','dades_generals_del_p_v_0', "visites_seguiment_complete"]
    df = drop_columns(df,cols_2_del)
    df = reorder_columns(df)
    for col in ["insuf_mitral_seguim","classe_funcional_seguim"]:
        idx = df.columns.get_loc(col)
        for i,val in enumerate(["IV","III","II","I"]):
            df[col] = df.apply(lambda row: replace_string(row[idx],val,str(4-i)), axis = 1)

    for col in ["redcap_repeat_instance","freq_card_seguim","amplada_qrs_seguim","feve_seguim",
    "dtdve_seguim","tiv_seguim","paret","auricula_seguim","tapse_seguim","feve_seguim",
    "temps_desac_seguim","paps_seguim","pes_seguim","talla_seguim","pas_seguim","pad_seguim",
    "fc_seguim","hb_seguim","ferritina_seguim","sat_transf_seguim","creat_seguim","fge_seguim",
    "urat_seguim","sodi_seguim","potassi_seguim","cloro_seguim","prot_seguim","albu_seguim",
    "ha1c_seguim","cole_seguim","colehdl_segui","coleldl_segui","trigli_seguim","albuorin_seguim",
    "albu_crorin_seguim","probnp_seguim","st2_seguim","tni_seguim","ca125_seguim",
    "prot_creorin_seguim","mg_diur_segui","mg_tiaz_seguim","mg_anti_seguim","mg_ieca_seguim",
    "mg_ara2_seguim","mg_beta_seguim","mg_sacu_seguim","mg_islgt2_seguim","mg_naco_seguim",
    "insuf_mitral_seguim","ona_a_seguim","ona_e_seguim","ona_e_prima_seguim"]:
        idx = df.columns.get_loc(col)
        df[col] = df.apply(lambda row: replace_string(row[idx],",","."), axis = 1)
    date_columns = ["data_visita_seguim","data_eco_seguim","fecha_anal_seguim"]
    for col in date_columns:
        idx = df.columns.get_loc(col)
        df[col] = df.apply(lambda row: reformat_date(row[idx],1), axis = 1)
    df = df.rename(columns = {'data_visita_seguim':'data_visita'})
    # Fixing relacio_seguim
    df["relacio_seguim"] = df.apply(lambda row: float(row.ona_e_seguim)/float(row.ona_e_prima_seguim), axis= 1)

    return df
#--------------------------------------------------
def execute_script():
    visita_basal,visita_seguiment, events = read_csvs()
    events = OR_events(events)
    visita_basal = OR_vb(visita_basal)
    visita_seguiment = OR_vs(visita_seguiment)
    # Change these values to modify the file names.
    names = ['OR_events','OR_vb','OR_vs']
    # Change this variable to modify the saving path.
    saving_path = './Temp/'
    dfs = [events,visita_basal,visita_seguiment]
    aux_fun.write_csvs(dfs,saving_path,names)
#--------------------------------------------------
