#--------------------------------------------------
import pandas as pd
import numpy as np
import datetime
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
def read_and_clean():
    visita_basal = pd.read_csv("./Temp/visita_basal_CS2.csv")
    visita_seguiment = pd.read_csv("./Temp/visita_seguiment_CS2.csv")
    events = pd.read_csv("./Temp/events_CS2.csv")
    # Deleting the previous temporary files
    del_csvs = ["visita_basal_CS2","visita_seguiment_CS2"]
    aux_fun.delete_csvs(del_csvs,"./Temp/")
    # Deleting unecessary columns.
    visita_basal = visita_basal.drop(columns= ["dias","visita_basal_complete"])
    # Filling values directly.
    visita_basal["redcap_repeat_instance"] = 0
    # Reordering the columns.
    cols = visita_basal.columns.tolist()
    cols = cols[:2] + cols[-1:]+ cols[2:-1]
    visita_basal = visita_basal[cols]
    visita_basal["days_since_start"] = 0
    visita_seguiment["procedencia"] = "UIC"
    return visita_basal,visita_seguiment,events
#--------------------------------------------------
def generate_column_names(name,up_lim):
    '''
    Auxiliary function that given a name and its range
    generates a list containg all the name+idx strings, formated as in the VH
    data frames.
    '''
    aux = []
    for i in range(1,up_lim):
        aux.append(name+"___"+str(i))
    return aux
#--------------------------------------------------
def replicate_columns(df,df2,col_name):
    '''
    Auxiliary function that given two dataframes, replicates
    the values of the desired column in the other dataframe.
    '''
    for person in df2['nhc'].unique():
        val = df.loc[df['nhc'] == person,col_name].values[0]
        df2.loc[df2['nhc'] == person,col_name] = val
#--------------------------------------------------
def duplicating_info(df,df2):
    '''
    Function that given two dataframes, one as a reference and
    one to impute values in, replicates the info of interest
    of such data frame.
    '''
    columns = []
    columns += generate_column_names('motiu_derivacio',9)
    columns += generate_column_names('etiologia',14)
    columns += generate_column_names('antecedents',22)
    aux_col = ["neoplasia_estat","neoplasia_qt","tipus_iqprevia","ingresos_basal",
               "ucies_basal","ingresuic_basal","tto_ev_tipo___1","tto_ev_tipo___4","temps_evolucio"]
    columns += aux_col
    for col in columns:
        replicate_columns(df,df2,col)
#--------------------------------------------------
def replicate_columns_with_events(df,df2,lu,col_name,af_value,lu_values, col_lu):
    '''
    Function that given two dataframes, one as a reference and
    one to impute values in, replicates the info of interest
    of such data frame taking into account a lock up data frame.
    '''
    df[col_name] = np.nan
    for person in df.nhc.unique():
        val = df2.loc[df2["nhc"] == person,col_name].values[0]
        if val == af_value:
            df.loc[df.nhc == person,col_name] = val
        else:
            # Selecting the first date that contain one of the look-up values.
            aux = lu.loc[lu["nhc"] == person,col_lu].values
            stop = False
            event_date = None
            for x in aux:
                if x[0] in lu_values and not stop:
                    event_date = x[1]
                    stop = True
            if event_date is None:
                df.loc[df.nhc == person,col_name] = val
            else:
                event_passed = False
                for i,row in df[df["nhc"] == person].iterrows():
                    if not event_passed and aux_fun.difference_in_days(row.data_visita,event_date,1) > 0:
                        df.at[i,col_name] = val
                    else:
                        event_passed = True
                        df.at[i,col_name] = af_value
#--------------------------------------------------
def fix_endovenous_treatment(df_vb,df_vs,df_ev):
    '''
    Function that treats problems in the given data frame related
    to endevenous treatment variables.
    '''
    aux1 = generate_column_names('tto_ev_tipo',6)
    aux2 = generate_column_names('tto_ev_tipo_primera',6)
    for i,row in df_vb.iterrows():
        for j in range(5):
            tt_prim = row[aux2[j]]
            if tt_prim == 'Checked':
                df_vb.at[i,aux1[j]] = 'Checked'
    df_vb.drop(columns=aux2, inplace=True)
    aux3 = ['tto_ev_tipo___2','tto_ev_tipo___3','tto_ev_tipo___5']
    lu_values = ['Requeriment de Ferro endovenós','Requeriment de tractament inotròpic endovenós', 'Requeriment de descongestió endovenosa a H. Dia']
    for i in range(len(aux3)):
        replicate_columns_with_events(df_vs,df_vb,df_ev,aux3[i],"Checked",[lu_values[i]],["tipus_event","data_visita","nhc"])
#--------------------------------------------------
def fix_following_treatment(df_vb,df_vs):
    '''
    Function that treats problems in the given data frame related
    to following treatment variables.
    '''
    col_aux = generate_column_names('ttm_basal',34)
    col_aux_seg = generate_column_names('ttm_seguim',34)
    patient_id = 'aux'
    values_to_replicate = df_vb.loc[0,col_aux]
    for i, row in df_vs.iterrows():
        aux = row.nhc
        if aux != patient_id: # Patient change.
            values_to_replicate = df_vb.loc[df_vb.nhc == aux, col_aux].values[0].tolist()
            patient_id = aux
        if row.canvi_ttm_seguim == "Si": # Change of the treatment
            values_to_replicate = row[col_aux_seg].values.tolist()
        else: # Values must be replicated.
            for j,val in enumerate(col_aux_seg):
                df_vs.at[i,val] = values_to_replicate[j]

    df_vs.drop(columns="canvi_ttm_seguim",inplace=True)
    return df_vb, df_vs
#--------------------------------------------------
def fix_hr_columns(df):
    '''
    Function that treats problems in the given data frame related
    to heart rate variables.
    '''
    # In the 'visita_seguiment' df, there are two columns that contain the
    # hr value. We priorized the values of the 'freq_cardiaca' columns as
    # stated by VH. If not, the other column is consulted.
    for i,row in df.iterrows():
        val = np.nan
        if not pd.isnull(row.freq_card_seguim):
            val = row.freq_card_seguim
        elif not pd.isnull(row.fc_seguim):
            val = row.fc_seguim
        df.at[i,"fc_seguim"] = val
    # Deleting one of the columns as it isn't needed anymore.
    df.drop(columns="freq_card_seguim",inplace=True)
    return df
#--------------------------------------------------
def unify_dfs(df,df2):
    '''
    Function that performs the union between the two given dataframes.
    To do so, the column names of one of the dataframes is modified to
    match the other.
    '''
    # Renaming the columns
    df.rename(columns={"ritme_basal": "ritme_base_seguim", "pes": "pes_seguim","talla": "talla_seguim", "pas": "pas_seguim","pad": "pad_seguim", "fc": "fc_seguim","cf_basal": "classe_funcional_seguim", "fecha_anal": "fecha_anal_seguim","hb_basal": "hb_seguim", "ferritina_basal": "ferritina_seguim","sat_transferr_basal": "sat_transf_seguim", "ha1c_basal": "ha1c_seguim","creat_basal": "creat_seguim", "fge_basal": "fge_seguim","urat_basal": "urat_seguim", "sodi_basal": "sodi_seguim","eco_basal" : "data_eco_seguim", "feve_basal" : "feve_seguim", "dtdve_basal" : "dtdve_seguim", "tiv_basal" : "tiv_seguim", "pp_basal" : "paret", "auric_esque_basal" : "auricula_seguim", "ona_e" : "ona_e_seguim", "ona_a" : "ona_a_seguim", "temps_desacceleracio" : "temps_desac_seguim", "ona_e_prima" : "ona_e_prima_seguim", "relacio_ones_e" : "relacio_seguim", "im_basal" : "insuf_mitral_seguim", "tapse_basal" : "tapse_seguim", "paps_basal" : "paps_seguim","potassi_basal" : "potassi_seguim", "cloro_basal" : "cloro_seguim", "tni_basal" : "tni_seguim", "probnp_basal" : "probnp_seguim", "cole_basal" : "cole_seguim", "colehdl_basal" : "colehdl_segui", "coleldl_basal" : "coleldl_segui", "trigli_basal" : "trigli_seguim", "prot_basal" : "prot_seguim", "albu_basal" : "albu_seguim", "ca125_basal" : "ca125_seguim", "st2_basal" : "st2_seguim", "albuorin_basal" : "albuorin_seguim", "protorin_basal" : "prot_creorin_seguim", "albu_cr_basal" : "albu_crorin_seguim","mg_antial" : "mg_anti_seguim", "mg_ieca" : "mg_ieca_seguim", "mg_ara2" : "mg_ara2_seguim", "mg_beta" :    "mg_beta_seguim", "mg_sac" : "mg_sacu_seguim", "mg_isglt2" : "mg_islgt2_seguim", "mg_naco" : "mg_naco_seguim","diuret_basal" : "diur_segui", "mg_diur" : "mg_diur_segui", "mg_tiaz" : "mg_tiaz_seguim","ttm_basal___1" : "ttm_seguim___1","ttm_basal___2" : "ttm_seguim___2","ttm_basal___3" : "ttm_seguim___3","ttm_basal___4" : "ttm_seguim___4","ttm_basal___5" : "ttm_seguim___5","ttm_basal___6" : "ttm_seguim___6","ttm_basal___7" : "ttm_seguim___7","ttm_basal___8" : "ttm_seguim___8","ttm_basal___9" : "ttm_seguim___9","ttm_basal___10" : "ttm_seguim___10","ttm_basal___11" : "ttm_seguim___11","ttm_basal___12" : "ttm_seguim___12","ttm_basal___13" : "ttm_seguim___13","ttm_basal___14" : "ttm_seguim___14","ttm_basal___15" : "ttm_seguim___15","ttm_basal___16" : "ttm_seguim___16","ttm_basal___17" : "ttm_seguim___17","ttm_basal___18" : "ttm_seguim___18","ttm_basal___19" : "ttm_seguim___19","ttm_basal___20" : "ttm_seguim___20","ttm_basal___21" : "ttm_seguim___21","ttm_basal___22" : "ttm_seguim___22","ttm_basal___23" : "ttm_seguim___23","ttm_basal___24" : "ttm_seguim___24","ttm_basal___25" : "ttm_seguim___25","ttm_basal___26" : "ttm_seguim___26","ttm_basal___27" : "ttm_seguim___27","ttm_basal___28" : "ttm_seguim___28","ttm_basal___29" : "ttm_seguim___29","ttm_basal___30" : "ttm_seguim___30","ttm_basal___31" : "ttm_seguim___31","ttm_basal___32" : "ttm_seguim___32","ttm_basal___33" : "ttm_seguim___33","tiaz_basal___1" : "tiaz_seguim___1","tiaz_basal___2" : "tiaz_seguim___2","tiaz_basal___3" : "tiaz_seguim___3","antial_basal___1" : "antial_seguim___1","antial_basal___2" : "antial_seguim___2","antial_basal___3" : "antial_seguim___3","iecas_basal___1" : "ieca_seguim___1","iecas_basal___2" : "ieca_seguim___2","iecas_basal___3" : "ieca_seguim___3","iecas_basal___4" : "ieca_seguim___4","ara2_basal___1" : "ara2_seguim___1","ara2_basal___2" : "ara2_seguim___2","ara2_basal___3" : "ara2_seguim___3","ara2_basal___4" : "ara2_seguim___4","beta_basal___1" : "beta_seguim___1","beta_basal___2" : "beta_seguim___2","beta_basal___3" : "beta_seguim___3","beta_basal___4" : "beta_seguim___4","beta_basal___5" : "beta_seguim___5","isglt2_basal___1" : "islgt2_seguim___1","isglt2_basal___2" : "islgt2_seguim___2","isglt2_basal___3" : "islgt2_seguim___3","isglt2_basal___4" : "islgt2_seguim___4","naco_basal___1" : "naco_seguim___1","naco_basal___2" : "naco_seguim___2","naco_basal___3" : "naco_seguim___3","naco_basal___4" : "naco_seguim___4","naco_basal___5" : "naco_seguim___5","trast_cond_basal":"trastorn_conduccio_t_v_1","qrs_basal":"amplada_qrs_seguim","diuret_basal___1":"diur_segui___1","diuret_basal___2":"diur_segui___2","diuret_basal___3":"diur_segui___3","estim_marcap_basal":"marca"}, inplace= True)
    # Performing the join.
    unified = pd.concat([df,df2], ignore_index=True)
    unified.sort_values(by=['nhc','redcap_repeat_instance'], inplace= True)
    unified.reset_index(drop=True, inplace=True)
    return unified
#--------------------------------------------------
def fix_echocardiograms(df):
    """
    Function that treats problems in the given data frame related
    to echocardiograms variables.
    """
    # Extracting all the information about 'ona e' and 'ona e' using their relationships.
    for i, row in df.iterrows():
        if (pd.isnull(row.relacio_seguim) == False) and (pd.isnull(row.ona_e_prima_seguim)== False) and (pd.isnull(row.ona_e_seguim) == False):
            pass
        elif (pd.isnull(row.relacio_seguim) != False) and (pd.isnull(row.ona_e_prima_seguim)== False) and (pd.isnull(row.ona_e_seguim) == False):
            df.at[i,"relacio_seguim"] = (row.ona_e_seguim/row.ona_e_prima_seguim)
        elif (pd.isnull(row.relacio_seguim) == False) and (pd.isnull(row.ona_e_prima_seguim) != False) and (pd.isnull(row.ona_e_seguim) == False):
            df.at[i,"ona_e_prima_seguim"] = (row.ona_e_seguim/row.relacio_seguim)
        elif (pd.isnull(row.relacio_seguim) == False) and (pd.isnull(row.ona_e_prima_seguim) == False) and (pd.isnull(row.ona_e_seguim) != False):
            df.at[i,"ona_e_seguim"] = (row.ona_e_prima_seguim * row.relacio_seguim)

    # Establishing as 'Non-Measured (NM)' all the echocardiograms columns that weren't measuered.
    for i, row in df.iterrows():
        aux = row.values[np.r_[51:62,63:65]].tolist()
        if all ([pd.isnull(x) for x in aux]):
            df.iloc[i,np.r_[51:62,63:65]] = "NM"
#--------------------------------------------------
def fix_electrocardiograms(df):
    """
    Function that treats problems in the given data frame related
    to electrocardiograms variables.
    """
    patient_id = 'aux'
    value = np.nan
    for i, row in df.iterrows():
        aux = row.nhc
        if aux != patient_id: # The patient has changed.
            value = row.amplada_qrs_seguim
            patient_id = aux
        if not pd.isnull(row.amplada_qrs_seguim): # New value detected.
            value = row.amplada_qrs_seguim
        else: #Hay que replicar los valores.
            df.at[i,"amplada_qrs_seguim"] = value
#--------------------------------------------------
def fix_analytics(df):
    """
    Function that treats problems in the given data frame related
    to electrocardiograms variables.
    """
    for i, row in df.iterrows():
        aux = row.values[np.r_[84:102]].tolist()
        if all ([pd.isnull(x) for x in aux]):
            df.iloc[i,np.r_[84:102]] = "NM"
#--------------------------------------------------
def bin_IMC(df):
    """
    Selected thresholds

    · IMC < 18.5  (under weight) → 1
    · IMC in [18.5,24.9] (healthy weight) → 2
    · IMC in [25.0,29.9] (overweight) → 3
    · 30.0 <= IMC (obesity) → 4
    """
    for i, row in df.iterrows():
        imc = float(row.IMC)
        if not pd.isnull(imc):
            if imc < 18.5: df.at[i,"IMC"] = 1
            elif imc < 24.9: df.at[i,"IMC"] = 2
            elif imc < 29.9: df.at[i,"IMC"] = 3
            else: df.at[i,"IMC"] = 4
#--------------------------------------------------
def meds_inference(df,col1,col2):
    """
    Auxiliary function that modifies the values of 'col2' to be consistent
    with the values present on 'col1'.
    are full.
    """
    df.loc[(df[col1] == "Unchecked") & (df[col2] != 0),col1] = "Checked"
    df.loc[(df[col1] == "Checked") & (pd.isnull(df[col2]) | (df[col2] == 0)),col2] = "1"
#--------------------------------------------------
def fix_meds(df):
    """
    Function that treats problems in the given data frame related
    to electrocardiograms variables.
    """
    # Imputing wrongly imputed values on meds.
    df.loc[(df["ttm_seguim___5"] == "Checked") & (df["tiaz_seguim___1"] == "Checked")
                  & (df["mg_tiaz_seguim"] == 0),"mg_tiaz_seguim"] = "2"
    cols1 = []
    cols1 += generate_column_names('ttm_seguim',8)
    cols1 += ["ttm_seguim___19","ttm_seguim___22"]
    cols2 = ["mg_ieca_seguim","mg_ara2_seguim","mg_sacu_seguim","mg_diur_segui",
             "mg_tiaz_seguim","mg_anti_seguim","mg_beta_seguim","mg_naco_seguim",
             "mg_islgt2_seguim"]
    for base_col,mod_col in zip(cols1,cols2):
        meds_inference(df,base_col,mod_col)
#--------------------------------------------------
def check_intervals(df,var,var_types,intervals,col_check):
    '''
    Function that modifies the 'col_check' value taking
    into account the references columns classifing on one
    of the provided intervals.
    '''
    for i ,row in df.iterrows():
        if row[var] == "Checked":
            aux = 0
            for j,typ in enumerate(var_types):
                if row[typ] == "Checked":
                    aux = j
            # Imputing NAs with the first category.
            if  type(row[col_check]) == type("1"):
                df.at[i, col_check] = int(row[col_check])
            # Checking the dosis level.
            elif aux != len(var_types)-1 and not pd.isnull(row[col_check]):
                if row[col_check] == intervals[aux][0]:
                    df.at[i,col_check] = 1 # Dosis minima.
                elif row[col_check] == intervals[aux][1]:
                    df.at[i,col_check] = 3 #Dosis maxima.
                elif row[col_check] > intervals[aux][0] and row[col_check] < intervals[aux][1]:
                    df.at[i,col_check] = 2 #Dosis intermedia.
                else:
                    df.at[i,col_check] = np.nan
            # A 'other' med class is selected.
            elif aux == len(var_types)-1:
                df.at[i,col_check] = 1
#--------------------------------------------------
def bin_meds(df):
    '''
    Function that splits the meds absolute dosis in categories
    depending on the dosis level.
    '''
    #Intervals
    beta = [[1.25,10],[6.25,50],[50,100],[1.25,10]]
    ieca = [[5,20],[2.5,10],[2.5,20]]
    ara2 = [[80,320],[12.5,150],[4,32]]
    antial = [[12.5,50],[12.5,50]]
    naco = [[0.0001,20],[0.0001,10],[0.0001,60],[0.0001,300]]
    tiaz = [[12.5,50],[12.5,50]]
    diur = [[20,10000],[2.5,10000]]
    islgt2 = [[10,25],[5,10],[100,300]]

    check_intervals(df,"ttm_seguim___2",["ara2_seguim___"+str(k) for k in range(1,4+1)],ara2,"mg_ara2_seguim")
    df.loc[(df.ttm_seguim___1 == "Checked") & (df.ieca_seguim___1 == "Checked") & (df.mg_ieca_seguim == 2.5),"mg_ieca_seguim"] = 5
    check_intervals(df,"ttm_seguim___1",["ieca_seguim___"+str(k) for k in range(1,4+1)],ieca,"mg_ieca_seguim")
    df.loc[(df.ttm_seguim___6 == "Checked") & (df.mg_anti_seguim == 6.25),"mg_anti_seguim"] = 12.5
    check_intervals(df,"ttm_seguim___6",["antial_seguim___"+str(k) for k in range(1,3+1)],antial,"mg_anti_seguim")
    check_intervals(df,"ttm_seguim___19",["naco_seguim___"+str(k) for k in range(1,5+1)],naco,"mg_naco_seguim")
    df.loc[(df.ttm_seguim___7 == "Checked") & (df.beta_seguim___2 == "Checked") & (df.mg_beta_seguim == 3.125),"mg_beta_seguim"] = 6.25
    df.loc[(df.ttm_seguim___7 == "Checked") & (df.mg_beta_seguim == 125),"mg_beta_seguim"] = 1.25
    df.loc[(df.ttm_seguim___7 == "Checked") & (df.mg_beta_seguim == 625),"mg_beta_seguim"] = 6.25
    check_intervals(df,"ttm_seguim___7",["beta_seguim___"+str(k) for k in range(1,5+1)],beta,"mg_beta_seguim")
    df.loc[df.mg_tiaz_seguim == 75,"mg_tiaz_seguim"] = 50
    check_intervals(df,"ttm_seguim___5",["tiaz_seguim___"+str(k) for k in range(1,3+1)],tiaz,"mg_tiaz_seguim")
    check_intervals(df,"ttm_seguim___4",["diur_segui___"+str(k) for k in range(1,3+1)],diur,"mg_diur_segui")
    check_intervals(df,"ttm_seguim___22",["islgt2_seguim___"+str(k) for k in range(1,4+1)],islgt2,"mg_islgt2_seguim")
#--------------------------------------------------
def execute_script():
    visita_basal,visita_seguiment,events = read_and_clean()
    fix_endovenous_treatment(visita_basal,visita_seguiment,events)
    duplicating_info(visita_basal,visita_seguiment)
    visita_basal, visita_seguiment = fix_following_treatment(visita_basal,visita_seguiment)
    visita_seguiment = fix_hr_columns(visita_seguiment)
    unified = unify_dfs(visita_basal,visita_seguiment)
    # Fixing wrong manualy introduced values
    # unified.at[517,"albuorin_seguim"] = unified.at[517,"albuorin_seguim"].split(" ")[0]
    unified.drop(columns ="antecedents___19",inplace = True)
    fix_echocardiograms(unified)
    fix_electrocardiograms(unified)
    fix_analytics(unified)
    bin_IMC(unified)
    fix_meds(unified)
    bin_meds(unified)
    # Change these values to modify the file names.
    names = ['fixed_CS3']
    # Change this variable to modify the saving path.
    saving_path = './Temp/'
    aux_fun.write_csvs([unified],saving_path,names)
#--------------------------------------------------
