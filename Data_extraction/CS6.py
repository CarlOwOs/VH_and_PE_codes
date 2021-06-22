import pandas as pd
import numpy as np
import datetime
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
def read_and_clipping():
    '''
    Function that reads the imputed data frame and clips float
    formated columns that should be integers.
    '''
    df = pd.read_csv("./Temp/imputed_R.csv",index_col= 0)
    # Deleting the previous temporary files
    aux_fun.delete_csvs(["imputed_R","continuous_CS5"],"./Temp/")
    # Deleting observations that do not contain the patient identifier (nhc).
    df = df[~pd.isnull(df.nhc)]
    df.reset_index(drop=True, inplace=True)
    # Clipping of the float formated variables.
    df.edat_visita = df.apply(lambda row : round(float(row.edat_visita)), axis = 1)
    df.days_since_diagnostic = df.apply(lambda row : round(float(row.days_since_diagnostic)), axis = 1)
    df.days_since_start = df.apply(lambda row : round(float(row.days_since_start)), axis = 1)
    df.diff_anal_seguim = df.apply(lambda row : round(float(row.diff_anal_seguim)), axis = 1)
    df.data_visita_year = df.apply(lambda row : round(float(row.data_visita_year)), axis = 1)
    df.data_visita_week = df.apply(lambda row : round(float(row.data_visita_week)), axis = 1)
    df.data_visita_month = df.apply(lambda row : round(float(row.data_visita_month)), axis = 1)
    df.data_visita_day = df.apply(lambda row : round(float(row.data_visita_day)), axis = 1)
    return df
#--------------------------------------------------
def extract_quantile_tresholds(df):
    '''
    Auxiliary function that given a df containing continuous
    variables,  obtains the quantile limits of those columns
    that will be used later on fraccionate the data.
    '''
    # Extraction of the quantile limits.
    intervals_dict = {}
    for i in np.r_[53:63,64:66,67,86:109]:
      aux1=df.iloc[:,i].tolist()
      # Checking if the variable contains information.
      aux2 = [float(x) for x in aux1 if x !="NM" and not pd.isnull(x)]
      if len(df.iloc[:,i].unique()) > 4:
        _,bins= pd.qcut(aux2, 4,retbins = True)
        intervals_dict[df.columns[i]] = bins
    return intervals_dict
#--------------------------------------------------
def bins_variable(val,dictionary,variable):
    '''
    Auxiliar function that given a value from a continuous variable,
    classifies them in one of each quantiles using the limits passed
    as input in a dictionary format.
    '''
    inter = dictionary[variable]
    if val <= inter[1]: return 1
    elif val <= inter[2]: return 2
    elif val <= inter[3]: return 3
    else: return 4
#--------------------------------------------------
def binning(df):
    '''
    Function that given a data frame with continuous columns,
    categorizes them according to their quantiles.
    '''
    df_cat = df.copy()
    intervals_dict = extract_quantile_tresholds(df_cat)
    for i, row in df_cat.iterrows():
        for j in np.r_[53:63,64:66,67,86:109]:
            if len(df_cat.iloc[:,j].unique()) > 4 and row[j] != "NM" and not pd.isnull(row[j]):
                val = bins_variable(float(row[j]),intervals_dict,df_cat.columns[j])
                df_cat.at[i,df_cat.columns[j]] = val
    return df_cat
#--------------------------------------------------
def convert_binary(df,col,aff_val,neg_val):
    '''
    Auxiliary function that given a binary variable, modifies its labels
    and introduces the values it recives as input
    '''
    column = df.columns[col]
    df.loc[df[column] == aff_val,column] = 1
    df.loc[df[column] == neg_val,column] = 0
#--------------------------------------------------
def convert_to_numbers(df,col):
    '''
    Auxiliary function that given a categorical variable index, modifies its labels
    and assigns a number to them.
    '''
    column = df.columns[col]
    aux = sorted(df[column].unique())
    for i,val in enumerate(aux):
        df.loc[df[column] == val,column] = i
#--------------------------------------------------
def convert_NM(df,col,val):
    '''
    Auxiliary function that given a column dataframe, imputes the 'NM'
    labeled observations with the provided input values
    '''
    column = df.columns[col]
    df[column] =df[column].astype(str)
    df.loc[df[column] == "NM",column] = val
    df[column] = pd.to_numeric(df[column])
#--------------------------------------------------
def fix_numerical_df(df):
    '''
    Function that given a df containg a mixture of formats
    performs modifications in multiple columns to adapt
    it use to ML models.
    '''
    for i in np.r_[9:50,74:79,109:145,146:149,150:153,154:158,159:163,164:169,171:175,176:181]:
        convert_binary(df,i,"Checked","Unchecked")
    convert_binary(df,66,"Fibril·lació auricular","Sinusal")
    convert_binary(df,69,"Si","No")
    convert_to_numbers(df,3)
    convert_to_numbers(df,8)
    convert_to_numbers(df,182)
    for i in np.r_[53:66,86:104]:
        convert_NM(df,i,0)
    # Now, several manually imputations will be performed. These modifications
    # take into account the natural ordering of the variable, making the difference
    # betwen them significant, in contrast to the convert_to_numbers function.
    #···············
    #Neoplasia estat
    df.loc[df.neoplasia_estat == "Cap","neoplasia_estat"] = 0
    df.loc[df.neoplasia_estat == "Prèvia","neoplasia_estat"] = 1
    df.loc[df.neoplasia_estat == "Activa","neoplasia_estat"] = 2
    #···············
    #Neoplasia quimio
    df.loc[df.neoplasia_qt == "Cap","neoplasia_qt"] = 0
    df.loc[df.neoplasia_qt == "No","neoplasia_qt"] = 1
    df.loc[df.neoplasia_qt == "Si","neoplasia_qt"] = 2
    #···············
    # Intervencio quirurgica previa
    df.loc[df.tipus_iqprevia == "Cap","tipus_iqprevia"] = 0
    df.loc[df.tipus_iqprevia == "Revascularizació","tipus_iqprevia"] = 1
    df.loc[df.tipus_iqprevia == "Valvular","tipus_iqprevia"] = 2
    df.loc[df.tipus_iqprevia == "Ambdues","tipus_iqprevia"] = 3
    #···············
    # Transtorn conduccio
    df.loc[df.trastorn_conduccio_t_v_1 == "Cap","trastorn_conduccio_t_v_1"] = 0
    df.loc[df.trastorn_conduccio_t_v_1 == "BBEFH","trastorn_conduccio_t_v_1"] = 1
    df.loc[df.trastorn_conduccio_t_v_1 == "BBDFH","trastorn_conduccio_t_v_1"] = 2
    df.loc[df.trastorn_conduccio_t_v_1 == "Trastorn inespecífic","trastorn_conduccio_t_v_1"] = 3
#--------------------------------------------------
def one_hot_dummy(df,column):
    '''
    Auxilary function that given a df and a column, converts it to
    a dummy column set in the same location that the original column
    was located
    '''
    last = len(df.columns)-1
    if any([pd.isnull(x) for x in df[column]]):
        df = pd.concat([df,pd.get_dummies(df[column], prefix=column,dummy_na=True,drop_first=True)],axis=1)
    else:
        df = pd.concat([df,pd.get_dummies(df[column], prefix=column,drop_first=True)],axis=1)
    pos = df.columns.get_loc(column)
    df.drop(columns = column ,axis=1, inplace=True)
    cols = df.columns.tolist()
    cols = cols[:pos] + cols[last:] + cols[pos:last]
    df = df[cols]
    return df
#--------------------------------------------------
def one_hot(df):
    '''
    Function that given a df performs the necessary modifications to
    generate a one-hot formated dataframe.
    '''
    for i in np.r_[9:50,74:79,109:145,146:149,150:153,154:158,159:163,164:169,171:175,176:181]:
        convert_binary(df,i,"Checked","Unchecked")

    convert_binary(df,66,"Fibril·lació auricular","Sinusal")
    convert_binary(df,69,"Si","No")
    for i in np.r_[53:66,86:104]:
        convert_NM(df,i,0)
    for i in ["sexe","procedencia","estacio_visita","neoplasia_estat","neoplasia_qt","tipus_iqprevia","trastorn_conduccio_t_v_1"]:
        df = one_hot_dummy(df,i)
    return df
#--------------------------------------------------
def generate_col_names(col_vect,name,size):
    '''
    Auxiliary function used for column name generation.
    '''
    for i in range(1,size+1):
        col_vect.append(name + "___"+str(i))
#--------------------------------------------------
def identify_prev_useless_cols():
    '''
    Auxiliary function that finds the columns
    that contain redundant or unuseful information.
    '''
    cols_2_del = []
    generate_col_names(cols_2_del,"naco_seguim",5)
    generate_col_names(cols_2_del,"islgt2_seguim",4)
    generate_col_names(cols_2_del,"beta_seguim",5)
    generate_col_names(cols_2_del,"ara2_seguim",4)
    generate_col_names(cols_2_del,"ieca_seguim",4)
    generate_col_names(cols_2_del,"antial_seguim",3)
    generate_col_names(cols_2_del,"tiaz_seguim",3)
    generate_col_names(cols_2_del,"diur_segui",3)
    cols_2_del.extend(["antecedents___18","relacio_seguim"])
    for i in [4,5,6,1,2,7,3,22,19]:
        cols_2_del.append("ttm_seguim___"+str(i))
    return cols_2_del
#--------------------------------------------------
def identify_unilabeled_cols(df,list_cols):
    '''
    Auxiliary function that finds the columns of the given df
    that only contain one value.
    '''
    for col in df.columns:
        if len(df[col].unique()) < 2 and col not in list_cols:
            list_cols.append(col)
#--------------------------------------------------
def delete_useless_columns(df):
    '''
    Function that deletes the useless columns of the
    given dataframe.
    '''
    cols_2_del = identify_prev_useless_cols()
    identify_unilabeled_cols(df, cols_2_del)
    return df.drop(columns = cols_2_del)
#-----------------------------------
def execute_script():
    df = read_and_clipping()
    df_cat = binning(df)
    #  Obtaining only-numerical dataframes.
    df_numbers = df_cat.copy()
    df_numbers_cont = df.copy()
    fix_numerical_df(df_numbers)
    fix_numerical_df(df_numbers_cont)
    # Obtaining one-hot dataframes
    df_oneh = df_cat.copy()
    df_oneh_cont = df.copy()
    df_oneh = one_hot(df_oneh)
    df_oneh_cont = one_hot(df_oneh_cont)
    df_numbers_del = delete_useless_columns(df_numbers)
    df_numbers_del_cont = delete_useless_columns(df_numbers_cont)
    df_oneh_del_cont = delete_useless_columns(df_oneh_cont)
    dfs = [df_numbers_del,df_numbers_del_cont,df_oneh_del_cont]
    # Change these values to modify the file names.
    names = ['df_numbers_del','df_numbers_del_cont','df_oneh_del_cont']
    # Change this variable to modify the saving path.
    saving_path = './Clean_data/'
    aux_fun.write_csvs(dfs,saving_path,names)
#--------------------------------------------------
