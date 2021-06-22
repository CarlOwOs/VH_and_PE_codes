'''
Script that extracts the necessary tables
from the VH sever databases.
'''
#--------------------------------------------------
from sqlalchemy import create_engine
import cx_Oracle
import pandas as pd
import Auxiliary.auxiliary_functions as aux_fun
#--------------------------------------------------
# SERVER CONNECTION VARIABLES
DIALECT = 'oracle'
SQL_DRIVER = 'cx_oracle'
USERNAME = 'XXX' #Due to confidentiality reasons, this values will not be provided
PASSWORD = 'xxxxxxx' #Due to confidentiality reasons, this values will not be provided
HOST = 'servora5.cs.vhebron.es' 
PORT = 1521 
DATABASE = 'BBDD5'
ENGINE_PATH_WIN_AUTH = DIALECT + '+' + SQL_DRIVER + '://' + USERNAME + ':' + PASSWORD +'@' + HOST + ':' + str(PORT) + '/' + DATABASE 
#--------------------------------------------------
def execute_script():
    engine = create_engine(ENGINE_PATH_WIN_AUTH)
    # Establishing the connection with the server.
    con = engine.connect()
    uic_df = pd.read_sql('select * from UPC_UIC', con)
    # Change these values to modify the file names.
    names = ['UIC']
    # Change this variable to modify the saving path.
    saving_path = './Temp/'
    dfs = [uic_df]
    aux_fun.write_csvs(dfs,saving_path,names)
#--------------------------------------------------
