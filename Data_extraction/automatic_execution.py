'''
Automatic script that performs the cleaning
process of the VH data.
'''
#--------------------------------------------------
import CS1
import CS2
import CS3
import CS4
import CS5
import CS6
import OR_python as OR
import Auxiliary.auxiliary_functions as aux_fun
import os
import shutil
import time
#--------------------------------------------------
def main():
    # Creating a temporary auxiliar folder.
    if os.path.exists("./Temp/"):
        shutil.rmtree("./Temp")
    os.mkdir("./Temp")
    CS1.execute_script()
    print("Performing the Open Refine cleaning process...")
    OR.execute_script()
    CS2.execute_script()
    CS3.execute_script()
    CS4.execute_script()
    CS5.execute_script()
    print("Starting R imputation process")
    #os.system('Rscript imputation.R > /dev/null 2>&1')
    os.system('Rscript imputation.R')
    while not os.path.exists('./Temp/imputed_R.csv'):
        time.sleep(1)
    CS6.execute_script()
main()
