"""
Python script to proccess the outcome of turbulent (fluxes) time traces analyisis from scripts gem_postproc_vary_test.sh and gem_da.py
- concatenates files for different runs into a common table file
- deletes repeated readings
"""
#TODO this can be moved to da_utils.py as a function

import sys
import csv
import glob
import os
from itertools import zip_longest, product
import numpy as np
import pandas as pd
import math

def clean_readings(data, option='repeat'):
    """
    Cleans reading that are deemed unnecessary in this list for time traces:
    option:
        - repeat: if a reading is almost exact repetition of the previoud reading, do not add to the resulting list
    """
    n = len(data)
    data_new = []

    if option == 'repeat':
        data_new = [data[i] for i in range(1, n) if not math.isclose(data[i], data[i-1])]
        data_new.insert(0, data[0])

    return data_new

def combine_csv_series(campname='csldvnei', num=11, code='gem', profile='ti', attribute='transp', quantity='flux'):
    """
    Load all CSV files related for a particualar campaign, submisstion and quantity
    and save a single CSV
    """

    n_read = 6

    prefix = '' # 'gem_data/'

    pattern = prefix+code+'_'+profile+'_'+attribute+'_'+quantity+'_evol_all_'+campname+'_'+str(num)+'_*.csv'

    runs_file_list = glob.glob(pattern) # looks up all files according to a certain name pattern

    runs_file_list.sort()

    n_runs = len(runs_file_list)

    data = []

    for r_f in runs_file_list:

        r = np.genfromtxt(r_f, delimiter=", ").T.tolist()
        
        # del r[n_read-1::n_read] # delete every n_read reading
        #r = clean_readings(r)
        
        data.append(r)

    data_t = list(zip_longest(*data)) # transpose list of lists - careful

    save_file = code+'_'+profile+'_'+attribute+'_'+quantity+'_tot_'+campname+'_'+str(num)+'.csv'
    with open(save_file, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerows(data_t)

def csv2pickle(campname='csldvnei', num=13, codes=['gem'], profiles=['ti','te','ni','ne'], attributes=['transp'], quantities=['flux']):
    """
    Loads a list of csv files, combines them into pandas dataframe and saves as a pickle file
    """

    prefix = '' # 'gem_data/'

    n_runs = 648

    columns = [p+'_'+a+'_'+q for p,a,q in product(profiles, attributes, quantities)]

    index = [x for x in range(n_runs)]

    for code in codes:
        
        df = pd.DataFrame(index=index, columns=columns)

        for profile in profiles:
            for attribute in attributes:
                for quantity in quantities:

                    column_name = profile+'_'+attribute+'_'+quantity

                    file_name = prefix+code+'_'+profile+'_'+attribute+'_'+quantity+'_tot_'+campname+'_'+str(num)+'.csv'

                    df_loc = pd.read_csv(file_name, delimiter=',')

                    # Each colum read from a CSV file is time traces for a particular case; 
                    #   cases corrsespond to rows in a global dataframe

                    for i,c in enumerate(df_loc.columns):

                        df[column_name].iloc[i] = df_loc[c].to_numpy()
    
        df_name = 'timetracesscan_all_'+campname+'_'+str(num)+'.pickle'
        df.to_pickle(df_name)

    return df


def main():

    profiles = ['ti', 'te', 'ni', 'ne']

    if len(sys.argv) < 2 :
        iternum = 13
    else:
        iternum = sys.argv[1]

    for p in profiles:
        combine_csv_series(num=iternum, profile=p)


if __name__ == '__main__':

    #main()

    csv2pickle()
