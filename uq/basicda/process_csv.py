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

from da_utils import plot_timetraces_act
from gem_da import profile_evol_plot

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

    if len(sys.argv) < 3 :
        prefix = '' # 'gem_data/'
    else:
        prefix = sys.argv[2]

    pattern = prefix+code+'_'+profile+'_'+attribute+'_'+quantity+'_evol_all_'+campname+'_'+str(num)+'_*.csv'

    runs_file_list = glob.glob(pattern) # looks up all files according to a certain name pattern

    runs_file_list.sort(key=lambda n: int(n[n.rfind('_')+1:-4]))  #.csv is 4 symbols

    n_runs = len(runs_file_list)

    data = []

    for r_f in runs_file_list:

        r = np.genfromtxt(r_f, delimiter=",", skip_header=0).T.tolist()
        
        # Cleaning:
        # options 1: delete every n-th reading
        # del r[n_read-1::n_read] # delete every n_read reading
        # option 2: go to a list of criteria
        # option 2.a: delete a reading if it is (almost) exactly the same as the previous one
        r = clean_readings(r, option='repeat')
        
        data.append(r)

    data_t = list(zip_longest(*data)) # transpose list of lists - careful

    save_file = code+'_'+profile+'_'+attribute+'_'+quantity+'_tot_'+campname+'_'+str(num)+'.csv'
    with open(save_file, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(['run_'+str(x+1) for x in range(len(runs_file_list))])
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

                        array_loc = df_loc[c].to_numpy()
                        array_loc = array_loc[~np.isnan(array_loc)]
                        df[column_name].iloc[i] = array_loc
    
        df_name = 'timetracesscan_all_'+campname+'_'+str(num)+'.pickle'
        df.to_pickle(df_name)

    return df

def plot_series(row=0, campname='csldvnei', num=16, code='gem', profile='ti', attribute='transp', quantity='flux'):
    """
    Plots time traces for of a QoI for a single case; from dataframe
    """

    df_name = f"timetracesscan_all_{campname}_{num}.pickle"
    df = pd.read_pickle(df_name)

    col = f"{profile}_{attribute}_{quantity}"

    y = df[col].iloc[row]
    y = y[~np.isnan(y)]

    y_avg = y.mean()
    y_std = y.std()
    y_sem = y_std

    plot_name = f"{col}_{campname}_{num}_{row+1}"
    plot_timetraces_act(y,y_avg,y_std,y_sem,plot_name,0.0,10)
    
    return 0

def plot_series_per_app(campname='csldvnei', num=17, code='gem', profile='ti', attribute='transp', quantity='flux'):
    """
    Plots time traces for a group of cases, corresponding to a particular campaign 'app' (here: flux tube); from dataframe
    """

    df_name = f"timetracesscan_all_{campname}_{num}.pickle"
    df = pd.read_pickle(df_name)

    col = f"{profile}_{attribute}_{quantity}"

    # Number of apps and cases per app
    n_ft = 8
    len_ft = 81 # len_ft = len(df.rows) // n_ft

    for i_ft in range(n_ft):

        y_ft = df[col].iloc[i_ft*len_ft : (i_ft+1)*len_ft-1]
        y_ft_list = [x.reshape(1,-1) for x in y_ft]
        
        labels = [f"run_{i}" for i in range(i_ft*len_ft, (i_ft+1)*len_ft-1)]

        profile_evol_plot(y_ft_list, labels=labels, name=f"{code}_{profile}_{attribute}_{quantity}_ft{i_ft}_{campname}_{num}", alignment='start');

    return 0 

def main():

    profiles = ['ti', 'te', 'ni', 'ne']

    if len(sys.argv) < 2 :
        iternum = 13
    else:
        iternum = int(sys.argv[1])

    for p in profiles:
        combine_csv_series(num=iternum, profile=p)

    csv2pickle(num=iternum)

    return 0

if __name__ == '__main__':

    main()
