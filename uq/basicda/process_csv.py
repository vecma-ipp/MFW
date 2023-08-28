"""
Python script to proccess the outcome of turbulent (fluxes) time traces analyisis from scripts gem_postproc_vary_test.sh and gem_da.py
- concatenates files for different runs into a common table file
- deletes repeated readings
"""
import sys
import csv
import glob
from  itertools import zip_longest
import numpy as np


code = 'gem'
profile = 'ti'
attribute = 'transp'
quantity = 'flux'
campname = 'csldvnei'
num = 11

n_read = 6

pattern = 'gem_data/'+code+'_'+profile+'_'+attribute+'_'+quantity+'_evol_all_'+campname+'_'+str(num)+'_*.csv'

runs_file_list = glob.glob(pattern) # looks up all files according to a certain name pattern

n_runs = len(runs_file_list)

data = []
for r_f in runs_file_list:
    r = np.genfromtxt(r_f, delimiter=", ").T.tolist()
    del r[n_read-1::n_read] # delete every n_read reading
    data.append(r)

data_t = list(zip_longest(*data)) # transpose list of lists - careful

save_file = code+'_'+profile+'_'+attribute+'_'+quantity+'_tot_'+campname+'_'+str(num)+'.csv'
with open(save_file, "w", newline="") as f:
    writer = csv.writer(f)

    writer.writerows(data_t)

