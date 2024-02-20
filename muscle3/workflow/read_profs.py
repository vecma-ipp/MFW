import sys
import os

import numpy as np
import pandas as pd
from itertools import product

import time as t

import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

# from ascii_cpo import read
# import easysurrogate as es

# #uqbasicdadir = '~/code/MFW/uq/basicda'
# uqbasicdadir = '/u/yyudin/code/MFW/uq/basicda/'
# sys.path.append(uqbasicdadir)
# from gem_da import profile_evol_load, profile_evol_plot
# from da_utils import read_cpo_file
# from extcodehelper import ExtCodeHelper

#muscle3srcdir = '~/code/MFW/muscle3/src'
muscle3srcdir = '/u/yyudin/code/MFW/muscle3/src/'
sys.path.append(muscle3srcdir)
from muscle_utils.utils import coreprof_to_input_value, read_files, read_equil_1d, plot_quantities, write_table_csv, compare_transp, plot_prof_time, read_profs
#src.utils:q


if __name__ == '__main__':

    if len(sys.argv) < 2 :
        codename = 'gem_'
    else:
        codename = str(sys.argv[1])

    if len(sys.argv) < 3 :
        dates = ['20230823_151955']
    else:
        dates = sys.argv[2:]

    if len(sys.argv) < 4 :
        # NOW WOULDN'T WORK!: the script accepts an arbitrary long list of 'dates' as input
        ref_data_filename = 'ref_train_data_5000.csv'
    else:
        ref_data_filename = sys.argv[-1]
    # ATTENTION: overload for now
    ref_data_filename = 'ref_train_data.csv'

    print(f"> Now postprocessing a MUSCLE3 workflow run")
    
    st = t.time()

    cpo_names = ['coreprof', 'coretransp', 'equilibrium']
    
    # Run the postprocessing function

    #dates = read_profs(codename=codename, dates=dates, ref_data_filename=ref_data_filename, cpo_names=cpo_names) # to read results of M3-WF run
    
    dates = read_profs(codename=codename, dates=dates, ref_data_filename=ref_data_filename, cpo_names=cpo_names, prefix_name='run_fusion_') # for the WF independent of its root directory location

    print(f"Postrprocessing finished, totally took {t.time()-st:.3f} s")

    ################
    #########
    # some set of first GEM-ETS-CHEASE runs with MUSCLE3
    #dates = ['20230818_135913', '20230821_161005', '20230822_150943', '20230823_151955', '20230824', '20230825', '20230828', '20230829', '20230830', '20230831', '20230901']
    #dates = ['20230918', '20230922']

    # plotting for standalone GEM to get good initial turbulence snapshots
    #dates = ['20230928', '20230929', '20231004', '20231006', '20231009', '20231010', '20231016',]# '20231017']
    #dates = read_profs(codename='', dates=dates, prefix_name='../standalone/bin/gem_develop_turbulence/', sufix_name='/', cpo_filebase='gem_', cpo_names=['coretransp']) # to read from stanalone runs

    # plotting LCFS for the same runs
    #read_equil(dates)
