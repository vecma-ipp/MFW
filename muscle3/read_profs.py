import sys, os, csv
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from itertools import product

from ascii_cpo import read

sys.path.append('../uq/basicda')
from gem_da import profile_evol_load, profile_evol_plot

def read_attrib(filename, quantity, attribute, coords, filetype='coreprof'):
    """
    Returns list of attribute values, for every given coordinate number
    """
    cpo_obj = read(filename, filetype)
    q = cpo_obj
    if filetype == 'coretransp':
        q = cpo_obj.values[0]
    q = getattr(q, quantity)
    a = getattr(q, attribute)

    if quantity[1] == 'i':
        # ..[0] for ions, if it is the first ion species
        a_s = [a[c][0] for c in coords]
    else:
        a_s = [a[c] for c in coords]
    return a_s

def read_files(foldername, quantity, attribute, coords, filetype='coreprof', date=''):
    
    file_base_tocheck = filetype+'_'
    file_ext = '.cpo'
    
    file_names = [f for f in os.listdir(foldername) if 
                        os.path.isfile(os.path.join(foldername, f)) and 
                        f.endswith(file_ext) and
                        f.startswith(file_base_tocheck)
            ]
    file_names.sort()

    attributes = []

    for f in file_names:
        a = read_attrib(foldername+f, quantity, attribute, coords, filetype)
        #print(f"a= {a}") ###DEBUG
        attributes.append(a)

    # List of lists to a numpy array
    atrributes_array = np.array(attributes)
    np.savetxt('res_'+quantity+'_'+attribute+'_'+date+'.csv', np.array(atrributes_array), delimiter=',')
    
    return atrributes_array

date = '20230622_135650'
codename = 'gem_surr'
load_fold_name = 'workflow/run_fusion_'+codename+'_'+date+'/instances/transport/workdir/'
save_fold_name = 'workflow/run_fusion_'+codename+'_'+date+'/'

#n_run = 10
#runnum_list = [r for r in range (n_run)]
n_rho_resol = 1

# load reference data for the surrogate training data set

if 'sur' in codename:
    ref_data_filename = 'ref_train_data.csv'
    ref_data = pd.read_csv(ref_data_filename, sep=',')

lookup_names= {
        "ti_value" : "$T_{{i}}$",
        "te_value" : "$T_{{e}}$",
        "ti_ddrho" : "$\\nabla T_{{i}}$",
        "te_ddrho" : "$\\nabla T_{{e}}$",
        'te_transp.flux': "$Q_{{e}}$", 
        'ti_transp.flux': "$Q_{{i}}$",
              }

cpo_names = ['coreprof', 'coretransp']

for cpo_name in cpo_names:

    if cpo_name == 'coreprof':
        
        quantities =  ['ti', 'te', 'ni', 'ne']
        
        attributes = ['ddrho', 'value', ]
        
        #coord_num = [68]
        #coord_num = [14, 30, 43, 55, 66, 76, 85, 95]
        coord_num = [x for x in range(0, 100, n_rho_resol)]

        n_ft = 68

        i_q_s = [0,1,2,3]
        j_a_s = [0,1]

    if cpo_name == 'coretransp':
        quantities = ['ti_transp', 'te_transp']

        attributes = ['flux', 'diff_eff', 'vconv_eff']

        if codename == 'gem_surr':
            coord_num = [0] # if n_fts==1
            n_ft = 0
        else:
            coord_num = [0,1,2,3,4,5,6,7] # if n_fts==8
            n_ft = 4


        i_q_s = [0,1]
        j_a_s = [0,1,2]

    for i_q, j_a in product(i_q_s, j_a_s):

        # Reading data of a particular attribute for all files produced by transport code
        data = read_files(load_fold_name, quantities[i_q], attributes[j_a], coords=coord_num, filetype=cpo_name, date=date)

        data_file_name = save_fold_name+'res_'+codename+'_'+quantities[i_q]+'_'+attributes[j_a]+'_'+date+'.csv'
        #data_file_name = 'workflow/run_fusion_gem0_20230614_171913/instances/turbulence/gem0_ti_transp_flux_20230614_171913.csv'
        #data = np.genfromtxt(data_file_name, delimiter=',')[::2].reshape(-1, 1) # ::2 due to two calls of printing from e.g. GEM0

        #print(f"data shape: {data.shape}") #DEBUG
        n_timesteps = data.shape[0]
        n_rhos = data.shape[1]

        #n_timesteps = 5
        # Plotting an attribute value at a the n_ft-th flux tube against time
        fig, ax = plt.subplots()
        ax.plot(np.linspace(0, n_timesteps, n_timesteps), data[:n_timesteps, n_ft], '.')
        ax.set_xlabel('${{t}}$, time-steps')
        #TODO: display integer numbers of time steps
        ax.set_ylabel(lookup_names[quantities[i_q]+'_'+attributes[j_a]] if (quantities[i_q]+'_'+attributes[j_a] in lookup_names) else quantities[i_q]+'_'+attributes[j_a])
        if 'sur' in codename:
            if quantities[i_q]+'_'+attributes[j_a] in ref_data.columns:
                min_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].min()
                max_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].max()
                ax.hlines(y = min_val, xmin=0, xmax=n_timesteps, color='r', linestyle='--', label='bounds of the training dataset')
                ax.hlines(y = max_val, xmin=0, xmax=n_timesteps, color='r', linestyle='--')
        ax.legend(loc='best')
        fig.savefig(save_fold_name+'res_'+codename+'_'+quantities[i_q]+'_'+attributes[j_a]+'_'+date+'.pdf')
        plt.close()

        # Plotting attribute profile for multiple time-steps
        n_timesteps_toplot = 1
        fig, ax = plt.subplots()
        for t in range(0, n_timesteps, n_timesteps_toplot):
        #for t in range(0, 5, 1):
            ax.plot(np.array(coord_num), data[t, :], label=f"t={t}", marker='.')
        #ax.set_yscale('symlog')
        ax.set_xlabel('rho coord')
        ax.set_ylabel(quantities[i_q]+'_'+attributes[j_a])
        ax.legend(loc='best')
        fig.savefig(save_fold_name+'prof_'+codename+'_'+quantities[i_q]+'_'+attributes[j_a]+'_'+date+'.pdf')
        plt.close()
