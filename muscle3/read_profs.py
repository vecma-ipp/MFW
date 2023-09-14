import sys
import os
import csv
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
    elif filetype == 'equilibrium':
        q = cpo_obj.eqgeometry

    q = getattr(q, quantity)
    a = getattr(q, attribute)

    if quantity[1] == 'i':
        # ..[0] for ions, if it is the first ion species
        a_s = [a[c][0] for c in coords]
    else:
        a_s = [a[c] for c in coords]
    return a_s

def read_time(filename, filetype='coreprof'):
    """
    Returns time read in the CPO (coreprofile) file
    """

    cpo_obj = read(filename, filetype)

    t = float(getattr(cpo_obj, 'time'))

    return t

def read_files(foldername, quantity, attribute, coords, filetype='coreprof', date=''):

    file_base_tocheck = filetype+'_'
    file_ext = '.cpo'

    """
    if foldername is list:
        file_names = [[f for f in os.listdir(d) if
                  os.path.isfile(os.path.join(d, f)) and
                  f.endswith(file_ext) and
                  f.startswith(file_base_tocheck)
                  ] for d in foldername]
        file_names = [fs.sort() for fs in file_names]
        #TODO turn into a single list of [foldername]/[filename]
        file_names = [foldername[i]+f for f in fs for i,fs in enumerate(file_names)]    
    else:
    """

    file_names = [f for f in os.listdir(foldername) if
                  os.path.isfile(os.path.join(foldername, f)) and
                  f.endswith(file_ext) and
                  f.startswith(file_base_tocheck)
                  ]
    file_names.sort()

    # add the last file, that might be named differently
    last_file_tent_name = 'ets_'+filetype+'_out'+file_ext
    if os.path.exists(os.path.join(foldername, last_file_tent_name)):
        file_names.append(last_file_tent_name)

    #print(file_names) ###DEBUG

    attributes = []
    times = []

    for f in file_names:
        a = read_attrib(foldername+f, quantity, attribute, coords, filetype)
        # print(f"a= {a}") ###DEBUG
        attributes.append(a)

        t = read_time(foldername+f, filetype)
        times.append(t)

    # List of lists to a numpy array
    atrributes_array = np.array(attributes)
    times_array = np.array(times)

    return atrributes_array, times_array

def read_equil(foldernames):
    """
    Reads equilibrium file and plots last closed flux surface 
    foldername: list of names of form: [year-month-day] e.g. 20230901
    """

    filetype = "equilibrium"
    quantity = "boundary"
    attribute = ["r", "z"]

    labels = [f"iteration #{(j+1)*100}" for j in range(len(foldernames))]

    fig, ax = plt.subplots()

    for i,foldername in enumerate(foldernames):
    
        filename = 'workflow/run_fusion_gem_'+ foldername + '/instances/equilibrium/workdir/chease_equilibrium_out.cpo' 

        cpo_obj = read(filename, filetype)
    
        rs = cpo_obj.eqgeometry.boundary[0].r
        zs = cpo_obj.eqgeometry.boundary[0].z

        ax.plot(rs, zs, label=labels[i])
    
    ax.set_aspect('equal', 'datalim')
    ax.legend(loc='best')
    fig.savefig(foldername[0]+'_equilibrium.pdf')
    plt.close()

def read_profs():

    if len(sys.argv) < 2 :
        #codename = 'gem_surr'
        codename = 'gem'
    else:
        codename = str(sys.argv[1])

    if len(sys.argv) < 3 :
        dates = ['20230823_151955']
    else:
        dates = sys.argv[2:]

    load_fold_names = ['workflow/run_fusion_'+codename + \
        '_'+date+'/instances/transport/workdir/' for date in dates]

    save_fold_name = 'workflow/run_fusion_'+codename+'_'+dates[0]+'_'+dates[-1]+'/'

    if not os.path.exists(save_fold_name):
        os.makedirs(save_fold_name)
    
    #n_run = 10
    #runnum_list = [r for r in range (n_run)]
    n_rho_resol = 1

    # Load reference data for the surrogate training data set

    if 'sur' in codename:
        ref_data_filename = 'ref_train_data.csv'
        ref_data = pd.read_csv(ref_data_filename, sep=',')

    lookup_names = {
        "ti_value": "$T_{{i}}$",
        "te_value": "$T_{{e}}$",
        "ti_ddrho": "$\\nabla T_{{i}}$",
        "te_ddrho": "$\\nabla T_{{e}}$",
        'te_transp.flux': "$Q_{{e}}$",
        'ti_transp.flux': "$Q_{{i}}$",
    }

    cpo_names = ['coreprof', 'coretransp']

    for cpo_name in cpo_names:

        if cpo_name == 'coreprof':

            quantities = ['ti', 'te', 'ni', 'ne']

            attributes = ['ddrho', 'value', ]

            #coord_num = [68]
            #coord_num = [14, 30, 43, 55, 66, 76, 85, 95]
            coord_num = [x for x in range(0, 100, n_rho_resol)]

            #n_ft = 68
            n_ft = coord_num[0]

            i_q_s = [0, 1, 2, 3] # indiced of quantities to go through
            j_a_s = [0, 1] # indiced of attributes to go through

        if cpo_name == 'coretransp':
            quantities = ['ti_transp', 'te_transp']

            attributes = ['flux', 'diff_eff', 'vconv_eff']

            if codename == 'gem_surr':
                #coord_num = [0]  # if n_fts==1
                coord_num = [0, 1, 2, 3, 4, 5, 6, 7] 
                n_ft = coord_num[-1]
            else:
                coord_num = [0, 1, 2, 3, 4, 5, 6, 7]  # if n_fts==8
                n_ft = coord_num[-1] #0 #4 # number of flux tube to plot for

            i_q_s = [0, 1] # indiced of quantities to go through
            j_a_s = [0, 1, 2] # indiced of attributes to go through

        for i_q, j_a in product(i_q_s, j_a_s):

            # Reading data of a particular attribute for all files produced by transport code
            # TODO: read data for many folders
            # TODO: better, first read all data into a single data structure e.g. pandas dataframe, accessing each file onse; then plot 
            data_list = []
            for load_fold_name, date in zip(load_fold_names, dates):
                data, times = read_files(
                    load_fold_name, quantities[i_q], attributes[j_a], coords=coord_num, filetype=cpo_name, date=date)
                data_list.append(data)
            data = np.concatenate(data_list, axis=0)

            data_file_name = save_fold_name+'res_'+codename+'_' + \
                quantities[i_q]+'_'+attributes[j_a]+'_'+dates[0]+'_'+dates[-1]+'.csv'
            np.savetxt(data_file_name, np.array(data), delimiter=',')
            # data = np.genfromtxt(data_file_name, delimiter=',')[::2].reshape(-1, 1) # ::2 due to two calls of printing from e.g. GEM0

            # print(f"data shape: {data.shape}") #DEBUG
            n_timesteps = data.shape[0]
            #print(f"size of data and time readings is the same: {n_timesteps == times.shape[0]}") ###DEBUG
            n_rhos = data.shape[1]

            color_list = ['b', 'g', 'r', 'y' , 'm', 'c', 'k']
            line_list = ['-', '--', '-.', ':']
            marker_list = ['', '.', 'o', 'v', '^', '<', '>']
            style_lists = [marker_list, line_list, color_list,] 
            fmt_list = [style for style in product(*style_lists)]

            ### Plotting an attribute value at a the n_ft-th flux tube against time
            
            fig, ax = plt.subplots()
            
            # x_values = np.linspace(0, n_timesteps, n_timesteps) # option 1: sequential number of time steps starting from 0
            x_values = times # option 2: timestamp read from coreprofile CPO files

            ax.plot(x_values,
                    data[:n_timesteps, n_ft], '.',
                    label=f"@{n_ft}")
            ax.set_xlabel('${{t}}$, time-steps')
            # TODO: display integer numbers of time steps
            ax.set_ylabel(lookup_names[quantities[i_q]+'_'+attributes[j_a]] if (
                quantities[i_q]+'_'+attributes[j_a] in lookup_names) else quantities[i_q]+'_'+attributes[j_a])
            
            if 'sur' in codename:
                if quantities[i_q]+'_'+attributes[j_a] in ref_data.columns:
                    min_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].min()
                    max_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].max()
                    ax.hlines(y=min_val, xmin=0, xmax=n_timesteps, color='r',
                            linestyle='--', label='bounds of the training dataset')
                    ax.hlines(y=max_val, xmin=0, xmax=n_timesteps,
                            color='r', linestyle='--')
            
            ax.legend(loc='best')
            fig.savefig(save_fold_name+'res_'+codename+'_' +
                        quantities[i_q]+'_'+attributes[j_a]+'_'+dates[0]+'_'+dates[-1]+'.pdf')
            plt.close()

            ### Plotting attribute profile for multiple time-steps
            
            n_timesteps_toplot = n_timesteps // 10 if n_timesteps // 10 > 0 else 1
            fig, ax = plt.subplots()

            timestep_iterator = range(0, n_timesteps, n_timesteps_toplot) # option 1: numbers of time steps
            # timestep_iterator = iter(times[0:n_timesteps:n_timesteps_toplot]) # option 2: times written to coreprof
            
            for i in timestep_iterator:

                ax.plot(np.array(coord_num),
                        data[i, :], 
                        label=f"t={times[i]}", 
                        color = fmt_list[i//n_timesteps_toplot][2],
                        linestyle = fmt_list[i//n_timesteps_toplot][1],
                        marker=fmt_list[i//n_timesteps_toplot][0],
                        #marker='.'
                        )
            
            # ax.set_yscale('symlog')
            ax.set_xlabel('rho coord')
            ax.set_ylabel(quantities[i_q]+'_'+attributes[j_a])
            ax.legend(loc='best')
            fig.savefig(save_fold_name+'prof_'+codename+'_' +
                        quantities[i_q]+'_'+attributes[j_a]+'_'+dates[0]+'_'+dates[-1]+'.pdf')
            plt.close()

    return dates

if __name__ == '__main__':
    
    dates = read_profs()

    #dates = ['20230818_135913', '20230821_161005', '20230822_150943', '20230823_151955', '20230824', '20230825', '20230828', '20230829', '20230830', '20230831', '20230901']
    #read_equil(dates)
