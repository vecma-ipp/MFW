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

def read_files(foldername, quantity, attribute, coords, filetype='coreprof', date='', basename=''):

    file_base_tocheck = basename+filetype+'_'
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

def plot_quantities(datadict, save_fold_name, coord_num_fts=[14,30,43,55,66,76,85,95], bool_sur_involved=False, n_run_per_ft=81, ref_data=None,):
    """
    Saves plots for a tensor square of set of all quantities read during simulation
    If bool_sur_involved, then ref_data should be passed to add lines of bounds of quantities extrema values occuring in a different simualtions
    """
    
    # Iterate over all pairs of quantity names
    for q_x,q_y in product(datadict.keys(), datadict.keys()):

        #print(f"- datadict shapes: {datadict[q_x].shape} {datadict[q_y].shape}") ###DEBUG

        #n_fts = min(datadict[q_x].shape[-1], datadict[q_y].shape[-1])
        n_fts = 8 # extract that's it 8 flux tube from 100 rho coordinates :)

        # Iterate over all flux tubes
        for n_ft in range(n_fts):
        
            fig, ax = plt.subplots()

            t_min = min(len(datadict[q_x][:,n_ft]), len(datadict[q_y][:,n_ft]))

            if 'transp' in q_x:
                coord_x = n_ft
            else:
                coord_x = coord_num_fts[n_ft]
            
            if 'transp' in q_y:
                coord_y = n_ft
            else:
                coord_y = coord_num_fts[n_ft]

            ax.plot(datadict[q_x][:t_min,coord_x], 
                    datadict[q_y][:t_min,coord_y],
                    marker='.',
                    )

            # Add vertical and horisontal lines from a reference dataset
            if bool_sur_involved:

                if q_y in ref_data.columns:
                    y_min_val = ref_data[q_y].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].min()
                    y_max_val = ref_data[q_y].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].max()
                    ax.hlines(y=y_min_val, xmin=datadict[q_x][:,coord_x].min(), xmax=datadict[q_x][:,coord_x].max(), 
                            color='r', linestyle='--', label='bounds of the training dataset')
                    ax.hlines(y=y_max_val, xmin=datadict[q_x][:,coord_x].min(), xmax=datadict[q_x][:,coord_x].max(),
                            color='r', linestyle='--')
                
                if q_x in ref_data.columns:
                    x_min_val = ref_data[q_x].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].min()
                    x_max_val = ref_data[q_x].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].max()
                
                    ax.vlines(x=x_min_val, ymin=datadict[q_y][:,coord_y].min(), ymax=datadict[q_y][:,coord_y].max(), 
                            color='r', linestyle='--',)
                    ax.vlines(x=x_max_val, ymin=datadict[q_y][:,coord_y].min(), ymax=datadict[q_y][:,coord_y].max(),
                            color='r', linestyle='--')

            ax.set_xlabel(q_x)
            ax.set_ylabel(q_y)
            fig.savefig(save_fold_name+'quant_'+codename+'_' +
                        q_x+'_'+q_y+'_'+'ft'+str(n_ft)+'_'+dates[0]+'_'+dates[-1]+'.pdf')
            plt.close()
    
    return 0

def write_table_csv(datadict, save_fold_name, coord_num_fts=[14,30,43,55,66,76,85,95]):
    """
    Writes a table to a CSV file, where columns are quantities at a particular flux tube and row are time readings
    """

    n_fts = 8
    
    # dictionary of correspondance of local quantity naming to the one used by Onnie Luk
    dict_naming_dict ={
        "te_transp_flux": "flux-Te-ft",
        "ti_transp_flux": "flux-Ti-ft",
        "te_transp_diff_eff": "diff-Te-ft",
        "ti_transp_diff_eff": "diff-Ti-ft",
        "te_transp_vconv_eff": "vconv-Te-ft",
        "ti_transp_vconv_eff": "vconv-Ti-ft",  
        "te_value": "Te-ft",
        "ti_value": "Ti-ft",
        "te_ddrho": "dTe-ft",
        "ti_ddrho": "dTi-ft",
        "ne_transp_flux": "flux-ne-ft",
        "ni_transp_flux": "flux-ni-ft",
        "ne_value": "ne-ft",
        "ni_value": "ni-ft",
        "ne_ddrho": "dne-ft",
        "ni_ddrho": "dni-ft",
    }

    t_max = max([v.shape[0] for v in datadict.values()])

    #datadict_table = {k+'_'+str(i%len(coord_num_fts)):vi for vi in v.T for k,v in datadict.items()}

    data_table_dict ={}
    data_table = np.zeros((t_max, len(datadict.keys())*n_fts))

    # Iterate over quantities - over flux tube - over time readings
    # write data into np.array[(#quantitites * #flux-tubes) x #readings] and to a dictionary{(quantities x flux-tubes):readings}
    for i_q,(key,val) in enumerate(datadict.items()):

        for n_ft in range(n_fts):

            data_table_dict[dict_naming_dict[key]+str(n_ft+1)] = np.zeros(val.shape[0])

            if 'transp' in key:
                coord = n_ft
            else:
                coord = coord_num_fts[n_ft]

            for it in range(val.shape[0]):

                data_table[it, i_q*n_fts+n_ft] = val[it,coord]
                data_table_dict[dict_naming_dict[key]+str(n_ft+1)][it] = val[it,coord]

    # write CSV file with header of quantities x flux-tubes spearated by 2 spaces
    with open(save_fold_name+'res_'+codename+'.csv', 'w') as csv_file:

        writer = csv.writer(csv_file, delimiter=' ')
        writer.writerow(data_table_dict.keys())

        for r in data_table:

            writer.writerow(r)
    
    return 0

def read_profs(codename='gem_', dates=['20230823_151955'], prefix_name='workflow/run_fusion_', sufix_name='/instances/transport/workdir/', cpo_filebase='', cpo_names = ['coreprof', 'coretransp']):

    load_fold_names = [prefix_name + codename + \
            date + sufix_name for date in dates]

    save_fold_name = prefix_name+codename+dates[0]+'_'+dates[-1]+'/'

    if not os.path.exists(save_fold_name):
        os.makedirs(save_fold_name)
    
    n_rho_resol = 1

    bool_sur_involved = 'sur' in codename or 'multiimpl' in codename
    bool_sur_involved = True

    # Load reference data for the surrogate training data set

    if bool_sur_involved:
        ref_data_filename = 'ref_train_data.csv'
        ref_data = pd.read_csv(ref_data_filename, sep=',')
        n_run_per_ft = 81

    lookup_names = {
        "ti_value": "$T_{{i}}$",
        "te_value": "$T_{{e}}$",
        "ti_ddrho": "$\\nabla T_{{i}}$",
        "te_ddrho": "$\\nabla T_{{e}}$",
        'te_transp.flux': "$Q_{{e}}$",
        'ti_transp.flux': "$Q_{{i}}$",
    }

    coord_num_fts = [14, 30, 43, 55, 66, 76, 85, 95]

    #cpo_names = ['coreprof', 'coretransp']

    times_coreprof = []

    datadict = {}

    for cpo_name in cpo_names:

        if cpo_name == 'coreprof':

            quantities = ['ti', 'te', 'ni', 'ne', ]

            attributes = ['ddrho', 'value', ]

            #coord_num = [68]
            coord_num = [x for x in range(0, 100, n_rho_resol)]

            #n_ft = 68
            #n_ft = coord_num[0]
            n_ft_transp = 0 # number of flux tube of interest
            n_ft = coord_num_fts[n_ft_transp] # plot profiles for the innermost flux tube 
            n_fts = coord_num_fts

            i_q_s = [0, 1, 2, 3] # indiced of quantities to go through
            j_a_s = [0, 1] # indiced of attributes to go through

        if cpo_name == 'coretransp':
            quantities = ['ti_transp', 'te_transp', ]

            attributes = ['flux', 'diff_eff', 'vconv_eff', ]

            if codename == 'gem_surr':
                #coord_num = [0]  # if n_fts==1
                coord_num = [0, 1, 2, 3, 4, 5, 6, 7] 
                n_ft = coord_num[-1]
            else:
                coord_num = [0, 1, 2, 3, 4, 5, 6, 7]  # if n_fts==8
                n_ft = coord_num[-1] #0 #4 # number of flux tube to plot for
            n_fts = coord_num

            i_q_s = [0, 1] # indices of quantities to go through
            j_a_s = [0, 1, 2] # indices of attributes to go through

        for i_q, j_a in product(i_q_s, j_a_s): # could be just [quantities]x[attributes] instead

            # Reading data of a particular attribute for all files produced by transport code
            # TODO: read data for many folders
            # TODO: better, first read all data into a single data structure e.g. pandas dataframe, accessing each file onse; then plot 
            data_list = []
            times_list = []
            for load_fold_name, date in zip(load_fold_names, dates):
                data, times = read_files(
                    load_fold_name, quantities[i_q], attributes[j_a], coords=coord_num, filetype=cpo_name, date=date, basename=cpo_filebase)
                data_list.append(data)
                times_list.append(times)
            data = np.concatenate(data_list, axis=0)
            times = np.concatenate(times_list)

            # Fill in a common datadict
            datadict[quantities[i_q]+'_'+attributes[j_a]] = data

            data_file_name = save_fold_name+'res_'+codename+'_' + \
                quantities[i_q]+'_'+attributes[j_a]+'_'+dates[0]+'_'+dates[-1]+'.csv'
            np.savetxt(data_file_name, np.array(data), delimiter=',')
            # data = np.genfromtxt(data_file_name, delimiter=',')[::2].reshape(-1, 1) # ::2 due to two calls of printing from e.g. GEM0

            # print(f"data shape: {data.shape}") #DEBUG
            n_timesteps = data.shape[0]
            #print(f"size of data and time readings is the same: {n_timesteps == times.shape[0]}") ###DEBUG
            n_rhos = data.shape[1]

            # Coretransp does not change timestamp correctly, so prefereably use transport (coreprof) time
            if cpo_name == 'coreprof': 
                times_coreprof = times
                n_timereadings = len(times_coreprof)
            elif cpo_name == 'coretransp':
                #times = times_coreprof # does not track which coreprof exactly provides time
                #times = np.linspace(0, n_timesteps, n_timesteps) # last coretransp file is absent (?)

                # Coreprof and Coretransp readings are of different length, so either reduce or enlarge time reading array
                #  here: extrapolate using the last delta-t
                if len(times_coreprof) > 1 :
                    times = times_coreprof[:n_timesteps] if n_timesteps <= n_timereadings else \
                        np.append(times_coreprof, np.arange( 2*times_coreprof[-1]- times_coreprof[-2], 
                            times_coreprof[-1] + (times_coreprof[-1] - times_coreprof[-2]) 
                            * (n_timesteps - n_timereadings + 1), times_coreprof[-1] - times_coreprof[-2] ))
                else:
                    times = np.linspace(0, n_timesteps, n_timesteps)
                
            # Display the array of actual ETS time step lengths
            #print(f"> delta-t array for {quantities[i_q]+'_'+attributes[j_a]}: \n{times[1:]-times[:-1]}")

            color_step = 0.07
            #color_list = ['b', 'g', 'r', 'y' , 'm', 'c', 'k']
            color_list = [(0.0, 1.-color_step*i, color_step*i) for i in range(1,10)]
            line_list = ['-', '--', '-.', ':']
            marker_list = ['', '.', 'o', 'v', '^', '<', '>']
            style_lists = [marker_list, line_list, color_list,] 
            fmt_list = [style for style in product(*style_lists)]

            ### Plotting an attribute value at a the n_ft-th flux tube against time
            
            # Iterate over the flux tube locations
            for k,n_ft in enumerate(n_fts):

                n_ft_transp = k

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
                
                if bool_sur_involved:
                    if quantities[i_q]+'_'+attributes[j_a] in ref_data.columns:
                        min_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].iloc[n_run_per_ft*(n_ft_transp):n_run_per_ft*(n_run_per_ft+1)].min()
                        max_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].iloc[n_run_per_ft*(n_ft_transp):n_run_per_ft*(n_run_per_ft+1)].max()
                        ax.hlines(y=min_val, xmin=x_values[0], xmax=x_values[-1], color='r',
                                linestyle='--', label='bounds of the training dataset')
                        ax.hlines(y=max_val, xmin=x_values[0], xmax=x_values[-1],
                                color='r', linestyle='--')
                
                ax.legend(loc='best')
                fig.savefig(save_fold_name+'res_'+codename+
                            quantities[i_q]+'_'+attributes[j_a]+'_ft'+str(n_ft)+'_'+dates[0]+'_'+dates[-1]+'.pdf')
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

    ### Plotting all quantities against each other

    plot_quantities(datadict=datadict, save_fold_name=save_fold_name, coord_num_fts=coord_num_fts, bool_sur_involved=bool_sur_involved, n_run_per_ft=n_run_per_ft, ref_data=ref_data)

    ### Write down a single csv for simulation results

    write_table_csv(datadict=datadict, save_fold_name=save_fold_name)

    return dates

if __name__ == '__main__':

    if len(sys.argv) < 2 :
        codename = 'gem_'
    else:
        codename = str(sys.argv[1])

    if len(sys.argv) < 3 :
        dates = ['20230823_151955']
    else:
        dates = sys.argv[2:]
    
    dates = read_profs(codename=codename, dates=dates) # to read results of M3-WF run


    # some set of first GEM-ETS-CHEASE runs with MUSCLE3
    #dates = ['20230818_135913', '20230821_161005', '20230822_150943', '20230823_151955', '20230824', '20230825', '20230828', '20230829', '20230830', '20230831', '20230901']
    #dates = ['20230918', '20230922']

    # plotting for standalone GEM to get good initial turbulence snapshots
    #dates = ['20230928', '20230929', '20231004', '20231006', '20231009', '20231010', '20231016']
    #dates = read_profs(codename='', dates=dates, prefix_name='../standalone/bin/gem_develop_turbulence/', sufix_name='/', cpo_filebase='gem_', cpo_names=['coretransp']) # to read from stanalone runs

    # plotting LCFS for the same runs
    #read_equil(dates)


