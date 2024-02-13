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
from muscle_utils.utils import coreprof_to_input_value, read_files, read_equil_1d, plot_quantities, write_table_csv, compare_transp, plot_prof_time
#src.utils:q

def read_profs(codename='gem_', dates=['20230823_151955'], prefix_name='workflow/run_fusion_', sufix_name='/instances/transport/workdir/', cpo_filebase='', cpo_names = ['coreprof', 'coretransp'], **kwargs):

    load_fold_names = [prefix_name + codename + \
            date + sufix_name for date in dates]

    save_fold_name = prefix_name+codename+dates[0]+'_'+dates[-1]+'/'

    if not os.path.exists(save_fold_name):
        os.makedirs(save_fold_name)
    
    n_rho_resol = kwargs['n_rho_resol'] if 'n_rho_resol' in kwargs else 1

    ref_option = kwargs['ref_option'] if 'ref_option' in kwargs else 'ft_col'

    bool_sur_involved = ('sur' or 'multiimpl' or 'manager') in codename 
    # ('sur' in codename) or ('multiimpl' in codename) or ('manager' in codename)
    bool_sur_involved = True

    # Load reference data for the surrogate training data set

    if bool_sur_involved:
        
        # # option 1 for reference data: 8*(3**4) samples
        # ref_data_filename = 'ref_train_data.csv'
        # n_run_per_ft = 81

        # # option 2 for reference data: 8*(5**4) samples
        # ref_data_filename = 'ref_train_data_5000.csv'
        # n_run_per_ft = 625

        # option 3 for reference data: LHC sample
        ref_data_filename = kwargs['ref_data_filename'] if 'ref_data_filename' in kwargs else 'ref_train_data_5000.csv'
        n_run_per_ft = 1500

        ref_data = pd.read_csv(ref_data_filename, sep=',')

    lookup_names = {
        "ti_value": "$T_{{i}}, eV$",
        "te_value": "$T_{{e}}, eV$",
        "ti_ddrho": "$\\nabla T_{{i}}, eV/m$",
        "te_ddrho": "$\\nabla T_{{e}}, eV/m$",
        "te_transp_flux": "$Q_{{e}}, W/m^{{2}}$",
        "ti_transp_flux": "$Q_{{i}}$, W/m^{{2}}$",
        "rho": "$\\rho_{{tor}}^{{norm}}$",
        "q" : "$q$",
        "gm3" : "$gm^{{3}}$",
    }

    lookup_names_short = {
        "ti_value": "$T_{{i}}$",
        "te_value": "$T_{{e}}$",
        "ti_ddrho": "$\\nabla T_{{i}}$",
        "te_ddrho": "$\\nabla T_{{e}}$",
        "te_transp_flux": "$Q_{{e}}$",
        "ti_transp_flux": "$Q_{{i}}$",
        "rho": "$\\rho_{{tor}}^{{norm}}$",
        "profiles_1d_q": "$q$",
        "profiles_1d_gm3" : "$gm^{{3}}$",
    }

    lookup_units = {
        "ti_value": "$eV$",
        "te_value": "$eV$",
        "ti_ddrho": "$eV/m$",
        "te_ddrho": "$eV/m$",
        "te_transp_flux": "$W/m^{{2}}$",
        "ti_transp_flux": "$W/m^{{2}}$",
        "rho": "",
        "profiles_1d_q" : "",
        "profiles_1d_gm3" : "",
    }

    color_step = 0.08
    color_list = ['b', 'g', 'r', 'y' , 'm', 'c', 'k']
    color_grad_list = [(1.-color_step*i, 0.0, color_step*i) for i in range(1,12)]
    line_list = ['-', '--', '-.', ':']
    marker_list = ['', '.', 'o', 'v', '^', '<', '>']
    style_lists = [marker_list, line_list, color_grad_list,] 
    fmt_list = [style for style in product(*style_lists)]

    coord_num_fts = [14, 30, 43, 55, 66, 76, 85, 95]
    coords_transp_gem = [0.143587306141853, 0.309813886880875, 0.442991137504578, 0.560640752315521, 0.668475985527039, 0.769291400909424, 0.864721715450287, 0.955828309059143]

    times_coreprof = []

    datadict = {}

    for cpo_name in cpo_names:

        if cpo_name == 'coreprof':

            #quantities = ['ti', 'te', 'ni', 'ne', ]
            quantities = ['ti', 'te',]

            attributes = ['value', 'ddrho', ]

            #coord_num = [68]
            coord_num = [x for x in range(0, 100, n_rho_resol)]

            #TODO: not true, should be read from CPO %rho_tor_norm !
            coord_rho_tor_norm = np.array(coord_num)/100.

            #n_ft = 68
            #n_ft = coord_num[0]
            n_ft_transp = 0 # number of flux tube of interest
            n_ft = coord_num_fts[n_ft_transp] # plot profiles for the innermost flux tube 
            n_fts = coord_num_fts

            #i_q_s = [0, 1, 2, 3] # indiced of quantities to go through
            i_q_s = [0, 1,] # indiced of quantities to go through
            j_a_s = [0, 1] # indiced of attributes to go through

        if cpo_name == 'coretransp':

            quantities = ['te_transp', 'ti_transp',]

            attributes = ['flux', 'diff_eff', 'vconv_eff', ]

            #if codename == 'gem_surr':
            if 'sur' in codename:
                #coord_num = [0]  # if n_fts==1
                coord_num = [0, 1, 2, 3, 4, 5, 6, 7] 
                n_ft = coord_num[-1]
            else:
                coord_num = [0, 1, 2, 3, 4, 5, 6, 7]  # if n_fts==8
                n_ft = coord_num[-1] #0 #4 # number of flux tube to plot for
            n_fts = coord_num

            coord_rho_tor_norm = coords_transp_gem

            i_q_s = [0, 1] # indices of quantities to go through
            j_a_s = [0, 1, 2] # indices of attributes to go through

        if cpo_name == 'equilibrium':

            quantities = ['profiles_1d']
            attributes = ['q', 'gm3']

            coord_num = [x for x in range(0, 100, n_rho_resol)]
            coord_rho_tor_norm = np.array(coord_num)/100.
            n_fts = coord_num_fts

            i_q_s = [0,] 
            j_a_s = [0, 1]

        # Create a plot for all quantities for this CPO type
        common_fig_ax_list = [plt.subplots() for _ in n_fts]

        with PdfPages(save_fold_name+'res_'+codename+'_'+cpo_name+'_'+dates[0]+'_'+dates[-1]+'.pdf') as pdf_evol, \
             PdfPages(save_fold_name+'prof_'+codename+'_'+cpo_name+'_'+dates[0]+'_'+dates[-1]+'.pdf') as pdf_prof:

            for j_a, i_q in product(j_a_s, i_q_s): # could be just [quantities]x[attributes] instead

                # Reading data of a particular attribute for all files produced by transport codes
                # TODO: better, first read all data into a single data structure e.g. pandas dataframe, accessing each file once; then plot 
                st = t.time()
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
                #data = np.genfromtxt(data_file_name, delimiter=',')[::2].reshape(-1, 1) # ::2 due to two calls of printing from e.g. GEM0

                print(f"Reading CPOs and writing CSVs for {quantities[i_q]}.{attributes[j_a]} took {t.time()-st:.3f} s")

                # Read the dimensions: time and radial coordiante
                #print(f"data shape: {data.shape}") ###DEBUG
                n_timesteps = data.shape[0]
                #print(f"size of data and time readings is the same: {n_timesteps == times.shape[0]}") ###DEBUG
                #n_rhos = data.shape[1]

                # Coretransp does not change timestamp correctly, so prefereably use transport (coreprof) time
                if cpo_name == 'coreprof': 
                    times_coreprof = times
                    n_timereadings = len(times_coreprof)
                elif cpo_name == 'coretransp' or 'equilibrium':
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
                #print(f"> delta-t array for {quantities[i_q]+'_'+attributes[j_a]}: \n{times[1:]-times[:-1]}") ###DEBUG

                ### Plotting an attribute value at a the n_ft-th flux tube against time
                
                # Iterate over the flux tube locations
                for k,n_ft in enumerate(n_fts):

                    n_ft_transp = k

                    #fig, ax = plt.subplots()
                    
                    # - option 1: sequential number of time steps starting from 0
                    #x_values = np.linspace(0, n_timesteps, n_timesteps)
                    # - option 2: timestamp read from coreprofile CPO files
                    x_values = times

                    ### Combine scalar-vs-time plots for different quantities and same flux tube
                    ax_loc = common_fig_ax_list[k][1] #.twinx()
                    fig_loc = common_fig_ax_list[k][0]

                    # Plot the bound of the reference data for this parameter (horisontal lines)
                    if bool_sur_involved:
                        if quantities[i_q]+'_'+attributes[j_a] in ref_data.columns:
                            #print(f"Plotting bounds for {quantities[i_q]+'_'+attributes[j_a]}") ###DEBUG

                            if ref_option == 'ft_col':
                                mask = ref_data['ft'] == n_ft_transp
                                min_val = ref_data[mask][quantities[i_q]+'_'+attributes[j_a]].min()
                                max_val = ref_data[mask][quantities[i_q]+'_'+attributes[j_a]].max()
                            else:
                                min_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].iloc[n_run_per_ft*(n_ft_transp):n_run_per_ft*(n_ft_transp+1)].min()
                                max_val = ref_data[quantities[i_q]+'_'+attributes[j_a]].iloc[n_run_per_ft*(n_ft_transp):n_run_per_ft*(n_ft_transp+1)].max()
                            
                            #median_val = (max_val + min_val) / 2.
                            diff_val = max_val - min_val

                            #print(f"> For {quantities[i_q]+'_'+attributes[j_a]} @ft#{k}: min={min_val}, max={max_val}") ###DEBUG
                        
                            ax_loc.hlines(y=(min_val-min_val)/diff_val, xmin=x_values[0], xmax=x_values[-1], color=color_list[(i_q*len(j_a_s) + j_a)%(len(color_list)-1)],
                                            linestyle='--',) # label=f"bounds of the training dataset")
                            ax_loc.hlines(y=(max_val-min_val)/diff_val, xmin=x_values[0], xmax=x_values[-1], color=color_list[(i_q*len(j_a_s) + j_a)%(len(color_list)-1)], 
                                            linestyle='--',)
                            
                            # Find intersection of the plot and bounds of the training dataset and add them as vertical lines
                            x_inters_ind = min([np.min(np.where(data[:n_timesteps, n_ft] < min_val)[0], initial=len(x_values)-1),
                                                np.min(np.where(data[:n_timesteps, n_ft] > max_val)[0], initial=len(x_values)-1)]) # TODO: add check for no intersection
                            ax_loc.vlines(x=x_values[x_inters_ind], ymin=0., ymax=1., color=color_list[(i_q*len(j_a_s) + j_a)%(len(color_list)-1)], 
                                            linestyle='--',)
                    
                        else:
                            min_val = 0.0
                            max_val = data[:n_timesteps, n_ft].max()
                            #median_val = (max_val + min_val) / 2.
                            diff_val = 1.0

                    ax_loc.plot(
                            x_values,
                            (data[:n_timesteps, n_ft] - min_val) / diff_val, 
                            marker='.',
                            color=color_list[(i_q*len(j_a_s) + j_a)%(len(color_list)-1)],
                            alpha=0.5,
                            label=f"{lookup_names_short[quantities[i_q]+'_'+attributes[j_a]] if (quantities[i_q]+'_'+attributes[j_a] in lookup_names_short) else quantities[i_q]+'_'+attributes[j_a]}, ${min_val:.2f}+{diff_val:.2f} \\cdot x$, {lookup_units[quantities[i_q]+'_'+attributes[j_a]] if (quantities[i_q]+'_'+attributes[j_a] in lookup_units) else ''}",
                            # , {lookup_names_short['rho']}={n_ft/100.}",
                                )

                    # Set up axis name, title, legend etc.
                    #ax_loc.set_ylabel(lookup_names[quantities[i_q]+'_'+attributes[j_a]] if (
                    #     quantities[i_q]+'_'+attributes[j_a] in lookup_names) else quantities[i_q]+'_'+attributes[j_a])
                    
                ### Plotting attribute profile for multiple time-steps
                st = t.time()
                plot_prof_time(data, pdf_prof, coord_rho_tor_norm, coords_transp_gem, times, n_timesteps, fmt_list, lookup_names, quantities[i_q], attributes[j_a])
                print(f"Plotting entire profiles with time for {quantities[i_q]}.{attributes[j_a]} took {t.time()-st:.3f} s")

        # Due to first iterating over quantities and then over flux-tube, saving block has to be separately
        with PdfPages(save_fold_name+'res_'+codename+'_'+cpo_name+'_'+dates[0]+'_'+dates[-1]+'.pdf') as pdf_evol:
            for ift,(f,a) in enumerate(common_fig_ax_list):
                # TODO: display integer numbers of time steps
                a.legend(loc='best')
                a.set_xlabel('${{t}}$, s')
                a.set_title(f"Plots for {cpo_name} @f.t.#{ift} ({lookup_names_short['rho']}={n_fts[ift]/100.})")

                #f.savefig(save_fold_name+'res_'+codename+'_'+cpo_name+'_common_nft'+str(ift)+'_'+dates[0]+'_'+dates[-1]+'.pdf')
                pdf_evol.savefig(f)
                plt.close(f)

    ### Plotting transport fluxes for two models
    # - option 0 should use the whole profile to interpolate on coretransp grid later
    # - option 1 should use calls to pyGEM0 per coreprof point
    # - option 2 should use all CPO files from /workdir/ - prefered, includes changing equilibrium !
    st = t.time()
    compare_transp(datadict, save_fold_name, codename, dates, times, input_folder_base=load_fold_names[0], coord_num_fts=coord_num_fts, option=2, modeltype='gem0py') 
    print(f"> time to plot transport for two models is: {t.time()-st:.3f} s")

    ### Plotting all quantities against each other
    if bool_sur_involved:
        ### ATTENTION - commented out
        pass
        #st = t.time()
        #plot_quantities(datadict=datadict, save_fold_name=save_fold_name, codename=codename, dates=dates, coord_num_fts=coord_num_fts, bool_sur_involved=bool_sur_involved, n_run_per_ft=n_run_per_ft, ref_data=ref_data)
        #print(f"> time to plot quantities against each other is: {t.time()-st:.3f} s")

    ### Write down a single csv for simulation results
    st = t.time()
    write_table_csv(datadict=datadict, save_fold_name=save_fold_name, codename=codename)
    print(f"> time to write CSVs down is: {t.time()-st:.3f} s")

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

    #########
    # some set of first GEM-ETS-CHEASE runs with MUSCLE3
    #dates = ['20230818_135913', '20230821_161005', '20230822_150943', '20230823_151955', '20230824', '20230825', '20230828', '20230829', '20230830', '20230831', '20230901']
    #dates = ['20230918', '20230922']

    # plotting for standalone GEM to get good initial turbulence snapshots
    #dates = ['20230928', '20230929', '20231004', '20231006', '20231009', '20231010', '20231016',]# '20231017']
    #dates = read_profs(codename='', dates=dates, prefix_name='../standalone/bin/gem_develop_turbulence/', sufix_name='/', cpo_filebase='gem_', cpo_names=['coretransp']) # to read from stanalone runs

    # plotting LCFS for the same runs
    #read_equil(dates)
