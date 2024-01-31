import sys
import os
import csv
import numpy as np
import pandas as pd
from itertools import product

import datetime
import time as t

import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

from ascii_cpo import read

sys.path.append('../uq/basicda')
from gem_da import profile_evol_load, profile_evol_plot
from extcodehelper import ExtCodeHelper


def read_attrib(filename, quantity, attribute, coords, filetype='coreprof'):
    """
    Returns list of attribute values, for every given coordinate number
    """
    cpo_obj = read(filename, filetype)
    q = cpo_obj
    
    if filetype == 'coretransp':
        q = cpo_obj.values[0]
    elif filetype == 'equilibrium' or 'coreprof':
        q = cpo_obj

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

    #TODO for the same type of CPOs, read a file once and return a dictionary of all quantities x attributes
    """
    # Block to read from a list foldername's
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

    file_names = [f for f in os.listdir(foldername) if # all files in the directory
                  os.path.isfile(os.path.join(foldername, f)) and # should be a file
                  f.endswith(file_ext) and # name should end with a particular extension e.g. .cpo
                  f.startswith(file_base_tocheck) # name should have particular prefix e.g. gem_coreprof_
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
        #TODO: iterate over a list of quantities
        a = read_attrib(foldername+f, quantity, attribute, coords, filetype)
        # print(f"a= {a}") ###DEBUG
        attributes.append(a)

        t = read_time(foldername+f, filetype)
        times.append(t)

    # List of lists to a numpy array
    atrributes_array = np.array(attributes)
    times_array = np.array(times)

    return atrributes_array, times_array

def read_equil_1d(foldername):
    """
    Reads %profiles_1d% from all equilibrium CPO files in a folder
    """

    n_coord = 100
    coords = [x for x in range(n_coord)]

    quantity = 'profiles_1d'
    attributes = ['q',]


    filetype = 'equilibrium'
    basename=''
    file_base_tocheck = basename+filetype+'_'
    file_ext = '.cpo'

    file_names = [f for f in os.listdir(foldername) if # all files in the directory
                  os.path.isfile(os.path.join(foldername, f)) and # should be a file
                  f.endswith(file_ext) and # name should end with a particular extension e.g. .cpo
                  f.startswith(file_base_tocheck) # name should have particular prefix e.g. gem_coreprof_
                  ]
    
    file_names.sort()

    # add the last file, that might be named differently
    last_file_tent_name = 'ets_'+filetype+'_out'+file_ext
    if os.path.exists(os.path.join(foldername, last_file_tent_name)):
        file_names.append(last_file_tent_name)

    attribute_vals = []
    times = []

    for f in file_names:
        for attribute in attributes:
            v = read_attrib(foldername+f, quantity, attribute, coords, filetype)
            # print(f"v= {v}") ###DEBUG
            attribute_vals.append(v)

        t = read_time(foldername+f, filetype)
        times.append(t)

    # List of lists to a numpy array
    atrributes_array = np.array(attribute_vals)
    times_array = np.array(times)

    return atrributes_array, times_array

def read_equil(foldernames):
    """
    Reads equilibrium file and plots last closed flux surface 
    foldername: list of names of form: [year-month-day] e.g. 20230901
    """

    # TODO iterate over files in folder, read %profiles_1d% !!!

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

def plot_quantities(datadict, save_fold_name, times=None, coord_num_fts=[14,30,43,55,66,76,85,95], bool_sur_involved=False, n_run_per_ft=81, ref_data=None,):
    """
    Saves plots for a tensor square of set of all quantities read during simulation
    If bool_sur_involved, then ref_data should be passed to add lines of bounds of quantities extrema values occuring in a different simualtions
    """

    ref_option = 'ft_col'

    with PdfPages(save_fold_name+'quant_'+codename+'_'+dates[0]+'_'+dates[-1]+'.pdf') as pdf:
    
        # Iterate over all pairs of quantity names
        for q_x,q_y in product(datadict.keys(), datadict.keys()):

            #print(f"- datadict shapes: {datadict[q_x].shape} {datadict[q_y].shape}") ###DEBUG

            #n_fts = min(datadict[q_x].shape[-1], datadict[q_y].shape[-1])
            n_fts = 8 # extract that's it 8 flux tubes from 100 rho coordinates :)

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

                if times is not None:
                    ax.plot(datadict[q_x][:t_min,coord_x], 
                            datadict[q_y][:t_min,coord_y],
                            c=times[:t_min],
                            marker='.',
                            cmap='viridis',
                            )
                else:
                    ax.plot(datadict[q_x][:t_min,coord_x], 
                            datadict[q_y][:t_min,coord_y],
                            marker='.',
                            )

                # Add vertical and horisontal lines from a reference dataset
                if bool_sur_involved:

                    if q_y in ref_data.columns:
                        
                        if ref_option == 'ft_col':
                            mask = ref_data['ft'] == n_ft
                            y_min_val = ref_data[mask][q_y].min()
                            y_max_val = ref_data[mask][q_y].max()
                        else:
                            y_min_val = ref_data[q_y].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].min()
                            y_max_val = ref_data[q_y].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].max()
                        
                        ax.hlines(y=y_min_val, xmin=datadict[q_x][:,coord_x].min(), xmax=datadict[q_x][:,coord_x].max(), 
                                color='r', linestyle='--', label='bounds of the training dataset')
                        ax.hlines(y=y_max_val, xmin=datadict[q_x][:,coord_x].min(), xmax=datadict[q_x][:,coord_x].max(),
                                color='r', linestyle='--')
                    
                    if q_x in ref_data.columns:
                        
                        if ref_option == 'ft_col':
                            mask = ref_data['ft'] == n_ft
                            x_min_val = ref_data[mask][q_x].min()
                            x_max_val = ref_data[mask][q_x].max()
                        else:
                            x_min_val = ref_data[q_x].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].min()
                            x_max_val = ref_data[q_x].iloc[n_run_per_ft*(n_ft):n_run_per_ft*(n_ft+1)].max()
                    
                        ax.vlines(x=x_min_val, ymin=datadict[q_y][:,coord_y].min(), ymax=datadict[q_y][:,coord_y].max(), 
                                color='r', linestyle='--',)
                        ax.vlines(x=x_max_val, ymin=datadict[q_y][:,coord_y].min(), ymax=datadict[q_y][:,coord_y].max(),
                                color='r', linestyle='--')

                ax.set_xlabel(q_x)
                ax.set_ylabel(q_y)
                ax.set_title(f"Plots for {q_x} vs {q_y} @f.t.#{n_ft}")

                #fig.savefig(save_fold_name+'quant_'+codename+'_' +q_x+'_'+q_y+'_'+'ft'+str(n_ft)+'_'+dates[0]+'_'+dates[-1]+'.pdf')
                pdf.savefig(fig)
                plt.close(fig)

        d = pdf.infodict()
        d['Title'] = 'Plots of all quantities'
        d['Author'] = u'Yehor Yudin'
        d['CreationDate'] = datetime.datetime.today()
    
    return 0

def write_table_csv(datadict, save_fold_name, coord_num_fts=[14,30,43,55,66,76,85,95]):
    """
    Writes a table to a CSV file, where columns are quantities at a particular flux tube and row are time readings
    """

    n_fts = 8
    
    # dictionary of correspondance of local quantity naming to the one used by Onnie Luk
    dict_naming_map ={
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

    dict_naming_numbers = {
        "te_value": 0,
        "ti_value": 1,
        "te_ddrho": 2,
        "ti_ddrho": 3,
        "te_transp_flux": 4,
        "ti_transp_flux": 5,
        "te_transp_diff_eff": 6,
        "ti_transp_diff_eff": 7,
        "te_transp_vconv_eff": 8,
        "ti_transp_vconv_eff": 9,
    }

    t_max = max([v.shape[0] for v in datadict.values()])

    #datadict_table = {k+'_'+str(i%len(coord_num_fts)):vi for vi in v.T for k,v in datadict.items()}

    data_table_dict ={}
    data_table = np.zeros((t_max, len(datadict.keys())*n_fts))

    # Iterate over quantities - over flux tube - over time readings
    # write data into np.array[(#quantitites * #flux-tubes) x #readings] and to a dictionary{(quantities x flux-tubes):readings}
    for i_q,(key,val) in enumerate(datadict.items()): #.items() are not ordered!

        if key in dict_naming_numbers:

            for n_ft in range(n_fts):

                data_table_dict[dict_naming_map[key]+str(n_ft+1)] = np.zeros(val.shape[0])

                if 'transp' in key:
                    coord = n_ft
                else:
                    coord = coord_num_fts[n_ft]

                for it in range(val.shape[0]):

                    data_table[it, dict_naming_numbers[key]*n_fts+n_ft] = val[it,coord]
                    data_table_dict[dict_naming_map[key]+str(n_ft+1)][it] = val[it,coord]

    # write CSV file with header of quantities x flux-tubes spearated by 2 spaces
    with open(save_fold_name+'res_'+codename+'.csv', 'w') as csv_file:

        writer = csv.writer(csv_file, delimiter=' ')
        writer.writerow(data_table_dict.keys())

        for r in data_table:

            writer.writerow(r)
    
    return 0 

def compare_transp(datadict, save_fold_name, times, input_folder_base='', coord_num_fts=None, option=1):
    """
    Takes a sequence of profile values and estimates transport using several models
    """
    # option 0 - model is called for all flux tube using a dictionary of core profiles (GEM flux tube rho_tor_norm locations are used)
    # option 1 - model is called per flux tube (single flux tube was used, custom rho_tor_norm for flux tubes were defined)
    # option 2 - model is called for all flux tubes using CPO objects (GEM flux tube rho_tor_norm locations are used)

    q_profile_list = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    q_transp_list = ['te_transp_flux', 'ti_transp_flux']

    if option == 1:
        #coords = [0.14, 0.31, 0.44, 0.56, 0.67, 0.77, 0.86, 0.95]
        # actually, both surrogate and code should write flux value on 'transport' grid
        coords = [0.143587306141853 , 0.309813886880875 , 0.442991137504578 , 0.560640752315521 , 0.668475985527039 , 0.769291400909424 , 0.864721715450287 , 0.955828309059143 ] #TODO double check
    else:
        coords = [0.143587306141853 , 0.309813886880875 , 0.442991137504578 , 0.560640752315521 , 0.668475985527039 , 0.769291400909424 , 0.864721715450287 , 0.955828309059143 ]
    
    if option == 0 or option == 2:
        xml_file = 'gem0.xml'
    else:
        xml_file = 'gem0_1ft.xml'

    if coord_num_fts is None:
        coord_num_fts = [x-1 for x in [15, 31, 44, 56, 67, 77, 86, 96]]

    n_fts = [i for i in range(len(coords))]
    #coords = coord_num_fts[:] # if coords are in coretransp notation i.e. flux tube numbers

    # due to different naming for the final state
    del_tstep = -1            
    #t_min = min(len(datadict[q_x][:,n_ft]), len(datadict[q_y][:,n_ft]))
    t_min = len(times) + del_tstep

    # initialise a model and its evaluation - here: GEM0
    model = ExtCodeHelper(2, xml_file=xml_file)

    # choose how to call GEM0
    if option == 0:
        # option 0: pass te,ti,te_ddrho,ti_ddrho at 1D grids of length 100
        model_call = lambda x: model.gem0_call_4param2target_fullarray(x)
    elif option == 1:
        # option 1: pass te,ti,te_ddrho,ti_ddrho and rho_tor_norm
        model_call = lambda x,rho_ind,rho: model.gem0_call_4param2target_array([x], [rho_ind], rho)
    elif option == 2:
        # option 2: pass equilibrium, coreprof, coretransp
        model_call = lambda eq,prof,transp: model.gem0_call_4param2target_cpo(eq,prof,transp)

    profile_corrected = np.zeros((len(q_profile_list), len(times), len(coords)))
    transp_new        = np.zeros( (len(q_transp_list), len(times), len(coords)))

    # for reading from CPO files directly
    if option == 2:
        input_folder_suffix = '' #'/instances/transport/workdir/'
        input_folder = input_folder_base + input_folder_suffix
        eq_filename = 'equilibrium_'
        prof_filename = 'coreprof_'
        transp_filename = 'coretransp_'
        cpo_extension='.cpo'

    # Iterate over the time step indices
    for t_ind in range(t_min):

        # for option 0: use a single call to pyGEM0, pass entire coreprof and get eniter coretransp array
        if option == 0:

            gem0_dict = {k:datadict[k][t_ind,:] for k in q_profile_list}

            tr, pr = model_call(gem0_dict)

            transp_new[:, t_ind, :] = tr

        # for option 1: call for given values, per flux tube
        if option == 1:

            for n_ft, rho_ind, rho in zip(n_fts, coord_num_fts, coords):

                input_data = np.array([datadict[q_profile][t_ind, rho_ind] for q_profile in q_profile_list])

                #print(f"input_data: {input_data}") ###DEBUG
                retval = model_call(input_data, rho_ind, rho) #option 1   
                #print(f"retval: {retval}") ###DEBUG
                
                transp_new[:, t_ind, n_ft] = retval[0][0][0]
                profile_corrected[:, t_ind, n_ft] = retval[1][0][0].reshape(-1)
                
                #print(f"transp_new: {transp_new[:, t_ind, n_ft]}") ###DEBUG

        # for option 2: pass the CPO objects
        if option == 2:

            eq = read(input_folder+eq_filename+str(t_ind).zfill(5)+cpo_extension, 'equilibrium')
            prof = read(input_folder+prof_filename+str(t_ind).zfill(5)+cpo_extension, 'coreprof')
            transp = read(input_folder+transp_filename+str(t_ind).zfill(5)+cpo_extension, 'coretransp')

            tr, pr, coret = model_call(eq, prof, transp) # option 2
            #print(f"tr={tr}") ###DEBUG
            transp_new[:, t_ind, :] = tr
        
    ### Plot transp fluxes vs time for two models alongside each other

    # Get a list of transp value for two models [{transp_quantity:np.array(times)}]
    transp_list = []
    transp_list.append({q: datadict[q] for q in q_transp_list})
    transp_list.append({'te_transp_flux': transp_new[0,:,:], 'ti_transp_flux': transp_new[1,:,:]})

    with PdfPages(save_fold_name+'transp_'+codename+'_'+dates[0]+'_'+dates[-1]+'.pdf') as pdf_transp: 
        
        for n_ft in n_fts:

            for i_q, q_transp in enumerate(q_transp_list):
        
                fig,ax = plt.subplots()

                for n_m,transp_vals in enumerate(transp_list):
                
                    ax.plot(times, transp_vals[q_transp][:,n_ft], label=f"model#{n_m} @ft#{n_ft}", marker='.', alpha=0.25)

                ax.set_xlabel('time, s')
                ax.set_ylabel(q_transp)
                ax.legend(loc='best')
                ax.set_title(f"Plots for {q_transp} @f.t.#{n_ft} (m0 is original, m1 is pyGEM0)")

                pdf_transp.savefig(fig)

    return 0

def plot_prof_time(data, pdf_prof, coord_rho_tor_norm, coords_transp_gem, times, n_timesteps, fmt_list, lookup_names, quantity_name, attribute_name):

    ### Plotting attribute profile for multiple time-steps
    
    n_timesteps_toplot = n_timesteps // 10 if n_timesteps // 10 > 0 else 1
    fig, ax = plt.subplots()

    timestep_iterator = range(0, n_timesteps, n_timesteps_toplot) # option 1: numbers of time steps
    # timestep_iterator = iter(times[0:n_timesteps:n_timesteps_toplot]) # option 2: times written to coreprof
    
    for i in timestep_iterator:

        ax.plot(
                #np.array(coord_num)/100.,
                coord_rho_tor_norm,
                data[i, :], 
                label=f"t={times[i]},s", 
                color = fmt_list[i//n_timesteps_toplot][2],
                linestyle = fmt_list[i//n_timesteps_toplot][1],
                marker=fmt_list[i//n_timesteps_toplot][0],
                alpha=0.5,
                #marker='.'
                )

    # Adding vertical lines to indicate the flux tube locations                      
    ax.vlines(x=coords_transp_gem, ymin=data.min(), ymax=data.max(), color='k', linestyle='--', label='flux tubes')
    
    # ax.set_yscale('symlog')
    ax.set_xlabel(lookup_names['rho'])
    ax.set_ylabel(lookup_names[quantity_name+'_'+attribute_name] if quantity_name+'_'+attribute_name in lookup_names else quantity_name+'_'+attribute_name)
    ax.legend(loc='best')

    #fig.savefig(save_fold_name+'prof_'+codename+'_' +quantities[i_q]+'_'+attributes[j_a]+'_'+dates[0]+'_'+dates[-1]+'.pdf')
    pdf_prof.savefig(fig)
    plt.close(fig)

    return 0

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
        "ti_transp_flux": "$Q_{{i}}$, W/m^{{2}}",
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
        "q": "$q$",
        "gm3" : "$gm^{{3}}$",
    }

    lookup_units = {
        "ti_value": "$eV$",
        "te_value": "$eV$",
        "ti_ddrho": "$eV/m$",
        "te_ddrho": "$eV/m$",
        "te_transp_flux": "$W/m^{{2}}$",
        "ti_transp_flux": "$W/m^{{2}}",
        "rho": "",
        "q" : "",
        "gm3" : "",
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
            attributes = ['q' , 'gm3']

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
    compare_transp(datadict, save_fold_name, times, input_folder_base=load_fold_names[0], coord_num_fts=coord_num_fts, option=2) 
    print(f"> time to plot transport for two models is: {t.time()-st:.3f} s")

    ### Plotting all quantities against each other
    if bool_sur_involved:
        ### ATTENTION - commented out
        pass
        #st = t.time()
        #plot_quantities(datadict=datadict, save_fold_name=save_fold_name, coord_num_fts=coord_num_fts, bool_sur_involved=bool_sur_involved, n_run_per_ft=n_run_per_ft, ref_data=ref_data)
        #print(f"> time to plot quantities against each other is: {t.time()-st:.3f} s")

    ### Write down a single csv for simulation results
    st = t.time()
    write_table_csv(datadict=datadict, save_fold_name=save_fold_name)
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
        # NOW WOULDN'T WORK!
        ref_data_filename = 'ref_train_data_5000.csv'
    else:
        ref_data_filename = sys.argv[-1]
    ref_data_filename = 'ref_train_data_5000.csv'

    print(f"> Now postprocessing a MUSCLE3 workflow run")
    
    st = t.time()

    cpo_names = ['coreprof', 'coretransp',] # 'equilibrium']
    dates = read_profs(codename=codename, dates=dates, ref_data_filename=ref_data_filename, cpo_names=cpo_names) # to read results of M3-WF run

    print(f"Postrprocessing finished, totally took {t.time()-st:.3f} s")

    # some set of first GEM-ETS-CHEASE runs with MUSCLE3
    #dates = ['20230818_135913', '20230821_161005', '20230822_150943', '20230823_151955', '20230824', '20230825', '20230828', '20230829', '20230830', '20230831', '20230901']
    #dates = ['20230918', '20230922']

    # plotting for standalone GEM to get good initial turbulence snapshots
    #dates = ['20230928', '20230929', '20231004', '20231006', '20231009', '20231010', '20231016',]# '20231017']
    #dates = read_profs(codename='', dates=dates, prefix_name='../standalone/bin/gem_develop_turbulence/', sufix_name='/', cpo_filebase='gem_', cpo_names=['coretransp']) # to read from stanalone runs

    # plotting LCFS for the same runs
    #read_equil(dates)

