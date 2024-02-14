import sys
import os
import csv
import numpy as np
import pandas as pd
import time as t
import datetime
from itertools import product

import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.ticker as ticker

import easysurrogate as es

# import ual
# import base

from ascii_cpo import read #, write_fstream, read_fstream, write

uqbasicdadir = '/u/yyudin/code/MFW/uq/basicda/'
sys.path.append(uqbasicdadir)
from gem_da import profile_evol_load, profile_evol_plot
from da_utils import read_cpo_file
from extcodehelper import ExtCodeHelper

def training_data_bounds(ref_data, input_names=['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho'], n_fts=8, n_run_per_ft=81, option='ft_col'):
    """
    Produces a dictionary for training parameters, 
    each having an array of min and max values occuring in the dataset 
        option: whether to use 'ft' column in the reference data or not
    
    """

    #print(f"ref_data : \n{ref_data.describe()}")###DEBUG

    train_bounds = {k:{'min':np.zeros(n_fts), 'max':np.zeros(n_fts)} for k in input_names}

    for input in input_names:
        for i in range(n_fts):
            if option == 'ft_col':
                mask = ref_data['ft'] == i
                ref_data_ft = ref_data[mask]
                train_bounds[input]['min'][i] = ref_data_ft[input].min()
                train_bounds[input]['max'][i] = ref_data_ft[input].max()
            else:
                train_bounds[input]['min'][i] = ref_data[input].iloc[n_run_per_ft*(i):n_run_per_ft*(i+1)].min()
                train_bounds[input]['max'][i] = ref_data[input].iloc[n_run_per_ft*(i):n_run_per_ft*(i+1)].max()

    return train_bounds

def check_outof_learned_bounds(input, reference):
    """
    input: an array of input values for surrogate of size (n_features, n_coordinates) in order 'te_value', 'ti_value', 'te_ddrho', 'ti_ddrho'
    reference: a dictionary of training data bounds in format {feature:{'max':array(n_coords),'min':array(n_coords)}}
    Returns: bool: True if input is outisde of the learned bounds
    """

    #print(f"reference:\n{reference}") ###DEBUG
    #print(f"input:\n{input}") ###DEBUG

    input_order = {'te_value':0, 'ti_value':1, 'te_ddrho':2, 'ti_ddrho':3}

    bool_outofbounds = False

    dict_outofbounds = {}

    for i,(k,vs) in enumerate(reference.items()): # dict items are not ordered!
        
        dict_outofbounds[k] = {
                               'greater': np.zeros(vs['max'].shape, dtype=bool),
                               'lesser' : np.zeros(vs['min'].shape, dtype=bool), 
                               'within' :  np.ones(vs['min'].shape, dtype=bool),
                               } # OHE; technicaly, 'within' is redundant and we need only 2 bits to encode 3 classes

        for j,v in enumerate(vs['max']): # can use .tolist() to be sure array is converted into a generator

            if v < input[input_order[k],j]:
                
                bool_outofbounds = True
                dict_outofbounds[k]['greater'][j] = True
                dict_outofbounds[k]['within'][j]  = False
                
        for j,v in enumerate(vs['min']):

            if v > input[input_order[k],j]: 

                bool_outofbounds = True
                dict_outofbounds[k]['lesser'][j] = True
                dict_outofbounds[k]['within'][j] = False

    #print(f"dict_outofbounds:\n{dict_outofbounds}") ###DEBUG

    return bool_outofbounds, dict_outofbounds

def coreprof_to_input_value(
            data, 
            rho_ind_s=None, # used only if rho_tor_norm is None
            rho_tor_norm=None, # rho_tor_nor of coretransp flux tubes
            prof_names=['te', 'ti'], 
            attrib_names=['value', 'ddrho'],
                     ):
    """
    Transforms coreprof message data into values acceptable by a surrogate model as an input in order 'te_value', 'te_ddrho', 'ti_value', 'ti_ddrho'
    TODO: look up EasyVVUQ cpo element for coreprof
    TODO: check that all input values are filled with defaults
    TODO: check if there is an implementation in easyvvuq utils
    """

    n = len(prof_names)
    m = len(attrib_names)
    d = len(rho_ind_s) if rho_ind_s is not None else len(rho_tor_norm)

    # d_tot = 100
    d_tot = len(data.rho_tor_norm)

    #rad_grid = np.linspace(0., 1., d_tot)
    rad_grid = data.rho_tor_norm # rho_tor_norm of coreprof radial grid points
    #ATTENTION: for gradients GEM/GEM0 uses rho_tor_norm and ETS uses rho_tor

    prof_vals = np.zeros((n*m, d))
    #print(f"Entering a function to parse CPO into surrogate input") ###DEBUG

    for i, prof_name in enumerate(prof_names):

        prof = getattr(data, prof_name)

        for j, attrib_name in enumerate(attrib_names):

            if attrib_name != 'ddrho':
                val_readings = getattr(prof, attrib_name)
                val_readings_interp = l3interp(y_in=val_readings, x_in=rad_grid, x_out=rho_tor_norm)
            else:
                # GEM/GEM0/SURR are accepting the ddrho definition on rho_tor_norm only!
                val_prime_readings = getattr(prof, 'value')
                val_readings_interp = l3deriv(y_in=val_prime_readings, x_in=rad_grid, x_out=rho_tor_norm)

            if rho_tor_norm is not None:

                prof_vals[i*m+j,:] = val_readings_interp
            
            else:
            # TODO: ideally, should not be used any more!; will fail for ddrho if rho_tor_norm is None
                for r, rho_ind in enumerate(rho_ind_s):

                    val_reading = val_readings[rho_ind]

                    if prof_name[1] == 'i':
                        # Here: ion profiles are 1D (no species dimension) ...
                        #val_readings = val_readings[0]
                        pass
                    elif prof_name[1] == 'e':
                        pass
                    else:
                        print('Error: Attributes have to belong either to ions or to electrons')

                    prof_vals[i*m+j][r] = val_reading

    return prof_vals

def coretransp_to_value(
        data,
        rho_ind_s,
        prof_names=['te_transp', 'ti_transp'],
        attrib_names=['flux'],
                       ):
    """
    Transforms coretransp message data into values that could be read and plotted
    NB!: nearly a duplicate of coreprof_to_input_value(), use that instead
    """

    n = len(prof_names)
    m = len(attrib_names)
    d = len(rho_ind_s)

    transp_vals = np.zeros((n*m, d))
    #print(f"Entering a function to parse CPO into transp data") ###DEBUG

    prof_val = data.values[0] # a difference from coreprof

    for i, prof_name in enumerate(prof_names):

        prof = getattr(prof_val, prof_name)

        for j, attrib_name in enumerate(attrib_names):

            val_readings = getattr(prof, attrib_name)

            for r, rho_ind in enumerate(rho_ind_s):

                val_reading = val_readings[rho_ind]

                if prof_name[1] == 'i':
                    val_reading = val_reading[0] # a difference from coreprof (should be)
                    #pass
                elif prof_name[1] == 'e':
                    pass

                transp_vals[i*m+j][r] = val_reading

    return transp_vals

def output_value_to_coretransp(
            fluxes_out, 
            coretransp_file, 
            r_s=[0], # array of flux tube numbers in coretransp (not rho values!)
            ion=0, # number of ion species
            rho_tor_norm_sur=None,
            rho_tor_norm_sim=None,
            prof_names=['te_transp', 'ti_transp',], #TODO: double check CPO format
            attributes=['flux',]
                              ):
    """
    Transforms flux mean values infered by model into a CPO coretransp datastracture
    """
    
    # Casting array elements to strings
    #fluxes_out_str = {k:np.array([str(v) for v in vs]) for k,vs in fluxes_out.items()}
    #fluxes_out_str = {k:np.array(['  '+str(v)+'E+00' for v in vs]) for k,vs in fluxes_out.items()} # now assumes that value fits to a zero exponent
    #print(f"> fluxes_out: {fluxes_out}") ###DEBUG

    coretransp_datastructure = read(coretransp_file, 'coretransp')

    if len(coretransp_datastructure.values[0].ti_transp.flux) != len(r_s):
        coretransp_datastructure.values[0].ti_transp.flux = np.zeros((1, len(r_s)))

    if len(coretransp_datastructure.values[0].te_transp.flux) != len(r_s):
        coretransp_datastructure.values[0].te_transp.flux = np.zeros((len(r_s)))     

    # if flux tube coordinates from surrogate and code are different, interpolate
    if rho_tor_norm_sur is not None and rho_tor_norm_sim is not None:
        for prof_name in prof_names:
            for attribute in attributes:
                fluxes_out[prof_name+'_'+attribute] = l3interp(y_in=fluxes_out[prof_name+'_'+attribute], x_in=rho_tor_norm_sur, x_out=rho_tor_norm_sim)

    # NB: when this is commented out - will not change the coretransp passed
    for prof_name in prof_names:

        for attribute in attributes:

            for r in r_s:

                if attribute == 'flux':

                    if prof_name == 'ti_transp':
                                               
                        #coretransp_datastructure.values[0].ti_transp.flux = np.zeros((1, len(r_s)))
                        coretransp_datastructure.values[0].ti_transp.flux[r, 0] = fluxes_out[prof_name+'_'+attribute][r]
                    
                    elif prof_name == 'te_transp':
           
                        #coretransp_datastructure.values[0].te_transp.flux = np.zeros((len(r_s)))                       
                        coretransp_datastructure.values[0].te_transp.flux[r] = fluxes_out[prof_name+'_'+attribute][r]
                                
                    else:
                        print('Error: currently only temperatures for two species are supported')
                
                else:
                    print('Erorr: currently only models infering fluxes are supported')

    return coretransp_datastructure

def l3interp(y_in, x_in, nr_in=None, y_out=None, x_out=None, nr_out=None):
    """
    Interpolation function
    perfroms Lagrange interpolation of degree 3 for values y_in on x_in 
    for points at x_out and writes the values at y_out
    """

    #print("> Interpolation, nr_in:{} nr_out:{}".format(nr_in, nr_out))
    #print('x_in[{}]: {}; x_in[0]: {}'.format(nr_in, x_in[nr_in - 1], x_in[0]))
    #print('x_out: {}'.format(x_out))

    if nr_in is None:
        nr_in = len(x_in)
    if x_out is None:
        x_out = x_in # should be identity then - in practice, could work differently
    if nr_out is None:
        nr_out = len(x_out)
    if y_out is None:
        y_out = np.zeros(nr_out)

    if x_in[nr_in - 1] > x_in[0]:
        jstart = 2
        jfirst = 0
        jlast = nr_out - 1 
        jstep = 1
    else:
        jstart = nr_out - 3
        jfirst = nr_out - 1
        jlast = 0
        jstep = -1

    j1 = jstart

    #print('y_out size is {}'.format(y_out.shape))
    #print('the iteration for interpolation is over {}; {}; {}; {}'.format(jstart, jfirst, jlast, jstep))

    for j in range(jfirst, jlast + 1, jstep):
        #print('j:{}'.format(j))
        x = x_out[j]
        while x >= x_in[j1] and nr_in - 2 > j1 > 1:
            j1 = j1 + jstep

        #print('j1:{}'.format(j1))
        j2 = j1 + jstep
        j0 = j1 - jstep
        jm = j1 - 2 * jstep

        #print('j2: {}; j1:{}, j0: {}; jm: {}'.format(j2, j1, j0, jm))

        # Extrapolate inside out

        x2 = x_in[j2]
        x1 = x_in[j1]
        x0 = x_in[j0]
        xm = x_in[jm]

        aintm = (x - x0) * (x - x1) * (x - x2) / ((xm - x0) * (xm - x1) * (xm - x2))
        aint0 = (x - xm) * (x - x1) * (x - x2) / ((x0 - xm) * (x0 - x1) * (x0 - x2))
        aint1 = (x - xm) * (x - x0) * (x - x2) / ((x1 - xm) * (x1 - x0) * (x1 - x2))
        aint2 = (x - xm) * (x - x0) * (x - x1) / ((x2 - xm) * (x2 - x0) * (x2 - x1))

        #print('interpol ref points : {} {} {} {} {}'.format(x0, x1, x2, xm, x))
        #print('interpol coefs : {} {} {} {}'.format(aintm, aint0, aint1, aint2))

        y_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]

        #print ('y vals: {} {} {} {}'.format(y_in[j0], y_in[j1], y_in[j2], y_in[jm]))
        #print('y_out : {}, res len:{}'.format(y_out[j], len(y_out)))

    return y_out

def l3deriv(y_in, x_in, nr_in=None, dydx_out=None, x_out=None, nr_out=None):
    """
    Derivative on interpolated values
    """

    #print("> Derivative on inter., nr_in:{} nr_out:{}".format(nr_in, nr_out))
    
    if nr_in is None:
        nr_in = len(x_in)
    if x_out is None:
        x_out = x_in # should be identity then
    if nr_out is None:
        nr_out = len(x_out)
    if dydx_out is None:
        dydx_out = np.zeros(nr_out)

    if x_in[nr_in - 1] > x_in[0]:
        jstart = 2
        jfirst = 0
        jlast = nr_out - 1
        jstep = 1
    else:
        jstart = nr_out - 3
        jfirst = nr_out - 1
        jlast = 0
        jstep = -1

    j1 = jstart

    #print('the iteration for interpolation is over {}; {}; {}; {}'.format(jstart, jfirst, jlast, jstep))

    for j in range(jfirst, jlast + 1, jstep):
        x = x_out[j]
        while x >= x_in[j1] and nr_in - 2 > j1 > 1:
            j1 = j1 + jstep

        j2 = j1 + jstep
        j0 = j1 - jstep
        jm = j1 - 2 * jstep

        #print('j2: {}; j1:{}, j0: {}; jm: {}'.format(j2, j1, j0, jm))

        # Extrapolate inside out

        x2 = x_in[j2]
        x1 = x_in[j1]
        x0 = x_in[j0]
        xm = x_in[jm]

        aintm = ((x - x1) * (x - x2) + (x - x0) * (x - x2) + (x - x0) * (x - x1)) / ((xm - x0) * (xm - x1) * (xm - x2))
        aint0 = ((x - x1) * (x - x2) + (x - xm) * (x - x2) + (x - xm) * (x - x1)) / ((x0 - xm) * (x0 - x1) * (x0 - x2))
        aint1 = ((x - x0) * (x - x2) + (x - xm) * (x - x2) + (x - xm) * (x - x0)) / ((x1 - xm) * (x1 - x0) * (x1 - x2))
        aint2 = ((x - x0) * (x - x1) + (x - xm) * (x - x1) + (x - xm) * (x - x0)) / ((x2 - xm) * (x2 - x0) * (x2 - x1))

        #print('interpol ref points : {} {} {} {} {}'.format(x0, x1, x2, xm, x))

        dydx_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]

        #print('y vals: {} {} {} {}'.format(y_in[jm], y_in[j0], y_in[j1], y_in[j2]))
        #print('y_out : {}, res len:{}'.format(dydx_out[j], len(dydx_out)))

    return dydx_out

def read_attrib(filename, quantity, attribute, coords, filetype='coreprof'):
    """
    Returns list of attribute values, for every given coordinate number
    """
    cpo_obj = read(filename, filetype)
    q = cpo_obj
    
    if filetype == 'coretransp':
        c = cpo_obj.values[0]
    elif filetype == 'equilibrium' or 'coreprof':
        c = cpo_obj

    q = getattr(c, quantity)

    if attribute != 'ddrho':
        a = getattr(q, attribute)
    # A hack to always get gradients on the required grid!
    else:
        rho_grid_orig = c.rho_tor
        # rho_tor_norm from a CPO could still be different from GEM/GEM0!
        rho_grid = c.rho_tor_norm
        a_prime = getattr(q, 'value')
        a = l3deriv(y_in=a_prime, x_in=rho_grid, x_out=rho_grid)

    if quantity[1] == 'i' and attribute != 'ddrho':
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

    #TODO for the same type of CPOs, read a file once and return a dictionary of all [quantities] x [attributes]
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

def compare_states(state_1, state_2, cpo_types=['coreprof']):
    """
    Compare two plasma states - TODO: >2 states (how? pairwise?), >2 CPOs each
     - described via a list of CPO files [coreprof, equilibrium]
     - state is a dictionary {cpo_type: file_name}
     - returns: a metrics value (d e R^0+)
    """
    
    ds = []
    for cpo_type in cpo_types:
        cpo_name_1 = state_1[cpo_type]
        cpo_name_2 = state_2[cpo_type]
        d = compare_cpo(cpo_name_1, cpo_name_2, cpo_type=cpo_type)
        ds.append(d)

    d_comp = sum(ds) / len(ds) # TODO differences have to be comparable
    return d_comp

def compare_cpo(cpo_name_1, cpo_name_2, profiles=['te', 'ti'], attributes=['value', 'ddrho'], cpo_type='coreprof'):
    """
    Compare two CPO files by the list of quantities of interest
    - uses a list of 1d profiles - TODO: scalars, 1D profiles, 2D fields
    """

    coords = [0.143587306141853 , 0.309813886880875 , 0.442991137504578 , 0.560640752315521 , 0.668475985527039 , 0.769291400909424 , 0.864721715450287 , 0.955828309059143 ] # any number of coordinates in rho_tor_norm
    ds = []

    _,cpo_1 = read_cpo_file(cpo_name_1, profiles, attributes, coords, cpo_type)
    _,cpo_2 = read_cpo_file(cpo_name_2, profiles, attributes, coords, cpo_type)

    for p,a in product(profiles, attributes):
        q1 = cpo_1[f"{p}_{a}"]
        q2 = cpo_2[f"{p}_{a}"]
        d = compare_profiles(q1, q2, 'srRMSE')
        ds.append(d)
    
    d_comp = sum(ds) / len(ds)
    return d_comp

def compare_profiles(x1, x2, criterion):
    """
    Compares two (1D) profiles by given criterion
    - TODO list of profiles, criteria is a lambda/function d(.,.)->R^+
    """
    # TODO: consider RMSE - 0-abs-value issue; consider log-scale
    if criterion == 'rRMSE':
        # relative root mean square error
        d  = np.sqrt((np.power(x1-x2,2)/np.abs(x1)).mean())
    elif criterion == 'srRMSE':
        # symmetrised relative root mean square error
        d = np.sqrt( (np.power(x1-x2,2) / 0.5*(np.abs(x1)+np.abs(x2))).mean() )
    elif criterion == 'L2':
        d = np.sqrt(np.power(x1-x2,2).mean()) # this is not L2, as mean is ./n
    elif criterion == 'L1':
        d = (x1-x2).mean() #?
    elif criterion == 'Linf':
        d = (x1-x2).max()
    else: 
        ValueError("Unknown criterion for 1D profiles comparison") 
        #TODO: consider Wasserstein distance: for Te/i ~ Heat capacity, for ne/ni ~ mass, for gradients ~ diffusivity(?, total?)
    return d

def plot_state_conv(data, filename, metric_name='srRMSE', normalised=True):
    """
    Plots convergence of metrics for pairs of states with a simulation for a given metric
    """

    col_name_dict = {
        'd_prevfin_gtstst': "d(fin-1, g.t.st.st)",
        'd_fin_gtststs': "d(fin, g.t.st.st)",
        'd_prevfin_fin': "d(fin, fin-1)"
    }

    fig, ax = plt.subplots()

    t = data['it']

    for col_name in data.columns:
        if col_name in col_name_dict.keys():

            norm = data[col_name].max() if normalised else 1.

            ax.plot(t, 
                    data[col_name] / norm,
                    alpha=0.8,
                    marker='.',
                    label=col_name_dict[col_name]
                    )

    ax.set_yscale("log")
    ax.legend(loc='best')

    ax.xaxis.set_major_locator(ticker.MultipleLocator(1))
    ax.yaxis.set_major_locator(ticker.LogLocator(base=10,))
    ax.grid(visible=True)

    ax.set_xlabel(f"Algorithm iterations $i$")
    ax.set_ylabel(f"{metric_name}, a.u.")
    ax.set_title(f"$D(s_{{i}},s_{{i-1}})${', norm-d to 1st it-n' if normalised else ''}")

    fig.savefig(f"{filename}.pdf")

    return 0

def plot_quantities(datadict, save_fold_name, codename=None, dates=None, times=None, coord_num_fts=[14,30,43,55,66,76,85,95], bool_sur_involved=False, n_run_per_ft=81, ref_data=None,):
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

def write_table_csv(datadict, save_fold_name, codename, coord_num_fts=[14,30,43,55,66,76,85,95]):
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
    #TODO: after introduction of equilibrium parsing, the size of the table row is larger than number of columns!

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

def compare_transp(datadict, save_fold_name, codename, dates, times, input_folder_base='', coord_num_fts=None, option=1, modeltype='gem0py'):
    """
    Takes a sequence of profile values and estimates transport using several models
    """
    # option 0 - model is called for all flux tube using a dictionary of core profiles (GEM flux tube rho_tor_norm locations are used)
    # option 1 - model is called per flux tube (single flux tube was used, custom rho_tor_norm for flux tubes were defined)
    # option 2 - model is called for all flux tubes using CPO objects (GEM flux tube rho_tor_norm locations are used)

    q_profile_list = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    q_transp_list = ['te_transp_flux', 'ti_transp_flux']

    coord_inds = [14, 30, 43, 55, 66, 76, 85, 95]
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

    nfts = len(coords)
    n_fts = [i for i in range(nfts)]
    #coords = coord_num_fts[:] # if coords are in coretransp notation i.e. flux tube numbers

    # due to different naming for the final state - could be an issue
    del_tstep = 0            
    #t_min = min(len(datadict[q_x][:,n_ft]), len(datadict[q_y][:,n_ft]))
    t_min = len(times) + del_tstep

    # initialise a model and its evaluation - here: GEM0
    if modeltype == 'gem0py':
        model = ExtCodeHelper(2, xml_file=xml_file)
    elif modeltype == 'esurr':
        input_names_ind_permut = [0,2,1,3]
        model_file_base = '../workflows/surrogate_for_workflow/gem0_es_model'
        model = []
        for ft in n_fts:
            model.append(es.Campaign(load_state=True, file_path=f"{model_file_base}_{ft}.pickle").surrogate)
        option = 2 # ATTENTION: override the option for surrogate!

    # choose how to call GEM0
    if modeltype == 'gem0py':
        if option == 0:
            # option 0: pass te,ti,te_ddrho,ti_ddrho at 1D grids of length 100
            model_call = lambda x: model.gem0_call_4param2target_fullarray(x)
        elif option == 1:
            # option 1: pass te,ti,te_ddrho,ti_ddrho and rho_tor_norm
            model_call = lambda x,rho_ind,rho: model.gem0_call_4param2target_array([x], [rho_ind], rho)
        elif option == 2:
            # option 2: pass equilibrium, coreprof, coretransp
            model_call = lambda eq,prof,transp: model.gem0_call_4param2target_cpo(eq,prof,transp)
    elif modeltype == 'esurr':
            model_call = lambda prof: [model[ft].predict(coreprof_to_input_value(prof,rho_ind_s=coord_inds,rho_tor_norm=coords)[input_names_ind_permut][:,ft].reshape(-1,1))[0] for ft in n_fts]

    profile_corrected = np.zeros((len(q_profile_list), t_min, nfts))
    transp_new        = np.zeros( (len(q_transp_list), t_min, nfts))

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

        if modeltype == 'gem0py':
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

        if modeltype == 'esurr':
            
            #eq = read(input_folder+eq_filename+str(t_ind).zfill(5)+cpo_extension, 'equilibrium')
            prof = read(input_folder+prof_filename+str(t_ind).zfill(5)+cpo_extension, 'coreprof')
            #transp = read(input_folder+transp_filename+str(t_ind).zfill(5)+cpo_extension, 'coretransp')

            tr = model_call(prof)

            #print(f"tr out: {tr}") ###DEBUG

            tr = np.array([t.reshape(-1) for t in tr]).transpose()

            transp_new[:, t_ind, :] = tr
        
    ### Plot transp fluxes vs time for two models alongside each other

    # Get a list of transp value for two models [{transp_quantity:np.array(times)}]
    transp_list = []
    transp_list.append({q: datadict[q] for q in q_transp_list})
    transp_list.append({'te_transp_flux': transp_new[0,:,:], 'ti_transp_flux': transp_new[1,:,:]})

    with PdfPages(save_fold_name+'transp_'+codename+'_'+modeltype+'_'+dates[0]+'_'+dates[-1]+'.pdf') as pdf_transp: 
        
        for n_ft in n_fts:

            for i_q, q_transp in enumerate(q_transp_list):
        
                fig,ax = plt.subplots()

                for n_m,transp_vals in enumerate(transp_list):
                
                    ax.plot(times, transp_vals[q_transp][:,n_ft], label=f"model#{n_m} @ft#{n_ft}", marker='.', alpha=0.25)

                ax.set_xlabel('time, s')
                ax.set_ylabel(q_transp)
                ax.legend(loc='best')
                ax.set_title(f"Plots for {q_transp} @f.t.#{n_ft} (m0 is WF, m1 is {modeltype})")

                pdf_transp.savefig(fig)

    return 0

def plot_prof_time(data, pdf_prof, coord_rho_tor_norm, coords_transp_gem, times, n_timesteps, fmt_list, lookup_names, quantity_name, attribute_name):

    """
    Plot profiles Y(rho_tor_norm), one plot every n_timestep
    """
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
                label=f"t={times[i]:.5f},s", 
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
