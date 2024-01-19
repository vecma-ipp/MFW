import pandas as pd
import numpy as np

import ual
import base

from ascii_cpo import read, write_fstream, read_fstream, write

def training_data_bounds(ref_data, input_names=['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho'], n_fts=8, n_run_per_ft=81, option='ft_col'):
    """
    Produces a dictionary for training parameters, 
    each having an array of min and max values occuring in the dataset 
        option: whether to use 'ft' column in the reference data or not
    
    """

    train_bounds = {k:{'min':np.zeros(n_fts), 'max':np.zeros(n_fts)} for k in input_names}

    for input in input_names:
        for i in range(n_fts):
            if option == 'ft_col':
                train_bounds[input]['min'][i] = ref_data[ref_data['ft'==i]][input].min()
                train_bounds[input]['max'][i] = ref_data[ref_data['ft'==i]][input].max()
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

    print(f"reference:\n{reference}") ###DEBUG
    print(f"input:\n{input}") ###DEBUG

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

    print(f"dict_outofbounds:\n{dict_outofbounds}") ###DEBUG

    return bool_outofbounds, dict_outofbounds

def coreprof_to_input_value(
            data, 
            rho_ind_s, 
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
    d = len(rho_ind_s)

    d_tot = 100

    #rad_grid = np.linspace(0., 1., d_tot)
    rad_grid = data.rho_tor_norm # rho_tor_norm of coreprof radial grid points

    prof_vals = np.zeros((n*m, d))
    print(f"Entering a function to parse CPO into surrogate input")

    for i, prof_name in enumerate(prof_names):

        prof = getattr(data, prof_name)

        for j, attrib_name in enumerate(attrib_names):

            val_readings = getattr(prof, attrib_name)

            if rho_tor_norm is None:

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
            
            else:

                val_readings_interp = l3interp(y_in=val_readings, x_in=rad_grid, x_out=rho_tor_norm)

                prof_vals[i*m+j,:] = val_readings_interp

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
    print(f"Entering a function to parse CPO into transp data")

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
