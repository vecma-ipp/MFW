import pandas as pd
import numpy as np

import ual
import base

from ascii_cpo import read, write_fstream, read_fstream, write

def training_data_bounds(ref_data, input_names=['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho'], n_fts=8, n_run_per_ft=81):
    """
    Produces a dictionary for training parameters, 
    each having an array of min and max values occuring in the dataset 
    """

    train_bounds = {k:{'min':np.zeros(n_fts), 'max':np.zeros(n_fts)} for k in input_names}

    for i in range(n_fts):
        for input in input_names:
            train_bounds[input]['min'][i] = ref_data[input].iloc[n_run_per_ft*(i):n_run_per_ft*(i+1)].min()
            train_bounds[input]['max'][i] = ref_data[input].iloc[n_run_per_ft*(i):n_run_per_ft*(i+1)].max()

    return train_bounds

def check_outof_learned_bounds(input, reference):
    """
    input: an array of input values for surrogate of size (n_features, n_coordinates)
    reference: a dictionary of training data bounds in format {feature:{'max':array(n_coords),'min':array(n_coords)}}
    Returns: bool: True if input is outisde of the learned bounds
    """

    bool_outofbounds = False

    dict_outofbounds = {}

    for i,(k,vs) in enumerate(reference.items()):
        
        dict_outofbounds[k] = {
                               'greater': np.zeros(vs['max'].shape, dtype=bool),
                               'lesser' : np.zeros(vs['min'].shape, dtype=bool), 
                               'within' :  np.ones(vs['min'].shape, dtype=bool),
                               } # OHE; technicaly, 'within' is redundant and we need only 2 bits to encode 3 classes

        for j,v in enumerate(vs['max']): # can use .tolist() to be sure array is converted into a generator

            if v < input[i,j]:
                
                bool_outofbounds = True
                dict_outofbounds[k]['greater'][j] = True
                dict_outofbounds[k]['within'][j]  = False
                
        for j,v in enumerate(vs['min']):

            if v > input[i,j]: 

                bool_outofbounds = True
                dict_outofbounds[k]['lesser'][j] = True
                dict_outofbounds[k]['within'][j] = False

    return bool_outofbounds, dict_outofbounds

def coreprof_to_input_value(
            data, 
            rho_ind_s, 
            prof_names=['te', 'ti'], 
            attrib_names=['value', 'ddrho'],
                     ):
    """
    Transforms coreprof message data into values acceptable by a surrogate model as an input
    TODO: look up EasyVVUQ cpo element for coreprof
    TODO: check that all input values are filled with defaults
    TODO: check if there is an implementation in easyvvuq utils
    """

    n = len(prof_names)
    m = len(attrib_names)
    d = len(rho_ind_s)

    prof_vals = np.zeros((n*m, d))
    print(f"Entering a function to parse CPO into surrogate input")

    for i, prof_name in enumerate(prof_names):

        prof = getattr(data, prof_name)

        for j, attrib_name in enumerate(attrib_names):

            for r, rho_ind in enumerate(rho_ind_s):

                val_readings = getattr(prof, attrib_name)

                if prof_name[1] == 'i':
                    # Here: ion profiles are 1D (no species dimension) ...
                    #val_readings = val_readings[0]
                    pass

                elif prof_name[1] == 'e':
                    pass
                
                else:
                    print('Error: Attributes have to belong either to ions or to electrons')

                val_reading = val_readings[rho_ind]

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
    print(f"Entering a function to parse CPO into transp data")

    for i, prof_name in enumerate(prof_names):

        prof = getattr(data, prof_name)

        for j, attrib_name in enumerate(attrib_names):

            for r, rho_ind in enumerate(rho_ind_s):

                val_readings = getattr(prof, attrib_name)

                if prof_name[1] == 'i':
                    val_readings = val_readings[0]

                elif prof_name[1] == 'e':
                    pass

                val_reading = val_readings[rho_ind]

                transp_vals[i*m+j][r] = val_reading

    return transp_vals

def output_value_to_coretransp(
            fluxes_out, 
            coretransp_file, 
            r_s = [0],
            ion = 0,
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

    # NB: when this is commented out and will not change the coretransp passed
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
