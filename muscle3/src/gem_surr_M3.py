import numpy as np
import pandas as pd

import logging

from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

from ascii_cpo import read, read_fstream, write, write_fstream
import ual
import base

from ctypes import c_char, string_at
import io, sys, copy
import pickle

import easysurrogate as es

from time import time as t

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
    TODO: check if there is an implementation in utils
    """

    n = len(prof_names)
    m = len(attrib_names)
    d = len(rho_ind_s)

    prof_vals = np.zeros((n*m, d))
    print(f"Entering a function to parse CPO into surrogate input")

    for i, prof_name in enumerate(prof_names):

        prof = getattr(data, prof_name)
        #print(f"prof: {prof}") ###DEBUG

        for j, attrib_name in enumerate(attrib_names):

            for r, rho_ind in enumerate(rho_ind_s):

                val_readings = getattr(prof, attrib_name)
                #print(f">val_readings={val_readings}") ###DEBUG

                if prof_name[1] == 'i':
                    # Here: ion profiles are 1D (no species dimension) ...
                    #val_readings = val_readings[0]
                    pass

                elif prof_name[1] == 'e':
                    pass
                
                else:
                    print('Error: Attributes have to belong either to ions or to electrons')

                #print(f">val_readings[rho_ind]={val_readings[rho_ind]}") ###DEBUG
                val_reading = val_readings[rho_ind]
                #print(f">val_reading: {val_reading}") ###DEBUG

                prof_vals[i*m+j][r] = val_reading

    return prof_vals

def output_value_to_coretransp(
            fluxes_out, 
            coretransp_file, 
            r_s = [0],
            ion = 0,
            prof_names=['ti_transp', 'te_transp',], #TODO: double check CPO format
            attributes=['flux',]
                              ):
    """
    Transforms flux mean values infered by model into a CPO coretransp datastracture
    """
    
    # Casting array elements to strings
    #fluxes_out_str = {k:np.array([str(v) for v in vs]) for k,vs in fluxes_out.items()}
    #fluxes_out_str = {k:np.array(['  '+str(v)+'E+00' for v in vs]) for k,vs in fluxes_out.items()} # now assumes that value fits to a zero exponent
    print(f"> fluxes_out: {fluxes_out}") ###DEBUG

    coretransp_datastructure = read(coretransp_file, 'coretransp')

    # NB: when this is commented out and will not change the coretransp passed
    for prof_name in prof_names:

        for attribute in attributes:

            for r in r_s:

                if attribute == 'flux':

                    if prof_name == 'ti_transp':

                        print(f"> flux datastructure : {fluxes_out[prof_name+'_'+attribute]}") ###DEBUG
                        #coretransp_datastructure.values[0].ti_transp.flux[r] = fluxes_out[prof_name+'_'+attribute]
                        #coretransp_datastructure.values[0].ti_transp.flux = []
                        #coretransp_datastructure.values[0].ti_transp.flux.append(fluxes_out[prof_name+'_'+attribute])
                        #coretransp_datastructure.values[0].ti_transp.flux = fluxes_out_str[prof_name+'_'+attribute].tolist()
                        # if len(vals) == 0:
                        #     vals.append([])
                        # vals[0].append(fluxes_out[prof_name+'_'+attribute][0])
                       
                        if len(coretransp_datastructure.values[0].ti_transp.flux) == 0:
                            coretransp_datastructure.values[0].ti_transp.flux = np.zeros((len(r_s), 1))
                        coretransp_datastructure.values[0].ti_transp.flux[r, 0] = fluxes_out[prof_name+'_'+attribute][r]
                    
                    elif prof_name == 'te_transp':

                        #coretransp_datastructure.values[0].te_transp.flux[r, ion] = fluxes_out[prof_name+'_'+attribute]
                        #coretransp_datastructure.values.te_transp.flux = []
                        #coretransp_datastructure.values.te_transp.flux = fluxes_out[prof_name+'_'+attribute].to_list()
                        #coretransp_datastructure.set_value(prof_name+'.'+attribute, fluxes_out_str[prof_name+'_'+attribute].tolist(), r_s)
                        #vals.append(fluxes_out[prof_name+'_'+attribute][0])
                        
                        if len(coretransp_datastructure.values[0].te_transp.flux) == 0:
                            coretransp_datastructure.values[0].te_transp.flux = np.zeros((len(r_s)))
                        coretransp_datastructure.values[0].te_transp.flux[r] = fluxes_out[prof_name+'_'+attribute][r]
                                
                    else:
                        print('Error: currently only temperatures for two species are supported')
                
                else:
                    print('Erorr: currently only models infering fluxes are supported')

    return coretransp_datastructure

def gem_surr_M3():
    """
    MUSCLE3 implementation of GEM surrogate
    Creates an ML model from a .pickle file
    Receives messages with coreprof [,equilibrium] and send messages with coretransp 
    """

    # Creating a MUSCLE3 instance
    instance = Instance({
        Operator.O_F:     ['coretransp_out',],
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
                        },
        #USES_CHECKPOINT_API,
                       )
    
    print(f"> Initialised turbulence instance") #DEBUG

    while instance.reuse_instance():
        # when is the instance constructed and destructed?
        print(f"> Entering a turbulence model iteration")

        rho_ind_s = instance.get_setting('rho_ind_s', '[float]]') #TODO: consider using function to calculate index from rho_tor
        prof_out_names = instance.get_setting('profs_out', 'str') #TODO: figure out how to pass lists of float/strings -> possible: ['float']/['str']
        model_file = instance.get_setting('surrogate_path', 'str')
        coretransp_default_file_name = instance.get_setting('cortransp_default', 'str')
        init_cpo_dir = instance.get_setting('init_cpo_dir', 'str')

        #coretransp_default_file_path = init_cpo_dir + '/' + coretransp_default_file_name
        coretransp_default_file_path = coretransp_default_file_name

        n_dim_out = len([prof_out_names])

        rho_ind_s = [int(x) for x in rho_ind_s]
        n_fts = len(rho_ind_s)

        # Initialising surrogate model
        #   for a one-shot training case should only be done once in f_init

        #TODO: consider other model initialisation e.g. using just an ML library
        #TODO: consider single model load per MUSCLE3 Manager run

        print('> Loading ES campaign with a surrogate')
        try:
            campaign = es.Campaign(load_state=True, file_path=model_file)
        except OSError:
            print('No such model file!')
            return
        model = campaign.surrogate
        print('> Got a surrogate from a ES campaign')
        #print(campaign.surrogate.model) ###DEBUG
        #print(f"y_train: {model.y_train}") ###DEBUG

        #TODO: read target names from campaign
        output_names = ['ti_transp_flux', 'te_transp_flux']
        #output_names = ['ti_transp_flux']

        input_names_ind_permut = [0,2,1,3]

        # Get a message form equilibrium code: NOT USED HERE!
        msg_in = instance.receive('equilibrium_in')
        print('> Got a message from EQUILIBRIUM')
        
        # Get a message from transport code
        msg_in = instance.receive('coreprof_in')
        print('> Got a message from TRANSP')

        # Update timestep number
        num_it = msg_in.timestamp + 1

        # Get profile byte array data from the message from TRANSP (and check what's inside)
        coreprof_in_data_bytes = msg_in.data

        # Save the binary buffer to a file
        devshm_file = "/dev/shm/ets_coreprof_in.cpo" #TODO: generate and store random name
        with open(devshm_file, "wb") as f:
            f.write(coreprof_in_data_bytes)
        start_t = t()
        coreprof_cpo_obj = read(devshm_file, "coreprof")
        print (f"> Reading CPO file {devshm_file} took {t()-start_t} s")

        # Convert profile byte array to (numpy array) string
        #coreprof_in_data = np.frombuffer(coreprof_in_data_bytes, dtype=c_char) # not required, ual interface require to read a file (or a string)
        #coreprof_in_data_str = coreprof_in_data_bytes.decode("utf-8")
        
        # Read the data string like it is a CPO file and find the turbulence-relevant array of values for given flux tubes
        #file_like_coreprof_in_data_str   = io.StringIO(coreprof_in_data_str) # TODO: the issue has to be here
                
        #itmobj = ual.itm()
        #glob_cpo = getattr(itmobj, "coreprof")
        #cpo = copy.deepcopy(glob_cpo)

        #coreprof_cpo_obj = read_fstream(file_like_coreprof_in_data_str, cpo, "coreprof") #TODO: fails here - StrionIO has no file descriptor

        profiles_in = coreprof_to_input_value(coreprof_cpo_obj, [rho_ind_s],)
        profiles_in = profiles_in[input_names_ind_permut] #TODO: either fix original order, or store permutation separately
        print('> Read incoming core profile \n {0}'.format(profiles_in))

        # Get (n_features, n_samples) from surrogate from (n_features, n_radial_points)
        if profiles_in.shape[1] == 1:
            profiles_in.squeeze()
        profiles_in.reshape(-1,1)
        print('> Reshaped core profile \n {0}'.format(profiles_in)) ###DEBUG

        # Read coreprof to either:
        #                   a. dictionary with keys being names of profiles and values being numpy arrays or lists
        #                   b+. numpy array of shape==(n_sample, n_profiles_dim, [n_rad_points]) and dtype=float

        #TODO use a surrogate for a vector output: ['te.transp.flux', 'ti.transp.flux']
        # Infer a mean flux value using a surrogate
        #   NB!: Assumes same model predicting for different rho_tor separately:
        for n_ft, r in enumerate(rho_ind_s):
            fluxes_out, fluxes_out_std = model.predict(profiles_in[:,n_ft])
            print('> Used a surrogate at rho_ind={r} to predict new Q_e,i={0}'.format(fluxes_out))
            print('> Predicted STDs of new Q_e,i={0}'.format(fluxes_out_std))

        #TODO: in principle with large scale separation local t_cur does not play role,
        # but one could also estimate time for turbulence saturation with surrogate 
        #TODO: find MUSCLE3 format for dictionaries or dataframes
        #TODO: initialise the default data to fill in coretransp structures - look up GEM0 in Python

        fluxes_out_dict = {k:fluxes_out[:][i] for i,k in enumerate(output_names)}
        
        coretransp_cpo_obj = output_value_to_coretransp(
                                        fluxes_out_dict, 
                                        coretransp_default_file_path, 
                                        r_s = [i for i,r in enumerate(rho_ind_s)],
                                        prof_names=['te_transp', 'ti_transp'],
                                                        )
        #write(coretransp_cpo_obj, "sur_coretransp_out.cpo", "coretransp") ###DEBUG

        # Creating a bytes variable to be sent via MUSCLE3, coretransp_cpo_obj -> bytes
        # coretransp_str_bytes = bytes(coretransp_cpo_obj)
        # coretransp_str_pickle = pickle.dumps(coretransp_cpo_obj)
        file_like_profiles_out_data_str = io.StringIO('')
        write_fstream(file_like_profiles_out_data_str, coretransp_cpo_obj, "coretransp")
        coretransp_str = file_like_profiles_out_data_str.getvalue()
        coretransp_bytes = bytes(coretransp_str, "utf-8")

        # with open("ets_coreprof_bin.cpo", "wb") as f:
        #     f.write(coretransp_str_bytes)

        # Sending a coretransp message
        print('> Gettting ready an outcoming core transp')
        msg_out = Message(num_it, None, coretransp_bytes)
        print('> Sending an outcoming core transp')

        #if instance.should_save_snapshot():
        #    msg_snapshot = Message(num_it, data=fluxes_out_dict) #TODO: check how to save dict

        instance.send('coretransp_out', msg_out)
        print('> Sent an outcoming core transp')


if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    gem_surr_M3()
