import numpy as np
import pandas as pd

import logging

from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

from ascii_cpo import read, read_fstream
import ual
import base

from ctypes import c_char, string_at
import io, sys, copy
import pickle

import easysurrogate as es


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
        print(f"prof: {prof}") ###DEBUG

        for j, attrib_name in enumerate(attrib_names):

            for r, rho_ind in enumerate(rho_ind_s):

                val_readings = getattr(prof, attrib_name)
                print(f">val_readings={val_readings}") ###DEBUG

                if prof_name[1] == 'i':
                    # Here: ion profiles are 1D (no species dimension) ...
                    #val_readings = val_readings[0]
                    pass

                elif prof_name[1] == 'e':
                    pass
                
                else:
                    print('Error: Attributes have to belong either to ions or to electrons')

                print(f">val_readings={val_readings}") ###DEBUG
                print(f">val_readings[rho_ind]={val_readings[rho_ind]}") ###DEBUG
                val_reading = val_readings[rho_ind]
                print(f">val_reading: {val_reading}") ###DEBUG

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

    print(f">fluxes_out: {fluxes_out}") ###DEBUG

    coretransp_datastructure = read(coretransp_file, 'coretransp')

    for prof_name in prof_names:

        for attribute in attributes:

            for r in r_s:

                if attribute == 'flux':

                    if prof_name == 'ti_transp':
                        #coretransp_datastructure.values[0].ti_transp.flux[r, ion] = fluxes_out[prof_name+'_'+attribute]
                        # print(f">coretransp_datastructure: {coretransp_datastructure}") ###DEBUG
                        # print(f">coretransp_datastructure.values: {coretransp_datastructure.values}") ###DEBUG
                        # print(f"coretransp_datastructure.values[0]: {coretransp_datastructure.values[0]}") ###DEBUG
                        # print(f">coretransp_datastructure.values.ti_transp: {coretransp_datastructure.values[0].ti_transp}") ###DEBUG
                        # print(f">coretransp_datastructure.values.ti_transp.flux: {coretransp_datastructure.values[0].ti_transp.flux}") ###DEBUG
                        
                        #coretransp_datastructure.values[0].ti_transp.flux[r] = fluxes_out[prof_name+'_'+attribute]
                        coretransp_datastructure.values[0].ti_transp.flux = []
                        coretransp_datastructure.values[0].ti_transp.flux.append(fluxes_out[prof_name+'_'+attribute])
                        
                        #print(f">coretransp_datastructure.values.ti_transp.flux: {coretransp_datastructure.values[0].ti_transp.flux}") ###DEBUG
                    
                    elif prof_name == 'te_transp':
                        #coretransp_datastructure.values[0].te_transp.flux[r, ion] = fluxes_out[prof_name+'_'+attribute]
                        coretransp_datastructure.values.te_transp.flux = []
                        coretransp_datastructure.values.te_transp.flux.append(fluxes_out[prof_name+'_'+attribute])
                    
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
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
        Operator.O_F:    ['coretransp_out',],
                        },
        #USES_CHECKPOINT_API,
                       )
    
    print(f"> Initialised turbulence instance") #DEBUG

    while instance.reuse_instance():
        # when is the instance constructed and destructed?
        print(f"> Entering a turbulence model iteration") #DEBUG

        rho_ind_s = instance.get_setting('rho_ind_s', 'int') #TODO: consider using function to calculate index from rho_tor
        prof_out_names = instance.get_setting('profs_out', 'str') #TODO: figure out how to pass lists of float/strings -> possible: ['float']/['str']
        model_file = instance.get_setting('surrogate_path', 'str')
        coretransp_default_file_name = instance.get_setting('cortransp_default', 'str')
        init_cpo_dir = instance.get_setting('init_cpo_dir', 'str')

        coretransp_default_file_path = init_cpo_dir + '/' + coretransp_default_file_name
        n_dim_out = len([prof_out_names])

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

        #TODO: read target names from campaign
        #output_names = ['ti_transp_flux', 'te_transp_flux']
        output_names = ['ti_transp_flux']

        # Get a message form equilibrium code: NOT USED HERE!
        msg_in = instance.receive('equilibrium_in')
        print('> Got a message from EQUILIBRIUM')
        
        # Get a message from transport code
        msg_in = instance.receive('coreprof_in')
        print('> Got a message from TRANSP')

        # Update timestep number
        num_it = msg_in.timestamp + 1

        # Get profile byte array data from the message from TRANSP (and check what's inside)
        profiles_in_data_bytes = msg_in.data
        #print(f"length of profile bytes received : {len(profiles_in_data_bytes)}") ###DEBUG
        #n_print = 4096; print(f"first {n_print} bytes(?) of the buffer is:\n{profiles_in_data_bytes[0:n_print]}") ###DEBUG
        #print(f"bytes(?) 8400-8600 of the buffer is:\n{profiles_in_data_bytes[8400:8600]}") ###DEBUG

        # Convert profile byte array to (numpy array) string
        #profiles_in_data = np.frombuffer(profiles_in_data_bytes, dtype=c_char) # not required, ual interface require to read a file (or a string)
        profiles_in_data_str = profiles_in_data_bytes.decode("utf-8")
        #n_print = len(profiles_in_data_str); print(f"first {n_print} elements of the data str is:\n{profiles_in_data_str[0:n_print]}") ###DEBUG
        
        # Read the data string like it is a CPO file and find the turbulence-relevant array of values for given flux tubes
        file_like_profiles_in_data_str   = io.StringIO(profiles_in_data_str) # TODO: the issue has to be here
        file_like_profiles_in_data_bytes = io.BytesIO(profiles_in_data_bytes)
        #file_like_profiles_in_data_text = io.TextIOWrapper(profiles_in_data_str, encoding='utf-8')

        ###DEBUG (next 3 lines): test the file_like_profiles_in_data_str -> carefull, moves the read head down the file
        # for i in range(1):
        #     stest = file_like_profiles_in_data_str.readline()
        #     print(f"line read from a stream made of the buffer string:\n{stest}")
                
        itmobj = ual.itm()
        glob_cpo = getattr(itmobj, "coreprof")
        cpo = copy.deepcopy(glob_cpo)
        #print(f"CPO object created empty is:\n{cpo}") ###DEBUG

        ###DEBUG (next 7 lines)
        # test_file_name = "../../../../../../workflows/AUG_28906_6/ets_coreprof_in.cpo"
        # with open(test_file_name, 'r') as f:
        #     test_file_string = f.read()
        # print(f"> String read from a test file is:\n{test_file_string}") ###DEBUG
        # print(f"> Equivalence of strings from file and from buffer: {test_file_string==profiles_in_data_str}")
        # profiles_cpo_obj_test = read(test_file_name, "coreprof")
        # print(f"> CPO object read from a test file is:\n{profiles_cpo_obj_test}") ###DEBUG

        ###DEBUG (next 8 lines): every string is the same!
        # f_test = open(test_file_name, 'r') 
        # f_test_lines = f_test.readlines()
        # f_string = io.StringIO(profiles_in_data_str)
        # f_string_lines = f_string.readlines()
        # print(f"Checking file and string streams line by line")        
        # for i,l1 in enumerate(f_test_lines):
        #     if l1 != f_string_lines[i]:
        #         print(f"string #{i} is different in StringIO!")

        #print("Starting to parse an IOString buffer") ###DEBUG
        profiles_cpo_obj = read_fstream(file_like_profiles_in_data_str, cpo, "coreprof") #TODO: fails here
        #print(f"CPO object read is:\n{profiles_cpo_obj}") ###DEBUG, profiles_cpo_obj and profiles_cpo_obj_test have to be the same

        #profiles_in =  coreprof_to_input_value(profiles_cpo_obj_test, [rho_ind_s],) ### DEBUG/TEST version
        profiles_in = coreprof_to_input_value(profiles_cpo_obj, [rho_ind_s],)
        print('> Read incoming core profile {0}'.format(profiles_in))

        # Get (n_features, n_samples) from surrogate from (n_features, n_radial_points)
        if profiles_in.shape[1] == 1:
            profiles_in.squeeze()
        profiles_in.reshape(-1,1)

        # Read coreprof to either:
        #                   a. dictionary with keys being names of profiles and values being numpy arrays or lists
        #                   b+. numpy array of shape==(n_sample, n_profiles_dim, [n_rad_points]) and dtype=float

        #TODO use a surrogate for a vector output: ['te.transp.flux', 'ti.transp.flux']
        # Infer a mean flux value using a surrogate
        fluxes_out, _ = model.predict(profiles_in)
        print('> Used a surrogate to predict new Q_i={0}'.format(fluxes_out))

        #TODO: in principle with large scale separation local t_cur does not play role,
        # but one could also estimate time for turbulence saturation with surrogate 
        #TODO: find MUSCLE3 format for dictionaries or dataframes
        #TODO: initialise the default data to fill in coretransp structures - look up GEM0 in Python

        fluxes_out_dict = {k:fluxes_out[i] for i,k in enumerate(output_names)}
        
        core_transp_datastructure = output_value_to_coretransp(fluxes_out_dict, coretransp_default_file_path, prof_names=['ti_transp'])

        # core_transp_datastructure -> bytes: check out options
        #core_transp_bytes = bytes(core_transp_datastructure)
        core_transp_bytes = pickle.dumps(core_transp_datastructure)

        print('> Gettting ready an outcoming core transp')
        msg_out = Message(num_it, None, core_transp_bytes)
        print('> Sending an outcoming core transp')

        #if instance.should_save_snapshot():
        #    msg_snapshot = Message(num_it, data=fluxes_out_dict) #TODO: check how to save dict

        instance.send('coretransp_out', msg_out)
        print('> Sent an outcoming core transp')


if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    gem_surr_M3()
