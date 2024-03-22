import numpy as np
import pandas as pd

#from ctypes import c_char, string_at
import io
import sys
import os

import random as r
from time import time as t

import logging
import pickle

from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

from ascii_cpo import read, write_fstream, read_fstream, write

import easysurrogate as es

from muscle_utils.utils import coreprof_to_input_value, output_value_to_coretransp, training_data_bounds, check_outof_learned_bounds, equilibrium_to_input_value, make_new_training_sample

def gem_surr_M3(id=0):
    """
    MUSCLE3 implementation of GEM surrogate
    Creates an ML model from a .pickle file
    Receives messages with coreprof [,equilibrium] and send messages with coretransp 
    """

    input_names_ind_permut = [0,2,1,3] # permuation of input names relative to one used in EasySurrogate: ['te_value', 'te_ddrho', 'ti_value', 'ti_ddrho'] -> ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    input_eq_names_ind_permut = [0,1]

    # Creating a MUSCLE3 instance
    instance = Instance({
        Operator.O_F:     ['coretransp_out', 'coretransp_uncertainty_out'],
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
                        },
        #USES_CHECKPOINT_API,
                       )
    
    print(f"> Initialised turbulence instance") #DEBUG

    # Reading settings from a ymmsl file
    ref_data_filename = instance.get_setting('training_dataset_filename', 'str')
    rho_ind_s = instance.get_setting('rho_ind_s', '[float]') #TODO : consider using function to calculate index from rho_tor; also can be read from the first coretranp received
    rho_tor_norm_sim = instance.get_setting('rho_tor_norm_sim', '[float]') #TODO: use interpolation on these, or ones from coretransp
    rho_tor_norm_sur = instance.get_setting('rho_tor_norm_sur', '[float]') #ATTENTION: for the GEM0 data generated after 15.12.2023 the 'sim' grid is used
    #output_names = instance.get_setting('profs_out', '[str]') #TODO: figure out how to pass lists of float/strings -> possible: ['float']/['str'] - not supported ATM
    model_file_base = instance.get_setting('surrogate_path', 'str') # prefix of a path to files with surrogate model
    coretransp_default_file_name = instance.get_setting('coretransp_default', 'str')
    #init_cpo_dir = instance.get_setting('init_cpo_dir', 'str')
    bool_send_uncertainty = instance.get_setting('bool_send_uncertainty', 'bool') # bool for all any of the inputs for any of the locations to be outside of reference data
    model_type = instance.get_setting('surrogate_type', 'str') # 'ann' or 'gpr
    retrain_distance = instance.get_setting('retrain_distance', 'float') # distance to move outside of reference data to retrain surrogate
    use_equilibrium = instance.get_setting('use_equilibrium', 'bool') # boolean whether the surrogate needs new equilibrium data to infer transport

    # Setting up pathes, dimensionalities etc.
    #coretransp_default_file_path = init_cpo_dir + '/' + coretransp_default_file_name
    coretransp_default_file_path = coretransp_default_file_name # redundant ATM

    rho_ind_s = [int(x) for x in rho_ind_s]
    n_fts = len(rho_ind_s)
    ft_numbers = [i for i in range(n_fts)]

    # Reference training data CSV used to train surrogate
    ref_data = pd.read_csv(ref_data_filename, sep=',')
    ref_bounds = training_data_bounds(ref_data, n_fts=n_fts)

    # Initialising surrogate model
    #   for a one-shot training case should only be done once in f_init

    #TODO: consider other model initialisation e.g. using just an ML library

    # Loading surrogate models
    print(f"> Loading ES campaign with surrogates")
    camps = []
    mods = []

    if model_type in ['gpr', 'ann']:
        for ft in range(n_fts): #TODO: if settings reading can be moved before the main loop, easysurrogates can be initialised once
            try:
                camps.append(es.Campaign(load_state=True, file_path=f"{model_file_base}_{ft}.pickle"))
            except OSError:
                print(f"No such model file!")
                return
            mods.append(camps[-1].surrogate)
        print(f"> Got surrogates from a ES campaign")
    elif model_type in ['scikit_regression']:
        for ft in range(n_fts):
            try:
                mods.append(pickle.load(open(f"{model_file_base}_{ft}.pickle", 'br')))
            except OSError:
                print(f"No such model file!")
                return
        print(f"> Got surrogates from a scikitlearn pickle files")
    else:
        ValueError(f"This type of models is not supported")

    #TODO: read target names from campaign / database / m3-setting
    output_names = ['te_transp_flux', 'ti_transp_flux'] # MUSCLE3 currently does not support list of stings as a setting
    n_dim_out = len(output_names)

    int_iteration = 0
    # Model inference loop / simulation time iteration
    while instance.reuse_instance():

        # when is the instance constructed and destructed?
        print(f"> Entering a turbulence model iteration num {int_iteration}")

        # Get a message form equilibrium code: NOT USED HERE!
        msg_in_eq = instance.receive('equilibrium_in')
        print(f"> Got a message from EQUILIBRIUM")
        
        # Get a message from transport code
        msg_in = instance.receive('coreprof_in')
        print('> Got a message from TRANSP')

        ## Treat incoming COREPROF

        # Read timestamp
        num_it = msg_in.timestamp
        # # Update timestep number - this is done in transport component only here
        # num_it = msg_in_coreprof.timestamp + 1

        # Get profile byte array data from the message from TRANSP (and check what's inside)
        coreprof_in_data_bytes = msg_in.data

        # Save the binary buffer to a file
        devshm_file = f"/dev/shm/ets_coreprof_in_{id}_{num_it:.6f}.cpo"
        with open(devshm_file, "wb") as f:
            f.write(coreprof_in_data_bytes)
        start_t = t()
        coreprof_cpo_obj = read(devshm_file, "coreprof")
        print (f">> Reading CPO file {devshm_file} took {t()-start_t} s")

        # After reading the profile from temporary file, make an array
        #   returns profiles in order [te_value, te_ddrho, ti_value, ti_ddrho]
        profiles_in = coreprof_to_input_value(coreprof_cpo_obj,
                                              rho_ind_s=rho_ind_s, # not used if rho_tor_norm_sur is passed!
                                              rho_tor_norm=rho_tor_norm_sim, # flux tube locations in coretransp
                                              )

        profiles_in = profiles_in[input_names_ind_permut] #TODO: either fix original order, or store permutation separately
        #print(f"> Read incoming core profile \n {profiles_in}") ###DEBUG

        # Get (n_features, n_samples) from surrogate from (n_features, n_radial_points)
        # TODO should be (n_features, n_radial_points) -> (n_features, n_radial_points, n_samples)
        if profiles_in.shape[1] == 1:
            profiles_in.squeeze()
        profiles_in.reshape(-1,1)
        #print(f"> Reshaped core profile \n {profiles_in}") ###DEBUG

        # Read coreprof to either:
        #                   a. dictionary with keys being names of profiles and values being numpy arrays or lists
        #                   b+. numpy array of shape==(n_sample, n_profiles_dim, [n_rad_points]) and dtype=float

        ## Treat incoming EQUILIBRIUM
        if use_equilibrium:
            
            equilibrium_in_data_bytes = msg_in_eq.data

            # Save the equilibrium binary buffer to a file
            devshm_eq_file = f"/dev/shm/ets_equilibrium_in_{id}_{num_it:.6f}.cpo"
            with open(devshm_eq_file, "wb") as f:
                f.write(equilibrium_in_data_bytes)
            start_t = t()
            equilibrium_cpo_obj = read(devshm_eq_file, "equilibrium")
            print (f">> Reading CPO file {devshm_eq_file} took {t()-start_t} s")

            # After reading the equilibrium from temporary file, make an array of values
            #   returns in order [q, gm3]
            equilibrium_in = equilibrium_to_input_value(equilibrium_cpo_obj,
                                                rho_tor_norm=rho_tor_norm_sim, # flux tube locations in coretransp
                                                )

            equilibrium_in = equilibrium_in[input_eq_names_ind_permut]
            #print(f"> Read incoming equilibrium \n {equilibrium_in}") ###DEBUG

            # Combine coreprof and equilibrium (append latter to profiles_in)
            profiles_in = np.concatenate([profiles_in, equilibrium_in], axis=0)

        ## Infer a mean flux value using a surrogate
        #   NB!: Iterate over models and save result into a common data structure
        fluxes_out     = np.zeros((n_fts, n_dim_out))
        fluxes_out_std = np.zeros((n_fts, n_dim_out))

        # Check if input values are within learned bounds
        bool_outofbounds, dict_outofbounds = check_outof_learned_bounds(profiles_in, ref_bounds)
        
        # Infere QoI values for every flux tube
        for n_ft, r in enumerate(rho_ind_s): # actual value not used!
            
            if model_type == 'ann':
                f_o = mods[n_ft].predict(profiles_in[:,n_ft])
                f_o_std = np.zeros(f_o.shape)
            elif model_type == 'gpr':
                f_o, f_o_std = mods[n_ft].predict(profiles_in[:,n_ft].reshape(-1,1))
            elif model_type =='scikit_regression':
                f_o, f_o_std = mods[n_ft].predict(profiles_in[:,n_ft].reshape(1,-1), return_std=True)
            else:
                ValueError(f"This type of model is not supported")
            
            fluxes_out[n_ft,:] = f_o.reshape(-1)
            fluxes_out_std[n_ft,:] = f_o_std.reshape(-1)
        
        print(f"> Used a surrogate at rho_ind={rho_tor_norm_sur} to predict new Q_e,i={fluxes_out}")
        print(f"> Predicted STDs of new Q_e,i={fluxes_out_std}")

        #TODO: in principle with large scale separation local t_cur does not play role,
        #       but one could also estimate time for turbulence saturation with surrogate - a scan needed
        #TODO: find MUSCLE3 format for dataframes?
        #TODO: initialise the default data to fill in coretransp structures - look up GEM0 in Python - could be done once

        fluxes_out_dict = {k:fluxes_out[:, i] for i,k in enumerate(output_names)}

        #print(f"fluxes_out_dict: \n{fluxes_out_dict}") ###DEBUG
        
        coretransp_cpo_obj = output_value_to_coretransp(
                                        fluxes_out_dict, 
                                        coretransp_default_file_path, 
                                        r_s=ft_numbers,
                                        rho_tor_norm_sim=rho_tor_norm_sim,
                                        rho_tor_norm_sur=rho_tor_norm_sur,
                                        prof_names=['te_transp', 'ti_transp'],
                                        attributes=['flux']
                                                        )

        # Creating a bytes variable to be sent via MUSCLE3, coretransp_cpo_obj -> bytes
        #print(f"ti_transp.flux: \n{coretransp_cpo_obj.values[0].ti_transp.flux}") ###DEBUG
        
        file_like_profiles_out_data_str = io.StringIO('')
        write_fstream(file_like_profiles_out_data_str, coretransp_cpo_obj, "coretransp")
        coretransp_str = file_like_profiles_out_data_str.getvalue()
        coretransp_bytes = bytes(coretransp_str, "utf-8")

        # Writing uncertainty information
        te_transp_flux_cov = np.divide(fluxes_out_std[:,0], fluxes_out[:,0])
        ti_transp_flux_cov = np.divide(fluxes_out_std[:,1], fluxes_out[:,1])

        #coretransp_uncertainty = np.array([ti_transp_flux_cov, te_transp_flux_cov, bool_outofbounds]) # array version of uncertainty data to send
        coretransp_uncertainty = {
                        'rel_ti_transp_flux_std': ti_transp_flux_cov,
                        'rel_te_transp_flux_std': te_transp_flux_cov,
                        'dict_outofbounds': dict_outofbounds,
                        'bool_outofbounds': bool_outofbounds,
                                 }

        # Sending a coretransp message
        print(f"> Gettting ready an outcoming core transp")
        msg_out = Message(num_it, None, coretransp_bytes)
        msg_unc_out = Message(num_it, None, coretransp_uncertainty)
        print(f"> Sending an outcoming core transp")

        #if instance.should_save_snapshot():
        #    msg_snapshot = Message(num_it, data=fluxes_out_dict) #TODO: check how to save dict

        # Sending uncertainty information
        if bool_send_uncertainty:
            print(f"> In this configuration: sending uncertainty info")
            instance.send('coretransp_uncertainty_out', msg_unc_out)

        # Write down a file with suggested points for surrogate retraining
            
        file_retrain_name = f"new_surrogate_points_it{str(int_iteration).zfill(5)}.csv"

        new_surrogate_sample = make_new_training_sample(profiles_in, dict_outofbounds, ref_bounds, retrain_distance, n_fts)

        new_surrogate_sample.to_csv(file_retrain_name)

        # Sending a coretransp message (the results of a surrogate)

        instance.send('coretransp_out', msg_out)
        print(f"> Sent an outcoming core transp")

        int_iteration = int_iteration + 1 
        sys.stdout.flush()

        print(f"> End of iteration {int_iteration-1}")

    return int_iteration


if __name__ == '__main__':

    print(f"{os.getcwd()}") ###DEBUG
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    run_id = sys.argv[1] if len(sys.argv)>1 else r.randint(1000_0000_0000, 9999_9999_9999)

    iter_count = gem_surr_M3(id=run_id)
