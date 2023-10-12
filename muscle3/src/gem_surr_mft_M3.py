import numpy as np
import pandas as pd

import logging

from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

from ascii_cpo import read, write_fstream, read_fstream, write

#from ctypes import c_char, string_at
import io

import easysurrogate as es

from time import time as t

from utils import coreprof_to_input_value, output_value_to_coretransp, training_data_bounds, check_outof_learned_bounds

def gem_surr_M3():
    """
    MUSCLE3 implementation of GEM surrogate
    Creates an ML model from a .pickle file
    Receives messages with coreprof [,equilibrium] and send messages with coretransp 
    """

    input_names_ind_permut = [0,2,1,3] # permuation of input names relative to one used in EasySurrogate

    # Creating a MUSCLE3 instance
    instance = Instance({
        Operator.O_F:     ['coretransp_out', 'coretransp_uncertainty_out'],
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
                        },
        #USES_CHECKPOINT_API,
                       )
    
    print(f"> Initialised turbulence instance") #DEBUG

    # Reference training data CSV used to train surrogate
    #ref_data_filename = 'ref_train_data.csv'
    ref_data_filename = instance.get_setting('training_dataset_filename', 'str')
    ref_data = pd.read_csv(ref_data_filename, sep=',')
    ref_bounds = training_data_bounds(ref_data)

    # Reading setting from a ymmsl file
    rho_ind_s = instance.get_setting('rho_ind_s', '[float]') #TODO: consider using function to calculate index from rho_tor
    #output_names = instance.get_setting('profs_out', '[str]') #TODO: figure out how to pass lists of float/strings -> possible: ['float']/['str']
    model_file_base = instance.get_setting('surrogate_path', 'str')
    coretransp_default_file_name = instance.get_setting('cortransp_default', 'str')
    init_cpo_dir = instance.get_setting('init_cpo_dir', 'str')

    # Setting up pathes, dimensionalities
    #coretransp_default_file_path = init_cpo_dir + '/' + coretransp_default_file_name
    coretransp_default_file_path = coretransp_default_file_name

    prof_out_names = ["te_transp_flux", "ti_transp_flux"] # MUSCLE3 currently does not support list of stings as a setting
    n_dim_out = len([prof_out_names])

    rho_ind_s = [int(x) for x in rho_ind_s]
    n_fts = len(rho_ind_s)

    # Initialising surrogate model
    #   for a one-shot training case should only be done once in f_init

    #TODO: consider other model initialisation e.g. using just an ML library
    #TODO: consider single model load per MUSCLE3 Manager run

    # Loading surrogate models
    print('> Loading ES campaign with surrogates')
    camps = []
    mods = []

    for ft in range(n_fts): #TODO: if settings reading can be moved before the main loop, easysurrogates can be initialised once
        try:
            camps.append(es.Campaign(load_state=True, file_path=f"{model_file_base}_{ft}.pickle"))
        except OSError:
            print('No such model file!')
            return
        mods.append(camps[-1].surrogate)
    print('> Got surrogates from a ES campaign')

    #TODO: read target names from campaign
    output_names = ['te_transp_flux', 'ti_transp_flux']
    #output_names = ['ti_transp_flux']

    # Model inference loop / simulation time iteration
    while instance.reuse_instance():

        # when is the instance constructed and destructed?
        print(f"> Entering a turbulence model iteration")

        # Get a message form equilibrium code: NOT USED HERE!
        msg_in_eq = instance.receive('equilibrium_in')
        print('> Got a message from EQUILIBRIUM')
        
        # Get a message from transport code
        msg_in = instance.receive('coreprof_in')
        print('> Got a message from TRANSP')

        # # Update timestep number
        # num_it = msg_in_coreprof.timestamp + 1
        # Read timestamp
        num_it = msg_in.timestamp

        # Get profile byte array data from the message from TRANSP (and check what's inside)
        coreprof_in_data_bytes = msg_in.data

        # Save the binary buffer to a file
        devshm_file = f"/dev/shm/ets_coreprof_in_{num_it:.6f}.cpo" #TODO: generate and store random name
        with open(devshm_file, "wb") as f:
            f.write(coreprof_in_data_bytes)
        start_t = t()
        coreprof_cpo_obj = read(devshm_file, "coreprof")
        print (f"> Reading CPO file {devshm_file} took {t()-start_t} s")

        # After reading the profile from temporary file, make an array
        profiles_in = coreprof_to_input_value(coreprof_cpo_obj, rho_ind_s,)
        profiles_in = profiles_in[input_names_ind_permut] #TODO: either fix original order, or store permutation separately
        print(f"> Read incoming core profile \n {profiles_in}")

        # Get (n_features, n_samples) from surrogate from (n_features, n_radial_points)
        # TODO should be (n_features, n_radial_points) -> (n_features, n_radial_points, n_samples)
        if profiles_in.shape[1] == 1:
            profiles_in.squeeze()
        profiles_in.reshape(-1,1)
        #print(f"> Reshaped core profile \n {profiles_in}") ###DEBUG

        # Read coreprof to either:
        #                   a. dictionary with keys being names of profiles and values being numpy arrays or lists
        #                   b+. numpy array of shape==(n_sample, n_profiles_dim, [n_rad_points]) and dtype=float

        #TODO use a surrogate for a vector output: ['te.transp.flux', 'ti.transp.flux']
        # Infer a mean flux value using a surrogate
        #   NB!: Iterate over models and save result into a commod data structure
        fluxes_out     = np.zeros((n_fts, len(output_names)))
        fluxes_out_std = np.zeros((n_fts, len(output_names)))

        # Check if input values are within learned bounds
        bool_outofbounds, dict_outofbounds = check_outof_learned_bounds(profiles_in, ref_bounds)
        
        # Infere QoI values for every flux tube
        for n_ft, r in enumerate(rho_ind_s):
            f_o, f_o_std = mods[n_ft].predict(profiles_in[:,n_ft].reshape(-1,1))
            fluxes_out[n_ft,:] = f_o.reshape(-1)
            fluxes_out_std[n_ft,:] = f_o_std.reshape(-1)
        
        print(f"> Used a surrogate at rho_ind={r} to predict new Q_e,i={fluxes_out}")
        print(f"> Predicted STDs of new Q_e,i={fluxes_out_std}")

        #TODO: in principle with large scale separation local t_cur does not play role,
        # but one could also estimate time for turbulence saturation with surrogate 
        #TODO: find MUSCLE3 format for dictionaries or dataframes
        #TODO: initialise the default data to fill in coretransp structures - look up GEM0 in Python

        fluxes_out_dict = {k:fluxes_out[:, i] for i,k in enumerate(output_names)}

        print(f"fluxes_out_dict: \n{fluxes_out_dict}")
        
        coretransp_cpo_obj = output_value_to_coretransp(
                                        fluxes_out_dict, 
                                        coretransp_default_file_path, 
                                        r_s = [i for i,r in enumerate(rho_ind_s)],
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
        #coretransp_uncertainty = [ti_transp_flux_cov, te_transp_flux_cov, bool_outofbounds]
        #coretransp_uncertainty = np.array(coretransp_uncertainty) # array version of uncertainty data to send
        coretransp_uncertainty = {
                        'rel_ti_transp_flux_std': ti_transp_flux_cov,
                        'rel_te_transp_flux_std': te_transp_flux_cov,
                        'dict_outofbounds': dict_outofbounds,
                        'bool_outofbounds': bool_outofbounds,
                                 }

        # Sending a coretransp message
        print('> Gettting ready an outcoming core transp')
        msg_out = Message(num_it, None, coretransp_bytes)
        msg_unc_out = Message(num_it, None, coretransp_uncertainty)
        print('> Sending an outcoming core transp')

        #if instance.should_save_snapshot():
        #    msg_snapshot = Message(num_it, data=fluxes_out_dict) #TODO: check how to save dict

        instance.send('coretransp_out', msg_out)
        instance.send('coretransp_uncertainty_out', msg_unc_out)
        print('> Sent an outcoming core transp')


if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    gem_surr_M3()
