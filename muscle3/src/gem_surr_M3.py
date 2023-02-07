import numpy as np
import pandas as pd

import logging

from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

from ascii_cpo import read
import base

import easysurrogate as es


def coreprof_to_input_value(
            data, 
            rho_ind_s, 
            prof_names=['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho',], 
            attrib_names=['',],
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

    for i, prof_name in enumerate(prof_names):

        prof = getattr(data.core.values[0], prof_name)

        for j, attrib_name in enumerate(attrib_names):

            for r, rho_ind in enumerate(rho_ind_s):

                val_reading = getattr(prof, attrib_name)[rho_ind]

                if prof_name[1] == 'i':
                    val_reading = val_reading[0]

                elif prof_name[1] == 'e':
                    pass
                
                else:
                    print('Error: Attributes have to belong either to ions or to electrons')

                prof_vals[i*m+j][r] = val_reading

    return prof_vals

def output_value_to_coretransp(
            fluxes_out, 
            coretransp_file, 
            r_s = [1],
            ion = 0,
            prof_names=['ti_transp', 'te_transp',], #TODO: double check CPO format
            attributes=['flux',]
                              ):
    """
    Transforms flux mean values infered by model into a CPO coretransp datastracture
    """

    coretransp_datastructure = read(coretransp_file, 'coretransp')

    for prof_name in prof_names:

        for attribute in attributes:

            for r in r_s:

                if attribute == 'flux':

                    if prof_name == 'ti_transp':
                        coretransp_datastructure.values[0].ti_transp.flux[r, ion] = fluxes_out[prof_name+'_'+attribute]
                    elif prof_name == 'te_transp':
                        coretransp_datastructure.values[0].te_transp.flux[r, ion] = fluxes_out[prof_name+'_'+attribute]
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
        Operator.F_INIT: ['coreprof_in',],
        Operator.F_INIT: ['equilibrium_in',],
        Operator.O_F:    ['coretransp_out',],
                        },
        #USES_CHECKPOINT_API,
                       )

    while instance.reuse_instance():
        # when is the instance constructed and destructed?

        rho_ind_s = instance.get_setting('rho_ind_s', 'float') #TODO: consider using function to calculate index fron rho_tor
        prof_out_names = instance.get_setting('profs_out', 'string') #TODO: figure out how to pass lists of float/strings
        model_file = instance.get_setting('surrogate_path', 'string')
        coretransp_default_file_name = instance.get_setting('cortransp_default', 'string')
        init_cpo_dir = instance.get_setting('init_cpo_dir', 'string')

        coretransp_default_file_path = init_cpo_dir + '/' + coretransp_default_file_name
        n_dim_out = len([prof_out_names])

        # Initialising surrogate model
        #   for a one-shot case should only be done once in f_init

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

        #TODO: read target names from campaign
        output_names = ['ti_transp_flux', 'te_transp_flux']

        # Get a message form equilibrium code: NOT USED HERE!
        msg_in = instance.receive('equilibrium_in')
        print('> Got a message from EQUILIBRIUM')
        
        # Get a message from transport code
        msg_in = instance.receive('coreprof_in')
        print('> Got a message from TRANSOP')

        num_it = msg_in.timestamp + 1

        profiles_in_data = msg_in.data.copy()
        profiles_in = coreprof_to_input_value(profiles_in_data, [rho_ind_s],)
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
        print('> Used a surrogate to predict new Qi={0}'.format(fluxes_out))

        #TODO: in principle with large scale separation local t_cur does not play role,
        # but one could also estimate time for turbulence saturation with surrogate 
        #TODO: find MUSCLE3 format for dictionaries or dataframes
        #TODO: initialise the default data to fill in coretransp structures - look up GEM0 in Python

        fluxes_out_dict = {k:fluxes_out[i] for i,k in enumerate(output_names)}
        
        core_transp_datastructure = output_value_to_coretransp(fluxes_out_dict, coretransp_default_file_path)

        print('> Gettting ready an outcoming core transp')
        msg_out = Message(num_it, None, core_transp_datastructure)
        print('> Sending an outcoming core transp')

        #if instance.should_save_snapshot():
        #    msg_snapshot = Message(num_it, data=fluxes_out_dict) #TODO: check how to save dict

        instance.send('coretransp_out', msg_out)
        print('> Sent an outcoming core transp')



if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    gem_surr_M3()
