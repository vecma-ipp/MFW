import logging
from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

import pandas as pd
import numpy as np
import io
import sys

from ascii_cpo import read, write_fstream

from utils import output_value_to_coretransp, coreprof_to_input_value, coretransp_to_value

def turbulence_model_manager():
    """
    Send and recieve data from transport and equilibrium components;
    Inside, chose which implementation of turbulence model to call
    """

    # Creating a MUSCLE3 instance
    instance = Instance({
        Operator.O_F:     ['coretransp_out',],
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
        Operator.S: ['coretransp_from_hifi_in', 'coretransp_from_lofi_in', 'coretransp_from_surr_in', 'coretransp_uncertainty_from_surr_in'],
        Operator.O_I: ['equilibrium_to_hifi_out', 'equilibrium_to_lofi_out', 'equilibrium_to_surr_out', 'coreprof_to_hifi_out','coreprof_to_lofi_out', 'coreprof_to_surr_out'],
                        },
                       )
    
    print(f"> Initialised turbulence selector")

    bool_call_hifi = False
    bool_call_lofi = False

    COV_USE_CRITERION = False
    LOFI_USE = True

    # Ordering of additional parameteres returned by surrogate component
    coretransp_uncertainty_dict = {
        'rel_ti_transp_flux_std': 0,
        'rel_te_transp_flux_std': 1,
        'bool_outofbounds':       2,
    }

    input_names_ind_permut = [0,2,1,3] # permuation of input names relative to one used in EasySurrogate

    # Threshold for Coefficient of Variation of surrogate output - TODO put in workflow settings
    ti_transp_flux_cov_reltol = 1e+0
    te_transp_flux_cov_reltol = 1e+0

    rho_ind_s = instance.get_setting('rho_ind_s', '[float]') # flux tubes / radial locations in coretransp / turbulence model
    #input_names = instance.get_setting('input_names', '[str]') # currently, MUSCLE3 does not support list of strings
    #output_names = instance.get_setting('output_names', '[str]') # currently, MUSCLE3 does not support list of strings
    coretransp_default_file_name = instance.get_setting('cortransp_default', 'str')

    rho_ind_s = [int(x) for x in rho_ind_s]

    input_names = ["te_value", "ti_value", "te_ddrho", "ti_ddrho"]
    output_names = ["te_transp_flux", "ti_transp_flux"]

    n_fts = len(rho_ind_s)
    n_inputs = len(input_names)
    n_outputs = len(output_names)

    int_iteration = 0 # counter of iterations in this script
    
    int_lofi_called = 0
    int_hifi_called = 0

    # Iteration of turbulence_manager usage
    while instance.reuse_instance():

        print(f"> Entering a turbulence manager iteration num {int_iteration}")

        # Get a message form equilibrium code
        msg_in_equilibrium = instance.receive('equilibrium_in')
        print(f"> Got a message from EQUILIBRIUM")
        
        # Get a message from transport code
        msg_in_coreprof = instance.receive('coreprof_in')
        print(f"> Got a message from TRANSP")

        # # Update timestep number
        # num_it = msg_in_coreprof.timestamp + 1
        # Read timestamp
        num_it = msg_in_coreprof.timestamp
        print(f"> Iteration for time: {num_it}")

        # Get profile and equilibrium byte array data from the messages
        equilibrium_in_data_bytes = msg_in_equilibrium.data
        coreprof_in_data_bytes = msg_in_coreprof.data

        ### Model selection and combination part start
        # 1. Pass coreprofile to surrogate implementation

        msg_out_coreprof = Message(num_it, None, coreprof_in_data_bytes)
        msg_out_equilibrium = Message(num_it, None, equilibrium_in_data_bytes)

        print(f"> Sending coreprof and equilibrium to TURBULENCE_SURR")

        instance.send('coreprof_to_surr_out', msg_out_coreprof)
        instance.send('equilibrium_to_surr_out', msg_out_equilibrium)

        # 2. Get coretranps (fluxes and their uncertainties) from surrogate implementation

        msg_in_coretransp = instance.receive('coretransp_from_surr_in')

        msg_in_coretransp_uncertainty = instance.receive('coretransp_uncertainty_from_surr_in')

        print(f"> Received coretransp from TURBULENCE_SURR")

        coretransp_bytes = msg_in_coretransp.data

        coretransp_uncertainty = msg_in_coretransp_uncertainty.data #array.copy()
        
        # 3. If criterion for combination of surrogate and lo-fi model works, fill in the flux values arrays with data from surr of lofi

        # 3.0. Get data about uncertainties of a surrogate 

        bool_outofbounds = coretransp_uncertainty['bool_outofbounds'] #[coretransp_uncertainty_dict['bool_outofbounds']]
        dict_outofbounds = coretransp_uncertainty['dict_outofbounds'] # this should be a dictionary!

        #print(f"dict_outofbounds: \n{dict_outofbounds}") ###DEBUG

        # 3.1. If at least one value is outside of learned support, call a low-fidelity model
        #   Criterion 1: if only a subset of flux tubes have its fluxes values out of learned bound
        print(f"> checking if lofi should be called: {bool_outofbounds}") ###DEBUG
        bool_call_lofi = bool_outofbounds
        
        if bool_call_lofi and LOFI_USE:

            print(f"> Attention: some of the input profiles are outside of the support of the surrogate!")

            print(f"> Sending coreprof and equilibrium to TURBULENCE_LOFI")

            instance.send('coreprof_to_lofi_out', msg_out_coreprof)
            instance.send('equilibrium_to_lofi_out', msg_out_equilibrium)

            msg_in_lofi_coretransp = instance.receive('coretransp_from_lofi_in')

            print(f"> Received coretransp from TURBULENCE_LOFI")

            coretransp_bytes_lofi = msg_in_lofi_coretransp.data

            # 3.2 Combine values from surrogate and low-fidelity model: use lo-fi when surrogate is out of its bounds
        
            #   Read relevant coretransp arrays
            print("> reading coretransp from surr and lofi") ###DEBUG
            #       surrogate datastructure
            devshm_file_surr = f"/dev/shm/surr_coretransp_in_{num_it:.6f}.cpo" #TODO: generate and store random name
            with open(devshm_file_surr, "wb") as f:
                f.write(coretransp_bytes)
            coretransp_cpo_obj_surr = read(devshm_file_surr, "coretransp")
            coretransp_cpo_array_surr = coretransp_to_value(
                                                        coretransp_cpo_obj_surr, 
                                                        rho_ind_s=[i for i,r in enumerate(rho_ind_s)],
                                                        prof_names=['te_transp', 'ti_transp'], 
                                                        attrib_names=['flux'],
                                                                )

            #       low-fidelity datastructure
            devshm_file_lofi = f"/dev/shm/lofi_coretransp_in_{num_it:.6f}.cpo" #TODO: generate and store random name
            with open(devshm_file_lofi, "wb") as f:
                f.write(coretransp_bytes_lofi)
            coretransp_cpo_obj_lofi = read(devshm_file_lofi, "coretransp")
            coretransp_cpo_array_lofi = coretransp_to_value(
                                                        coretransp_cpo_obj_lofi, 
                                                        rho_ind_s=[i for i,r in enumerate(rho_ind_s)], 
                                                        prof_names=['te_transp', 'ti_transp'], 
                                                        attrib_names=['flux'],
                                                                ) # extract names from the list of output names

            #TODO: look up reading functions for coretransp
            print("> filling in arrays from surr and lofi") ###DEBUG
            # Fill in new array with right values
            #   fill in array of bools for every flux tube, True if values are inside bounds
            bool_coord_inbounds = np.ones(n_fts, dtype=bool)
            for feature,dict_bounds in dict_outofbounds.items():
                for i,bool_coord in enumerate(dict_bounds['within'].array):
                    bool_coord_inbounds[i] = bool_coord_inbounds[i] and bool_coord
                    print(f">> {feature} @ft#{i} is in bounds: {bool_coord}")
            
            print(f"> bool_coord_inbounds = {bool_coord_inbounds}") ###DEBUG

            fluxes_out = np.zeros((n_fts, n_outputs), dtype=float)
            for i,bool_coord_ib in enumerate(bool_coord_inbounds):
                if bool_coord_ib:
                    fluxes_out[i,:] = coretransp_cpo_array_surr[:,i]
                else:
                    fluxes_out[i,:] = coretransp_cpo_array_lofi[:,i]

            fluxes_out_dict = {k:fluxes_out[:, i] for i,k in enumerate(output_names)}

            # Put the array in new coretransp datastructure
            print(f"> creating a new message for coretransp") ###DEBUG
            coretransp_cpo_obj = output_value_to_coretransp(
                                        fluxes_out_dict, 
                                        coretransp_default_file_name, 
                                        r_s=[i for i,r in enumerate(rho_ind_s)],
                                        prof_names=['te_transp', 'ti_transp'],
                                        attributes=['flux']
                                                        )
            
            file_like_transp_out_data_str = io.StringIO('')
            write_fstream(file_like_transp_out_data_str, coretransp_cpo_obj, "coretransp")
            coretransp_str = file_like_transp_out_data_str.getvalue()
            coretransp_bytes = bytes(coretransp_str, "utf-8")

            int_lofi_called = int_lofi_called + 1

        # 4. Criterion to run simulation / criterion to use high-fidelity model
        print(f"> checking if hifi should be called") ###DEBUG
        # Criterion 0: stub
        #bool_call_hifi = not bool_call_hifi # stub to simply alternate between two implementations

        # Criterion 1: check if uncertainties (QoI CoV) exceed threshold - disabled with COV_USE_CRITERION
        ti_transp_flux_cov = coretransp_uncertainty['rel_ti_transp_flux_std'].array #[coretransp_uncertainty_dict['rel_ti_transp_flux_std']]
        te_transp_flux_cov = coretransp_uncertainty['rel_te_transp_flux_std'].array #[coretransp_uncertainty_dict['rel_te_transp_flux_std']]
        #print(f"> Coefficient of Variation against {ti_transp_flux_cov_reltol} is: {ti_transp_flux_cov:.3f}") ###DEBUG

        if np.any(ti_transp_flux_cov > ti_transp_flux_cov_reltol) and np.any(te_transp_flux_cov > te_transp_flux_cov_reltol) and COV_USE_CRITERION:
            bool_call_hifi = True

        # Criterion 2: check if all input values are outside learned bounds   
        bool_outofbounds_total = not sum([sum(v['within'].array) for k,v in dict_outofbounds.items()]) # should be 1 if all values for 'within' were False; could be implemented with AND and without processing 'not'
        print(f" All input parameters are outise of bounds: {bool_outofbounds_total}") ###DEBUG

        if bool_outofbounds_total:
            bool_call_hifi = True # redundant, could be replaced by RHS value and print moved tp the next IF
            print(f"> Attention: all input profiles are outside of the support of the surrogate!")

        # OVERRIDE: never call hifi
        bool_call_hifi = False

        # 5. If the criterion is satisfied, pass coreprofile and equilibrium to simulation implementation, 
        #           then get back the coretransp - does not use any information from the low-fidelity model does not chose between two
        #    (if criterion is satisfied, overwrite the coretransp from surrogate)

        if bool_call_hifi:

            print(f"> Sending coreprof and equilibrium to TURBULENCE_HIFI")

            instance.send('coreprof_to_hifi_out', msg_out_coreprof)
            instance.send('equilibrium_to_hifi_out', msg_out_equilibrium)

            msg_in_coretransp = instance.receive('coretransp_from_hifi_in')

            print(f"> Received coretransp from TURBULENCE_HIFI")
            
            coretransp_bytes = msg_in_coretransp.data

            int_hifi_called = int_hifi_called + 1
        
        else:

            print(f" Not using high-fidelity model on this iteration")
            #pass # redundant, may be of use later

        # 6. Send the coretransp to transport component
        ### Model selection and combination part finish

        # Sending a coretransp message
        msg_out = Message(num_it, None, coretransp_bytes)
        instance.send('coretransp_out', msg_out)
        print(f"> Sent an outcoming coretransp")

        int_iteration = int_iteration + 1 
        sys.stdout.flush()

    return int_lofi_called, int_hifi_called


if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    lofi_count, hifi_count = turbulence_model_manager()

    print(f" Counted lofi model times: {lofi_count} \n Counted hifi model times: {hifi_count}")
