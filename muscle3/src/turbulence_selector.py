import logging
from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

import pandas as pd
import numpy as np

def turbulence_model_selector():
    """
    Send and recieve data from transport and equilibrium components;
    Inside, chose which implementation of turbulence model to call
    """

    # Creating a MUSCLE3 instance
    instance = Instance({
        Operator.O_F:     ['coretransp_out',],
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
        Operator.S: ['coretransp_fromsim_in', 'coretransp_fromsur_in', 'coretransp_uncertainty_fromsur_in'],
        Operator.O_I: ['equilibrium_tosim_out', 'equilibrium_tosur_out', 'coreprof_tosim_out', 'coreprof_tosur_out'],
                        },
                       )
    
    print(f"> Initialised turbulence selector")

    bool_call_sim = False # initial value of bollean to run simulation

    COV_CRITERION_USE = False # sets if criteria should concider Coefficent of Variance of model uncertainty

    # Ordering of additional parameteres returned by surrogate component
    coretransp_uncertainty_dict = {
        'rel_ti_transp_flux_std': 0,
        'rel_te_transp_flux_std': 1,
        'bool_outofbounds':       2,
    }

    # Threshold for Coefficient of Variation of surrogate output
    ti_transp_flux_cov_reltol = 1e+0 #TODO: move to settings

    while instance.reuse_instance():

        print(f"> Entering a turbulence selector iteration")

        # Get a message form equilibrium code
        msg_in_equilibrium = instance.receive('equilibrium_in')
        print('> Got a message from EQUILIBRIUM')
        
        # Get a message from transport code
        msg_in_coreprof = instance.receive('coreprof_in')
        print('> Got a message from TRANSP')

        # # Update timestep number
        # num_it = msg_in_coreprof.timestamp + 1
        # Read timestamp
        num_it = msg_in_coreprof.timestamp
        print(f"> Iteration for time: {num_it}")

        # Get profile and equilibrium byte array data from the messages
        equilibrium_in_data_bytes = msg_in_equilibrium.data
        coreprof_in_data_bytes = msg_in_coreprof.data

        ### Model selection part start
        # 1. Pass coreprofile to surrogate implementation

        msg_out_coreprof = Message(num_it, None, coreprof_in_data_bytes)
        msg_out_equilibrium = Message(num_it, None, equilibrium_in_data_bytes)

        print(f"> Sending coreprof and equilibrium to TURBULENCE_SUR")

        instance.send('coreprof_tosur_out', msg_out_coreprof)
        instance.send('equilibrium_tosur_out', msg_out_equilibrium)

        # 2. Get coretranps (fluxes and their uncertainties) from surrogate implementation

        msg_in_coretransp = instance.receive('coretransp_fromsur_in')

        msg_in_coretransp_uncertainty = instance.receive('coretransp_uncertainty_fromsur_in')

        print(f"> Received coretransp from TURBULENCE_SUR")

        coretransp_bytes = msg_in_coretransp.data

        coretransp_uncertainty = msg_in_coretransp_uncertainty.data #data.array.copy()

        # 3. Criterion to run simulation

        # Check if uncertainties exceed threshold
        ti_transp_flux_cov = coretransp_uncertainty['rel_ti_transp_flux_std'].array #[coretransp_uncertainty_dict['rel_ti_transp_flux_std']]
        print(f"> Coefficient of Variation at ft {0} against {ti_transp_flux_cov_reltol} is: {ti_transp_flux_cov[0]:.3f}") ###DEBUG

        if np.any(ti_transp_flux_cov > ti_transp_flux_cov_reltol) and COV_CRITERION_USE:
            bool_call_sim = True
        
        #bool_call_sim = not bool_call_sim # stub to simply alternate between two implementations

        # Check if input values are within learned bounds
        bool_outofbounds = coretransp_uncertainty['bool_outofbounds'] #[coretransp_uncertainty_dict['bool_outofbounds']]
       
        if bool_outofbounds:
            bool_call_sim = True
            print(f"> Attention: the input profiles are outside of the support of the surrogate!")

        # 4. If the criterion is satisfied, pass coreprofile and equilibrium to simulation implementation, 
        #           then get back the coretransp
        #    (if criterion is satisfied, overwrite the coretransp from surrogate)

        if bool_call_sim:

            print(f"> Sending coreprof and equilibrium to TURBULENCE_SIM")

            instance.send('coreprof_tosim_out', msg_out_coreprof)
            instance.send('equilibrium_tosim_out', msg_out_equilibrium)

            msg_in_coretransp = instance.receive('coretransp_fromsim_in')

            print(f"> Received coretransp from TURBULENCE_SIM")
            
            coretransp_bytes = msg_in_coretransp.data
        
        else:
            pass

        # 5. Send the coretransp to transport component
        ### Model selection part finish

        # Sending a coretransp message
        msg_out = Message(num_it, None, coretransp_bytes)
        instance.send('coretransp_out', msg_out)
        print('> Sent an outcoming coretransp')

    return 0


if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    turbulence_model_selector()