import logging
from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator


def turbulence_model_selector():
    """
    Send and recieve data from transport and equilibrium components;
    Inside, chose which implementation of turbulence model to call
    """

    # Creating a MUSCLE3 instance
    instance = Instance({
        Operator.O_F:     ['coretransp_out',],
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
        Operator.S: ['coretransp_fromsim_in', 'coretransp_fromsur_in'],
        Operator.O_I: ['equilibrium_tosim_out', 'equilibrium_tosur_out', 'coreprof_tosim_out', 'coreprof_tosur_out'],
                        },
                       )
    
    print(f"> Initialised turbulence selector")

    ti_transp_flux_std_reltol = 1e+0

    bool_call_sim = False

    while instance.reuse_instance():

        print(f"> Entering a turbulence selector iteration")

        # Get a message form equilibrium code
        msg_in_equilibrium = instance.receive('equilibrium_in')
        print('> Got a message from EQUILIBRIUM')
        
        # Get a message from transport code
        msg_in_coreprof = instance.receive('coreprof_in')
        print('> Got a message from TRANSP')

        # Update timestep number
        num_it = msg_in_coreprof.timestamp + 1

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

        print(f"> Received coretransp from TURBULENCE_SUR")

        coretransp_bytes = msg_in_coretransp.data

        #TODO!!!: pass uncertainty information in coretransp CPO 

        # 3. Criterion: check if uncertainties exceed threshold

        bool_call_sim = not bool_call_sim 
        # NB: stab to check two implementations, just alternate between implementations

        #ti_transp_flux_std > ti_transp_flux_std_reltol:
        #    bool_call_sim = True

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