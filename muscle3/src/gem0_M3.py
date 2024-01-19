import numpy as np
import pandas as pd

import logging

from libmuscle import Instance, Message, USES_CHECKPOINT_API
from ymmsl import Operator

from ascii_cpo import read, write_fstream, read_fstream, write

import io
import sys, os, shutil, subprocess

import easysurrogate as es

from time import time as t

from utils import coreprof_to_input_value, output_value_to_coretransp, training_data_bounds, check_outof_learned_bounds

gem0_path = "/u/yyudin/code/MFW/uq/basicda"
sys.path.append(gem0_path)
from extcodehelper import ExtCodeHelper

def gem0_M3():
    """
    MUSCLE3 implementation of GEM surrogate
    Creates an ML model from a .pickle file
    Receives messages with coreprof [,equilibrium] and send messages with coretransp 
    """

    input_names_ind_permut = [0,2,1,3] # permuation of input names relative to one used in EasySurrogate: ['te_value', 'te_ddrho', 'ti_value', 'ti_ddrho'] -> ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']

    # Creating a MUSCLE3 instance
    instance = Instance({
        Operator.O_F:     ['coretransp_out', 'coretransp_uncertainty_out'],
        Operator.F_INIT: ['coreprof_in', 'equilibrium_in'],
                        },
        #USES_CHECKPOINT_API,
                       )
    
    print(f"> Initialised turbulence instance") #DEBUG

    # Reading settings from a ymmsl file
    #ref_data_filename = instance.get_setting('training_dataset_filename', 'str')
    rho_ind_s = instance.get_setting('rho_ind_s', '[float]') #TODO : consider using function to calculate index from rho_tor; also can be read from the first coretranp received
    rho_tor_norm_sim = instance.get_setting('rho_tor_norm_sim', '[float]') #TODO: use interpolation on these, or ones from coretransp
    rho_tor_norm_sur = instance.get_setting('rho_tor_norm_sur', '[float]')
    #prof_out_names = instance.get_setting('profs_out', '[str]') #TODO: figure out how to pass lists of float/strings -> possible: ['float']/['str'] - not supported ATM
    coretransp_default_file_name = instance.get_setting('coretransp_default', 'str')
    equilibrium_default_file_name = instance.get_setting('equilibrium_default', 'str')
    coreprof_default_file_name = instance.get_setting('coreprof_default', 'str')
    #init_cpo_dir = instance.get_setting('init_cpo_dir', 'str')
    xml_default_file_name = instance.get_setting('xml_default', 'str') 

    # Setting up pathes, dimensionalities etc.

    prof_out_names = ["te_transp_flux", "ti_transp_flux"] # MUSCLE3 currently does not support list of stings as a setting
    n_dim_out = len(prof_out_names)

    rho_ind_s = [int(x) for x in rho_ind_s]
    n_fts = len(rho_ind_s)

    # Initialising code (GEM0) model
    print(f"> Creating pyGEM0 object")
    # eq_file = equilibrium_default_file_name
    # eq_file_loc = '/gem0_equilibrium_in.cpo'
    # command = [f"cp", f"{eq_file}", f"{eq_file_loc}"]
    # #os.symlink(eq_file, eq_file_loc)
    # #shutil.copy(eq_file, eq_file_loc)
    # ret = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    # #print(f"> equilibrium cpo copied : {ret.returncode} ; {ret.stdout} ; {ret.stderr}") ###DEBUG

    model = ExtCodeHelper(option=2, xml_file=xml_default_file_name, equilibrium_file=equilibrium_default_file_name, coreprof_file=coreprof_default_file_name)

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

        # Read timestamp
        num_it = msg_in.timestamp

        # Get profile and equilibrium byte array data from the message from TRANSP (and check what's inside)
        coreprof_in_data_bytes = msg_in.data
        equilibrium_in_data_bytes = msg_in_eq.data

        # Save the binary coreprof buffer to a file
        devshm_file = f"/dev/shm/ets_coreprof_in_{num_it:.6f}.cpo" #TODO: generate and store random name
        with open(devshm_file, "wb") as f:
            f.write(coreprof_in_data_bytes)
        #start_t = t()
        coreprof_cpo_obj = read(devshm_file, "coreprof")
        #print (f"> Reading CPO file {devshm_file} took {t()-start_t} s")

        # Save the binary equilibrium buffer to a file
        devshm_file_eq = f"/dev/shm/ets_equilibrium_in_{num_it:.6f}.cpo" #TODO: generate and store random name
        with open(devshm_file_eq, "wb") as f:
            f.write(equilibrium_in_data_bytes)
        #start_t = t()
        equilibrium_cpo_obj = read(devshm_file_eq, "equilibrium")
        #print (f"> Reading CPO file {devshm_file} took {t()-start_t} s")

        # Infere QoI values for every flux tube

        _,_,coretransp_cpo_obj = model.gem0_call_4param2target_cpo(equilibrium=equilibrium_cpo_obj, coreprof=coreprof_cpo_obj)
        
        #TODO: in principle with large scale separation local t_cur does not play role,
        #       but one could also estimate time for turbulence saturation with surrogate - a scan needed
        #TODO: find MUSCLE3 format for dataframes?
        
        # Creating a bytes variable to be sent via MUSCLE3, coretransp_cpo_obj -> bytes
        
        file_like_profiles_out_data_str = io.StringIO('')
        write_fstream(file_like_profiles_out_data_str, coretransp_cpo_obj, "coretransp")
        coretransp_str = file_like_profiles_out_data_str.getvalue()
        coretransp_bytes = bytes(coretransp_str, "utf-8")

        # Sending a coretransp message
        print(f"> Gettting ready an outcoming core transp")
        msg_out = Message(num_it, None, coretransp_bytes)
        print(f"> Sending an outcoming core transp")

        #if instance.should_save_snapshot():
        #    msg_snapshot = Message(num_it, data=fluxes_out_dict) #TODO: check how to save dict

        # Sending a coretransp message (the results of a surrogate)

        instance.send('coretransp_out', msg_out)
        print(f"> Sent an outcoming core transp")

        int_iteration = int_iteration + 1 
        sys.stdout.flush()

        print(f"> End of iteration {int_iteration-1}")

    return int_iteration


if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.INFO)

    iter_count = gem0_M3()
