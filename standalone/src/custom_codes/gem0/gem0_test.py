#!/usr/bin/env python3

#import gem0
#from gem0 import gem

import numpy as np
import matplotlib.pyplot as plt

#from euitm_schemas import  type_coreprof, type_equilibrium, type_coretransp
#from read_structures import open_read_file, close_read_file, read_cpo
#from write_structures import open_write_file, close_write_file, write_cpo
#from deallocate_structures import deallocate_cpo

from gem0_singleton import GEM0Singleton

# init_step = 0 # inital step count

def get_batch(basefolder):
    folder_list = []
    # TODO: get batch representation so that a list of modified gem0singletone files could be created
    # a list of input file folder locations
    return folder_list

def run_batch(folder_list):
    # TODO: for every input folder modify/create gem0singleton and run gem0
    # should actually call easyvvuq-qcgpj?!
    return 0

def plot_res(x, y):
    plt.plot(x, y, label='python-gem0 response in ti')
    plt.savefig('pythongem0_tifl_vs_tival')
    plt.close()

def run_mult_ios():
    #TODO should probably use easyvvuq-qcgpj?
    gem0_object = GEM0Singleton()
    tifls = []
    tidrs = []
    tevl, tivl, tegr, tigr = gem0_object.get_curr_params()
    #tigrs = np.linspace(0.5*tigr, 1.5*tigr, 100)
    tivls = np.linspace(0.5*tivl, 1.5*tivl, 20)
    param = {'ti.value': tevl, 'te.value': tivl, 'te.ddrho': tegr, 'ti.ddrho': tigr}
    #for ti_grad_val in tigrs:
    for ti_val_val in tivls:
        #param['ti.ddrho'] = ti_grad_val 
        param['ti.value'] = ti_val_val
        _, tifl, _, tidr = gem0_object.gem0_call(param)
        tifls.append(tifl)
        tidrs.append(tidr)
    
    #plot_res(tigrs, tifls)
    plot_res(tivls, tifls)
    return tidrs


if __name__ == '__main__':
    gem0_object = GEM0Singleton()
    gem0_object.gem0_test()

    #if it's index 69 for AUG_28906_6/*coreprof.cpo, so:
    #∇Te = -3138.03503336047
    #∇Ti  = -2420.64001419732


    #tifls = run_mult_ios()
