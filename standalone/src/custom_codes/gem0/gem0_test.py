#!/usr/bin/env python3

#import gem0
#from gem0 import gem

import numpy as np
import matplotlib.pyplot as plt

#from euitm_schemas import type_coreprof, type_equilibrium, type_coretransp
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
 
def plot_gem0_res(x, y):
    plt.plot(x, y, label='python-gem0 response in tigr')
    plt.xlabel('tegr')
    plt.ylabel('tefl')
    plt.savefig('pythongem0_tefl_vs_tegr.png')
    plt.close()

def run_mult_ios():
    #TODO should probably use easyvvuq-qcgpj?
    gem0_object = GEM0Singleton()
    tifls = []
    tigrs = []
    tefls = []
    tegrs = []
    tevl, tivl, tegr, tigr = gem0_object.get_curr_params()
    tigrs = np.linspace(0.5*tigr, 1.5*tigr, 100)
    tivls = np.linspace(0.5*tivl, 1.5*tivl, 20)
    tegrs = np.linspace(0.5*tegr, 1.5*tegr, 100)
    tevls = np.linspace(0.5*tevl, 1.5*tevl, 20)
    param = {'ti.value': tevl, 'te.value': tivl, 'te.ddrho': tegr, 'ti.ddrho': tigr}
    #for ti_grad_val in tigrs:
    #for ti_val_val in tivls:
    for te_grad_val in tegrs:
    #for te_val_val in tevls:
        #param['ti.ddrho'] = ti_grad_val 
        #param['ti.value'] = ti_val_val
        param['te.ddrho'] = te_grad_val
        #param['te.value'] = te_val_val
        tefl, tifl, tegr, tigr = gem0_object.gem0_call(param)
        tifls.append(tifl)
        tefls.append(tefl)
        #tigrs.append(tigr)
    
    #plot_res(tigrs, tifls)
    #plot_gem0_res(tegrs, tifls)
    plot_gem0_res(tegrs, tefls)
    return tifls, tigrs


if __name__ == '__main__':
    gem0_object = GEM0Singleton()
    gem0_object.gem0_test()

    #TODO: add grammar checks to code base

    #if it's index 69 for AUG_28906_6/*coreprof.cpo, so:
    #∇Te = -3138.03503336047
    #∇Ti  = -2420.64001419732

    tidrs = run_mult_ios()
