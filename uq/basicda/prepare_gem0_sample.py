
import sys

import numpy as np
import pandas as pd

from da_utils import read_cpo_file, write_gem0_fromfile, write_profs_fromfile_grid


if __name__ == '__main__':

    # Get the run and iteration identifiers
    folder_in = sys.argv[1]
    wf_id = sys.argv[2]
    itnum = sys.argv[3] 

    # Set the fixed params: qauntities and coretransp grid
    quantities = ['te', 'ti']
    attributes = ['value', 'ddrho']

    ft_rho_tor_norm = [0.143587306141853 , 0.309813886880875 , 0.442991137504578 , 0.560640752315521 , 0.668475985527039 , 0.769291400909424 , 0.864721715450287 , 0.955828309059143]

    # Read the last 'point' (in 'core profile' space) for the workflow run
    final_point = read_cpo_file(folder_in, prof_names=quantities, attrib_names=attributes, coords=ft_rho_tor_norm, filename=f"ets_coreprof_out.cpo")
    final_point.to_csv(f"final_point_{wf_id}_{itnum}.csv")
    #print(f"final_point=\n{final_point}")###DEBUG

    # Create a grid (in 'core profile' space) around the 'point' read
    grid_file = f"grid_it_{wf_id}_{itnum}.csv"
    param_grid = write_profs_fromfile_grid(final_point, filename_out=grid_file, num_steps=2)
    #print(f"param_grid[-1]=\n{param_grid.iloc[-1]}")###DEBUG

    # Evaluate pyGEM0 for every point of the new grid
    gem0_file = f"gem0py_new_{wf_id}_{itnum}.csv"
    write_gem0_fromfile(param_grid, gem0_file)

    ###
