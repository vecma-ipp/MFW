
import sys

import numpy as np
import pandas as pd

from da_utils import read_cpo_files, write_gem0_fromfile, write_profs_fromfile_grid


def main():

    # Get the run and iteration identifiers
    folder_in = sys.argv[1]
    wf_id = sys.argv[2]
    itnum = sys.argv[3] 

    n_p_p_p = int(sys.argv[4]) if len(sys.argv)>4 else 3

    coreprof_filename = f"ets_coreprof_out.cpo"

    equilibrium_filename = sys.argv[5]       if len(sys.argv)>5 else f"ets_equilibrium_out.cpo"
    include_equilibrium =  bool(sys.argv[6]) if len(sys.argv)>6 else False

    # Set the fixed params: qauntities and coretransp grid
    quantities = ['te', 'ti']
    attributes = ['value', 'ddrho']

    ft_rho_tor_norm = [0.143587306141853 , 0.309813886880875 , 0.442991137504578 , 0.560640752315521 , 0.668475985527039 , 0.769291400909424 , 0.864721715450287 , 0.955828309059143]

    # Read the last 'point' (in 'core profile' space) for the workflow run
    final_point = read_cpo_files(folder_in, prof_names=quantities, attrib_names=attributes, coords=ft_rho_tor_norm, filename=coreprof_filename, filetype='coreprof')

    if include_equilibrium:
        equil_quantities = ['profiles_1d']
        equil_attributes = ['q', 'gm3']
        final_point_2term = read_cpo_files(folder_in, prof_names=equil_quantities, attrib_names=equil_attributes, coords=ft_rho_tor_norm, filename=equilibrium_filename, filetype='equilibrium')
        #print(final_point); print(final_point_2term) ###DEBUG
        final_point = final_point.set_index('ft').join(final_point_2term.set_index('ft'), on='ft') # 'ft' should work as suffix, if there is one 'point' per f.-t.
        final_point.reset_index(inplace=True)
        #print(final_point) ###DEBUG

    final_point.to_csv(f"final_point_{wf_id}_{itnum}.csv")
    #print(f"final_point=\n{final_point}")###DEBUG

    # Create a grid (in 'core profile' space) around the 'point' read
    grid_file = f"grid_it_{wf_id}_{itnum}.csv"

    # Define go to create new input samples: number of new samples per dimemsion and step size
    num_steps = (n_p_p_p - 1) // 2
    exp_factor = 0.33
    # if len(sys.argv)>7:
    #     num_steps = int(sys.argv[6])
    #     exp_factor = int(sys.argv[7])
    # else:
    #     if include_equilibrium:
    #         num_steps = 1
    #         exp_factor = 0.33
    #     else:
    #         # num_steps = 2
    #         # exp_factor = 0.25
    #         num_steps = 1
    #         exp_factor = 0.33

    param_grid = write_profs_fromfile_grid(final_point, filename_out=grid_file, num_steps=num_steps, exp_factor=exp_factor)
    #print(f"param_grid[-1]=\n{param_grid.iloc[-1]}")###DEBUG

    # Evaluate pyGEM0 for every point of the new grid
    gem0_file = f"gem0py_new_{wf_id}_{itnum}.csv"

    # # - option A.1 if only the default equilibrium to be used
    # write_gem0_fromfile(param_grid, gem0_file,)
    # - option A.2 If the background equilibrium has to be changed
    write_gem0_fromfile(param_grid, gem0_file, equilibrium_file=equilibrium_filename)

    ###


if __name__ == '__main__':

    main()
