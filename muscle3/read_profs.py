import sys, os, csv
import numpy as np
from ascii_cpo import read

sys.path.append('../uq/basicda')
from gem_da import profile_evol_load, profile_evol_plot

def read_attrib(filename, quantity, attribute, coord, filetype='coreprof'):
    cpo_obj = read(filename, filetype)
    q = getattr(cpo_obj, quantity)
    a = getattr(q, attribute)
    return a[coord]

def read_files(foldername, quantity, attribute, coord, filetype='coreprof'):
    
    file_base_tocheck = filetype+'_'
    file_ext = '.cpo'
    
    file_names = [f for f in os.listdir(foldername) if 
                        os.path.isfile(os.path.join(foldername, f)) and 
                        f.endswith(file_ext) and
                        f.startswith(file_base_tocheck)
            ]
    file_names.sort()

    attributes = []

    for f in file_names:
        a = read_attrib(foldername+f, quantity, attribute, coord, filetype)
        attributes.append(a)

    np.savetxt('res_'+quantity+'_'+attribute+'.csv', np.array(attributes), delimiter=',')
    
    return attributes

fold_name = 'workflow/run_fusion_gem_surr_20230606_145056/instances/transport/workdir/'

n_run = 10
runnum_list = [r for r in range (n_run)]

name = 'coreprof'

quantities =  ['ti']
attributes = ['value']

coord_num = [68]

read_files(fold_name, quantities[0], attributes[0], coord=coord_num[0], filetype='coreprof')

# val_ev_s, file_names = profile_evol_load(prof_names=quantities, 
#                                         attrib_names=attributes, 
#                                         coord_len=len(coord_num), 
#                                         folder_name=fold_name, 
#                                         file_code_name=name, 
#                                         name_postfix='',
#                                         file_base_intermediate='',
#                                         )

# profile_evol_plot(val_ev_s, 
#                   labels=['ti_value'], 
#                   name=name+'_'+quantities[0]+'_'+attributes[0], 
#                   alignment='start', 
#                   vertline=False) 
