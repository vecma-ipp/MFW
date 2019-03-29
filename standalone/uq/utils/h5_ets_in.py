import h5py
import numpy as np
import time
# Write
runs = ['run_'+str(i) for i in range(6**8)]

t = time.time()

with h5py.File('ets_in.h5', 'w') as h5:
    # Uncertain paramters: name and cpo source
    g = h5.create_group('params')
    g.attrs['te_transp.diff_eff'] = 'coretransp'

    # Quantity of interset: name and cpo source
    g = h5.create_group('qoi')
    g.attrs['te'] = 'coreprof'
    g.attrs['ne'] = 'coreprof'

    # Output file containg value of QoI => other hdf5 file
    g = h5.create_group('outputs')
    g.attrs['out_file'] = 'output.csv'

    # Inputs parameters after sampling
    h5.create_group('inputs')
    for rname in runs:
        g = h5.create_group('inputs/'+rname)
        g.create_dataset('te_transp.diff_eff',
                data=[0.22603400938641854, 0.22603400938641842, 0.22603400864185433,
                      0.22603400938641820, 0.22603400938641820,
                      0.22603400938641820, 0.22603400938641820, 0.22603400938641820])

t = time.time() - t
print("time = ", t)

## Read
#with h5py.File('ets_in.h5', 'r') as h5:
#
#    for rname in runs:
#        g = h5.get('inputs/'+rname)
#        param_values = np.array(g.get('diff_eff'))
#        print(rname, param_values)
