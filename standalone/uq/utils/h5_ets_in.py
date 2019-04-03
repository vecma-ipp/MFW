import h5py
import numpy as np
import time

# Write
t = time.time()
with h5py.File('/ptmp/ljala/ets_in.h5', 'w') as h5:
    # Uncertain paramters: name and cpo source
    g1 = h5.create_group('params')
    g1.attrs['te_transp.diff_eff'] = 'coretransp'

    # Quantity of interset: name and cpo source
    g2 = h5.create_group('qoi')
    g2.attrs['te'] = 'coreprof'
    g2.attrs['ne'] = 'coreprof'

    # Output file containg value of QoI => other hdf5 file
    g3 = h5.create_group('outputs')
    g3.attrs['out_file'] = 'output.csv'

    # Inputs parameters after sampling
    g4 = h5.create_group('inputs')
    for i in range(10**6):
        rname = 'Run_'+str(i)
        gr = g4.create_group(rname)
        gr.create_dataset('te_transp.diff_eff',
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
