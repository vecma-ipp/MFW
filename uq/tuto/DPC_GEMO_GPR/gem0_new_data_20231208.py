#! /usr/bin/env python

import pandas as pd
import numpy as np
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF
import matplotlib
#matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import pickle
import time

D = pd.read_csv('gem0_new_data_20231208.csv')
x_labels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
y_labels = ['te_transp_flux', 'ti_transp_flux']

fits = []
for ft in range(8):

    print(f'\nFitting flux tube {ft}')
    X = np.array(D[D.ft==ft][x_labels])
    Y = np.array(D[D.ft==ft][y_labels])
    X_mean = X.mean(axis=0)
    X_std = X.std(axis=0)
    X_norm = (X-X_mean)/X_std
    Y_mean = Y.mean(axis=0)
    Y_std = Y.std(axis=0)
    Y_norm = (Y-Y_mean)/Y_std
    
    fit_start = time.time()
    kernel = 1 * RBF(length_scale=[1.0, 1.0, 1.0, 1.0], length_scale_bounds=(1e-1, 1e3))
    GP = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=20)
    GP.fit(X_norm, Y_norm)
    print(f'Fitting took {time.time()-fit_start:0.2f} seconds')
    print(f'{GP.kernel_ = }')
    print(f'{GP.log_marginal_likelihood_value_ = }')
    fits.append(GP)
    
    l    = np.linspace(-2,2,41)
    cuts = [ np.dstack((l,   l*0, l*0, l*0))[0],
             np.dstack((l*0, l,   l*0, l*0))[0],
             np.dstack((l*0, l*0, l,   l*0))[0],
             np.dstack((l*0, l*0, l*0, l))[0] ]
    sels = [ (np.abs(X_norm[:,1]) < 1e-14) & (np.abs(X_norm[:,2]) < 1e-14) & (np.abs(X_norm[:,3]) < 1e-14),
             (np.abs(X_norm[:,0]) < 1e-14) & (np.abs(X_norm[:,2]) < 1e-14) & (np.abs(X_norm[:,3]) < 1e-14),
             (np.abs(X_norm[:,0]) < 1e-14) & (np.abs(X_norm[:,1]) < 1e-14) & (np.abs(X_norm[:,3]) < 1e-14),
             (np.abs(X_norm[:,0]) < 1e-14) & (np.abs(X_norm[:,1]) < 1e-14) & (np.abs(X_norm[:,2]) < 1e-14) ]
    
    plt.figure(figsize=(18,12), layout='constrained')
    for i in range(4):
    
        Y_prediction_mean, Y_prediction_std = GP.predict(cuts[i], return_std=True)

        plt.subplot(2,2,i+1)
    
        plt.plot(cuts[i][:,i]*X_std[i]+X_mean[i], Y_prediction_mean*Y_std+Y_mean)
        plt.gca().set_prop_cycle(None)
        plt.plot(cuts[i][:,i]*X_std[i]+X_mean[i], Y_prediction_mean*Y_std+Y_mean + Y_prediction_std*Y_std*1.96, ':')
        plt.gca().set_prop_cycle(None)
        plt.plot(cuts[i][:,i]*X_std[i]+X_mean[i], Y_prediction_mean*Y_std+Y_mean - Y_prediction_std*Y_std*1.96, ':')
    
        plt.gca().set_prop_cycle(None)
        plt.plot(X_norm[sels[i]][:,i]*X_std[i]+X_mean[i], Y_norm[sels[i]]*Y_std+Y_mean, 'o', label=y_labels)
        plt.legend(loc=0)
        plt.xlabel(x_labels[i])

    plt.suptitle(f'flux tube {ft}')
    
    plt.savefig(f'flux_tube_{ft}.png')
    plt.savefig(f'flux_tube_{ft}.pdf')
    
pickle.dump(fits, open('fits.pickle','bw'))

for i in range(8):
    pickle.dump(fits[i], open(f"fits_{i}.pickle", 'bw'))

