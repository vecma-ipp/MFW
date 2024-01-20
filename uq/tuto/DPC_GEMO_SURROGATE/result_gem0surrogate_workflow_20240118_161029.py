#! /usr/bin/env python
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pickle

def find(name, data=D): return [ s for s in np.array(data.columns) if re.findall(name, s) ]

D = pd.read_csv('result_gem0surrogate_workflow_20240118_161029.csv', sep=' ')

F = pd.read_csv('../YY_2023-12-16/gem0_new_data_20231208.csv', sep=',')

Ti  = find('^Ti-ft')
Te  = find('^Te-ft')
dTi = find('^dTi-ft')
dTe = find('^dTe-ft')

Fields = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
Data   = [Te, Ti, dTe, dTi]
xrange = [0, len(D['Ti-ft1'])-1]

for ft in range(8):
    plt.figure(figsize=(18,12), layout='constrained')
    for i in range(4):
        plt.subplot(2,2,i+1)
#        plt.plot(xrange, [F[F.ft==ft].max()[Fields[i]], F[F.ft==ft].max()[Fields[i]]], label=f"max")
        plt.plot(D[Data[i][ft]], label=f"simulation")
#        plt.plot(xrange, [F[F.ft==ft].min()[Fields[i]], F[F.ft==ft].min()[Fields[i]]], label=f"min")
        plt.plot(xrange, np.array([np.array([F[F.ft==ft][Fields[i]].unique()]),np.array([F[F.ft==ft][Fields[i]].unique()])])[:,0,:], 'k')
        plt.ylabel(Fields[i])
        plt.legend(loc=0)
    plt.suptitle(f"flux tube {ft} (counting from 0)")
    plt.savefig(f'time_traces_flux_tube_{ft}.png')
    plt.savefig(f'time_traces_flux_tube_{ft}.pdf')


fits = pickle.load(open('../YY_2023-12-16/fits.pickle', 'br'))
x_labels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
y_labels = ['te_transp_flux', 'ti_transp_flux']
for ft in range(8):
    X = np.array(F[F.ft==ft][x_labels])
    Y = np.array(F[F.ft==ft][y_labels])
    X_mean = X.mean(axis=0)
    X_std = X.std(axis=0)
    X_norm = (X-X_mean)/X_std
    Y_mean = Y.mean(axis=0)
    Y_std = Y.std(axis=0)
    Y_norm = (Y-Y_mean)/Y_std

    ref = np.array([np.array(D[Data[i][ft]])[-1] for i in range(4)])
    ref_norm = (ref-X_mean)/X_std

    l    = np.linspace(-0.5,0.5,21)
    cuts = [ np.dstack((ref_norm[0]+l,   ref_norm[1]+l*0, ref_norm[2]+l*0, ref_norm[3]+l*0))[0],
             np.dstack((ref_norm[0]+l*0, ref_norm[1]+l,   ref_norm[2]+l*0, ref_norm[3]+l*0))[0],
             np.dstack((ref_norm[0]+l*0, ref_norm[1]+l*0, ref_norm[2]+l,   ref_norm[3]+l*0))[0],
             np.dstack((ref_norm[0]+l*0, ref_norm[1]+l*0, ref_norm[2]+l*0, ref_norm[3]+l  ))[0] ]

    plt.figure(figsize=(18,12), layout='constrained')
    for i in range(4):
    
        Y_prediction_mean, Y_prediction_std = fits[ft].predict(cuts[i], return_std=True)

        plt.subplot(2,2,i+1)
    
        plt.plot(cuts[i][:,i]*X_std[i]+X_mean[i], Y_prediction_mean*Y_std+Y_mean, label=y_labels)
        plt.gca().set_prop_cycle(None)
        plt.plot(cuts[i][:,i]*X_std[i]+X_mean[i], Y_prediction_mean*Y_std+Y_mean + Y_prediction_std*Y_std*1.96, ':')
        plt.gca().set_prop_cycle(None)
        plt.plot(cuts[i][:,i]*X_std[i]+X_mean[i], Y_prediction_mean*Y_std+Y_mean - Y_prediction_std*Y_std*1.96, ':')
        P = F[F.ft==ft][Fields[i]].unique()
        plt.plot(P, P*0, 'ko')
        plt.plot(ref[i], 0.0, 'k+')
        
        plt.legend(loc=0)
        plt.xlabel(x_labels[i])

    plt.suptitle(f'flux tube {ft}\n{x_labels} : {ref}')
    
    plt.savefig(f'gpr_predictions_flux_tube_{ft}.png')
    plt.savefig(f'gpr_predictions_flux_tube_{ft}.pdf')

