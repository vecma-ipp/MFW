__author__ = 'Anna Nikishova'

import os
import numpy as np
from scipy.interpolate import griddata
import matplotlib.pyplot as plt


def plot_res(prediction, original, name, num, out_color):
    plt.figure(figsize=(5, 11))
    axis1 = plt.subplot(311)
    plt.title('{} training sample of size: {}'.format(type_train, str(len(train_n))))
    plt.plot(prediction, '.', label='Linear interpolator', color=out_color)
    plt.plot(original, '.', label='Turbulence model', color='green')
    plt.xlabel('Run number')
    plt.ylabel('Prediction of {}'.format(name))
    plt.legend()
    plt.grid()
    # plt.yscale("log")
    plt.subplot(312)
    plt.plot(prediction- original, '.', color=out_color)
    plt.grid()
    # plt.yscale("log")
    plt.xlabel('Run number')
    plt.ylabel('Error')
    plt.subplot(313)
    plt.plot(np.fabs(prediction- original)/original * 100, '.', color=out_color)
    plt.grid()
    plt.xlabel('Run number')
    plt.ylabel('Relative error (%)')
    # plt.yscale("log")
    plt.tight_layout()
    plt.savefig('../data/GP_prediction_' + name + '_' + type_train + '_' + str(len(train_n)) + '(linear).png',bbox_inches='tight', dpi=100)
    plt.show()
    plt.clf()

#path_to_datafile = 'data/AUG_gem_inoutput.txt'
path_to_datafile = '../data/AUG_gem_inoutput.txt'

data = np.genfromtxt(path_to_datafile, skip_header=1)
input_samples_all = data[:, 1:58]
output_samples_all = data[:, 58:]
input_dim = 8
N_runs = len(input_samples_all[:, 0])

names_in_outputs = ['time', 'Te-ft1', 'Te-ft2', 'Te-ft3', 'Te-ft4', 'Te-ft5', 'Te-ft6', 'Te-ft7', 'Te-ft8', 'dTe-ft1', 'dTe-ft2', 'dTe-ft3', 'dTe-ft4', 'dTe-ft5', 'dTe-ft6', 'dTe-ft7', 'dTe-ft8', 'Ti-ft1', 'Ti-ft2', 'Ti-ft3', 'Ti-ft4', 'Ti-ft5', 'Ti-ft6', 'Ti-ft7', 'Ti-ft8', 'dTi-ft1', 'dTi-ft2', 'dTi-ft3', 'dTi-ft4', 'dTi-ft5', 'dTi-ft6', 'dTi-ft7', 'dTi-ft8', 'ne-ft1', 'ne-ft2', 'ne-ft3', 'ne-ft4', 'ne-ft5', 'ne-ft6', 'ne-ft7', 'ne-ft8', 'dne-ft1', 'dne-ft2', 'dne-ft3', 'dne-ft4', 'dne-ft5', 'dne-ft6', 'dne-ft7', 'dne-ft8', 'q-ft1', 'q-ft2', 'q-ft3', 'q-ft4', 'q-ft5', 'q-ft6', 'q-ft7', 'q-ft8', 'B0', 'flux-Te-ft1', 'flux-Te-ft2', 'flux-Te-ft3', 'flux-Te-ft4', 'flux-Te-ft5', 'flux-Te-ft6', 'flux-Te-ft7', 'flux-Te-ft8', 'flux-Ti-ft1', 'flux-Ti-ft2', 'flux-Ti-ft3', 'flux-Ti-ft4', 'flux-Ti-ft5', 'flux-Ti-ft6', 'flux-Ti-ft7', 'flux-Ti-ft8', 'flux-ne-ft1', 'flux-ne-ft2', 'flux-ne-ft3', 'flux-ne-ft4', 'flux-ne-ft5', 'flux-ne-ft6', 'flux-ne-ft7', 'flux-ne-ft8']

num_output = 0
input_samples8 = input_samples_all[:, num_output::8]
output_samples = output_samples_all[:, num_output]

# Here I excluded the input paprameters that did not vary in the data set
input_samples = np.zeros((N_runs, 5))
input_samples[:,:4] = input_samples8[:,:4]
input_samples[:,4] = input_samples8[:,6]

N_tr = 1000
type_train = 'regular'#'random'#
if type_train == 'random':
    train_n = np.random.randint(0, N_runs, N_tr)
else:
    train_n = np.arange(0, N_runs, 5)

X = input_samples[train_n]
Y = output_samples[train_n]

start = 0
end = 300

prediction = griddata(X, Y, input_samples, method='linear')

plot_res(prediction, output_samples, names_in_outputs[58+num_output], '1', 'blue')
