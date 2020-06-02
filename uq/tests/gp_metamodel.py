__author__ = 'Anna'

import os
# import easyvvuq as uq
# import chaospy as cp
import timeit
import matplotlib.pyplot as plt
import numpy as np
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF
from sklearn.gaussian_process.kernels \
    import RBF, WhiteKernel, ConstantKernel as C
from load_data import load_one_data_set
import sklearn


def plot_res(prediction, original, name, num, out_color):
    plt.figure(figsize=(5, 11))
    plt.subplot(311)
    plt.title('{} training sample of size: {}'.format(type_train, str(len(train_n))))
    plt.plot(prediction, '.', label='GP metamodel', color=out_color)
    plt.plot(original, '.', label='Turbulence model', color='green')
    plt.xlabel('Run number')
    plt.ylabel('Prediction of {}'.format(name))
    plt.legend()
    plt.grid()
    plt.yscale("log")
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
    plt.savefig('Figures/GP_prediction_' + num + '_' + type_train + '_' + str(len(train_n)) + '.png',bbox_inches='tight', dpi=100)
    plt.clf()

N_runs = 625
input_dim = 4
input_samples = np.zeros((N_runs, input_dim))

output_dim = 2
output_samples = np.zeros((N_runs, output_dim))

for run in range(N_runs):
    te_value, ti_value, te_ddrho, ti_ddrho, te_transp_flux, ti_transp_flux = load_one_data_set(run+1)

    input_samples[run] = te_value, ti_value, te_ddrho, ti_ddrho
    output_samples[run] = te_transp_flux, ti_transp_flux

print(input_samples)

N_tr = 300
type_train = 'random'#'regular'#
if type_train == 'random':
    train_n = np.random.randint(0, N_runs, N_tr)
else:
    train_n = np.arange(0, N_runs, 3)
    train_n = np.append(train_n, N_runs-1)

X = input_samples[train_n]
Y = output_samples[train_n]

start = timeit.timeit()
n_features = 4
kernel = Matern(length_scale=[100, 100, 100, 100], nu=0.5) + RBF(length_scale=[100, 100, 100, 100])

gpr = GaussianProcessRegressor(kernel=kernel, random_state=0).fit(X, Y)
print(gpr.kernel_)
prediction_y = gpr.predict(input_samples)

end = timeit.timeit()
print("GP took ", end - start)

color_1 = 'blue'
color_2 = 'orange'

plot_res(prediction_y[:, 0], output_samples[:, 0], r'$T_e$', '1', color_1)
plot_res(prediction_y[:, 1], output_samples[:, 1], r'$T_i$', '2', color_2)

