__author__ = 'Anna Nikishova & Jalal Lakhlili & Yehor Yudin'

import os
import timeit
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from ascii_cpo import read
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF, ConstantKernel

from sklearn.metrics import mean_squared_error as mse

# for model saving and preparation
import pickle
import pickletools
from joblib import dump, load

# Load data from CPOs, example:
# corep_file: gem_coreprof_in.cpo
# coret_file: gem_coretransp_out.cpo
def load_data(corep_file, coret_file, flux_tube_index):
    # inputs
    corep = read(corep_file, 'coreprof')
    te_value = corep.te.value[flux_tube_index]
    ti_value = corep.ti.value[flux_tube_index]
    te_ddrho = corep.te.ddrho[flux_tube_index]
    ti_ddrho = corep.ti.ddrho[flux_tube_index]

    # outputs (1 flux tube)
    coret = read(coret_file, 'coretransp')
    te_transp_flux = coret.values[0].te_transp.flux[0]
    ti_transp_flux = coret.values[0].ti_transp.flux[0]

    return te_value, ti_value, te_ddrho, ti_ddrho, te_transp_flux, ti_transp_flux
    #return te_value, te_ddrho, te_transp_flux

# The results are saved in uq/data/outputs folder
def plot_res(prediction, original, name, num, out_color, output_folder):
    plt.ioff()
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
    plt.yscale("symlog")
    plt.xlabel('Run number')
    plt.ylabel('Error')
    plt.subplot(313)
    plt.plot(np.fabs(prediction- original)/original * 100, '.', color=out_color)
    plt.grid()
    plt.xlabel('Run number')
    plt.ylabel('Relative error (%)')
    #plt.yscale("log")
    plt.tight_layout()
    plt.savefig(output_folder + '/GP_prediction_' + num + '_' + type_train + '_' + str(len(train_n)) + '.png',bbox_inches='tight', dpi=100)
    plt.clf()


# Main
if __name__ == "__main__":

    # TODO to modify
    WORKDIR = os.environ['WORKDIR']
    #campaign_id = "wvkryt88_sequential"
    #campaign_id = "jh2q6ts1"
    #campaign_id = "b9e9pzco"
    #campaign_id = "61_e9zvw66q"
    data_dir = WORKDIR + "/Fusion_Inputs/UQ_GEM_Data/runs/"
    #data_dir = WORKDIR + "/UQ_GEM0_" + campaign_id + "/runs/"

    # Campaign for mutliapp
    flux_tube_index = 69 # 61
    N_runs = 625 #16
    input_dim = 4

    input_samples = np.zeros((N_runs, input_dim))

    output_dim = 1 #2
    output_samples = np.zeros((N_runs, output_dim))

    for run in range(N_runs):

        corep_file = data_dir + "Run_"+str(run+1)+"/gem_coreprof_in.cpo" #gem0_coreprof_in.cpo
        coret_file = data_dir + "Run_"+str(run+1)+"/gem_coretransp_out.cpo" #gem0_coreprof_in.cpo

        #te_value, _, te_ddrho, _, te_transp_flux, _ = load_data(corep_file, coret_file, flux_tube_index)
        te_value, ti_value, te_ddrho, ti_ddrho, te_transp_flux, ti_transp_flux = load_data(corep_file, coret_file, flux_tube_index)

        #input_samples[run] = te_value, te_ddrho
        input_samples[run] = te_value, ti_value, te_ddrho, ti_ddrho
        
        #output_samples[run] = te_transp_flux
        output_samples[run] = te_transp_flux, ti_transp_flux

    #print(input_samples)
    #print(output_samples)	

    N_tr = 300 #7

    type_train = 'random' #'regular'#
    if type_train == 'random':
        train_n = np.random.randint(0, N_runs, N_tr)
    else:
        train_n = np.arange(0, N_runs, 3)
        train_n = np.append(train_n, N_runs-1)

    X = input_samples[train_n]
    Y = output_samples[train_n]

    #print(train_n)

    test_n = np.array([el for el in list(range(0, N_runs)) if el not in train_n])
    X_test = input_samples[test_n]
    Y_test = output_samples[test_n]

    #print(test_n)
	
    start = timeit.timeit()

    n_features = 4 #2
    #kernel = Matern(length_scale=[100,100], nu=0.5) + RBF(length_scale=[100,100])
    #kernel = Matern(length_scale=[100, 100, 100, 100], nu=0.5) + RBF(length_scale=[100, 100, 100, 100])
    kernel = ConstantKernel() + Matern(length_scale=[100, 100, 100, 100], nu=0.5) + WhiteKernel(noise_level=0.5)
    # to do noise level from GEM

    gpr = GaussianProcessRegressor(kernel=kernel, random_state=0).fit(X, Y)
    #print(gpr.kernel_)

    # --- Save and loaad model, then use to predice
    #st_gpr = pickle.dumps(gpr)
    #gpr_l1 = pickle.loads(st_gpr)

    mod_folder = os.path.abspath('data/models')
    mod_filename = 'gpr_gem_1.joblib'
    model_path = os.path.join(mod_folder, mod_filename)

    dump(gpr, model_path)
    gpr_l2 = load(model_path)

    prediction_y = gpr_l2.predict(X_test)

    end = timeit.timeit()
    print("GP took ", end - start)

    color_1 = 'blue'
    color_2 = 'orange'

    plot_res(prediction_y[:, 0], Y_test[:, 0], r'$T_e$', '1', color_1, WORKDIR+'/outputs/gem/') #+campaign_id)
    plot_res(prediction_y[:, 1], Y_test[:, 1], r'$T_i$', '2', color_2, WORKDIR+'/outputs/gem/') #/gem0')
    
    print ('MSE of the GPR predistion is: {0}'.format(mse(Y_test, prediction_y)))

