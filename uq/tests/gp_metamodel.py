__author__ = 'Anna Nikishova & Jalal Lakhlili & Yehor Yudin'

import os
import sys
import timeit
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF, ConstantKernel
from sklearn.metrics import mean_squared_error as mse
from sklearn import preprocessing

import GPy
from scipy import stats

import csv
import pandas as pd

#importing ual
sys.path.append('C:\\Users\\user\\Documents\\UNI\\MPIPP\\PHD\\code\\MFW\\uq\\tuto\\DPC\\ASCII_UAL')
from ascii_cpo import read

sys.path.append("..")
from data_test import plot_camp_vals, get_camp_dataframe
from basicda.da_utils import read_sim_csv

# for model saving and preparation
import pickle
import pickletools
from joblib import dump, load

# Load data from CPOs, example:
# corep_file: gem_coreprof_in.cpo
# coret_file: gem_coretransp_out.cpo

N_runs = 625
csv_file = 'gem_data_625.txt'
Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
Ylabels = ['te_transp_flux', 'ti_transp_flux']

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
    #TODO save in csv!!!
    #return te_value, te_ddrho, te_transp_flux

def create_dataset(data_dir):
    # Campaign for mutliapp
    flux_tube_index = 69  # 61
    N_runs = 625  # 16
    input_dim = 4

    input_samples = np.zeros((N_runs, input_dim))

    output_dim = 2  # 2
    output_samples = np.zeros((N_runs, output_dim))

    for run in range(N_runs):
        corep_file = data_dir + "Run_" + str(run + 1) + "/gem_coreprof_in.cpo"  # gem0_coreprof_in.cpo
        coret_file = data_dir + "Run_" + str(run + 1) + "/gem_coretransp_out.cpo"  # gem0_coreprof_in.cpo

        # te_value, _, te_ddrho, _, te_transp_flux, _ = load_data(corep_file, coret_file, flux_tube_index)
        te_value, ti_value, te_ddrho, ti_ddrho, te_transp_flux, ti_transp_flux = load_data(corep_file, coret_file,
                                                                                           flux_tube_index)

        # input_samples[run] = te_value, te_ddrho
        input_samples[run] = te_value, ti_value, te_ddrho, ti_ddrho

        # output_samples[run] = te_transp_flux
        output_samples[run] = te_transp_flux, ti_transp_flux

        # print(input_samples)
        # print(output_samples)

        return input_samples, output_samples

def load_csv_file(data_dir, input_file='gem_data_625.txt'):
    N_runs = 625
    frac_train = 0.5
    input_dim = 4
    output_dim = 2

    input_samples = np.zeros((N_runs, input_dim))
    output_samples = np.zeros((N_runs, output_dim))

    input_file = data_dir + '\\' + input_file

    with open(input_file, 'r') as inputfile:
        datareader = csv.reader(inputfile, delimiter=',')
        i = 0
        for row in datareader:
            input_samples[i] = row[0:input_dim]
            output_samples[i] = row[input_dim:input_dim+output_dim]
            i = i + 1

    return input_samples, output_samples

# The results are saved in uq/data/outputs folder
def plot_res(prediction, original, name, num, len, out_color, output_folder):
    plt.ioff()
    plt.figure(figsize=(5, 11))
    plt.subplot(311)
    plt.title('{} training sample of size: {}'.format(type_train, str(len)))
    plt.plot(prediction, '.', label='GP metamodel', color=out_color)
    plt.plot(original, '.', label='Turbulence model', color='green')
    plt.xlabel('Run number')
    plt.ylabel('Prediction of {}'.format(name))
    plt.legend()
    plt.grid()
    #plt.yscale("log")
    plt.subplot(312)
    err_abs = abs(prediction - original)
    #print(err_abs.where(abs(err_abs)>2e5)) # no where ar nparray
    plt.plot(err_abs, '.', color=out_color)
    plt.grid(b=True, which='both')
    #plt.yscale("symlog")
    plt.xlabel('Run number')
    plt.ylabel('Error')
    plt.subplot(313)
    plt.plot(np.fabs(prediction - original)/original * 100, '.', color=out_color)
    plt.grid()
    plt.xlabel('Run number')
    plt.ylabel('Relative error (%)')
    #plt.yscale("log")
    plt.tight_layout()
    plt.savefig(output_folder + 'GP_prediction_' + num + '_' + type_train + '_' + str(len) + '.png',
                bbox_inches='tight', dpi=100)
    plt.clf()

def choose_high_error_indices(y, threshold):
    #ind_flat = np.logical_or([ind[:, i] for i in range(ind.shape[1])])
    #ind_flat = np.logical_or(ind[:, 0], ind[:, 1])
    ind_flat = (y > threshold).any(axis=1)
    ind_flat = np.where(ind_flat)[0]
    return ind_flat

def choose_high_value_indices(y, threshold):
    #y_ht = [np.where(abs(y[:,it]) > thr[it]) for it in range(len(thr))]
    y_ht = np.array([abs(y[:, it]) > threshold[it] for it in range(len(threshold))]).transpose()
    y_ht = y_ht.all(axis=1)
    y_ht = np.where(y_ht)
    return y_ht

def analyse_hp_distribution(gpr_gpy):
    v_min = s_hp.min()
    v_max = s_hp.max()
    labels = ['kern_variance', 'kern_lengthscale', 'noise_variance']
    vs = np.linspace(v_min, v_max, 100)
    for i in range(s_hp.shape[1]-1):
        kernel = stats.gaussian_kde(s_hp[:, i])  # numpy.linalg.LinAlgError: singular matrix
        plt.plot(vs, kernel(vs), label=labels[i])
    plt.legend()
    plt.savefig('tr_ds_fl_kernel_LS.png')
    plt.close()

def high_gradient_dataset(input_samples, output_samples):
    ### --- train model on high abs gradients only
    ind_hg = choose_high_value_indices(input_samples, [0., 0., 3200., 3200.])
    print('number of runs with high gradient values: {} \n'.format(ind_hg))
    #print('all high gradient samples should be here '.format(input_samples[ind_hg, :]))
    X_high_g = input_samples[ind_hg]
    Y_high_g = output_samples[ind_hg]
    X_high_g_train = input_samples[np.intersect1d(ind_hg, train_n)]
    Y_high_g_train = output_samples[np.intersect1d(ind_hg, train_n)]
    X_high_g_test = input_samples[np.intersect1d(ind_hg, test_n)]
    Y_high_g_test = output_samples[np.intersect1d(ind_hg, test_n)]
    kernel3 = Matern(length_scale=[25, 25, 15, 15], nu=0.5) + RBF(length_scale=[25, 25, 15, 15])
    gpr_hg = GaussianProcessRegressor(kernel=kernel3, random_state=0).fit(X_high_g_train, Y_high_g_train)
    print(gpr_hg.kernel.theta)
    prediction_y_high_gradient = gpr_hg.predict(X_high_g)
    return prediction_y_high_gradient

def high_error_model_retrain(input_samples, output_samples, gpr):
    ### --- Make a separate model on high errors
    prediction_y_full_set = gpr.predict(input_samples)
    err_abs_full_set = np.subtract(prediction_y_full_set, output_samples)
    err_rel_full_set = np.divide(err_abs_full_set, output_samples)
    ind_high = choose_high_error_indices(err_rel_full_set, 0.5)  # [0]
    print('runs from training dataset for which model performs poorly: {} \n'.format(np.intersect1d(ind_high, train_n)))
    print('numbers of runs for which model produces high error: {} \n'.format(ind_high))
    Y_high = output_samples[ind_high]
    X_high = input_samples[ind_high]
    #print(X_high)
    #kernel2 = Matern(length_scale=[25, 25, 25, 25], nu=0.5) + RBF(length_scale=[25, 25, 25, 25])
    kernel2 = Matern(length_scale=[1., 1., 1., 1.], nu=0.5)
    gpr_high = GaussianProcessRegressor(kernel=kernel2, random_state=0).fit(X_high, Y_high)
    #ls_h = gpr_high.kernel.theta
    #print(ls_h)
    prediction_y_high = gpr_high.predict(X_high)
    return gpr_high

def gpy_ard_training(gpr_gpy):
    gpr_gpy[".*Gaussian_noise"] = gpr_gpy.Y.var()*0.01
    gpr_gpy[".*Gaussian_noise"].fix()
    gpr_gpy.optimize(max_iters=500)
    gpr_gpy[".*Gaussian_noise"].unfix()
    gpr_gpy[".*Gaussian_noise"].constrain_positive()
    gpr_gpy.optimize_restarts(20, optimizer="bfgs", max_iters=1000, verbose=False)
    return gpr_gpy

# Main
if __name__ == "__main__":

    # TODO to modify
    #WORKDIR = os.environ['WORKDIR']
    WORKDIR = "c:\\Users\\user\\Documents\\UNI\\MPIPP\\PHD\\code"
    #campaign_id = "wvkryt88_sequential"
    #campaign_id = "jh2q6ts1"
    #campaign_id = "b9e9pzco"
    #campaign_id = "61_e9zvw66q"
    data_dir = WORKDIR + "/Fusion_Inputs/UQ_GEM_Data/runs/"
    #data_dir = WORKDIR + "\\Fusion_Inputs\\UQ_GEM_Data\\runs\\"
    #data_dir = WORKDIR + "/UQ_GEM0_" + campaign_id + "/runs/"

    input_samples, output_samples = load_csv_file(WORKDIR)
    #data = np.concatenate((np.array(input_samples), np.array(output_samples)), axis=1)
    #np.savetxt("gem_data_625.txt", data, delimiter=",")

    ### --- Using data_test to store data as pandas dataframe
    # data_sim = get_camp_dataframe(WORKDIR + "\\Fusion_Inputs\\UQ_GEM_Data\\",
    #                                          codename="gem", input_index=[69])
    # data_sim, data_sim_x, data_sim_y = read_sim_csv("gem_campaign_data.csv")
    # data_sim = pd.DataFrame({Xlabels[0]: input_samples[:, 0], Xlabels[1]: input_samples[:, 1],
    #                          Xlabels[2]: input_samples[:, 2], Xlabels[3]: input_samples[:, 3],
    #                          Ylabels[0]: output_samples[:, 0], Ylabels[1]: output_samples[:, 1]})
    # plot_camp_vals(data_sim, "gem")

    ### --- Preparing data: preprocesing and splitting
    #N_runs = 625
    N_tr = 200  #7

    type_train = 'random'  #'regular'#
    if type_train == 'random':
        train_n = np.random.randint(0, N_runs, N_tr)
    else:
        train_n = np.arange(0, N_runs, 3)
        train_n = np.append(train_n, N_runs-1)

    output_samples = np.log(output_samples)

    scalerX = preprocessing.StandardScaler().fit(input_samples)
    input_samples = scalerX.transform(input_samples)

    scalerY = preprocessing.StandardScaler().fit(output_samples)
    output_samples = scalerY.transform(output_samples)

    data_sim = pd.DataFrame({Xlabels[0]: input_samples[:, 0], Xlabels[1]: input_samples[:, 1],
                             Xlabels[2]: input_samples[:, 2], Xlabels[3]: input_samples[:, 3],
                             Ylabels[0]: output_samples[:, 0], Ylabels[1]: output_samples[:, 1]})
    plot_camp_vals(data_sim, "gem")

    X = input_samples[train_n]
    Y = output_samples[train_n]

    test_n = np.array([el for el in list(range(0, N_runs)) if el not in train_n])
    X_test = input_samples[test_n]
    Y_test = output_samples[test_n]

    ### --- GPR from scikitlearn
    # start = timeit.timeit()
    #
    # n_features = 4  #2
    # #kernel = Matern(length_scale=[100,100], nu=0.5) + RBF(length_scale=[100,100])
    # kernel = Matern(length_scale=[1., 1., 1., 1.], nu=0.5) + RBF(length_scale=[1., 1., 1., 1.])
    # #kernel = ConstantKernel() + Matern(length_scale=[100, 100, 100, 100], nu=0.5) + WhiteKernel(noise_level=0.5)
    # # to do noise level from GEM
    #
    # gpr = GaussianProcessRegressor(kernel=kernel, random_state=0, n_restarts_optimizer=15).fit(X, Y)
    # #print(gpr.kernel_)
    # ls = gpr_high.kernel.theta
    # print(ls)
    #
    # prediction_y, prediction_std = gpr.predict(X_test, return_std=True)
    #
    # end = timeit.timeit()
    # print("GP fitting took ", end - start)
    #
    # err_abs = np.subtract(prediction_y, Y_test)
    # err_rel = np.divide(abs(err_abs), Y_test)

    # --- Save and load model, then use to predict
    #st_gpr = pickle.dumps(gpr)
    #gpr_l1 = pickle.loads(st_gpr)

    ###mod_folder = os.path.abspath('data/models')
    ##mod_filename = 'gpr_gem_1.joblib'
    ##model_path = os.path.join(mod_folder, mod_filename)

    ###dump(gpr, model_path)
    ###gpr_l2 = load(model_path)

    ###prediction_y = gpr_l2.predict(X_test)

    #  --- the leftover data
    #ind_low = np.array([el for el in list(range(0, N_runs)) if el not in ind_high])
    #Y_low = output_samples[ind_low]

    ### --- GPY case

    #ker = GPy.kern.RBF(input_dim=1, active_dims=0) * \
    #      GPy.kern.RBF(input_dim=1, active_dims=1) * \
    #      GPy.kern.RBF(input_dim=1, active_dims=2) * \
    #      GPy.kern.RBF(input_dim=1, active_dims=3)  # kernel as product of kernels taking single dimensions, different lenghtscale

    ker = GPy.kern.Matern32(input_dim=4)

    # if 4d is too hard to analyse, use 2d -> to generate use GEM0 data sampled from QMC, or from python version
    gpr_gpy = GPy.models.GPRegression(X, Y, ker, initialize=True)  # for a vector output try coregionalization B x K
    gpr_gpy = gpy_ard_training(gpr_gpy)

    #gpr_gpy.kern.lengthscale.set_prior(GPy.priors.Gamma.from_EV(1., 10.))
    #gpr_gpy.kern.variance.set_prior(GPy.priors.Gamma.from_EV(1., 10.))
    #gpr_gpy.likelihood.variance.set_prior(GPy.priors.Gamma.from_EV(1., 10.))
    print(gpr_gpy)
    hmc = GPy.inference.mcmc.HMC(gpr_gpy, stepsize=5e-2)
    s_hp = hmc.sample(num_samples=300)  # Burn-in for MCMC
    s_hp = hmc.sample(num_samples=300)
    plt.plot(s_hp, '.')  #, labels=labels)
    plt.savefig('gp_hypar_vals.png')
    plt.close()
    print(gpr_gpy)
    #prediction_y, std = gpr_gpy.predict(X_test)

    ### model on high errors test samples
    prediction_y_full_set, std_first_full = gpr_gpy.predict(input_samples)
    err_abs_full_set = np.subtract(prediction_y_full_set, output_samples)
    err_rel_full_set = np.divide(err_abs_full_set, output_samples)
    ind_high = choose_high_error_indices(err_rel_full_set, 0.5)  # [0]
    Y_high = output_samples[ind_high]
    X_high = input_samples[ind_high]
    print(ind_high)
    train_n_h = np.random.choice(ind_high, int(len(ind_high)*0.7), replace=False)
    test_n_h = np.array([el for el in list(range(0, N_runs)) if el not in train_n_h])
    X_high_train = input_samples[train_n_h]
    Y_high_train = output_samples[train_n_h]
    X_high_test = input_samples[test_n_h]
    Y_high_test = output_samples[test_n_h]

    ker2 = GPy.kern.Matern32(input_dim=4)
    #ker2 = GPy.kern.Matern32(input_dim=4) + GPy.kern.Bias()

    gpr_gpy_h = GPy.models.GPRegression(X_high_train, Y_high_train, ker2, initialize=True)
    gpr_gpy_h = gpy_ard_training(gpr_gpy_h)
    print(gpr_gpy_h)
    #gpr_gpu_he = GPy.inference.mcmc.HMC(gpr_gpy, stepsize=5e-2)
    prediction_y_high_grad_set, std = gpr_gpy_h.predict(X_high)

    ### --- PLOTTING
    # print results
    color_1 = 'blue'
    color_2 = 'orange'

    #plot_res(prediction_y[:, 0], Y_test[:, 0], r'$TFl_e$', '1gpy', len(X), color_1, WORKDIR+'\\')  #+campaign_id)
    #plot_res(prediction_y[:, 1], Y_test[:, 1], r'$TFL_i$', '2gpy', len(X), color_1, WORKDIR+'\\')  #/gem0')

    plot_res(prediction_y_high_grad_set[:, 0], Y_high[:, 0], r'$TFL_e$', '1', len(X), color_1, WORKDIR+'\\highgrad_')
    plot_res(prediction_y_high_grad_set[:, 1], Y_high[:, 1], r'$TFL_e$', '2', len(X), color_1, WORKDIR + '\\highgrad_')

    # plt.plot(prediction_std, '.')
    # plt.savefig('std_for_runs.png')
    # plt.close()
    
    print('MSE of the GPR prediction is: {0}'.format(mse(Y_high, prediction_y_high_grad_set))) # other metrics?

