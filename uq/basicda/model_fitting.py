
import numpy as np
import sklearn
from scipy.optimize import curve_fit

from da_utils import exponential_model_sp

#from ascii_cpo import read

def read():
    return

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

def read_orig_data(data_dir="/Fusion_Inputs/UQ_GEM_Data/runs/", n_runs=625):
    flux_tube_index = 69

    input_dim = 4
    input_samples = np.zeros((n_runs, input_dim))
    output_dim = 2
    output_samples = np.zeros((n_runs, output_dim))

    for run in range(n_runs):
        corep_file = data_dir + "Run_" + str(run + 1) + "/gem_coreprof_in.cpo"
        coret_file = data_dir + "Run_" + str(run + 1) + "/gem_coretransp_out.cpo"

        # te_value, _, te_ddrho, _, te_transp_flux, _ = load_data(corep_file, coret_file, flux_tube_index)
        te_value, ti_value, te_ddrho, ti_ddrho, te_transp_flux, ti_transp_flux = load_data(corep_file, coret_file,
                                                                                           flux_tube_index)

        # input_samples[run] = te_value, te_ddrho
        input_samples[run] = te_value, ti_value, te_ddrho, ti_ddrho

        # output_samples[run] = te_transp_flux
        output_samples[run] = te_transp_flux, ti_transp_flux

def call_gem0():
    return 0

def func_eval(fpars):
    return call_gem0()

def test_func(x, a, m, s):
    """
    test function is a gaussian curve
    :param x: varaiable
    :param fpars: [mult. coef., mean, scale]
    :return: the value of
    """
    y = a * np.exp(-(x-m)**2 / (2*s*s))
    return y

def fit_norm(data_y=[], n_fp=[]):
    x = np.linspace(0, 1, 101)
    # y =  read_orig_data() # take from existing GEM data
    y = test_func(x, 2.33, 0.21, 1.51) + np.random.normal(0, 0.2, x.size)

    # init_vals = [0.5, 0.6, 0.7] # read current values from gem0 implementation
    init_vals = [1, 0, 1]  # for [amp, cen, wid]

    best_vals, covar = curve_fit(test_func, x, y, p0=init_vals)
    print('best_vals: {}'.format(best_vals))

def fit_exp(x, y, z, f=exponential_model_sp):
    X = np.dstack((x, y))[0]
    init_vals = [1500.0, 10.0, 10.0, 0.0, 0.0, np.e]
    best_vals, covar = curve_fit(f, X, z, p0=init_vals)
    print('best_vals: {}'.format(best_vals))
    return best_vals

#fit()