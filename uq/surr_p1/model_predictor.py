import os
#import timeit
#import matplotlib.pyplot as plt
import numpy as np
from ascii_cpo import read
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF
from sklearn.gaussian_process.kernels \
    import RBF, WhiteKernel, ConstantKernel as C

# for model loading and output writing
#import pickle
#import pickletools
from joblib import dump, load
import easymfw.templates.CPOElement as CPOElement


# Load data from CPOs, example:
# corep_file: gem_coreprof_in.cpo
# coret_file: gem_coretransp_out.cpo
def load_data(corep_file, flux_tube_index):
    # inputs
    corep = read(corep_file, 'coreprof')
    te_value = corep.te.value[flux_tube_index]
    ti_value = corep.ti.value[flux_tube_index]
    te_ddrho = corep.te.ddrho[flux_tube_index]
    ti_ddrho = corep.ti.ddrho[flux_tube_index]

    # outputs (1 flux tube)
    # coret = read(coret_file, 'coretransp')
    # te_transp_flux = coret.values[0].te_transp.flux[0]
    # ti_transp_flux = coret.values[0].ti_transp.flux[0]

    return te_value, ti_value, te_ddrho, ti_ddrho


def save_data(coret_file,value):
    cpo = CPOElement(coret_file, "coretransp")
    cpo.set_value("ti_transp.flux", value)


# Main
if __name__ == "__main__":

    WORKDIR = os.environ['WORKDIR']
    data_dir = WORKDIR + "/Fusion_Inputs/UQ_GEM_Data/runs/"

    flux_tube_index = 69  #TODO has to be read from seomwhere
    #N_runs = 625
    dim_pred_sample = 1
    input_dim = 4
    input_samples = np.zeros((dim_pred_sample, input_dim))

    output_dim = 2
    output_samples = np.zeros((dim_pred_sample, output_dim))

    # case when we read one set of CPO files from the local directory
    # and get one output CPO file with a prediceted value
    for run in range(dim_pred_sample):
        corep_file = "/gem_coreprof_in.cpo"
        #coret_file = "/gem_coretransp_out.cpo"

        te_value, ti_value, te_ddrho, ti_ddrho = load_data(corep_file, flux_tube_index)

        input_samples[run] = te_value, ti_value, te_ddrho, ti_ddrho
        #output_samples[run] = te_transp_flux, ti_transp_flux

    X = input_samples[...]

    #start = timeit.timeit()
    #n_features = 4
    #kernel = Matern(length_scale=[100, 100, 100, 100], nu=0.5) + RBF(length_scale=[100, 100, 100, 100])
    #gpr = GaussianProcessRegressor(kernel=kernel, random_state=0).fit(X, Y)

    mod_folder = '/data'
    mod_filename = 'gpr_gem_1.joblib'
    model_path = os.path.join(mod_folder, mod_filename)

    gpr = load(model_path)

    prediction_y = gpr.predict(input_samples)

    for run in range(dim_pred_sample):
        coret_file = "/gem_coretransp_out.cpo"
        save_data(coret_file, prediction_y[run])

    print("> Prediction finished")