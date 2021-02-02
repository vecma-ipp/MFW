import pandas as pd
import numpy as np
import math
import matplotlib.pylab as plt
import time

import pandas as pd
import csv
import itertools

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF, WhiteKernel, ConstantKernel
from sklearn.neural_network import MLPRegressor

from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler

from da_utils import *

import sys
import os

from joblib import dump, load

from extcodehelper import ExtCodeHelper


def grad_utility(y_observ, sigma):
    
    utility = []

    return utility

def get_domain_grid(self, x_domain_params, mode='LHS'):
    """
    Creates a domain grid for input paramters for surrogate
    """
    ndim = x_domain_params.shape[1]
    nfeat = x_domain_params.shape[0]
    x_domain = np.zeros((nfeat, ndim))
    if mode == 'LHS':
        x_domain = x_domain # TODO get the LHC set of points
    return x_domain

def get_ongrid(x_mesh, x, y):
    x1_size = x_mesh[0].shape[0]
    x2_size = x_mesh[0].shape[1]
    y_ongrid = np.zeros((x1_size, x2_size))
    for i in range(x1_size): #TODO very very bad, arbitrary dimension as well? vectorize???
        for j in range(x2_size):
            for k in range(len(y)): # TODO VERY VERY VERY BAD
                if  x[k][0] == x_mesh[0][i][j] and x[k][1] == x_mesh[1][i][j]:
                    y_ongrid[i,j] = y[k]

    return y_ongrid

def get_localmax_brut(utility):
    loc_maxs = []
    size1 = utility.shape[0]
    size2 = utility.shape[1]

    for i in range(1, size1-1): #TODO very bad, make with a TF filter?

        if utility[i,0] > utility[i,1] and utility[i,0] > utility[i-1,0] and utility[i,0] > utility[i+1,0]:
            loc_maxs.append([i,0])

        for j in range(1, size2-1):
            if   utility[i,j] > utility[i,j+1] and \
                 utility[i,j] > utility[i,j-1] and \
                 utility[i,j] > utility[i+1,j] and \
                 utility[i,j] > utility[i-1,j]:
               loc_maxs.append([i,j])

        if utility[i,size2-1] > utility[i,size2-2] and utility[i,size2-1] > utility[i+1,size2-1] and utility[i,size2-1] > utility[i-1,size2-1]:
            loc_maxs.append([i,size2-1])

    for j in range(1, size2-1):
        if utility[0,j] > utility[1,j] and utility[0,j] > utility[0,j-1] and utility[0,j] > utility[0,j+1]:
            loc_maxs.append([0,j])
        if utility[size1-1,j] > utility[size1-2,j] and utility[size1-1,j] > utility[size1-1,j-1] and utility[size1-1,j] > utility[size1-1,j+1]:
            loc_maxs.append([size1-1,j])

    if utility[0,0] > utility[0,1] and utility[0,0] > utility[1,0]:
        loc_maxs.append([0,0])
    if utility[0,size2-1] > utility[0,size2-2] and utility[0,size2-1] > utility[1,size2-1]:
        loc_maxs.append([0,size2-1])
    if utility[size1-1,0] > utility[size1-1,1] and utility[size1-1,0] > utility[size1-2,0]:
        loc_maxs.append([size1-1,0])
    if utility[size1-1,size2-1] > utility[size1-1,size2-2] and utility[size1-1,size2-1] > utility[size1-2,size2-1]:
        loc_maxs.append([size1-1,size2-1])
    
    loc_maxs = [el for ind, el in enumerate(loc_maxs) if el not in loc_maxs[:ind]]
    return loc_maxs

def get_max_decomp(utility, n_batch=4):
        loc_loc_maxs = []
        #n_batch = 9
        n_batch_pd = int(math.pow(n_batch, 1/len(utility.shape)))
        subd_size1 = utility.shape[0] // n_batch_pd
        subd_size2 = utility.shape[1] // n_batch_pd

        for i in range(n_batch_pd - 1):
            for j in range(n_batch_pd - 1):
                #x_mesh_subd = x_mesh[0][i*subd_size1:(i+1)*subd_size1, j*subd_size2:(j+1)*subd_size2]
                utility_subd = utility[i*subd_size1:(i+1)*subd_size1, j*subd_size2:(j+1)*subd_size2]
                loc_loc_maxs.append( [sum(x) for x in zip( list( np.unravel_index(utility_subd.argmax(), utility_subd.shape)),
                                                           [i*subd_size1, j*subd_size2]) ])
            utility_subd = utility[i*subd_size1:(i+1)*subd_size1, (n_batch_pd-1)*subd_size2:]
            loc_loc_maxs.append( [sum(x) for x in zip( list( np.unravel_index(utility_subd.argmax(), utility_subd.shape)),
                                                       [i*subd_size1, (n_batch_pd-1)*subd_size2]) ])
            
        for j in range(n_batch_pd - 1):
            utility_subd = utility[(n_batch_pd-1)*subd_size1:, j*subd_size2:(j+1)*subd_size2]
            loc_loc_maxs.append( [sum(x) for x in zip( list( np.unravel_index(utility_subd.argmax(), utility_subd.shape)),
                                                       [(n_batch_pd-1)*subd_size1, j*subd_size2]) ])

        utility_subd = utility[(n_batch_pd-1)*subd_size1:, (n_batch_pd-1)*subd_size2:]
        loc_loc_maxs.append( [sum(x) for x in zip( list( np.unravel_index(utility_subd.argmax(), utility_subd.shape)), 
                                                   [(n_batch_pd-1)*subd_size1, (n_batch_pd-1)*subd_size2]) ])
        #print(utility_subd)                             
        #print(loc_loc_maxs)
        return loc_loc_maxs

def get_new_sample(x, utility):
    return x[utility.argmax()]

def get_new_candidates(x_mesh, utility): # TODO: get an array of candidates, for each "variance anti-node"
    cands = []
    ### --- find local maximum of sigma
    # work for 2D only now
    #print(utility)
    loc_maxs = []

    # peaks1 = []
    # peaks2 = []
    # for i in range(utility.shape[1]):
    #     print(utility[:,i])
    #     peaks2.append((np.where((utility[1:-1, i] > utility[0:-2, i]) 
    #                     * (utility[1:-1, i] > utility[2:, i]))[0] + 1))
    # for i in range(utility.shape[0]):
    #     peaks1.append(np.where((utility[i, 1:-1] > utility[i, 0:-2])
    #                 * (utility[i, 1:-1] > utility[i, 2:]))[0] + 1)
    # print(peaks1)
    # print(peaks2)
    # cands = x[peaks1 and peaks2]

    ### brute force local maxima search:
    all_localmax = True
    if all_localmax:
        loc_maxs = get_localmax_brut(utility)
        if len(loc_maxs) == 0:
            loc_maxs = [ list( np.unravel_index(utility.argmax(), utility.shape) ) ]
        #print(loc_maxs)

    ### --- easy option 1: split domain in fixes subdomains and find local max for every each
    ### --- make adaptive decomposition according to number of new points
    domain_split = False
    if domain_split:
        loc_maxs = get_max_decomp(utility, n_batch=9)

    for lm in loc_maxs:
        cands.append([ x_mesh[0][lm[0], lm[1]], x_mesh[1][lm[0], lm[1]] ])
    #print(cands)

    ### --- easy option 2: apply a mask for neighbours of the known points/ or threshold for too bad utility
    ### --- apply both and domain decompositions
    #nodes_inds = utility < 1e-6

    #for i in range(len(nodes_inds)-1):
    #    cands.append(utility[nodes_inds[i], nodes_inds[i+1]].argmax())

    ### --- option 3: make adaptive grid, considering geometrical element around known samples
    ### --- e.g. Voronoi tesselation and consider to mask a band around border?

    ### --- option 4: make a sparse grid (or just space filling set of points) and consider only them

    ### -- option 5: iterative optimiser started from several point in paramter space

    return cands

def stop_train_criterium_rsd(y_pred, sigma, eps=0.005):
    rsd_min = (sigma/abs(y_pred)).min()
    print('rsd : {}'.format(rsd_min))
    return rsd_min < eps

def stop_train_criterium_rmse(y_pred, y, eps=0.05): # TODO: get the reasonable rmse threshold for each given case + check for multivariate f-s
    rmse = np.sqrt(((y - y_pred) ** 2).sum() / len(y))
    print('rmse : {}'.format(rmse))
    return rmse < eps, rmse

def white_out_linear_trend(x_observ, y_observ): #TODO use some decomposition / better transfromation/ consider nonlinearity and higher dimensions
    #TODO: use Cholesky decomposition; consider SVD (may be for higher dimension)
    #thetas = np.zeros(x_observ.shape[1])
    reg = LinearRegression().fit(x_observ, y_observ)
    #thetas = reg.coef_
    #theta0 = reg.intercept_
    y_observ_trend = reg.predict(x_observ)
    y_whitened = y_observ - y_observ_trend
    return y_whitened, reg

def data_preprocessing(x_observ, y_observ):
    return x_observ, y_observ

def white_reverse_linear_trend(x_observ, y_observ_white, reg):
    
    y_observ_trend = reg.predict(x_observ)
    y_observ = y_observ_white + y_observ_trend

    return y_observ

def GPR_analysis_toy(x_data, x_domain, y_par=[0.1, 9.9, 20], x_par=[0, 10, 64], f=lambda x: x*np.sin(x), eps=1.0, scale=1e7):

    # case: with noise - NO
    #X = np.atleast_2d(np.linspace(y_par[0], y_par[1], y_par[2])).T

    x_observ = np.atleast_2d(x_data[:, 0]).T
    y_observ = f(x_observ).ravel() #TODO reuse the old function evaluations

    y_observ = y_observ/scale #scale naive-est

    # add noise - NO
    # dy = 0.5 + eps * np.random.random(y.shape)
    #noise = np.random.normal(0, dy)
    #y += noise

    # GP model - kernels
    # TODO: compose a suitable kernel/ methods to defince kernel
    #kernel = ConstantKernel(1.0, (1e-3, 1e3)) * RBF(10, (1e-2, 1e2))
    kernel = ConstantKernel() + Matern() # + WhiteKernel(1.0)
    #kernel = ConstantKernel(1e7, (1e-8, 1e+10)) + Matern(length_scale=1e1, length_scale_bounds=(1e-8, 1e+10)) # for GEM in Te/Ti unscaled

    #gp = GaussianProcessRegressor(kernel=kernel, alpha=dy**2, n_restarts_optimizer=9)
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9) # no noise

    start_ts = time.time()
    gp.fit(x_observ, y_observ)
    print("time to train GP model: " + str(time.time() - start_ts) + " seconds")

    # predictions + MSE
    y_pred, sigma = gp.predict(x_domain, return_std=True)

    y_observ = y_observ*scale  #scale naive-est
    y_pred = y_pred*scale
    sigma = sigma*scale

    #print(sigma)
    return x_observ, y_observ, y_pred, sigma

def GPR_analysis_2d(x_observ, y_observ, x_domain, x_par=[[0.,1.,8],[0.,1.,8]]):
    
    #y_sc_factor = 1e4
    #x_sc_factor = 1e4

    # --- observation in Y for fitting
    #y_observ = f(x_observ) # TODO: pass instead of recalcualtion

    ##print("x_domain: "); print(x_domain)
    ##print("x_observ_sc: "); print(x_observ)
    ##print("y_observ_sc: "); print(y_observ)

    # --- domain points for prediction
    #x1 = np.linspace(*x_par[0]) # TODO use passed domain
    #x2 = np.linspace(*x_par[1])
    #x = np.transpose([np.tile(x1, len(x2)), np.repeat(x1, len(x2))])
    #x = x

    y_observ_new, reg = white_out_linear_trend(x_observ, y_observ)

    #kernel = ConstantKernel() + Matern + WhiteKernel(1e-4) # TODO white kernel has issues with singularities? e.g. gets log(0) somewhere?
    kernel = ConstantKernel() + ConstantKernel() * Matern() #TODO after preprocessing to white noize at [0;1]^2 (better [-1;1]^2 ?) consider scale limits + get initials from datapoints??? 
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9)

    start_ts = time.time()
    gp.fit(x_observ, y_observ_new)
    print("time to train GP model: " + str(time.time() - start_ts) + " seconds")
    #print(gp.get_params(True))

    y_pred, sigma = gp.predict(x_domain, return_std=True) 

    ##print("sigma: "); print(sigma)
    ##print("y_pred: "); print(y_pred)

    y_pred_new = white_reverse_linear_trend(x_domain, y_pred, reg)

    return x_observ, y_observ, y_pred_new, sigma, gp
    #return x_observ * x_sc_factor, y_observ * y_sc_factor, y_pred * y_sc_factor, sigma * y_sc_factor

def GPR_analysis(dim=2, xdomain_par=[[-1.,1.,8],[-1.,1.,8]], func=exponential_model): #TODO arbitrary dimension, print resuluts in a clear way
    xdomain_par =[ 0., 1., 8]**dim # TODO move into initalisation list
    xdata = np.linspace(xdomain_par) # TODO move inot initialisation list

    X = xdata
    y = np.array([func(coord, xdomain_par) for coord in X]).reshape(-1,1)

    #x0 = np.linspace(domain_par[0], domain_par[1], domain_par[2])
    #x1 = np.linspace(domain_par[3], domain_par[4], domain_par[5])
    #x = np.transpose([np.tile(x0, len(x1)), np.repeat(x0, len(x1))])

    kernel = C() + Matern() + WhiteKernel()
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9)

    start_ts = time.time()
    gp.fit(X, y)
    print("time to train GP model: " + str(time.time() - start_ts) + " seconds")

    y_pred, sigma = gp.predict(x, return_std=True) 

    return y_pred, sigma

def FFNN_Regression_toy(y=0, f=0, n=20, eps=1.0):

    # function to fit
    if f==0:
        f = lambda x:  x * np.sin(x)

    # case: noise
    if y==0:
        X = np.atleast_2d(np.linspace(0.1, 9.9, n)).T
        y = f(X).ravel()
        dy = 0.5 + eps * np.random.random(y.shape)
        noise = np.random.normal(0, dy)
        y += noise

    # input space mesh, the prediction and
    x = np.atleast_2d(np.linspace(0, 10, 1000)).T

    # Analysis: network set-up, training, exploitation
    ffnr = MLPRegressor(random_state=1, max_iter=500)
    ffnr.fit(X, y)
    y_pred = ffnr.predict(x)

    # Plot function,prediction and 95% confidence interval
    plt.figure()
    plt.plot(x, f(x), 'r:', label=r'$f(x) = x\,\sin(x)$')
    plt.errorbar(X.ravel(), y, dy, fmt='r.', markersize=10, label='Observations')
    plt.plot(x, y_pred, 'b-', label='Prediction')
    # plt.fill(np.concatenate([x, x[::-1]]),
    #         np.concatenate([y_pred - 1.9600 * sigma,
    #                         (y_pred + 1.9600 * sigma)[::-1]]),
    #         alpha=.5, fc='b', ec='None', label='95% confidence interval')
    plt.xlabel('$x$')
    plt.ylabel('$f(x)$')
    plt.ylim(-10, 20)
    plt.legend(loc='upper left')
    plt.show(block=True)

def get_1d_slice(x_domain, y_test, x_observ, y_observ):
    x1_slice_value = x_observ[32*17, 0]
    x_observ_inds = x_observ[:,0]==x1_slice_value
    x_observ_slice = x_observ[x_observ_inds, 0]
    y_observ_slice = y_observ[x_observ_inds]

def decompose_domain_binary(x_domain, x_split_ind):
    """
     for now 1D array split into two
    :param x_domain: numpy array of 1d, every point is and obsereved x
    """
    x_domains_new = [ x_domain[0:x_split_ind], x_domain[x_split_ind+1,-1] ]
    # TODO: x_split = *[0, xsplit, -1]; x_domains_new = [for x_split in x_split_ind x[x_split-1:x_split]]  
    
    return x_domains_new

def test_gp_domain_decomposition(x_domain, y_test, x_observ, y_observ):
    # Runs through a 1D array, splits it into 2 parts and trains a pair or models for 2 subdomains
    # checks which binary domain decompostion leads to a better fit
    kernal = kernel = ConstantKernel() + ConstantKernel() * Matern()
    gp = GaussianProcessRegressor(kernel=kernel, n_restart_optimizer=9)
    errs = []

    for ind in range(len(x_domain)):
        models = []
        thetas = []
        errloc = 0.
        x_domains = decompose_domain_binary(x_domain)
        y_preds = decompose_domain_binary(y_pred)
        y_tests = decompose_domain_binary(y_test)
        for i in range(len(x_domains)-1):
            models.append(gp.fit(x_domains[i], y_observ))
            thetas.append(gp.kernel.theta)
            _, err = stop_train_criterium_rmse(y_pred, y_test.T.reshape(-1))
            errloc = err + errloc
            print(thetas)
        errs.append(errloc)
    
    plt.plot(range(len(x_domain)), errs, label='error of the splitting')
    return 0

def prep_1d_gem0_data():

    n_init = 8
    ext_code_helper_1 = ExtCodeHelper(1)

    function = lambda x: ext_code_helper_1.gem0_call_teflteval_array(x)
    x_param = [400., 2000, 32] # for gem in te-val #TODO change the gradient sampling!
        
    x_obs = np.zeros((n_init, 2))
    x_domain = np.atleast_2d(np.linspace(*x_param)).T
    y_test = function(x_domain) # TODO: some of the things e.g. x_domain are never changed - should be returned all the time

    plot_response_1d(x_domain, y_test)

    #y_scaling = lambda y: (y - y_test.min()) / (y_test.max() - y_test.min()) # scale to [0;1]
    #x_scaling = lambda x: (x - x_domain.min()) / (x_domain.max() - x_domain.min())
    #simple_nonstationary = lambda x, y: y - y_test.min() + (x - x_domain.min())*(y_test.max() - y_test.min())/(x_domain.max() - x_domain.min()) # TODO check if intependent calls are comiled out
        
    n_init = 6
    # data[:, 0] = np.linspace(*x_param[:-1], n_init)
    x_obs[:, 0] = np.random.rand(n_init)*(x_param[1] - x_param[0]) + x_param[0] #TODO either choose among grid points or make grid irregular
    y_obs = function(x_obs)

    return x_domain, y_test, x_obs, y_obs

def surrogate_loop(pardim):
    np.random.seed(int(time.time()))
    errors = []

    new_points = []
    n_init = 4

    ext_code_helper_1 = ExtCodeHelper(1)
    ext_code_helper_4 = ExtCodeHelper(4)

    if pardim == 1:
        #function = lambda x: x * np.cos(1.0 * x)
        #function = lambda x: (np.e**(-1.0 * x)) * np.cos(2.0 * np.pi * x)
        #function = lambda x: np.e**(+1.0*x)
        #x_param = [0., 1.5, 32] for cos 

        function = lambda x: ext_code_helper_1.gem0_call_teflteval_array(x)
        x_param = [400., 2000, 32] # for gem in te-val #TODO change the gradient sampling!
        
        function4 = lambda x: np.array(ext_code_helper_4.gem0_call_tefltegrad_array(x))
        x_param = [-6000., -200., 64] # for gem in te-grad

        function1 = lambda x: np.array(ext_code_helper_1.gem0_call_tefltegrad_array(x))
        x_param = [-3800., -2000., 32]

        #function = lambda x: np.array(ext_code_helper.gem0_call_tifltigrad_array(x))
        #x_param = [-5000., -500., 45] # for gem in ti-grad # 09.12 plot the reponse. is there a local minimum? equilibrium at -24202420?
        
        x_data = np.zeros((n_init, 2))
        x_domain = np.atleast_2d(np.linspace(*x_param)).T
        y_test = function4(x_domain) # TODO: some of the things e.g. x_domain are never changed - should be returned all the timei

        y_test1 = function(x_domain)
        plot_response_1d(x_domain, [y_test, y_test1], ylabels=['chigb4', 'chigb1'])

        y_scaling = lambda y: (y - y_test.min()) / (y_test.max() - y_test.min()) # scale to [0;1]
        x_scaling = lambda x: (x - x_domain.min()) / (x_domain.max() - x_domain.min())
        simple_nonstationary = lambda x, y: y - y_test.min() + (x - x_domain.min())*(y_test.max() - y_test.min())/(x_domain.max() - x_domain.min()) # TODO check if intependent calls are comiled out
        
        # data[:, 0] = np.linspace(*x_param[:-1], n_init)
        x_data[:, 0] = np.random.rand(n_init)*(x_param[1] - x_param[0]) + x_param[0] #TODO either choose among grid points or make grid irregular

        for i in range(16):
            x_observ, y_observ, y_pred, sigma = GPR_analysis_toy(x_data, x_domain, y_par=x_param, x_par=x_param, f=function, eps=0.0)
            x_n = get_new_sample(x_domain, sigma)
            plot_prediction_variance(x_observ, y_observ, x_domain, y_test, y_pred, sigma, function, [x_n], new_points, rmse=0.0, funcname='gem0, flTi in gradTi')

            stop_crit, err = stop_train_criterium_rmse(y_pred, y_test.T.reshape(-1), 1e3) # for normalized problems chooes rmse threshold ~0.05
            errors.append(err)

            new_points = [x_n] #TODO choice is always chosen from grid -> apply simple refinement?
            x_data = np.concatenate((x_data, np.array([[x_n[0], 0.0]])), axis=0) #TODO adapt grid around new candidates? 
            #data = np.append(data, x_n.reshape(1,-1), axis=0)
            if stop_crit:
                print("Reached stopping criterium!")
                break

    elif pardim == 2:
        # --- Chose the fuction and its domain

        #function = lambda x: (np.e**(-1. * x[:,0] - 1. * x[:,1])) * np.cos(np.pi * (x[:,0]*x[:,0] + x[:,1]*x[:,1]))  
        #x_param = [[0., 1.5, 8], [0., 1.5, 8]]
        #y_scale = 1.

        #function = lambda x: np.array(gem0_call_tefltevltivl_array(x)) # TODO double check numpy dimensions
        function = lambda x: np.array(ext_code_helper.gem0_call_tefltevltegrad_array(x))
        #x_param = [[200., 4800, 128], [-8000, 0., 128]] # square/rectangle in domain in {Te}x{gradTe}
        x_param = [[400., 2400, 16], [-3600., 0., 16]] 

        # --- Prepare the domain in X and test Y values
        x1 = np.linspace(*x_param[0])
        x2 = np.linspace(*x_param[1])
        x_domain = np.transpose([np.tile(x1, len(x2)), np.repeat(x2, len(x1))]) #TODO very bulky and ineffective? -> more dims?
        y_test = function(x_domain)

        ### MESHES FOR TEST SPACE ----------------------------------------------------------
        ### x and y values arranged in a grid now:
        # get dimentionality if parameter space i.e. number of features
        ndim_domain = 2 # x_domain.shape[0]
        # from X domain paramters get a tuple to descibe number of points along each dimension 
        y_size = (x_param[0][-1],)
        for i in range(1, ndim_domain):
            y_size += (x_param[i][-1],)
        y_test_ongrid = np.zeros(y_size)

        x_meshes = [x1, x2]
        x_size = y_size + (2,)
        x_domain_ongrid = np.zeros((x_size))
        x_domain_mesh = np.meshgrid(*x_meshes)

        #print(x_domain_mesh[0].shape)
        #print(len(x1), len(x2))
        y_domain = np.zeros(x_domain_mesh[0].shape) # TODO the slowest part of the initialization
        for i in range(len(x1)): #TODO very very bad, arbitrary dimension as well? vectorize???
            for j in range(len(x2)):
                y_domain[i,j] = function([ [x_domain_mesh[0][i, j], x_domain_mesh[1][i, j]] ])

        #print(y_domain)
        #print(y_test)

        ### INITIAL TRAINING POINTS (RANDOM) ------------------------------------------------
        # --- Chose one/several of the grid point as initial point at random
        x_data = np.zeros((n_init,2))
        x_data[:, 0] = x1[np.random.randint(low=0, high=len(x1)-1, size=n_init)]
        x_data[:, 1] = x2[np.random.randint(low=0, high=len(x2)-1, size=n_init)]
        #x_data[:,0] = np.random.rand(n_init)*(x_param[0][1] - x_param[0][0]) + x_param[0][0] # chose of rand unmber in domain in 1d
        #x_data[:,1] = np.random.rand(n_init)*(x_param[1][1] - x_param[1][0]) + x_param[1][0]

        # --- Some data and functions need to preprocessing - for now only done when dealing wiht GPR
        x_scale = 1. #1e4
        x_domain_min = np.amin(x_domain, 0)
        x_domain_max = np.amax(x_domain, 0)
        x_scaling = lambda x: (x - x_domain_min) / (x_domain_max - x_domain_min)
        x_rev_scaling = lambda x: x_domain_min + x * (x_domain_max - x_domain_min)

        y_test_min = np.amin(y_test, 0)
        y_test_max = np.amax(y_test, 0)
        y_test_scaling = lambda y: (y - y_test_min) / (y_test_max - y_test_min)
        y_test_rev_scaling = lambda y: y_test_min + y * (y_test_max - y_test_min) 

        simple_whitening = lambda x, y: y - y_test_min + (x - x_domain_min)*(y_test_max - y_test_min)/(x_domain_max - x_domain_min) # TODO check if intependent calls are compiled out

        ##print("y_test: "); print(y_test)

        for i in range(5):
            print("iteration nu {}".format(i))
            start_ts = time.time()

            y_observ = function(x_data)
            # --- some scaling depends on observed data:
            #y_observ_min = y_observ.min()
            #y_observ_max = y_observ.max()
            #y_observ_scaling = lambda y: (y - y_observ_min) / (y_observ_max - y_observ_min)
            #y_observ_rev_scaling = lambda y: y_observ_min + y *  (y_observ_max - y_observ_min)

            # --- fit the regeresson and chose a new sample
            x_observ, y_observ, y_pred, sigma, model = GPR_analysis_2d(
                                                 np.apply_along_axis(x_scaling, 1, x_data), 
                                                 np.apply_along_axis(y_test_scaling, 0, y_observ),
                                                 np.apply_along_axis(x_scaling, 1, x_domain),
                                                 x_par=x_param) # TODO: use GPflow implementation, reuse intermediate data (older covariance)

            x_observ = np.apply_along_axis(x_rev_scaling, 1, x_observ)
            y_observ = np.apply_along_axis(y_test_rev_scaling, 0, y_observ)
            y_pred = np.apply_along_axis(y_test_rev_scaling, 0, y_pred)
            sigma = np.apply_along_axis(y_test_rev_scaling, 0, sigma)

            #for datas in y_observ, y_pred, sigma:
            #    datas = np.apply_along_axis(y_test_rev_scaling, 0, datas)
            
            ### --- Chose samples for the new model
            start_resample_ts = time.time()
            #x_n = get_new_sample(x_domain, sigma)
            sigma_ongrid = get_ongrid(x_domain_mesh, x_domain, sigma)
            x_new_batch = get_new_candidates(x_domain_mesh, sigma_ongrid)
            #print('new {} samples: {}'.format(len(x_new_batch), x_new_batch))
            new_points = (np.array(x_new_batch), function(np.array(x_new_batch)))
            print("Choosing new samples took: " + str(time.time() - start_resample_ts) + " seconds")

            # --- Check the error for convergence
            stop_crit, err = stop_train_criterium_rmse(y_pred, y_test, 1e2) #1e4
            errors.append(err)

            if i%1 == 0:
                plot_prediction_variance_2d(x_observ, y_observ, x_domain, y_test, y_pred, sigma, new_points, funcname='gem0')

            #x_data_old = np.append(x_data, x_n.reshape(1, -1), axis=0)
            x_data = np.append(x_data, x_new_batch, axis=0)
            print("Single training iteration took: " + str(time.time() - start_ts) + " seconds")
            if stop_crit:
                print("Reached stopping criterium!")
                break

    
    #TODO: utilize the model: save a model file, use in a UQ loop
    fin_mod_path = os.path.join(os.path.abspath('../data/models'), 'gpr_mod_gem.joblib')
    dump(model, fin_mod_path)

    pd.DataFrame(np.concatenate((x_domain, y_pred.reshape(-1,1)), axis=1), columns={'te.value', 'te.ddrho', 'te.flux'}) \
                 .to_csv(os.path.join(os.path.join('../data'), 'surrogate_results.csv'))


    #get_1d_slice(x_domain, y_test, x_observ, y_observ)

    plot_error(errors, 'RMSE')

    # --- plot histograms of results
    y_observ_clean = np.apply_along_axis(y_test_scaling, 0, y_observ)
    y_observ_clean, _ = white_out_linear_trend(x_observ, y_observ_clean)
    y_pred_clean = np.apply_along_axis(y_test_scaling, 0, y_pred)
    y_pred_clean, _ = white_out_linear_trend(x_domain, y_pred_clean)
    #plot_histograms(y_observ, y_observ_clean, y_pred, y_pred_clean)

def surrogate_utility(x_train_data, y_train_data, x_roi_data, original_model):
    # Error Reduction Estimation: acquisition function for sampling next point to a surragte based on inferred
    # variance of the model on current iterations 
    utility = []
    kernel = RBF()
    gpr = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=4)

    for x in x_roi_data:
        x_new_train_data = x_train_data.append(x)
        y_new_train_data = y_train_data.append(original_model(x))
        gpr = gpr.fit(x_new_train_data, y_new_train_data)
        x_new_test_data = np.delete(x_roi_data, np.argwhere(x_roi_data == x))
        y_new_test_data = np.array([original_model(x) for x in x_new_test_data])
        y_new_pred_data, new_var = gpr.pred(x_new_test_data)
        utility.append(new_var)

    return -utility.sum()

plt.ion()

### --- Surrogate loop
#surrogate_loop(1)

#GP split
#x_domain, y_test, x_obs, y_obs = prep_1d_gem0_data()
#test_gp_domain_decomposition(x_domain, y_test, x_obs, y_obs)

### --- Get data from GEM0 for offline training
# gem0_helper = ExtCodeHelper(1)
#
# function = lambda x: np.array(gem0_helper.gem0_call_tifltegradtigrad_array(x))  # TODO TODO add gem0 check the rho taken and if a range of values around is sampled
# x_param = [[-5000., -1000., 32], [-5000., -1000., 32]]
#
# import lhsmdu
#
# x_domain = lhsmdu.sample(2, 50).reshape(-1, 2)
# for dim in range(len(x_param)):
#     x_domain[:, dim] = x_param[dim][0] + x_domain[:, dim] * (x_param[dim][1] - x_param[dim][0])
#
# x_domain = np.array(x_domain)
# y_test = function(x_domain)
#
# df = pd.DataFrame()
#
# df['te.ddrho'] = x_domain[:, 0]
# df['ti.ddrho'] = x_domain[:, 1]
# df['ti.flux']  = y_test[:, 0]
#
# df.to_csv('gem0_lhc_res.csv')

### --- Produce GEM0 results for coordiantes of paramteric space read from a GEM campaign
gem0_helper = ExtCodeHelper(3)
function = lambda x: np.array(gem0_helper.gem0_call_4param2target_array(x))

def load_csv_file(data_dir='', input_file='gem_data_625.txt'):
    N_runs = 625
    input_dim = 4
    output_dim = 2

    input_samples = np.zeros((N_runs, input_dim))
    output_samples = np.zeros((N_runs, output_dim))

    #nput_file = data_dir + '\\' + input_file

    with open(input_file, 'r') as inputfile:
        datareader = csv.reader(inputfile, delimiter=',')
        i = 0
        for row in datareader:
            input_samples[i] = row[0:input_dim]
            output_samples[i] = row[input_dim:input_dim+output_dim]
            i = i + 1

    return input_samples, output_samples

x_domain, y_gem  = load_csv_file()
#y_test = function(x_domain)
#y_test = y_test.reshape(625, 2)

df = pd.DataFrame()

df['te.value'] = x_domain[:, 0]
df['ti.value'] = x_domain[:, 1]
df['te.ddrho'] = x_domain[:, 2]
df['ti.ddrho'] = x_domain[:, 3]

#df['te.flux'] = y_test[:, 0]
#df['ti.flux'] = y_test[:, 1]

df.to_csv('gem0_625_res.csv')

### --- Try to fit GEM0 for GEM data

from scipy.optimize import curve_fit

def func(x,
         th,
         #br,
         #ep,
         #cd,
         #cp
         ):
    y_res = gem0_helper.gem0obj.gem0_fit_call(x,
                                              th,
                                              # br,
                                              # ep,
                                              #cd,
                                              #cp
                                              )
    res = np.array(y_res)
    return res[:, 1]

function = lambda x: np.array(gem0_helper.gem0_call_tifltigrad_array(x))

inds = grid_slice(x_domain,[3])
xcur = x_domain[inds, [3]].reshape(-1, 1)
print(y_gem[inds, 1])
print(function(xcur).reshape(-1))

#popt, pconv = curve_fit(func, x_domain[:25, [3]], y_gem[:25, 1],
popt, pconv = curve_fit(func, xcur, y_gem[inds, 1],
                                                  p0=[
                                                       6.,
                                                       # 10.,
                                                       # 3.5,
                                                       # 3.,
                                                       # 0.7
                                                      ],
                                                     bounds=(0.01, 1000.0),
                                                     method='trf',
                                                     #jac='3-point'
                        )  # check if should actually be (625,4) + add bounds
#  OptimizeWarning: Covariance of the parameters could not be estimated
#   category=OptimizeWarning) ... [[inf ..]]

print(popt)
print(pconv)

print(func(xcur, *popt))
