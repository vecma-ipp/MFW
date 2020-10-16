import pandas as pd
import numpy as np
import math
import matplotlib.pylab as plt
import time

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF, WhiteKernel, ConstantKernel
from sklearn.neural_network import MLPRegressor

from sklearn.linear_model import LinearRegression

from da_utils import *

import sys
import os

#TODO: make a new package + install / or get relative paths consistent
sys.path.append(os.path.abspath("../../standalone/src/custom_codes/gem0"))
import importlib.util
spec = importlib.util.spec_from_file_location("gem0_singleton", os.path.abspath("../../standalone/src/custom_codes/gem0/gem0_singleton.py"))
gem0_singleton = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gem0_singleton)
from gem0_singleton import GEM0Singleton

def get_new_sample(x, utility):
    return x[utility.argmax()]

def get_new_candidates(x, utility): # TODO: get an array of candidates, for each "variance anti-node"
    cands = []
    nodes_inds = utility < 1e-6
    for i in range(len(nodes_inds)-1):
        cands.append(utility[nodes_inds[i], nodes_inds[i+1]].argmax())
    return cands

def stop_train_criterium_rsd(y_pred, sigma, eps=0.005):
    rsd_min = (sigma/abs(y_pred)).min()
    print('rsd : {}'.format(rsd_min))
    return rsd_min < eps

def stop_train_criterium_rmse(y_pred, y, eps=0.05): # TODO: get the reasonable rmse threshold for each given case + check for multivariate f-s
    rmse = np.sqrt(((y - y_pred) ** 2).sum() / len(y))
    print('rmse : {}'.format(rmse))
    return rmse < eps, rmse

def white_out_linear_trend(x_observ, y_observ):
    #thetas = np.zeros(x_observ.shape[1])
    reg = LinearRegression().fit(x_observ, y_observ)
    #thetas = reg.coef_
    #theta0 = reg.intercept_
    y_observ_trend = reg.predict(x_observ)
    y_whitened = y_observ - y_observ_trend
    return y_whitened, reg

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
    kernel = ConstantKernel() + Matern() #TODO after preprocessing to white noize at [0;1]^2 (better [-1;1]^2 ?) consider scale limits + get initials from datapoints??? 
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9)

    start_ts = time.time()
    gp.fit(x_observ, y_observ_new)
    print("time to train GP model: " + str(time.time() - start_ts) + " seconds")

    y_pred, sigma = gp.predict(x_domain, return_std=True) 

    ##print("sigma: "); print(sigma)
    ##print("y_pred: "); print(y_pred)

    y_pred_new = white_reverse_linear_trend(x_domain, y_pred, reg)

    return x_observ, y_observ, y_pred_new, sigma
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

def surrogate_loop(pardim):
    np.random.seed(int(time.time()))
    errors = []

    gem0obj = GEM0Singleton()

    def gem0_call_teflteval_array(x):
        res = []
        for el in x: 
            res.append([gem0obj.gem0_call({'te.value': el[0]})[0]])
        return np.array(res)

    def gem0_call_teflteval_log_array(x):
        res = []
        for el in x: 
            res.append([math.log(gem0obj.gem0_call({'te.value': el[0]})[0])]) #TODO actually negative values
        return np.array(res)

    def gem0_call_tefltegrad_array(x):
        res = []
        for el in x: 
            res.append([gem0obj.gem0_call({'te.ddrho': el[0]})[0]])
        return np.array(res)

    def gem0_call_tefltevltegrad_array(x):
        """
        calls the gem0 code for desired te.valus and te.ddrho
        :param x: x[0] is desired tevalue, x[1] is desired tegrad
        """
        res = []
        for el in x:
            res.append(gem0obj.gem0_call({'te.value': el[0], 'te.ddrho': el[1]})[0])
        return res

    def gem0_call_tefltevltivl_array(x):
        """
        calls the gem0 code for desired te.valus and te.ddrho
        :param x: x[0] is desired tevalue, x[1] is desired tegrad
        """
        res = []
        for el in x:
            res.append(gem0obj.gem0_call({'te.value': el[0], 'ti.value': el[1]})[0])
        return res

    new_points = []
    n_init = 1

    if pardim == 1:
        #function = lambda x: x * np.cos(1.0 * x)
        #function = lambda x: (np.e**(-1.0 * x)) * np.cos(2.0 * np.pi * x)
        #function = lambda x: np.e**(+1.0*x)
        #x_param = [0., 1.5, 32] for cos 

        function = lambda x: gem0_call_teflteval_array(x)
        x_param = [400., 2000, 32] # for gem in te-val #TODO change the gradient sampling!
        
        function = lambda x: np.array(gem0_call_tefltegrad_array(x))
        x_param = [-6000., -200., 64] # for gem in te-grad
        
        x_data = np.zeros((n_init, 2))
        x_domain = np.atleast_2d(np.linspace(*x_param)).T
        y_test = function(x_domain) # TODO: some of the things e.g. x_domain are never changed - should be returned all the time

        y_scaling = lambda y: (y - y_test.min()) / (y_test.max() - y_test.min()) # scale to [0;1]
        x_scaling = lambda x: (x - x_domain.min()) / (x_domain.max() - x_domain.min())
        simple_nonstationary = lambda x, y: y - y_test.min() + (x - x_domain.min())*(y_test.max() - y_test.min())/(x_domain.max() - x_domain.min()) # TODO check if intependent calls are comiled out
        
        # data[:, 0] = np.linspace(*x_param[:-1], n_init)
        x_data[:, 0] = np.random.rand(n_init)*(x_param[1] - x_param[0]) + x_param[0] #TODO either choose among grid points or make grid irregular

        for i in range(16):
            x_observ, y_observ, y_pred, sigma = GPR_analysis_toy(x_data, x_domain, y_par=x_param, x_par=x_param, f=function, eps=0.0)
            x_n = get_new_sample(x_domain, sigma)
            plot_prediction_variance(x_observ, y_observ, x_domain, y_test, y_pred, sigma, function, [x_n], new_points, funcname='gem0, flTe in Te')

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
        function = lambda x: np.array(gem0_call_tefltevltegrad_array(x))
        x_param = [[200., 4800, 64], [-8000, 0., 64]] # square/rectangle in domain in {Te}x{gradTe}
        #x_param = [[400., 2400, 32], [-3600., 0., 32]] 

        # --- Prepare the domain in X and test Y values
        x1 = np.linspace(*x_param[0])
        x2 = np.linspace(*x_param[1])
        x_domain = np.transpose([np.tile(x1, len(x2)), np.repeat(x2, len(x1))]) #TODO very bulky and ineffective? -> more dims
        y_test = function(x_domain)

        # --- Chose one of the grid point as initial point at random
        x_data = np.zeros((n_init,2))
        x_data[:, 0] = x1[np.random.randint(low=0, high=len(x1)-1, size=n_init)]
        x_data[:, 1] = x2[np.random.randint(low=0, high=len(x2)-1, size=n_init)]
        #x_data[:,0] = np.random.rand(n_init)*(x_param[0][1] - x_param[0][0]) + x_param[0][0] # chose of rand unmber in domain in 1d
        #x_data[:,1] = np.random.rand(n_init)*(x_param[1][1] - x_param[1][0]) + x_param[1][0]

        # --- Some data and functions need to preprocessin - for now only done when dealing wiht GPR
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

        for i in range(80):
            print("iteration nu {}".format(i))

            y_observ = function(x_data)
            # --- some scaling depends on observed data:
            y_observ_min = y_observ.min()
            y_observ_max = y_observ.max()
            y_observ_scaling = lambda y: (y - y_observ_min) / (y_observ_max - y_observ_min)
            y_observ_rev_scaling = lambda y: y_observ_min + y *  (y_observ_max - y_observ_min)

            # --- fit the regeresson and chose a new sample
            x_observ, y_observ, y_pred, sigma = GPR_analysis_2d(
                                                 np.apply_along_axis(x_scaling, 1, x_data), 
                                                 np.apply_along_axis(y_test_scaling, 0, y_observ),
                                                 np.apply_along_axis(x_scaling, 1, x_domain),
                                                 x_par=x_param)

            x_observ = np.apply_along_axis(x_rev_scaling, 1, x_observ)
            y_observ = np.apply_along_axis(y_test_rev_scaling, 0, y_observ)
            y_pred = np.apply_along_axis(y_test_rev_scaling, 0, y_pred)
            sigma = np.apply_along_axis(y_test_rev_scaling, 0, sigma)

            #for datas in y_observ, y_pred, sigma:
            #    datas = np.apply_along_axis(y_test_rev_scaling, 0, datas)
            
            # --- chose samples for the new model
            x_n = get_new_sample(x_domain, sigma)

            stop_crit, err = stop_train_criterium_rmse(y_pred, y_test, 0.05) #1e4
            errors.append(err)

            new_points = (x_n, function(np.array([x_n])))
            if i%1 == 0:
                plot_prediction_variance_2d(x_observ, y_observ, x_domain, y_test, y_pred, sigma, new_points, funcname='gem0')

            x_data = np.append(x_data, x_n.reshape(1, -1), axis=0)
            if stop_crit:
                print("Reached stopping criterium!")
                break
    
    #x1_slice_value = x_observ[32*17, 0]
    #x_observ_inds = x_observ[:,0]==x1_slice_value
    #x_observ_slice = x_observ[x_observ_inds, 0]
    #y_observ_slice = y_observ[x_observ_inds]

    plot_error(errors, 'RMSE')

def surrogate_utility(x_train_data, y_train_data, x_roi_data, original_model):
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


#np.random.seed(2)  # check other random seeds
plt.ion()

### surrogate loop:
#surrogate_loop(2)
surrogate_loop(2)