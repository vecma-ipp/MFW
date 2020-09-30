import pandas as pd
import numpy as np
import matplotlib.pylab as plt
#import datetime
import time

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF, WhiteKernel, ConstantKernel
from sklearn.neural_network import MLPRegressor

from da_utils import *

def get_new_sample(x, utility):
    return x[utility.argmax()]

def stop_train_criterium_rsd(y_pred, sigma, eps=0.005):
    rsd_min = (sigma/abs(y_pred)).min()
    print('rsd : {}'.format(rsd_min))
    return rsd_min < eps

def stop_train_criterium_rmse(y_pred, y, eps=0.05):
    rmse = np.sqrt(((y - y_pred) ** 2).sum() / len(y))
    print('rmse : {}'.format(rmse))
    return rmse < eps, rmse

def GPR_analysis_toy(data, y_par=[0.1, 9.9, 20], x_par=[0, 10, 64], f=lambda x: x*np.sin(x), eps=1.0):

    # case: with noise - NO
    #X = np.atleast_2d(np.linspace(y_par[0], y_par[1], y_par[2])).T

    x_observ = np.atleast_2d(data[:, 0]).T
    y_observ = f(x_observ).ravel()

    # add noise - NO
    # dy = 0.5 + eps * np.random.random(y.shape)
    #noise = np.random.normal(0, dy)
    #y += noise

    # input space mesh, the prediction and
    x_domain = np.atleast_2d(np.linspace(*x_par)).T

    # GP model
    #kernel = ConstantKernel(1.0, (1e-3, 1e3)) * RBF(10, (1e-2, 1e2))  # initial kernel is N(1.0, 0.01)
    #kernel = ConstantKernel() + RBF() 
    kernel = ConstantKernel() + Matern(1.0) # + WhiteKernel(1.0)
    #gp = GaussianProcessRegressor(kernel=kernel, alpha=dy**2, n_restarts_optimizer=9)
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9) # no noise

    #start_ts = datetime.datetime.now()
    start_ts = time.time()
    gp.fit(x_observ, y_observ)
    #print("time to train GP model: " + str((datatime.datatime.now() - start_ts).total_seconds()) + " seconds")
    print("time to train GP model: " + str(time.time() - start_ts) + " seconds")

    # predictions + MSE
    y_pred, sigma = gp.predict(x_domain, return_std=True)

    # plot predictions with variance
    #y_test = plot_prediction_variance(x_observ, y_observ, x, y_pred, sigma, f)
    #return x, y_test, y_pred, sigma

    return x_observ, y_observ, x_domain, y_pred, sigma

def GPR_analysis_2d(data, domain_par, func):

    X = data
    y = np.array([func(coord, [1.0, 1.0, 1.0]) for coord in X]).reshape(-1,1)

    x0 = np.linspace(domain_par[0], domain_par[1], domain_par[2])
    x1 = np.linspace(domain_par[3], domain_par[4], domain_par[5])
    x = np.transpose([np.tile(x0, len(x1)), np.repeat(x0, len(x1))])

    kernel = C() + Matern([1.0, 1.0]) + WhiteKernel(1e-6)
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9)

    #start_ts = datetime.datetime.now()
    start_ts = time.time()
    gp.fit(X, y)
    #print("time to train GP model: " + str((datatime.datatime.now() - start_ts).total_seconds()) + " seconds")
    print("time to train GP model: " + str(time.time() - start_ts) + " seconds")

    y_pred, sigma = gp.predict(x, return_std=True)

    #plot_prediction_variance(X, y, x, y_pred, sigma, func)

    return x, y, y_pred, sigma

def FNN_Regression_toy(y=0, f=0, n=20, eps=1.0):

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

    if pardim == 1:
        # function = lambda x: x * np.cos(1.0 * x)
        function = lambda x: (np.e**(-1.0 * x)) * np.cos(2.0 * np.pi * x)
        #function = lambda x: np.e**(+1.0*x)
        x_param = [0., 1.5, 32]
        n_init = 1
        data = np.zeros((n_init, 2))
        new_points = []
        # data[:, 0] = np.linspace(*x_param[:-1], n_init)
        data[:, 0] = np.random.rand(n_init)*(x_param[1] - x_param[0]) + x_param[0]
        for i in range(15):
            x_observ, y_observ, x_domain, y_pred, sigma = GPR_analysis_toy(data, y_par=[0.0, 1.5, 32], x_par=x_param, f=function, eps=0.0)
            x_n = get_new_sample(x_domain, sigma)

            y_test = function(x_domain) # TODO: some of the things e.g. x_domain are never chenged - should be returned all the time
            plot_prediction_variance(x_observ, y_observ, x_domain, y_test, y_pred, sigma, function, [x_n], new_points)
            #y_test = y_test.T.reshape(-1)

            stop_crit, err = stop_train_criterium_rmse(y_pred, y_test.T.reshape(-1), 0.01)
            errors.append(err)

            new_points = [x_n]
            data = np.concatenate((data, np.array([[x_n[0], 0.0]])), axis=0)
            #data = np.append(data, x_n.reshape(1,-1), axis=0)
            if stop_crit:
                print("Reached stopping criterium!")
                break

    elif pardim == 2:
        X = np.linspace(0.2, 2.8, 3)
        Y = np.linspace(0.2, 2.8, 3)
        #data = np.dstack((X, Y))[0]
        data = np.transpose([np.tile(X, len(Y)), np.repeat(Y, len(X))])
        for i in range(3):
            x, y, y_pred, sigma = GPR_analysis_2d(data, domain_par=[0.0, 3.0, 16, 0.0, 3.0, 16], func=exponential_model)
            x_n = get_new_sample(x, sigma)
            data = np.append(data, x_n.reshape(1, -1), axis=0)
            #print('new data {} and std {}'.format(x_n, std))
    
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

### FFNR model and test:
# FNN_Regression_toy()

### surrogate loop:
#surrogate_loop(2)
surrogate_loop(1)