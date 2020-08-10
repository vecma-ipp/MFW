import pandas as pd
import numpy as np
import matplotlib.pylab as plt

import datetime

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF, WhiteKernel, ConstantKernel as C

from sklearn.neural_network import MLPRegressor

plt.ion()
np.random.seed(2)  # check other random seeds - huge difference! (42, 100 are bad)


def plot_prediction_variance(X, y,  x, y_pred, dy, sigma, f):
    # Plot function,prediction and 95% confidence interval
    plt.figure()
    plt.plot(x, f(x), 'r:', label=r'$f(x) = x\,\sin(x)$')
    plt.errorbar(X.ravel(), y, dy, fmt='r.', markersize=10, label='Observations')
    plt.plot(x, y_pred, 'b-', label='Prediction')
    plt.fill(np.concatenate([x, x[::-1]]),
             np.concatenate([y_pred - 1.9600 * sigma,
                             (y_pred + 1.9600 * sigma)[::-1]]),
             alpha=.5, fc='b', ec='None', label='95% confidence interval')
    plt.xlabel('$x$')
    plt.ylabel('$f(x)$')
    plt.ylim(-10, 20)
    plt.legend(loc='upper left')
    plt.show(block=True)


def GPR_analysis_toy(y_par=[0.1, 9,9, 20], x_par=[0, 10, 100], f=lambda x: x*np.sin(x), eps=1.0):

    # function to fit
    #if f==0:
    #    f = lambda x:  x * np.sin(x)

    # case: with noise
    X = np.atleast_2d(np.linspace(y_par[0], y_par[1], y_par[2])).T
    y = f(X).ravel()

    # add noise
    dy = 0.5 + eps * np.random.random(y.shape)
    noise = np.random.normal(0, dy)
    y += noise

    # input space mesh, the prediction and
    x = np.atleast_2d(np.linspace(x_par[0], x_par[1], x_par[2])).T

    # GP model
    kernel = C(1.0, (1e-3, 1e3)) * RBF(10, (1e-2, 1e2))  # initial kernel is N(1.0, 0.01)
    gp = GaussianProcessRegressor(kernel=kernel, alpha=dy**2, n_restarts_optimizer=9)

    # fit - exact algo?
    start_ts = datetime.datetime.now()
    gp.fit(X, y)
    print("time to train GP model: " + str((datetime.datetime.now() - start_ts).total_seconds()) + " seconds")

    # predictions + MSE
    y_pred, sigma = gp.predict(x, return_std=True)

    # plot predictions with variance
    plot_prediction_variance(X, y, x, y_pred, dy, sigma, f)


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


def AUG_GM_date_explore(filename='AUG_gem_inoutput.txt'):
    """
    :param filename: a csv file with the sim data
    :return:
    """

    """Read GEM data """
    AUG_gem = pd.read_table(filename, delimiter='  *')
    pd.options.display.max_columns = AUG_gem.shape[1]

    """ Data exploration """
    desc = AUG_gem.describe(include='all')
    #print(desc)

    #splot = AUG_gem.plot('dTe-ft1', ['flux-Ti-ft1'], 'scatter', logy=True)
    #splot.figure.savefig('scatterplot1.pdf')
    #plt.show(block=True)

    flti1_plot_time = AUG_gem.plot('time', ['flux-Ti-ft3'])
    flti1_plot_time.figure.savefig('fluxTi3_wtime.pdf')
    plt.show(block=True)

    y1 = AUG_gem['dTe-ft1']
    print('[' + str(y1.min()) + ';' + str(y1.max()) + ']')
    y1 = pd.to_numeric(y1, downcast='float')
    y1.fillna(value=pd.np.nan, inplace=True)

    #GP_analysis_toy(X=AUG_gem['time'], y=y1)


### GPR model and analysis
#GPR_analysis_toy(y_par=[0.1, 9.9, 50], x_par=[0, 13, 1300], f=lambda x: x*np.cos(0.25*x), eps=3.0)

AUG_GM_date_explore(filename='../data/AUG_gem_inoutput.txt')


### FFNR model and test:
# FNN_Regression_toy()