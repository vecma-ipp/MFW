import numpy as np
import chaospy as cp

import time as t

from sklearn.gaussian_process import GaussianProcessRegressor as GPR
from sklearn.gaussian_process.kernels import DotProduct, Matern, RBF, WhiteKernel, ConstantKernel

from matplotlib import pyplot as plt

def exp_deterministic_mean(x_domain, a=1., r=2.):
    return a * (np.exp(-r * x_domain[0]) - np.exp(-r * x_domain[1])) / (r * (x_domain[1] - x_domain[0]))

def exp_deterministic_var(x_domain, a=1., r=2.):
    exp_mean = exp_deterministic_mean(x_domain, a, r)
    return a*a*(np.exp(-2*r*x_domain[0])-np.exp(-2*r*x_domain[1]))/(2*r*(x_domain[1]-x_domain[0]))-exp_mean*exp_mean

def plot_model_function(x_train, y_train, x_test, y_pr_mean, y_pr_std,
                        x_int=None, y_int=None, x_prior=None, ax=None, fig=None):
    """
    Plots a graph of model y(x) for surrogate points, training and testing values of x,y
    and surrogate prediction mean with 95% confidence interval.
    :param x_train: x parameter values with which the model was trained
    :param y: y=f(x)+eps target values with which the model was trained
    :param y_pr_mean:
    :param y_pr_std:
    :param x_int: true x paramter value
    :param y_int: true y target value
    :param x_prior:
    :param ax:
    :param fig:
    :return:
    """

    if ax == None and fig == None:
        fig, ax = plt.subplots(figsize=(10, 6))

    ax.plot(x_train, y_train, 'ro-', label='training  func. values')
    ax.plot(x_test, y_pr_mean, 'b-', label='prediction mean')
    ax.fill_between(x_test, y_pr_mean - 1.9600 * y_pr_std,
                            y_pr_mean + 1.9600 * y_pr_std,
                    alpha=.3, fc='b', label='95% conf. int.')

    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_title('Model func. and GPR fitting res-s')

    if x_prior is not None:
        ax.plot(x_train, x_prior.pdf(x_train), label='x prior')

    if x_int is not None and y_int is not None:
        ax.plot(x_int, y_int, 'ko', markersize=8.5, label='true point')

    ax.grid(True, which='both')
    ax.legend(loc='best')
    #plt.show()

def plot_1d_distrib(y_bin_edges, p_y, names={'y': 'y', 'x': 'x'},
                    x_true=None, y_true=None, ax=None, fig=None):

    if ax == None and fig == None:
        fig, ax = plt.subplots(figsize=(10, 6))

    im = ax.step(0.5 * (y_bin_edges[:-1] + y_bin_edges[1:]), p_y, 'o-', where='mid')

    # area_under_curve = np.trapz(np.multiply(p_y, y_bin_edges[1:] - y_bin_edges[:-1]))
    # MC did not calculate integrals considering trapezoidal rule
    # could re-weight extremal points of domain and result will be smaller
    area_under_curve = np.sum(np.multiply(p_y, y_bin_edges[1:] - y_bin_edges[:-1]))


    ax.set_title('Prob. for {}; prior {}'.
                 #'; tot. prob.: 1.0~={:1.3}'.
                 format(names['y'], names['x'],
                 #       area_under_curve
                        ))

    if x_true is not None and y_true is not None:
        ax.plot(x_true, y_true, 'ko', markersize=8.5)

    ax.set_xlabel('{}'.format(names['y']))
    ax.set_ylabel('p({})'.format(names['y']))

    ax.tick_params(which='both')
    ax.grid(True, which='both')

def plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y, names={'x':'x', 'y':'y', 'label': 'p(x|y)'}, ax=None, fig=None):

    if ax == None and fig == None:
        fig, ax = plt.subplots(figsize=(10, 6))

    im = ax.imshow(p_x_y[:, ::-1].T)  # y should be displayed backwards

    ax.set_xlabel(names['x'])
    ax.set_ylabel(names['y'])
    ax.set_title(names['label'])

    y_bin_edges_back = y_bin_edges[::-1]

    # for pyplot heatmap y is on top, put tick backwards
    #ax.set_xticklabels(np.around(0.5 * (x_bin_edges[:-1] + x_bin_edges[1:]), decimals=2))
    #ax.set_yticklabels(np.around(0.5 * (y_bin_edges_back[:-1] + y_bin_edges_back[1:]), decimals=2))

    ax.set_xticklabels(np.around(x_bin_edges, decimals=2), Rotation=90)
    ax.set_yticklabels(np.around(y_bin_edges_back, decimals=2))

    ax.tick_params(axis='both', labelsize=6)

    ax.set_xticks(np.arange(len(x_bin_edges) - 1.))
    ax.set_yticks(np.arange(len(y_bin_edges) - 1.))

    fig.colorbar(im, ax=ax)

    ax.set_title("Prob. for {}".format(names['label']))
    return ax

def combined_plot(x_bin_edges, y_bin_edges, p_x_y, p_y_x, p_x_prior, p_x, c_y, p_y,
                  X_train, y_train, x_true, y_true, y_s):

    fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(16, 9))
    #fig.tight_layout()
    fig.subplots_adjust(wspace=0.32, hspace=0.4)

    # the prior p(x) density
    p_x_prior_array = p_x_prior.pdf(0.5*(x_bin_edges[1:] + x_bin_edges[:-1]))
    plot_1d_distrib(x_bin_edges, p_x_prior_array, ax=axs[0][1], fig=fig,
                    names={'y': 'x', 'x': 'x~U({x_bin_edges.min():1.2}, {x_bin_edges.max():1.2})'})

    # the conditional likelihood p(x|y) density
    plot_2d_distrib(x_bin_edges, y_bin_edges, np.log2(p_x_y),  # TODO mask probabilities equal 0.0
                    names={'x': 'x', 'y': 'y', 'label': 'log2 p(x|y)'},
                    ax=axs[1][1], fig=fig)

    # the marginal p(y) density, or normalisation
    plot_1d_distrib(y_bin_edges, c_y, ax=axs[1][0], fig=fig,
                    names={'y': 'y', 'x': 'x~U({:1.2}, {:1.2})'.format(x_bin_edges.min(), x_bin_edges.max())})
    #axs[1][0].view_init(30, 90)

    # the prior p(y) density
    p_y_prior_array = p_y.pdf(0.5*(y_bin_edges[1:] + y_bin_edges[:-1]))
    plot_1d_distrib(y_bin_edges, p_y_prior_array, ax=axs[1][2], fig=fig,
                    names={'y': 'y~N({:1.2}, {:1.2})'.format(y_true, y_s), 'x': 'x'})

    # final marginal p(x) density for parameter value
    plot_1d_distrib(x_bin_edges, p_x, ax=axs[2][1], fig=fig, x_true=x_true, y_true=y_true,
                    names={'y': 'x', 'x': 'y~N({:1.2}, {:1.2})'.format(y_true, y_s)})

    # the surrogate likelihood p(y|x)
    p_y_x_array = np.zeros((len(x_bin_edges) - 1, len(y_bin_edges) - 1))
    x_tests = []
    y_pr_means = []
    y_pr_stds = []
    for x_i in range(len(x_bin_edges) - 1):
        x_val = (0.5*(x_bin_edges[1:] + x_bin_edges[:-1]))[x_i]
        y_mean, y_std = p_y_x.predict(x_val.reshape(-1, 1), return_std=True)

        x_tests.append(x_val)
        y_pr_means.append(y_mean)
        y_pr_stds.append(y_std)

        y_pdf = cp.Normal(y_mean, y_std)
        p_y_x_array[x_i, :] = y_pdf.pdf(0.5*(y_bin_edges[1:] + y_bin_edges[:-1]))

    plot_2d_distrib(x_bin_edges, y_bin_edges, p_y_x_array,
                    names={'x': 'x', 'y': 'y', 'label': 'p(y|x)'},
                    ax=axs[0][0], fig=fig)

    # plot the graph of training data
    plot_model_function(X_train, y_train, np.array(x_tests).reshape(-1),
                                          np.array(y_pr_means).reshape(-1),
                                          np.array(y_pr_stds).reshape(-1),
                                          x_int=x_true, y_int=y_true,
                                          ax=axs[2][2], fig=fig)

    fig.savefig('BI_MC_plots\ibp_mc_gp_n0025_r_2_y_m_{:1.3}_y_s_{:1.3}.png'.format(y_true, y_s))
