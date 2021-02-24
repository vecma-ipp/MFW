import numpy as np
import chaospy as cp

import math as m
import time as t

from itertools import product

import bisect
import collections

from sklearn.gaussian_process import GaussianProcessRegressor as GPR
from sklearn.gaussian_process.kernels import DotProduct, Matern, RBF, WhiteKernel, ConstantKernel


from scipy.special import erf
from scipy.stats import rv_discrete
from scipy.integrate import quad

from matplotlib import pyplot as plt

def test_gaussian_integral(y, x_domain, sigma):
    """
    calculate pdf(y) = int_{x1}^{x2}_{p(y|x)p(x)dx} for a given y

    in case p(x) is uniform on [x1;x2] and p(y|x)~N(y,s)

    here p(y|x) depends only on |x-y|

    check: test for unity function

    :return:  1 / (x2-x1) [ erf((x2-y) / s*sqrt(2)) - erf((x1-y) / s*sqrt(2)) ]
    """

    sqrt2 = 1.41421356237
    return 1./(x_domain[1]-x_domain[0]) * (erf((x_domain[1]-y)/(sigma*sqrt2)) - erf((x_domain[0]-y)/(sigma*sqrt2)))

def walk_bins(y_samples, y_bin_edges): # TODO renormalize everything so bin edges are not passed! Replace by discrete rv!
    """
    Calculate distribution in y as a histogram, considering given samples and bins
    +1 in bin if y e [y_bin_min, y_bin_max)
    y < y_min -> discarded //assigned to first bin <- not discarding leads to high probability at edges if domain is too narrow
    y > y_max -> discarded //assigned to last bin
    :param y_samples: sorted array of y samples
    :param y_bin_edges: array of length y_bins+1 with edges of bins in Y
    :return: count of samples that were assigned to each bin
    // could be array of values e [0.; 1.] of length y_bins, each element is P(y)/Dy probability of y to be in the bin, by bin size
    """
    y_bins = len(y_bin_edges) - 1
    p_y_res = np.zeros(y_bins)

    y_bin_st = 0
    inc = 1
    for y in y_samples:  # no need to store all the x and y values - a lot of if's
        #inc = 1
        # for every y (bin) count x samples to later integrate over X
        if y < y_bin_edges[0]:
            continue
        if y > y_bin_edges[-1]:
            continue
        for y_bin in range(y_bin_st, y_bins):  # TODO replace with bisection?
            if y < y_bin_edges[y_bin + 1]:
                y_bin_st = y_bin
                break
        p_y_res[y_bin] += inc  # vectorized np operations for entire loop / a bin?

    return p_y_res

def weighted_sample(p_x, x_n, x_bin_edges):
    """
    TODO not tested, returns a dictionary
    Sample x of a discrete weighted random variable
    :param p_x: a piecewise-linear pdf of x
    :param x_n: number of x samples to return
    :param x_bin_edges:
    :return: x samples
    """

    weights = p_x*(x_bin_edges[1:] - x_bin_edges[:-1])
    population = 0.5*(x_bin_edges[1:] + x_bin_edges[:-1])

    tot_weight = weights.sum()
    cdf = weights.cumsum() / tot_weight

    counts = collections.defaultdict(int)
    for i in range(x_n):
        x = np.random.random()
        idx = bisect.bisect(cdf, x)
        counts[population[idx]] += 1

    return counts

def hist_sample(p_x, x_n, x_bin_edges):
    """
    Sample x from a histogram representation of pdf(x)
    :param p_x: a piecewise-linear pdf of x
    :param x_n: number of x samples to return
    :param x_bin_edges:
    :return: x samples from a piecewise-constant pdf
    """

    x_bins = len(x_bin_edges) - 1
    x_samples = []

    # TODO reuse walk_bins(), it is similar to the algorithm for cdf_bins
    # SHOULD BE: inverse-transform of the p-w pdf from a p_x
    cdf_bin_edges = np.zeros((x_bins + 1))
    cdf_bin_edges[1:] = np.cumsum(p_x*(x_bin_edges[1:] - x_bin_edges[:-1]))
    cdf_bin_edges[-1] = 1.

    x_unif = np.random.rand(x_n)
    x_unif = np.sort(x_unif)

    cdf_bin_st = 0
    for x_u in x_unif:
        # should not be needed. BUT a double check if a bug appears here for some reason
        if x_u < cdf_bin_edges[0]:
            continue
        if x_u > cdf_bin_edges[-1]:
            continue
        for cdf_bin in range(cdf_bin_st, x_bins):
            if x_u < cdf_bin_edges[cdf_bin + 1]:
                cdf_bin_st = cdf_bin
                break

        # add new uniform r.v. from a bin range

        # x_samples.append(x_bin_edges[x_bin] + (x_bin_edges[x_bin + 1] - x_bin_edges[
        #    x_bin]) * np.random.rand())  # make one rand call for end-sample?

        #x_samples.append(np.random.uniform(x_bin_edges[cdf_bin], x_bin_edges[cdf_bin + 1]))

        x_samples.append(np.random.uniform(0.5*(x_bin_edges[cdf_bin] + x_bin_edges[cdf_bin + 1])))

    return x_samples

def discrete_hist_sample(p_x, x_n, x_bin_edges):
    """

    :param p_x:
    :param x_n:
    :return:
    """
    # TODO did not compare with hist_sample() yet

    print('scipy pk sum: {}'.format(np.multiply(p_x, x_bin_edges[1:] - x_bin_edges[:-1]).sum()))

    dist = rv_discrete(name='dist', values=(0.5*(x_bin_edges[:-1] + x_bin_edges[1:]),
                                            np.multiply(p_x, (x_bin_edges[1:] - x_bin_edges[:-1]))))

    x_samples = dist.rvs(size=x_n)

    return x_samples

def compute_normalizing_coefficient(p_x, p_y_x, y_bin_edges, n_samples=50_000):
    """
    compute the determinant of bayesian rule i.e.  int_X_{p(y|x)p(x)dx}=pdf(y)=C(y) for given model
    simply calculates the number of resulting y-s in bins, and renormalizes count to fit p.w.c. pdf
    :param p_x: a chaospy.distribution object OR an array of values of piece-wise pdf for x e [x_i;x_i+1)
    :param p_y_x: model with predict() method providing function p(y|x) in form of mean and std
    :param y_bin_edges: an array of edges of 1D bins
    :param n_samples: number of samples to use in MC integration (for marginalization)
    :return: C(y) as p.w.c. pdf for marginalized y
    """

    # if isinstance(p_x, cp.distributions.baseclass.Dist):
    #    p_x = p_x.pdf(np.linspace(*x_domain))

    y_bins = len(y_bin_edges) - 1

    x_n = m.ceil(m.sqrt(n_samples))
    y_n_per_x = int(n_samples / x_n)

    p_y_res = np.zeros(y_bins)

    for x_i in range(x_n):
        x = p_x.sample()  # make two options: a chaospy object, or a binned array

        m_y_x, std_y_x = p_y_x.predict(x.reshape(-1, 1),
                                       return_std=True)   # consider passing different ML models or just values from data structure
        dist_y_x = cp.Normal(mu=m_y_x, sigma=std_y_x)

        # here we draw the same number of y samples for every x already drawn
        y_samples = dist_y_x.sample(y_n_per_x)
        y_samples = np.sort(y_samples)

        p_y_res = p_y_res + walk_bins(y_samples, y_bin_edges)

    # check if all y samples where assigned to bins
    # print("p(y) MC counts {} =?= {:1.1}".format(n_samples, p_y_res.sum()))

    # TODO check if this helps to mitigate cancelation effects
    #p_y_res[abs(p_y_res) < n_samples*1e-8] += 1
    p_y_res += 1

    # normalizing discrete distribution to unity
    p_y_res = p_y_res / p_y_res.sum()

    # finding values of piecewise-linear pdf
    p_y_res = np.divide(p_y_res, y_bin_edges[1:] - y_bin_edges[:-1])

    # plot_1d_distrib(y_bin_edges, p_y_res, names={'y': 'y', 'x': 'x~U'})   # does it looke "rugged" only beacuse MC slow convergency? -> try different bin number and samples number

    return p_y_res

def compute_x_y_posterior(p_x_prior, p_y_x, x_bin_edges, y_bin_edges, n_samples=10_000):  # non-unifrom signature, for different p_y_x
    """
    compute the posterior, the parameter distribution p(x|y)=p(y|x)p(x)/p(y), of the given model p(y|x)
    p(y) is integral over x int_{X}_{p(y|x)p(x)} ;  p(x) is prior (given)
    :return: p.w.c. pdf p(x|y) as a 2D array of values p for a grid of XxY inputs
    """

    x_bins = len(x_bin_edges) - 1
    y_bins = len(y_bin_edges) - 1

    x_n = m.ceil(m.sqrt(n_samples))
    y_n_per_x = int(n_samples / x_n)

    # p(x|y) is R^2 -> [0,1]
    p_x_y_res = np.zeros((x_bins, y_bins))

    # for x_i in range(x_n):  # TODO reuse compute_normalizing_coefficient and walk_bins
    #     x = p_x_prior.sample()
    #
    #     m_y_x, std_y_x = p_y_x.predict(x.reshape(-1, 1), return_std=True)
    #     dist_y_x = cp.Normal(mu=m_y_x, sigma=std_y_x)  # consider non-normal distributions
    #
    #     y_samples_loc = dist_y_x.sample(y_n_per_x)
    #     y_samples_loc = np.sort(y_samples_loc)
    #
    #     # iterate over all x_bins, y_samples, y_bins and count? how to do it simpler for e.g. gaussian*gaussian?
    #
    #     for x_bin in range(x_bins):
    #         if x_bin_edges[x_bin] <= x < x_bin_edges[x_bin + 1]:
    #             break
    #     p_x_y_res[x_bin, :] = p_x_y_res[x_bin, :] + walk_bins(y_samples_loc, y_bin_edges)


    #p(y|x), piecewise-linear pdf in an array porduced
    p_y_x_array = np.zeros((len(x_bin_edges) - 1, len(y_bin_edges) - 1))
    for x_i in range(len(x_bin_edges) - 1):
        x_val = (0.5*(x_bin_edges[1:] + x_bin_edges[:-1]))[x_i]
        y_mean, y_std = p_y_x.predict(x_val.reshape(-1, 1), return_std=True)
        y_pdf = cp.Normal(y_mean, y_std)
        p_y_x_array[x_i, :] = y_pdf.pdf(0.5*(y_bin_edges[1:] + y_bin_edges[:-1]))

    # p(y) this should be an integral over x
    c_y = compute_normalizing_coefficient(p_x_prior, p_y_x, y_bin_edges)

    # p(x), piecewise-linear pdf in an array produced
    p_x_prior_array = p_x_prior.pdf(0.5*(x_bin_edges[:-1] + x_bin_edges[1:]))

    #plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res, {'label': 'p(x|y) counts unnormalized'})

    for y_i in range(y_bins):  # TODO (important) formulate in matrix notation (how?)

        # # normalize counts to [0;1] interval for every y
        # # int_{X}_{p(x|y)dx}=1 for any y
        # p_x_y_res[:, y_i] = p_x_y_res[:, y_i] / p_x_y_res[:, y_i].sum()

        # # finding values of piecewise-linear pdf
        # p_x_y_res[:, y_i] = np.divide(p_x_y_res[:, y_i], x_bin_edges[1:] - x_bin_edges[:-1])


        # #print('for p(x|y~{:1.2}) check on prob sum: 1.0~={:1.3}'.format((
        # #                        y_bin_edges[y_i+1] + y_bin_edges[y_i])*0.5,
        # #                        np.multiply(p_x_y_res[:, y_i], x_bin_edges[1:] - x_bin_edges[:-1]).sum()))


        # works only if number of bins in y in c_y is the same as p_x_y -> use interpolation of p(y) if not
        # p(x|y)=p(y|x)p(x)/p(y)
        p_x_y_res[:, y_i] = np.multiply(p_y_x_array[:, y_i], p_x_prior_array[:])
        p_x_y_res[:, y_i] = p_x_y_res[:, y_i] / c_y[y_i]

    #plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res)  # passing not probabilities, but still counts # also some values at edges are nans

    return p_x_y_res, c_y

def compute_x_distribution(p_y, p_x_y, x_bin_edges, y_bin_edges, n_samples=4_000_000):
    """
    computes the results of the surrogate bayesian inversion p(x) = int_Y_{p(x|y)p(y)}
    TODO can it be same function as compute_normalizing_coefficient() ?

    TODO (conceptual)(important) conditional probabilities could be defined through a surrogate (?)
    TODO options --- choose one!
    1) train another GP for posterior
    2) MCMC
    3) particle filter
    4) importance sampling -> formulate R-N derivative based on the shape of p(x|y) (find new suitable probability space were it is close to uniform)

    #TODO hence already regions of high posterior density are observed, add maximum variane MC (with measure transformation) -> is it the same as importance sampling?
    #TODO for marginalization: the distribution might be narrow enough to take determenistic values for some cases

    the only difference from calculating normalizing coefficient c_y is signature, here p_x_y is not an ML model but an array of p.w.c. pdf val-s
    :return:
    """

    x_bins = len(x_bin_edges) - 1
    y_bins = len(y_bin_edges) - 1

    y_n = m.ceil(m.sqrt(n_samples))
    x_n_per_y = int(n_samples / y_n)

    p_x_res = np.zeros(x_bins)
    x_samples = np.zeros(x_n_per_y)

    #debug_discard_y_n = 0
    for y_i in range(y_n):

        y = p_y.sample()
        # define for which bin of y we sample x
        # debug check for y samples outside of domain
        if y < y_bin_edges[0]:
            continue
        if y >= y_bin_edges[-1]:
            continue
        for y_bin in range(y_bins):  # get a batch of y samples
            if y_bin_edges[y_bin] <= y < y_bin_edges[y_bin + 1]:
                break

        # draw samples using p_y_x histogram representation
        x_samples = hist_sample(p_x_y[:, y_bin], x_n_per_y, x_bin_edges)
        x_samples = np.sort(x_samples)

        # debug check for unusually high p(x)@x_max - > did not help
        #x_samples = x_samples[x_samples >= x_bin_edges[0]]
        #x_samples = x_samples[x_samples < x_bin_edges[-1]]

        p_x_res += walk_bins(x_samples, x_bin_edges)

    # print("p(x) MC counts: {} =?= {:1.1}".format(n_samples, p_x_res.sum()))
    # print("Number of y samples discarded due to lying outside of support is {}".format(debug_discard_y_n))

    # normalizing discrete distribution to unity
    p_x_res = p_x_res / p_x_res.sum()

    # finding values of piecewise-linear pdf
    p_x_res = np.divide(p_x_res, x_bin_edges[1:] - x_bin_edges[:-1])

    return p_x_res

def plot_model_function(x_train, y_train, x_test, y_pr_mean, y_pr_std,
                        x_int=None, y_int=None, x_prior=None, ax=None, fig=None):
    """
    here we consider only training and no testing
    :param x:
    :param y:
    :param y_pr_mean:
    :param y_pr_std:
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
                    names={'y': 'x', 'x': 'x~U({:1.2}, {:1.2})'.format(x_bin_edges.min(), x_bin_edges.max())})

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


def exponent(x, A=1.0, r=2.):
    return A * np.e ** (-np.dot(x, r))

# --- Training data
x_domain = [0.1, 2.5]
x_tr_n = 6
np.random.seed(1)

#X_train = x_domain[0] + np.random.rand(x_tr_n) * (x_domain[1] - x_domain[0])
X_train = np.linspace(x_domain[0], x_domain[1], x_tr_n)
X_train = X_train[np.argsort(X_train)]
X_train = X_train.reshape(-1, 1)

noize_level = 2.5e-2
y_train = np.array([exponent(x) + np.random.normal(0, noize_level, 1) for x in X_train])
#y_train = np.array([exponent(x) for x in X_train])

y_domain = [exponent(x_domain[0]) + .15, exponent(x_domain[1]) - .15]

if y_domain[1] < y_domain[0]:
    y_domain = [y_domain[1], y_domain[0]]
#if y_domain[0] < 0.:
#    y_domain[0] = 0.

# --- Testing data
x_domain = [x_domain[0] - 0.15, x_domain[1]]
y_domain = [y_domain[0]*0.8, y_domain[1]*1.1]
x_ts_n = 20
X_test = np.sort(x_domain[0] + np.random.rand(x_ts_n) * (x_domain[1] - x_domain[0])).reshape(-1, 1)

# --- Prepare a model
kernel = ConstantKernel()*Matern() + WhiteKernel()
surrogate = GPR(kernel=kernel).fit(X_train, y_train)

# --- Model training results
y_mean, y_std = surrogate.predict(X_train, return_std=True)  # start at (0.5, 0.65)

y_mean_test, y_mean_std = surrogate.predict(X_test, return_std=True)

# --- Prior believe on x distribution
p_x_prior = cp.Uniform(x_domain[0], x_domain[1])
#p_x_prior = cp.Normal(0.5*(x_domain[0] + x_domain[1]), 0.25*(x_domain[1] - x_domain[0]))

# --- Distribution of y for given x-prior
#p_y = compute_normalizing_coefficient(x_prior, surrogate, x_domain=x_domain, y_domain=y_domain, x_n=5000, y_n=50000, y_bins=50)

# --- Tests for analytical results for simple p(x) and p(y|x)
#p_y_test = np.array([test_gaussian_integral(y, x_domain, s) for [y,s] in zip(y_mean_test, y_mean_std)])
#plot_1d_distrib(np.linspace(*y_domain, 21), p_y_test, names={'y': 'gauassian integral', 'x': 'x'})

# ---- Compute posterior
x_bins = 25
y_bins = 25

x_bin_edges = np.linspace(*x_domain, x_bins + 1)
y_bin_edges = np.linspace(*y_domain, y_bins + 1)

start_post_calc = t.time()
p_x_y, c_y = compute_x_y_posterior(p_x_prior, surrogate, x_bin_edges, y_bin_edges)
end_post_calc = t.time()
print('posterior calculation took {:2.3} s'.format(end_post_calc - start_post_calc))

#debugging
#print(np.where(abs(c_y) < 1e-9))
#print(np.where(p_x_y < 1e-9))
#highpos_pxy = np.where(p_x_y > 1e2)
#print([(x,y) for x,y in zip(highpos_pxy[0], highpos_pxy[1])])

# --- Assume desirable y distribution
# TODO test range of parameters that influence the marginal x variance: training data noize, y prior variance, ...
# TODO position of y mean (resulting variance will be higher for more gentle slope of model function

x_trues = [0.0, 0.5, 1.0, 2.5]
y_trues = [exponent(x_true) for x_true in x_trues]
#print("x true : {:.3} \ny true : {:.3}".format(x_true, y_true))

y_p_sigmas = [0.005, 0.025, 0.1]
for (x_true, y_p_sigma) in product(x_trues, y_p_sigmas):
    y_true = exponent(x_true)
    p_y = cp.Normal(y_true, y_p_sigma)
    start_marg_calc = t.time()
    p_x = compute_x_distribution(p_y, p_x_y, x_bin_edges, y_bin_edges, n_samples=1_000_000) #TODO (important) p(x) accumulates suspicious samples in the end of x support
    end_marg_calc = t.time()
    print('marginal x distribution calculation took {:2.3} s'.format(end_marg_calc - start_marg_calc))
    combined_plot(x_bin_edges, y_bin_edges, p_x_y, surrogate, p_x_prior, p_x, c_y, p_y,
                  X_train=X_train,
                  y_train=y_train,
                  x_true=x_true,
                  y_true=y_true,
                  y_s=y_p_sigma)
