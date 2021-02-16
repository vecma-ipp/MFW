import numpy as np
import chaospy as cp

from sklearn.gaussian_process import GaussianProcessRegressor as GPR
from sklearn.gaussian_process.kernels import DotProduct, WhiteKernel, Matern, RBF, WhiteKernel, ConstantKernel

from da_utils import *

from matplotlib import pyplot as plt

def walk_y_bins(p_y_res, y_samples, y_bin_edges, y_bins):
    y_bin_st = 0
    for y in y_samples:  # no need to store all the x and y values - but a lot of if's
        # for every y (bin) calculate integral over all X values
        y_bin = y_bin_st - 1
        for y_i in range(y_bin_st, y_bins):
            y_bin = y_bin + 1
            if y < y_bin_edges[y_bin + 1]:
                y_bin_st = y_bin
                break
        p_y_res[y_bin] = p_y_res[y_bin] + 1  # vectorized np operations?
    return p_y_res

def compute_normalizing_coefficient(p_x, p_y_x, x_domain, y_domain, x_n=1000, y_n=5000, y_bins=20):
    """
    compute the determinant of bayesian rule i.e.  int_X_{p(y|x)p(x)dx}=pdf(y)=C(y) for given model
    simply calculates the number of resulting y-s in bins
    :param p_x: an array of values of p(x e [x_i;x_i+1))
    :param p_y_x: model providing p(y|x) in form of mean and std
    """

    #if isinstance(p_x, cp.distributions.baseclass.Dist):
    #    p_x = p_x.pdf(np.linspace(*x_domain))

    p_y_res = np.zeros((y_bins))
    y_bin_edges = np.linspace(*y_domain, y_bins+1)

    y_n_per_x = int(y_n / x_n)

    for x_i in range(x_n):

        x = p_x.sample()

        m_y_x, std_y_x = p_y_x.predict(x.reshape(-1, 1), return_std=True)  # consider passing different ML models or just values from data structure
        dist_y_x = cp.Normal(mu=m_y_x, sigma=std_y_x)  # consider non-normal distributions

        # here we draw number of y-s for every x already drawn // proportional to probability of x
        y_samples = dist_y_x.sample(y_n_per_x)
        y_samples = np.sort(y_samples)

        p_y_res = walk_y_bins(p_y_res, y_samples, y_bin_edges, y_bins)

    #print("{} =?= {:1.1}".format(y_n, p_y_res.sum()))  # check if all y samples where assigned to bins

    p_y_res = p_y_res / float(y_n)

    plot_1d_distib(y_bin_edges, p_y_res)  # why does it look ragged? + try different bin number and samples number

    return p_y_res

def computex_x_posterior(p_prior_x, p_y_x, x_domain, y_domain, x_n=1000, y_per_x=20, x_bins=10, y_bins=10):  # non-unifromal signature
    """
    compute the posterior, the parameter distribution p(x|y)=p(y|x)p(x)/p(y), of the given model p(y|x)
    p(y) is integral, all p(x) is prior (given)
    :return: an array of values p for a grid of y values
    """

    ys = np.zeros((x_n*y_per_x))

    # p(x|y) is R^2 -> [0,1]
    p_x_y_res = np.zeros((x_bins, y_bins))
    x_bin_edges = np.linspace(*x_domain, x_bins+1)
    y_bin_edges = np.linspace(*y_domain, y_bins+1)

    for x_i in range(x_n):
        x = p_prior_x.sample()  # sample batch and then sort

        m_y_x, std_y_x = p_y_x.predict(x.reshape(-1, 1), return_std=True)  # use batch-prediction
        dist_y_x = cp.Normal(mu=m_y_x, sigma=std_y_x)  # consider non-normal distributions

        y_samples_loc = dist_y_x.sample(y_per_x)
        y_samples_loc = np.sort(y_samples_loc)

        # iterate over all x_bins, y_samples, y_bins and count? how to do it simpler for e.g. gaussian*gaussian?

        for x_bin in range(x_bins):
            if x >= x_bin_edges[x_bin] and x < x_bin_edges[x_bin + 1]:
                p_x_y_res[x_bin, :] = walk_y_bins(p_x_y_res[x_bin, :], y_samples_loc, y_bin_edges, y_bins)
                break

    # this should be an integral over x
    c_y = compute_normalizing_coefficient(p_prior_x, p_y_x, x_domain, y_domain, y_bins=y_bins)
    
    for x_i in range(x_bins):
        p_x_y_res[x_i, :] = p_x_y_res[x_i, :] / p_x_y_res[x_i, :].sum()
        # works only if number of bins in y is the same -> use interpolation of p(y)
        p_x_y_res[x_i, :] = np.divide(p_x_y_res[x_i, :], c_y) # TODO check if c_y is so small that pushes p_x_y to ~1e2

    plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res)

    return p_x_y_res

def compute_x_distribution(p_x_y, p_y, y_n = 1000, x_n=100, x_bins=20):
    """
    computes the results of the surrogate bayesian inversion p(x) = int_Y_{p(x|y)p(y)}
    :return:
    """
    #TODO implement
    return 0


def plot_model_function(x, y, y_pr_mean, y_pr_std, x_prior=None):
    # here we consider only training and no testing
    plt.plot(x, y, 'r-', label='true function')
    plt.plot(x, y_pr_mean, 'b-', label='prediction mean')
    plt.fill(np.concatenate([x, x[::-1]]),
             np.concatenate([y_pr_mean - 1.9600 * y_pr_std.reshape(-1, 1),
                             (y_pr_mean + 1.9600 * y_pr_std.reshape(-1, 1))[::-1]]),
             alpha=.3, fc='b', label='95% confidence interval')

    plt.xlabel('x')
    plt.ylabel('y')
    plt.title('Model function and GPR fitting results')

    if x_prior is not None:
        plt.plot(x, x_prior.pdf(x), label='x prior')

    plt.legend(loc='best')
    plt.show()

def plot_1d_distib(y_bin_edges, p_y):

    fig, ax = plt.subplots()
    im = ax.plot(0.5*(y_bin_edges[:-1] + y_bin_edges[1:]), p_y, 'bo-')

    ax.set_title('Probability for y. Given prior of x. Check on sum: 1.0~={:1.3}'.format(p_y.sum()))
    ax.set_xlabel('y')
    ax.set_ylabel('p(y)')
    plt.show()

def plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y):

    fig, ax = plt.subplots(figsize=(10, 6))
    im = ax.imshow(p_x_y)

    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_title('p(x|y)')

    ax.set_xticklabels(np.around(0.5*(x_bin_edges[:-1]+x_bin_edges[1:]), decimals=2))
    ax.set_yticklabels(np.around(0.5*(y_bin_edges[:-1]+y_bin_edges[1:]), decimals=2))

    ax.set_xticks(np.arange(len(x_bin_edges)) - 1.5)
    ax.set_yticks(np.arange(len(y_bin_edges)) - 1.5)

    fig.colorbar(im)

#    for i in np.arange(np.shape(p_x_y)[0]):
#        for j in np.arange(np.shape(p_x_y)[1]):
#            text = ax.text(j, i, int(p_x_y[i, j]), ha="center", va="center", color="w")
    ax.set_title("Probability for p(x|y) in bins")
    plt.show()

def exponent(x, A=1.0, r=1.0):
    return A*np.e**(-np.dot(x, r))

# training data
x_domain = [0.1, 2.5]
x_tr_n = 6
X_train = x_domain[0] + np.random.rand(x_tr_n) * (x_domain[1] - x_domain[0])
X_train = X_train[np.argsort(X_train)]
X_train = X_train.reshape(-1, 1)

y_train = np.array([exponent(x) + np.random.normal(0, 1e-2, 1) for x in X_train])
y_domain = [exponent(x_domain[0])-1e-1, exponent(x_domain[1])+1e-1]
if y_domain[1] < y_domain[0]:
    y_domain = [y_domain[1], y_domain[0]]

# prepare a model
kernel = ConstantKernel() + RBF() + WhiteKernel()
surrogate = GPR(kernel=kernel).fit(X_train, y_train)

# training results
y_mean, y_std = surrogate.predict(X_train, return_std=True)

# prior believe on x distribution
x_prior = cp.Normal(1.2, 0.4)

plot_model_function(X_train, y_train, y_mean, y_std, x_prior=x_prior)

# distribution of y for given x-prior

#p_y = compute_normalizing_coefficient(x_prior, surrogate, x_domain=x_domain, y_domain=y_domain, x_n=2000, y_n=10000, y_bins=10)

p_x_y = compute_posterior(x_prior, surrogate, x_domain, y_domain, x_n=8000, y_per_x=25, x_bins=20, y_bins=20)
