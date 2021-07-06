import pytest
import numpy as np
import time as t
import math as m
import cmath as cm

from itertools import product

from sklearn.gaussian_process import GaussianProcessRegressor as GPR
from sklearn.gaussian_process.kernels import RBF, ConstantKernel, Matern, WhiteKernel

import chaospy as cp

from scipy.special import erf, erfi

from uq.basicda.Bayes import Bayes

from matplotlib import pyplot as plt
from uq.basicda.model_inverse_simple import combined_plot, plot_model_function

import lhsmdu

"""
TODO: make tests for
 - marginalization: pdf, E, Var +
 - surrogate convergence: residuals between real function and mean of surrogate prediction
 - surrogate statistics calculation: E, Var, Sobol indices
 """

def exponent(x, r=1., a=1., eps=0.):
    # function is currently unvectorized
    return a * np.exp(-r*x)  #+ np.random.normal(0, eps, 1)

def damped_oscilator(x, om=0.5, g=0.05, a=1.):
    return a * np.exp(-g*np.square(x)) * np.cos(om * x)

def exp_mean(x_domain, r=1., a=1.):
    # here we consider x as a uniform random variable at x_domain
    # If x is considered a stochastic parameter, then we calculate statistic for the model at point 'r'
    return a * (np.exp(-r * x_domain[0]) - np.exp(-r * x_domain[1])) / (r * (x_domain[1] - x_domain[0]))

def exp_var(x_domain, r=1., a=1.):
    exp_mean_val = exp_mean(x_domain, r, a)
    return a*a*(np.exp(-2*r*x_domain[0])-np.exp(-2*r*x_domain[1]))/(2*r*(x_domain[1]-x_domain[0]))-exp_mean_val*exp_mean_val

def exp_norm_mean(x_domain, r=1., a=1., mu=0.5, sigma=0.1):
    mu = 0.5*(x_domain[0] + x_domain[-1])
    sigma = (x_domain[-1] - x_domain[0]) / 6.
    return (-0.5 * a * np.exp(0.5*r*(r*sigma*sigma - 2*mu))) * \
           (erf((x_domain[0] - mu + r*sigma*sigma)/(np.sqrt(2)*sigma)) -
            erf((x_domain[1] - mu + r*sigma*sigma)/(np.sqrt(2)*sigma)))

def exp_norm_var(x_domain, r=1., a=1., mu=0.5, sigma=0.1):
    mu = 0.5*(x_domain[0] + x_domain[-1])
    sigma = (x_domain[-1] - x_domain[0]) / 6.
    exp_norm_mean_val = exp_norm_mean(x_domain, r, a, mu, sigma)
    return (-0.5 * a * a * np.exp(2.*r*(r*sigma*sigma - mu))) * \
           (erf((x_domain[0] - mu + 2.*r*sigma*sigma)/(np.sqrt(2)*sigma)) -
            erf((x_domain[1] - mu + 2.*r*sigma*sigma)/(np.sqrt(2)*sigma))) - \
            exp_norm_mean_val * exp_norm_mean_val

def damped_oscilator_mean(x_domain, om=0.5, g=0.05, a=1.):
    return (-a * 1j * cm.sqrt(cm.pi) * cm.exp(-om * om/(4 * g))) * \
           (erfi((om - 2 * 1j * g * x_domain[0]) / (2 * cm.sqrt(g))) - erfi((om + 2 * 1j * g * x_domain[0]) / (2 * cm.sqrt(g))) - \
            erfi((om - 2 * 1j * g * x_domain[1]) / (2 * cm.sqrt(g))) + erfi((om + 2 * 1j * g * x_domain[1]) / (2 * cm.sqrt(g)))) / \
           (4 * (x_domain[1] - x_domain[0]) * cm.sqrt(g))

def damped_oscilator_var(x_domain, om=0.5, g=0.05, a=1.):
    return a * cm.sqrt(cm.pi) * cm.exp(-om*om/(2*g)) *\
           (2*cm.exp(om*om/(2*g)) * (erf(x_domain[1]*cm.sqrt(2*g)) - erf(x_domain[0]*cm.sqrt(2*g))) -
            erf((2*g*x_domain[0] + 1j*om)/cm.sqrt(2*g)) + 1j*erfi((om+2*1j*g*x_domain[0])/cm.sqrt(2*g)) +
            erf((2*g*x_domain[1] - 1j*om)/cm.sqrt(2*g)) + erf((2*g*x_domain[1] + 1j*om)/cm.sqrt(2*g))) / \
           (8 * cm.sqrt(2*g) * (x_domain[1] - x_domain[0])) - damped_oscilator_mean(x_domain, om, g, a)**2

def damped_oscilator_normal_mean(x_domain=[-8., 12.], om=0.5, g=0.05, a=1., mu=2., sigma=20/6.):
    mu = 0.5*(x_domain[0] + x_domain[-1])
    sigma = (x_domain[-1] - x_domain[0]) / 6.

    return 0.288561  # for default values

def damped_oscilator_normal_var(x_domain=[-8., 12.], om=0.5, g=0.05, a=1., mu=2., sigma=20/6.):
    mu = 0.5*(x_domain[0] + x_domain[-1])
    sigma = (x_domain[-1] - x_domain[0]) / 6.
    damped_oscilator_mean_val = damped_oscilator_mean(x_domain, om, g, mu, sigma)

    return 0.281715   # fro defaul values

def harmonic_product_mean(x_domain, y_domain, a=1., b=2.7):
    """
    Mean of f(a,b|x,y) = sin(ax)cos(by) for x~U[x0, x1] y~U[y0,y1]
    :param x_domain: list of interval edges where x is defined, x~Uniform(x_domain[0], x_domain[1])
    :param y_domain: list of interval edges where y is defined, y~Uniform(y_domain[0], y_domain[1])
    :param a: frequency in x
    :param b: frequency in y
    :return: E_x,y_(f(a,b)) = int_{x0-x1}_{y0-y1}_{sin(ax)cos(by)dp(y)dp(x)}
    """

    return (np.cos(a * x_domain[0]) - np.cos(a * x_domain[1])) * \
           (np.sin(b * y_domain[0]) - np.sin(b * y_domain[1])) / \
           (a * b * (x_domain[1] - x_domain[0]) * (y_domain[1] - y_domain[0]))

def harmonic_product_var(x_domain, y_domain, a=1., b=2.7):
    """
    Mean of sin(ax)cos(by) for x~U[x0, x1] y~U[y0,y1]
    :param x_domain: list of interval edges where x is defined, x~Uniform(x_domain[0], x_domain[1])
    :param y_domain: list of interval edges where y is defined, y~Uniform(y_domain[0], y_domain[1])
    :param a: frequency in x
    :param b: frequency in y
    :return: Var_x,y_(f(a,b)) = int_{x0-x1}_{y0-y1}_{sin^2(ax)cos^2(by)dp(y)dp(x)} - E(f)^2
    """

    return (2 * a * (x_domain[0] - x_domain[1]) - np.sin(2 * a * x_domain[0]) + np.sin(2 * a * x_domain[1])) * \
           (2 * b * (y_domain[0] - y_domain[1]) + np.sin(2 * b * y_domain[0]) - np.sin(2 * b * y_domain[1])) / \
           (16 * a * b * (x_domain[1] - x_domain[0]) * (y_domain[1] - y_domain[0]))

def func_OK2002_1D(x):
    return 5 + x + np.cos(x)

def g_function(u, a=[0.0, 0.5, 3.0, 9.0, 99.0], d=1):
    g = 1.
    if d == 1:
        g = (np.abs(4. * u - 2.) + a[0]) / (1. + a[0])
    else:
        for j in range(d):
            g *= (np.abs(4. * u[j] - 2.) + a[j]) / (1. + a[j])
    return g

def g_function_sobol_idices(a=[0.0, 0.5, 3.0, 9.0, 99.0], d=1):
    V_i = np.zeros(d)
    for i in range(d):
        V_i[i] = 1. / (3. * (1. + a[i])**2)
    V = np.prod(1. + V_i) - 1.
    return V_i / V

def ishigami_function(x, a=7., b=0.1):
    return np.sin(x[0]) + a * np.sin(x[1]) * np.sin(x[1]) + b * x[2] * x[2] * x[2] * x[2] * np.sin(x[0])

def ishigami_first_sobols(a=7., b=0.1):
    V1 = 0.5 * (1. + b * np.power(np.pi, 4)/5.)**2
    V2 = a*a / 8.
    V3 = 0.
    V = a*a / 8. + b*np.power(np.pi, 4) / 5. + b*b*np.power(np.pi, 8.) / 18. + 0.5
    return np.array([V1/float(V), V2/float(V), V3/float(V)])

def ishigami_total_sobols(a=7., b=0.1):
    V1 = b*np.power(np.pi, 4)/5. + b*b*np.power(np.pi, 8.)/18. + 0.5
    V2 = a*a/8.
    V3 = 8.*b*b*np.power(np.pi, 8.)/225.
    V = a*a / 8. + b*np.power(np.pi, 4) / 5. + b*b*np.power(np.pi, 8.) / 18. + 0.5
    return np.array([V1/float(V), V2/float(V), V3/float(V)])

def ishigami_second_sobols(a=7., b=0.1):
    S13 = 8.*b*b*np.power(np.pi, 8.)/225.
    V = a*a / 8. + b*np.power(np.pi, 4) / 5. + b*b*np.power(np.pi, 8.) / 18. + 0.5
    return np.array([[0.,0.,S13], [0.,0.,0,], [S13,0.,0.]])

def p_y_identity_analytical(y, x_domain, sigma):
    """
    calculate pdf(y) = int_{x_domain}_{p(y|x)p(x)dx} for a given y

    test for unity function y=y(x)=x

    in case p(x) is uniform on x_domain=[x1;x2] and p(y|x)~N(x,sigma)

    here p(y|x) depends only on |x-y|

    :return:  1 / (x2-x1) [ erf((x2-y) / s*sqrt(2)) - erf((x1-y) / s*sqrt(2)) ]
    """

    sqrt2 = 1.41421356237
    return (erf((x_domain[1] - y) / (sigma * sqrt2)) - erf((x_domain[0] - y) / (sigma * sqrt2))) / (x_domain[1] - x_domain[0])


#==============================================================
#======================== TESTS ===============================
#==============================================================

@pytest.fixture(params=np.logspace(-1, 1, 3).tolist())
def fixture_param_range(request):
    return request.param

@pytest.mark.skip()
def test_compute_normalizing_coefficient():

    bayes = Bayes()

    x_domain = [0, 1]
    x = np.linspace(x_domain[0], x_domain[1], 10)
    dx = x[1] - x[0]

    y = x

    y_edges = np.linspace(-0.05, 1.05, 11)

    p_y_x = GPR(kernel=RBF()).fit(x.reshape(-1, 1), y)

    p_x = cp.Uniform(0, 1)

    p_y = bayes.compute_p_y(p_x, p_y_x, y_edges, n_samples=10_000)

    # --- Validation value
    x = 0.6
    y = 0.6
    subdom = [0.25, 0.75]
    _, y_x_sigma = p_y_x.predict(np.array(x).reshape(-1, 1), return_std=True)
    Q_test = p_y_identity_analytical(y, x_domain, y_x_sigma)

    p_y_result = p_y[int(x//dx)]

    print(y_x_sigma)

    assert abs(Q_test - p_y_result) < 1e-7

@pytest.mark.parametrize("x_domain, func, func_mean, func_var, params", [
                             ([0.8, 1.2], exponent, exp_mean, exp_var, {'r': np.logspace(-1, 1, 3).tolist()}),
                             ([-8, 12], damped_oscilator, damped_oscilator_mean, damped_oscilator_var, {'om': [0.1, 0.5, 1.0]}),
                         ])
def test_integrator(x_domain, func, func_mean, func_var, params):

    bayes = Bayes()

    #x_domain = [0.8, 1.2]
    x_n_bins = 50
    x_bin_edges = np.linspace(x_domain[0], x_domain[1], x_n_bins).reshape(-1)

    p_x = cp.Uniform(x_bin_edges[0], x_bin_edges[-1])
    p_x_array = p_x.pdf(0.5 * (x_bin_edges[1:] + x_bin_edges[:-1]))

    for k, v in params.items():
        for par in v:

            y_mean_an = func_mean([x_bin_edges[0], x_bin_edges[-1]], par)
            y_var_an = func_var([x_bin_edges[0], x_bin_edges[-1]], par)

            E_y_det = bayes.integrator.integrate_with_measure(x_bin_edges, p_x_array, f=lambda x: func(x, par), rule="riemann")
            Var_y_det = bayes.integrator.integrate_with_measure(x_bin_edges, p_x_array,
                                                                f=lambda x: np.power(func(x, par), 2), rule="riemann") \
                        - E_y_det * E_y_det

            print("\nAn-al. E(y) = {:.4} ; Var(y) = {:.4} ".format(y_mean_an, y_var_an))
            print("int    E(y) = {:.4} ; Var(y) = {:.4} ".format(E_y_det, Var_y_det))

            assert abs((y_mean_an - E_y_det) / y_mean_an) < 1e-2
            assert abs((y_var_an - Var_y_det) / y_var_an) < 1e-2

@pytest.mark.skip()
@pytest.mark.parametrize("name, x_domain, func, func_mean, func_var, params", [
                             ('exp', [0.8, 1.2], exponent, exp_mean, exp_var, {'r': np.logspace(-1, 1, 3).tolist()}),
                             ('damp_osc', [-8, 12], damped_oscilator, damped_oscilator_mean, damped_oscilator_var, {'om': [0.1, 0.5, 1.0]}),
                         ])
def test_gpr_exp_mean(name, x_domain, func, func_mean, func_var, params):

    bayes = Bayes()

    x_n_tr = 10
    x = np.linspace(x_domain[0], x_domain[1], x_n_tr)
    dx = x[1] - x[0]

    x_n_bins = 50
    x_bin_edges = np.linspace(x_domain[0] - dx/2., x_domain[1] + dx/2., x_n_bins)

    y = func(x)
    y_domain = [exponent(x_domain[1]), exponent(x_domain[0])]
    y_bin_edges = np.linspace(exponent(x_domain[1] - dx / 2.), exponent(x_domain[0] + dx / 2.), x_n_bins).reshape(-1)

    y_mean_an = exp_mean([x_bin_edges[0], x_bin_edges[-1]])

    p_y_x = GPR(kernel=ConstantKernel()*Matern() + WhiteKernel()).fit(x.reshape(-1, 1), y)
    p_x = cp.Uniform(0, 1)

    p_y = bayes.compute_p_y(p_x, p_y_x, y_bin_edges, 50_000)
    E_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y)

    assert abs(y_mean_an - E_y) < 1e-7

@pytest.mark.parametrize("name, x_domain, func, prior_name, func_mean, func_var, params", [
                             ('exp', [0.8, 1.2], exponent, 'uniform', exp_mean, exp_var, {'r': np.logspace(-1, 1, 3).tolist()}),
                             ('exp', [0.8, 1.2], exponent, 'normal', exp_norm_mean, exp_norm_var, {'r': np.logspace(-1, 1, 3).tolist()}),
                             ('damp_osc', [-8, 12], damped_oscilator, 'uniform', damped_oscilator_mean, damped_oscilator_var, {'om': [0.1, 0.5, 1.0]}),
                             #('damp_osc', [-8, 12], damped_oscilator, 'normal', damped_oscilator_normal_mean, damped_oscilator_normal_var, {'om': [0.1, 0.5, 1.0]}),
                         ])
def test_gpr_var(name, x_domain, func, prior_name, func_mean, func_var, params):
    # currently: 216 test cases totally, 2 assertions each
    # strictly saying, the moments calculated for determenistic function adn for stochastic one may not coincide
    # TODO assuming constant sigma(x)=sigma calculate gaussian integral for the stochastic f(x)=Aexp(-rx)
    # GPR should converge to a small variance, which is bounded by irreducible variance of measurement error / true noise
    # Then, using GPR statistic calculation should converge

    eps_y_prob = 0.1
    x_n_tr_s = [15, 10, 5]

    x_n_bins = 50
    y_n_bins = 50

    x_bin_edges = np.linspace(x_domain[0], x_domain[1], x_n_bins + 1).reshape(-1)

    x_test = (0.5 * (x_bin_edges[1:] + x_bin_edges[:-1])).reshape(-1, 1)

    bayes = Bayes()

    priors = {'uniform': cp.Uniform(x_bin_edges[0], x_bin_edges[-1]),
              'normal': cp.Normal(0.5*(x_bin_edges[0]+x_bin_edges[-1]), (x_bin_edges[-1]-x_bin_edges[0])/6.)}

    p_x = priors[prior_name]

    for k, v in params.items():
        for par in v:
            print("\nTests for {}, param={} and {} prior".format(name, par, prior_name))
            for x_n_tr in x_n_tr_s:

                y_mean_an = func_mean([x_bin_edges[0], x_bin_edges[-1]], par)
                y_var_an = func_var([x_bin_edges[0], x_bin_edges[-1]], par)

                dx = (x_domain[1] - x_domain[0]) / x_n_tr
                x_train = np.linspace(x_domain[0] + dx / 2., x_domain[1] - dx / 2., x_n_tr).reshape(-1, 1)

                y_train = func(x_train, par)
                y_test = func(x_test, par)

                y_domain = [y_test.min(), y_test.max()]
                y_domain_len = y_domain[1] - y_domain[0]
                y_domain = [y_domain[0] - eps_y_prob * y_domain_len,
                            y_domain[1] + eps_y_prob * y_domain_len]

                y_bin_edges = np.linspace(y_domain[0], y_domain[1], y_n_bins + 1).reshape(-1)

                kernel = ConstantKernel(constant_value_bounds=[1e-5, 2.5e-2])*Matern() + WhiteKernel(noise_level_bounds=[1e-6, 1e-2])
                #kernel = ConstantKernel() * Matern() + WhiteKernel()
                p_y_x = GPR(kernel=kernel).fit(x_train, y_train)

                bayes.set_surrogate(p_y_x)
                bayes.set_domain_bins(x_domain, y_domain, x_n_bins, y_n_bins)

                y_pred_mean, y_pred_std = p_y_x.predict(x_test, return_std=True)
                plot_model_function(x_train.reshape(-1), y_train.reshape(-1), x_test.reshape(-1), y_pred_mean.reshape(-1), y_pred_std, y_test=y_test)
                plt.savefig('BI_MC_plots\gpr_{}_par{}_prior{}_tr{}.png'.format(name, par, prior_name, x_n_tr))

                print("\nAn-al. E(y) = {:.4} ; Var(y) = {:.4} ".format(y_mean_an, y_var_an))
                print(f"Number of training evaluations is {x_n_tr}")

                for n_samples in np.logspace(6, 3, 4).tolist():
                    p_y_kde = bayes.compute_p_y_kde(p_x, p_y_x, y_bin_edges, 1, n_samples)
                    E_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, rule="kde_riemann")
                    Var_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, lambda x: x * x,
                                                                        rule="kde_riemann") \
                                - E_y_kde * E_y_kde

                    rel_error_mean_kde = abs((y_mean_an - E_y_kde) / y_mean_an)
                    rel_error_var_kde = abs((y_var_an - Var_y_kde) / y_var_an)

                    p_y = bayes.compute_p_y(p_x, p_y_x, y_bin_edges, n_samples)
                    E_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y, rule="riemann")
                    Var_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y, lambda x: x*x, rule="riemann") - E_y_kde * E_y_kde

                    rel_error_mean = abs((y_mean_an - E_y) / y_mean_an)
                    rel_error_var = abs((y_var_an - Var_y) / y_var_an)

                    print("GPR+kde E(y) = {:.4} ; Var(y) = {:.4} ; number of samples: {}. Relative errors are {:.5} and {:.5}"
                          .format(E_y_kde, Var_y_kde, n_samples, rel_error_mean_kde, rel_error_var_kde))
                    print("GPR+pwc E(y) = {:.4} ; Var(y) = {:.4} ; number of samples: {}. Relative errors are {:.5} and {:.5}"
                          .format(E_y, Var_y, n_samples, rel_error_mean, rel_error_var))

                    # assert abs((y_mean_an - E_y_kde) / y_mean_an) < 1e-2  # Calculate how much error is acceptable due to uncertain model
                    # assert abs((y_var_an - Var_y_kde) / y_var_an) < 1e-2

def test_GPR_vs_PCE():
    # --- Training data
    x_domain = [0.0, 1.5]
    x_n_tr = 9
    dx = (x_domain[1] - x_domain[0]) / x_n_tr
    np.random.seed(1)

    #X_train = x_domain[0] + np.random.rand(x_tr_n) * (x_domain[1] - x_domain[0])
    X_train = np.linspace(x_domain[0]+dx/2., x_domain[1]-dx/2., x_n_tr).reshape(-1, 1)
    #X_train = X_train[np.argsort(X_train)] # if random-> ON

    # !!! for testing
    noize_level = 5e-2
    y_train = np.array([exponent(x, eps=noize_level) for x in X_train]) + np.random.normal(0, noize_level, len(X_train)).reshape(-1, 1)
    #y_train = exponent(X_train, eps=noize_level)  # will not work with noize due to no vectorisation

    y_domain = [exponent(x_domain[0]), exponent(x_domain[1])]
    eps_y_prob = 0.1
    y_domain = [min(y_domain), max(y_domain)]
    y_domain_len = y_domain[1] - y_domain[0]
    y_domain = [y_domain[0] - eps_y_prob * y_domain_len,
                y_domain[1] + eps_y_prob * y_domain_len]

    # --- Testing data
    x_ts_n = 20
    X_test = np.sort(x_domain[0] + np.random.rand(x_ts_n) * (x_domain[1] - x_domain[0])).reshape(-1, 1)

    # --- Prepare a model
    kernel = ConstantKernel(constant_value_bounds=[1e-5, 1e+3]) * Matern() + WhiteKernel(
        noise_level_bounds=[1e-4, 1e+3])
    #kernel = ConstantKernel() * Matern() + WhiteKernel()
    surrogate = GPR(kernel=kernel).fit(X_train, y_train)

    # --- Model training results
    #y_mean, y_std = surrogate.predict(X_train, return_std=True)  # start at (0.5, 0.65)

    y_mean_test, y_std_test = surrogate.predict(X_test, return_std=True)

    print(f'\navg GPR std on a test set {np.mean(y_std_test)}')

    # --- Prior believe on x distribution
    p_x_prior = cp.Uniform(x_domain[0], x_domain[1])
    sigma_factor = 6.
    sigma_factor = 12.
    m_shift_const = 0.
    m_shift_const = +0.35
    p_x_prior = cp.Normal(0.5*(x_domain[0]+x_domain[-1]) + m_shift_const, (x_domain[-1]-x_domain[0])/sigma_factor)

    # --- Distribution of y for given x-prior

    x_bins = 50
    y_bins = 50

    x_bin_edges = np.linspace(*x_domain, x_bins + 1).reshape(-1)
    # !!! for testing use exact X and Y domain
    y_bin_edges = np.linspace(*y_domain, y_bins + 1).reshape(-1)

    y_mid_vals = 0.5*(y_bin_edges[1:] + y_bin_edges[:-1]).reshape(-1, 1)
    x_mid_vals = 0.5*(x_bin_edges[1:] + x_bin_edges[:-1]).reshape(-1, 1)

    bayes = Bayes()
    bayes.set_surrogate(surrogate)
    bayes.set_domain_bins(x_domain, y_domain, x_bins, y_bins)  # TODO: replace everywhere!

    # |Y|=50, |X|=50; n_samles = 4M -> err=0.0987
    # |Y|=50, |X|=50; n_samles = 16M -> t=2:12 err=0.011
    # |Y|=100, |X|=100; n_samles = 48M -> t=4:12 err=0.013
    # |Y|=200, |X|=200; n_samles = 48M -> t=4:35 err=0.013 -> go through Bayes function once more

    # sklearn sampling
    # |Y|=50, |X|=50; n_samles = 4M -> err=0.032
    # |Y|=50, |X|=50; n_samles = 16M -> t=3:58, 3:07 err=0.039, 0.039

    #sklearn sampling + riemann sum integration
    # |Y|=50, |X|=50; n_samles = 4M -> t=0:49; err=0.032
    # |Y|=50, |X|=50; n_samles = 16M -> t=4:41 ; err=0.039

    #sklearn sampling in p_y, kde sampling in E_y, trapezoidal

    p_y = bayes.compute_p_y(p_x_prior, surrogate, y_bin_edges, n_samples=40_000)

    p_y_kde = bayes.compute_p_y_kde(p_x_prior, ndim=1, n_samples=40_000)

    c_y = np.exp(p_y_kde.score_samples(y_mid_vals))

    # DEBUG: compare p(y) computed via binning and via KDE
    p_y_kde_points = np.exp(p_y_kde.score_samples(y_mid_vals))
    fig, ax = plt.subplots()
    ax.plot(y_mid_vals, p_y, label="pwc")
    ax.plot(y_mid_vals, p_y_kde_points, label="kde")
    ax.legend(loc='best')
    fig.savefig('BI_MC_plots/kde_vs_pwc.png')

    #assert np.allclose(p_y, p_y_kde_points)

    y_mean_an = exp_mean([x_bin_edges[0], x_bin_edges[-1]])
    y_var_an = exp_var([x_bin_edges[0], x_bin_edges[-1]])
    y_mean_an = exp_norm_mean([x_bin_edges[0], x_bin_edges[-1]])
    y_var_an = exp_norm_var([x_bin_edges[0], x_bin_edges[-1]])
    print(f'\nAn. deter-ic E(y) = {y_mean_an:.4} ; Var(y) = {y_var_an:.4}')

    # --- Compute statistics in y using surrogate
    print("\nStatistics for y=1.*exp(-2.*x), x~Uniform([0.0, 2.5])")

    E_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y)
    Var_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y, lambda x: x*x) - E_y*E_y

    E_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, rule="kde_riemann")
    Var_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, lambda x: x*x, rule="kde_riemann") \
                - E_y_kde*E_y_kde

    rel_error_mean = abs((y_mean_an - E_y) / y_mean_an)
    rel_error_var = abs((y_var_an - Var_y) / y_var_an)
    rel_error_mean_kde = abs((y_mean_an - E_y_kde) / y_mean_an)
    rel_error_var_kde = abs((y_var_an - Var_y_kde) / y_var_an)

    print("GPR+pwc E(y) = {:.4} ; Var(y) = {:.4} ; number of samples: {}. Relative errors are {:.5} and {:.5}"
          .format(E_y, Var_y, len(X_train), rel_error_mean, rel_error_var))
    print("GPR+kde E(y) = {:.4} ; Var(y) = {:.4} ; number of samples: {}. Relative errors are {:.5} and {:.5}"
          .format(E_y_kde, Var_y_kde, len(X_train), rel_error_mean_kde, rel_error_var_kde))

    # --- Compute statistics in y using PCE
    polynomial_order = 2
    polynomial_expansion = cp.generate_expansion(polynomial_order, p_x_prior)
    quadrature_order = polynomial_order * 2
    #x_pce_s, w_pce_s = cp.generate_quadrature(quadrature_order, p_x_prior, rule="gauss_legendre")
    x_pce_s, w_pce_s = cp.generate_quadrature(quadrature_order, p_x_prior, rule="gaussian")
    evaluations = [exponent(x_pce, eps=noize_level) + np.random.normal(0, noize_level, 1)[0] for x_pce in x_pce_s.T]
    pce_approx = cp.fit_quadrature(polynomial_expansion, x_pce_s, w_pce_s, evaluations)
    y_mean_pce = cp.E(pce_approx, p_x_prior)  # known to be slow in ChaosPy!
    y_std_pce = cp.Var(pce_approx, p_x_prior)
    rel_error_mean_pce = abs((y_mean_an - y_mean_pce[0]) / y_mean_an)
    rel_error_var_pce = abs((y_var_an - y_std_pce[0]) / y_var_an)
    print("PCE    E(y) = {:.4} ; Var(y) = {:.4} ; number of samples: {}. Relative errors are {:.5} and {:.5}"
          .format(y_mean_pce[0], y_std_pce[0], (polynomial_order+1)**2, rel_error_mean_pce, rel_error_var_pce))

    # --- Tests for analytical results for simple p(x) and p(y|x)
    #p_y_test = np.array([test_gaussian_integral(y, x_domain, s) for [y,s] in zip(y_mean_test, y_mean_std)])
    #plot_1d_distrib(np.linspace(*y_domain, 21), p_y_test, names={'y': 'gauassian integral', 'x': 'x'})

    #assert abs(E_y - y_mean_an)/y_mean_an < 1e-2
    #assert abs(Var_y - y_var_an)/y_mean_an < 1e-2

    # ---- Compute posterior
    start_post_calc = t.time()
    p_x_y = bayes.compute_x_y_posterior(p_x_prior, surrogate, x_bin_edges, y_bin_edges)
    end_post_calc = t.time()
    print('posterior calculation took {:2.3} s'.format(end_post_calc - start_post_calc))

######## LLH TEST ############
    # --- Compute posterior via log-likelohood formula
    start_post_calc = t.time()
    p_x_y_vialog = np.zeros((x_bins, y_bins))
    for i in range(len(x_mid_vals)):
        for j in range(len(y_mid_vals)):
            p_x_y_vialog[i, j] = np.exp(bayes.log_likelihood_x_y(x_mid_vals[i], y_mid_vals[j],
                                                                surrogate, p_x_prior, p_y_kde))
    end_post_calc = t.time()
    print('posterior calculation with LLH took {:2.3} s'.format(end_post_calc - start_post_calc))
    # OBSERVATION: compuatation of p(x,y) for 50x50 bins took 3.86s with LLh, compared to 0.496 with MC and 10^4 samples

    # print(p_x_y)
    # print([(x,y) for x,y in zip(np.where(p_x_y_vialog==np.inf)[0], np.where(p_x_y_vialog==np.inf)[1])])
    # assert np.allclose(p_x_y, p_x_y_vialog)
##############################

    #debug: location of small p_y and high p_x_y values
    #print(np.where(abs(c_y) < 1e-9))
    #print(np.where(abs(p_x_y) < 1e-9))
    #highpos_pxy = np.where(p_x_y > 1e2)
    #print([(x,y) for x,y in zip(highpos_pxy[0], highpos_pxy[1])])

    # --- Assume desirable prior y distribution
    # TODO test range of parameters that influence the marginal x variance: training data noize, y prior variance, ...
    # TODO position of y mean (resulting variance will be higher for more gentle slope of model function)

    x_trues = [x_domain[0]+0.5, (x_domain[0]+x_domain[1])/2., x_domain[1]-0.5]
    y_trues = [exponent(x_true) for x_true in x_trues]
    #print("x true : {:.3} \ny true : {:.3}".format(x_true, y_true))

    y_p_sigmas = [0.01, 0.05, 0.25]
    for (x_true, y_p_sigma) in product(x_trues, y_p_sigmas):
        y_true = exponent(x_true)
        p_y = cp.Normal(y_true, y_p_sigma)
        start_marg_calc = t.time()
        #p_x = bayes.compute_x_distribution(p_y, p_x_y, x_bin_edges, y_bin_edges, n_samples=10_000)  #TODO (important) p(x) accumulates suspicious samples in the end of x support
        p_x_kde = bayes.compute_x_distribution_kde(p_y, p_x_y, x_bin_edges, y_bin_edges, 1, 160_000)
        end_marg_calc = t.time()
        print('marginal x distribution calculation took {:2.3} s'.format(end_marg_calc - start_marg_calc))
        p_x = np.exp(p_x_kde.score_samples(x_mid_vals))
        combined_plot(x_bin_edges, y_bin_edges, p_x_y, surrogate, p_x_prior, p_x, c_y, p_y,
                      X_train=X_train,
                      y_train=y_train,
                      x_true=x_true,
                      y_true=y_true,
                      y_s=y_p_sigma)

        # --- test for posterior computation ----
        x_map = p_x.max()
        #assert abs((x_true - x_map)/x_true) < 1e-2
        # TODO think of cases where moments of p(x) can be calculated analytically

@pytest.mark.skip()
def test_GPR_vs_PCE_2D():
    np.set_printoptions(precision=3)
    g_a = [0.5, 3.0]
    # --- Training data
    x_domain = [[0.0, 1.0], [0.0, 1.0]]
    x_n_tr = 50
    dx = [(x_domain[0][1] - x_domain[0][0]) / x_n_tr, (x_domain[1][1] - x_domain[1][0]) / x_n_tr]

    X_train = lhsmdu.sample(2, x_n_tr).reshape(-1, 2)
    for dim in range(X_train.shape[-1]):
        X_train[:, dim] = x_domain[dim][0] + X_train[:, dim] * (x_domain[dim][1] - x_domain[dim][0])
    X_train = np.asarray(X_train)

    # use noise level 0 for analytic solution
    y_train = np.array([g_function(X_train[i, :].reshape(-1), a=g_a, d=2) for i in range(X_train.shape[0])])

    # y_domain = [g_function(np.array([x_domain[0][0], x_domain[1][0]]),
    #             g_function(np.array([x_domain[0][1], x_domain[1][1]]))]

    eps_y_prob = 0.1
    # y_domain = [[min(y_domain[0]), max(y_domain[0])], [min(y_domain[1]), max(y_domain[1])]] # if y is 2D
    y_domain = [min(y_train), max(y_train)]
    # y_domain_len = [y_domain[0][1] - y_domain[0][0], y_domain[1][1] - y_domain[1][0]] # if y is 2d
    y_domain_len = y_domain[1] - y_domain[0]
    # y_domain = [[y_domain[0][0] - eps_y_prob * y_domain_len[0],
    #              y_domain[0][1] + eps_y_prob * y_domain_len[0]],
    #             [y_domain[1][0] - eps_y_prob * y_domain_len[1],
    #              y_domain[1][1] + eps_y_prob * y_domain_len[1]]]
    y_domain = [y_domain[0] - eps_y_prob * y_domain_len,
                y_domain[1] + eps_y_prob * y_domain_len]

    # --- Testing data
    x_ts_n = 200
    X_test = np.zeros((x_ts_n, 2))
    X_test[:, 0] = np.sort(x_domain[0][0] + np.random.rand(x_ts_n) * (x_domain[0][1] - x_domain[0][0])).reshape(-1)
    X_test[:, 1] = np.sort(x_domain[1][0] + np.random.rand(x_ts_n) * (x_domain[1][1] - x_domain[1][0])).reshape(-1)
    y_test = np.array([g_function(X_test[i, :].reshape(-1), a=g_a, d=2) for i in range(X_test.shape[0])])

    # --- Prepare a model
    kernel = ConstantKernel() * Matern() * Matern()   # + WhiteKernel()
    surrogate = GPR(kernel=kernel).fit(X_train, y_train)

    # --- Model training results
    y_mean_test, y_std_test = surrogate.predict(X_test, return_std=True)

    #DEBUG
    print("\n")
    print(surrogate.kernel_)
    print(surrogate.kernel_.k2.anisotropic)
    print('Testing R2 score {}'.format(surrogate.score(X_test, y_test)))
    re = abs(y_test - y_mean_test) / y_test
    print('Testing RE avg {}'.format(re.sum()/x_ts_n))

    print(f'\navg GPR std on a test set {np.mean(y_std_test)}')

    # --- Prior believe on x distribution
    p_x_prior = cp.J(cp.Uniform(x_domain[0][0], x_domain[0][1]),
                     cp.Uniform(x_domain[1][0], x_domain[1][1]))

    # --- Distribution of y for given x-prior
    x_bins = [16, 16]
    y_bins = 32

    x_bin_edges = [np.linspace(*x_domain[0], x_bins[0] + 1).reshape(-1),
                   np.linspace(*x_domain[1], x_bins[1] + 1).reshape(-1)]
    # for testing use exact X and Y domain
    y_bin_edges = np.linspace(*y_domain, y_bins + 1).reshape(-1)

    y_mid_vals = 0.5*(y_bin_edges[1:] + y_bin_edges[:-1]).reshape(-1, 1)
    x_mid_vals = [0.5*(x_bin_edges[0][1:] + x_bin_edges[0][:-1]).reshape(-1, 1),
                  0.5 * (x_bin_edges[1][1:] + x_bin_edges[1][:-1]).reshape(-1, 1)]

    # DEBUG
    #x_mid_vals_points = np.meshgrid(x_mid_vals[0], x_mid_vals[1])
    #print(x_mid_vals)
    # TODO: Sobol indices are of same value ~0.98 for both features, and different from the analytic ones - check what surrogate returns
    #y_surr_predicts = surrogate.predict(x_mid_vals_points)
    #print(y_surr_predicts)
    #y_g_values = g_function_sobol_idices(x_mid_vals_points, a=g_a, d=2)
    #print(y_surr_predicts)

    bayes = Bayes()
    bayes.set_surrogate(surrogate)
    bayes.set_domain_bins(x_domain, y_domain, x_bins, y_bins, n_dim=2)

    p_y_kde = bayes.compute_p_y_kde(p_x_prior, ndim=2, n_samples=40_000)

    S_i_an = g_function_sobol_idices(a=g_a, d=2)  # for 2D sobols don't sum up

    print('\nAnalytic S_i = {}'.format(S_i_an))

    # --- Compute statistics in y using surrogate
    print("\nStatistics for y=g_function(x), x~Uniform([0.0, 1.0])^2")

    E_y_gp = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, rule="kde_riemann")
    Var_y_gp = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, lambda x: x*x, rule="kde_riemann") \
                - E_y_gp * E_y_gp

    # dimension to analyse 1
    n_dim_out = (1, )
    n_dim_stay = 0
    p_y_x1_x2 = bayes.compute_p_y_x_part(p_x_prior, ndim_active=n_dim_out)  # dp(y,x1|x2)
    p_y_x1x2 = bayes.p_y_x_surrogate_likelihood  # TODO has to be dp(y|x1,x2) , as 3D array
    # Next two object should represent functions over X1 domain
    E_y_x1_gp = np.zeros(x_bins[n_dim_stay])
    Var_y_x1_gp = np.zeros(x_bins[n_dim_stay])
    for x1 in range(x_bins[n_dim_stay]):   # TODO rewrite the underlying formula for E_X-1_Y|X1
        E_y_x1_gp[x1] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x1_x2[:, x1], rule="riemann")
        E_y_x1_gp[x1] = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_out[0]], p_y_x1x2[:, x1, :], rule="riemann") # TODO: has to be integral over X2 here (for every values of Y, X1)
        Var_y_x1_gp[x1] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x1_x2[:, x1],
                                        lambda x: x*x, rule="riemann") - E_y_x1_gp[x1] * E_y_x1_gp[x1]
    p_x1 = p_x_prior[n_dim_stay].pdf(x_mid_vals[n_dim_stay]).reshape(-1)
    Var_y_x1_gp_func = lambda x: Var_y_x1_gp[bayes.walk_bins(x, x_bin_edges[n_dim_stay])[-1]]
    E_Var_y_x1_gp = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x1, lambda x: Var_y_x1_gp_func(x), rule='riemann')
    S_1_gp = (Var_y_gp - E_Var_y_x1_gp) / Var_y_gp

    ####################
    # dimension to analyse 1 - alternative calculation: Var_i (E_-i(y|x_i))
    n_dim_out = (1, )
    n_dim_stay = 0

    p_y_x1 = bayes.compute_p_y_x_part(p_x_prior, ndim_active=n_dim_out)  # dp(y|x1)
    #p_y_x2 = bayes.compute_p_y_x_part(p_x_prior, ndim_active=(n_dim_stay,))  # dp(y|x2)

    E_x2_y_x1_gp = np.zeros(x_bins[n_dim_stay])
    for x1 in range(x_bins[n_dim_stay]):
        E_x2_y_x1_gp[x1] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x1[:, x1], rule="riemann")

    p_x1 = p_x_prior[n_dim_stay].pdf(x_mid_vals[n_dim_stay]).reshape(-1)

    E_x2_y_x1_gp_func = lambda x: E_x2_y_x1_gp[bayes.walk_bins(x, x_bin_edges[n_dim_stay])[-1]]

    E_E_x2_y_x1_gp_2 =  bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x1, lambda x: E_x2_y_x1_gp_func(x), rule='riemann')
    Var_E_x2_y_x1_gp_2 = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x1, lambda x: np.power(E_x2_y_x1_gp_func(x), 2), rule='riemann')\
                         - E_E_x2_y_x1_gp_2 * E_E_x2_y_x1_gp_2

    S_1_gp_2 = Var_E_x2_y_x1_gp_2 / Var_y_gp
    ##################

    ##################
    # dimension to analyse 1 - alternative calculation: considering GP mean value only
    n_dim_out = (1, )
    n_dim_stay = 0
    x1_mids = 0.5*(x_bin_edges[0][1:] - x_bin_edges[0][:-1])

    p_x2 = p_x_prior[n_dim_out[0]].pdf(x_mid_vals[n_dim_stay]).reshape(-1)
    p_x1 = p_x_prior[n_dim_stay].pdf(x_mid_vals[n_dim_stay]).reshape(-1)

    E_x2_y_x1_gp = np.zeros(x_bins[n_dim_stay])
    for x1 in range(x_bins[n_dim_stay]):
        #x1_coord = bayes.walk_bins([x1], x_bin_edges[n_dim_stay])[-1]
        x1_coord = x1_mids[x1]
        y_func = lambda x: bayes.p_y_x_surrogate_likelihood.predict(np.vstack([x1_coord*np.ones(len(x)), x]).transpose())
        E_x2_y_x1_gp[x1] = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_out[0]], p_x2, y_func, rule="riemann")

    E_x2_y_x1_gp_func = lambda x: E_x2_y_x1_gp[bayes.walk_bins(x, x_bin_edges[n_dim_stay])[-1]]

    E_E_x2_y_x1_gp_3 = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x1, E_x2_y_x1_gp_func, rule='riemann')
    Var_E_x2_y_x1_gp_3 = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x1, lambda x: np.power(E_x2_y_x1_gp_func(x), 2), rule='riemann')\
                       - E_E_x2_y_x1_gp_3 * E_E_x2_y_x1_gp_3

    S_1_gp_3 = Var_E_x2_y_x1_gp_3 / Var_y_gp
    ################


    # dimension to analyse 2
    n_dim_out = (0, )
    n_dim_stay = 1
    p_y_x2_x1 = bayes.compute_p_y_x_part(p_x_prior, ndim_active=n_dim_out)  # dp(y,x1|x2)
    # Next two object should represent functions over X1 domain
    E_y_x2_gp = np.zeros(x_bins[n_dim_stay])
    Var_y_x2_gp = np.zeros(x_bins[n_dim_stay])
    for x2 in range(x_bins[n_dim_stay]):
        E_y_x2_gp[x2] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x2_x1[:, x2], rule="riemann")
        Var_y_x2_gp[x2] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x2_x1[:, x2],
                                        lambda x: x*x, rule="riemann") - E_y_x2_gp[x2] * E_y_x2_gp[x2]
    p_x2 = p_x_prior[n_dim_stay].pdf(x_mid_vals[n_dim_stay]).reshape(-1)
    Var_y_x2_gp_func = lambda x: Var_y_x2_gp[bayes.walk_bins(x, x_bin_edges[n_dim_stay])[-1]]
    E_Var_y_x2_gp = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x2, lambda x: Var_y_x2_gp_func(x), rule='riemann')

    S_2_gp = (Var_y_gp - E_Var_y_x2_gp) / Var_y_gp

    rel_error_s_i = [abs((S_1_gp - S_i_an[0]) / S_i_an[0]),
                     abs((S_2_gp - S_i_an[1]) / S_i_an[1])]

    print("GPR S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(1, S_1_gp_3, len(X_train), rel_error_s_i[0]))
    print("GPR S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(2, S_2_gp, len(X_train), rel_error_s_i[1]))

    # --- Compute statistics in y using PCE
    polynomial_order = 3
    polynomial_expansion = cp.generate_expansion(polynomial_order, p_x_prior)
    quadrature_order = polynomial_order * 2
    x_pce_s, w_pce_s = cp.generate_quadrature(quadrature_order, p_x_prior, rule="gauss_legendre")
    evaluations = [g_function(x_pce, a=g_a, d=2) for x_pce in x_pce_s.T]
    pce_approx = cp.fit_quadrature(polynomial_expansion, x_pce_s, w_pce_s, evaluations)
    S_i_pce = cp.Sens_m(pce_approx, p_x_prior)
    Var_pce = cp.Var(pce_approx, p_x_prior)
    rel_error_mean_pce = abs((S_i_an - S_i_pce) / S_i_an)
    print("\n")
    print("PCE Var = {}"
          .format(1, Var_pce))
    print("PCE S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(1, S_i_pce[0], (polynomial_order+1)**2, rel_error_mean_pce[0]))
    print("PCE S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(2, S_i_pce[1], (polynomial_order+1)**2, rel_error_mean_pce[1]))

@pytest.mark.skip()
def test_GPR_vs_PCE_3D():
    np.random.seed(1)
    np.set_printoptions(precision=3)
    # --- Training data
    x_domain = [[-np.pi, np.pi]]*3

    # --- Testing data
    x_ts_n = 5000
    X_test = np.zeros((x_ts_n, 3))
    X_test[:, 0] = np.array(x_domain[0][0] + np.random.rand(x_ts_n) * (x_domain[0][1] - x_domain[0][0])).reshape(-1)
    X_test[:, 1] = np.array(x_domain[1][0] + np.random.rand(x_ts_n) * (x_domain[1][1] - x_domain[1][0])).reshape(-1)
    X_test[:, 2] = np.array(x_domain[2][0] + np.random.rand(x_ts_n) * (x_domain[2][1] - x_domain[2][0])).reshape(-1)
    y_test = np.array([ishigami_function(X_test[i, :].reshape(-1)) for i in range(X_test.shape[0])])

    x_n_tr_s = [10, 25, 50, 75, 100]
    # TODO: test fit of GPs for Ishigami for its slices
    # TODO: add the sequential design/optimisation
    x_n_tr = 100
    for x_n_tr in x_n_tr_s:
        dx = [(x_domain[0][1] - x_domain[0][0]) / x_n_tr,
              (x_domain[1][1] - x_domain[1][0]) / x_n_tr,
              (x_domain[2][1] - x_domain[2][0]) / x_n_tr]

        X_train = lhsmdu.sample(3, x_n_tr).reshape(-1, 3)
        for dim in range(X_train.shape[-1]):
            X_train[:, dim] = x_domain[dim][0] + X_train[:, dim] * (x_domain[dim][1] - x_domain[dim][0])
        X_train = np.asarray(X_train)

        # use noise level 0 for analytic solution
        y_train = np.array([ishigami_function(X_train[i, :].reshape(-1)) for i in range(X_train.shape[0])])

        eps_y_prob = 0.1
        y_domain = [min(y_train), max(y_train)]
        y_domain_len = y_domain[1] - y_domain[0]
        y_domain = [y_domain[0] - eps_y_prob * y_domain_len,
                    y_domain[1] + eps_y_prob * y_domain_len]

        # --- Prepare a model
        kernel = ConstantKernel() * Matern() * Matern() * Matern()   # + WhiteKernel()
        kernel = ConstantKernel(1.) * RBF(np.ones(3))
        surrogate = GPR(kernel=kernel).fit(X_train, y_train)

        # --- Model training results
        y_mean_test, y_std_test = surrogate.predict(X_test, return_std=True)

        #DEBUG
        print("\n")
        print(surrogate.kernel_)
        print(surrogate.kernel_.k2.anisotropic)
        #print('Training X: {}'.format(X_train))
        print('Testing R2 score {}'.format(surrogate.score(X_test, y_test)))
        re = (y_test - y_mean_test) / y_test
        print('Testing RE avg {}'.format(re.sum()/x_ts_n))
        #print('Actual valeus {}'.format(y_test))
        #print('Infered valeus {}'.format(y_mean_test))
        fix, axs = plt.subplots(2,1)
        axs[0].hist(y_test, 40, label='true')
        axs[1].hist(y_mean_test, 40, label='surrogate')
        plt.legend(loc='best')
        plt.savefig('hist_ishigami_tr{}.png'.format(x_n_tr))

    # --- Surrogate utilization

    print(f'\navg GPR std on a test set {np.mean(y_std_test)}')

    # --- Prior believe on x distribution
    p_x_prior = cp.J(cp.Uniform(x_domain[0][0], x_domain[0][1]),
                     cp.Uniform(x_domain[1][0], x_domain[1][1]),
                     cp.Uniform(x_domain[2][0], x_domain[2][1]))

    # --- Distribution of y for given x-prior
    x_bins = [10, 10, 10]
    y_bins = 32

    x_bin_edges = [np.linspace(*x_domain[0], x_bins[0] + 1).reshape(-1),
                   np.linspace(*x_domain[1], x_bins[1] + 1).reshape(-1),
                   np.linspace(*x_domain[2], x_bins[2] + 1).reshape(-1)]
    # for testing use exact X and Y domain
    y_bin_edges = np.linspace(*y_domain, y_bins + 1).reshape(-1)

    y_mid_vals = 0.5*(y_bin_edges[1:] + y_bin_edges[:-1]).reshape(-1, 1)
    x_mid_vals = [0.5*(x_bin_edges[0][1:] + x_bin_edges[0][:-1]).reshape(-1, 1),
                  0.5 * (x_bin_edges[1][1:] + x_bin_edges[1][:-1]).reshape(-1, 1),
                  0.5 * (x_bin_edges[2][1:] + x_bin_edges[2][:-1]).reshape(-1, 1)]

    bayes = Bayes()
    bayes.set_surrogate(surrogate)
    bayes.set_domain_bins(x_domain, y_domain, x_bins, y_bins, n_dim=2)

    p_y_kde = bayes.compute_p_y_kde(p_x_prior, ndim=3, n_samples=40_000)

    S_i_an = ishigami_first_sobols()

    print('\nAnalytic S_i = {}'.format(S_i_an))

    # --- Compute statistics in y using surrogate
    print("\nStatistics for y=Ishigami(x), x~Uniform([0.0, 1.0])^3")

    E_y_gp = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, rule="kde_riemann")
    Var_y_gp = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, lambda x: x*x, rule="kde_riemann") \
                - E_y_gp * E_y_gp

    # dimension to analyse 1
    n_dim_out = (1, 2)
    n_dim_stay = 0
    p_y_x1_x2x3 = bayes.compute_p_y_x_part(p_x_prior, ndim=3, ndim_active=n_dim_out, n_samples=40_000)  # dp(y,x1|x2,x3)
    # Next two object should represent functions over X1 domain
    E_y_x1_gp = np.zeros(x_bins[n_dim_stay])
    Var_y_x1_gp = np.zeros(x_bins[n_dim_stay])
    for x1 in range(x_bins[n_dim_stay]):
        E_y_x1_gp[x1] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x1_x2x3[:, x1], rule="riemann")
        Var_y_x1_gp[x1] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x1_x2x3[:, x1],
                                        lambda x: x*x, rule="riemann") - E_y_x1_gp[x1] * E_y_x1_gp[x1]
    p_x1 = p_x_prior[n_dim_stay].pdf(x_mid_vals[n_dim_stay]).reshape(-1)
    Var_y_x1_gp_func = lambda x: Var_y_x1_gp[bayes.walk_bins(x, x_bin_edges[n_dim_stay])[-1]]
    E_Var_y_x1_gp = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x1, lambda x: Var_y_x1_gp_func(x), rule='riemann')
    S_1_gp = (Var_y_gp - E_Var_y_x1_gp) / Var_y_gp

    # dimension to analyse 2
    n_dim_out = (0, 2)
    n_dim_stay = 1
    p_y_x2_x1x3 = bayes.compute_p_y_x_part(p_x_prior, ndim=3, ndim_active=n_dim_out, n_samples=40_000)  # dp(y,x2|x1,x3)
    # Next two object should represent functions over X2 domain
    E_y_x2_gp = np.zeros(x_bins[n_dim_stay])
    Var_y_x2_gp = np.zeros(x_bins[n_dim_stay])
    for x2 in range(x_bins[n_dim_stay]):
        E_y_x2_gp[x2] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x2_x1x3[:, x2], rule="riemann")
        Var_y_x2_gp[x2] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x2_x1x3[:, x2],
                                        lambda x: x*x, rule="riemann") - E_y_x2_gp[x2] * E_y_x2_gp[x2]
    p_x2 = p_x_prior[n_dim_stay].pdf(x_mid_vals[n_dim_stay]).reshape(-1)
    Var_y_x2_gp_func = lambda x: Var_y_x2_gp[bayes.walk_bins(x, x_bin_edges[n_dim_stay])[-1]]
    E_Var_y_x2_gp = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x2, lambda x: Var_y_x2_gp_func(x), rule='riemann')
    S_2_gp = (Var_y_gp - E_Var_y_x2_gp) / Var_y_gp

    # dimension to analyse 3
    n_dim_out = (0, 1)
    n_dim_stay = 2
    p_y_x3_x1x2 = bayes.compute_p_y_x_part(p_x_prior, ndim=3, ndim_active=n_dim_out, n_samples=40_000)  # dp(y,x3|x1,x2)
    # Next two object should represent functions over X3 domain
    E_y_x3_gp = np.zeros(x_bins[n_dim_stay])
    Var_y_x3_gp = np.zeros(x_bins[n_dim_stay])
    for x3 in range(x_bins[n_dim_stay]):
        E_y_x3_gp[x3] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x3_x1x2[:, x3], rule="riemann")
        Var_y_x3_gp[x3] = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_x3_x1x2[:, x3],
                                        lambda x: x*x, rule="riemann") - E_y_x3_gp[x3] * E_y_x3_gp[x3]
    p_x3 = p_x_prior[n_dim_stay].pdf(x_mid_vals[n_dim_stay]).reshape(-1)
    Var_y_x3_gp_func = lambda x: Var_y_x3_gp[bayes.walk_bins(x, x_bin_edges[n_dim_stay])[-1]]
    E_Var_y_x3_gp = bayes.integrator.integrate_with_measure(x_bin_edges[n_dim_stay],
                                                            p_x3, lambda x: Var_y_x3_gp_func(x), rule='riemann')
    S_3_gp = (Var_y_gp - E_Var_y_x3_gp) / Var_y_gp

    rel_error_s_i = [abs((S_1_gp - S_i_an[0]) / S_i_an[0]),
                     abs((S_2_gp - S_i_an[1]) / S_i_an[1]),
                     abs((S_3_gp - S_i_an[1]) / S_i_an[1])]

    print("GPR S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(1, S_1_gp, len(X_train), rel_error_s_i[0]))
    print("GPR S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(2, S_2_gp, len(X_train), rel_error_s_i[1]))
    print("GPR S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(3, S_3_gp, len(X_train), rel_error_s_i[2]))

    # --- Compute statistics in y using PCE
    polynomial_order = 8
    polynomial_expansion = cp.generate_expansion(polynomial_order, p_x_prior)
    quadrature_order = polynomial_order * 2
    x_pce_s, w_pce_s = cp.generate_quadrature(quadrature_order, p_x_prior, rule="gauss_legendre")
    evaluations = [ishigami_function(x_pce) for x_pce in x_pce_s.T]
    pce_approx = cp.fit_quadrature(polynomial_expansion, x_pce_s, w_pce_s, evaluations)
    S_i_pce = cp.Sens_m(pce_approx, p_x_prior)
    rel_error_mean_pce = abs((S_i_an - S_i_pce) / S_i_an)
    print("\n")
    print("PCE S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(1, S_i_pce[0], (polynomial_order+1)**2, rel_error_mean_pce[0]))
    print("PCE S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(2, S_i_pce[1], (polynomial_order+1)**2, rel_error_mean_pce[1]))
    print("PCE S_{} = {:.4} ; number of samples: {}. Relative errors are {}"
          .format(3, S_i_pce[2], (polynomial_order+1)**2, rel_error_mean_pce[2]))
