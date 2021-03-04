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

"""
TOD* make tests for
 - marginalization: pdf, E, Var
 - surrogate convergence: residuals between real function and mean of surrogate prediction
 - surrogate statistics calculation: E, Var, Sobol indices
 """

def exponent(x, a=1., r=2., eps=0.):
    return a * np.e ** (-r*x) + np.random.normal(0, eps, x.shape[0])

def damped_oscilator(x, a=1., g=0.05, om=0.5):
    return a * np.exp(-g*np.square(x)) * np.cos(om * x)

def exp_mean(x_domain, a=1., r=2.):
    # here we consider x as a uniform random variable at x_domain
    # If x is considered a stochastic parameter, then we calculate statistic for the model at point 'r'
    return a * (np.exp(-r * x_domain[0]) - np.exp(-r * x_domain[1])) / (r * (x_domain[1] - x_domain[0]))

def exp_var(x_domain, a=1., r=2.):
    exp_mean_val = exp_mean(x_domain, a, r)
    return a*a*(np.exp(-2*r*x_domain[0])-np.exp(-2*r*x_domain[1]))/(2*r*(x_domain[1]-x_domain[0]))-exp_mean_val*exp_mean_val

def damped_oscilator_mean(x_domain, a=1., g=0.05, om=0.5):
    return (a * 1j * cm.sqrt(m.pi) * cm.exp(-om * om/(4 * g))) * \
           (erfi((om - 2 * 1j * g * x_domain[0]) / (2 * cm.sqrt(g))) - erfi((om + 2 * 1j * g * x_domain[0]) / (2 * cm.sqrt(g))) - \
            erfi((om - 2 * 1j * g * x_domain[1]) / (2 * cm.sqrt(g))) + erfi((om + 2 * 1j * g * x_domain[1]) / (2 * cm.sqrt(g)))) / \
           (4 * (x_domain[1] - x_domain[0]) * cm.sqrt(g))

def damped_scilator_var(x_domain, a=1., g=0.05, om=0.5):
    return 0.

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

    p_y = bayes.compute_normalizing_coefficient(p_x, p_y_x, y_edges, n_samples=10_000)

    # --- Validation value
    x = 0.6
    y = 0.6
    subdom = [0.25, 0.75]
    _, y_x_sigma = p_y_x.predict(np.array(x).reshape(-1, 1), return_std=True)
    Q_test = p_y_identity_analytical(y, x_domain, y_x_sigma)

    p_y_result = p_y[int(x//dx)]

    print(y_x_sigma)

    assert abs(Q_test - p_y_result) < 1e-7

@pytest.mark.skip()
def test_integrator():

    bayes = Bayes()

    x_domain = [0.8, 1.2]
    x_n_bins = 50
    x_bin_edges = np.linspace(x_domain[0], x_domain[1], x_n_bins).reshape(-1)

    p_x = cp.Uniform(x_bin_edges[0], x_bin_edges[-1])
    p_x_array = p_x.pdf(0.5 * (x_bin_edges[1:] + x_bin_edges[:-1]))

    for r in np.logspace(-1, 2, 4).tolist():

        y_mean_an = exp_mean([x_bin_edges[0], x_bin_edges[-1]], r=r)
        y_var_an = exp_var([x_bin_edges[0], x_bin_edges[-1]], r=r)

        E_y_det = bayes.integrator.integrate_with_measure(x_bin_edges, p_x_array, f=lambda x: exponent(x, r=r), rule="riemann")
        Var_y_det = bayes.integrator.integrate_with_measure(x_bin_edges, p_x_array,
                                                            f=lambda x: np.power(exponent(x, r=r), 2), rule="riemann") \
                    - E_y_det * E_y_det

        print("\nAn-al. E(y) = {:.4} ; Var(y) = {:.4} ".format(y_mean_an, y_var_an))
        print("int    E(y) = {:.4} ; Var(y) = {:.4} ".format(E_y_det, Var_y_det))

        assert abs(y_mean_an - E_y_det) < 1e-3
        assert abs(y_var_an - Var_y_det) < 1e-4

@pytest.mark.skip()
def test_gpr_exp_mean():

    bayes = Bayes()

    x_domain = [0, 1]
    x_n_tr = 10
    x = np.linspace(x_domain[0], x_domain[1], x_n_tr)
    dx = x[1] - x[0]

    x_n_bins = 50
    x_bin_edges = np.linspace(x_domain[0] - dx/2., x_domain[1] + dx/2., x_n_bins)

    y = exponent(x)
    y_domain = [exponent(x_domain[1]), exponent(x_domain[0])]
    y_bin_edges = np.linspace(exponent(x_domain[1] - dx / 2.), exponent(x_domain[0] + dx / 2.), x_n_bins).reshape(-1)

    y_mean_an = exp_mean([x_bin_edges[0], x_bin_edges[-1]])

    p_y_x = GPR(kernel=ConstantKernel()*Matern() + WhiteKernel()).fit(x.reshape(-1, 1), y)
    p_x = cp.Uniform(0, 1)

    p_y = bayes.compute_normalizing_coefficient(p_x, p_y_x, y_bin_edges, 50_000)
    E_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y)

    assert abs(y_mean_an - E_y) < 1e-7

def test_gpr_exp_var():
    # strictly saying, the moments calculated for determenistic function adn for stochastic one may not conicide
    #TODO assuming constant sigma(x)=sigma calculate gaussian integral for the stochastic f(x)=Aexp(-rx)
    # With GPR statistic calculation should convergere with small variance, which boundeded by irreducible variance of
    # measurement error / true noize

    x_domain = [0.8, 1.2]
    x_n_tr = 5
    x_n_bins = 50
    y_n_bins = 50

    x_bin_edges = np.linspace(x_domain[0], x_domain[1], x_n_bins + 1).reshape(-1)
    dx = (x_domain[1] - x_domain[0]) / x_n_tr
    x_train = np.linspace(x_domain[0]+dx/2., x_domain[1]-dx/2., x_n_tr)

    y_train = exponent(x_train)
    y_bin_edges = np.linspace(exponent(x_bin_edges[-1]), exponent(x_bin_edges[0]), y_n_bins + 1).reshape(-1)

    y_mean_an = exp_mean([x_bin_edges[0], x_bin_edges[-1]])
    y_var_an = exp_var([x_bin_edges[0], x_bin_edges[-1]])

    p_y_x = GPR(kernel=ConstantKernel(constant_value_bounds=[1e-5, 2.5e-2])*Matern() + WhiteKernel(noise_level_bounds=[1e-6, 1e-1])).fit(x_train.reshape(-1, 1), y_train)
    p_x = cp.Uniform(x_bin_edges[0], x_bin_edges[-1])

    x_test = 0.5*(x_bin_edges[1:]+x_bin_edges[:-1])
    y_pred_mean, y_pred_std = p_y_x.predict(x_test.reshape(-1, 1), return_std=True)
    plot_model_function(x_train, y_train, x_test, y_pred_mean, y_pred_std)
    plt.savefig('BI_MC_plots\gpr.png')

    print("\nAn-al. E(y) = {:.4} ; Var(y) = {:.4} ".format(y_mean_an, y_var_an))

    bayes = Bayes()

    for n_samples in np.logspace(6, 3, 4).tolist():
        p_y_kde = bayes.compute_p_y_kde(p_x, p_y_x, y_bin_edges, n_samples)
        E_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, rule="kde_riemann")
        Var_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, lambda x: x*x, rule="kde_riemann") \
                    - E_y_kde * E_y_kde

        #p_y = bayes.compute_normalizing_coefficient(p_x, p_y_x, y_bin_edges, n_samples)
        #E_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y, rule="riemann")
        #Var_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y, lambda x: x*x, rule="riemann") - E_y_kde * E_y_kde

        print("GPR+kde E(y) = {:.4} ; Var(y) = {:.4} ; number of samples: {} ".format(E_y_kde, Var_y_kde, n_samples))
        #print("GPR+pwc E(y) = {:.4} ; Var(y) = {:.4} ; number of samples: {} ".format(E_y, Var_y, n_samples))

        assert abs((y_mean_an - E_y_kde) / y_mean_an) < 1e-2
        assert abs((y_var_an - Var_y_kde) / y_var_an) < 1e-2

def test_GPR_vs_PCE():
    # --- Training data
    x_domain = [0.0, 2.5]
    x_n_tr = 9
    dx = (x_domain[1] - x_domain[0]) / x_n_tr
    np.random.seed(1)

    #X_train = x_domain[0] + np.random.rand(x_tr_n) * (x_domain[1] - x_domain[0])
    X_train = np.linspace(x_domain[0]+dx/2., x_domain[1]-dx/2., x_n_tr)
    #X_train = X_train[np.argsort(X_train)] # if random-> ON
    X_train = X_train.reshape(-1, 1)

    noize_level = 2.5e-2
    # !!! for analytical testing
    noize_level = 0.
    #y_train = np.array([exponent(x, eps=noize_level) for x in X_train])
    y_train = exponent(X_train, eps=noize_level)

    #y_domain_delta = 0.  #.15
    y_domain = [exponent(x_domain[0]), exponent(x_domain[1])]
    if y_domain[1] < y_domain[0]:
        y_domain = [y_domain[1], y_domain[0]]

    #if y_domain[0] < 0.:
    #    y_domain[0] = 0.

    # --- Testing data
    #x_domain = [x_domain[0] - 0.15, x_domain[1]]
    #y_domain = [y_domain[0]*0.8, y_domain[1]*1.1]
    x_ts_n = 20
    X_test = np.sort(x_domain[0] + np.random.rand(x_ts_n) * (x_domain[1] - x_domain[0])).reshape(-1, 1)

    # --- Prepare a model
    kernel = ConstantKernel(constant_value_bounds=[1e-5, 2.5e-2]) * Matern() + \
                            WhiteKernel(noise_level_bounds=[1e-6, 1e-1])
    surrogate = GPR(kernel=kernel).fit(X_train, y_train)

    # --- Model training results
    y_mean, y_std = surrogate.predict(X_train, return_std=True)  # start at (0.5, 0.65)

    y_mean_test, y_mean_std = surrogate.predict(X_test, return_std=True)

    print(f'\navg GPR std on a test set {np.mean(y_mean_std)}')

    # --- Prior believe on x distribution
    p_x_prior = cp.Uniform(x_domain[0], x_domain[1])
    #p_x_prior = cp.Normal(0.5*(x_domain[0] + x_domain[1]), 0.25*(x_domain[1] - x_domain[0]))

    # --- Distribution of y for given x-prior

    x_bins = 20  #50
    y_bins = 20  #50

    x_bin_edges = np.linspace(*x_domain, x_bins + 1).reshape(-1)
    # !!! for testing use exact X and Y domain
    y_domain = [exponent(x_domain[1]), exponent(x_domain[0])]
    y_bin_edges = np.linspace(*y_domain, y_bins + 1).reshape(-1)

    bayes = Bayes()
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

    p_y = bayes.compute_normalizing_coefficient(p_x_prior, surrogate, y_bin_edges, n_samples=1_000_000)

    p_y_kde = bayes.compute_p_y_kde(p_x_prior, surrogate, y_bin_edges, n_samples=1_000_000)

    # # DEBUG
    # p_y_kde_points = np.exp(p_y_kde.score_samples(0.5*(y_bin_edges[1:] + y_bin_edges[:-1]).reshape(-1, 1)))
    # fig, ax = plt.subplots()
    # ax.plot(0.5*(y_bin_edges[1:] + y_bin_edges[:-1]), p_y, label="pwc")
    # ax.plot(0.5*(y_bin_edges[1:] + y_bin_edges[:-1]), p_y_kde_points, label="kde")
    # ax.legend(loc='best')
    # fig.savefig('kde_vs_pwc.png')

    #assert np.allclose(p_y, p_y_kde_points)

    # --- Compute statistics in y using surrogate

    print("\nStatistics for y=1.*exp(-2.*x), x~Uniform([0.0, 2.5])")

    E_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y)
    Var_y = bayes.integrator.integrate_with_measure(y_bin_edges, p_y, lambda x: x*x) - E_y*E_y

    E_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, rule="kde_riemann")
    Var_y_kde = bayes.integrator.integrate_with_measure(y_bin_edges, p_y_kde, lambda x: x*x, rule="kde_riemann") - E_y*E_y
    ### TODO kde computation yield E_y ~50 larger than the analytical one -> compare p_y and p_y_kde pointwise

    print("GPR+MC   E(y) = {:.4} ; Var(y) = {:.4} ; number of evaluations: {} ".format(E_y, Var_y, len(X_train)))
    print("GPR+kde   E(y) = {:.4} ; Var(y) = {:.4} ; number of evaluations: {} ".format(E_y_kde, Var_y_kde, len(X_train)))

    # --- Compute statistics in y using PCE
    polynomial_order = 2
    polynomial_expansion = cp.generate_expansion(polynomial_order, p_x_prior)
    quadrature_order = polynomial_order * 2
    x_pce_s, w_pce_s = cp.generate_quadrature(quadrature_order, p_x_prior, rule="gauss_legendre")
    evaluations = [exponent(x_pce, eps=noize_level) for x_pce in x_pce_s.T]
    pce_approx = cp.fit_quadrature(polynomial_expansion, x_pce_s, w_pce_s, evaluations)
    y_mean_pce = cp.E(pce_approx, p_x_prior)  # known to be slow in ChaosPy!
    y_std_pce = cp.Var(pce_approx, p_x_prior)
    print("PCE     E(y) = {:.4} ; Var(y) = {:.4} ; number of evaluations: {} ".
          format(y_mean_pce[0], y_std_pce[0], (polynomial_order+1)**2))

    # --- Tests for analytical results for simple p(x) and p(y|x)
    #p_y_test = np.array([test_gaussian_integral(y, x_domain, s) for [y,s] in zip(y_mean_test, y_mean_std)])
    #plot_1d_distrib(np.linspace(*y_domain, 21), p_y_test, names={'y': 'gauassian integral', 'x': 'x'})

    y_mean_an = exp_mean([x_bin_edges[0], x_bin_edges[-1]])
    y_var_an = exp_var([x_bin_edges[0], x_bin_edges[-1]])
    print(f'An. deter-ic E(y) = {y_mean_an:.4} ; Var(y) = {y_var_an:.4}')

    assert abs(E_y - y_mean_an)/y_mean_an < 1e-3

    assert abs(Var_y - y_var_an)/y_mean_an < 1e-4

    # ---- Compute posterior

    start_post_calc = t.time()
    p_x_y, c_y = bayes.compute_x_y_posterior(p_x_prior, surrogate, x_bin_edges, y_bin_edges)
    end_post_calc = t.time()
    print('posterior calculation took {:2.3} s'.format(end_post_calc - start_post_calc))

    #debugging
    #print(np.where(abs(c_y) < 1e-9))
    #print(np.where(p_x_y < 1e-9))
    #highpos_pxy = np.where(p_x_y > 1e2)
    #print([(x,y) for x,y in zip(highpos_pxy[0], highpos_pxy[1])])

    # --- Assume desirable prior y distribution
    # TODO test range of parameters that influence the marginal x variance: training data noize, y prior variance, ...
    # TODO position of y mean (resulting variance will be higher for more gentle slope of model function

    x_trues = [0.0, 0.75, 2.5]
    y_trues = [exponent(x_true) for x_true in x_trues]
    #print("x true : {:.3} \ny true : {:.3}".format(x_true, y_true))

    y_p_sigmas = [0.005, 0.025, 0.1]
    for (x_true, y_p_sigma) in product(x_trues, y_p_sigmas):
        y_true = exponent(x_true)[0]
        p_y = cp.Normal(y_true, y_p_sigma)
        start_marg_calc = t.time()
        p_x = bayes.compute_x_distribution(p_y, p_x_y, x_bin_edges, y_bin_edges, n_samples=1_000_000)  #TODO (important) p(x) accumulates suspicious samples in the end of x support
        end_marg_calc = t.time()
        print('marginal x distribution calculation took {:2.3} s'.format(end_marg_calc - start_marg_calc))
        combined_plot(x_bin_edges, y_bin_edges, p_x_y, surrogate, p_x_prior, p_x, c_y, p_y,
                      X_train=X_train,
                      y_train=y_train,
                      x_true=x_true,
                      y_true=y_true,
                      y_s=y_p_sigma)
