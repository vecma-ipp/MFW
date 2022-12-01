import os
import numpy as np
import pandas as pd
import chaospy as cp
import time as t

import glob
import ast
import itertools

import matplotlib
#matplotlib.use('Agg')
import matplotlib.pyplot as plt
params = { #"text.usetex": True,
          'axes.labelsize': 10,
          'axes.titlesize': 11.0,
          'xtick.labelsize': 8,
          'ytick.labelsize': 8,
          'axes.titlepad': 3,
          'axes.labelpad': .7,
          'legend.fontsize': 9,
          'legend.handlelength': 1.2}
plt.rcParams.update(params)
import matplotlib.tri as mtri

from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.axes_grid1 import make_axes_locatable

from sklearn.neighbors import KernelDensity

#from extcodehelper import ExtCodeHelper
#import lhsmdu
from shutil import copyfileobj
import re

def exponential_model(x, theta=[1.0, 1.0, 1.0]):
    """
    :param theta: paramters of size (n+1,1); A = theta[0] and theta[1:] are rates r
    :param x: coordinates of size (n,1)
    :return: A*exp(-(r,x))
    """
    return theta[0]*np.e**(-np.dot(x, np.array(theta[1:])))

def exponential_model_sp(xs, a, k, l, s, b, e=np.e):
    """
    Expoential model with signature fit for curve_fit at scipy
    """
    return b + a*e**(-xs[:,0]*k-xs[:,1]*l)

def linear_model_sp(xs, a, k, l,):
    return a + xs[:,0]*k + xs[:,1]*l

def cossin_model(x, theta):
    return np.cos(x*theta[0])*np.sin(x*theta[1])

def walklevel(some_dir, level=1):
    """
    function to iterate over files of a subfolder of selected level of nesting
    :param some_dir:
    :param level:
    :return:
    """
    some_dir = some_dir.rstrip(os.path.sep)
    assert os.path.isdir(some_dir)
    num_sep = some_dir.count(os.path.sep)
    for root, dirs, files in os.walk(some_dir):
        yield root, dirs, files
        num_sep_this = root.count(os.path.sep)
        if num_sep + level <= num_sep_this:
            del dirs[:]

def write_gem0_offline(n_samples=1000, n_dim=4, filename='gem0_lhc_res.csv'):
    # Get data from GEM0 for offline training

    st = t.time()
    gem0_helper = ExtCodeHelper(1)
    print(f'time to create extcodehelper: {t.time()-st}')

    if n_dim == 4:
        function = lambda x: np.array(gem0_helper.gem0_call_4param2target_array(x))
        x_param = [[500., 2400, 16], [500., 2400, 16], [-5000., -1000., 16], [-5000., -1000., 16]]
    elif n_dim == 2:
        function = lambda x: np.array(gem0_helper.gem0_call_tefltegradtigrad_array(x))
        x_param = [[-5000., -1000., 16], [-5000., -1000., 16]]

    st = t.time()
    x_domain = lhsmdu.sample(n_dim, n_samples).reshape(-1, n_dim)
    for dim in range(len(x_param)):
        x_domain[:, dim] = x_param[dim][0] + x_domain[:, dim] * (x_param[dim][1] - x_param[dim][0])
    print(f'time to create LHC-S: {t.time() - st}')

    x_domain = np.array(x_domain)

    y_test = function(x_domain)
    y_test = y_test.reshape(y_test.shape[0], y_test.shape[-1])

    df = pd.DataFrame()

    if n_dim == 4:
        df['te.value'] = x_domain[:, 0]
        df['ti.value'] = x_domain[:, 1]
        df['te.ddrho'] = x_domain[:, 2]
        df['ti.ddrho'] = x_domain[:, 3]
        df['te.flux'] = y_test[:, 0]
        df['ti.flux'] = y_test[:, 1]
    elif n_dim == 2:
        df['te.ddrho'] = x_domain[:, 0]
        df['ti.ddrho'] = x_domain[:, 1]
        df['ti.flux'] = y_test[:, 0]

    df.to_csv(filename)

def remove_header_spaces_csv(filename='AUG_gem0_inoutput_v3.txt', DATADIR='MFW"\\uq\\data\\', WORKDIR="c:\\Users\\user\\Documents\\UNI\\MPIPP\\PHD\\code\\"):
    with open(WORKDIR + DATADIR + filename, 'r', newline='') as inputfile:
        with open(WORKDIR + DATADIR + filename + '.tmp', 'w', newline='') as outputfile:
            firstline = inputfile.readline()
            newfirstline = ' '.join(firstline.split())
            outputfile.write(newfirstline)
            copyfileobj(inputfile, outputfile)

def load_wf_csv_file(data_dir = "c:\\Users\\user\\Documents\\UNI\\MPIPP\\PHD\\cod\\Fusion_Inputs\\UQ_GEM_Data\\runs\\",
                     input_file='AUG_gem_inoutput.txt',
                     Xlabels = ['Te-ft5', 'Ti-ft5', 'dTe-ft5', 'dTi-ft5'],
                     Ylabels = ['flux-Te-ft5', 'flux-Ti-ft5']
                     ):

    # Xlabels = ['Unnamed: 5', 'Unnamed: 13', 'Unnamed: 21', 'Unnamed: 29']
    # Ylabels = ['Unnamed: 62', 'Unnamed: 70']

    df = pd.read_csv(data_dir + '\\' + input_file, delimiter=' ', header='infer')

    input_samples = df[Xlabels]
    output_samples = df[Ylabels]

    time = df['time']

    # print(input_samples.head())
    # print(output_samples.head())

    # return input_samples.to_numpy()[:, 1:], output_samples.to_numpy()[:]
    return input_samples, output_samples, time

def read_data_totensor(folder):
    Tes, Tis, Tegs, Tigs, Tefs, Tifs = get_camp_data(folder)
    X = pd.DataFrame(list(zip(Tes, Tis, Tigs, Tegs)))
    Xmat = np.array([np.unique(X[:, i]) for i in range(X.shape[1])]) 
    return X, Xmat

#def get_slice(Xmat, n_slice = [3]):
#     Xsl = np.array([i for i in itertools.product([],[],[],np.linspace(Xmat))

def read_sim_csv(input_filename):
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    Ylabels = ['te_transp_flux', 'te_transp_flux']
    df = pd.read_csv(input_filename)
    X = df[Xlabels]
    Y = df[Ylabels]
    #return X, Y
    return df, X, Y

def grid_slice(data, retinds):
    """
    :param data: nupy array (n_features, n_samples)
    :param retinds: features which should vary in a resulting slice
    :return:
        indices of datset which correspond to middle slice along the retinds
    """
    # get the unique values of the datapoints at grid
    xvals = np.array([np.unique(data[:, i]) for i in range(data.shape[1])])

    # get the index of the central element of the grid
    midind = xvals.shape[1] // 2 + 1

    # get the indices of dimensions that you don't need to return
    sliceinds = [x for x in np.arange(data.shape[1]) if x not in retinds]

    # start with all the rows of the dataset (all the points at grid)
    rowinds = np.array([True for x in np.arange(data.shape[0])])  # TODO: initialize in a quicker way

    # exclude the points that don't lie on the central slices
    for sliceind in sliceinds:
        rowinds = np.logical_and(rowinds,
                                 (abs(data[:, sliceind] - xvals[sliceind, midind]) < 1e-12))

    rowinds = np.where(rowinds)[0]

    return rowinds

def plot_camp_vals(data, name='gem0'):
    """
    Plots input and output values against run number
    :argument data: pandas dataframe with features and results of the campaign, result of read_sim_csv()
    """
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    Ylabels = ['te_transp_flux', 'ti_transp_flux']
    # Ylabels = ['te_transp_flux']
    data.reset_index()

    data[Xlabels].plot(style='o')
    plt.savefig(name + '_camp_par_vals.png')
    plt.close()

    data[Ylabels].plot(style='o', logy=True)
    plt.savefig(name + '_camp_res_vals.png')
    plt.close()

def plot_2d_map(df, X, Y, inds=[2,3]):
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    Ylabels = ['te_transp_flux', 'te_transp_flux']

    #XYpv = pd.pivot_table(pd.concat([X, Y], axis=1, sort=False), 
    #                    index=Xlabels[inds[0]], 
    #                    columns=Xlabels[inds[1]], 
    #                    values=Ylabels[1])

    x1lab = Xlabels[inds[0]]
    x2lab = Xlabels[inds[1]]
    ylab = Ylabels[1]

    #fig, ax = plt.subplots()
    #ax.pcolormesh(XYpv, cmap=plt.cm.Reds, alpha=1)
    #plt.pcolormesh(X.loc[ :, [Xlabels[inds[0]], Xlabels[inds[1]]] ], 
    #               Y.loc[ :, [Ylabels[1]] ])
    ax = df.plot.scatter(x = x1lab,
                    y = x2lab,
                    c = ylab,
                    colormap = 'viridis',
                    norm = matplotlib.colors.LogNorm())
    #ax.set_scale('log')
    #for row in df.iterrows():
    #    ax.annotate(row[ylab], (row[x1lab].value, row[x2lab].value))
    plt.savefig("plot2d_" + x1lab + "_" + x2lab + ".png")
    plt.close()

def plot_3d_wire(df, xinds=[2,3], yind=1):
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho'] # make global / part of class/ passable
    Ylabels = ['te_transp_flux', 'ti_transp_flux']

    x1lab = Xlabels[xinds[0]]
    x2lab = Xlabels[xinds[1]]
    ylab = Ylabels[yind]
   
    X = df[x1lab].unique() # check if later ok
    Y = df[x2lab].unique()
    X, Y = np.meshgrid(X, Y)
    Z = np.array(df[ylab]).reshape(X.shape[0], Y.shape[0], X.shape[0], Y.shape[0]) # check if shapes are universal
    Z = Z[:, :, Z.shape[2]//5, Z.shape[3]//5] # take only central values, think of taking means, interpolating

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    #ax.plot_wireframe(X, Y, Z, cmap='viridis')
    ax.scatter3D(X, Y, Z, cmap='viridis')
    #ax.zaxis._set_scale('log')
    ax.set_xlabel(x1lab)
    ax.set_ylabel(x2lab)
    ax.set_zlabel(ylab)
    plt.savefig("plot3d_" + x1lab + "_" + x2lab + "_" + ylab + "_mid_wf.png")
    plt.close()

def plot_3d_suraface(x ,y, z, name):
    """
    Prints a surface for function f:X*Y->Z arbitrary list of coordinates
    i.e. f(x[i],y[i])=z[i] using triangulated mesh at X*Y
    :param x: list of coordinates in X
    :param y: list of coordinates in Y
    :param z: list of results f(x, y) in Z
    :param name: name of function, to be used in plot file name
    :return:
    """
    triang = mtri.Triangulation(x, y)
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_trisurf(triang, z, cmap='viridis')
    ax.scatter(x, y, z, marker='.', alpha=0.3)
    ax.view_init(elev=60, azim=-45)
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('F')
    plt.savefig('3dmeshplot_' + name + '.png')
    plt.show()
    plt.close()

def plot_model_response(n_points=128, a_interval=[0., 10.], b_interval=[0., 10.],
                        function=exponential_model, name='exp', x_value=1.):
    """
    Plots a surface of a function as f(x|a,b):A*B->Z on the given rectangular region of A*B for given x
    :param n_points: number of samples in A*B
    :param a_interval: interval of interest in A
    :param b_interval: interval of interest in B
    :param function: function for pointwise mapping f
    :param name: name of function, for choosing and for naming files
    :param: x_value: value of coordinate in X
    :return:
    """
    func_dict = {'cossin': (cossin_model, [[0.05, 5.0], [0.05, 5.0]], 1.),
                 'exp': (exponential_model, [[0, 10.], [0., 10.]], 1.),
                 }

    #if name == 'cossin':
    #    x_value = 1.0
    #    n_points = 128
    #    a_interval = [0.05, 5.0]
    #    b_interval = [0.05, 5.0]
    #    function = cossin_model
    #elif name == 'exp':
    #    pass

    (function, [a_interval, b_interval], x_value) = func_dict[name]

    a = np.random.rand(n_points) * (a_interval[1] - a_interval[0]) + a_interval[0]
    b = np.random.rand(n_points) * (b_interval[1] - b_interval[0]) + b_interval[0]
    ab = np.dstack((a, b))[0]
    z = np.array([function(x_value, par_value) for par_value in ab]).reshape(-1,)

    plot_3d_suraface(a, b, z, function.__name__)

def plot_mult_lines(x, y, params, name):
    """
    Plots mutiple realisations for functions of type f(x|a,b):X->Y
    for different values of (a, b)
    :param x: list of lists coordinates in X
    :param y: list of lists results in Y
    :param params: list of parameter realisations
    :param name: name of function
    :return:
    """
    fig = plt.figure()
    #ax = fig.add_subplot(111, projection='2d')
    ax = fig.add_subplot(111)

    for p in range(len(params)):
        plt.plot(x[p], y[p], label=str(params[p]))

    ax.set_xlabel('X')
    ax.set_ylabel('F(x)')
    plt.legend()
    plt.title('Graphs for F(x) for multiple values of parameters')
    plt.savefig('shoot_' + name + '.png')
    plt.close()

def plot_distr(dist=cp.J(cp.Uniform(0.8, 1.2), cp.Uniform(0.8, 1.2))):
    """
    Create contour plot for the probability density function f: X1xX2->[0;1]
    """
    grid_init, grid_rate = grid = np.meshgrid(np.linspace(0.8, 1.2, 32), np.linspace(0.8, 1.2, 32))
    contour = plt.contourf(grid_init, grid_rate, dist.pdf(grid), levels=20, cmap="gray", alpha=0.3)
    colorbar = plt.colorbar(contour)
    # Create scatter plot for 50 random samples
    np.random.seed(123)
    samples = dist.sample(50)
    plt.scatter(*samples, marker="x", color="k")
    # Make figure pretty
    plt.axis([0.5, 1.5, 0.5, 1.5])
    plt.xlabel("omega_a (uniform)")
    plt.ylabel("omega_b (uniform)")
    colorbar.ax.set_ylabel("Joint probability density")
    plt.savefig('dist' + '.png')
    plt.close()

def plot_uncertainties(f, E, Std, X, K, N):
    """
    Plots a mean and shaded 1-sigma interval fun a f(X)
    # TODO reduce, this is a simpler version of function of plot_prediction_variance()
    :param f: function object
    :param E: expectation for function value
    :param Std: standart deviation of function value
    :param X: domain (roi) of function
    :param K: number of expansion terms (for naming)
    :param N: number of basis polynomials (for naming)
    :return:
    """
    plt.xlabel("x")
    plt.ylabel("approximation")
    #plt.axis([0, 5, -1, 1])
    plt.fill_between(X, E-Std, E+Std, alpha=0.25, color="r")
    plt.plot(X, E, "r-")
    plt.savefig('toy' + '_cos_sin' + '_var_' + str(K) + '_' + str(N) +'.png')
    plt.close()

def plot_convergence(sample_sizes, errors_mean, errors_variance):
    """
    Plots a dependency of UQ error in E and Var from the size of training dataset
    # TODO do not need it
    :param sample_sizes:
    :param errors_mean:
    :param errors_variance:
    :return:
    """
    # Error plot for mean
    plt.loglog(sample_sizes, errors_mean, "ko-", label="mean")
    # Error plot for variance
    plt.loglog(sample_sizes, errors_variance, "ko--", label="variance")
    #plt.axis([min(sample_sizes), max(sample_sizes), 1e-16, 1e-2])
    plt.xlabel("N samples")
    plt.ylabel("MSE")
    plt.legend()
    plt.savefig('toy' + '_gem0' + '_convergence' + '.png')
    plt.close()

def plot_response_1d(x_domain, y_tests, ylabels=['1'], f = lambda x : x):
    """
    Plot a function y(x) for any R->R response for a code
    # TODO simple, may be just delete
    :param x_domain:
    :param y_tests:
    :param ylabels:
    :param f:
    :return:
    """
    wrt_dir = os.path.join(os.environ['PWD'])
    plt.figure()
    #y_test = f(x_domain)
    for i in range(len(y_tests)):
        plt.plot(x_domain, y_tests[i], 'r:', label='GEM0') #r'$f(x) = x\,\sin(x)$')
        plt.ylabel(ylabels[i])
    plt.legend()
    plt.xlabel(r'$\nabla Te$')
    plt.savefig(os.path.join(wrt_dir, 'response' + str(len(y_tests))+'.png'))  #TODO save img in current folder

def plot_prediction_variance(x_observ, y_observ, x_domain, y_test, y_pred, sigma, x_newpoints=[], y_newpoints=[], rmse=0.0, funcname='e^-x cos x', dy=0):
    """ 
    Plots prediction and 95% confidence interval for a R->R responce and its model
    :param x_observ: domain values for function evaluations
    :param y_observ: function evaluation values
    :param x_domain: values of domain (RoI)
    :param y_test: function values for points at x_domain
    :param y_pred: function values prediceted by model
    :param sigma: std for model predictions
    :param f: true function
    :param: x_newpoints: a point in space domain that would be added to training set in the NEXT optimisation iteration
    """

    #wrt_dir = os.path.join(os.environ['PWD'], 'locmaxsamp')
    wrt_dir = ''

    #plot_response_1d(x_domain, y_test, f)

    plt.errorbar(x_observ, y_observ, dy, fmt='r.', markersize=10, label='Simulation')
    plt.plot(x_domain, y_pred, 'b-', label='Prediction')
    plt.fill(np.concatenate([x_domain, x_domain[::-1]]),
             np.concatenate([y_pred - 1.9600 * sigma,
                             (y_pred + 1.9600 * sigma)[::-1]]),
             alpha=.5, fc='b', ec='None', label='95% confidence interval')
    #plt.vlines(x_choice, -1, 1, colors='k', alpha=0.5, linestyles='dashed', label='new opt choice')
    if len(x_newpoints) != 0:
        plt.plot(x_newpoints, y_newpoints, 'go', markersize=12, label='new samples')
    plt.xlabel(r'$\nabla Ti$')
    plt.ylabel('$Ti\_flux$')
    plt.title('GPR results for ' + funcname + ' for ' + str(len(y_observ)) + ' number of function evaluations')
               # + 'PRediction RMSE is ' + str(rmse))
    plt.legend(loc='upper right')
    #plt.show(block=True)
    plt.savefig(os.path.join(wrt_dir, 'surr_gem0_tfluxtiddrho_' + str(len(y_observ)) + '.png'))
    plt.close()
    #return y_test.T.reshape(-1)

def plot_prediction_withtime(x_observ, y_observ, x_domain, y_test, y_pred, sigma, x_newpoints=[], y_newpoints=[],  rmse=0.0, funcname='e^-x cos x', dy=0):
    """
    Plots prediction and 95% confidence interval for multiple, gradually improving, model versions
    :param x_observ: list of domain values for function evaluations
    :param y_observ: list of function evaluation values
    :param x_domain: values of domain (RoI)
    :param y_test: function values for points at x_domain
    :param y_pred: list of function values predicted by model
    :param sigma: list of std for model predictions
    :param f: true function
    :param x_newpoints: a point in space domain that would be added to training set in the NEXT optimisation iteration
    """

    wrt_dir = ''
    n = len(y_pred)
    xscale = float(1000)
    yscale = float(1000)
    #yax_lims = np.array([-7e+3, 2.4e+5])/yscale
    yax_lims = np.array([-7e+3, 2.4e+5]) / yscale

    plt.close()
    plt.clf()
    fig, ax = plt.subplots()

    cmap_newpoints = plt.get_cmap('Greens')
    cmpap_sims = plt.get_cmap('Blues')
    cmap_test = plt.get_cmap('Reds')

    ax.scatter(x_observ[0]/xscale, y_observ[0]/yscale, c='g', s=8)

    for i in range(0, n-1):

        #ax.errorbar(x_observ[i].ravel(), y_observ[i], dy, fmt='.', c='r', markersize=10)  #label='Simulation')

        #ax.plot(x_domain, y_pred[i], 'b-', alpha=((i+1)/float(n)))  #, label='Prediction')

        ax.fill(np.concatenate([x_domain/xscale, x_domain[::-1]/xscale]),
                np.concatenate([y_pred[i]/yscale - (1.9600/yscale) * sigma[i].reshape(-1,1),
                               (y_pred[i]/yscale + (1.9600/yscale) * sigma[i].reshape(-1,1))[::-1]]),
                alpha=.07, fc='b', ec='None', label="_nolegend_")  #, label='95% confidence interval')

        if i > 0:
            if len(x_newpoints[i-1]) != 0:
                ax.plot(np.array(x_newpoints[i-1])/xscale, y_newpoints[i-1]/yscale,
                        'o', c='g', markersize=9*(i+6)/float(n+6), alpha=1.0)  # label='order of sampling')
            #ax.annotate('{}'.format(i+len(y_observ[0])), (np.array(x_newpoints[i-1])/xscale, y_newpoints[i-1]))

    # plot training points and confidence interval for last optimization iteration
    #ax.errorbar(x_observ[n-1], y_observ[n-1], dy, fmt='g.', markersize=5)  # label='Simulations')

    ax.plot(x_domain/xscale, y_pred[n-1]/yscale, 'b-', alpha=(n/float(n)), label='Prediction Mean')

    ax.fill(np.concatenate([x_domain/xscale, x_domain[::-1]/xscale]),
            np.concatenate([y_pred[n-1]/yscale - (1.9600/yscale) * sigma[n-1].reshape(-1,1),
                           (y_pred[n-1]/yscale + (1.9600/yscale) * sigma[n-1].reshape(-1,1))[::-1]]),
            alpha=.07, fc='b', ec='None', label='95% confidence interval')

    # plot the last training point of optimization, add plot labels
    if len(x_newpoints[n-2]) != 0:
        ax.plot(np.array(x_newpoints[n-2])/xscale, y_newpoints[n-2]/yscale, 'o', c='g', markersize=9,
                alpha=1.0, label='Training Simulations')
        #ax.annotate('{}'.format(n+len(y_observ[0]), (np.array(x_newpoints[i-1])/xscale, y_newpoints[i-1]))

    ax.plot(x_domain/xscale, y_test/yscale, 'r.', label='Testing Simulations')

    ax.set_xlabel(r'$ \nabla Te \; [keV/m] $')
    ax.set_ylabel('$ Te\_flux \; [kW/m^{2}] $')
    ax.set_title('GPR results for ' + funcname + ' for ' + str(len(y_observ[-2])) + ' training simulations', pad=10)
    # + 'PRediction RMSE is ' + str(rmse))
    ax.legend(loc='upper right')
    ax.set_ylim(yax_lims)
    ax.ticklabel_format(style='sci', useMathText=True)
    plt.savefig(os.path.join(wrt_dir, 'TE_surrogate_gem0_teflux_teddrho_total_' + str(len(y_observ[-2])) + '.pdf'))
    plt.close()

def add_density_x(x, range=[200, 400]):
    """
    Gets a sample of x_vec (x e X) calculates it KDE density at the domain and add it to the plot of response X->Y
    :param x:
    :param range:
    :return:
    """
    xinds = np.arange(200, 400)
    dat = x[xinds, np.newaxis]
    x_domain = np.linspace(dat.min(), dat.max(), 1000)[:, np.newaxis]

    kde = KernelDensity(kernel='gaussian', bandwidth=150.).fit(dat)
    log_dens = kde.score_samples(x_domain[:, 0].reshape(-1, 1))

    plt.fill(np.concatenate([x_domain, x_domain[::-1]]),
              np.concatenate([np.zeros(len(log_dens)), 100*np.exp(log_dens)[::-1]]))

def plot_prediction_variance_2d(x_observ, y_observ, x_domain, y_test, y_pred, sigma, newpoints, funcname):

    #wrt_dir = os.path.join(os.environ['SCRATCH'], 'outputs/plots/res_debug')
    #wrt_dir = os.path.join(os.environ['PWD'], 'locmaxsamp')
    wrt_dir = "surr2d"

    cmap_val = 'viridis' #"RdBu_r"
    xlabel_val = r'$\nabla T_{e}$'
    ylabel_val = r'$\nabla T_{i}$'

    # Plot function,prediction and 95% confidence interval
    x1o = x_observ[:,0]
    x2o = x_observ[:,1]
    x1i = x_domain[:,0]
    x2i = x_domain[:,1]

    y_test = y_test.reshape(-1,)
    y_pred = y_pred.reshape(-1,)
    y_observ = y_observ.reshape(-1,)
    
    fig, (ax1, ax2, ax3) = plt.subplots(nrows=1, ncols=3, figsize=(9, 3.7))

    ### --- First plot for response function
    cntr1 = ax1.tricontourf(x1i, x2i, y_test, levels=12, cmap=cmap_val)

    divider1 = make_axes_locatable(ax1)
    cax1 = divider1.append_axes("right", size="8%", pad=0.1)

    cbar1 = fig.colorbar(cntr1, cax1)

    ax1.set_title('Ground truth Ti flux response', pad=10.0)
    ax1.set_xlabel(xlabel_val)   #(r'$Te$')
    ax1.set_ylabel(ylabel_val)   #(r'$\nabla Te$')
    #cbar1.set_label(r'$Q_{T_{e}}$')
    ax1.set_aspect('equal')

    ### --- Second plot for GPR mean
    cntr2 = ax2.tricontourf(x1i, x2i, y_pred, levels=12, cmap=cmap_val)
    ax2.scatter(x1o, x2o, c=y_observ, cmap=cmap_val, edgecolors='k', s=12)
    divider2 = make_axes_locatable(ax2)
    cax2 = divider2.append_axes("right", size="8%", pad=0.1)
    cbar2 = fig.colorbar(cntr2, cax2, boundaries=[0, 180000])
    #ax2.set_title('GPR results for f=(' + funcname + ') with ' + str(len(y_observ)) + ' # func. eval-s')
    ax2.set_title('Ti flux prediction for training set of {} '.format(len(y_observ)), pad=10.0)
    ax2.set_xlabel(xlabel_val)   #(r'$Te$')
    ax2.set_ylabel(ylabel_val)   #(r'$\nabla Te$')
    ax2.set_aspect('equal')
    if len(newpoints) != 0:  #TODO fix two different scatter plots ; fix scatter plots margin iside the figure box
        ax2.scatter(newpoints[0][:,0], newpoints[0][:,1], c=newpoints[1][:,0], edgecolors='g', s=14) #, label='new samples')

    ### --- Third plot for the neg-utility (GPR STD)
    cntr3 = ax3.tricontourf(x1i, x2i, sigma, levels=14, cmap=cmap_val)
    divider3 = make_axes_locatable(ax3)
    cax3 = divider3.append_axes("right", size="8%", pad=0.1)
    cbar3 = fig.colorbar(cntr3, cax3, boundaries=[0, 20000])
    #ax2.plot(x, y, 'ko', ms=3)
    ax3.set_title('Surrogate uncertainty (st.dev.)', pad=10.0)  #(r'GPR $\sigma$')
    ax3.set_xlabel(xlabel_val)  #(r'$Te$')
    ax3.set_ylabel(ylabel_val)  #(r'$\nabla Te$')
    ax3.set_aspect('equal')

    ################################
    #plt.plot(x_domain, y_test, 'r:', label='e-|x|^2 * cos(|x|^2)') #r'$f(x) = \cos(ax)\,\sin(bx)$')
    #plt.legend(loc='upper right')

    plt.tight_layout()
    plt.subplots_adjust()  # TODO should have less margin at saved figure
    plt.savefig(os.path.join(wrt_dir, 'Ti_surr2d_gem0_' + str(len(y_observ))+'.png'), dpi=1250)
    plt.close()

def plot_error(err, name):
    #wrt_dir = os.path.join(os.environ['SCRATCH'], 'outputs/plots/res128') # wrt_dir = ''
    #wrt_dir = os.path.join(os.environ['PWD'] , 'locmaxsamp')
    wrt_dir = ""
    plt.semilogy(range(1, len(err) + 1), err, label=name)
    #plt.yscale("log")
    plt.xlabel('n. interations')
    plt.ylabel('error')
    plt.title('Error of GPR surrogate predictions at function evaluations')
    plt.savefig(os.path.join(wrt_dir, 'TEsurr_gem0_err_' + name + '.png'))
    plt.close()

def plot_histograms(y_orig, y_orig_clean, y_pred, y_pred_clean):
    wrt_dir = os.path.join(os.environ['SCRATCH'], 'outputs/plots/res_debug')
    wrt_dir = os.path.join(os.environ['PWD'], 'locmaxsamp')
    
    plt.hist(y_orig, bins=len(y_orig)//10)
    plt.savefig(os.path.join(wrt_dir, 'y_orig.png'))
    plt.close()

    plt.hist(y_pred, bins=24)
    plt.savefig(os.path.join(wrt_dir, 'y_pred.png'))
    plt.close()

    plt.hist(y_orig_clean, bins=len(y_orig)//10)
    plt.savefig(os.path.join(wrt_dir, 'y_orig_cl.png'))
    plt.close()

    plt.hist(y_pred_clean, bins=24)
    plt.savefig(os.path.join(wrt_dir, 'y_pred_cl.png'))
    plt.close()

def plot_sobols_pie(sobol_ind_vals, labels, name=''):
    """
    Saves PNG pie charts representing Sobol indices as partition of unity 
    for paramaters names with labels
    """
    explode = [.0,]*len(sobol_ind_vals)
    #explode[np.argmax(sobol_ind_vals)] = 0.1 #comments disables highlighting of the largest fraction
    
    fig1, ax1 = plt.subplots(figsize=(10,10))
    
    ps,ts = ax1.pie(sobol_ind_vals,
             explode=explode,
             #labels=labels,
             #autopct='%.2f%%',
             #labeldistance=1.05,
             #pctdistance=1.0,
             shadow=False, 
             startangle=0, 
             textprops={"fontsize":18})

    labels_long = ['{0} - {1:1.2f}%'.format(l,100.*v) for l,v in zip(labels, sobol_ind_vals)]

    ax1.legend(ps, labels_long, loc='center left', fontsize=18)

    plt.savefig('sobols_pie_'+name+'.png')
    plt.close()

def read_sobols_from_logs(labels=['te_val', 'ti_val', 'te_grad', 'ti_grad']):

    # read sobol values in a dict    
    list_log_filenames = glob.glob('test*log*')
    # TODO: actually some log may contain garbage values

    sob_list_list = []

    for log in list_log_filenames:
        with open(log, 'r') as input:

            sob_dict = {}
            ti_transp = False

            for line in input:

                # find if in ti_transp
                if 'TI TRANSP FLUX' in line:
                    ti_transp = True

                # find Sobols
                if ti_transp and 'Sob1' in line:
                    line = line.replace('array([', '')
                    line = line.replace('])', '')
                    line = line[7:-1]
                    #line = "\""+line[7:-1]+"\"" 

                    # read sobol values in a dict
                    sob_dict = ast.literal_eval(line)

                    # put values in list(of logs) of lists(of values)
                    sob_list = list(sob_dict.values())

                    break

            if ti_transp:
                sob_list_list.append(sob_list)

    return sob_list_list

def merge_result_csv(file_list, output_name='resuq_extended'):
    """
    Takes a list of filenames of CSVs with turbulence code results and saves a CSV which is a combination of all runs
    """

    df = pd.concat([pd.read_csv(f, delimiter=',', engine='python') for f in file_list])

    df.to_csv(output_name + '.csv')

def produce_stats_dataframes(runs_input_vals, val_trend_avg_s, val_std_s, stats_df, scan_df, 
                             n_lensample=1, runn=0, p='ti_transp', a='flux'):
    """
    Composed pandas dataframes with code input/output and its statistics with single entry for a code run
         Parameters:
             
         Returns: 
             pandas DataFrame with rows - runs for different cases; columns - code io and stats
    """
    if n_lensample==1:
        n_lensample = val_trend_avg_s[runn-1].shape[-1]
   
    #print('acf-corrected sample length: {0}'.format(n_lensample)) ###DEBUG

    # stats_df = stats_df.append(#(stats_df,
    #                       pd.Series(
    #                            data={'mean': val_trend_avg_s[runn-1][0][0],
    #                                  'std': val_std_s[runn-1][0][0]},
    #                            name=runn-1)
    #                      #), axis=1
    #                     ) # probably a bad workaround

    stats_df = pd.concat([
                        stats_df,
                        pd.Series(
                            data={'mean': val_trend_avg_s[runn-1][0][0],
                                  'std': val_std_s[runn-1][0][0]},
                            name=runn-1,
                                 ).to_frame().T,
                             ],
                         #axis=0,
                         #ignore_index=True,
                        )

    #print('stats_df: \n {0}'.format(stats_df)) ###DEBUG
    #print('stats_df_new: \n {0}'.format(stats_df_new)) ###DEBUG

    scan_data = runs_input_vals[runn-1]
    scan_data[p+'_'+a] = val_trend_avg_s[runn-1][0][0]
    scan_data[p+'_'+a+'_std'] = val_std_s[runn-1][0][0]
    scan_data[p+'_'+a+'_stem'] = scan_data[p+'_'+a+'_std'] / np.sqrt(n_lensample)
    scan_data[p+'_'+a+'_acn'] = n_lensample

    scan_data_new = {}
    for k,v in scan_data.items():
        scan_data_new[k.replace('.', '_')] = v

    # scan_df = scan_df.append(#(scan_df,
    #                      pd.Series(
    #                           data=scan_data_new,
    #                           name=runn-1,
    #                           #index=runn-1,
    #                               )
    #                     #), axis=1
    #                    )
    # #TODO: name is NaN; and index is saved in csv but not recognised in dataframe

    scan_df = pd.concat([
                        scan_df,
                        pd.Series(
                               data=scan_data_new,
                               name=runn-1,
                               #index={runn-1},
                                 ).to_frame().T,
                            ],
                         #axis=0,
                         #ignore_index=True,
                        )

    #print('scan_df: \n {0}'.format(scan_df)) ###DEBUG
    #print('scan_df_new: \n {0}'.format(scan_df_new)) ###DEBUG

    return scan_df, stats_df

def plot_response_cuts(data, input_names, output_names, compare_vals=None, foldname='', traces=None, hists=None):
    """
    Plot different cuts (fixing all input parameter values but one) of code response for given QoI
        Parameters:
            data: pandas dataframe with input and output values of code
                  note: pass means of the outputs
            input_names: list of stings with input names, corresponds to dataframe columns, 
                         also used for plot labels
            output_names: list of strings with output names, corresponds to dataframe columns,
                          also used for plot labels
            compare_vals: for a single QoI name - a tuple of type (mean, min value, max_value)
            foldname: string of original campaign id to destinguish plot names
            traces: list of lists with values over time for each cases described in 'data'
            hists: histograms to be plotteed in an array
    """

    # Define a set of styles for different lists plotted
    color_list = ['b', 'g', 'r', 'y' , 'm', 'c', 'k']
    line_list = ['-', '--', '-.', ':']
    marker_list = ['', '.', 'o', 'v', '^', '<', '>']
    style_lists = [marker_list, line_list, color_list,] 
    fmt_list = [style for style in itertools.product(*style_lists)]

    if len(input_names) == 1:
        # If the dimension is one there are no cuts, only a single plot

        axes = data.plot(x=input_names[0], 
                        y=output_names[0],
                        yerr=output_names[0]+'_stem',
                        kind='scatter', #'line',
                        style={'color': 'b', 'marker':'.', 'linestyle':''},
                        title='Response for a single argument',
                        xlabel=input_names[0],
                        ylabel=output_names[0]
                        )

        for i in range(len(data.index)):
            axes.annotate('{0:1.3f}'.format(data.at[i, output_names[0]+'_stem'] / data.at[i, output_names[0]]), 
                          (data.at[i, input_names[0]], data.at[i, output_names[0]]))

        axes.get_figure().savefig('scan_'+output_names[0]+'_'+foldname+'.svg')
        plt.close()                        

    else:

        n_inputs = len(input_names)
        n_points = len(data.index)
        n_points_perdim = int(n_points ** (1./n_inputs))
        n_fixvals = n_points_perdim ** (n_inputs - 1)
        n_plots = (n_inputs) * (n_fixvals)

        qoi_name = output_names[0] # TODO to make modifiable
        qoi_std_name = qoi_name + '_std'
        qoi_stem_name= qoi_name + '_stem'

        #print([n_inputs, n_points, n_points_perdim, n_fixvals, n_plots, qoi_name]) ###DEBUG
        #print('n_inputs={0}, n_points={1}, n_ppdim={2}, n_fv={3}, n_pl={4}'.format(n_inputs, n_points, n_points_perdim, n_fixvals, n_plots)) ###DEBUG
        
        # Get unique values for every input dimension
        input_vals_unique = np.array([np.unique(data[i]) for i in input_names])
        #print("input_vals_unique={}".format(input_vals_unique)) ###DEBUG
    
        # A generator for numbers for input dimension numbers
        input_inds = range(n_inputs)

        fig, ax = plt.subplots(n_inputs, n_fixvals, 
                            figsize=(100, 20)
                            )

        # if traces are passed, create same type of plot layout but with time traces 
        if traces is not None:
            fig_tr, ax_tr = plt.subplots(n_inputs, n_fixvals, 
                                figsize=(175, 25)
                                        )
        
        if hists is not None:
            fig_hs, ax_hs = plt.subplots(n_inputs, n_fixvals, 
                                figsize=(100, 20)
                                        )

        # Iterate over all the input dimensions, selecting single one as a running variable
        for i_ip in range(n_inputs):

            running_ip_name = input_names[i_ip]
            input_names_left = [n for n in input_names if n != running_ip_name]
            input_inds_left = [n for n in input_inds if n != i_ip]
            fixed_ip_names = ''.join([n+'&' for n in input_names_left])

            #offset = 1

            input_fixvals_unique = list(itertools.product(
                                        *[input_vals_unique[i,:].tolist() for i in input_inds_left]))
            
            #print([i_ip, running_ip_name, input_names_left, 
            #      input_inds_left, fixed_ip_names, input_fixvals_unique]) ###DEBUG

            #Create figure for combined scan, one for every input variable
            fig_loc, ax_loc = plt.subplots(1, 1, figsize=(7,7))

            # Iterate over the all combinations of input parameters values to be kept const for a cut
            for j_fixval in range(n_fixvals):

                # Choosing data for the cut 
        
                #pivot_fixed_val = data[input_names_left][j_fixval+offset]
                #tot_ind = data[input_names_left].where() #TODO location where element is equal to pivot_fixed_val

                #tot_ind = np.arange(j_fixval*offset,
                #                    (j_fixval+n_points_perdim-1)*offset,
                #                    offset).tolist()
                # TODO how to save symbolic expression for an array cut?
                
                input_fixvals = input_fixvals_unique[j_fixval]
                fixval_query = ''.join([n+'=='+str(v)+' & ' for (n,v) in zip(input_names_left, input_fixvals)])[:-3]
                # TODO query using abs(x-x*)<e_tol, or math.isclose() !           
    
                #print([tot_ind, input_fixvals, fixval_query]) ###DEBUG            

                data_slice = data.query(fixval_query)  #TODO FAILS IN THE LATEST PANDAS VERSION
                #TODO: probably a very slow way to access a DataFrame - still think how to offset with 3-nested loop

                # TODO: choose one of three ways: 
                #                      1) offsets - has to be 3 nested loops
                #                      2) filtering - inside current two loops, use np.unique() -> using
                #                      3) transform into ndarray of 4D, the iterate (look tuto notebook) - not general for different number of params
    
                #fixed_ip_vals      = data[input_names_left][tot_ind[0]] # TODO count tot_ind
                #fixed_ip_vals_str  = ''.join([str(v)+';' for v in fixed_ip_vals.to_list()]) 
                fixed_ip_val_str = ''.join([n+'='+str(round(v, 1))+';' for (n,v) in zip(input_names_left, input_fixvals)])

                x_io  = data_slice[running_ip_name].to_numpy()
                y_qoi = data_slice[qoi_name].to_numpy()
                y_std = data_slice[qoi_std_name].to_numpy()
                y_stem= data_slice[qoi_stem_name].to_numpy()
                inds  = data_slice.index.to_list()

                #print('inds : {0}'.format(inds)) ###DEBUG
    
                # Plotting part of iteration

                #ax[i_ip][j_fixval].plot(x_io, y_running_ip_nameqoi, 'o-', 
                #                   label='Response for ({})->({}) for {}'.
                #                   format(running_ip_name, qoi_name, fixed_ip_val_str))         
                
                ax[i_ip][j_fixval].errorbar(x_io, y_qoi, yerr=1.96*y_stem,
                                            #fmt='-o', 
                                            uplims=False, lolims=False,
                                            label='Response for ({})->({}) for {}'.
                                                format(running_ip_name, qoi_name, fixed_ip_val_str)
                                            )
        
                ax[i_ip][j_fixval].set_xlabel(r'{}'.format(running_ip_name))
                ax[i_ip][j_fixval].set_ylabel(r'{}'.format(qoi_name))
                ax[i_ip][j_fixval].set_title(r'{}'.format(fixed_ip_val_str), fontsize=10)
        
                #ax[i_ip][j_fixval].set_ylim(1.5E+6, 3.8E+6) # TODO make limits variable
                ax[i_ip][j_fixval].set_ylim(-5.E+5, 4.E+6)
                #ax[i_ip][j_fixval].legend(loc='best') 

                #Do the same for a combined plot
                ax_loc.errorbar(x_io, y_qoi, yerr=1.96*y_stem, 
                                            uplims=False, lolims=False,
                                            label=r'{}'.format(fixed_ip_val_str),
                                            alpha=0.5,
                                            color=fmt_list[j_fixval][2],
                                            linestyle=fmt_list[j_fixval][1],
                                            marker=fmt_list[j_fixval][0],
                               )


                # if y values (mean, min, max) to compare are passed, plot the horizontal lines
                if compare_vals is not None:
                    
                    ax[i_ip][j_fixval].hlines(y=compare_vals[0], xmin=x_io.min(), xmax=x_io.max(), color='r', linestyle='-' )
                    ax[i_ip][j_fixval].hlines(y=compare_vals[1], xmin=x_io.min(), xmax=x_io.max(), color='r', linestyle='--')
                    ax[i_ip][j_fixval].hlines(y=compare_vals[2], xmin=x_io.min(), xmax=x_io.max(), color='r', linestyle='--')

                    ax_loc.hlines(y=compare_vals[0], xmin=x_io.min(), xmax=x_io.max(), color='r', linestyle='-',  alpha=0.5)
                    ax_loc.hlines(y=compare_vals[1], xmin=x_io.min(), xmax=x_io.max(), color='r', linestyle='--', alpha=0.5)
                    ax_loc.hlines(y=compare_vals[2], xmin=x_io.min(), xmax=x_io.max(), color='r', linestyle='--', alpha=0.5)

                if traces is not None:

                    n = max([v.shape[-1] for v in traces])
                    ts = np.arange(n)
                     
                    for k in inds:

                        n_cur = len(traces[k][0])

                        x_io_val = data[running_ip_name].iloc[k]
                        x_io_val_str = '{0}={1}'.format(running_ip_name, x_io_val)

                        #print('trace passed to cut plot: {0}'.format(traces[k][:])) ###DEBUG
                        #print('traces have len= {0}'.format(n_cur)) ###DEBUG

                        ax_tr[i_ip][j_fixval].plot(np.arange(n_cur), traces[k][0][:],
                                                   #label='time trace for ({})->({}) for {}'.format(running_ip_name, qoi_name, fixed_ip_val_str),
                                                   label=r'{}'.format(x_io_val_str),
                                                  )

                        ax_tr[i_ip][j_fixval].set_xlabel(r't.st. for {0}, runs#{1}'.format(running_ip_name, inds))
                        ax_tr[i_ip][j_fixval].set_ylabel(r'{0}'.format(qoi_name))
                        ax_tr[i_ip][j_fixval].set_title(r'{0}'.format(fixed_ip_val_str), fontsize=12)
                        
                        ax_tr[i_ip][j_fixval].set_ylim(-5.E+5, 4.E+6)
                        ax_tr[i_ip][j_fixval].legend(loc='best', prop={'size':12})

                        if hists is not None:
                            
                            ax_hs[i_ip][j_fixval].hist(traces[k][0][:],
                                                       bins=n//64,
                                                       label=r'{}'.format(x_io_val_str),
                                                       alpha=0.75,
                                                      )

                            ax_hs[i_ip][j_fixval].set_xlabel(r'val-s for {0}, runs#{1}'.format(running_ip_name, inds))
                            ax_hs[i_ip][j_fixval].set_ylabel(r'#runs')
                            ax_hs[i_ip][j_fixval].set_title(r'{0}'.format(fixed_ip_val_str), fontsize=11)
                        
                            ax_hs[i_ip][j_fixval].legend(loc='best', prop={'size':11})
        
            #offset *= 2
            # Filter on dataframe could be completely replaced by an offseting for different chosen parameter        
    
            #Set and save scans combined by the input value
            ax_loc.set_xlabel(r'{}'.format(running_ip_name))
            ax_loc.set_ylabel(r'{}'.format(qoi_name))
            ax_loc.set_title(r'Scans for {0}'.format(running_ip_name))
            ax_loc.set_ylim(-1.E+5, 4.E+6)
            ax_loc.legend(loc='best',prop={'size':7}) #TEMP -probably an overload for a poster
            fig_loc.tight_layout()
            fig_loc.savefig('scan_comb_{0}_{1}.svg'.format(running_ip_name, foldname))
        
        fig.tight_layout()
        #fig.suptitle('GEM response in {} around profile values'.format(qoi_name)) #TODO: pass names as arguments    
        #fig.subplots_adjust(top=0.8)

        fig.savefig('scan_{0}_{1}.pdf'.format(qoi_name, foldname))

        if traces is not None:
            fig_tr.tight_layout()
            fig_tr.savefig('scan_traces_{0}_{1}.pdf'.format(qoi_name, foldname))
        
        if hists is not None:
            fig_hs.tight_layout()
            fig_hs.savefig('scan_hists_{0}_{1}.pdf'.format(qoi_name, foldname))
            
        plt.close()
