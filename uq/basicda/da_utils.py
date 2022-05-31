import os
import numpy as np
import pandas as pd
import chaospy as cp
import time as t

import glob
import ast

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
    cntr1 = ax1.tricontourf(x1i, x2i, y_test, levels=12, cmap="RdBu_r")
    divider1 = make_axes_locatable(ax1)
    cax1 = divider1.append_axes("right", size="8%", pad=0.1)
    cbar1 = fig.colorbar(cntr1, cax1)
    ax1.set_title('Ground truth Te flux response', pad=10.0)
    ax1.set_xlabel(r'$T_{e}$')   #(r'$Te$')
    ax1.set_ylabel(r'$ \nabla T_{e}$')   #(r'$\nabla Te$')
    #cbar1.set_label(r'$\Phi_{T_{e}}$')
    ax1.set_aspect('equal')

    ### --- Second plot for GPR mean
    cntr2 = ax2.tricontourf(x1i, x2i, y_pred, levels=12, cmap="RdBu_r")
    ax2.scatter(x1o, x2o, c=y_observ, cmap='RdBu_r', edgecolors='k', s=12)
    divider2 = make_axes_locatable(ax2)
    cax2 = divider2.append_axes("right", size="8%", pad=0.1)
    cbar2 = fig.colorbar(cntr2, cax2, boundaries=[0, 180000])
    #ax2.set_title('GPR results for f=(' + funcname + ') with ' + str(len(y_observ)) + ' # func. eval-s')
    ax2.set_title('Te flux prediction for training set of {} '.format(len(y_observ)), pad=10.0)
    ax2.set_xlabel(r'$T_{e}$')   #(r'$Te$')
    ax2.set_ylabel(r'$ \nabla T_{e}$')   #(r'$\nabla Te$')
    ax2.set_aspect('equal')
    if len(newpoints) != 0:  #TODO fix two different scatter plots ; fix scatter plots margin iside the figure box
        ax2.scatter(newpoints[0][:,0], newpoints[0][:,1], c=newpoints[1][:,0], edgecolors='g', s=14) #, label='new samples')

    ### --- Third plot for the neg-utility (GPR STD)
    cntr3 = ax3.tricontourf(x1i, x2i, sigma, levels=14, cmap="RdBu_r")
    divider3 = make_axes_locatable(ax3)
    cax3 = divider3.append_axes("right", size="8%", pad=0.1)
    cbar3 = fig.colorbar(cntr3, cax3, boundaries=[0, 20000])
    #ax2.plot(x, y, 'ko', ms=3)
    ax3.set_title('Surrogate uncertainty (st.dev.)', pad=10.0)  #(r'GPR $\sigma$')
    ax3.set_xlabel(r'$T_{e}$')  #(r'$Te$')
    ax3.set_ylabel(r'$ \nabla T_{e}$')  #(r'$\nabla Te$')
    ax3.set_aspect('equal')

    ################################
    #plt.plot(x_domain, y_test, 'r:', label='e-|x|^2 * cos(|x|^2)') #r'$f(x) = \cos(ax)\,\sin(bx)$')
    #plt.legend(loc='upper right')

    plt.tight_layout()
    plt.subplots_adjust()  # TODO should have less margin at saved figure
    plt.savefig(os.path.join(wrt_dir, 'TE_surr2d_gem0_' + str(len(y_observ))+'.png'))
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
    explode[np.argmax(sobol_ind_vals)] = 0.1
    
    fig1, ax1 = plt.subplots()
    ax1.pie(sobol_ind_vals, explode=explode, labels=labels, 
            autopct='%.2f', shadow=False, startangle=0)

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

