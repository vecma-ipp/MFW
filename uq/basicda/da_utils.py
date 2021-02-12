import os
import numpy as np
import pandas as pd
import chaospy as cp

import matplotlib
#matplotlib.use('Agg')
import matplotlib.pyplot as plt
params = { #"text.usetex": True,
          'axes.labelsize': 8,
          'axes.titlesize': 9,
          'xtick.labelsize': 6,
          'ytick.labelsize': 6,
          'axes.titlepad': .5,
          'axes.labelpad': .5,
          'legend.fontsize': 11,
          'legend.handlelength': .5}
plt.rcParams.update(params)
import matplotlib.tri as mtri

from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.axes_grid1 import make_axes_locatable

from sklearn.neighbors import KernelDensity

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
    # Create contour plot for the probability density function
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
    Plots prediction and 95% confidence interval
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

    plt.errorbar(x_observ.ravel(), y_observ, dy, fmt='r.', markersize=10, label='Simulation')
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
    :param: x_newpoints: a point in space domain that would be added to training set in the NEXT optimisation iteration

    """

    wrt_dir = ''
    n = len(y_pred)

    fig, ax = plt.subplots()

    cmap_newpoints = plt.get_cmap('Greens')
    cmpap_sims = plt.get_cmap('Blues')
    cmap_test = plt.get_cmap('Reds')

    for i in range(0, n-1):

        ax.errorbar(x_observ[i].ravel(), y_observ[i], dy, fmt='.', c='r', markersize=10)  #label='Simulation')
        #ax.plot(x_domain, y_pred[i], 'b-', alpha=((i+1)/float(n)))  #, label='Prediction')
        ax.fill(np.concatenate([x_domain, x_domain[::-1]]),
                 np.concatenate([y_pred[i] - 1.9600 * sigma[i],
                                 (y_pred[i] + 1.9600 * sigma[i])[::-1]]),
                 alpha=.1, fc='b', ec='None')  #, label='95% confidence interval')

        if i > 0:
            if len(x_newpoints[i-1]) != 0:
                ax.plot(x_newpoints[i-1], y_newpoints[i-1], 'o', c='r', markersize=10*(i+20)/float(n+20), alpha=1.0)  # label='order of sampling')
            #ax.annotate('{}'.format(i), (newpoints[i], val))

    # plot points and confidence interval for last optimization iterations
    ax.errorbar(x_observ[n-1], y_observ[n-1], dy, fmt='r.', markersize=5)  # label='Simulations')
    ax.plot(x_domain, y_pred[n-1], 'b-', alpha=(n/float(n)), label='Mean Prediction')
    ax.fill(np.concatenate([x_domain, x_domain[::-1]]),
            np.concatenate([y_pred[0] - 1.9600 * sigma[0],
                           (y_pred[0] + 1.9600 * sigma[0])[::-1]]),
            alpha=.1, fc='b', ec='None', label='95% confidence interval')

    # plot the last point of optimization, add plot labels
    if len(x_newpoints[n-2]) != 0:
        ax.plot(x_newpoints[n-2], y_newpoints[i-1], 'o', c='r', markersize=10,
                alpha=1.0, label='Training Simulations')

    ax.plot(x_domain, y_test, 'r.', label='Testing Simulations')

    ax.set_xlabel(r'$\nabla Ti$')
    ax.set_ylabel('$Ti\_flux$')
    ax.set_title('GPR results for ' + funcname + ' for ' + str(len(y_observ)) + ' number of simulations')
    # + 'PRediction RMSE is ' + str(rmse))
    ax.legend(loc='upper right')
    ax.set_ylim([-7e+3, 2.4e+5])
    plt.savefig(os.path.join(wrt_dir, 'surrogate_gem0_tiflux_tiddrho_total_' + str(len(y_observ)) + '.pdf'))
    plt.close()

def add_density_x(x, range=[200, 400]):
    xinds = np.arange(200, 400)
    dat = x[xinds, np.newaxis]
    x_domain = np.linspace(dat.min(), dat.max(), 1000)[:, np.newaxis]

    kde = KernelDensity(kernel='gaussian', bandwidth=150.).fit(dat)
    log_dens = kde.score_samples(x_domain[:, 0].reshape(-1, 1))

    plt.fill( np.concatenate([x_domain, x_domain[::-1]]),
              np.concatenate([np.zeros(len(log_dens)), 100*np.exp(log_dens)[::-1]]))


def plot_prediction_variance_2d(x_observ, y_observ, x_domain, y_test, y_pred, sigma, newpoints, funcname):

    #wrt_dir = os.path.join(os.environ['SCRATCH'], 'outputs/plots/res_debug')
    #wrt_dir = os.path.join(os.environ['PWD'], 'locmaxsamp')
    wrt_dir = ""

    # Plot function,prediction and 95% confidence interval
    x1o = x_observ[:,0]
    x2o = x_observ[:,1]
    x1i = x_domain[:,0]
    x2i = x_domain[:,1]

    # print("shapes : {} {}".format(newpoints[0].shape, newpoints[1].shape))

    y_test = y_test.reshape(-1,)
    y_pred = y_pred.reshape(-1,)
    y_observ = y_observ.reshape(-1,)
    
    fig, (ax1, ax2, ax3) = plt.subplots(nrows=1, ncols=3, figsize=(10, 5))

    ### --- First plot for response function
    #ax1.contour(x1, x2, y1, levels=14, linewidths=0.5, colors='k')
    cntr1 = ax1.tricontourf(x1i, x2i, y_test, levels=12, cmap="RdBu_r")
    divider1 = make_axes_locatable(ax1)
    cax1 = divider1.append_axes("right", size="7%", pad=0.07)
    plt.colorbar(cntr1, cax=cax1)
    ax1.set_title('Ground truth Te flux response')
    plt.xlabel("Te") #(r'$Te$')
    plt.ylabel("gradTe") #(r'$\nabla Te$')
    ax1.set_aspect('equal')

    ### --- Second plot for GPR mean
    cntr2 = ax2.tricontourf(x1i, x2i, y_pred, levels=12, cmap="RdBu_r")
    ax2.scatter(x1o, x2o, c=y_observ, cmap='RdBu_r', edgecolors='k', s=12)
    divider2 = make_axes_locatable(ax2)
    cax2 = divider2.append_axes("right", size="7%", pad=0.07)
    plt.colorbar(cntr2, cax=cax2)
    #ax2.set_title('GPR results for f=(' + funcname + ') with ' + str(len(y_observ)) + ' # func. eval-s')
    ax2.set_title('Te flux prediction for {} evaluations'.format(len(y_observ)))
    plt.xlabel("Te")  #(r'$Te$')
    plt.ylabel("gradTe")  #(r'$\nabla Te$')
    ax2.set_aspect('equal')
    if len(newpoints) != 0:  #TODO fix two different scatter one plot (what did I mean?)
        ax2.scatter(newpoints[0][:,0], newpoints[0][:,1], c=newpoints[1][:,0], edgecolors='g', s=14) #, label='new samples')

    ### --- Third plot for the neg-utility (GPR STD)
    #ax2.tricontour(x1i, x2i, sigma, levels=14, linewidths=0.5, colors='k')
    cntr3 = ax3.tricontourf(x1i, x2i, sigma, levels=14, cmap="RdBu_r")
    divider3 = make_axes_locatable(ax3)
    cax3 = divider3.append_axes("right", size="7%", pad=0.07)
    plt.colorbar(cntr3, cax=cax3)
    #ax2.plot(x, y, 'ko', ms=3)
    ax3.set_title('GPR sigma') #(r'GPR $\sigma$')
    plt.xlabel("Te")  #(r'$Te$')
    plt.ylabel("gradTe")  #(r'$\nabla Te$')
    ax3.set_aspect('equal')

    ################################
    #plt.plot(x_domain, y_test, 'r:', label='e-|x|^2 * cos(|x|^2)') #r'$f(x) = \cos(ax)\,\sin(bx)$')
    #plt.legend(loc='upper right')

    plt.tight_layout()
    plt.subplots_adjust()  # TODO should have less margin at saved figure
    plt.savefig(os.path.join(wrt_dir, 'surr2d_gem0_' + str(len(y_observ))+'.png'))
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
    plt.savefig(os.path.join(wrt_dir, 'surr_gem0_err_' + name + '.png'))
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
