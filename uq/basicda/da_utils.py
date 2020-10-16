import os
import numpy as np
import pandas as pd
import chaospy as cp

import matplotlib
#matplotlib.use('Agg')
import matplotlib.pyplot as plt
params = { #"text.usetex": True,
          'axes.labelsize': 6,
          'axes.titlesize':5,
          'xtick.labelsize':5,
          'ytick.labelsize':5,
          'axes.titlepad': 1,
          'axes.labelpad': 1,
          'legend.fontsize': 7, 
          'legend.handlelength': 1}
plt.rcParams.update(params)
import matplotlib.tri as mtri

from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.axes_grid1 import make_axes_locatable

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
    :param y: list of cordinates in Y
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

def plot_prediction_variance(x_observ, y_observ, x_domain, y_test, y_pred, sigma, f, x_choice=[], newpoints=[], rmse=0.0, funcname='e^-x cos x', dy=0):
    """ 
    Plots prediction and 95% confidence interval
    :param x_observ: domain values for function evaluations
    :param y_observ: function evaluation values
    :param x_domain: values of domain (RoI)
    :param y_test: function values for points at x_domain
    :param y_pred: function values prediceted by model
    :param sigma: std for model predictions
    :param f: true function
    """

    plt.figure()
    #y_test = f(x_domain)
    plt.plot(x_domain, y_test, 'r:', label=funcname) #r'$f(x) = x\,\sin(x)$')
    plt.errorbar(x_observ.ravel(), y_observ, dy, fmt='r.', markersize=10, label='Observations')
    plt.plot(x_domain, y_pred, 'b-', label='Prediction')
    plt.fill(np.concatenate([x_domain, x_domain[::-1]]),
             np.concatenate([y_pred - 1.9600 * sigma,
                             (y_pred + 1.9600 * sigma)[::-1]]),
             alpha=.5, fc='b', ec='None', label='95% confidence interval')
    plt.vlines(x_choice, -1, 1, colors='k', alpha=0.5, linestyles='dashed', label='new opt choice')
    if len(newpoints) != 0:
        plt.plot(newpoints, f(np.array(newpoints)), 'go', markersize=12, label='new samples')
    plt.xlabel(r'$\nabla Te$')
    plt.ylabel('$Te flux$')
    plt.title('GPR results for f=(' + funcname + ') with ' + str(len(y_observ)) + ' number of function evaluations') 
               # + 'PRediction RMSE is ' + str(rmse))
    plt.legend(loc='upper right')
    #plt.show(block=True)
    plt.savefig('surr_gem0_Teddrho_' + str(len(y_observ)) + '.pdf')
    plt.close()
    #return y_test.T.reshape(-1)

def plot_prediction_variance_2d(x_observ, y_observ, x_domain, y_test, y_pred, sigma, newpoints, funcname):

    wrt_dir = os.path.join(os.environ['SCRATCH'], 'outputs/plots/res128')
    #wrt_dir = os.environ['PWD']

    # Plot function,prediction and 95% confidence interval
    x1o = x_observ[:,0]
    x2o = x_observ[:,1]
    x1i = x_domain[:,0]
    x2i = x_domain[:,1]
    
    fig, (ax1, ax2, ax3) = plt.subplots(nrows=1, ncols=3)

    ### --- First plot for response function
    #ax1.contour(x1, x2, y1, levels=14, linewidths=0.5, colors='k')
    cntr1 = ax1.tricontourf(x1i, x2i, y_test, levels=12, cmap="RdBu_r")
    divider1 = make_axes_locatable(ax1)
    cax1 = divider1.append_axes("right", size="8%", pad=0.08)
    plt.colorbar(cntr1, cax=cax1)
    ax1.set_title('Ground truth Te flux response')
    plt.xlabel("Te") #(r'$Te$')
    plt.ylabel("gradTe") #(r'$\nabla Te$')
    ax1.set_aspect('equal')

    ### --- Second plot for GPR mean
    cntr2 = ax2.tricontourf(x1i, x2i, y_pred, levels=12, cmap="RdBu_r")
    ax2.scatter(x1o, x2o, c=y_observ, cmap='RdBu_r', edgecolors='k', s=6)
    divider2 = make_axes_locatable(ax2)
    cax2 = divider2.append_axes("right", size="8%", pad=0.08)
    plt.colorbar(cntr2, cax=cax2)
    #ax2.set_title('GPR results for f=(' + funcname + ') with ' + str(len(y_observ)) + ' # func. eval-s')
    ax2.set_title('Te flux prediction for {} evaluations'.format(len(y_observ)))
    plt.xlabel("Te") #(r'$Te$')
    plt.ylabel("gradTe") #(r'$\nabla Te$')
    ax2.set_aspect('equal')
    if len(newpoints) != 0: #TODO fix two differen scatter one plot
        ax2.scatter(newpoints[0][0], newpoints[0][1], c=newpoints[1][0], edgecolors='g', s=8) #, label='new samples')

    ### --- Third plot for the neg-utility (GPR STD)
    #ax2.tricontour(x1i, x2i, sigma, levels=14, linewidths=0.5, colors='k')
    cntr3 = ax3.tricontourf(x1i, x2i, sigma, levels=14, cmap="RdBu_r")
    divider3 = make_axes_locatable(ax3)
    cax3 = divider3.append_axes("right", size="8%", pad=0.08)
    plt.colorbar(cntr3, cax=cax3)
    #ax2.plot(x, y, 'ko', ms=3)
    ax3.set_title('GPR sigma') #(r'GPR $\sigma$')
    plt.xlabel("Te") #(r'$Te$')
    plt.ylabel("gradTe") #(r'$\nabla Te$')
    ax3.set_aspect('equal')

    ################################
    #plt.plot(x_domain, y_test, 'r:', label='e-|x|^2 * cos(|x|^2)') #r'$f(x) = \cos(ax)\,\sin(bx)$')
    #plt.legend(loc='upper right')

    plt.tight_layout()
    plt.subplots_adjust() # TODO should have less margin at saved figure
    plt.savefig(os.path.join(wrt_dir, 'surr2d_gem0_' + str(len(y_observ))+'.png'))
    plt.close()

def plot_error(err, name):
    wrt_dir = os.path.join(os.environ['SCRATCH'], 'outputs/plots/res128') # wrt_dir = ''
    #wrt_dir = os.environ['PWD']
    plt.plot(range(1, len(err) + 1), err, label=name)
    plt.xlabel('n. interations')
    plt.ylabel('error')
    plt.title('Error of GPR surrogate predictions at function evaluations')
    plt.savefig(os.path.join(wrt_dir, 'surr_gem0_err_' + name + '.png'))
    plt.close()

def plot_histograms(y_orig, y_orig_clean, y_pred, y_pred_clean):
    wrt_dir = os.path.join(os.environ['SCRATCH'], 'outputs/plots/res128')
    
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