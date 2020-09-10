
from da_utils import *
from model_fitting import *

def calc_var_pce(N=2, K=4, dist=cp.J(cp.Uniform(0.1, 0.3), cp. Uniform(0.7, 1.3)), f=(lambda th,x: np.cos(th[0]*x) * np.sin(th[1]*x))):
    thetas, ws = cp.generate_quadrature(K, dist, rule="gaussian")
    pexp = cp.orth_ttr(N, dist)
    xs = np.linspace(0, 5, 32)
    evals = [f(theta, xs) for theta in thetas.T]
    fns = cp.fit_quadrature(pexp, thetas, ws, evals)
    exp = cp.E(fns, dist)
    std = cp.Std(fns, dist)
    var = cp.Var(fns, dist)
    plot_unc(f, exp, std, xs, K, N)
    return exp, std, var, fns

def perf_pce_conv_analysis(true_mean=-1, true_variance=0.02, f=(lambda a,x: np.cos(a*x))):
    polynomial_orders = list(range(1, 4))
    sample_sizes = []
    errors_mean = []
    errors_variance = []
    for order in polynomial_orders:
        exp, std, _, _ = calc_var_num(N=order, K=8)
        sample_sizes.append((order+1)**2)
        errors_mean.append(np.mean(np.abs(exp-true_mean)))
        errors_variance.append(np.mean(np.abs(exp-true_variance)))

    plot_conv(sample_sizes, errors_mean, errors_variance)

def test_bivariate(f=lambda x, y, a, b: cos(a*x)*sin(b*y), val=[np.pi, np.pi/2], priors=[[1.0, ],[]]):
    return 0

def test_cossin_proj():
    a = np.array([0.1, 1.0, 5.0])
    b = np.array([1.0, 2.0])
    #par = np.tensordot(a.T,b,1)
    par = np.transpose([np.tile(a, len(b)), np.repeat(b, len(a))])
    X = []
    Y = []
    x = np.linspace(0.0, np.pi/2, 64)
    for parval in par:
        X.append(x)
        y = cossin_model(x, parval)
        Y.append(y)
    plot_mult_lines(X, Y, par, 'cossinproj')

def test_data(datafile):
    dat, _, _ = read_sim_csv(datafile)
    pord = 4
    # plot_2d_map(data, X,Y)
    plot_3d_wire(dat, yind=1)
    x = dat['te_ddrho'].unique()
    y = dat['ti_ddrho'].unique()
    x, y = np.meshgrid(x, y)
    # z = dat['te_transp_flux'].to_numpy().reshape(((int(len(x)**(1.0/pord)),)*4))
    z = dat['te_transp_flux'].to_numpy().reshape((x.shape[0], y.shape[0], x.shape[0], y.shape[0]))
    z = z[:, :, z.shape[2] // 5, z.shape[3] // 5]
    x = np.ravel(x)
    y = np.ravel(y)
    z = np.ravel(z)
    #plot_3d_suraface(x, y, z, 'gemdata')
    return x, y, z


def test_exp_fitting(X, Y, Z):
    # fit_exp(X, Y, Z)
    best_vals = fit_exp_lin(X, Y, Z)
    new_exp_theta = [np.e ** best_vals[0], -best_vals[1], -best_vals[2]]
    Z_mod = np.array([exponential_model(coord, new_exp_theta) for coord in np.dstack((X, Y))[0]])
    rmse = np.sqrt(((Z - Z_mod) ** 2).sum()) / len(Z)
    # print('error limits are for abs: {} , rel: {}'.format(abs_error.max(),rel_error.max()))
    print('RMSE of exp fitting is {}'.format(rmse))

datafile = "../data/gem_uq_inoutput.csv"

###---Plot scatter plot of Te and Te_flux for a  campaing----
#teprvals, tiprvals, tegrvals, tigrvals, teflvals, tiflvals = get_camp_data(basefolder, ft1_indx)
#plot_scatter_2D(teprvals, teflvals, 'fuswfGEM0_ft61_PCE')
#print("Te flux values: ")
#print(teflvals)

###---Plot color mesh---
#X = read_data_totensor(basefolder)
#X = np.array([teprvals, tiprvals, tegrvals, tigrvals])
#X = np.transpose(X)
#Y = np.array([teflvals, tiflvals])
#Y = np.transpose(Y)


###---Find variance of parameter numerically---
#exp, std, var, fns = calc_var_num()
#xs = np.linspace(0,10,100)
#ind = (np.abs(xs - np.pi)).argmin()
#print(exp[ind])

###---Convergence for polynomial orders---
#perf_analysis()

###---Plot surface for an exponent---
#plot_model_response(name='exp')

###---Plot response surfaces for GEM data---
X, Y, Z = test_data(datafile)

###---See what exponent might model the data---
test_exp_fitting(X, Y, Z)

###---Plot cos(ax)*sin(bx) at x=pi/4---
#test_cossin()
#test_cossin_proj()
