
from da_utils import *


def calc_var_num(N=2, K=4, dist=cp.J(cp.Uniform(0.1, 0.3), cp. Uniform(0.7, 1.3)), f=(lambda th,x: np.cos(th[0]*x) * np.sin(th[1]*x))):
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

def perf_analysis(true_mean=-1, true_variance=0.02, f=(lambda a,x: np.cos(a*x))):
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

###!!!
#X, Y = read_sim_csv(datafile)
#data, X, Y = read_sim_csv(datafile)
#plot_2d_map(data, X,Y)
#plot_3d_wire(data, yind=1)


###---Find variance of parameter numerically---
exp, std, var, fns = calc_var_num()
#xs = np.linspace(0,10,100)
#ind = (np.abs(xs - np.pi)).argmin()
#print(exp[ind])

###---Convergence for polynomial orders---
perf_analysis()