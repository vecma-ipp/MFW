# -*- coding: UTF-8 -*-
import os
import numpy as np
from scipy.interpolate import splrep, splev
from ascii_cpo import read
import matplotlib.pylab as plt
from mpl_toolkits.mplot3d import Axes3D


# Computes the Spline parameterization
def compute_sites(x, y, method="uniform"):
    assert len(x) == len(y)

    n = len(x)

    if method == "uniform":
        u = np.linspace(0., 1., n)

    else:
        u = np.zeros(n)
        u[n-1] = 1.
        di = np.zeros(n-1)
        d  = 0.

        if method == "chord":
            for i in range(1, n):
                di[i-1] = np.sqrt((x[i]-x[i-1])**2 + (y[i]-y[i-1])**2)
                d = d + di[i-1]

        if method == "centripetal":
            for i in range(1, n):
                di[i-1] = np.sqrt(np.sqrt((x[i]-x[i-1])**2 + (y[i]-y[i-1])**2))
                d = d + di[i-1]

        for i in range(1, n-1):
                u[i] = u[i-1] + di[i-1]/d

    return u

# Approximate a set points of coordinates (x,y) using a Splines
def approximate_curve(x, y, n_elements, degree, param_method="centripetal"):
    assert len(x) == len(y)

    # The parameterization
    u = compute_sites(x, y, param_method)

    # Knots vector to define spline space
    T = np.linspace(0., 1., n_elements+1)[1:-1]

    # Weights
    w = np.ones(len(x))

    # To interpolate the endpoints
    w[0]  = 1.e16
    w[-1] = 1.e16

    # Find the knot points
    tck_x = splrep(u, x, w=w, k=degree, xb=0., xe=1., t=T)
    tck_y = splrep(u, y, w=w, k=degree, xb=0., xe=1., t=T)

    return tck_x, tck_y

# Approximation and plot of the pressure profile
def test_approximation(cpo_dir):
    # get pressure fom the equilibrium
    eq_file = cpo_dir + '/ets_equilibrium_in.cpo'
    eq = read(eq_file, 'equilibrium')
    rho = eq.profiles_1d.rho_tor
    p = eq.profiles_1d.pressure

    # Spline approximation (control points = n_elements + degree)
    n_elements = 2
    degree = 3
    tck_x, tck_y = approximate_curve(rho, p, n_elements, degree)

    # Get the control points
    Px = tck_x[1][: n_elements + degree]
    Py = tck_y[1][: n_elements + degree]

    # Evaluate spline
    # r_norm = [i/max(r) for i in r]
    r = np.linspace(0., 1., 200)
    ra = splev(r, tck_x)
    pa = splev(r, tck_y)

    # Plots
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.plot(rho, p, "b.",  markersize=5, alpha=0.8, label='Pressure Profile')
    ax.plot(ra, pa, 'g-', label='Approxiamtion')
    ax.plot(Px, Py, 'ro', label='Control Points')
    ax.errorbar(Px[:-1], Py[:-1], yerr=800, fmt='ro')

    ax.set_xlabel(r'$\rho_{tor} \, [m]$')
    ax.set_ylabel('P [bar]')
    ax.set_title('ETS + EQ Update output  \n Spline approximation of the pressure profile')

    ax.legend()
    plt.grid()
    plt.show()

# Print correlation between coeffs
def test_correlations(runs_dir):
    # 3D scatterplot
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    eq_file = runs_dir + '/ets_equilibrium_0.cpo'
    eq = read(eq_file, 'equilibrium')
    rho = eq.profiles_1d.rho_tor

    for i in range(1296):
        eq_file = runs_dir + '/ets_equilibrium_'+str(i)+'.cpo'
        eq = read(eq_file, 'equilibrium')
        p = eq.profiles_1d.pressure

        tck_x, tck_y = approximate_curve(rho, p, n_elements=2, degree=3)
        X = tck_y[1][3]
        Y = tck_y[1][2]
        Z = tck_y[1][1]
        ax.scatter(X, Y, Z, marker ='o')

    ax.set_xlabel(r'$P_4$')
    ax.set_ylabel(r'$P_3$')
    ax.set_zlabel(r'$P_1$')
    ax.set_title('Pressure approximation: spline coeffs. correlation')

    plt.grid()
    plt.show()

if __name__ == "__main__":

    cpo_dir  = os.path.abspath("../../data/AUG_28906_5/BGB_GEM_SPREAD/4FT")
    test_approximation(cpo_dir)

    runs_dir = os.path.abspath("/ptmp/ljala/runs")
    test_correlations(runs_dir)
