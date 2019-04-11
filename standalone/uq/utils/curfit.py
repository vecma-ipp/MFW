# -*- coding: UTF-8 -*-
import os
import numpy as np
from scipy.interpolate import splrep, splev
from ascii_cpo import read
import matplotlib.pylab as plt

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
def test_approximation(eq_file):
    # get pressure fom the equilibrium
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
def test_correlations():
    fig1 = plt.figure()
    ax1  = fig1.add_subplot(111)
    fig2 = plt.figure()
    ax2  = fig2.add_subplot(111)
    fig3 = plt.figure()
    ax3  = fig3.add_subplot(111)
    fig4 = plt.figure()
    ax4  = fig4.add_subplot(111)
    fig5 = plt.figure()
    ax5  = fig5.add_subplot(111)

    n = 3
    p = 3
    corep_file = 'Run_0/ets_coreprof_out.cpo'
    corep = read(corep_file, 'coreprof')
    rho = corep.rho_tor_norm

    for i in range(36):
        corep_file = 'Run_'+str(i)+  '/ets_coreprof_out.cpo'
        corep = read(corep_file, 'coreprof')
        te  = corep.te.value
        u = compute_sites(rho, te)
        tck_x, tck_y = approximate_curve(rho, te, u, n, p)
        xa, ya = spl(rho, tck_x, tck_y)
        # Control points:
        Px = tck_x[1][:n+p]
        Py = tck_y[1][:n+p]


        #ax1.plot(rho, te, 'b.')
        #ax1.plot(xa, ya, 'r-')
        #ax3.plot(Px, Py, 'r.')
        ax1.plot(Py[0], Py[1], 'C1.')
        ax2.plot(Py[0], Py[2], 'C2.')
        ax3.plot(Py[0], Py[3], 'C3.')
        ax4.plot(Py[0], Py[4], 'C4.')
        ax5.plot(Py[0], Py[5], 'C5.')

    #ax1.set_title('Te Profiles + Spline approximations')
    ax1.set_title(r'$\alpha_2 \quad vs. \quad \alpha_1$')
    ax1.set_xlabel(r'$\alpha_1$')
    ax1.set_ylabel(r'$\alpha_2$')

    ax2.set_title(r'$\alpha_3 \quad vs. \quad \alpha_1$')
    ax2.set_xlabel(r'$\alpha_1$')
    ax2.set_ylabel(r'$\alpha_3$')

    ax3.set_title(r'$\alpha_4 \quad vs. \quad \alpha_1$')
    ax3.set_xlabel(r'$\alpha_1$')
    ax3.set_ylabel(r'$\alpha_4$')

    ax4.set_title(r'$\alpha_5 \quad vs. \quad \alpha_1$')
    ax4.set_xlabel(r'$\alpha_1$')
    ax4.set_ylabel(r'$\alpha_5$')

    ax5.set_title(r'$\alpha_6 \quad vs. \quad \alpha_1$')
    ax5.set_xlabel(r'$\alpha_1$')
    ax5.set_ylabel(r'$\alpha_6$')

    plt.grid()
    plt.show()

if __name__ == "__main__":

    cpo_dir  = os.path.abspath("../../data/AUG_28906_5/BGB_GEM_SPREAD/4FT")

    # approxiamtion of the pressure profile
    eq_file = cpo_dir + '/ets_equilibrium_in.cpo'
    test_approximation(eq_file)

