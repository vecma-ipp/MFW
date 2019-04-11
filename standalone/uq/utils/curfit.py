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

# Evaluate spline
def spl(u, tck_x, tck_y):
    xa = splev(u, tck_x)
    ya = splev(u, tck_y)
    return xa, ya


# Approximate a set points of coordinates (x,y) using a Splines
def approximate_curve(x, y, n_elemts=2, degree=3, param_method="centripetal"):
    assert len(x) == len(y)

    # For the  parameterization
    u = compute_sites(x, y, param_method)
    #m = max(x)
    #u = [i/m for i in x]

    # Knots vector to define spline space
    T = np.linspace(0., 1., n_elements+1)[1:-1]

    # Weights
    w = np.ones(len(x))

    #  To interpolate the endpoints
    w[0]  = 1.e16
    w[-1] = 1.e16

    #  The parameterization

    # Find the knot points
    tck_x = splrep(u, x, w=w, k=degree, xb=0., xe=1., t=T)
    tck_y = splrep(u, y, w=w, k=degree, xb=0., xe=1., t=T)

    return tck_x, tck_y


if __name__ == "__main__":
    cpo_dir  = os.path.abspath("../../data/AUG_28906_5/BGB_GEM_SPREAD/4FT")

    #
    eq_file = cpo_dir + '/ets_equilibrium_in.cpo'
    eq = read(eq_file, 'equilibrium')
    rho = eq.profiles_1d.rho_tor
    p = eq.profiles_1d.pressure

    m = max(rho)
    rho_norm = [i/m for i in rho]
    #
    n_elements = 2
    degree = 3
    u = compute_sites(rho, p, "centripetal")

    tck_x, tck_y = approximate_curve(rho, p)
    xa, ya = spl(u, tck_x, tck_y)
    xb, yb = spl(rho_norm, tck_x, tck_y)

    Px = tck_x[1][: n_elements + degree]
    Py = tck_y[1][: n_elements + degree]

    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.plot(rho, p, "b-",  markersize=5, alpha=0.75, label='Pressure Profile')
    ax.plot(xa, ya, 'C1.', alpha=1., label='using u')
    ax.plot(xb, yb, 'C2+', label='using rho_norm')
    ax.plot(Px, Py, 'ro', label='Control Points')
    ax.set_xlabel(r'$\rho_{tor} \, [m]$')
    ax.set_ylabel('P [bar]')

    ax.set_title('ETS + EQ Update output  \n Spline approximation of the pressure profile')
    ax.legend()
    ax.grid()
    plt.show()

