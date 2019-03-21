# -*- coding: UTF-8 -*-
import os
import numpy as np
from scipy.interpolate import splrep, splev
from ascii_cpo import read
import matplotlib.pylab as plt

# Computes the parameter ui such that C(ui) = (xi, yi)
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


# Parameterization of y=f(x) using splines
def approximate_curve(x, y, u, n, p):
    """
    Approximate a set points of coordinates (x,y) using a spline of degree p
    with n elements.

    """

    assert len(x) == len(y)

    # Knots vector to define spline space
    T = np.linspace(0., 1., n+1)[1:-1]

    # Weights
    w = np.ones(len(x))

    #  To interpolate the endpoints
    w[0]  = 1.e16
    w[-1] = 1.e16

    #  The parameterization

    # Find the knot points
    tck_x = splrep(u, x, w=w, k=p, xb=0., xe=1., t=T)
    tck_y = splrep(u, y, w=w, k=p, xb=0., xe=1., t=T)

    return tck_x, tck_y


if __name__ == "__main__":
    cpo_dir  = os.path.abspath("../../data/AUG_28906_5/BGB_GEM_SPREAD/4FT")

    #
    corep_file = cpo_dir + '/ets_coreprof_in.cpo'
    corep = read(corep_file, 'coreprof')
    rho = corep.rho_tor
    te  = corep.te.value

    #
    n = 3
    p = 4
    u = compute_sites(rho, te)
    tck_x, tck_y = approximate_curve(rho, te, u, n, p)
    xa, ya = spl(u, tck_x, tck_y)

    # Control points:
    Px = tck_x[1][:n+p]
    Py = tck_y[1][:n+p]

    plt.plot(rho, te, 'b-', label='Te Profile')
    plt.plot(rho, ya, 'g-', label='Approximation')
    plt.plot(Px, Py, 'ro')

    plt.legend()
    plt.grid()
    plt.show()
