import numpy as np
from scipy.interpolate import splrep, splev


# Spline fitting
def spl_fit(x, n, p):
    # x: vertices to approximate
    # n: control points number
    # p: spline degree

    # Vertices number
    m = len(x)

    # The parameterization
    u = np.linspace(0., 1., m)

    # Knots vector to define spline space
    T = np.linspace(0., 1., n-p+1)[1:-1]

    # Weights
    w = np.ones(m)

    # To interpolate the endpoints
    w[0]  = 1.e16
    w[-1] = 1.e16

    # Find the knot points
    tck = splrep(u, x, w=w, k=p, xb=0., xe=1., t=T)

    # outputs: knots and control points
    return tck[0], tck[1][:n]

# Spline evaluation
def spl_eval(xgrid, t, c, p):
    # t: knot vector
    # c: control points
    # p: spline degree
    tck = [t, c, p]

    # Evaluate spline
    y = splev(xgrid, tck)
    return y

