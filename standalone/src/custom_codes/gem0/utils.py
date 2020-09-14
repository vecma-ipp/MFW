
def l3interp(y_in, x_in, nr_in, y_out, x_out, nr_out):
    """
    interpolation function
    """

    if x_in[nr_in] > x_in[0]:
        jstart = 2
        jfirst = 0
        jlast = nr_out
        jstep = 1
    else:
        jstart = nr_out - 2
        jfirst = nr_out
        jlast = 1
        jstep = -1

    j1 = jstart

    for j in range(jfirst, jlast, jstep):
        if isinstance(x_out, float) == 1:
            x = x_out
        else:
            x = x_out[j]
        while x >= x_in[j1] and nr_in - 1.0 > j1 > 2:
            j1 = j1 + jstep

        j2 = j1 + jstep
        j0 = j1 - jstep
        jm = j1 - 2 * jstep

    # extrapolate inside out

        x2 = x_in[j2]
        x1 = x_in[j1]
        x0 = x_in[j0]
        xm = x_in[jm]

        aintm = (x - x0) * (x - x1) * (x - x2) / ((xm - x0) * (xm - x1) * (xm - x2))
        aint0 = (x - xm) * (x - x1) * (x - x2) / ((x0 - xm) * (x0 - x1) * (x0 - x2))
        aint1 = (x - xm) * (x - x0) * (x - x2) / ((x1 - xm) * (x1 - x0) * (x1 - x2))
        aint2 = (x - xm) * (x - x0) * (x - x1) / ((x2 - xm) * (x2 - x0) * (x2 - x1))

        y_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]


def l3deriv(y_in, x_in, nr_in, dydx_out, x_out, nr_out):
    """
    derivtive on interpolated values
    """

    if (x_in[nr_in] > x_in[0]):
        jstart = 2
        jfirst = 0
        jlast = nr_out
        jstep = 1
    else:
        jstart = nr_out - 2
        jfirst = nr_out
        jlast = 1
        jstep = -1

    j1 = jstart

    for j in range(jfirst, jlast, jstep):
        x = x_out[j]
        while x >= x_in[j1] and nr_in - 1 > j1 > 2:
            j1 = j1 + jstep

        j2 = j1 + jstep
        j0 = j1 - jstep
        jm = j1 - 2 * jstep

        # extrapolate inside out

        x2 = x_in[j2]
        x1 = x_in[j1]
        x0 = x_in[j0]
        xm = x_in[jm]

        aintm = ((x - x1) * (x - x2) + (x - x0) * (x - x2) + (x - x0) * (x - x1)) / ((xm - x0) * (xm - x1) * (xm - x2))
        aint0 = ((x - x1) * (x - x2) + (x - xm) * (x - x2) + (x - xm) * (x - x1)) / ((x0 - xm) * (x0 - x1) * (x0 - x2))
        aint1 = ((x - x0) * (x - x2) + (x - xm) * (x - x2) + (x - xm) * (x - x0)) / ((x1 - xm) * (x1 - x0) * (x1 - x2))
        aint2 = ((x - x0) * (x - x1) + (x - xm) * (x - x1) + (x - xm) * (x - x0)) / ((x2 - xm) * (x2 - x0) * (x2 - x1))

        dydx_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]
