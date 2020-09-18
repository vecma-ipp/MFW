
def l3interp(y_in, x_in, nr_in, y_out, x_out, nr_out): #TODO currently intepolated values is garbage
    """
    interpolation function
    """

    #print('x_in[{}]: {}; x_in[0]: {}'.format(nr_in, x_in[nr_in], x_in[0]))
    #print('x_out: {}'.format(x_out))

    if x_in[nr_in] > x_in[0]:
        jstart = 2
        jfirst = 0
        jlast = nr_out + 1
        jstep = 1
    else:
        jstart = nr_out - 2
        jfirst = nr_out
        jlast = 0
        jstep = -1

    j1 = jstart

    #print('y_out size is {}'.format(y_out.shape))
    #print('the iteration for interpolation is over {}; {}; {}'.format(jfirst, jlast, jstep))

    for j in range(jfirst, jlast, jstep):
        if isinstance(x_out, float) == 1:
            x = x_out # TODO: should not be that, might read garbage
        else:
            x = x_out[j]
        while x >= x_in[j1] and nr_in - 1 > j1 > 2:
            j1 = j1 + jstep

        j2 = j1 + jstep
        j0 = j1 - jstep
        jm = j1 - 2 * jstep

        #print('j2: {}; j0: {}; jm: {}'.format(j2, j0, jm))

    # extrapolate inside out

        x2 = x_in[j2]
        x1 = x_in[j1]
        x0 = x_in[j0]
        xm = x_in[jm]

        aintm = (x - x0) * (x - x1) * (x - x2) / ((xm - x0) * (xm - x1) * (xm - x2))
        aint0 = (x - xm) * (x - x1) * (x - x2) / ((x0 - xm) * (x0 - x1) * (x0 - x2))
        aint1 = (x - xm) * (x - x0) * (x - x2) / ((x1 - xm) * (x1 - x0) * (x1 - x2))
        aint2 = (x - xm) * (x - x0) * (x - x1) / ((x2 - xm) * (x2 - x0) * (x2 - x1))

        #print('interpol ref points : {} {} {} {} {}'.format(x0, x1, x2, xm, x))
        #print('interpol coefs : {} {} {} {}'.format(aintm, aint0, aint1, aint2))

        if isinstance(y_in, float) or isinstance(y_out, float):
            y_out = aintm * y_in + aint0 * y_in + aint1 * y_in + aint2 * y_in
        else:
            y_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]

        #print('y_out : {}'.format(y_out))

def l3deriv(y_in, x_in, nr_in, dydx_out, x_out, nr_out):
    """
    derivtive on interpolated values
    """

    if x_in[nr_in] > x_in[0]:
        jstart = 2
        jfirst = 0
        jlast = nr_out + 1
        jstep = 1
    else:
        jstart = nr_out - 2
        jfirst = nr_out
        jlast = 0
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

        if isinstance(y_in, float):
            dydx_out = aintm * y_in + aint0 * y_in + aint1 * y_in + aint2 * y_in
        else:
            dydx_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]
