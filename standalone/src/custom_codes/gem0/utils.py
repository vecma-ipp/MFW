
def l3interp(y_in, x_in, nr_in, y_out, x_out, nr_out): #TODO currently intepolated values is garbage
    """
    Interpolation function
    perfroms Lagrange interpolation of degree 3 for values y_in on x_in 
    for points at x_out and writes the values at y_out
    """

    #print("> Interpolation, nr_in:{} nr_out:{}".format(nr_in, nr_out))
    #print('x_in[{}]: {}; x_in[0]: {}'.format(nr_in, x_in[nr_in], x_in[0]))
    #print('x_out: {}'.format(x_out))

    if x_in[nr_in - 1] > x_in[0]:
        jstart = 2
        jfirst = 0
        jlast = nr_out - 1 
        jstep = 1
    else:
        jstart = nr_out - 3
        jfirst = nr_out - 1
        jlast = 0
        jstep = -1

    j1 = jstart

    #print('y_out size is {}'.format(y_out.shape))
    #print('the iteration for interpolation is over {}; {}; {}; {}'.format(jstart, jfirst, jlast, jstep))

    for j in range(jfirst, jlast + 1, jstep):
        x = x_out[j]
        while x >= x_in[j1] and nr_in - 2 > j1 > 1:
            j1 = j1 + jstep

        j2 = j1 + jstep
        j0 = j1 - jstep
        jm = j1 - 2 * jstep

        #print('j2: {}; j1:{}, j0: {}; jm: {}'.format(j2, j1, j0, jm))

    # Extrapolate inside out

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

        y_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]

        #print ('y vals: {} {} {} {}'.format(y_in[j0], y_in[j1], y_in[j2], y_in[jm]))
        #print('y_out : {}, res len:{}'.format(y_out[j], len(y_out)))

        return y_out

def l3deriv(y_in, x_in, nr_in, dydx_out, x_out, nr_out):
    """
    Derivtive on interpolated values
    """

    #print("> Derivative on inter., nr_in:{} nr_out:{}".format(nr_in, nr_out))

    if x_in[nr_in - 1] > x_in[0]:
        jstart = 2
        jfirst = 0
        jlast = nr_out - 1
        jstep = 1
    else:
        jstart = nr_out - 3
        jfirst = nr_out - 1
        jlast = 0
        jstep = -1

    j1 = jstart

    #print('the iteration for interpolation is over {}; {}; {}; {}'.format(jstart, jfirst, jlast, jstep))

    for j in range(jfirst, jlast + 1, jstep):
        x = x_out[j]
        while x >= x_in[j1] and nr_in - 2 > j1 > 1:
            j1 = j1 + jstep

        j2 = j1 + jstep
        j0 = j1 - jstep
        jm = j1 - 2 * jstep

        #print('j2: {}; j1:{}, j0: {}; jm: {}'.format(j2, j1, j0, jm))

        # Extrapolate inside out

        x2 = x_in[j2]
        x1 = x_in[j1]
        x0 = x_in[j0]
        xm = x_in[jm]

        aintm = ((x - x1) * (x - x2) + (x - x0) * (x - x2) + (x - x0) * (x - x1)) / ((xm - x0) * (xm - x1) * (xm - x2))
        aint0 = ((x - x1) * (x - x2) + (x - xm) * (x - x2) + (x - xm) * (x - x1)) / ((x0 - xm) * (x0 - x1) * (x0 - x2))
        aint1 = ((x - x0) * (x - x2) + (x - xm) * (x - x2) + (x - xm) * (x - x0)) / ((x1 - xm) * (x1 - x0) * (x1 - x2))
        aint2 = ((x - x0) * (x - x1) + (x - xm) * (x - x1) + (x - xm) * (x - x0)) / ((x2 - xm) * (x2 - x0) * (x2 - x1))

        #print('interpol ref points : {} {} {} {} {}'.format(x0, x1, x2, xm, x))

        dydx_out[j] = aintm * y_in[jm] + aint0 * y_in[j0] + aint1 * y_in[j1] + aint2 * y_in[j2]

        #print ('y vals: {} {} {} {}'.format(y_in[jm], y_in[j0], y_in[j1], y_in[j2]))
        #print('y_out : {}, res len:{}'.format(dydx_out[j], len(dydx_out)))

        return dydx_out