# -*- coding: UTF-8 -*-
import chaospy as cp


# Return cp.distribution
def get_dist(dist_name, mean, margin_error):

    # list cp distrubutions here
    if dist_name == "Normal":
        dist = cp.Normal(mean, margin_error*mean)

    elif dist_name == "Uniform":
        lo = (1. - 0.5*margin_error)*mean
        up = (1. + 0.5*margin_error)*mean
        if mean == 0.:
            up = margin_error

        dist = cp.Uniform(lo, up)

    else:
        print("Error: Bad dist_mame")

    return dist


