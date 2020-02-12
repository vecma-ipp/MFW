import logging
import chaospy as cp


# Return cp.distribution
def get_dist(dist_name, mean, margin_error):

    # list cp distrubutions here
    if dist_name.lower() == "normal":
            dist = cp.Normal(mean, margin_error*mean)

    elif dist_name.lower() == "uniform":
        lo = (1. - 0.5*margin_error)*mean
        up = (1. + 0.5*margin_error)*mean
        if mean == 0.:
            up = margin_error
        dist = cp.Uniform(lo, up)

    # TODO add all cp distributions
    else:
        msg = "Unknown distribution name: " + dist_name
        logging.error(msg)
        raise Exception(msg)

    return dist
