import logging
import chaospy as cp


# Return cp.distribution
def get_dist(dist_name, mean, margin_error=None, std=None):

    # list cp distrubutions here
    if dist_name == "Normal":
        if margin_error is not None:
            dist = cp.Normal(mean, margin_error*mean)
        elif std is not None:
            dist = cp.Normal(mean, std)
        else:
            msg = "Wrong distribution arguments."
            logger.error(msg)
            raise Exception(msg)

    elif dist_name == "Uniform":
        lo = (1. - 0.5*margin_error)*mean
        up = (1. + 0.5*margin_error)*mean
        if mean == 0.:
            up = margin_error
        dist = cp.Uniform(lo, up)

    # TODO add all cp distributions
    else:
        msg = "Unknown distribution name."
        logger.error(msg)
        raise Exception(msg)

    return dist
