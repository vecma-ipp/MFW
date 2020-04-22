import logging
import numpy as np
import chaospy as cp


# Return cp.distribution
def get_dist(dist_name, value, margin_error):

    # list cp distrubutions here
    if dist_name.lower() == "normal":
            if type(value) == list:
                d = []
                for v in value:
                    d.append(cp.Normal(v, margin_error*v))
                dist = cp.J(*d)
            else:
                dist = cp.Normal(value, margin_error*value)

    elif dist_name.lower() == "uniform":

        if type(value) == list:
            d = []
            for v in value:
                lo = (1. - 0.5*margin_error)*v
                up = (1. + 0.5*margin_error)*v
                if mean == 0.:
                    up = margin_error

                d.append(cp.Uniform(lo, up))
            dist = cp.J(*d)
        else:
            lo = (1. - 0.5*margin_error)*value
            up = (1. + 0.5*margin_error)*value
            if mean == 0.:
                up = margin_error

            dist = cp.Uniform(lo, up)

    # TODO add all cp distributions
    else:
        msg = "Unknown distribution name: " + dist_name
        logging.error(msg)
        raise Exception(msg)

    return dist

# FOR VALIDATION
# To be moved to chaospy
class Split_Normal():
    """
    The split-normal distribution is a result from joining at the mode
    the corresponding halves of two normal distributions with the same
    mode but different variances.
    """
    def __init__(self, mode, sig1, sig2):
        self._mo = mode
        self._sig1 = sig1
        self._sig2 = sig2

        # Mean and STD
        self.mu = mode + np.sqrt(2/np.pi)*(sig2-sig1)
        self.sigma = np.sqrt((1.-2./np.pi)*(sig2-sig1)**2 + sig1*sig2)
        #Normal.__init__(self, mu=mu, sigma=sigma)
        #Dist.__init__(self)

    def pdf(self, x):
        a = np.sqrt(2./np.pi)/(self._sig1+self._sig2)

        x = np.asfarray(x)
        x = np.sort(x)

        x1 = x[x<=self._mo]
        y1 = a*np.e**(-(x1-self._mo)**2/(2.*self._sig1**2))

        x2 = x[x>self._mo]
        y2 = a*np.e**(-(x2-self._mo)**2/(2.*self._sig2**2))

        y = np.concatenate((y1, y2))
        return y

# To be moved to chaospy
class Assymetric_Normal():
    """
    """
    def __init__(self, mu, sig1, sig2):
        self._mu = mu
        self._sig1 = sig1
        self._sig2 = sig2

        #Dist.__init__(self)

    def pdf(self, x):

        x = np.asfarray(x)
        x = np.sort(x)

        a1 = 1./(self._sig1*np.sqrt(2.*np.pi))
        x1 = x[x<=self._mu]
        y1 = a1*np.e**(-(x1-self._mu)**2/(2.*self._sig1**2))

        a2 = 1./(self._sig2*np.sqrt(2.*np.pi))
        x2 = x[x>self._mu]
        y2 = a2*np.e**(-(x2-self._mu)**2/(2.*self._sig2**2))

        y = np.concatenate((y1, y2))

        return y
