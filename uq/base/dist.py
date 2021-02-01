import logging
import numpy as np
import chaospy as cp


__all__ = ['get_dist', 'xml_inputs', 'cpo_inputs', 'ftube_indices']


def get_dist(name, value, err):
    """
    Parameters
    ----------
    name : str
        The distribution name
    value : float or int
        The mean value of the dist.
    err  : float
        The error margin

    Returns
    -------
    chaospy.Dist: the output distribution.
    """

    if name.lower() == "normal":
        if value == 0.:
            dist = cp.Normal(value, err)
        else:
            dist = cp.Normal(value, err*np.abs(value))

    elif name.lower() == "uniform":
        lo = (1. - err)*value
        up = (1. + err)*value
        if value == 0.:
            up = err
        dist = cp.Uniform(lo, up)

    # TODO add other distributions
    else:
        msg = "Unknown distribution name: " + dist_name
        logging.error(msg)
        raise Exception(msg)

    return dist

class Asymmetric_Normal():
    """
    The two pieces normal distribution is a result from joining halves of
    two normal distributions with the same mean but two different variances.
    """
    def __init__(self, mu, sig1, sig2):
        self.mu = mu
        self.sig1 = sig1
        self.sig2 = sig2

        self.dist1 = cp.Normal(mu, sig1)
        self.dist2 = cp.Normal(mu, sig2)

    # Probability density function
    def pdf(self, x):

        x = np.asfarray(x)
        x = np.sort(x)

        x1 = x[x<=self.mu]
        y1 = self.dist1.pdf(x1)

        x2 = x[x>self.mu]
        y2 = self.dist2.pdf(x2)

        y = np.concatenate((y1, y2))
        return y

    # Cumulative distribution function
    def cdf(self, x):
        x = np.asfarray(x)
        x = np.sort(x)

        x1 = x[x<=self.mu]
        y1 = self.dist1.cdf(x1)

        x2 = x[x>self.mu]
        y2 = self.dist2.cdf(x2)

        y = np.concatenate((y1, y2))
        return y

# With mode
class Split_Normal():
    """
    The split-normal distribution is a result from joining at the mode
    the corresponding halves of two normal distributions with the same
    mode but two different variances.
    """
    def __init__(self, mode, sig1, sig2):
        self.mode = mode
        self.sig1 = sig1
        self.sig2 = sig2

        pi = np.pi

        # Mean, Std. Dev, Skewness
        self.mu = mode + np.sqrt(2/pi) * (sig2 - sig1)
        self.sigma = np.sqrt((1. -2/pi) * (sig2 - sig1)**2 + sig1 * sig2)
        self.skew = np.sqrt(2/pi)*(sig2 - sig1)*((4/pi - 1)*(sig2 - sig1)**2 + sig1*sig2)

        # Normalizing constant
        self.A = np.sqrt(2./pi) / (sig1 + sig2)

        # Halves distributions
        self.dist1 = cp.Normal(mode, sig1)
        self.dist2 = cp.Normal(mode, sig2)

    # Probability density function
    def pdf(self, x):
        x = np.asfarray(x)
        x = np.sort(x)

        x1 = x[x<=self.mode]
        y1 = 2*self.sig1/(self.sig1 + self.sig2) * self.dist1.pdf(x1)

        x2 = x[x>self.mode]
        y2 = 2*self.sig2/(self.sig1 + self.sig2) * self.dist2.pdf(x2)

        y = np.concatenate((y1, y2))
        return y

    # Cumulative distribution function
    def cdf(self, x):
        x = np.asfarray(x)
        x = np.sort(x)

        x1 = x[x<=self.mode]
        y1 = 2*self.sig1/(self.sig1 + self.sig2)*self.dist1.cdf(x1)

        x2 = x[x>self.mode]
        y2 = 1+ 2*self.sig2/(self.sig1 + self.sig2)*(self.dist2.cdf(x2) - 1)

        y = np.concatenate((y1, y2))
        return y


# to EasyVVUQ with doc
class ValidateCompatibility():
    def __init__(self, weight_factor=0.5):
        """Measure compatibility between two QoI distributions.
        Each distribution is characterized by three moments:
        Mean, variance and skewness.
        Lower metric means higher compatibility.

        Parameters
        ----------
        weight_factor : float, optional
           parameter in [0, 1]
           default: 0.5
        """

        if weight_factor < 0. or weight_factor > 1.:
            raise RuntimeError("Validate_Compatibility: Wrong parameter value.")

        self._weight_factor = weight_factor

    def element_name(self):
        return "Validate_Compatibility"

    def element_version(self):
        return "0.1"

    @property
    def weight_factor(self):
        return self._weight_factor

    @weight_factor.setter
    def weight_factor(self, weight_factor):
        """
        Parameters
        ----------
        weight_factor : float
        """

        if weight_factor < 0. or weight_factor > 1.:
            raise RuntimeError("set_weight_factor: wrong parameter value.")
        self._weight_factor = weight_factor

    def dist(self, mom1, mom2):
        """ Compute distance between

        Parameters
        ----------
        mom1: list
              contains three moments of the first distribution: mean,
              variance and skewness
        mom2: list
              contains three moments of the second distribution: mean,
              variance and skewness

        Returns
        -------

        """
        m1 = mom1[0]
        v1 = mom1[1]
        s1 = mom1[2]
        m2 = mom2[0]
        v2 = mom2[1]
        s2 = mom2[2]
        term1 = (m2 - m1)**2 / (2 * (v1 + v2) + (m2 - m1)**2)
        term2 = (s2 - s1)**2 / (2 * (v1 + v2) + (m2 - m1)**2 + (abs(s1) + abs(s2))**2)
        return (1 - self._weight_factor) * term1 + self._weight_factor * term2


class Ztest():

    def __init__(self, mu1, mu2, sig1, sig2, n1, n2):
    "
    Compare 2 distributions using the Z-test

    Parameters
    ----------
    where mu  = mean
	  sig = standard deviation
	  n   = number of data points
    from a distribution
    "
        self.mu1 = mu1
        self.mu2 = mu2
        self.sig1= sig1
        self.sig2= sig2
        self.n1  = n1
        self.n2  = n2

    def element_name(self):
        return "Z-test"

    def element_version(self):
        return "0.1"

    def score(self)
	"
	compute the z-score between two distributions
	"
	numerator = abs(self.mu1 - self.mu2)
	denominator = np.sqrt(self.sig1**2 / self.n1 + self.sig2**2 / self.n2)
	return numerator / max(denominator, 1.e-20)
