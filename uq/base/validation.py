import logging
import numpy as np
import chaospy as cp


# TODO issue for Chaospy
class Split_Normal():
    """
    The two pieces normal distribution is a result from joining halves of
    two normal distributions with the same mean but two different
    variances which gives an asymmetric normal distribution.
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


# TODO to EasyVVUQ with doc (Onnies paper)
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

# TODO also to Easyvvuq
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
