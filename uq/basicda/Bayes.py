import numpy as np
import chaospy as cp
import math as m

import collections
import bisect

from scipy.stats import rv_discrete

from uq.basicda.Integrator import Integrator

from sklearn.neighbors import KernelDensity

class Bayes:
    """
    TODO: should be a class to track distributions and statistics
    TODO Sampler should be a separate class as well
    """

    def __init__(self, n_dim_x=1, n_dim_y=1):
        self.n_dim_x = n_dim_x
        self.n_dim_y = n_dim_y
        self.integrator = Integrator(n_dim_X=n_dim_x)
        return

    def walk_bins(self, y_samples, y_bin_edges): # TODO renormalize everything so bin edges are not passed! Replace by discrete rv!
        """
        Calculate distribution in y as a histogram, considering given samples and bins
        +1 in bin if y e [y_bin_min, y_bin_max)
        y < y_min -> discarded //assigned to first bin <- not discarding leads to high probability at edges if domain is too narrow
        y > y_max -> discarded //assigned to last bin
        :param y_samples: sorted array of y samples
        :param y_bin_edges: array of length y_bins+1 with edges of bins in Y
        :return: count of samples that were assigned to each bin
        // could be array of values e [0.; 1.] of length y_bins, each element is P(y)/Dy probability of y to be in the bin, by bin size
        """
        y_bins = len(y_bin_edges) - 1
        p_y_res = np.zeros(y_bins)

        y_bin_st = 0
        inc = 1
        for y in y_samples:  # no need to store all the x and y values - a lot of if's
            #inc = 1
            # for every y (bin) count x samples to later integrate over X
            if y < y_bin_edges[0]:
                continue
            if y > y_bin_edges[-1]:
                continue
            for y_bin in range(y_bin_st, y_bins):  # TODO replace with bisection?
                if y < y_bin_edges[y_bin + 1]:
                    y_bin_st = y_bin
                    break
            p_y_res[y_bin] += inc  # vectorized np operations for entire loop / a bin?

        return p_y_res

    def weighted_sample(self, p_x, x_n, x_bin_edges):
        """
        Returns a sample from a given pdf, with probability "weight" proportional to p_x
        TODO not tested, returns a dictionary
        Sample x of a discrete weighted random variable
        :param p_x: a piecewise-linear pdf of x
        :param x_n: number of x samples to return
        :param x_bin_edges:
        :return: x samples
        """

        weights = p_x*(x_bin_edges[1:] - x_bin_edges[:-1])
        population = 0.5*(x_bin_edges[1:] + x_bin_edges[:-1])

        tot_weight = weights.sum()
        cdf = weights.cumsum() / tot_weight

        counts = collections.defaultdict(int)
        for i in range(x_n):
            x = np.random.random()
            idx = bisect.bisect(cdf, x)
            counts[population[idx]] += 1

        return counts

    def hist_sample(self, p_x, x_n, x_bin_edges):
        """
        Sample x from a histogram representation of pdf(x)
        :param p_x: a piecewise-linear pdf of x
        :param x_n: number of x samples to return
        :param x_bin_edges:
        :return: x samples from a piecewise-constant pdf
        """

        x_bins = len(x_bin_edges) - 1
        x_samples = []

        # TODO reuse walk_bins(), it is similar to the algorithm for cdf_bins
        # SHOULD BE: inverse-transform of the p-w pdf from a p_x
        cdf_bin_edges = np.zeros((x_bins + 1))
        cdf_bin_edges[1:] = np.cumsum(p_x*(x_bin_edges[1:] - x_bin_edges[:-1]))
        cdf_bin_edges[-1] = 1.

        x_unif = np.random.rand(x_n)
        x_unif = np.sort(x_unif)

        cdf_bin_st = 0
        for x_u in x_unif:
            # should not be needed. BUT a double check if a bug appears here for some reason
            if x_u < cdf_bin_edges[0]:
                continue
            if x_u > cdf_bin_edges[-1]:
                continue
            for cdf_bin in range(cdf_bin_st, x_bins):
                if x_u < cdf_bin_edges[cdf_bin + 1]:
                    cdf_bin_st = cdf_bin
                    break

            # add new uniform r.v. from a bin range

            # x_samples.append(x_bin_edges[x_bin] + (x_bin_edges[x_bin + 1] - x_bin_edges[
            #    x_bin]) * np.random.rand())  # make one rand call for end-sample?

            #x_samples.append(np.random.uniform(x_bin_edges[cdf_bin], x_bin_edges[cdf_bin + 1]))

            x_samples.append(np.random.uniform(0.5*(x_bin_edges[cdf_bin] + x_bin_edges[cdf_bin + 1])))

        return x_samples

    def discrete_hist_sample(self, p_x, x_n, x_bin_edges):
        """
        Samples a realisation of random variable with given p.w.c. pdf
        using scipy rv_discrete class
        :param p_x:
        :param x_n:
        :return:
        """
        # TODO did not compare with hist_sample() yet

        print('scipy pk sum: {}'.format(np.multiply(p_x, x_bin_edges[1:] - x_bin_edges[:-1]).sum()))

        dist = rv_discrete(name='dist', values=(0.5*(x_bin_edges[:-1] + x_bin_edges[1:]),
                                                np.multiply(p_x, (x_bin_edges[1:] - x_bin_edges[:-1]))))

        x_samples = dist.rvs(size=x_n)

        return x_samples

    def kde_sample(self, p_x, x_n, x_bin_edges=None):
        """
        Samples a realisation of a random variable with given kde pdf
        :param p_x: sklearn KernelDensity object, or any object providing .sample() method
        :param x_n: number of samples to draw
        :param x_bin_edges:
        :return: an array of x_n float numbers
        """

        x_samples = p_x.sample(x_n)

        return x_samples

    def compute_normalizing_coefficient(self, p_x, p_y_x, y_bin_edges, n_samples=10_000):
        """
        compute the determinant of bayesian rule i.e.  C(y)=pdf(y)_marg=int_X_{p(y|x)p(x)dx} for given model
        simply calculates the number of resulting y-s in bins, and renormalizes counts to fit p.w.c. pdf
        :param p_x: a chaospy.distribution object OR an array of values of p.w.c. pdf for x e [x_i;x_i+1)
        :param p_y_x: model with predict() method providing function p(y|x) in form of mean and std, and sample_y()
                 method providing sampling r.v. values with p(x|y) distribution
        :param y_bin_edges: an array of edges of 1D bins
        :param n_samples: number of samples to use in MC integration (for marginalization)
        :return: C(y) as p.w.c. pdf for marginalized y
        """

        # if isinstance(p_x, cp.distributions.baseclass.Dist):
        #    p_x = p_x.pdf(np.linspace(*x_domain))

        y_bins = len(y_bin_edges) - 1

        x_n = m.ceil(m.sqrt(n_samples))
        y_n_per_x = int(n_samples / x_n)

        p_y_res = np.zeros((y_bins))

        for x_i in range(x_n):

            # option 1.1: prior is a chaospy object
            x = p_x.sample()

            # option 1.2: prior is p.w.c. with values ofr bins being stored in an array

            # here we draw the same number of y samples for every x already drawn

            # option 2.1: uses sampling methods provided by an ML model
            y_samples = p_y_x.sample_y(x.reshape(-1, 1), n_samples=y_n_per_x).reshape(-1, 1)

            # option 2.2: knowing that conditional distribution is normal, create an r.v. for every x value
            #m_y_x, std_y_x = p_y_x.predict(x.reshape(-1, 1),
            #                               return_std=True)   # consider passing different ML models or just values from data structure
            #dist_y_x = cp.Normal(mu=m_y_x, sigma=std_y_x)
            #y_samples = dist_y_x.sample(y_n_per_x)  # TODO use inverse CDF sampling

            y_samples = np.sort(y_samples)

            p_y_res += self.walk_bins(y_samples, y_bin_edges)

        # check if all y samples where assigned to bins
        # print("p(y) MC counts {} =?= {:1.1}".format(n_samples, p_y_res.sum()))

        # TODO check if this helps to mitigate cancellation effects
        #p_y_res[abs(p_y_res) < n_samples*1e-6] += 1
        p_y_res += 1

        # normalizing discrete distribution to unity
        p_y_res = p_y_res / p_y_res.sum()

        # finding values of piecewise-linear pdf
        p_y_res = np.divide(p_y_res, y_bin_edges[1:] - y_bin_edges[:-1])

        # plot_1d_distrib(y_bin_edges, p_y_res, names={'y': 'y', 'x': 'x~U'})   # does it looke "rugged" only beacuse MC slow convergency? -> try different bin number and samples number

        return p_y_res

    def compute_p_y_kde(self, p_x, p_y_x, y_bin_edges=None, n_samples=10_000):
        """
        compute the determinant of bayesian rule i.e.  C(y)=pdf(y)_marg=int_X_{p(y|x)p(x)dx} for given model
        samples a number of resulting y-s, and fits the kde
        :param p_x: a chaospy.distribution object
        :param p_y_x: model with sample_y() method providing sampling r.v. values with p(x|y) distribution
        :param y_bin_edges: an array of edges of 1D bins
        :param n_samples: number of samples to use in MC integration (for marginalization)
        :return: C(y) as KDE object
        """
        # TODO analyse limits on storage of the samples
        # TODO analyse the influence of bandwidth -> there ara analytic formulas for each distribution

        x_n = m.ceil(m.sqrt(n_samples))
        y_n_per_x = int(n_samples / x_n)

        y_samples_tot = []

        for x_i in range(x_n):
            x = p_x.sample()  # make two options: a chaospy object, or a binned array

            # here we draw the same number of y samples for every x already drawn

            y_samples = p_y_x.sample_y(x.reshape(-1, 1), n_samples=y_n_per_x).reshape(y_n_per_x)

            y_samples_tot.extend(y_samples.tolist())

        y_samples_tot = np.array(y_samples_tot).reshape(-1, 1)  # too large if we use the same scheme as MC

        sigma_y_emp = y_samples_tot.std()

        #kde_bandwidth = (y_bin_edges[1] - y_bin_edges[0]) / (0.05 * (len(y_bin_edges) - 1))
        #kde_bandwidth = (y_bin_edges[1] - y_bin_edges[0]) * 0.15
        #kde_bandwidth = 1.06 * sigma_y_emp * np.power(n_samples, 0.2)  #-> this results in a flat kde density!
        kde_bandwidth = (y_bin_edges[1] - y_bin_edges[0]) / 40.  # Wouter took 40 bins in his pdf calculation with surrogate

        p_y = KernelDensity(kernel='gaussian', bandwidth=kde_bandwidth).fit(y_samples_tot)

        return p_y

    def compute_x_y_posterior(self, p_x_prior, p_y_x, x_bin_edges, y_bin_edges, n_samples=10_000):  # non-unifrom signature, for different p_y_x
        """
        compute the posterior, the parameter distribution p(x|y)=p(y|x)p(x)/p(y), of the given model p(y|x)
        p(y) is integral over x int_{X}_{p(y|x)p(x)} ;  p(x) is prior (given)
        :return: p.w.c. pdf p(x|y) as a 2D array of values p for a grid of XxY inputs
        """

        x_bins = len(x_bin_edges) - 1
        y_bins = len(y_bin_edges) - 1

        x_n = m.ceil(m.sqrt(n_samples))
        y_n_per_x = int(n_samples / x_n)

        # p(x|y) is R^2 -> [0,1]
        p_x_y_res = np.zeros((x_bins, y_bins))

        # for x_i in range(x_n):
        #     x = p_x_prior.sample()
        #
        #     m_y_x, std_y_x = p_y_x.predict(x.reshape(-1, 1), return_std=True)
        #     dist_y_x = cp.Normal(mu=m_y_x, sigma=std_y_x)  # consider non-normal distributions
        #
        #     y_samples_loc = dist_y_x.sample(y_n_per_x)
        #     y_samples_loc = np.sort(y_samples_loc)
        #
        #     # iterate over all x_bins, y_samples, y_bins and count? how to do it simpler for e.g. gaussian*gaussian?
        #
        #     for x_bin in range(x_bins):
        #         if x_bin_edges[x_bin] <= x < x_bin_edges[x_bin + 1]:
        #             break
        #     p_x_y_res[x_bin, :] = p_x_y_res[x_bin, :] + walk_bins(y_samples_loc, y_bin_edges)


        #p(y|x) is produced as a piecewise-linear pdf with its values written in an array
        p_y_x_array = np.zeros((len(x_bin_edges) - 1, len(y_bin_edges) - 1))
        for x_i in range(len(x_bin_edges) - 1):
            x_val = (0.5*(x_bin_edges[1:] + x_bin_edges[:-1]))[x_i]
            y_mean, y_std = p_y_x.predict(x_val.reshape(-1, 1), return_std=True)
            y_pdf = cp.Normal(y_mean, y_std)
            p_y_x_array[x_i, :] = y_pdf.pdf(0.5*(y_bin_edges[1:] + y_bin_edges[:-1]))
            # REPLACE p.w.c.array-isation!

        # p(y) this should be an integral over x
        c_y = self.compute_normalizing_coefficient(p_x_prior, p_y_x, y_bin_edges)

        # p(x), piecewise-linear pdf in an array produced
        p_x_prior_array = p_x_prior.pdf(0.5*(x_bin_edges[:-1] + x_bin_edges[1:]))

        #plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res, {'label': 'p(x|y) counts unnormalized'})

        for y_i in range(y_bins):  # TODO (important) formulate in matrix notation (how?)
                                   # -> with representation of every integrand as linear combiantion of basis function,
                                   # the integration can be perfromed for basis only

            # # normalize counts to [0;1] interval for every y
            # # int_{X}_{p(x|y)dx}=1 for any y
            # p_x_y_res[:, y_i] = p_x_y_res[:, y_i] / p_x_y_res[:, y_i].sum()

            # # finding values of piecewise-linear pdf
            # p_x_y_res[:, y_i] = np.divide(p_x_y_res[:, y_i], x_bin_edges[1:] - x_bin_edges[:-1])


            # #print('for p(x|y~{:1.2}) check on prob sum: 1.0~={:1.3}'.format((
            # #                        y_bin_edges[y_i+1] + y_bin_edges[y_i])*0.5,
            # #                        np.multiply(p_x_y_res[:, y_i], x_bin_edges[1:] - x_bin_edges[:-1]).sum()))


            # works only if number of bins in y in c_y is the same as p_x_y -> use interpolation of p(y) if not
            # p(x|y)=p(y|x)p(x)/p(y)
            p_x_y_res[:, y_i] = np.multiply(p_y_x_array[:, y_i], p_x_prior_array[:])
            p_x_y_res[:, y_i] = p_x_y_res[:, y_i] / c_y[y_i]

        #plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res)  # passing not probabilities, but still counts # also some values at edges are nans

        return p_x_y_res, c_y

    def compute_x_distribution(self, p_y, p_x_y, x_bin_edges, y_bin_edges, n_samples=1_000_000):
        """
        computes the results of the surrogate bayesian inversion p(x) = int_Y_{p(x|y)p(y)}
        TODO can it be same function as compute_normalizing_coefficient() ?

        TODO (conceptual)(important) conditional probabilities could be defined through a surrogate (?)
        TODO options --- choose one!
        1) train another GP for posterior
        2) Inverse as a polynomial surrogate

        #TODO hence already regions of high posterior density are observed, add maximum variane MC (with measure transformation) -> is it the same as importance sampling?
        #TODO for marginalization: the distribution might be narrow enough to take determenistic values for some cases
        Options to sample from p(x|y):
        1) MCMC
        2) particle filter
        3) importance sampling -> formulate R-N derivative based on the shape of p(x|y) (find new suitable probability space were it is close to uniform)

        the only difference from calculating normalizing coefficient c_y is signature, here p_x_y is not an ML model but an array of p.w.c. pdf val-s
        :return:
        """

        x_bins = len(x_bin_edges) - 1
        y_bins = len(y_bin_edges) - 1

        y_n = m.ceil(m.sqrt(n_samples))
        x_n_per_y = int(n_samples / y_n)

        p_x_res = np.zeros(x_bins)
        x_samples = np.zeros(x_n_per_y)

        #debug_discard_y_n = 0
        for y_i in range(y_n):

            y = p_y.sample()
            # define for which bin of y we sample x
            # debug: check for y samples outside of domain
            if y < y_bin_edges[0]:
                continue
            if y >= y_bin_edges[-1]:
                continue
            for y_bin in range(y_bins):  # get a batch of y samples
                if y_bin_edges[y_bin] <= y < y_bin_edges[y_bin + 1]:
                    break

            # draw samples using p_y_x histogram representation
            x_samples = self.hist_sample(p_x_y[:, y_bin], x_n_per_y, x_bin_edges)
            x_samples = np.sort(x_samples)

            # debug check for unusually high p(x)@x_max - > did not help
            #x_samples = x_samples[x_samples >= x_bin_edges[0]]
            #x_samples = x_samples[x_samples < x_bin_edges[-1]]

            p_x_res += self.walk_bins(x_samples, x_bin_edges)

        # print("p(x) MC counts: {} =?= {:1.1}".format(n_samples, p_x_res.sum()))
        # print("Number of y samples discarded due to lying outside of support is {}".format(debug_discard_y_n))

        # normalizing discrete distribution to unity
        p_x_res = p_x_res / p_x_res.sum()

        # finding values of piecewise-constant pdf
        p_x_res = np.divide(p_x_res, x_bin_edges[1:] - x_bin_edges[:-1])

        return p_x_res
