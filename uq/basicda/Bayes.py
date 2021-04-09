import numpy as np
import chaospy as cp
import math as m

import collections
import bisect
from itertools import product

from scipy.stats import rv_discrete

from uq.basicda.Integrator import Integrator

from sklearn.neighbors import KernelDensity

class Bayes:
    """
    TODO sampler should be a separate class as well
    """

    def __init__(self, n_dim_x=1, n_dim_y=1):
        self.n_dim_x = n_dim_x
        self.n_dim_y = n_dim_y
        self.integrator = Integrator(n_dim_X=n_dim_x)

    def set_surrogate(self, p_x_y):
        self.p_y_x_surrogate_likelihood = p_x_y

    def set_domain_bins(self, x_domain, y_domain, x_n_bins, y_n_bins, n_dim=1):
        self.x_domain = x_domain
        self.y_domain = y_domain

        self.x_n_bins = x_n_bins
        self.y_n_bins = y_n_bins

        if n_dim == 1:
            self.x_bin_edges = np.linspace(x_domain[0], x_domain[1], x_n_bins + 1).reshape(-1)
        elif n_dim == 2:
            self.x_bin_edges = [np.linspace(x_domain[0][0], x_domain[0][1], x_n_bins[0] + 1).reshape(-1),
                                np.linspace(x_domain[1][0], x_domain[1][1], x_n_bins[1] + 1).reshape(-1)]
        self.y_bin_edges = np.linspace(y_domain[0], y_domain[1], y_n_bins + 1).reshape(-1)

        if n_dim == 1:
            self.x_mids = 0.5 * (self.x_bin_edges[:-1] + self.x_bin_edges[1:])
        if n_dim == 2:
            self.x_mids = [0.5 * (self.x_bin_edges[0][:-1] + self.x_bin_edges[0][1:]),
                           0.5 * (self.x_bin_edges[1][:-1] + self.x_bin_edges[1][1:])]

        self.y_mids = 0.5 * (self.y_bin_edges[:-1] + self.y_bin_edges[1:])

        if n_dim == 0:
            self.x_bins_hs = self.x_bin_edges[1:] - self.x_bin_edges[:-1]
        elif n_dim == 1:
            self.x_bins_hs = [self.x_bin_edges[0][1:] - self.x_bin_edges[0][:-1],
                              self.x_bin_edges[1][1:] - self.x_bin_edges[1][:-1]]
        self.y_bins_hs = self.y_bin_edges[1:] - self.y_bin_edges[:-1]

    def walk_bins(self, y_samples, y_bin_edges):  # TODO renormalize everything so bin edges are not passed!
        """
        Calculate distribution in y as a histogram, considering given samples and bins
        +1 in bin if y e [y_bin_min, y_bin_max)
        y < y_min -> discarded //assigned to first bin <- not discarding leads to high probability at edges if domain is too narrow
        y > y_max -> discarded //assigned to last bin
        :param y_samples: sorted list of y samples
        :param y_bin_edges: array of length y_bins+1 with edges of bins in Y
        :return: count of samples that were assigned to each bin
        // could be array of values e [0.; 1.] of length y_bins, each element is P(y)/Dy probability of y to be in the bin, by bin size
        """
        y_bins = len(y_bin_edges) - 1
        p_y_res = np.zeros(y_bins)
        bin_nums = []

        if isinstance(y_samples, np.ndarray):
            y_samples = y_samples.tolist()

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
            bin_nums.append(y_bin)

        return p_y_res, np.array(bin_nums)

    def get_bin_number(self, y, y_bin_edges):
        y_bins = len(y_bin_edges) - 1
        y_bin_def = 0

        if y < y_bin_edges[0]:
            return y_bin_def
        if y > y_bin_edges[-1]:
            return -1

        for y_bin in range(y_bin_def, y_bins):  # TODO replace with bisection?
            if y < y_bin_edges[y_bin + 1]:
                return y_bin

        return y_bin_def

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

    def hist_sample(self, p_x, x_n, x_bin_edges):  # TODO Replace by discrete rv!
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
        #cdf_bin_edges[-1] = 1.

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

            x_samples.append(np.random.uniform(x_bin_edges[cdf_bin], x_bin_edges[cdf_bin + 1]))

            #x_samples.append(np.random.uniform(0.5*(x_bin_edges[cdf_bin] + x_bin_edges[cdf_bin + 1])))

        return x_samples

    def discrete_hist_sample(self, p_x, x_n, x_bin_edges):
        """
        Samples a realisation of random variable with given p.w.c. pdf
        using scipy rv_discrete class
        :param p_x:
        :param x_n:
        :return:
        """
        # TODO was not compared with hist_sample() yet

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

    def log_likelihood_x_y(self, x, y, p_y_x, p_pr_x, p_ev_y=None):
        """
        Retruns l_x|y(.) value for given x value
        :param x:
        :param y:
        :param p_y_x: sklearn model object, or any object providing .predict(x) method
        :param p_pr_x: chasopy distribution object, or any object providing .mom(i) method
        :param p_ev_y: sklearn KDE object, or any object providing .score_samples(y) method
        :return:
        """
        if p_ev_y is not None:  # TODO:  vectorize the function, should be doable
            log_p_ev_y = p_ev_y.score_samples(np.array(y).reshape(-1, 1))
        else:
            log_p_ev_y = 0.0

        l_pr_x = 0.0
        if isinstance(p_pr_x, cp.Uniform):
            l_pr_x = -np.log(p_pr_x.upper - p_pr_x.lower)
        if isinstance(p_pr_x, cp.Normal):
            mu_pr_x = p_pr_x.mom(1)
            sigma_pr_x = np.sqrt(p_pr_x.mom(2) - mu_pr_x * mu_pr_x)
            l_pr_x = -0.5*np.log(np.pi) - np.log(sigma_pr_x) - (x-mu_pr_x)*(x-mu_pr_x)/(2*sigma_pr_x*sigma_pr_x)

        mu_lh_y_x, sigma_lh_y_x = p_y_x.predict(np.array(x).reshape(-1, 1), return_std=True)

        l_x_y = -0.5*np.log(np.pi) - np.log(sigma_lh_y_x) - \
                (y - mu_lh_y_x)*(y - mu_lh_y_x) / (2*sigma_lh_y_x*sigma_lh_y_x) - \
                l_pr_x - log_p_ev_y
        return l_x_y

    def compute_p_y(self, p_x=None, p_y_x=None, y_bin_edges=None, n_samples=10_000):
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

        x_n = m.ceil(m.sqrt(n_samples))
        y_n_per_x = int(n_samples / x_n)

        p_y_res = np.zeros(self.y_n_bins)

        for x_i in range(x_n):

            # option 1.1: prior is a chaospy object
            x = p_x.sample()

            # option 1.2: prior is p.w.c. with values ofr bins being stored in an array

            # here we draw the same number of y samples for every x already drawn

            # option 2.1: uses sampling methods provided by an ML model
            y_samples = self.p_y_x_surrogate_likelihood.sample_y(x.reshape(-1, 1), n_samples=y_n_per_x).reshape(-1, 1)

            # option 2.2: knowing that conditional distribution is normal, create an r.v. for every x value
            #m_y_x, std_y_x = p_y_x.predict(x.reshape(-1, 1),
            #                               return_std=True)   # consider passing different ML models or just values from data structure
            #dist_y_x = cp.Normal(mu=m_y_x, sigma=std_y_x)
            #y_samples = dist_y_x.sample(y_n_per_x)  # TODO use inverse CDF sampling

            y_samples = np.sort(y_samples)

            p_y_temp, _ = self.walk_bins(y_samples, self.y_bin_edges)
            p_y_res += p_y_temp

        # check if all y samples where assigned to bins
        # print("p(y) MC counts {} =?= {:1.1}".format(n_samples, p_y_res.sum()))

        # TODO check if this helps to mitigate cancellation effects
        #p_y_res[abs(p_y_res) < n_samples*1e-6] += 1
        p_y_res += 1

        # normalizing discrete distribution to unity
        p_y_res = p_y_res / p_y_res.sum()

        # finding values of piecewise-linear pdf
        p_y_res = np.divide(p_y_res, self.y_bins_hs)

        # plot_1d_distrib(y_bin_edges, p_y_res, names={'y': 'y', 'x': 'x~U'})   # does it looke "rugged" only beacuse MC slow convergency? -> try different bin number and samples number

        return p_y_res

    def compute_p_y_kde(self, p_x, p_y_x=None, y_bin_edges=None, ndim=1, n_samples=10_000):
        """
        compute the determinant of bayesian rule i.e.  C(y)=pdf(y)_marg=int_X_{p(y|x)p(x)dx} for given model
        samples a number of resulting y-s, and fits the kde
        :param p_x: a chaospy.distribution object for a distribution over a X c R^ndim
        :param p_y_x: model with sample_y() method providing sampling r.v. values with p(y|x) distribution
        :param y_bin_edges: an array of edges of 1D bins
        :param n_samples: number of samples to use in MC integration (for marginalization)
        :return: C(y) as KDE object
        """
        # TODO analyse limits on storage of the samples
        # TODO analyse the influence of bandwidth -> there are analytic formulas for each distribution

        x_n = m.ceil(m.pow(n_samples, ndim/float(ndim+1)))
        y_n_per_x = int(n_samples / x_n)

        y_samples_tot = []

        for x_i in range(x_n):
            x = p_x.sample()  # make two options: a chaospy object, or a binned array

            y_samples = self.p_y_x_surrogate_likelihood.sample_y(x.reshape(-1, ndim), n_samples=y_n_per_x).reshape(y_n_per_x)

            y_samples_tot.extend(y_samples.tolist())

        y_samples_tot = np.array(y_samples_tot).reshape(-1, 1)  # too large if we use the same scheme as MC

        sigma_y_emp = y_samples_tot.std()

        #kde_bandwidth = (y_bin_edges[-1] - y_bin_edges[0]) / (0.05 * (len(y_bin_edges) - 1))
        #kde_bandwidth = (y_bin_edges[-1] - y_bin_edges[0]) * 0.15
        #kde_bandwidth = 1.06 * sigma_y_emp * np.power(n_samples, 0.2)  #-> this results in a flat kde density!
        kde_bandwidth = (self.y_bin_edges[-1] - self.y_bin_edges[0]) / 40.  # Wouter took 40 bins in his pdf calculation with surrogate

        p_y = KernelDensity(kernel='gaussian', bandwidth=kde_bandwidth).fit(y_samples_tot)

        self.p_y_marginal_evidence = p_y

        return p_y

    def compute_p_y_x_part(self, p_x, p_y_x=None, ndim=2, ndim_active=1, n_samples=10_000):
        """
        Marginalizes a distribution over a subset of dimensions I={i}
        if i=2 then  p(y|x1) = Int_X2_(p(y|x1,x1)p(x2)dx2)
        #TODO has to work in th same way as .compute_x_distribution_kde() ???
        :param p_x: original multivariate prior distribution of inputs, a cp.J object
        :param p_y_x: original multivariate likelihood distribution, a surrogate object for multivariate model
        :param x_bin_edges: list of arrays with domain bin edges
        :param y_bin_edges: array of range bin edges
        :param ndim: number of dimensions of an input
        :param ndim_active: numbers of features or input component which is marginalised out, count from 0
        :param n_samples: number of x samples
        :return: array for p.w.c. distribution p(y|x_{-i})
        """

        inc = 1
        # n_dim_left = abs(ndim_active - 1)  # for total 2D case

        dims = [x for x in np.arange(ndim)]
        dims_out = ndim_active
        dims_left = tuple([x for x in dims if x not in dims_out])

        x_n = m.ceil(m.pow(n_samples, ndim/float(ndim+1)))
        # x_n_per_d = m.floor(m.pow(x_n, 1./float(ndim)))
        y_n_per_x = int(n_samples / x_n)

        bin_res_sizes = (self.y_n_bins, ) + tuple([self.x_n_bins[d] for d in dims_left])
        p_y_x_res = np.zeros(bin_res_sizes)

        for x_i in range(x_n):

            x_sample = p_x.sample()

            # define for which x-bin of YxX we sample x
            for n_dim_left_new in range(len(dims_left)):
                x_bin_list = []
                if x_sample[n_dim_left_new] < self.x_bin_edges[n_dim_left_new][0]:
                    continue
                if x_sample[n_dim_left_new] >= self.x_bin_edges[n_dim_left_new][-1]:
                    continue
                for x_bin in range(self.x_n_bins[n_dim_left_new]):  # get a batch of y samples
                    if self.x_bin_edges[n_dim_left_new][x_bin] <= x_sample[n_dim_left_new] \
                            < self.x_bin_edges[n_dim_left_new][x_bin + 1]:
                        x_bin_list.append(x_bin)
                        break

            y_sample = self.p_y_x_surrogate_likelihood.sample_y(
                x_sample.reshape(-1, ndim), n_samples=y_n_per_x).reshape(-1).tolist()

            for y in y_sample:
                # define for which y-bin of YxX we sample x
                if y < self.y_bin_edges[0]:
                    continue
                if y >= self.y_bin_edges[-1]:
                    continue
                for y_bin in range(self.y_n_bins):  # get a batch of y samples
                    if self.y_bin_edges[y_bin] <= y < self.y_bin_edges[y_bin + 1]:
                        break

                bin_ind = (y_bin, ) + tuple(x_bin_list)
                p_y_x_res[bin_ind] += inc

        # normalizing discrete distribution to unity
        # for n_dim_left_new in range(len(dims_left)):
        #     for x in range(self.x_n_bins[n_dim_left_new]):
        #         xind = tuple()
        #         p_y_x_res[:, ind] = p_y_x_res[:, ind] / p_y_x_res.sum(0)

        # norms = p_y_x_res.sum(axis=0)
        # for xinds in product(range(self.x_n_bins[0]), range(self.x_n_bins[1])):
        #     p_y_x_res[:, xinds[0], xinds[1]] = norms[xinds]  # workaround for ndim=2

        for x in range(p_y_x_res.shape[1]):
            p_y_x_res[:, x] = p_y_x_res[:, x] / p_y_x_res[:, x].sum()

        return p_y_x_res

    def compute_x_y_posterior(self, p_x_prior, p_y_x=None, x_bin_edges=None, y_bin_edges=None, n_samples=10_000):  # non-unifrom signature, for different p_y_x
        """
        compute the posterior, the parameter distribution p(x|y)=p(y|x)p(x)/p(y), of the given model p(y|x)
        p(y) is integral over x int_{X}_{p(y|x)p(x)} ;  p(x) is prior (given)
        :return: p.w.c. pdf p(x|y) as a 2D array of values p for a grid of XxY inputs
        """

        x_n = m.ceil(m.sqrt(n_samples))
        y_n_per_x = int(n_samples / x_n)

        # p(x|y) is R^2 -> [0,1]
        p_x_y_res = np.zeros((self.x_n_bins, self.y_n_bins))

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
        p_y_x_array = np.zeros((self.x_n_bins, self.y_n_bins))
        for x_i in range(self.x_n_bins):
            x_val = self.x_mids[x_i]
            y_mean, y_std = self.p_y_x_surrogate_likelihood.predict(x_val.reshape(-1, 1), return_std=True)
            y_pdf = cp.Normal(y_mean, y_std)
            p_y_x_array[x_i, :] = y_pdf.pdf(self.y_mids)
            # REPLACE p.w.c.array-isation!

        # p(y) this should be an integral over x
        c_y = self.compute_p_y(p_x_prior)

        # p(x), piecewise-linear pdf in an array produced
        p_x_prior_array = p_x_prior.pdf(self.x_mids)

        #plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res, {'label': 'p(x|y) counts unnormalized'})

        for y_i in range(self.y_n_bins):  # TODO (important) formulate in matrix notation (how?)
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

        self.p_x_y_posterior = p_x_y_res

        return p_x_y_res

    def compute_x_y_posterior_ndim(self, p_x_prior, p_y_x=None, c_y=None, x_bin_edges=None, y_bin_edges=None, ndim=2):  # non-unifrom signature, for different p_y_x
        """
        compute the posterior, the parameter distribution p(x|y)=p(y|x)p(x)/p(y), of the given model p(y|x)
        p(y) is integral over x int_{X}_{p(y|x)p(x)dx} ;  p(x) is prior over X c R^ndim (given)
        :return: p.w.c. pdf p(x|y) as a (ndim+1)D array of values p for a grid of Xi x...x Xndim x Y inputs
        """
        # TODO find apropriate expression in terms of kernels as basis functions

        # p(x|y) is R^2 -> [0,1]
        p_x_y_res = np.zeros((self.x_n_bins, self.y_n_bins))

        # --- calculation using pointwise calls for (x,y)
        if isinstance(self.p_y_marginal_evidence, KernelDensity):
            for x in self.x_mids.tolist():
                for y in self.y_mids.tolist():
                    p_x_y_res = np.exp(self.log_likelihood_x_y(x, y, self.p_y_x_surrogate_likelihood, p_x_prior, self.p_y_marginal_evidence))

        # --- calculation values for bins
        #p(y|x) is produced as a piecewise-linear pdf with its values written in an array
        p_y_x_array = np.zeros((self.x_n_bins, self.y_n_bins))
        for x_i in range(self.x_n_bins):
            x_val = self.x_mids[x_i]
            y_mean, y_std = self.p_y_x_surrogate_likelihood.predict(x_val.reshape(-1, 1), return_std=True)
            y_pdf = cp.Normal(y_mean, y_std)
            p_y_x_array[x_i, :] = y_pdf.pdf(self.y_mids)
            # REPLACE p.w.c.array-isation!

        #c_y = self.compute_p_y_kde(p_x_prior, p_y_x, ndim=ndim, y_bin_edges=y_bin_edges)

        # p(x), piecewise-linear pdf in an array produced
        p_x_prior_array = p_x_prior.pdf(self.x_mids)

        #plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res, {'label': 'p(x|y) counts unnormalized'})

        for y_i in range(self.y_n_bins):  # TODO (important) formulate in matrix notation (how?)
                                   # -> with representation of every integrand as linear combiantion of basis function,
                                   # the integration can be performed for basis only

            # works only if number of bins in y in c_y is the same as p_x_y -> use interpolation of p(y) if not
            # p(x|y)=p(y|x)p(x)/p(y)
            p_x_y_res[:, y_i] = np.multiply(p_y_x_array[:, y_i], p_x_prior_array[:])
            p_x_y_res[:, y_i] = p_x_y_res[:, y_i] / self.p_y_marginal_evidence[y_i]

        #plot_2d_distrib(x_bin_edges, y_bin_edges, p_x_y_res)  # passing not probabilities, but still counts # also some values at edges are nans

        self.p_x_y_posterior = p_x_y_res

        return p_x_y_res

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

            p_x_temp, _ = self.walk_bins(x_samples, x_bin_edges)
            p_x_res += p_x_temp

        # print("p(x) MC counts: {} =?= {:1.1}".format(n_samples, p_x_res.sum()))
        # print("Number of y samples discarded due to lying outside of support is {}".format(debug_discard_y_n))

        # normalizing discrete distribution to unity
        p_x_res = p_x_res / p_x_res.sum()

        # finding values of piecewise-constant pdf
        p_x_res = np.divide(p_x_res, x_bin_edges[1:] - x_bin_edges[:-1])

        return p_x_res

    def compute_x_distribution_kde(self, p_y, p_x_y, x_bin_edges=None, y_bin_edges=None, ndim=1, n_samples=1_000_000):
        """
        compute the determinant of bayesian rule i.e.  C(y)=pdf(y)_marg=int_X_{p(y|x)p(x)dx} for given model
        samples a number of resulting y-s, and fits the kde
        :param p_x: a chaospy.distribution object for a distribution over a X c R^ndim
        :param p_y_x: model with sample_y() method providing sampling r.v. values with p(y|x) distribution
        :param y_bin_edges: an array of edges of 1D bins
        :param n_samples: number of samples to use in binning (for marginalization)
        :return: marginal posterior p(x) as KDE object
        """

        x_n_per_y = m.ceil(m.pow(n_samples, ndim/float(ndim+1)))
        y_n = int(n_samples / x_n_per_y)

        x_samples_tot = []

        for y_i in range(y_n):  # TODO batch/vectorise/replace
            y = p_y.sample()

            y_bin = self.get_bin_number(y, self.y_bin_edges)
            x_samples = self.hist_sample(self.p_x_y_posterior[:, y_bin], x_n_per_y, self.x_bin_edges)  # TODO: THIS HAS TO BE RAPLACED BY MCMC

            x_samples_tot.extend(x_samples)

        x_samples_tot = np.array(x_samples_tot).reshape(-1, 1)

        sigma_x_emp = x_samples_tot.std()
        #kde_bandwidth = 1.06 * sigma_x_emp * np.power(n_samples, 0.2)
        kde_bandwidth = (self.x_bin_edges[-1] - self.x_bin_edges[0]) / 40.

        p_x = KernelDensity(kernel='gaussian', bandwidth=kde_bandwidth).fit(x_samples_tot)

        self.p_x_marginal_posterior = p_x

        return p_x

    def map_dist_loss_term(self, x, y_star, p_x_res):
        """
        Returns a value of the additive term of a loss function, which optimization defines
        the active learning scheme for a surrogate; The loss term is distance to the MAP of the input parameter
        for given target value
        :return:
        """
        x_map = p_x_res.max()  # TODO stor the functions as class members and recalculate for new y prior distributions
        return (x - x_map) * (x - x_map)

    def compute_first_sobol(self, p_x_prior, n_dim_out, n_dim_stay):
        """
        Computes a first Sobol index using the given likelihood
        :param n_dim_out:
        :param n_dim_stay: number of paramaters (input dimesion) for which the Sobol index is computed
        :return:
        """

        n_dim_out = (1,)
        n_dim_stay = 0

        p_y_kde = self.compute_p_y_kde(p_x_prior, ndim=2, n_samples=40_000)

        E_y_gp = self.integrator.integrate_with_measure(self.y_bin_edges, p_y_kde, rule="kde_riemann")
        Var_y_gp = self.integrator.integrate_with_measure(self.y_bin_edges, p_y_kde, lambda x: x * x, rule="kde_riemann") \
                   - E_y_gp * E_y_gp

        p_y_x1_x2 = self.compute_p_y_x_part(p_x_prior, ndim_active=n_dim_out)  # dp(y,x1|x2)

        # Next two object should represent functions over X1 domain
        E_y_x1_gp = np.zeros(self.x_n_bins[n_dim_stay])
        Var_y_x1_gp = np.zeros(self.x_n_bins[n_dim_stay])
        for x1 in range(self.x_n_bins[n_dim_stay]):
            E_y_x1_gp[x1] = self.integrator.integrate_with_measure(self.y_bin_edges, p_y_x1_x2[:, x1], rule="riemann")
            Var_y_x1_gp[x1] = self.integrator.integrate_with_measure(self.y_bin_edges, p_y_x1_x2[:, x1],
                                                                      lambda x: x * x, rule="riemann") - E_y_x1_gp[x1] * \
                              E_y_x1_gp[x1]

        p_x1 = p_x_prior[n_dim_stay].pdf(self.x_mids[n_dim_stay]).reshape(-1)

        Var_y_x1_gp_func = lambda x: Var_y_x1_gp[self.walk_bins(x, self.x_bin_edges[n_dim_stay])[-1]]

        E_Var_y_x1_gp = self.integrator.integrate_with_measure(self.x_bin_edges[n_dim_stay],
                                                                p_x1, lambda x: Var_y_x1_gp_func(x), rule='riemann')
        S_1_gp = (Var_y_gp - E_Var_y_x1_gp) / Var_y_gp

        return S_1_gp
