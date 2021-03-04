import numpy as np

from scipy.integrate import quad


class Integrator:

    def __init__(self, n_dim_X=1):
        self.n_dim_X = 1
        return

    def integrate_with_measure(self, X_domain, dpx, f=(lambda x: x), rule="trapezoidal"):
        """
        Calculated an integral Int_{X_domain}_(f(x)dp(x))
        Accepts X_domain c R^1
        Performs computation assuming dp(x) is a p.w.c pdf over X_domain
        applies a simplest trapezoidal integration
        :param X_domain: edges of bins of the integration domain X
        :param dpx: p.w.c. measure dp(x) on X
        :param f: function of x which we integrate
        :return: integrate value, a scalar
        """

        integration_result = 0.

        if rule == "trapezoidal":
            # i = 0
            # for (x_a, x_b) in zip(X_domain[:-1], X_domain[1:]):
            #     integration_result += 0.5 * (x_b - x_a) * (f(x_a) + f(x_b)) * dpx[i]
            #     i += 1

            #for i in range(len(dpx)):  # different from midpoint, the extremes are re-weighted
            #    integration_result += 0.5 * (X_domain[i+1] - X_domain[i]) * (f(X_domain[i]) + f(X_domain[i+1])) * dpx[i]

            h_s = (X_domain[1:] - X_domain[:-1])
            f_w_s = 0.5 * (f(X_domain[1:]) + f(X_domain[:-1]))
            integration_result = sum(h_s * f_w_s * dpx)  #TODO Why pdf is not == 0.0, but accumulates in the begining?

        if rule == "simpson_1_3":  # RETHINK! Applicable if f(.) and pdx(.) could be defined for any x e X

            h_s = (X_domain[1:] - X_domain[:-1])
            f_w_s = f(X_domain[:-2]) + 4*f(X_domain[1:-1]) + f(X_domain[2:]) / 6.
            integration_result = h_s * f_w_s * dpx

        if rule == "riemann":

            h_s = (X_domain[1:] - X_domain[:-1])
            x_mids = 0.5*(X_domain[:-1] + X_domain[1:])
            f_w_s = f(x_mids)
            integration_result = sum(h_s * f_w_s * dpx)

        if rule == "kde_riemann":
            # pdf is a KDE object providing .score(X) method that returns log-pdf
            # KDE is O(n^2), whereas p.w.c. computation is O(n) -> check the scaling in practice!
            # could be resolved during a dispatch and merged with a riemann rule

            # NOTE 1: Replacing a binning computation with KDE is equivalent to a basis change
            # (double check the underlying formulas, since kernel is a function of metric value, and a bin-function is not)
            # in first case, the pdf function is decomposed into a L.C. of rectangular func-s with finite support
            # in the second case, the basis is in kernel functions, and weight is a position of a sample

            # NOTE 2: GP is also a decomposition of likelihood using gaussian kernels as a set of basis functions,
            # Could the posterior computation be performed easily, for example by multiplication of
            # matrix and vector of corresponding weights?

            h_s = (X_domain[1:] - X_domain[:-1])
            x_mids = 0.5 * (X_domain[:-1] + X_domain[1:])
            f_w_s = f(x_mids)
            pdf_vals = np.exp(dpx.score_samples(0.5*(X_domain[:-1] + X_domain[1:]).reshape(-1, 1)))
            integration_result = sum(h_s * f_w_s * pdf_vals)

        if rule == "b_quad":
            # This needs tighter integration with bayes, since a marginal integration would require access to surrogate
            # p(y|x) posterior
            raise RuntimeError("Integration rule {} is not implemented yet.".format(rule))

        return integration_result

