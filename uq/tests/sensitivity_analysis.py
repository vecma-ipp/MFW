import chaospy as cp


# Test of SA for GEM production

input_samples = []
uncertain_parameters = []

qoi = []
output_samples = {}
for q in qoi:
    output_sample[q] =

polynomial_order = 3

# distribution is a list to be computed from input samples
distribution = cp.SampleDist(input_samples)

# Create the Multivariate normal distribution
dist_R = []
for parameter in uncertain_parameters:
    dist_R.append(cp.Normal())

dist_R = cp.J(*dist_R)

P = cp.orth_ttr(polynomial_order, dist_R)

nodes_R, weights_R = cp.generate_quadrature(quadrature_order,
                                            dist_R,
                                            rule="J",
                                            sparse=True)


nodes = distribution.inv(dist_R.fwd(nodes_R))
# weights = weights_R*distribution.pdf(nodes)/dist_R.pdf(nodes_R)

# QoI approximation
for q in qoi:
    fit = cp.fit_quadrature(P, nodes, weights_R, samples[k])
    sobols_first_narr = cp.Sens_m(fit, distribution)
    i_par = 0
    for parameter in uncertain_parameters:
    sobols_first_dict[param_name] = sobols_first_narr
