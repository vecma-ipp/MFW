import os
import time
import numpy   as np
import chaospy as cp
import matplotlib.pylab as plt
from ascii_cpo import read
from shutil    import copyfile
from wrappers  import run_ets, update_coret_file

# To compute elapsed time
start_time = time.time()

# Inputs: cpo files
data_path  = "../data/ETS/gem_spread_4"
coret_file = data_path + "/ets_coretransp_in.cpo"
coret      = read(coret_file, "coretransp")

# Hermite polynomial order and numper of uncertain parameters
k = 3

# Uncertain parameters uncertainty and distrubutions
npar = 4
diff_eff = coret.values[0].te_transp.diff_eff
c0 = cp.Normal(diff_eff[0], 0.2*diff_eff[0])
c1 = cp.Normal(diff_eff[1], 0.2*diff_eff[1])
c2 = cp.Normal(diff_eff[2], 0.2*diff_eff[2])
c3 = cp.Normal(diff_eff[3], 0.2*diff_eff[3])

# Joint probability distribution
dist = cp.J(c0, c1, c2, c3)

# Generate orthogonal polynomials corresponding to the joint disctribution
P = cp.orth_ttr(k, dist)

# Quadrature nodes and wights
nodes, w = cp.generate_quadrature(order=k+1, domain=dist, rule='G')

# Evaluate the computational model in the sample points (nodes)
samples_te = []
for i in range((k+2)**npar):
    tmp_dir  = data_path + '/tmp_' + str(i)
    tmp_file = tmp_dir + '/ets_coretransp_in.cpo'
    if not os.path.exists(tmp_dir):
        os.makedirs(tmp_dir)
    copyfile(coret_file, tmp_file)

    update_coret_file(tmp_file, npar, nodes.T[i])
    run_ets(data_path, i)

    corep_file = tmp_dir + '/ets_coreprof_out.cpo'
    corep      = read(corep_file, "coreprof")
    samples_te.append(corep.te.value)

# Create approximate solver
te_hat = cp.fit_quadrature(P, nodes, w, samples_te)

# Statistical infos
mean = cp.E(te_hat, dist)
var  = cp.Var(te_hat, dist)
std  = cp.Std(te_hat, dist)

#
elapsed_time = time.time() - start_time
print('>>> Elapsed time = ', elapsed_time)

# Plots
corep_file = data_path + '/ets_coreprof_in.cpo'
corep      = read(corep_file, "coreprof")
rho = corep.rho_tor_norm
te  = corep.te.value

# ... Mean +- deviation
m1 = []
m2 = []
for i in range(100):
    m1.append(mean[i] + std[i])
    m2.append(mean[i] - std[i])

#plt.plot(rho, mean, label='Mean')
#plt.plot(rho, te, label='Initial Te')

plt.plot(rho, te,   'g.', label='Initial Te')
plt.plot(rho, mean, 'r.', label='Mean')
plt.plot(rho, m1, 'b-', alpha=0.25)
plt.plot(rho, m2, 'b-', alpha=0.25)
plt.fill_between(rho, m1, m2, alpha=0.25, label= r'Mean $\pm$ deviation')

plt.title('ETS UQ (PY) with 4 params (using gem_spread_4 data)')
plt.xlabel(r'$\rho_{tor}$ normalized')
plt.ylabel('Temperature')

plt.legend()
plt.grid()
plt.show()
