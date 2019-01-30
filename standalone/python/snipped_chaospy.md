
```python
# Input
coret  = read("data/ets_coretransp_in.cpo", "coretransp")

# Uncertain parameters and distrubutions
diff_eff = coret.values[0].te_transp.diff_eff
c0 = cp.Normal(diff_eff[0], 0.2*diff_eff[0])
c1 = cp.Normal(diff_eff[1], 0.2*diff_eff[1])

# Joint probability distribution
dist = cp.J(c0, c1)

# Generate Hermite polynomials (order k)
k = 4
P = cp.orth_ttr(k, dist)

# Quadrature nodes and weights
nodes, w = cp.generate_quadrature(order=k+1, domain=dist, rule='Gaussian')

# Evaluate the computational model in the sample points (nodes)
samples_te = []
for i in range((k+2)**2):
    tmp_file_in  = update_coret("/ets_coretransp_in.cpo", nodes.T[i])
    tmp_file_out = run_ets(tmp_file)
    corep        = read(tmp_file_out, "coreprof")
    samples_te.append(corep.te.value)

# Create approximate solver
te_hat = cp.fit_quadrature(P, nodes, w, samples_te)

# Statistical infos
mean = cp.E(te_hat, dist)
var  = cp.Var(te_hat, dist)
std  = cp.Std(te_hat, dist)

```
