
**Parameter description**
```python
# Input
coret  = read("data/ets_coretransp_in.cpo", "coretransp")

# Uncertain parameters (numbers n)
diff_eff = coret.values[0].te_transp.diff_eff

# Probability distribution
dist_param = [cp.Normal(diff_eff[i], 0.2*diff_eff[i]) for i in range(n)]
dist_joint = cp.J(dist_param)

# Hermite polynomials (order k)
P = cp.orth_ttr(k, dist)
```
**Sampler**
```
# Quadrature nodes and weights
nodes, w = cp.generate_quadrature(order=k+1, domain=dist, rule='Gaussian')
```
**Campaign**
```
# Evaluate the computational model in the sample points (nodes)
samples_te = []
for i in range((k+2)**n):
    tmp_file_in  = update_coret("/ets_coretransp_in.cpo", nodes.T[i])
    tmp_file_out = run_ets(tmp_file)
    corep        = read(tmp_file_out, "coreprof")
    samples_te.append(corep.te.value)
```
**Decoder**
```
# Create approximate solver
te_hat = cp.fit_quadrature(P, nodes, w, samples_te)
```
**Analysis**
```
# Statistical infos
mean = cp.E(te_hat, dist)
var  = cp.Var(te_hat, dist)
std  = cp.Std(te_hat, dist)
```
