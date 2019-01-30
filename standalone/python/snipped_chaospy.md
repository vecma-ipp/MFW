
**`Parameters description`**
```python
# Input data
coret = read("ets_coretransp_in.cpo", "coretransp")
# Uncertain parameters (numbers n=2) and distributions
diff_eff = coret.values[0].te_transp.diff_eff
c0 = cp.Normal(diff_eff[0], 0.2*diff_eff[0])
c1 = cp.Normal(diff_eff[1], 0.2*diff_eff[1])
# Joint probability distribution
dist = cp.J(c0, c1)
# Hermite polynomials (order k=4)
P = cp.orth_ttr(k, dist)
# Quadrature nodes and weights: preparing Sampler
nodes, w = cp.generate_quadrature(order=k+1, domain=dist, rule='Gaussian')
samples_te = []
```
**`Campaign`**
```python
# Evaluate the computational model in the sample points (nodes)
for i in range((k+2)**n):
    # Encoder: simulation input
    tmp_file_in  = update_coret("/ets_coretransp_in.cpo", nodes.T[i])
    # Run execution: call transport code
    run_ets(tmp_file_in, tmp_file_out)
    # Collate
    corep = read(tmp_file_out, "coreprof")
    # Sampler
    samples_te.append(corep.te.value)
```
**`Decoder`**
```python
# Create approximate solver
te_hat = cp.fit_quadrature(P, nodes, w, samples_te)
```
**`Analysis`**
```python
# Statistical infos
mean     = cp.E(te_hat, dist)
variance = cp.Var(te_hat, dist)
std      = cp.Std(te_hat, dist)
```
