
```python
        # Input file containing parameters of interest
        coret = read("ets_coretransp_in.cpo", "coretransp")
        
        # Uncertain parameters (numbers n=2) and distributions
        diff_eff = coret.values[0].te_transp.diff_eff
        dist_1 = cp.Normal(diff_eff[0], 0.2*diff_eff[0])
        dist_2 = cp.Normal(diff_eff[1], 0.2*diff_eff[1])
        
        # Joint probability distribution
        dist = cp.J(dist_1, dist_2)
        
        # Hermite polynomials (order k=4)
        P = cp.orth_ttr(k, dist)
        
        # Encoder: preparing the sampling
        nodes, weights = cp.generate_quadrature(order=k+1, domain=dist, rule='Gaussian')
        samples_te = [0]*(k+2)**n

        # Evaluate the computational model in the sample points (nodes)
        for i in range((k+2)**n):
            # Encoder: simulation input
            tmp_file_in  = update_coret("/ets_coretransp_in.cpo", nodes.T[i])
            
            # Run execution: call transport code
            run_ets(tmp_file_in, tmp_file_out)
            
            # Decoder: aggregate the results 
            corep = read(tmp_file_out, "coreprof")
            samples_te[i] = corep.te.value

        # Collate: create approximate solver
        te_hat = cp.fit_quadrature(P, nodes, weights, samples_te)

        # Analysis: compute statistical informations
        mean     = cp.E(te_hat, dist)
        variance = cp.Var(te_hat, dist)
        std      = cp.Std(te_hat, dist)
```
