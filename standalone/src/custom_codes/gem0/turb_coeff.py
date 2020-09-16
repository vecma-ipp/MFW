
nrho_transp = 0
nion = 0

write_diags = True
write_cpos = True
hmode = True

thresh = 6. # turbulence effects threshold; check range: 5.-7. ; NOT USED CURRENTLY
beta_reduction = 10. # reduction for high transport; check range: 8-12 ; NOT USED CURRENTLY
etae_pinch = 3. # for some ratio of gradient the pinch is created; might be only for particles; check range: 2.8-3.2
chi_d = 3. # normalisation or scaling; check range: 2.5
chiratio_phi = 0.7 # rotation transport coefficient; might be not need, or only for ions; check range: 0.5-1.0

ra0 = 0.7 # (desired) flux tube coordinate in case of single flux tube