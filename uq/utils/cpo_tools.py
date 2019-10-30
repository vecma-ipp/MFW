import sys
import numpy as np
from ascii_cpo import read


# Get a list of indices in rho_tor_norm (in coreprof) that correspond
# to the closest rho_thor_norm of flux tubes (in coretransp).
#   corprof cpo file in one of gem0 input
#   coretransp cpo file is the gem0 outpout
def get_flux_index(corep_file, coret_file):
    corep = read(corep_file, 'coreprof')
    coret = read(coret_file, 'coretransp')

    # rho_tor_norm_transp_flux
    rt = coret.values[0].rho_tor_norm
    n_flux = len(rt)
    print('rho_cores = ', rt)

    # rho_tor_norm vector in coreprof
    r = corep.rho_tor_norm

    # the closest rho_tor in coreprof
    rp = [r.flat[np.abs(r - rt[i]).argmin()] for i in range(n_flux)]
    print('rho_corep = ', rp)

    # the correponding index
    idx = [list(r).index(rp[i]) for i in range(n_flux)]
    return idx
