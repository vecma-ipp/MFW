# IT IS A PART OF LIBBDS mod_turb

#import ITM_Types
#import Euitm_schemas

from ual.coretransp import coretransp
from ual.coretransp import valuesstruct_arraycoretransp_valuesObj

import numpy as np

def turb_constructor(coretransp, nrho0, nrho, nion):

    # allocate transport structure

    #print('nrho0: {}; nrho: {}; nion: {} '.format(nrho0, nrho, nion))

    coretransp.values.array.append(valuesstruct_arraycoretransp_valuesObj())

    coretransp.values[0].sigma = np.empty((nrho0))

    coretransp.values[0].rho_tor = np.empty((nrho0))
    coretransp.values[0].rho_tor_norm = np.empty((nrho0))

    coretransp.values[0].ne_transp.flux = np.zeros((nrho0))
    coretransp.values[0].te_transp.flux = np.zeros((nrho0))
    coretransp.values[0].ni_transp.flux = np.zeros((nrho0, nion))
    coretransp.values[0].ti_transp.flux = np.zeros((nrho0, nion))
    coretransp.values[0].vtor_transp.flux = np.zeros((nrho0, nion))
    coretransp.values[0].ne_transp.diff_eff = np.zeros((nrho0, 3)) # TODO: check exact sizes in Fortran
    coretransp.values[0].te_transp.diff_eff = np.zeros((nrho0))
    coretransp.values[0].ni_transp.diff_eff = np.zeros((nrho0, nion, 3))
    coretransp.values[0].ti_transp.diff_eff = np.zeros((nrho0, nion))
    coretransp.values[0].vtor_transp.diff_eff = np.zeros((nrho0, nion))
    coretransp.values[0].ne_transp.vconv_eff = np.zeros((nrho0, 3))
    coretransp.values[0].te_transp.vconv_eff = np.zeros((nrho0))
    coretransp.values[0].ni_transp.vconv_eff = np.zeros((nrho0, nion, 3))
    coretransp.values[0].ti_transp.vconv_eff = np.zeros((nrho0, nion))
    coretransp.values[0].vtor_transp.vconv_eff = np.zeros((nrho0, nion))

    return coretransp
