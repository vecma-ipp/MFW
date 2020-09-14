
#import ITM_Types
#import Euitm_schemas
#import coretransp_types

import numpy as np

def turb_constructor(coretransp, nrho0, nrho, nion):

    # allocate transport structure

     #ALLOCATE(coretransp.values)

     coretransp.values.sigma = np.array((nrho - nrho0))

     # ALLOCATE(coretransp.values.rho_tor(nrho0:nrho))
     # ALLOCATE(coretransp.values.rho_tor_norm(nrho0:nrho))
     # ALLOCATE(coretransp.values.ne_transp.flux(nrho0:nrho))
     # ALLOCATE(coretransp.values.te_transp.flux(nrho0:nrho))
     # ALLOCATE(coretransp.values.ni_transp.flux(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values.ti_transp.flux(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values.vtor_transp.flux(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values.ne_transp.diff_eff(nrho0:nrho,3))
     # ALLOCATE(coretransp.values.te_transp.diff_eff(nrho0:nrho))
     # ALLOCATE(coretransp.values.ni_transp.diff_eff(nrho0:nrho,nion,3))
     # ALLOCATE(coretransp.values.ti_transp.diff_eff(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values.vtor_transp.diff_eff(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values.ne_transp.vconv_eff(nrho0:nrho,3))
     # ALLOCATE(coretransp.values.te_transp.vconv_eff(nrho0:nrho))
     # ALLOCATE(coretransp.values.ni_transp.vconv_eff(nrho0:nrho,nion,3))
     # ALLOCATE(coretransp.values.ti_transp.vconv_eff(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values.vtor_transp.vconv_eff(nrho0:nrho,nion))

     #coretransp.values.ne_transp.flux = 0. 
     #coretransp.values.te_transp.flux = 0. 
     #coretransp.values.ni_transp.flux = 0. 
     #coretransp.values.ti_transp.flux = 0. 
     #coretransp.values.vtor_transp.flux = 0. 
     #coretransp.values.ne_transp.diff_eff = 0. 
     #coretransp.values.te_transp.diff_eff = 0. 
     #coretransp.values.ni_transp.diff_eff = 0. 
     #coretransp.values.ti_transp.diff_eff = 0. 
     #coretransp.values.vtor_transp.diff_eff = 0. 
     #coretransp.values.ne_transp.vconv_eff = 0. 
     #coretransp.values.te_transp.vconv_eff = 0. 
     #coretransp.values.ni_transp.vconv_eff = 0. 
     #coretransp.values.ti_transp.vconv_eff = 0. 
     #coretransp.values.vtor_transp.vconv_eff = 0. 

