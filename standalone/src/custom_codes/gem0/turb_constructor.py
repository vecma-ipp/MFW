
import ITM_Types #TODO
import Euitm_schemas
import coretransp_types

def turb_constructor(coretransp, nrho0, nrho, nion):

    # allocate transport structure

     #ALLOCATE(coretransp.values[0])

     coretransp.values[0].sigma[nrho0:nrho]

     # ALLOCATE(coretransp.values[0].rho_tor(nrho0:nrho))
     # ALLOCATE(coretransp.values[0].rho_tor_norm(nrho0:nrho))
     # ALLOCATE(coretransp.values[0].ne_transp.flux(nrho0:nrho))
     # ALLOCATE(coretransp.values[0].te_transp.flux(nrho0:nrho))
     # ALLOCATE(coretransp.values[0].ni_transp.flux(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values[0].ti_transp.flux(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values[0].vtor_transp.flux(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values[0].ne_transp.diff_eff(nrho0:nrho,3))
     # ALLOCATE(coretransp.values[0].te_transp.diff_eff(nrho0:nrho))
     # ALLOCATE(coretransp.values[0].ni_transp.diff_eff(nrho0:nrho,nion,3))
     # ALLOCATE(coretransp.values[0].ti_transp.diff_eff(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values[0].vtor_transp.diff_eff(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values[0].ne_transp.vconv_eff(nrho0:nrho,3))
     # ALLOCATE(coretransp.values[0].te_transp.vconv_eff(nrho0:nrho))
     # ALLOCATE(coretransp.values[0].ni_transp.vconv_eff(nrho0:nrho,nion,3))
     # ALLOCATE(coretransp.values[0].ti_transp.vconv_eff(nrho0:nrho,nion))
     # ALLOCATE(coretransp.values[0].vtor_transp.vconv_eff(nrho0:nrho,nion))

     coretransp.values[0].ne_transp.flux = 0. 
     coretransp.values[0].te_transp.flux = 0. 
     coretransp.values[0].ni_transp.flux = 0. 
     coretransp.values[0].ti_transp.flux = 0. 
     coretransp.values[0].vtor_transp.flux = 0. 
     coretransp.values[0].ne_transp.diff_eff = 0. 
     coretransp.values[0].te_transp.diff_eff = 0. 
     coretransp.values[0].ni_transp.diff_eff = 0. 
     coretransp.values[0].ti_transp.diff_eff = 0. 
     coretransp.values[0].vtor_transp.diff_eff = 0. 
     coretransp.values[0].ne_transp.vconv_eff = 0. 
     coretransp.values[0].te_transp.vconv_eff = 0. 
     coretransp.values[0].ni_transp.vconv_eff = 0. 
     coretransp.values[0].ti_transp.vconv_eff = 0. 
     coretransp.values[0].vtor_transp.vconv_eff = 0. 

