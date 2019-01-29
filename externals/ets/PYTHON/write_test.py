import ual
import numpy

def write_cpo(shot,run):

    cpo = ual.itm(shot,run,shot,0)
    cpo.create()
    nb_cpos=1
    cpo.coreimpurArray.resize(nb_cpos)

    NRHO    = 100  ;    NTHETA  = 100;     NION    = 1;     NIMP    = 1;    NZIMP   = 1
    TIME     = 1.0 ;    RHOB    = 2.
    RHO    = RHOB*numpy.float64(numpy.array(range(NRHO)))/(NRHO-1)

    ci=numpy.zeros([NRHO,NIMP,NZIMP])

    cpo.coreimpurArray.array[0].time              = TIME
#    cpo.coreimpurArray.array[0].rho_tor           = RHO
#    cpo.coreimpurArray.array[0].nz                = ci+0.0
#    cpo.coreimpurArray.array[0].flux.flux_dv      = ci+0.0
#    cpo.coreimpurArray.array[0].z                 = ci+1.0   
#    cpo.coreimpurArray.array[0].zsq               = ci+1.0
    print cpo.coreimpurArray.array[0]

    cpo.coreimpurArray.put()
    cpo.close()

write_cpo(4,6)
