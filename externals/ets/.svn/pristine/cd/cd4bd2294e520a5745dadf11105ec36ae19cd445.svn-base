#Definition of the class structures in file ual.py
import ual
import numpy
import math
import scipy.integrate

'''
Write a standard starting configuration (should be the same as the fortran version) in 4/1
'''

def integrate(x,y):
    z=numpy.zeros(y.shape)
    z[1:] = scipy.integrate.cumtrapz(y, x=x)
    return z


def write_cpo(shot,run):
    '''Class Itm is the main class for the UAL. It contains a set of field classes, each
    corresponding to a CPO defined in the UAL
    The parameters passed to this creator define the shot and run number. The second pair of
    arguments defines the reference shot and run
    and is used when the a new database is created, as in this example.
    '''

    itm_ev=1.602176487e-19
    itm_pi = 3.141592653589793238462643383280

    cpo = ual.itm(shot,run,shot,0)
    cpo.create()
    nb_cpos=1
    cpo.equilibriumArray.resize(nb_cpos)
    cpo.toroidfieldArray.resize(nb_cpos)

    NRHO        = 100
    NTHETA      = 100

    # Global parameters:
    TIME                     = 1.0  
    R0                       = 6.2                                         # R0      [m]
    B0                       = 5.3                                         # B0      [T]
    RHOB                     = 2.                                          # RHO     [m]
    EL                       = 1.8                                         # elong   [-]
    TR_U                     = 0.4                                         # tr_up   [-]
    TR_L                     = 0.4                                         # tr_low  [-]

    # Boundary values:
    CURR_TOTAL               = 6.E6                                        # total current [A]
    JPAR0                    = 1.E6                                        # Jpar    [A/m^2] 
    JPARB                    = 0.                                          # Jpar    [A/m^2] 
    NE0                      = 1.E20                                       # ne      [m^-3]
    NEB                      = 5.E19                                       # ne      [m^-3]
    TE0                      = 1.E3                                        # Te      [eV]
    TEB                      = 5.E2                                        # Te      [eV]

    # Profiles:
    RHO    = RHOB*numpy.float64(numpy.array(range(NRHO)))/(NRHO-1)
    RHOMOD = (1.  -RHO**2/RHOB**2)
    
    NE     = (NE0-NEB)     * RHOMOD + NEB                   # ne      [m^-3]
    TE     = (TE0-TEB)     * RHOMOD + TEB                   # Te      [eV]
    PR     = NE * TE * itm_ev * 2                           # Pr      [Pa]
    
    while True:
        JPAR   = (JPAR0-JPARB) * RHOMOD + JPARB                 # Jpar    [A/m^2]
        INTJpar = integrate(RHO,JPAR*RHO)
        QSF     = RHO**2*B0/R0/1.25e-6/INTJpar
        FUN     = QSF
        FUN     = 2.*itm_pi*B0/QSF
        QSF[0]  = QSF[1]
        FUN[0]  = FUN[1]
        PSI     = integrate(RHO,FUN*RHO)
        FUN     = JPAR*4.  *itm_pi**2*RHO*R0
        INTJpar = integrate(RHO,FUN)
        CURR    = INTJpar[-1]/(2.  *itm_pi*R0)
        print CURR
        if abs(1.0 - CURR/CURR_TOTAL) < 1.0E-5 : break
        JPAR0             = JPAR0 * CURR_TOTAL / CURR

   

    # EQUILIBRIUM:
    #==============================================================================================
    # +++ Set up equilibrium parameters:

    e0=numpy.zeros([NRHO])

    cpo.equilibriumArray.array[0].time                       = TIME
    cpo.equilibriumArray.array[0].global_param.i_plasma      = CURR
    cpo.equilibriumArray.array[0].global_param.toroid_field.r0 = R0
    cpo.equilibriumArray.array[0].global_param.toroid_field.b0 = B0
    cpo.equilibriumArray.array[0].global_param.mag_axis.position.r = R0
    cpo.equilibriumArray.array[0].global_param.mag_axis.position.z = 0.0
    cpo.equilibriumArray.array[0].global_param.mag_axis.bphi   = B0
    cpo.equilibriumArray.array[0].global_param.mag_axis.q      = QSF[1]
    
    cpo.equilibriumArray.array[0].eqgeometry.elongation        = EL
    cpo.equilibriumArray.array[0].eqgeometry.tria_upper        = TR_U
    cpo.equilibriumArray.array[0].eqgeometry.tria_lower        = TR_L
    
    cpo.equilibriumArray.array[0].profiles_1d.rho_tor          = RHO
    cpo.equilibriumArray.array[0].profiles_1d.q                = QSF
    cpo.equilibriumArray.array[0].profiles_1d.pressure         = PR
    cpo.equilibriumArray.array[0].profiles_1d.jparallel        = JPAR
    
    cpo.equilibriumArray.array[0].profiles_1d.gm1              = 4.  *itm_pi**2*RHO/R0 
    cpo.equilibriumArray.array[0].profiles_1d.gm2              = e0+ 1.  /R0**2
    cpo.equilibriumArray.array[0].profiles_1d.gm3              = e0+ 1.   
    cpo.equilibriumArray.array[0].profiles_1d.gm4              = e0+ 1.  /B0**2
    cpo.equilibriumArray.array[0].profiles_1d.gm5              = e0+ B0**2 
    cpo.equilibriumArray.array[0].profiles_1d.gm6              = 4.  *itm_pi**2*RHO*R0/B0**2 
    cpo.equilibriumArray.array[0].profiles_1d.gm7              = e0+ 1.   
    cpo.equilibriumArray.array[0].profiles_1d.volume           = 2.  *itm_pi**2*RHO**2*R0
    cpo.equilibriumArray.array[0].profiles_1d.vprime           = 4.  *itm_pi**2*RHO*R0
    cpo.equilibriumArray.array[0].profiles_1d.area             = itm_pi*RHO**2
    cpo.equilibriumArray.array[0].profiles_1d.aprime           = e0+ 4.  *itm_pi**2*R0 
    cpo.equilibriumArray.array[0].profiles_1d.F_dia            = e0+ B0*R0
    cpo.equilibriumArray.array[0].profiles_1d.rho_vol          = numpy.sqrt(cpo.equilibriumArray.array[0].profiles_1d.volume/cpo.equilibriumArray.array[0].profiles_1d.volume[-1])
    
    cpo.equilibriumArray.array[0].profiles_1d.elongation       = e0+EL
    cpo.equilibriumArray.array[0].profiles_1d.tria_upper       = e0+TR_U
    cpo.equilibriumArray.array[0].profiles_1d.tria_lower       = e0+TR_L
    cpo.equilibriumArray.array[0].profiles_1d.r_inboard        = R0 - RHO
    cpo.equilibriumArray.array[0].profiles_1d.r_outboard       = R0 + RHO
    
    cpo.equilibriumArray.array[0].profiles_1d.psi              = PSI
    cpo.equilibriumArray.array[0].profiles_1d.phi              = RHO**2 * itm_pi * B0
    
    cpo.equilibriumArray.array[0].global_param.volume          = cpo.equilibriumArray.array[0].profiles_1d.volume[-1]
    cpo.equilibriumArray.array[0].global_param.area            = cpo.equilibriumArray.array[0].profiles_1d.area[-1]
    
    cpo.equilibriumArray.array[0].coord_sys.position.r=numpy.zeros([NRHO,NTHETA+1])
    cpo.equilibriumArray.array[0].coord_sys.position.z=numpy.zeros([NRHO,NTHETA+1])
    for itheta in range(NTHETA+1):
        theta=itheta*2.0*itm_pi/NTHETA
        for irho in range(NRHO):
            cpo.equilibriumArray.array[0].coord_sys.position.r[irho,itheta] = R0 + RHO[irho] * (numpy.cos(theta)-0.5*(TR_U+TR_L)*(numpy.sin(theta))**2)
            cpo.equilibriumArray.array[0].coord_sys.position.z[irho,itheta] = RHO[irho] * EL * numpy.sin(theta)
    print cpo.equilibriumArray.array[0].coord_sys.position.r.shape,cpo.equilibriumArray.array[0].coord_sys.position.z.shape

    cpo.equilibriumArray.array[0].eqgeometry.boundary.r=cpo.equilibriumArray.array[0].coord_sys.position.r[-1,:]
    cpo.equilibriumArray.array[0].eqgeometry.boundary.z=cpo.equilibriumArray.array[0].coord_sys.position.r[-1,:]

    #==============================================================================================


    # TOROIDFIELD:
    #==============================================================================================
    # +++ Set up toroidal field:
    cpo.toroidfieldArray.array[0].time                       = TIME
    cpo.toroidfieldArray.array[0].r0                         = R0
    cpo.toroidfieldArray.array[0].current.value              = CURR
    cpo.toroidfieldArray.array[0].bvac_r.value               = B0*R0
    
    #==============================================================================================

    cpo.equilibriumArray.put()
    cpo.toroidfieldArray.put()

    cpo.close()

write_cpo(4,201)
