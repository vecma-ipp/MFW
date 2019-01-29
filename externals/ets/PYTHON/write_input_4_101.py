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
    cpo.coreprofArray.resize(nb_cpos)
    cpo.equilibriumArray.resize(nb_cpos)
    cpo.coretranspArray.resize(nb_cpos)
    cpo.coresourceArray.resize(nb_cpos)
    cpo.coreimpurArray.resize(nb_cpos)
    cpo.toroidfieldArray.resize(nb_cpos)

    NRHO        = 100
    NTHETA      = 100
    NION        = 1
    NIMP        = 1
    NZIMP       = 1

    # Global parameters:
    TIME                     = 1.0  
    R0                       = 6.2                                         # R0      [m]
    B0                       = 5.3                                         # B0      [T]
    A0                       = 2.                                          # A0      [m]
    RHOB                     = A0                                          # RHO     [m]
    EL                       = 1.                                          # elong   [-]
    TR_U                     = 0.                                          # tr_up   [-]
    TR_L                     = 0.                                          # tr_low  [-]

    # Boundary values:
    CURR_TOTAL               = 6.E6                                        # total current [A]
    JPAR0                    = 1.E6                                        # Jpar    [A/m^2] 
    JPARB                    = 0.                                          # Jpar    [A/m^2] 
    NE0                      = 1.E20                                       # ne      [m^-3]
    NEB                      = 5.E19                                       # ne      [m^-3]
    NI0                      = 1.E20                                       # ni      [m^-3]
    NIB                      = 5.E19                                       # ni      [m^-3]
    TE0                      = 1.E3                                        # Te      [eV]
    TEB                      = 5.E2                                        # Te      [eV]
    TI0                      = 1.E3                                        # Ti      [eV]
    TIB                      = 5.E2                                        # Ti      [eV]
    VTOR0                    = -2.E4                                       # Vtor    [m/s]
    VTORB                    = 0.                                          # Vtor    [m/s]

    # Profiles:
    RHO    = RHOB*numpy.float64(numpy.array(range(NRHO)))/(NRHO-1)
    RHOMOD = (1.  -RHO**2/RHOB**2)
    
    NE     = (NE0-NEB)     * RHOMOD + NEB                   # ne      [m^-3]
    TE     = (TE0-TEB)     * RHOMOD + TEB                   # Te      [eV]
    PR     = NE * TE * itm_ev                               # Pr      [Pa]
    NI     = ((NI0-NIB) * RHOMOD + NIB).reshape(NRHO,1).repeat(NION,axis=1)       # ni      [m^-3]
    TI     = ((TI0-TIB) * RHOMOD + TIB).reshape(NRHO,1).repeat(NION,axis=1)       # Ti      [eV]
    VTOR   = ((VTOR0-VTORB) * RHOMOD + VTORB).reshape(NRHO,1).repeat(NION,axis=1) # Vtor    [m/s]
    PR     = PR + (NI * TI).sum(axis=1) * itm_ev              # Pr      [Pa]
    
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

    # COREPROF:
    #==============================================================================================
    # +++ Set up profiles of plasma parameters:

    cpo.coreprofArray.array[0].time                          = TIME
    cpo.coreprofArray.array[0].toroid_field.r0               = R0
    cpo.coreprofArray.array[0].toroid_field.b0               = B0
    cpo.coreprofArray.array[0].globalparam.current_tot       = CURR
    cpo.coreprofArray.array[0].rho_tor                       = RHO
    cpo.coreprofArray.array[0].profiles1d.jtot.value         = JPAR
    cpo.coreprofArray.array[0].profiles1d.q.value            = QSF
    cpo.coreprofArray.array[0].psi.value                     = PSI
    cpo.coreprofArray.array[0].ne.value                      = NE
    cpo.coreprofArray.array[0].te.value                      = TE
    cpo.coreprofArray.array[0].ni.value                      = NI
    cpo.coreprofArray.array[0].ti.value                      = TI
    cpo.coreprofArray.array[0].vtor.value                    = VTOR
    cpo.coreprofArray.array[0].profiles1d.pr_perp.value      = PR
    cpo.coreprofArray.array[0].profiles1d.zeff.value         = numpy.ones(NRHO)
    cpo.coreprofArray.array[0].psi.sigma_par.value           = numpy.zeros(NRHO)

    # +++ Set up boundary conditions:
    
    vbc=numpy.zeros(3)
    vbci=numpy.zeros([3,NION])
    tbci=numpy.zeros(NION, dtype=numpy.int32)
    spc=numpy.zeros(NION)
    
    cpo.coreprofArray.array[0].psi.boundary.value  = numpy.copy(vbc)
    cpo.coreprofArray.array[0].ne.boundary.value   = numpy.copy(vbc)
    cpo.coreprofArray.array[0].te.boundary.value   = numpy.copy(vbc)
    cpo.coreprofArray.array[0].ni.boundary.type    = numpy.copy(tbci)
    cpo.coreprofArray.array[0].ti.boundary.type    = numpy.copy(tbci)
    cpo.coreprofArray.array[0].vtor.boundary.type  = numpy.copy(tbci)
    cpo.coreprofArray.array[0].ni.boundary.value   = numpy.copy(vbci)
    cpo.coreprofArray.array[0].ti.boundary.value   = numpy.copy(vbci)
    cpo.coreprofArray.array[0].vtor.boundary.value = numpy.copy(vbci)
    
    cpo.coreprofArray.array[0].psi.boundary.type             = 2
    cpo.coreprofArray.array[0].psi.boundary.value[0]         = cpo.coreprofArray.array[0].globalparam.current_tot
    
    cpo.coreprofArray.array[0].ne.boundary.type              = 1
    cpo.coreprofArray.array[0].ne.boundary.value[0]          = cpo.coreprofArray.array[0].ne.value[-1]
    
    cpo.coreprofArray.array[0].te.boundary.type              = 1
    cpo.coreprofArray.array[0].te.boundary.value[0]          = cpo.coreprofArray.array[0].te.value[-1]
    
    cpo.coreprofArray.array[0].ni.boundary.type              = tbci+1
    cpo.coreprofArray.array[0].ni.boundary.value[0,:]        = cpo.coreprofArray.array[0].ni.value[-1,:]
    
    cpo.coreprofArray.array[0].ti.boundary.type              = tbci+1
    cpo.coreprofArray.array[0].ti.boundary.value[0,:]        = cpo.coreprofArray.array[0].ti.value[-1,:]   
    
    cpo.coreprofArray.array[0].vtor.boundary.type            = tbci+1
    cpo.coreprofArray.array[0].vtor.boundary.value[0,:]      = cpo.coreprofArray.array[0].vtor.value[-1,:]

    cpo.coreprofArray.array[0].composition.zion              = spc+1.0
    cpo.coreprofArray.array[0].composition.amn               = spc+2.0

    #==============================================================================================


    # CORETRANSP:
    #==============================================================================================
    # +++ Set up profiles of transport coefficients:

    tdv = numpy.zeros([NRHO])
    tdvi = numpy.zeros([NRHO,NION])
    tdvc = numpy.zeros([NRHO,3])
    tdvic = numpy.zeros([NRHO,NION,3])

    cpo.coretranspArray.array[0].time                             = TIME
    cpo.coretranspArray.array[0].rho_tor                          = RHO                       #rho     [m]
    cpo.coretranspArray.array[0].sigma                            = tdv+2.0E7                 #sigma   [1/(Ohm*m)]
    TINDEX = 1

    cpo.coretranspArray.array[0].ne_transp.diff_eff               = numpy.copy(tdvc)
    cpo.coretranspArray.array[0].ne_transp.vconv_eff              = numpy.copy(tdvc)
    cpo.coretranspArray.array[0].ni_transp.diff_eff               = numpy.copy(tdvic)
    cpo.coretranspArray.array[0].ni_transp.vconv_eff              = numpy.copy(tdvic)

    cpo.coretranspArray.array[0].ne_transp.diff_eff[:,TINDEX]     = 1.0                       #Diff_ne [m^2/s]
    cpo.coretranspArray.array[0].ne_transp.vconv_eff[:,TINDEX]    = 0.0                       #Vcon_ne [m/s]
    cpo.coretranspArray.array[0].ni_transp.diff_eff[:,:,TINDEX]   = 1.0                       #Diff_ne [m^2/s]
    cpo.coretranspArray.array[0].ni_transp.vconv_eff[:,:,TINDEX]  = 0.0                       #Vcon_ne [m/s]

    cpo.coretranspArray.array[0].te_transp.diff_eff               = tdv+1.5                   #Diff_Te [m^2/s]
    cpo.coretranspArray.array[0].te_transp.vconv_eff              = tdv+0.0                   #Vcon_Te [m/s]
    cpo.coretranspArray.array[0].ti_transp.diff_eff               = tdvi+1.5                  #Diff_Ti [m^2/s]
    cpo.coretranspArray.array[0].ti_transp.vconv_eff              = tdvi+0.0                  #Vcon_Ti [m/s]
    cpo.coretranspArray.array[0].vtor_transp.diff_eff             = tdvi+1.0                  #Diff_Vt [m^2/s]
    cpo.coretranspArray.array[0].vtor_transp.vconv_eff            = tdvi+0.0                  #Vcon_Vt [m/s]

    #==============================================================================================


    # CORESOURCE:
    #==============================================================================================
    # +++ Set up profiles of sources:

    s=numpy.zeros([NRHO])
    si=numpy.zeros([NRHO,NION])

    cpo.coresourceArray.array[0].time                        = TIME
    cpo.coresourceArray.array[0].rho_tor                     = RHO                       #rho     [m]
    
    cpo.coresourceArray.array[0].j                           = s+0.0                     #j_ni    [A/m^2]
    cpo.coresourceArray.array[0].sigma                       = s+0.0                     #sigma   [1/(Ohm*m)]
    cpo.coresourceArray.array[0].qe.exp                      = s+5.0E4                   #Qe_exp  [W/m^3]
    cpo.coresourceArray.array[0].qe.imp                      = s+0.0                     #Qe_imp  [1/m^3/s]
    cpo.coresourceArray.array[0].si.exp                      = si+5.0E19                 #Si_exp  [1/m^3/s]
    cpo.coresourceArray.array[0].si.imp                      = si+0.0                    #Si_imp  [1/s]
    cpo.coresourceArray.array[0].qi.exp                      = si+5.0E4                  #Qi_exp  [W/m^3]
    cpo.coresourceArray.array[0].qi.imp                      = si+0.0                    #Qi_imp  [1/m^3/s]
    cpo.coresourceArray.array[0].ui.exp                      = si+0.0                    #Ui_exp  [kg/m/s^2]
    cpo.coresourceArray.array[0].ui.imp                      = si+0.0                    #Ui_imp  [kg/m^2/s]

    #==============================================================================================


    # COREIMPUR:
    #==============================================================================================
    # +++ Set up impurity profiles:

    ci=numpy.zeros([NRHO,NIMP,NZIMP])

    cpo.coreimpurArray.array[0].time                         = TIME
    cpo.coreimpurArray.array[0].rho_tor                      = RHO                       #rho     [m]
    cpo.coreimpurArray.array[0].nz                           = ci+0.0
    cpo.coreimpurArray.array[0].flux.flux_dv                 = ci+0.0
    cpo.coreimpurArray.array[0].z                            = ci+1.0   
    cpo.coreimpurArray.array[0].zsq                          = ci+1.0
    print cpo.coreimpurArray.array[0]

    #==============================================================================================


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
    cpo.equilibriumArray.array[0].eqgeometry.a_minor           = A0

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
    
    cpo.equilibriumArray.array[0].coord_sys.position.r=numpy.zeros([NRHO,NTHETA])
    cpo.equilibriumArray.array[0].coord_sys.position.z=numpy.zeros([NRHO,NTHETA])
    for itheta in range(NTHETA):
        theta=itheta*2.0*itm_pi/NTHETA
        for irho in range(NRHO):
            cpo.equilibriumArray.array[0].coord_sys.position.r[irho,itheta] = R0 + RHO[irho] * (numpy.cos(theta)-0.5*(TR_U+TR_L)*(numpy.sin(theta))**2)
            cpo.equilibriumArray.array[0].coord_sys.position.z[irho,itheta] = RHO[irho] * EL * numpy.sin(theta)
    print cpo.equilibriumArray.array[0].coord_sys.position.r.shape,cpo.equilibriumArray.array[0].coord_sys.position.z.shape
    cpo.equilibriumArray.array[0].eqgeometry.boundary.r = cpo.equilibriumArray.array[0].coord_sys.position.r[-1,:] 
    cpo.equilibriumArray.array[0].eqgeometry.boundary.z = cpo.equilibriumArray.array[0].coord_sys.position.z[-1,:]
    cpo.equilibriumArray.array[0].eqgeometry.boundary.npoints = NTHETA

    #==============================================================================================


    # TOROIDFIELD:
    #==============================================================================================
    # +++ Set up toroidal field:
    cpo.toroidfieldArray.array[0].time                       = TIME
    cpo.toroidfieldArray.array[0].r0                         = R0
    cpo.toroidfieldArray.array[0].current.value              = CURR
    cpo.toroidfieldArray.array[0].bvac_r.value               = B0*R0
    
    #==============================================================================================

    cpo.coreprofArray.put()
    cpo.equilibriumArray.put()
    cpo.coretranspArray.put()
    cpo.coresourceArray.put()
    cpo.coreimpurArray.put()
    cpo.toroidfieldArray.put()

    cpo.close()

write_cpo(4,101)
