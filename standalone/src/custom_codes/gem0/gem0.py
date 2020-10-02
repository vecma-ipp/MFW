import numpy as np
import math
import copy

from turb_constructor import turb_constructor
from utils import l3interp, l3deriv  # TODO test more
import assign_turb_parameters
from phys_constants import *


def gem(eq, coreprof, coretransp, code_parameters):

    time = 0.

    codename = [] #TODO check if should be wrritten as list/array
    codeversion = []

    # XML declarataion
    codename.append('GEM0')
    codeversion.append('4.10b')

    coretransp.codeparam.codename = codename

    # Assign params

    #if code_parameters.get_value('parameters') == None:
    #    print('ERROR: GEM0 parameters not associated!')
    #else:
    #    coretransp.codeparam.parameters = np.array(np.size(code_parameters.parameters))

    print('GEM0 Parameters : ')
    print(code_parameters)

    # Add to coretransp

    coretransp.codeparam.codename = codename[0]
    coretransp.codeparam.codeversion = codeversion[0]
    #coretransp.codeparam.parameters = code_parameters.parameters

    # Assign code paramteters to interval variables

    #return_status = assign_turb_parameters(code_parameters) #TODO check if actually something cannot be initialized in runtime
    #if return_status != 0:
    #    print('ERROR: Could not assign GEM0 parameters!')
    
    # WORKAROUND
    write_cpos = code_parameters['flags.write_cpos']
    write_diags = code_parameters['flags.write_diags']
    q_choice = code_parameters['flags.q_choice']
    hmode = code_parameters['flags.hmode']

    nrho_transp = code_parameters['grid.nrho_transp']
    #nion_prof = code_parameters['grid.nion']
    nion = code_parameters['grid.nion']
    nion_prof = code_parameters['grid.nion_prof']
    ra0 = code_parameters['grid.ra0']

    thresh = code_parameters['physical.thresh']
    beta_reduction = code_parameters['physical.beta_reduction'] 
    etae_pinch = code_parameters['physical.etae_pinch']
    chi_d = code_parameters['physical.chi_d'] 
    chiratio_phi = code_parameters['physical.chiratio_phi']


    print('> Done assigning GEM0 parameters')
    
    # Write I/O CPOs
    if write_cpos:
        write(coreprof, 'coreprof')  #TODO check if the same as python interface
        write(eq, 'equil')

    # Grid size for equilibrium and profiles, ion species nu

    npsi = np.size(eq.profiles_1d.rho_tor)
    nrho_prof = np.size(coreprof.rho_tor)

    # Grid and coretransp
    if nrho_transp == 0:
        nrho_transp = (nrho_prof - 1)/2
    if nion == 0:
        nion = nion_prof

    coretransp = turb_constructor(coretransp, 1, nrho_transp, nion)

    # Copy composition
    coretransp.compositions = copy.deepcopy(coreprof.compositions) #TODO check against python interface

    # Geometry and transport grid
    a00 = eq.eqgeometry.a_minor
    b00 = eq.global_param.toroid_field.b0
    r00 = eq.global_param.toroid_field.r0
    rho_tor_max = max(eq.profiles_1d.rho_tor)

    if eq.eqgeometry.boundary != None:
        if not eq.eqgeometry.boundary[0].r is None:
            b00 = b00 * r00
            r_min = min(eq.eqgeometry.boundary[0].r)
            r_max = max(eq.eqgeometry.boundary[0].r)
            a00 = (r_max - r_min)/2
            r00 = (r_max + r_min)/2
            b00 = b00 / r00

    rho_eq = np.empty((npsi))
    rho0 = np.empty((nrho_prof))
    rho = np.empty((nrho_transp))

    rho0 = coreprof.rho_tor
    # use to compare gradient values from code and from cpo file 
    rho0_bound = max(rho0) # correction for gradient calculation (rho_tor_norm/ rho_tor = 1.43840892 for AUG)
    rho0 = rho0/rho0_bound
    rho_eq = eq.profiles_1d.rho_tor / rho_tor_max

    if nrho_transp == 1:
        rho = np.array([ra0])
    elif nrho_transp == (nrho_prof-1)/2 :
            rho = rho0[1:nrho_transp-1:2]
    else:
        rho = np.array([((1.0/(2*nrho_transp))*(2*x+1))**0.7 for x in range(nrho_transp)])

    print('rho: {}'.format(rho))

    gm3 = np.empty(nrho_transp)

    if not eq.profiles_1d.gm3 is None:
        gm3 = l3interp(eq.profiles_1d.gm3, rho_eq, npsi, gm3, rho, nrho_transp)
    else:
        gm3 = np.array([1.0 for i in gm3])

    nnex  = np.empty((nrho_transp))
    ttex  = np.empty((nrho_transp))
    nnix  = np.empty((nrho_transp))
    ttix  = np.empty((nrho_transp))
    zeffx = np.empty((nrho_transp))
    qqx   = np.empty((nrho_transp))
    rlnex = np.empty((nrho_transp))
    rlnix = np.empty((nrho_transp))
    rltex = np.empty((nrho_transp))
    rltix = np.empty((nrho_transp))
    shatx = np.empty((nrho_transp))
    chix  = np.empty((nrho_transp))

    cpo_teddrho = np.empty((nrho_transp))
    cpo_tiddrho = np.empty((nrho_transp))

    # Main ion params

    ionmass = md
    ioncharge = ee

    # Interpolate to get params and gradients

    zeffx = l3interp(coreprof.profiles1d.zeff.value, rho0, nrho_prof, zeffx, rho, nrho_transp)

    # Q-profile

    cases = ["equilibrium", "coreprof", "jtot"]
    print('q_choice: {}'.format(q_choice))

    if q_choice == "equilibrium":
        qqx = l3interp(eq.profiles_1d.q, rho_eq, npsi, qqx, rho, nrho_transp)
        shatx = l3deriv( eq.profiles_1d.q, rho_eq, npsi, shatx, rho, nrho_transp)
    if q_choice == "coreprof":
        qqx = l3interp( coreprof.profiles1d.q.value, rho0, nrho_prof, qqx, rho, nrho_transp)
        shatx = l3deriv( coreprof.profiles1d.q.value, rho0, nrho_prof, shatx, rho, nrho_transp)
    if q_choice == "jtot":
        if coreprof.profiles1d.q.value is None:
            coreprof.profiles1d.q.value[nrho_prof] = np.zeros((1))
            qq0 = coreprof.profiles1d.q.value
            jj0 = coreprof.profiles1d.jtot.value
            qq0[0] = 0.0
            for i in range(0,nrho_prof):
                qq0[i] = qq0[i-1] + 0.5*(rho0[i]*rho0[i-1] - rho0[i-1]) * (jj0[i]+jj0[i-1])
            qq0 = mu_0 * qq0 * r00 / (2.0 * b00)
            qq0[0] = 1.0
            qq0 = rho0 * rho0 / qq0
            qq0[0] = 2.0 * qq0[1] - qq0[2]
            qqx = l3interp(qq0, rho0, nrho_prof, qqx, rho, nrho_transp)
            shatx = l3deriv(qq0, rho0, nrho_prof, shatx, rho, nrho_transp)

    print('qqx: {}'.format(qqx))
    shatx = shatx * rho / qqx

    #print('nnex size is {}'.format(nnex.shape))

    nnex = l3interp(coreprof.ne.value, rho0, nrho_prof, nnex, rho, nrho_transp)
    ttex = l3interp(coreprof.te.value, rho0, nrho_prof, ttex, rho, nrho_transp)
    rlnex = l3deriv(coreprof.ne.value, rho0, nrho_prof, rlnex, rho, nrho_transp)
    rltex = l3deriv(coreprof.te.value, rho0, nrho_prof, rltex, rho, nrho_transp)

    cpo_teddrho = l3interp(coreprof.te.ddrho, rho0, nrho_prof, cpo_teddrho, rho, nrho_transp)

    #print('profile te: {}; interpolated te: {}'.format(coreprof.te.value, ttex))

    #print('nnex: {}'.format(nnex))
    #print('ttex: {}'.format(ttex))
    teddrho = rltex

    rlnex = rlnex / nnex
    rltex = rltex / ttex

    # Species loop
    print('nion:{} nion_prof:{} nrho_prof:{} nrho_transp:{}'.format(nion, nion_prof, nrho_prof, nrho_transp))

    for ion in range(nion):

        #print('ti value at profile: {}, rho0: {}, nrho_prof: {}, rho: {}, nrho_transp: {}'
        #      .format(coreprof.ti.value[:, ion], rho0, nrho_prof, rho, nrho_transp))

        nnix = l3interp(coreprof.ni.value[:, ion], rho0, nrho_prof, nnix, rho, nrho_transp)
        ttix = l3interp(coreprof.ti.value[:, ion], rho0, nrho_prof, ttix, rho, nrho_transp)
        rlnix = l3deriv(coreprof.ni.value[:, ion], rho0, nrho_prof, rlnix, rho, nrho_transp)
        rltix = l3deriv(coreprof.ti.value[:, ion], rho0, nrho_prof, rltix, rho, nrho_transp)

        cpo_tiddrho = l3interp(coreprof.ti.ddrho[:, ion], rho0, nrho_prof, cpo_tiddrho, rho, nrho_transp)

        #one flux tube case: get the intepolated gradinets
        tiddrho = rltix
        print('\n>>> Trying to get effective gradient sfor the code: ')
        print('int teddrho?: {} ; int tiddrho?: {}'.format(teddrho, tiddrho))
        print('cpo teddrho : {} ; cpo tiddrho : {}'.format(cpo_teddrho * rho0_bound, cpo_tiddrho * rho0_bound))
        print('\n')

        rlnix = rlnix / nnix
        rltix = rltix / ttix

        # Radial grid

        for i in range(nrho_transp):

            nne = nnex[i]
            nni = nnix[i]
            tte = ttex[i]
            tti = ttix[i]
            zeff = zeffx[i]

            rlne = rlnex[i] / a00
            rlte = rltex[i] / a00
            rlni = rlnix[i] / a00
            rlti = rltix[i] / a00

            qq = qqx[i]
            shat = shatx[i]

            # Local parameters

            rhos = math.sqrt(cc * cc * ionmass * kb * tte / (ee * ee * b00 * b00))
            cs = math.sqrt(kb * tte / ionmass)
            taui = tti / tte

            #print('some intial params. cc: {}; ionmass: {}; tte: {}; ee:{}; b00: {}; rhos: {}'.format(cc, ionmass, tte, ee, b00, rhos))

            beta = mu_0 * nne * kb * tte / (b00 * b00)
            rmue = me / ionmass
            rnue = (lcoul / 3.44e11) * zeff * nne / (tte ** 1.5)
            rnui = (lcoul / 2.09e13) * nni / (tti ** 1.5)

            # Normalized parameters

            # lperp = 1. / max(1. / r00, abs(rlte))
            lperp = 1.0 / max(1.0 / r00, abs(rlte), abs(rlne))
            # lperp = 1. / max(1. / (64. * rhos), abs(rlte), abs(rlne))

            epss = qq * r00 / lperp
            epss = epss * epss
            rmue = rmue * epss
            beta = beta * epss
            rnue = rnue * lperp / cs

            # GyroBohm diffusion coefficient uses R_0
            # Downward correction

            if ion == 0:
                #chigb = rhos * rhos * cs / r00
                #chigb = chigb * 40.0 / math.sqrt(1.0 + (beta_reduction * beta) ** 2.0)
                #chigb = chigb * max(0.0, (1.0 - thresh / abs((r00 * rlti))))

                chigb = rhos * rhos * cs / lperp

                #print('SOME COEFS: {} {} {} {}'.format(1/lperp, (1/r00), max(0.0, (1.0 - thresh / abs((r00 * rlti)))), 
                #      40.0 / math.sqrt(1.0 + (beta_reduction * beta) ** 2.0)))

                #print('some intial params. nion: {}; nrho_transp: {}; rhos: {}; cs:{}; lperp: {}'.format(nion, nrho_transp, rhos, cs, lperp))

                if(hmode):
                    # chigb = 0.3 * chigb
                    chigb = 1.0 * chigb
                else:
                    # chigb = chigb * (1.0 + beta * math.sqrt(rmue))
                    chigb = chigb * (1.0 + rmue * (beta / rmue + math.sqrt(rnue))) / (1.0 + 0.1 * beta)
                    # chigb = chigb * (1.0 +(beta / rmue) * (rmue - 0.1))

                chix[i] = chigb
            else:
                chigb = chix[i]

            # Diffusion coefficients in this model 
            # Coefficient is 3/2 due to Poynting flux cancelation

            diffe = chigb / chi_d
            chie = chigb
            diffi = diffe
            chii = chigb

            # Basic pinch dynamics
            # Ions set via ambipolarity

            vconve = diffe * (rlte / etae_pinch)
            vconvi = vconve

            # Effective flux velocities in this model
            # Coeficient is 3/2 due to Poynting flux cancelation
            # For temperatures uses conductive part

            rlne = rlnex[i] / rho_tor_max
            rlte = rltex[i] / rho_tor_max
            rlni = rlnix[i] / rho_tor_max
            rlti = rltix[i] / rho_tor_max

            ffe = - diffe * rlne + vconve
            gge = - chie * rlte

            ffi = - diffi * rlni + vconvi
            ggi = - chii * rlti

            # CPO diffusivities, velocities, fluxes
            # For a mean field conserved quantity solver heat fluxes are totals
            # Different models can reconstruct theirs using the D's and V's
            # Coeficient is 3/2 due to Poynting flux cancelation

            if ion == 0:

                #print('ne_transp.diff_eff size: {}; diff_eff : {}'
                #      .format(coretransp.values[0].ne_transp.diff_eff.shape, diffe.shape))

                te_transp_flux = nne * kb * tte * gge * gm3[i] # TODO: check for multiple flu tubes why it is not assigned
                
                coretransp.values[0].ne_transp.diff_eff[i, 1] = diffe
                coretransp.values[0].te_transp.diff_eff[i] = chie
                coretransp.values[0].ne_transp.vconv_eff[i, 1] = vconve
                coretransp.values[0].ne_transp.flux[i] = nne * ffe * gm3[i]
                coretransp.values[0].te_transp.flux[i] = te_transp_flux

            ti_transp_flux = nni * kb * tti * ggi * gm3[i]

            coretransp.values[0].ni_transp.diff_eff[i, ion, 1] = diffi
            coretransp.values[0].ti_transp.diff_eff[i, ion] = chii
            coretransp.values[0].ni_transp.vconv_eff[i, ion, 1] = vconvi
            coretransp.values[0].ni_transp.flux[i, ion] = nni * ffi * gm3[i]
            coretransp.values[0].ti_transp.flux[i, ion] = ti_transp_flux

    # End species loop

    #print('ti flux value is : {}; from nni : {}, kb: {}, tti: {}, ggi: {}, (chii: {}, rlti: {}),  gm3: {}'.format(
    #      coretransp.values[0].ti_transp.flux, nni, kb, tti, ggi, chii, rlti, gm3[0]))

    #print('rho :{}; rho_tor_max: {}'.format(rho, rho_tor_max))

    # Set transp grid in the CPO

    coretransp.values[0].rho_tor_norm = rho
    coretransp.values[0].rho_tor = rho_tor_max * np.array(rho)

    # Set other ion ceofficients with ration switches

    coretransp.values[0].vtor_transp.diff_eff = chiratio_phi * coretransp.values[0].ti_transp.diff_eff

    # Timestamp
    time = time + 1.0
    coretransp.time = time

    # Write diagnostics
    if write_diags:
        # write a data file for turbulence diagnostics

        # open(10, file='turbdiags.dat', form='formatted', position='append')
        # write(10, *) 'eq coreprof coretransp sizes', size(eq), size(coreprof), size(coretransp)
        # write(10, *) 'coretransp sizes', nrho_transp, nion
        # WRITE(10, *)"  rho           Fe         Qe         Fi         Qi"
        for i in range(nrho_transp):
            # WRITE(10, 100)
            write(
            coretransp.values[0].rho_tor_norm[i],
            coretransp.values[0].ne_transp.flux[i],
            coretransp.values[0].te_transp.flux[i],
            coretransp.values[0].ni_transp.flux[i, 0],
            coretransp.values[0].ti_transp.flux[i, 0])

    # Write out cpos
    #if write_cpos:
    #    # open_write_file(12, 'cout_000') # ???
    #    write(coretransp, 'coretransp') # check were it is the same as python interface

    return coretransp, te_transp_flux, ti_transp_flux, teddrho, tiddrho

