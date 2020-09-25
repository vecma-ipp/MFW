import numpy as np
import math
import copy

from ascii_cpo import read, write, copy #TODO double check if the same as fortran interface

#from turb_coeff import write_diags, write_cpos, hmode, nrho_transp, nion, thresh, beta_reduction, etae_pinch, chi_d, chiratio_phi, ra0 
from turb_constructor import turb_constructor
from utils import l3interp, l3deriv  # TODO test more
import assign_turb_parameters
from phys_constants import *


def gem(eq, coreprof, coretransp, code_parameters):

    from turb_coeff import write_diags, write_cpos, hmode, nrho_transp, nion, thresh, beta_reduction, etae_pinch, chi_d, chiratio_phi, ra0
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
    nrho_transp = code_parameters['grid.nrho_transp']
    nion_prof = code_parameters['grid.nion']
    q_choice = code_parameters['flags.write_cpos']

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
    rho0 = rho0/max(rho0)
    rho_eq = eq.profiles_1d.rho_tor / rho_tor_max

    if nrho_transp == 1:
        rho = np.array([ra0])
    elif nrho_transp == (nrho_prof-1)/2 :
            rho = rho0[1:nrho_transp-1:2]
    else:
        rho = [((1.0/(2*nrho_transp))*(2*x+1))**0.7 for x in range(nrho_transp)]

    gm3 = np.empty(nrho_transp)

    if not eq.profiles_1d.gm3 is None:
        gm3 = l3interp(eq.profiles_1d.gm3, rho_eq, npsi, gm3, rho, nrho_transp)
    else:
        gm3 = np.array([1.0 for i in gm3])
    
    # nnex  = np.empty(coreprof.ne.value.size)
    # ttex  = np.empty(coreprof.te.value.size)
    # nnix  = np.empty(coreprof.ni.value.size)
    # ttix  = np.empty(coreprof.ti.value.size)
    # zeffx = np.empty(coreprof.profiles1d.zeff.value.size)
    # qqx   = np.empty((nrho_transp))
    # rlnex = np.empty(coreprof.ne.value.size)
    # rlnix = np.empty(coreprof.ni.value.size)
    # rltex = np.empty(coreprof.te.value.size)
    # rltix = np.empty(coreprof.ti.value.size)
    # shatx = np.empty((nrho_transp))
    # chix  = np.empty((nrho_transp))

    nnex  = np.empty(nrho_transp)
    ttex  = np.empty(nrho_transp)
    nnix  = np.empty(nrho_transp)
    ttix  = np.empty(nrho_transp)
    zeffx = np.empty(nrho_transp)
    qqx   = np.empty((nrho_transp))
    rlnex = np.empty(nrho_transp)
    rlnix = np.empty(nrho_transp)
    rltex = np.empty(nrho_transp)
    rltix = np.empty(nrho_transp)
    shatx = np.empty(nrho_transp)
    chix  = np.empty(nrho_transp)

    # Main ion params

    ionmass = md
    ioncharge = ee

    # Interpolate to get params and gradients

    zeffx = l3interp(coreprof.profiles1d.zeff.value, rho0, nrho_prof, zeffx, rho, nrho_transp)

    # Q-profile

    cases = ["equilibrium", "coreprof", "jtot"]

    if q_choice == "equilibrium":
        qqx = l3interp(eq.profiles_1d.q, rho_eq, npsi, qqx, rho, nrho_transp)
    if q_choice == "coreprof":
        shatx = l3interp(coreprof.profiles_1d.q, rho_eq, npsi-1, shatx, rho, nrho_transp-1)
    if q_choice == "jtot":
        if coreprof.profiles.q.value == None:
            coreprof.profiles_1d.q.value[nrho_prof] = np.zeros((1))
            qq0 = coreprof.profiles_1d.q.value
            jj0 = coreprof.profiles_1d.jtot.value
            qq0[0] = 0.0
            for i in range(0,nrho_prof):
                qq0[i] = qq0[i-1] + 0.5*(rho0[i]*rho0[i-1] - rho0[i-1]) * (jj0[i]+jj0[i-1])
            qq0 = mu_0 * qq0 * r00 / (2.0 * b00)
            qq0[0] = 1.0
            qq0 = rho0 * rho0 / qq0
            qq0[0] = 2.0 * qq0[1] - qq0[2]
            qqx = l3interp(qq0, rho0, nrho_prof-1, qqx, rho, nrho_transp)
            shatx = l3deriv(qq0, rho0, nrho_prof-1, shatx, rho, nrho_transp)


    shatx = shatx * rho / qqx

    #print('nnex size is {}'.format(nnex.shape))

    nnex = l3interp(coreprof.ne.value, rho0, nrho_prof, nnex, rho, nrho_transp)
    ttex = l3interp(coreprof.te.value, rho0, nrho_prof, ttex, rho, nrho_transp)
    rlnex = l3deriv(coreprof.ne.value, rho0, nrho_prof, rlnex, rho, nrho_transp)
    rltex = l3deriv(coreprof.te.value, rho0, nrho_prof, rltex, rho, nrho_transp)

    #print('profile te: {}; interpolated te: {}'.format(coreprof.te.value, ttex))

    rlnex = rlnex / nnex
    rltex = rltex / ttex

    # Species loop

    for ion in range(nion):

        #print('ti value at profile: {}, rho0: {}, nrho_prof: {}, rho: {}, nrho_transp: {}'
        #      .format(coreprof.ti.value[:, ion], rho0, nrho_prof, rho, nrho_transp))

        nnix = l3interp(coreprof.ni.value[:, ion], rho0, nrho_prof, nnix, rho, nrho_transp)
        ttix = l3interp(coreprof.ti.value[:, ion], rho0, nrho_prof, ttix, rho, nrho_transp)
        rlnix = l3deriv(coreprof.ni.value[:, ion], rho0, nrho_prof, rlnix, rho, nrho_transp)
        rltix = l3deriv(coreprof.ti.value[:, ion], rho0, nrho_prof, rltix, rho, nrho_transp)

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
                # chigb = rhos * rhos * cs / r00
                # chigb = chigb * 40.0 / math.sqrt(1.0 + (beta_reduction * beta) ** 2.0)
                # chigb = chigb * max(0.0, (1.0 - thresh / abs((r00 * rlti))))

                chigb = rhos * rhos * cs / lperp

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

            # diffusion coefficients in this model
            # coefficient is 3/2 due to Poynting flux cancelation

            diffe = chigb / chi_d
            chie = chigb
            diffi = diffe
            chii = chigb

            # basic pinch dynamics
            # ions set via ambipolarity

            vconve = diffe * (rlte / etae_pinch)
            vconvi = vconve

            # effective flux velocities in this model
            # coeficient is 3/2 due to Poynting flux cancelation
            # for temperatures use conductive part

            rlne = rlnex[i] / rho_tor_max
            rlte = rltex[i] / rho_tor_max
            rlni = rlnix[i] / rho_tor_max
            rlti = rltix[i] / rho_tor_max

            ffe = - diffe * rlne + vconve
            gge = - chie * rlte

            ffi = - diffi * rlni + vconvi
            ggi = - chii * rlti

            # CPO diffusivities, velocities, fluxes
            # for a mean field conserved quantity solver heat fluxes are totals
            # different models can reconstruct theirs using the D's and V's
            # coeficient is 3/2 due to Poynting flux cancelation

            if ion == 0:

                #print('ne_transp.diff_eff size: {}; diff_eff : {}'
                #      .format(coretransp.values[0].ne_transp.diff_eff.shape, diffe.shape))
                
                coretransp.values[0].ne_transp.diff_eff[i, 1] = diffe
                coretransp.values[0].te_transp.diff_eff[i] = chie
                coretransp.values[0].ne_transp.vconv_eff[i, 1] = vconve
                coretransp.values[0].ne_transp.flux[i] = nne * ffe * gm3[i]
                coretransp.values[0].te_transp.flux[i] = nne * kb * tte * gge * gm3[i]

            coretransp.values[0].ni_transp.diff_eff[i, ion, 1] = diffi
            coretransp.values[0].ti_transp.diff_eff[i, ion] = chii
            coretransp.values[0].ni_transp.vconv_eff[i, ion, 1] = vconvi
            coretransp.values[0].ni_transp.flux[i, ion] = nni * ffi * gm3[i]
            coretransp.values[0].ti_transp.flux[i, ion] = nni * kb * tti * ggi * gm3[i]

    # End species loop

    #print('ti flux value is : {}; from nni : {}, kb: {}, tti: {}, ggi: {}, (chii: {}, rlti: {}),  gm3: {}'.format(
    #      coretransp.values[0].ti_transp.flux, nni, kb, tti, ggi, chii, rlti, gm3[0]))

    # Set transp grid in the CPO

    #print('rho :{}; rho_tor_max: {}'.format(rho, rho_tor_max))

    coretransp.values[0].rho_tor_norm = rho
    coretransp.values[0].rho_tor = rho_tor_max * np.array(rho)

    # Set other ion ceofficients with ration switches

    coretransp.values[0].vtor_transp.diff_eff = chiratio_phi * coretransp.values[0].ti_transp.diff_eff

    # timestamp
    time = time + 1.0
    coretransp.time = time

    # write diags
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

    # write out cpos
    if write_cpos:
        # open_write_file(12, 'cout_000') # ???
        write(coretransp, 'coretransp') # check were it is the same as python interface

    return coretransp

