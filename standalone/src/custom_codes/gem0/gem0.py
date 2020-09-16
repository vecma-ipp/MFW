import numpy as np
import math

from ascii_cpo import read, write, copy #TODO double check if the same as fortran interface

from turb_coeff import write_diags, write_cpos, hmode, nrho_transp, nion, thresh, beta_reduction, etae_pinch, chi_d, chiratio_phi, ra0 #TODO: check if overwritten properly

from turb_constructor import turb_constructor  #TODO check if structure is asctually created
from utils import l3interp, l3deriv  # TODO check implementation correctness

import assign_turb_parameters

from phys_constants import *

import copy


def gem(eq, coreprof, coretransp, code_parameters):

    codename = [] #TODO check if should be wrritten as list/array
    codeversion = [] #TODO check

    # XML declarataion
    codename.append('GEM0')
    codeversion.append('4.10b')

    coretransp.codeparam.codename = codename #TODO check if CPOs should be taken as root

    # Assign params
    #if code_parameters.get_value('parameters') == None:
    #    print('ERROR: GEM0 parameters not associated!')
    #else:
    #    coretransp.codeparam.parameters = np.array(np.size(code_parameters.parameters))

    print('GEM0 Parameters : ')
    #print(code_parameters.parameters)

     # Add to coretransp
    #coretransp.codeparam.codename = codename
    #coretransp.codeparam.codeversion = codeversion
    #coretransp.codeparam.parameters = code_parameters.parameters

    # Asign code paramteters to interval variables
    #return_status = assign_turb_parameters(code_parameters) #TODO check if actually something cannot be initialized in runtime
    #if return_status != 0:
    #    print('ERROR: Could not assign GEM0 parameters!')
    
    # VERY BAD WORKAROUND
    write_cpos = True
    write_cpos = code_parameters.get_value('flags.write_cpos')
    write_diags = True
    hmode = True
    nrho_transp = 1 
    nion_prof = 1
    q_choice = "equilibrium"
    
    print('Done assigning GEM0 parameters')
    
    # Write I/O CPOs
    if write_cpos:
        write(coreprof, 'coreprof')  #TODO check if the same as python interface
        write(eq, 'equil')

    # Grid size for equilibrium and profiles,
    # Ion species nu

    npsi = np.size(eq.profiles_1d.rho_tor)
    nrho_prof = np.size(coreprof.rho_tor)
    nion = np.size(coreprof.ni.value, 0)

    # Grid and coretransp
    if nrho_transp == 0:
        nrho_transp = (nrho_prof - 1)/2
    if nion == 0:
        nion = nion_prof

    turb_constructor(coretransp, 1, nrho_transp, nion)

    # copy composition
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

    rho_eq = np.array((npsi))
    rho0 = np.array((nrho_prof))
    rho = np.array((nrho_transp))

    rho_eq = eq.profiles_1d.rho_tor / rho_tor_max
    rho0 = np.zeros(nrho_prof)
    rho = np.zeros(nrho_transp)

    if nrho_transp == 1:
        rho = ra0
    elif nrho_transp == (nrho_transp-1)/2 :
            rho = rho0[1:nrho_transp-1:2]
    else:
        rho = [((1.0/(2*nrho_transp))*(2*x-1))**0.7 for x in range(nrho_transp)]

    gm3 = np.empty(nrho_transp)

    if not eq.profiles_1d.gm3 is None:
        l3interp(eq.profiles_1d.gm3, rho_eq, npsi-1, gm3, rho, nrho_transp-1)  #TODO check implementation
    else:
        gm3 = [1.0 for i in gm3]
    

    # ???
    nnex  = np.empty(coreprof.ne.value.size)
    ttex  = np.empty(coreprof.te.value.size)
    nnix  = np.empty(coreprof.ni.value.size)
    ttix  = np.empty(coreprof.ti.value.size)
    zeffx = np.empty(coreprof.profiles1d.zeff.value.size)
    qqx   = np.empty((nrho_transp))
    rlnex = np.empty(coreprof.ne.value.size)
    rlnix = np.empty(coreprof.ni.value.size)
    rltex = np.empty(coreprof.te.value.size)
    rltix = np.empty(coreprof.ti.value.size)
    shatx = np.empty((nrho_transp))
    chix  = np.empty((nrho_transp))

    # Main ion params

    ionmass = md
    ioncharge = ee

    # Interpolate to get params and gradients

    l3interp(coreprof.profiles1d.zeff.value, rho0, nrho_prof-1, zeffx, rho, nrho_transp-1)  #TODO check implementation

    # Q-profile

    cases = ["equilibrium", "coreprof", "jtot"]

    if q_choice == "equilibrium":
        l3interp(eq.profiles_1d.q, rho_eq, npsi-1, qqx, rho, nrho_transp-1)
    if q_choice == "coreprof":
        l3interp(coreprof.profiles_1d.q, rho_eq, npsi-1, shatx, rho, nrho_transp)
    if q_choice == "jtot":
        if coreprof.profiles.q.value == None : #TODO check what it deos ALLOCATE(coreprof(1)%profiles1d%q%value(nrho_prof)) ???
            coreprof.profiles_1d.q.value[nrho_prof] = np.zeros((1)) #TODO how to pass size to ALLOCATE ?
            qq0 = coreprof.profiles_1d.q.value
            jj0 = coreprof.profiles_1d.jtot.value
            qq0[0] = 0.0
            for i in range(1,nrho_prof):
                qq0[i] = qq0[i-1] + 0.5*(rho0[i]*rho0[i-1] - rho0[i-1]) * (jj0[i]+jj0[i-1])
            qq0 = mu_0 * qq0 * r00 / (2.0 * b00)
            qq0[0] = 1.0
            qq0 = rho0 * rho0 / qq0
            qq0[0] = 2.0 * qq0[1] - qq0[2]
            l3interp(qq0, rho0, nrho_prof-1, qqx, rho, nrho_transp)
            l3deriv(qq0, rho0, nrho_prof-1, shatx, rho, nrho_transp)


    shatx = shatx * rho / qqx

    l3interp(coreprof.ne.value, rho0, nrho_prof-1, nnex, rho, nrho_transp-1)
    l3interp(coreprof.te.value, rho0, nrho_prof-1, ttex, rho, nrho_transp-1)
    l3deriv(coreprof.ne.value, rho0, nrho_prof-1, rlnex, rho, nrho_transp-1)
    l3deriv(coreprof.te.value, rho0, nrho_prof-1, rltex, rho, nrho_transp-1)

    rlnex = rlnex / nnex
    rltex = rltex / ttex

    # Species loop

    for ion in range(nion):

        l3interp(coreprof.ni.value[:, ion], rho0, nrho_prof-1, nnix, rho, nrho_transp-1)
        l3interp(coreprof.ti.value[:, ion], rho0, nrho_prof-1, ttix, rho, nrho_transp-1)
        l3deriv(coreprof.ti.value[:, ion], rho0, nrho_prof-1, rlnix, rho, nrho_transp-1)
        l3deriv(coreprof.ti.value[:, ion], rho0, nrho_prof-1, rltix, rho, nrho_transp-1)

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

            beta = mu_0 * nne * kb * tte / (b00 * b00)
            rmue = me / ionmass
            rnue = (lcoul / 3.44e11) * zeff * nne / (tte ** 1.5)
            rnui = (lcoul / 2.09e13) * nni / (tti ** 1.5)

            # normalized parameters

            # lperp = 1. / max(1. / r00, abs(rlte))
            lperp = 1.0 / max(1.0 / r00, abs(rlte), abs(rlne))
            # lperp = 1. / max(1. / (64. * rhos), abs(rlte), abs(rlne))

            epss = qq * r00 / lperp
            epss = epss * epss
            rmue = rmue * epss
            beta = beta * epss
            rnue = rnue * lperp / cs

            # gyroBohm diffusion coefficient uses R_0
            # downward correction

            if ion == 0:
                # chigb = rhos * rhos * cs / r00
                # chigb = chigb * 40.0 / math.sqrt(1.0 + (beta_reduction * beta) ** 2.0)
                # chigb = chigb * max(0.0, (1.0 - thresh / abs((r00 * rlti))))

                chigb = rhos * rhos * cs / lperp

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

            print('some resulting datastruct (ne transp): '.format(coretransp.values)) # TODO check why cpo*.values is neither list nor anyhting else

            if ion == 0:

                coretransp.values[0].ne_transp.diff_eff[i, 1] = diffe
                coretransp.values[0].te_transp.diff_eff[i] = chie
                coretransp.values[0].ne_transp.vconv_eff[i, 1] = vconve
                coretransp.values[0].ne_transp.flux[i] = nne * ffe * gm3[i]
                coretransp.values[0].te_transp.flux[i] = nne * kb * tte * gge * gm3[i]

            coretransp.values[1].ni_transp.diff_eff[i, ion, 1] = diffi
            coretransp.values[0].ti_transp.diff_eff[i, ion] = chii
            coretransp.values[0].ni_transp.vconv_eff[i, ion, 1] = vconvi
            coretransp.values[0].ni_transp.flux[i, ion] = nni * ffi * gm3[i]
            coretransp.values[0].ti_transp.flux[i, ion] = nni * kb * tti * ggi * gm3[i]

    # End species loop

    # Set transp grid in the CPO

    coretransp.value[0].rho_tor_norm = rho
    coretransp.value[0].rho_tor = rho * rho_tor_max

    # Set other ion ceofficients with ration switches

    coretransp.values[0].vtor_transp.diff_eff = chiratio_phi * coretransp.values[0].ti_transp.diff_eff

    # timestamp
    time = time + 1.0  #TODO check if it is read from CPO
    coretransp.time = time

    # write diags
    if write_diags:
        # open(10, file='turbdiags.dat', form='formatted', position='append')
        # write(10, *) 'eq coreprof coretransp sizes', size(eq), size(coreprof), size(coretransp)
        # write(10, *) 'coretransp sizes', nrho_transp, nion
        # WRITE(10, *)"  rho           Fe         Qe         Fi         Qi"
        for i in range(nrho_transp):
            # WRITE(10, 100)
            write(
            coretransp.values[0].rho_tor_norm(i),
            coretransp.values[0].ne_transp.flux(i),
            coretransp.values[0].te_transp.flux(i),
            coretransp.values[0].ni_transp.flux(i, 1),
            coretransp.values[0].ti_transp.flux(i, 1))

    # write out cpos
    if write_cpos:
        # open_write_file(12, 'cout_000') # ???
        write(coretransp, 'coretransp') # check were it is the same as python interface


