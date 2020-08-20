import numpy as np
import math

from ascii_cpo import read, write, copy_cpo #TODO double check

import turb_coeff #TODO: check what's there

import turb_constructor  #TODO what is that
import l3interp  #TODO from libbsd ???
import l3deriv  #TODO from libbds ???

import assign_turb_parameters
import open_write_file

from libbds import phys_constants

def gem(eq, coreprof, coretransp,code_parameters):

    # XML declarataion
    codename[0] = 'GEM0'
    codeversion[0] = '4.10b'

    coretransp[0].codeparam.codename = codename

    # Assign params
    if code_parameters.parameters == None:
        print('ERROR: GEM0 parameters not associated!')
    else:
        coretransp[0].codeparam.parameters = np.array(np.size(code_parameters.parameters))

    print('GEM0 Parameters : ')
    print(code_parameters.parameters)

     # Add to coretransp
    coretransp[0].codeparam.codename = codename
    coretransp[0].codeparam.codeversion = codeversion
    coretransp[0].codeparam.parameters = code_parameters.parameters

    # Asign code paramteters to interval variabe;s
    return_status = assign_turb_parameters(code_parameters) #TODO check implementation
    if return_status != 0:
        print('ERROR: Could not assign GEM0 parameters!')
    print('Done assigning GEM0 parameters')

    # Write I/O CPOs
    if write_cpos:
        open_write_file('GEMCPOs')  #TODO check where it is
        write(coreprof[0], 'coreprof')  #TODO check if the same as python interface
        write(eq[0], 'equil')

    # Grid size for equilibrium and profiles,
    # Ion species nu

    npsi = np.size(eq[0].profiles_1d.rho_tor)
    nrho_prof = np.size(coreprof[0].rho_tor)
    nion = np.size(coreprof[0].ni.value, 0)

    # Grid and coretransp
    if nrho_transp == 0:
        nrho_transp = (nrho_prof - 1)/2
    if nion == 0:
        nion = nion_prof

    turb_constructor(coretransp[0], 1, nrho_transp, nion) #TODO find implementation!

    # copy composition
    copy_cpo(coreprof[0].compositions, coretransp[0].compostions) #TODO check against python interface

    # Geometry and transport grid
    a00 = eq[0].eqgeometry.a_minor
    b00 = eq[0].global_param.toroid_field.b0
    r00 = eq[0].global_param.toroid_field.r0
    rho_tor_max = max(eq[0].profiles_1d.rho_tor)

    if eq[0].eqgeometry.boundar != None:
        if eq[0].eqgeometry.boundary[0].r != None:
            b00 = b00 * r00
            r_min = min(eq[0].eqgeometry.boundary[0].r)
            r_max = max(eq[0].eqgeometry.boundary[0].r)
            a00 = (r_max - r_min)/2
            r00 = (r_max + r_min)/2
            b00 = b00 / r00

    rho_eq = np.array((npsi))
    rho0 = np.array((nrho_prof))
    rho = np.array((nrho_transp))

    rho_eq = eq[0].profiles.rho_tor / rho_tor_max
    rho0 = np.zeros(nrho_prof)
    rho = np.zeros(nrho_transp)

    if nrho_transp == 1:
        rho = ra0
    elif nrho_transp == (nrho_transp-1)/2 :
            rho = rho0[1:nrho_transp-1:2]
    else:
        rho = [((1.0/(2*nrho_transp))*(2*x-1))**0.7 for x in range(nrho_transp)]

    gm3 = np.zeros((nrho_transp))

    if eq[0].profiles_1d.gm3 != None:
        l3interp(eq[0].profiles_1d.gm3, rho_eq, npsi, gm3, rho, nrho_transp)  #TODO check implementation
    else:
        gm3 = 1.0

    # ???
    nnex = np.array((nrho_transp))
    ttex = np.array((nrho_transp))
    nnix = np.array((nrho_transp))
    ttix = np.array((nrho_transp))
    zeffx = np.array((nrho_transp))
    qqx = np.array((nrho_transp))
    rlnex = np.array((nrho_transp))
    rlnix = np.array((nrho_transp))
    rltex = np.array((nrho_transp))
    rltix = np.array((nrho_transp))
    shatx = np.array((nrho_transp))
    chix = np.array((nrho_transp))

    # Main ion params

    ionmass = md
    ioncharge = ee

    # Interpolate to get params and gradients

    l3interp(coreprof[0].profiles.zeff.value, rho0, nrho_prof, zeffx, rho, nrho_transp)  #TODO check implementation

    # Q-profile

    cases = ["equilibrium", "coreprof", "jtot"]

    if q_choice == "equilibrium":
        l3interp(eq[0].profiles_1d.q, rho_eq, npsi, qqx, rho, nrho_transp)
    if q_choice == "coreprof":
        l3interp(coreprof[0].profiles_1d.q, rho_eq, npsi, shatz, rho, nrho_transp)
    if q_choice == "jtot":
        if coreprof[0].profiles.q.value == None : #TODO check what it deos ALLOCATE(coreprof(1)%profiles1d%q%value(nrho_prof)) ???
            coreprof[0].profiles_1d.q.value[nrho_prof] = np.zeros((1)) #TODO how to pass size to ALLOCATE ?
            qq0 = coreprof[0].profiles_1d.q.value
            jj0 = coreprof[0].profiles_1d.jtot.value
            qq0[0] = 0.0
            for i in range(1,nrho_prof):
                qq0[i] = qq0[i-1] + 0.5*(rho0[i]*rho0[i-1] - rho0[i-1]) * (jj0[i]+jj0[i-1])
            qq0 = mu_0 * qq0 * r00 / (2.0 * b00)
            qq0[0] = 1.0
            qq0 = rho0 * rho0 / qq0
            qq0[0] = 2.0 * qq0[1] - qq0[2]
            l3interp(qq0, rho0, nrho_prof, qqx, rho, nrho_transp)
            l3deriv(qq0, rho0, nrho_prof, shatx, rho, nrho_transp)


    shatx = shatx * rho / qqx

    l3interp(coreprof[0].ne.value, rho0, nrho_prof, nnex, rho, nrho_transp)
    l3interp(coreprof[0].te.value, rho0, nrho_prof, ttex, rho, nrho_transp)
    l3deriv(coreprof[0].ne.value, rho0, nrho_prof, rlnex, rho, nrho_transp)
    l3deriv(coreprof[0].te.value, rho0, nrho_prof, rltex, rho, nrho_transp)

    rlnex = rlnex / nnex
    rltex = rltex / ttex

    # Species loop

    for ion in range(nion):

        l3interp(coreprof[0].ni.value[:, ion], rho0, nrho_prof, nnix, rho, nrho_transp)
        l3interp(coreprof[0].ti.value[:, ion], rho0, nrho_prof, ttix, rho, nrho_transp)
        l3deriv(coreprof[0].ti.value[:, ion], rho0, nrho_prof, rlnix, rho, nrho_transp)
        l3deriv(coreprof[0].ti.value[:, ion], rho0, nrho_prof, rltix, rho, nrho_transp)

        rlnix = rlnix / nnix
        rltix = rltix / ttix

        # Radial grid

        for i in range(nrho_transp):

            nne = nnex[i]
            nni = nnix[i]
            tte = ttex[i]
            tti = ttix[i]
            zeff = zeff[i]

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

            if ion == 0:

                coretransp[0].values[0].ne_transp.diff_eff[i, 1] = diffe
                coretransp[0].values[0].te_transp.diff_eff[i] = chie
                coretransp[0].values[0].ne_transp.vconv_eff[i, 1] = vconve
                coretransp[0].values[0].ne_transp.flux[i] = nne * ffe * gm3[i]
                coretransp[0].values[0].te_transp.flux[i] = nne * kb * tte * gge * gm3[i]

            coretransp[0].values[1].ni_transp.diff_eff[i, ion, 1] = diffi
            coretransp[0].values[0].ti_transp.diff_eff[i, ion] = chii
            coretransp[0].values[0].ni_transp.vconv_eff[i, ion, 1] = vconvi
            coretransp[0].values[0].ni_transp.flux[i, ion] = nni * ffi * gm3[i]
            coretransp[0].values[0].ti_transp.flux[i, ion] = nni * kb * tti * ggi * gm3[i]

    # End species loop

    # Set transp grid in the CPO

    coretransp[0].value[0].rho_tor_norm = rho
    coretransp[0].value[0].rho_tor = rho * rho_tor_max

    # Set other ion ceofficients with ration switches

    coretransp[0].values[0].vtor_transp.diff_eff = chiratio_phi * coretransp[0].values[0].ti_transp.diff_eff

    # timestamp
    time = time + 1.0
    coretransp[0].time = time

    # write diags
    if write_diags:
        # open(10, file='turbdiags.dat', form='formatted', position='append')
        # write(10, *) 'eq coreprof coretransp sizes', size(eq), size(coreprof), size(coretransp)
        # write(10, *) 'coretransp sizes', nrho_transp, nion
        # WRITE(10, *)"  rho           Fe         Qe         Fi         Qi"
        for i in range(nrho_transp):
            # WRITE(10, 100)
            write(
            coretransp[0].values[0].rho_tor_norm(i),
            coretransp[0].values[0].ne_transp.flux(i),
            coretransp[0].values[0].te_transp.flux(i),
            coretransp[0].values[0].ni_transp.flux(i, 1),
            coretransp[0].values[0].ti_transp.flux(i, 1))

    # write out cpos
    if write_cpos:
        # open_write_file(12, 'cout_000') # ???
        write(coretransp[0], 'coretransp') # check were it is the same as python interface


