SUBROUTINE GEM(eq, coreprof, coretransp, code_parameters)

!...  a simple stand-in for GEM

  USE Turb_Coeff

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq(:)
  TYPE (type_coreprof), pointer :: coreprof(:)
  TYPE (type_coretransp), pointer :: coretransp(:)
  TYPE (type_param) :: code_parameters

  INTEGER(ITM_I4) :: i,ion
  INTEGER(ITM_I4) :: npsi,nrho_prof,nion_prof
  REAL(R8), SAVE :: time = 0.0_R8
  REAL(R8) :: a00,b00,r00,rho_tor_max
  REAL(R8) :: nne,tte,nni,tti,taui,rlne,rlte,rlni,rlti,rhos,cs,zeff,qq
  REAL(R8) :: rnue,rnui,beta,shat,rmue,epss,lperp,ionmass,ioncharge
  REAL(R8) :: chigb,ffe,ffi,gge,ggi
  REAL(R8) :: diffe,diffi,chie,chii,vconve,vconvi,yconve,yconvi

  REAL(R8), DIMENSION(:), ALLOCATABLE, SAVE :: &
       nnex,ttex,nnix,ttix,qqx,zeffx,rlnex,rlnix,rltex,rltix,shatx,chix

  REAL(R8), DIMENSION(:), POINTER :: rho_tor,qq0,jj0,rho0,tt0

!...  XML declarations

  integer(ITM_I4) :: return_status

  character(len = 132), target :: codename(1) = 'GEM0'
  character(len = 132), target :: codeversion(1) = '4.10b'

!...  assign parms

  allocate(coretransp(1))
  allocate(coretransp(1)%codeparam%codename(1))
  allocate(coretransp(1)%codeparam%codeversion(1))
  if (.not. associated(code_parameters%parameters)) then
    write(*,*) 'ERROR: GEM0 parameters not associated!'
    stop
  else
    allocate(coretransp(1)%codeparam%parameters(size( &
     code_parameters%parameters)))
  end if

!.  write(*,*) 'GEM0 Parameters : ', code_parameters%parameters

!-- add to coretransp
  coretransp(1)%codeparam%codename = codename
  coretransp(1)%codeparam%codeversion = codeversion
  coretransp(1)%codeparam%parameters = code_parameters%parameters

!-- assign code parameters to internal variables
  call assign_turb_parameters(code_parameters, return_status)

  if (return_status /= 0) then
    write(*,*) 'ERROR: Could not assign GEM0 parameters.'
    return
  end if

!.  write(*,*) 'done assigning GEM0 parameters'

!...  write out input cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'GEMCPOs' )
  call write_cpo(coreprof(1), 'Coreprof' )
  call write_cpo(eq(1), 'Equil' )
  call close_write_file

  END IF

!...  find grid size for equilibrium and profiles
!...  find number of ion species from coreprof

  npsi=SIZE(eq(1)%profiles_1d%rho_tor)
  nrho_prof=SIZE(coreprof(1)%rho_tor)
  nion_prof=SIZE(coreprof(1)%ni%value, 2)

!...  set grid and allocate coretransp

  IF (nrho_transp == 0) nrho_transp = (nrho_prof-1)/2
  IF (nion == 0) nion = nion_prof

  CALL Turb_Constructor(coretransp(1), 1, nrho_transp, nion)

!...  copy composition over

  call copy_cpo(coreprof(1)%compositions,coretransp(1)%compositions)

!...  set up transport as a function of parameters

  a00=eq(1)%eqgeometry%a_minor
  b00=eq(1)%global_param%toroid_field%b0
  r00=eq(1)%global_param%toroid_field%r0
  rho_tor_max=MAXVAL(coreprof(1)%rho_tor)

!...  set transport grid

  IF (nrho_transp == (nrho_prof-1)/2) THEN
     coretransp(1)%values(1)%rho_tor_norm= &
          coreprof(1)%rho_tor(2:nrho_prof-1:2)/rho_tor_max
  ELSE
     coretransp(1)%values(1)%rho_tor_norm= &
          (/ ((1.0/REAL(2*nrho_transp))*(2*i-1), i=1,nrho_transp) /)**0.7
  END IF

  coretransp(1)%values(1)%rho_tor = rho_tor_max * &
       coretransp(1)%values(1)%rho_tor_norm

!...  allocations with coretransp grid

!  ALLOCATE(rho_tor(nrho_transp))
  ALLOCATE(nnex(nrho_transp))
  ALLOCATE(ttex(nrho_transp))
  ALLOCATE(nnix(nrho_transp))
  ALLOCATE(ttix(nrho_transp))
  ALLOCATE(zeffx(nrho_transp))
  ALLOCATE(qqx(nrho_transp))
  ALLOCATE(rlnex(nrho_transp))
  ALLOCATE(rlnix(nrho_transp))
  ALLOCATE(rltex(nrho_transp))
  ALLOCATE(rltix(nrho_transp))
  ALLOCATE(shatx(nrho_transp))
  ALLOCATE(chix(nrho_transp))

!...  main ion parameters

  ionmass=md
  ioncharge=ee

!...  do the interpolation to get parameters and gradients
!...  species independent stuff first then electrons then ions

  rho_tor => coretransp(1)%values(1)%rho_tor

  CALL L3interp( coreprof(1)%profiles1d%zeff%value, &
       coreprof(1)%rho_tor, nrho_prof, zeffx, rho_tor, nrho_transp)

  SELECT CASE (q_choice)
  CASE ("equilibrium")
     CALL L3interp( eq(1)%profiles_1d%q, &
          eq(1)%profiles_1d%rho_tor, npsi, qqx, rho_tor, nrho_transp)
     CALL L3deriv( eq(1)%profiles_1d%q, &
          eq(1)%profiles_1d%rho_tor, npsi, shatx, rho_tor, nrho_transp)
  CASE ("coreprof")
     CALL L3interp( coreprof(1)%profiles1d%q%value, &
          coreprof(1)%rho_tor, nrho_prof, qqx, rho_tor, nrho_transp)
     CALL L3deriv( coreprof(1)%profiles1d%q%value, &
          coreprof(1)%rho_tor, nrho_prof, shatx, rho_tor, nrho_transp)
  CASE ("jtot")
     IF (.NOT. ASSOCIATED(coreprof(1)%profiles1d%q%value)) &
          ALLOCATE(coreprof(1)%profiles1d%q%value(nrho_prof))
     qq0 => coreprof(1)%profiles1d%q%value
     jj0 => coreprof(1)%profiles1d%jtot%value
     rho0 => coreprof(1)%rho_tor
     qq0(1)=0.
     DO i=2,nrho_prof
        qq0(i)=qq0(i-1)+0.5*(rho0(i)*rho0(i)-rho0(i-1)*rho0(i-1)) &
             * (jj0(i)+jj0(i-1))
     END DO
     qq0=mu_0*qq0*r00/(2.*b00)
     qq0(1)=1.
     qq0=rho0*rho0/qq0
     qq0(1)=2.*qq0(2)-qq0(3)
     CALL L3interp( qq0, rho0, nrho_prof, qqx, rho_tor, nrho_transp)
     CALL L3deriv( qq0, rho0, nrho_prof, shatx, rho_tor, nrho_transp)
  END SELECT

  shatx=shatx*rho_tor/qqx

  CALL L3interp( coreprof(1)%ne%value, coreprof(1)%rho_tor, nrho_prof, &
       nnex, rho_tor, nrho_transp)
  CALL L3interp( coreprof(1)%te%value, coreprof(1)%rho_tor, nrho_prof, &
       ttex, rho_tor, nrho_transp)
  CALL L3deriv( coreprof(1)%ne%value, coreprof(1)%rho_tor, nrho_prof, &
       rlnex, rho_tor, nrho_transp)
  CALL L3deriv( coreprof(1)%te%value, coreprof(1)%rho_tor, nrho_prof, &
       rltex, rho_tor, nrho_transp)

  rlnex=rlnex/nnex
  rltex=rltex/ttex

!...  species loop

  DO ion=1,nion

  CALL L3interp( coreprof(1)%ni%value(:,ion), coreprof(1)%rho_tor, nrho_prof, &
       nnix, rho_tor, nrho_transp)
  CALL L3interp( coreprof(1)%ti%value(:,ion), coreprof(1)%rho_tor, nrho_prof, &
       ttix, rho_tor, nrho_transp)
  CALL L3deriv( coreprof(1)%ni%value(:,ion), coreprof(1)%rho_tor, nrho_prof, &
       rlnix, rho_tor, nrho_transp)
  CALL L3deriv( coreprof(1)%ti%value(:,ion), coreprof(1)%rho_tor, nrho_prof, &
       rltix, rho_tor, nrho_transp)

  rlnix=rlnix/nnix
  rltix=rltix/ttix

!...  on the radial grid...

  DO i=1,nrho_transp

     nne=nnex(i)
     nni=nnix(i)
     tte=ttex(i)
     tti=ttix(i)
     zeff=zeffx(i)

     rlne=rlnex(i)
     rlte=rltex(i)
     rlni=rlnix(i)
     rlti=rltix(i)

     qq=qqx(i)
     shat=shatx(i)

!...  define local parameters

     rhos=SQRT(cc*cc*ionmass*kb*tte/(ee*ee*b00*b00))
     cs=SQRT(kb*tte/ionmass)
     taui=tti/tte

     beta=mu_0*nne*kb*tte/(b00*b00)
     rmue=me/ionmass
     rnue=(lcoul/3.44e11_R8)*zeff*nne/(tte**1.5)
     rnui=(lcoul/2.09e13_R8)*nni/(tti**1.5)

!...  normalised parameters

!     lperp=1./MAX(1./r00, ABS(rlte))
     lperp=1./MAX(1./r00, ABS(rlte), ABS(rlne))
!     lperp=1./MAX( 1./(64.*rhos), ABS(rlte), ABS(rlne))

     epss=qq*r00/lperp
     epss=epss*epss
     rmue=rmue*epss
     beta=beta*epss
     rnue=rnue*lperp/cs

!...  the baseline gyroBohm diffusion coefficient uses R_0
!...  simple downward correction due to usual beta values

     IF (ion == 1) THEN
!        chigb=rhos*rhos*cs/r00
!        chigb=chigb*40.0_R8/SQRT(1.0_R8+(beta_reduction*beta)**2.0_R8)
!        chigb=chigb*MAX(0., (1.0_R8-thresh/ABS((r00*rlti))))

        chigb=rhos*rhos*cs/lperp
        IF (hmode) THEN
!           chigb=0.3*chigb
           chigb=1.0*chigb
        ELSE
!           chigb=chigb*(1.0_R8+beta*SQRT(rmue))
           chigb=chigb*(1.0_R8+rmue*(beta/rmue+SQRT(rnue)))/(1.0_R8+0.1_R8*beta)
!           chigb=chigb*(1.0_R8+(beta/rmue)*(rmue-0.1_R8))
        END IF

        chix(i)=chigb
     ELSE
        chigb=chix(i)
     END IF

!...  the diffusion coefficients in this model
!...  the coefficient is 3/2 due to the Poynting flux cancellation

     diffe=chigb/chi_d
     chie=chigb
     diffi=diffe
     chii=chigb

     IF (ion == 1) THEN
        coretransp(1)%values(1)%ne_transp%diff_eff(i,2) = diffe
        coretransp(1)%values(1)%te_transp%diff_eff(i) = chie
     END IF

     coretransp(1)%values(1)%ni_transp%diff_eff(i,ion,2) = diffi
     coretransp(1)%values(1)%ti_transp%diff_eff(i,ion) = chii

!...  basic pinch dynamics in this model
!...  ions set via ambipolarity

     vconve=diffe*(rlte/etae_pinch)
     vconvi=vconve

     IF (ion == 1) THEN
        coretransp(1)%values(1)%ne_transp%vconv_eff(i,2) = vconve
     END IF

     coretransp(1)%values(1)%ni_transp%vconv_eff(i,ion,2) = vconvi

!...  the effective flux velocities in this model
!...  the coefficient is 3/2 due to the Poynting flux cancellation
!...  for temperatures use the conductive part

     ffe= - diffe*rlne + vconve
     gge= - chie*rlte

     ffi= - diffi*rlni + vconvi
     ggi= - chii*rlti

!...  the fluxes themselves
!...  for a mean field conserved quantity solver heat fluxes are totals
!...  different models can reconstruct theirs using the D's and V's
!...  the coefficient is 3/2 due to the Poynting flux cancellation

     IF (ion == 1) THEN
        coretransp(1)%values(1)%ne_transp%flux(i) = nne*ffe
        coretransp(1)%values(1)%te_transp%flux(i) = nne*kb*tte*gge
     END IF

     coretransp(1)%values(1)%ni_transp%flux(i,ion) = nni*ffi
     coretransp(1)%values(1)%ti_transp%flux(i,ion) = nni*kb*tti*ggi

  END DO

!...  end species loop

  END DO

!...  set other ion coefficients with ratio switches

  coretransp(1)%values(1)%vtor_transp%diff_eff= &
       chiratio_phi*coretransp(1)%values(1)%ti_transp%diff_eff

!...  stamp time

  time = time + 1.0_R8

  coretransp(1)%time=time

!...  write diags

  IF (write_diags) THEN

  open (10, file = 'turbdiags.dat', form = 'formatted',position='append')
  write (10,*) 'eq coreprof coretransp sizes',size(eq),size(coreprof),size(coretransp)
  write (10,*) 'coretransp sizes',nrho_transp,nion
  WRITE (10,*) "  rho           Fe         Qe         Fi         Qi"
  DO i=1,nrho_transp
     WRITE (10,100) coretransp(1)%values(1)%rho_tor_norm(i), &
        coretransp(1)%values(1)%ne_transp%flux(i), &
        coretransp(1)%values(1)%te_transp%flux(i), &
        coretransp(1)%values(1)%ni_transp%flux(i,1), &
        coretransp(1)%values(1)%ti_transp%flux(i,1)
  END DO
  close (10)

100 FORMAT(7g11.3)
110 FORMAT(9f8.3)

  END IF

!...  write out output cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'cout_000' )
  call write_cpo(coretransp(1), 'Coretransp' )
  call close_write_file

  END IF

!...  clean up

!  DEALLOCATE(rho_tor)
  DEALLOCATE(nnex)
  DEALLOCATE(ttex)
  DEALLOCATE(nnix)
  DEALLOCATE(ttix)
  DEALLOCATE(zeffx)
  DEALLOCATE(qqx)
  DEALLOCATE(rlnex)
  DEALLOCATE(rlnix)
  DEALLOCATE(rltex)
  DEALLOCATE(rltix)
  DEALLOCATE(shatx)
  DEALLOCATE(chix)

END SUBROUTINE GEM
