SUBROUTINE EtaiGB(eq, coreprof, coretransp, code_parameters)

!...  a simple turbulent transport model using gyroBohm diffusion
!...  includes an ITG threshold model
!...  mainly for testing purposes
!...  simple pinches are assumed to relax density to eta_e = etae_pinch
!...  pure deuterium fully ionised neutral plasma is assumed
!...  fluxes are assumed to be defined _between_ coreprof grid points
!...  edit this if you want many ion species

#ifdef MPI
  USE MPEs
#endif
  USE Phys_constants
  USE Mod_Turb
  USE ETAIGB_Coeff


  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq(:)
  TYPE (type_coreprof), pointer :: coreprof(:)
  TYPE (type_coretransp), pointer :: coretransp(:)
  type (type_param) :: code_parameters

  INTEGER(ITM_I4) :: i,jm,j0,j1,j2
  INTEGER(ITM_I4) :: nrho_prof,nion_prof
  REAL(R8) :: x,aintm,aint0,aint1,aint2,xm,x0,x1,x2
  REAL(R8) :: a00,b00,r00,hra
  REAL(R8) :: nne,tte,nni,tti,taui,rlne,rlte,rlti,rhos,cs,zeff,qq
  REAL(R8) :: rnue,rnui,beta,shat,rmue,epss,lperp
  REAL(R8) :: chigb,ffe,ffi,gge,ggi
  REAL(R8) :: diffe,diffi,chie,chii,vconve,vconvi,yconve,yconvi

!...  XML declarations

  integer(ITM_I4) :: return_status

  character(len = 132), target :: codename(1) = 'ETAIGB'
  character(len = 132), target :: codeversion(1) = '0'

!...  if running MPI you need these

#ifdef MPI
  INTEGER :: mype,npes
  CALL MPI_Comm_size( MPI_COMM_WORLD, npes, ierr )
  CALL MPI_Comm_rank( MPI_COMM_WORLD, mype, ierr )
#endif

!...  find grid size for profiles
!...  default sets grid size to that of profiles
!...  find number of ion species also from coreprof

  nrho_prof=SIZE(coreprof(1)%rho_tor)
  nion_prof=SIZE(coreprof(1)%ni%value)/nrho_prof

!...  allocations

  IF (.NOT. ASSOCIATED(coretransp)) THEN
     ALLOCATE(coretransp(1))
	allocate(coretransp(1)%values(1))

!...  open files and get parms

#ifdef MPI
  DO ipe=0,npes-1
     IF (ipe == mype) THEN
#endif

  if (.not. associated(code_parameters%parameters)) then
    write(6, *) 'ERROR: code parameters not associated!'
    stop
  else
    allocate(coretransp(1)%codeparam%parameters(size(code_parameters%parameters)))
  end if

  allocate(coretransp(1)%codeparam%codename(1))
  allocate(coretransp(1)%codeparam%codeversion(1))

  coretransp(1)%codeparam%codename = codename
  coretransp(1)%codeparam%codeversion = codeversion
  coretransp(1)%codeparam%parameters = code_parameters%parameters

  call assign_etaigb_parameters(code_parameters, return_status)

  if (return_status /= 0) then
    write(*,*) 'ERROR: Could not assign ETAIGB parameters.'
    return
  end if

  write(*,*) 'done assigning ETAIGB parameters'

#ifdef MPI
     END IF
     CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  END DO
#endif

!...  allocations

  IF (nrho_transp == 0) nrho_transp = nrho_prof
  IF (nion == 0) nion = nion_prof

  CALL Turb_Constructor(coretransp(1), 1, nrho_transp, nion)

!...  done initialisation

  END IF

!...  find grid sizes for transp

  nrho_transp=SIZE(coretransp(1)%values(1)%rho_tor)
  nion=SIZE(coretransp(1)%values(1)%ti_transp%flux)/nrho_transp

!...  set up transport as a function of parameters

  a00=eq(1)%eqgeometry%a_minor
  b00=eq(1)%global_param%toroid_field%b0
  r00=eq(1)%global_param%toroid_field%r0

!...  set transport grid

  hra=1.0_R8/nrho_transp
  coretransp(1)%values(1)%rho_tor_norm=(/ (hra*(i-0.5_R8),i=1,nrho_transp) /)
  coretransp(1)%values(1)%rho_tor=coretransp(1)%values(1)%rho_tor_norm*a00

!...  at each location...

  j1=3
  DO i=1,nrho_transp

!...  find the radial point against the coreprof grid

     x=coretransp(1)%values(1)%rho_tor(i)
     DO WHILE (x >= coreprof(1)%rho_tor(j1) .AND. j1 < nrho_prof-1)
        j1=j1+1
     END DO

     j2=j1+1
     j0=j1-1
     jm=j1-2

     x2=coreprof(1)%rho_tor(j2)
     x1=coreprof(1)%rho_tor(j1)
     x0=coreprof(1)%rho_tor(j0)
     xm=coreprof(1)%rho_tor(jm)

     aintm=(x-x0)*(x-x1)*(x-x2)/((xm-x0)*(xm-x1)*(xm-x2))
     aint0=(x-xm)*(x-x1)*(x-x2)/((x0-xm)*(x0-x1)*(x0-x2))
     aint1=(x-xm)*(x-x0)*(x-x2)/((x1-xm)*(x1-x0)*(x1-x2))
     aint2=(x-xm)*(x-x0)*(x-x1)/((x2-xm)*(x2-x0)*(x2-x1))

!...  local parameters

     nne=aintm*coreprof(1)%ne%value(jm)+ &
          aint0*coreprof(1)%ne%value(j0)+ &
          aint1*coreprof(1)%ne%value(j1)+ &
          aint2*coreprof(1)%ne%value(j2)
     tte=aintm*coreprof(1)%te%value(jm)+ &
          aint0*coreprof(1)%te%value(j0)+ &
          aint1*coreprof(1)%te%value(j1)+ &
          aint2*coreprof(1)%te%value(j2)
     if(associated(coreprof(1)%profiles1d%zeff%value)) then
        zeff=aintm*coreprof(1)%profiles1d%zeff%value(jm)+ &
             aint0*coreprof(1)%profiles1d%zeff%value(j0)+ &
             aint1*coreprof(1)%profiles1d%zeff%value(j1)+ &
             aint2*coreprof(1)%profiles1d%zeff%value(j2)
     else
        zeff=1.0_R8
     endif
     nni=aintm*coreprof(1)%ni%value(jm,1)+ &
          aint0*coreprof(1)%ni%value(j0,1)+ &
          aint1*coreprof(1)%ni%value(j1,1)+ &
          aint2*coreprof(1)%ni%value(j2,1)
     tti=aintm*coreprof(1)%ti%value(jm,1)+ &
          aint0*coreprof(1)%ti%value(j0,1)+ &
          aint1*coreprof(1)%ti%value(j1,1)+ &
          aint2*coreprof(1)%ti%value(j2,1)
     qq=aintm*coreprof(1)%profiles1d%q%value(jm)+ &
          aint0*coreprof(1)%profiles1d%q%value(j0)+ &
          aint1*coreprof(1)%profiles1d%q%value(j1)+ &
          aint2*coreprof(1)%profiles1d%q%value(j2)

!...  local gradients

     aintm=((x-x1)*(x-x2)+(x-x0)*(x-x2)+(x-x0)*(x-x1)) &
          /((xm-x0)*(xm-x1)*(xm-x2))
     aint0=((x-x1)*(x-x2)+(x-xm)*(x-x2)+(x-xm)*(x-x1)) &
          /((x0-xm)*(x0-x1)*(x0-x2))
     aint1=((x-x0)*(x-x2)+(x-xm)*(x-x2)+(x-xm)*(x-x0)) &
          /((x1-xm)*(x1-x0)*(x1-x2))
     aint2=((x-x0)*(x-x1)+(x-xm)*(x-x1)+(x-xm)*(x-x0)) &
          /((x2-xm)*(x2-x0)*(x2-x1))

     rlne=aintm*coreprof(1)%ne%value(jm)+ &
          aint0*coreprof(1)%ne%value(j0)+ &
          aint1*coreprof(1)%ne%value(j1)+ &
          aint2*coreprof(1)%ne%value(j2)
     rlte=aintm*coreprof(1)%te%value(jm)+ &
          aint0*coreprof(1)%te%value(j0)+ &
          aint1*coreprof(1)%te%value(j1)+ &
          aint2*coreprof(1)%te%value(j2)
     rlti=aintm*coreprof(1)%ti%value(jm,1)+ &
          aint0*coreprof(1)%ti%value(j0,1)+ &
          aint1*coreprof(1)%ti%value(j1,1)+ &
          aint2*coreprof(1)%ti%value(j2,1)
     shat=aintm*coreprof(1)%profiles1d%q%value(jm)+ &
          aint0*coreprof(1)%profiles1d%q%value(j0)+ &
          aint1*coreprof(1)%profiles1d%q%value(j1)+ &
          aint2*coreprof(1)%profiles1d%q%value(j2)

     rlne=rlne/nne
     rlte=rlte/tte
     rlti=rlti/tti
     shat=shat*x/qq

!...  define local parameters

     rhos=cc*SQRT(md*kb*tte)/(ee*b00)
     cs=SQRT(kb*tte/md)
     taui=tti/tte

     beta=mu_0*nne*kb*tte/(b00*b00)
     rmue=me/md
     rnue=(lcoul/3.44e11_R8)*zeff*nne/(tte**1.5)
     rnui=(lcoul/2.09e13_R8)*nni/(tti**1.5)

!...  normalised parameters

  lperp=1./MAX( 1./(64.*rhos), ABS(rlte), ABS(rlti), ABS(rlne))

  epss=qq*r00/lperp
  epss=epss*epss
  rmue=rmue*epss
  beta=beta*epss
  rnue=rnue*lperp/cs

!...  the baseline gyroBohm diffusion coefficient uses R_0
!...  simple downward correction due to usual beta values

     chigb=rhos*rhos*cs/r00

     chigb=chigb*40.0_R8/SQRT(1.0_R8+(beta_reduction*beta)**2.0_R8)

     chigb=chigb*MAX(tfloor, (1.0_R8-thresh/ABS((r00*rlti))))

!...  the diffusion coefficients in this model
!...  the coefficient is 3/2 due to the Poynting flux cancellation

     diffe=chigb/chi_d
     diffi=diffe
     chie=chigb
     chii=chigb

     coretransp(1)%values(1)%ne_transp%diff_eff(i,2) = diffe
     coretransp(1)%values(1)%te_transp%diff_eff(i) = chie
     coretransp(1)%values(1)%ni_transp%diff_eff(i,1,2) = diffi
     coretransp(1)%values(1)%ti_transp%diff_eff(i,1) = chii

!...  the effective flux velocities in this model
!...  the coefficient is 3/2 due to the Poynting flux cancellation
!...  for temperatures use the conductive part

     ffe= - diffe*(rlne-rlte/etae_pinch)/r00
     gge= - chie*rlte/r00
     ffi=ffe
     ggi= - chii*rlti/r00

!...  basic pinch dynamics in this model

     coretransp(1)%values(1)%ne_transp%vconv_eff(i,2) = diffe*(rlte/etae_pinch)
     coretransp(1)%values(1)%te_transp%vconv_eff(i) = 0._R8
     coretransp(1)%values(1)%ni_transp%vconv_eff(i,1,2) = diffe*(rlte/etae_pinch)
     coretransp(1)%values(1)%ti_transp%vconv_eff(i,1) = 0._R8

!...  the fluxes themselves
!...  for a mean field conserved quantity solver heat fluxes are totals
!...  different models can reconstruct theirs using the D's and V's
!...  the coefficient is 3/2 due to the Poynting flux cancellation

     coretransp(1)%values(1)%ne_transp%flux(i) = nne*ffe
     coretransp(1)%values(1)%te_transp%flux(i) = nne*kb*tte*(1.5_R8*ffe+gge)
     coretransp(1)%values(1)%ni_transp%flux(i,1) = nni*ffe
     coretransp(1)%values(1)%ti_transp%flux(i,1) = nni*kb*tti*(1.5_R8*ffe+ggi)

  END DO

END SUBROUTINE EtaiGB
