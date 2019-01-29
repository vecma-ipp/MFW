SUBROUTINE FC2k_IMP4DV(EQUILIBRIUM, COREPROF, CORETRANSP_IN, CORETRANSP_OUT)

  USE EUITM_SCHEMAS
  USE ITM_CONSTANTS
  USE ALLOCATE_DEALLOCATE
  USE ITM_TYPES
  USE COPY_STRUCTURES
  USE  DEALLOCATE_STRUCTURES

  
  IMPLICIT NONE

  TYPE (type_EQUILIBRIUM),  POINTER :: EQUILIBRIUM(:)
  TYPE (type_COREPROF),     POINTER :: COREPROF(:)
  TYPE (type_coretransp),   POINTER :: CORETRANSP_IN(:)
  TYPE (type_coretransp),   POINTER :: CORETRANSP_OUT(:)

  INTEGER                           :: i
  INTEGER                           :: NPSI, NRHO_PROF

  INTEGER,               PARAMETER  :: NSLICE = 1          !number of CPO ocurancies in the work flow
  INTEGER                           :: NRHO                !number of radial points     (input, determined from COREPROF CPO)
  INTEGER                           :: NION, ION           !number of ion species       (input, determined from COREPROF CPO)
  INTEGER                           :: NIMP                !number of impurities                (input)
  INTEGER                           :: NNUCL               !number of nuclei species
  INTEGER,              ALLOCATABLE :: NZIMP(:)            !number of ionization states for each impurity
  INTEGER                           :: NNEUT               !number of neutrals species
  INTEGER,              ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
  INTEGER,              ALLOCATABLE :: NTYPE(:)            !number of types for each neutral
  





  REAL(R8)                         :: a00,b00,r00,rho_tor_max

  REAL(R8)                         :: nni,tti,rlni,rlti,llni,llti
  REAL(R8)                         :: ffi,ggi,diffi,chii,vconvi,yconvi

  REAL(R8)                         :: nne,tte,rlne,rlte,llne,llte
  REAL(R8)                         :: ffe,gge,diffe,chie,vconve,yconve

  REAL(R8),            ALLOCATABLE :: gm3(:),gm7(:)

  REAL(R8),            ALLOCATABLE :: rltix(:,:),ffix(:,:), ggix(:,:)
  REAL(R8),            ALLOCATABLE :: nnix(:,:), ttix(:,:), rlnix(:,:)

  REAL(R8),            ALLOCATABLE :: rltex(:),  ffex(:),   ggex(:)
  REAL(R8),            ALLOCATABLE :: nnex(:),   ttex(:),   rlnex(:)

  REAL(R8),            ALLOCATABLE :: RHO_PROF(:),   RHO_TRANSP(:)




! +++  grid sizes
  NPSI           = SIZE(EQUILIBRIUM(1)%profiles_1d%rho_tor)
  NRHO_PROF      = SIZE(COREPROF(1)%rho_tor)
  NRHO           = SIZE(CORETRANSP_IN(1)%VALUES(1)%rho_tor)


! +++ Allocate output CPO:
  CALL GET_COMP_DIMENSIONS       (CORETRANSP_IN(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
  CALL ALLOCATE_CORETRANSP_CPO   (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  CORETRANSP_OUT)
  call deallocate_cpo(CORETRANSP_OUT(1)%COMPOSITIONS)
  CALL COPY_CPO                  (CORETRANSP_IN(1)%COMPOSITIONS, CORETRANSP_OUT(1)%COMPOSITIONS)



  ALLOCATE        (  gm3(NRHO))
  ALLOCATE        (  gm7(NRHO))

  ALLOCATE        (RHO_PROF(NRHO_PROF))
  ALLOCATE        (RHO_TRANSP(NRHO))

  ALLOCATE        ( nnix(NRHO,NION))
  ALLOCATE        ( ttix(NRHO,NION))
  ALLOCATE        (rlnix(NRHO,NION))
  ALLOCATE        (rltix(NRHO,NION))
  ALLOCATE        ( ffix(NRHO,NION))
  ALLOCATE        ( ggix(NRHO,NION))

  ALLOCATE        ( nnex(NRHO))
  ALLOCATE        ( ttex(NRHO))
  ALLOCATE        (rlnex(NRHO))
  ALLOCATE        (rltex(NRHO))
  ALLOCATE        ( ffex(NRHO))
  ALLOCATE        ( ggex(NRHO))


  RHO_PROF       = COREPROF(1)%rho_tor
  RHO_TRANSP     = CORETRANSP_IN(1)%VALUES(1)%rho_tor


  nnix           = 0.0_R8
  ttix           = 0.0_R8
  rlnix          = 0.0_R8
  rltix          = 0.0_R8
  ffix           = 0.0_R8
  ggix           = 0.0_R8

  nnex           = 0.0_R8
  ttex           = 0.0_R8
  rlnex          = 0.0_R8
  rltex          = 0.0_R8
  ffex           = 0.0_R8
  ggex           = 0.0_R8



! +++  Get geometry:

  r00  =  EQUILIBRIUM(1)%global_param%toroid_field%r0

  IF (ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm3)) THEN
     CALL L3interp( EQUILIBRIUM(1)%profiles_1d%gm3, EQUILIBRIUM(1)%profiles_1d%rho_tor, NPSI, &
                    gm3,                            RHO_TRANSP,           NRHO)
  ELSE
     gm3=1.0
  END IF

  IF (ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm7)) THEN
     CALL L3interp( EQUILIBRIUM(1)%profiles_1d%gm7, EQUILIBRIUM(1)%profiles_1d%rho_tor, NPSI, &
                    gm7,                            RHO_TRANSP,           NRHO)
  ELSE
     gm7=1.0
  END IF


! +++  Get parameters:

  CALL L3interp   ( COREPROF(1)%ne%value,     RHO_PROF,      NRHO_PROF,                       &
                    nnex,                     RHO_TRANSP,    NRHO)
  CALL L3interp   ( COREPROF(1)%te%value,     RHO_PROF,      NRHO_PROF,                       &
                    ttex,                     RHO_TRANSP,    NRHO)
  CALL L3deriv    ( COREPROF(1)%ne%value,     RHO_PROF,      NRHO_PROF,                       &
                    rlnex,                    RHO_TRANSP,    NRHO)
  CALL L3deriv    ( COREPROF(1)%te%value,     RHO_PROF,      NRHO_PROF,                       &
                    rltex,                    RHO_TRANSP,    NRHO)

  ffex           =  CORETRANSP_IN(1)%VALUES(1)%ne_transp%flux
  ggex           =  CORETRANSP_IN(1)%VALUES(1)%te_transp%flux



  DO ion=1,NION
     CALL L3interp( COREPROF(1)%ni%value(:,ion), RHO_PROF,      NRHO_PROF,                    &
                    nnix(:,ion),                 RHO_TRANSP,    NRHO)
     CALL L3interp( COREPROF(1)%ti%value(:,ion), RHO_PROF,      NRHO_PROF,                    &
                    ttix(:,ion),                 RHO_TRANSP,    NRHO)
     CALL L3deriv ( COREPROF(1)%ni%value(:,ion), RHO_PROF,      NRHO_PROF,                    &
                    rlnix(:,ion),                RHO_TRANSP,    NRHO)
     CALL L3deriv ( COREPROF(1)%ti%value(:,ion), RHO_PROF,      NRHO_PROF,                    &
                    rltix(:,ion),                RHO_TRANSP,    NRHO)

     ffix(:,ion) =  CORETRANSP_IN(1)%VALUES(1)%ni_transp%flux(:,ion)
     ggix(:,ion) =  CORETRANSP_IN(1)%VALUES(1)%ti_transp%flux(:,ion)

  END DO



! +++  assume fluxes get Ds and Vs

  DO i=1,NRHO

        tte      = ttex(i)
        nne      = nnex(i)
        rlne     =-rlnex(i)/nne
        rlte     =-rltex(i)/tte
        ffe      = ffex(i)/nne
        gge      = ggex(i)/(nne*itm_ev*tte)

        llne     = 1./MAX(1./r00, ABS(rlne))
        llte     = 1./MAX(1./r00, ABS(rlte))

        diffe    = ABS(ffe)*llne
        chie     = ABS(gge)*llte

        diffe    = MAX(diffe, 0.2_R8*chie)
        chie     = MAX(chie,  0.2_R8*diffe)

        vconve   = ffe - diffe*rlne
        yconve   = gge - chie*rlte

        diffe    = diffe/gm3(i)
        chie     = chie/gm3(i)

        vconve   = vconve/gm7(i)
        yconve   = yconve/gm7(i)

        CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff(i,2)      = diffe
        CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff(i)        = chie
        CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff(i,2)     = vconve
        CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff(i)       = yconve


     DO ion=1,NION

        tti      = ttix(i,ion)
        nni      = nnix(i,ion)
        rlni     =-rlnix(i,ion)/nni
        rlti     =-rltix(i,ion)/tti
        ffi      = ffix(i,ion)/nni
        ggi      = ggix(i,ion)/(nni*itm_ev*tti)

        llni     = 1./MAX(1./r00, ABS(rlni))
        llti     = 1./MAX(1./r00, ABS(rlti))

        diffi    = ABS(ffi)*llni
        chii     = ABS(ggi)*llti

        diffi    = MAX(diffi, 0.2_R8*chii)
        chii     = MAX(chii,  0.2_R8*diffi)

        vconvi   = ffi - diffi*rlni
        yconvi   = ggi - chii*rlti

        diffi    = diffi/gm3(i)
        chii     = chii/gm3(i)

        vconvi   = vconvi/gm7(i)
        yconvi   = yconvi/gm7(i)

        CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff(i,ion,2)  = diffi
        CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff(i,ion)    = chii
        CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff(i,ion,2) = vconvi
        CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff(i,ion)   = yconvi
     END DO

  END DO




! +++  time

  CORETRANSP_OUT(1)%time                      = CORETRANSP_IN(1)%time
  CORETRANSP_OUT(1)%VALUES(1)%rho_tor         = RHO_TRANSP
  CORETRANSP_OUT(1)%VALUES(1)%rho_tor_norm    = RHO_TRANSP/RHO_TRANSP(NRHO)


! +++  clean up

  DEALLOCATE(gm3)
  DEALLOCATE(gm7)

  DEALLOCATE(nnix)
  DEALLOCATE(ttix)
  DEALLOCATE(rlnix)
  DEALLOCATE(rltix)
  DEALLOCATE(ffix)
  DEALLOCATE(ggix)

  DEALLOCATE(nnex)
  DEALLOCATE(ttex)
  DEALLOCATE(rlnex)
  DEALLOCATE(rltex)
  DEALLOCATE(ffex)
  DEALLOCATE(ggex)

  DEALLOCATE(RHO_PROF)
  DEALLOCATE(RHO_TRANSP)

  RETURN

END SUBROUTINE FC2k_IMP4DV
