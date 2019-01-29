! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module converts to/from CPOs to ETS types
!>
!> \author D.Kalupin
!>
!> \version "$Id: convert.f90 1671 2015-03-26 14:12:48Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE CONVERT

  IMPLICIT NONE 

CONTAINS



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine converts CPOs into the ETS derived types. 
!>
!> \author D.Kalupin
!>
!> \version "$Id: convert.f90 1671 2015-03-26 14:12:48Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE CONVERT_CPO_TO_ETS_TYPES                                             &
         (EQUILIBRIUM_OLD, EQUILIBRIUM_ITER, COREPROF_OLD, COREPROF_ITER,         &
         CORETRANSP, CORESOURCE, COREIMPUR, CONTROL_INTEGER, CONTROL_DOUBLE,      &
!
         GEOMETRY, PROFILES, TRANSPORT, SOURCES, IMPURITY, EVOLUTION, CONTROL)


!-------------------------------------------------------!
!     This routine converts CPOs into the ETS           !
!     derived types.                                    !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS

    USE ETS_PLASMA
    USE ALLOCATE_DEALLOCATE

    IMPLICIT NONE


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM),       POINTER       :: EQUILIBRIUM_OLD(:)  !input CPO with geometry
    TYPE (TYPE_EQUILIBRIUM),       POINTER       :: EQUILIBRIUM_ITER(:) !input CPO with geometry
    TYPE (TYPE_COREPROF),          POINTER       :: COREPROF_OLD(:)     !input CPO with plasma profiles
    TYPE (TYPE_COREPROF),          POINTER       :: COREPROF_ITER(:)    !input CPO with plasma profiles
    TYPE (TYPE_CORETRANSP),        POINTER       :: CORETRANSP(:)       !input CPO with transport coefficients
    TYPE (TYPE_CORESOURCE),        POINTER       :: CORESOURCE(:)       !input CPO with sources
    TYPE (TYPE_COREIMPUR),         POINTER       :: COREIMPUR(:)        !input CPO with impurity profiles
    INTEGER,                       INTENT(IN)    :: CONTROL_INTEGER(3)  !integer control parameters
    REAL (R8),                     INTENT(IN)    :: CONTROL_DOUBLE(6)   !real control parameters

! +++ ETS derived types:
    TYPE (MAGNETIC_GEOMETRY),      INTENT(INOUT) :: GEOMETRY            !contains all geometry quantities
    TYPE (PLASMA_PROFILES),        INTENT(INOUT) :: PROFILES            !contains profiles of plasma parameters
    TYPE (TRANSPORT_COEFFICIENTS), INTENT(INOUT) :: TRANSPORT           !contains profiles of trasport coefficients
    TYPE (SOURCES_AND_SINKS),      INTENT(INOUT) :: SOURCES             !contains profiles of sources
    TYPE (IMPURITY_PROFILES),      INTENT(INOUT) :: IMPURITY            !contains profiles of impurities 
    TYPE (TIME_EVOLUTION),         INTENT(INOUT) :: EVOLUTION           !contains all parameters required by time evolution
    TYPE (RUN_CONTROL),            INTENT(INOUT) :: CONTROL             !contains all parameters required by run


! +++ Dimensions & indexes:
    INTEGER                                      :: NEQ,   NEQ_OLD
    INTEGER                                      :: NRHO,  NRHO_OLD
    INTEGER                                      :: NRHO_TR
    INTEGER                                      :: NRHO_SRC
    INTEGER                                      :: NRHO_IMP

    INTEGER                                      :: NNUCL
    INTEGER                                      :: NION, IION, IFL
    INTEGER                                      :: NIMP, IIMP       
    INTEGER,                         ALLOCATABLE :: NZIMP(:)
    INTEGER                                      :: IZIMP       
    INTEGER                                      :: NNEUT
    INTEGER,                         ALLOCATABLE :: NCOMP(:)
    INTEGER,                         ALLOCATABLE :: NTYPE(:)


! +++ Local variables:
    REAL (R8)                                    :: VOL_EQ, ERR_EQ, VOL_C, ERR_C, VOL2_EQ, VOL2_C

    REAL (R8),                       ALLOCATABLE :: FUN(:)



! +++ Dimensions:
    NEQ                      = SIZE(EQUILIBRIUM_ITER(1)%profiles_1d%psi)
    NEQ_OLD                  = SIZE(EQUILIBRIUM_OLD(1)%profiles_1d%psi)
    NRHO                     = SIZE(COREPROF_ITER(1)%rho_tor)
    NRHO_OLD                 = SIZE(COREPROF_OLD(1)%rho_tor)
    CALL GET_COMP_DIMENSIONS       (COREPROF_ITER(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)





! +++ Convert geometry:
    GEOMETRY%RHO             = COREPROF_ITER(1)%rho_tor
    GEOMETRY%R0              = EQUILIBRIUM_ITER(1)%global_param%toroid_field%r0
    GEOMETRY%B0              = EQUILIBRIUM_ITER(1)%global_param%toroid_field%b0
    GEOMETRY%RGEO            = EQUILIBRIUM_ITER(1)%eqgeometry%geom_axis%r
    GEOMETRY%BGEO            = GEOMETRY%B0*GEOMETRY%R0/GEOMETRY%RGEO

    EVOLUTION%BTM            = EQUILIBRIUM_OLD(1)%global_param%toroid_field%b0 * &
                               EQUILIBRIUM_OLD(1)%global_param%toroid_field%r0/EQUILIBRIUM_OLD(1)%eqgeometry%geom_axis%r


    CALL L3deriv (EQUILIBRIUM_ITER(1)%profiles_1d%volume,  EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor,  NEQ,  &
                  GEOMETRY%VPR,                            GEOMETRY%RHO,                             NRHO)
    CALL L3interp(EQUILIBRIUM_ITER(1)%profiles_1d%gm3,     EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor,  NEQ,  &
                  GEOMETRY%G1,                             GEOMETRY%RHO,                             NRHO)
    
    CALL L3interp(EQUILIBRIUM_ITER(1)%profiles_1d%gm8,     EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor,  NEQ,  &
                  GEOMETRY%G2,                             GEOMETRY%RHO,                             NRHO)
    CALL L3interp(EQUILIBRIUM_ITER(1)%profiles_1d%gm2,     EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor,  NEQ,  &
                  GEOMETRY%G3,                             GEOMETRY%RHO,                             NRHO)
    CALL L3interp(EQUILIBRIUM_ITER(1)%profiles_1d%F_dia,   EQUILIBRIUM_ITER(1)%profiles_1d%rho_tor,  NEQ,  &
                  GEOMETRY%FDIA,                           GEOMETRY%RHO,                             NRHO)

    IF (GEOMETRY%VPR(1).LE.0._R8) GEOMETRY%VPR(1) = 0._R8






! +++ Convert profiles:
    DO IION =1, NION
       PROFILES%ZION(IION)   = COREPROF_ITER(1)%compositions%ions(IION)%zion 
       PROFILES%ZION2(IION)  = COREPROF_ITER(1)%compositions%ions(IION)%zion**2 
       PROFILES%MION(IION)   = COREPROF_ITER(1)%compositions%nuclei(COREPROF_ITER(1)%compositions%ions(IION)%nucindex)%amn
    END DO

    PROFILES%PSI             = COREPROF_ITER(1)%psi%value
    PROFILES%DPSI            = COREPROF_ITER(1)%psi%ddrho
    PROFILES%QSF             = COREPROF_ITER(1)%profiles1d%q%value
    PROFILES%PSI_BND         = COREPROF_ITER(1)%psi%boundary%value
    PROFILES%PSI_BND_TYPE    = COREPROF_ITER(1)%psi%boundary%type
    PROFILES%CURR_PAR        = COREPROF_ITER(1)%profiles1d%jtot%value
    PROFILES%CURR_TOR        = COREPROF_ITER(1)%profiles1d%jphi%value

    PROFILES%NI              = COREPROF_ITER(1)%ni%value
    PROFILES%DNI             = COREPROF_ITER(1)%ni%ddrho
    PROFILES%NI_BND          = COREPROF_ITER(1)%ni%boundary%value
    PROFILES%NI_BND_TYPE     = COREPROF_ITER(1)%ni%boundary%type
    PROFILES%NI_BND_RHO      = COREPROF_ITER(1)%ni%boundary%rho_tor

    PROFILES%NE              = COREPROF_ITER(1)%ne%value
    PROFILES%DNE             = COREPROF_ITER(1)%ne%ddrho
    PROFILES%NE_BND          = COREPROF_ITER(1)%ne%boundary%value
    PROFILES%NE_BND_TYPE     = COREPROF_ITER(1)%ne%boundary%type
    PROFILES%NE_BND_RHO      = COREPROF_ITER(1)%ne%boundary%rho_tor

    PROFILES%ZEFF            = COREPROF_ITER(1)%profiles1d%zeff%value

    PROFILES%TI              = COREPROF_ITER(1)%ti%value
    PROFILES%DTI             = COREPROF_ITER(1)%ti%ddrho
    PROFILES%TI_BND          = COREPROF_ITER(1)%ti%boundary%value
    PROFILES%TI_BND_TYPE     = COREPROF_ITER(1)%ti%boundary%type
    PROFILES%TI_BND_RHO      = COREPROF_ITER(1)%ti%boundary%rho_tor

    PROFILES%TE              = COREPROF_ITER(1)%te%value
    PROFILES%DTE             = COREPROF_ITER(1)%te%ddrho
    PROFILES%TE_BND          = COREPROF_ITER(1)%te%boundary%value
    PROFILES%TE_BND_TYPE     = COREPROF_ITER(1)%te%boundary%type
    PROFILES%TE_BND_RHO      = COREPROF_ITER(1)%te%boundary%rho_tor

    PROFILES%VTOR            = COREPROF_ITER(1)%vtor%value
    PROFILES%DVTOR           = COREPROF_ITER(1)%vtor%ddrho
    PROFILES%VTOR_BND        = COREPROF_ITER(1)%vtor%boundary%value
    PROFILES%VTOR_BND_TYPE   = COREPROF_ITER(1)%vtor%boundary%type
    PROFILES%VTOR_BND_RHO    = COREPROF_ITER(1)%vtor%boundary%rho_tor

    PROFILES%ZEFF            = COREPROF_ITER(1)%profiles1d%zeff%value



! +++ Profiles at previous time step:
    CALL L3deriv (EQUILIBRIUM_OLD(1)%profiles_1d%volume,   EQUILIBRIUM_OLD(1)%profiles_1d%rho_tor,   NEQ_OLD,  &
                  EVOLUTION%VPRM,                          GEOMETRY%RHO,                             NRHO)
    CALL L3interp(EQUILIBRIUM_OLD(1)%profiles_1d%gm8,      EQUILIBRIUM_OLD(1)%profiles_1d%rho_tor,   NEQ_OLD,  &
                  EVOLUTION%G2M,                           GEOMETRY%RHO,                             NRHO)
    CALL L3interp(COREPROF_OLD(1)%psi%value,               COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                  EVOLUTION%PSIM,                          GEOMETRY%RHO,                             NRHO)
    CALL L3interp(COREPROF_OLD(1)%psi%ddrho,               COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                  EVOLUTION%DPSIM,                         GEOMETRY%RHO,                             NRHO) 
    CALL L3interp(COREPROF_OLD(1)%ne%value,                COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                  EVOLUTION%NEM,                           GEOMETRY%RHO,                             NRHO)
    CALL L3interp(COREPROF_OLD(1)%ne%ddrho,                COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                  EVOLUTION%DNEM,                          GEOMETRY%RHO,                             NRHO) 
    CALL L3interp(COREPROF_OLD(1)%te%value,                COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                  EVOLUTION%TEM,                           GEOMETRY%RHO,                             NRHO)
    CALL L3interp(COREPROF_OLD(1)%te%ddrho,                COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                  EVOLUTION%DTEM,                          GEOMETRY%RHO,                             NRHO) 
    DO IION = 1, NION
       IF(IION.LE.SIZE(COREPROF_OLD(1)%ni%value, DIM=2))                                                       &
       CALL L3interp(COREPROF_OLD(1)%ni%value(:,iion),     COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                     EVOLUTION%NIM(:,iion),                GEOMETRY%RHO,                             NRHO)
       CALL L3interp(COREPROF_OLD(1)%ni%ddrho(:,iion),     COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                     EVOLUTION%DNIM(:,iion),               GEOMETRY%RHO,                             NRHO) 
       IF(IION.LE.SIZE(COREPROF_OLD(1)%ti%value, DIM=2))                                                       &
       CALL L3interp(COREPROF_OLD(1)%ti%value(:,iion),     COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                     EVOLUTION%TIM(:,iion),                GEOMETRY%RHO,                             NRHO)
       CALL L3interp(COREPROF_OLD(1)%ti%ddrho(:,iion),     COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                     EVOLUTION%DTIM(:,iion),               GEOMETRY%RHO,                             NRHO) 
       IF(IION.LE.SIZE(COREPROF_OLD(1)%vtor%value, DIM=2))                                                     &
       CALL L3interp(COREPROF_OLD(1)%vtor%value(:,iion),   COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                     EVOLUTION%VTORM(:,iion),              GEOMETRY%RHO,                             NRHO)
       CALL L3interp(COREPROF_OLD(1)%vtor%ddrho(:,iion),   COREPROF_OLD(1)%rho_tor,                  NRHO_OLD, &
                     EVOLUTION%DVTORM(:,iion),             GEOMETRY%RHO,                             NRHO) 
    END DO



! +++ Convert transport:
    NRHO_TR                  = SIZE(CORETRANSP(1)%VALUES(1)%rho_tor)
    TRANSPORT%C1(1)          = 0.0E0_R8                           
    TRANSPORT%C1(2)          = 1.5E0_R8                               
    TRANSPORT%C1(3)          = 2.5E0_R8                               


    CALL L3interp   (CORETRANSP(1)%VALUES(1)%sigma,                        CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%SIGMA,                                      GEOMETRY%RHO,                    NRHO) 
    DO IFL=1,3
       CALL L3interp(CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,IFL),    CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%DIFF_NE(:,IFL),                             GEOMETRY%RHO,                    NRHO) 
       CALL L3interp(CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff(:,IFL),   CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%VCONV_NE(:,IFL),                            GEOMETRY%RHO,                    NRHO) 
    END DO
    CALL L3interp   (CORETRANSP(1)%VALUES(1)%te_transp%diff_eff,           CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%DIFF_TE,                                    GEOMETRY%RHO,                    NRHO) 
    CALL L3interp   (CORETRANSP(1)%VALUES(1)%te_transp%vconv_eff,          CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%VCONV_TE,                                   GEOMETRY%RHO,                    NRHO) 

    DO IION = 1, NION
       DO IFL=1,3
          CALL L3interp(CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(:,IION,IFL),  CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                        TRANSPORT%DIFF_NI(:,IION,IFL),                           GEOMETRY%RHO,                    NRHO) 
          CALL L3interp(CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff(:,IION,IFL), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                        TRANSPORT%VCONV_NI(:,IION,IFL),                          GEOMETRY%RHO,                    NRHO) 
       ENDDO
       CALL L3interp(CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(:,IION),   CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%DIFF_TI(:,IION),                            GEOMETRY%RHO,                    NRHO) 
       CALL L3interp(CORETRANSP(1)%VALUES(1)%ti_transp%vconv_eff(:,IION),  CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%VCONV_TI(:,IION),                           GEOMETRY%RHO,                    NRHO) 
       CALL L3interp(CORETRANSP(1)%VALUES(1)%vtor_transp%diff_eff(:,IION), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%DIFF_VTOR(:,IION),                          GEOMETRY%RHO,                    NRHO) 
       CALL L3interp(CORETRANSP(1)%VALUES(1)%vtor_transp%vconv_eff(:,IION),CORETRANSP(1)%VALUES(1)%rho_tor, NRHO_TR,  &
                     TRANSPORT%VCONV_VTOR(:,IION),                         GEOMETRY%RHO,                    NRHO) 
    ENDDO
    TRANSPORT%QGI            = 0.e0_R8                            !not defined yet in the CORETRANSP




! +++ Convert sources:
    NRHO_SRC                 = SIZE(CORESOURCE(1)%VALUES(1)%rho_tor)

    CALL L3interp   (CORESOURCE(1)%VALUES(1)%sigma,                       CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%SIGMA,                                       GEOMETRY%RHO,                     NRHO) 
    CALL L3interp   (CORESOURCE(1)%VALUES(1)%j,                           CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%CURR_EXP,                                    GEOMETRY%RHO,                     NRHO) 
    CALL L3interp   (CORESOURCE(1)%VALUES(1)%qe%exp/itm_ev,               CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%QE_EXP,                                      GEOMETRY%RHO,                     NRHO) 
    CALL L3interp   (CORESOURCE(1)%VALUES(1)%qe%imp,                      CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%QE_IMP,                                      GEOMETRY%RHO,                     NRHO) 
    CALL L3interp   (CORESOURCE(1)%VALUES(1)%se%exp,                      CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%SE_EXP,                                      GEOMETRY%RHO,                     NRHO) 
    CALL L3interp   (CORESOURCE(1)%VALUES(1)%se%imp,                      CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%SE_IMP,                                      GEOMETRY%RHO,                     NRHO) 

    DO IION = 1, NION
       CALL L3interp(CORESOURCE(1)%VALUES(1)%si%exp(:,IION),              CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%SI_EXP(:,IION),                              GEOMETRY%RHO,                     NRHO) 
       CALL L3interp(CORESOURCE(1)%VALUES(1)%si%imp(:,IION),              CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%SI_IMP(:,IION),                              GEOMETRY%RHO,                     NRHO) 
       CALL L3interp(CORESOURCE(1)%VALUES(1)%qi%exp(:,IION)/itm_ev,       CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%QI_EXP(:,IION),                              GEOMETRY%RHO,                     NRHO) 
       CALL L3interp(CORESOURCE(1)%VALUES(1)%qi%imp(:,IION),              CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%QI_IMP(:,IION),                              GEOMETRY%RHO,                     NRHO) 
       CALL L3interp(CORESOURCE(1)%VALUES(1)%ui%exp(:,IION),              CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%UI_EXP(:,IION),                              GEOMETRY%RHO,                     NRHO) 
       CALL L3interp(CORESOURCE(1)%VALUES(1)%ui%imp(:,IION),              CORESOURCE(1)%VALUES(1)%rho_tor,  NRHO_SRC,  &
                     SOURCES%UI_IMP(:,IION),                              GEOMETRY%RHO,                     NRHO) 
    ENDDO
    SOURCES%CURR_IMP         = 0.e0_R8                            !not defined by the CORESOURCE CPO




! +++ Convert impurities:
    IF(ASSOCIATED(coreimpur)) THEN
       nimp     = SIZE(COREIMPUR(1)%impurity)
       NRHO_IMP = SIZE(COREIMPUR(1)%rho_tor)
    ELSE
       nimp=0
    ENDIF
    DO iimp = 1, nimp
       DO izimp = 1, nzimp(iimp)
          CALL L3interp(COREIMPUR(1)%impurity(iimp)%nz(:,izimp),            COREIMPUR(1)%rho_tor,   NRHO_IMP,   &
                        IMPURITY%NZ(:,iimp,izimp),                          GEOMETRY%RHO,           NRHO)
          CALL L3interp(COREIMPUR(1)%impurity(iimp)%flux%flux_dv(:,izimp),  COREIMPUR(1)%rho_tor,   NRHO_IMP,   &
                        IMPURITY%FLUX_NZ(:,iimp,izimp),                     GEOMETRY%RHO,           NRHO)
          CALL L3interp(COREIMPUR(1)%impurity(iimp)%z(:,izimp),             COREIMPUR(1)%rho_tor,   NRHO_IMP,   &
                        IMPURITY%ZIMP(:,iimp,izimp),                        GEOMETRY%RHO,           NRHO)
          CALL L3interp(COREIMPUR(1)%impurity(iimp)%zsq(:,izimp),           COREIMPUR(1)%rho_tor,   NRHO_IMP,   &
                        IMPURITY%ZIMP2(:,iimp,izimp),                       GEOMETRY%RHO,           NRHO)
       END DO
    ENDDO



! +++ Convert control parameters:
    CONTROL%SOLVER_TYPE                 = CONTROL_INTEGER(1)
    CONTROL%QUASI_NEUT                  = CONTROL_INTEGER(3)
    CONTROL%TAU                         = CONTROL_DOUBLE(1)
    CONTROL%AMIX                        = CONTROL_DOUBLE(2) 
    CONTROL%AMIXTR                      = CONTROL_DOUBLE(3)
    CONTROL%CONV                        = CONTROL_DOUBLE(4) 
    CONTROL%CONVREC                     = CONTROL_DOUBLE(5)
    CONTROL%OHMIC_HEATING_MULTIPLIER    = CONTROL_DOUBLE(6)



    IF(ALLOCATED(FUN)) DEALLOCATE (FUN)


    RETURN


  END SUBROUTINE CONVERT_CPO_TO_ETS_TYPES


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine converts ETS into the CPOs derived types.
!>
!> \author D.Kalupin
!>
!> \version "$Id: convert.f90 1671 2015-03-26 14:12:48Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE CONVERT_ETS_TO_CPO_TYPES                                 &
       (GEOMETRY, PROFILES, TRANSPORT, SOURCES, COREPROF)

!-------------------------------------------------------!
!     This routine converts ETS into the CPOs           !
!     derived types.                                    !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE itm_constants

    USE ETS_PLASMA

    IMPLICIT NONE

! +++ ETS derived types:
    TYPE (MAGNETIC_GEOMETRY),      INTENT(IN)  :: GEOMETRY  !contains all geometry quantities
    TYPE (PLASMA_PROFILES),        INTENT(IN)  :: PROFILES  !contains profiles of plasma parameters
    TYPE (TRANSPORT_COEFFICIENTS), INTENT(IN)  :: TRANSPORT !contains profiles of trasport coefficients
    TYPE (SOURCES_AND_SINKS),      INTENT(IN)  :: SOURCES   !contains profiles of sources

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),          POINTER     :: COREPROF(:)

! +++ COCOS:
    COREPROF(1)%datainfo%cocos               = 13

! +++ Convert geometry:
    COREPROF(1)%rho_tor                      = GEOMETRY%RHO
    COREPROF(1)%toroid_field%r0              = GEOMETRY%R0 
    COREPROF(1)%toroid_field%b0              = GEOMETRY%B0 



! +++ Convert profiles:
    COREPROF(1)%psi%value                    = PROFILES%PSI 
    COREPROF(1)%psi%ddrho                    = PROFILES%DPSI !AF - 25.Sep.2014
    COREPROF(1)%psi%sigma_par%value          = PROFILES%SIGMA
    COREPROF(1)%psi%boundary%value           = PROFILES%PSI_BND
    COREPROF(1)%psi%boundary%type            = PROFILES%PSI_BND_TYPE
!    COREPROF(1)%psi%boundary%rho             = PROFILES%PSI_BND_RHO
    COREPROF(1)%psi%jni%value                = PROFILES%JNI


    COREPROF(1)%ni%value                     = PROFILES%NI
    COREPROF(1)%ni%ddrho                     = PROFILES%DNI !AF - 25.Sep.2014
    COREPROF(1)%ni%flux%flux_dv              = PROFILES%FLUX_NI
    COREPROF(1)%ni%boundary%value            = PROFILES%NI_BND
    COREPROF(1)%ni%boundary%type             = PROFILES%NI_BND_TYPE
    COREPROF(1)%ni%boundary%rho_tor          = PROFILES%NI_BND_RHO
    COREPROF(1)%ni%transp_coef%diff          = PROFILES%DIFF_NI
    COREPROF(1)%ni%transp_coef%vconv         = PROFILES%VCONV_NI
    COREPROF(1)%ni%source_term%value         = PROFILES%SOURCE_NI
    COREPROF(1)%ni%source_term%integral      = PROFILES%INT_SOURCE_NI

    COREPROF(1)%ne%value                     = PROFILES%NE
    COREPROF(1)%ne%ddrho                     = PROFILES%DNE !AF - 25.Sep.2014
    COREPROF(1)%ne%flux%flux_dv              = PROFILES%FLUX_NE
    COREPROF(1)%ne%boundary%value            = PROFILES%NE_BND
    COREPROF(1)%ne%boundary%type             = PROFILES%NE_BND_TYPE
    COREPROF(1)%ne%boundary%rho_tor          = PROFILES%NE_BND_RHO
    COREPROF(1)%ne%transp_coef%diff          = PROFILES%DIFF_NE
    COREPROF(1)%ne%transp_coef%vconv         = PROFILES%VCONV_NE
    COREPROF(1)%ne%source_term%value         = PROFILES%SOURCE_NE
    COREPROF(1)%ne%source_term%integral      = PROFILES%INT_SOURCE_NE


    COREPROF(1)%profiles1d%zeff%value        = PROFILES%ZEFF

    COREPROF(1)%ti%value                     = PROFILES%TI
    COREPROF(1)%ti%ddrho                     = PROFILES%DTI !AF - 25.Sep.2014
    COREPROF(1)%ti%flux%flux_dv              = PROFILES%FLUX_TI*itm_ev
    COREPROF(1)%ti%boundary%value            = PROFILES%TI_BND
    COREPROF(1)%ti%boundary%type             = PROFILES%TI_BND_TYPE
    COREPROF(1)%ti%boundary%rho_tor          = PROFILES%TI_BND_RHO
    COREPROF(1)%ti%transp_coef%diff          = PROFILES%DIFF_TI
    COREPROF(1)%ti%transp_coef%vconv         = PROFILES%VCONV_TI
    COREPROF(1)%ti%source_term%value         = PROFILES%SOURCE_TI * itm_ev
    COREPROF(1)%ti%source_term%integral      = PROFILES%INT_SOURCE_TI * itm_ev

    COREPROF(1)%te%value                     = PROFILES%TE
    COREPROF(1)%te%ddrho                     = PROFILES%DTE !AF - 25.Sep.2014
    COREPROF(1)%te%flux%flux_dv              = PROFILES%FLUX_TE*itm_ev
    COREPROF(1)%te%boundary%value            = PROFILES%TE_BND
    COREPROF(1)%te%boundary%type             = PROFILES%TE_BND_TYPE
    COREPROF(1)%te%boundary%rho_tor          = PROFILES%TE_BND_RHO
    COREPROF(1)%te%transp_coef%diff          = PROFILES%DIFF_TE
    COREPROF(1)%te%transp_coef%vconv         = PROFILES%VCONV_TE
    COREPROF(1)%te%source_term%value         = PROFILES%SOURCE_TE * itm_ev
    COREPROF(1)%te%source_term%integral      = PROFILES%INT_SOURCE_TE * itm_ev

    COREPROF(1)%vtor%value                   = PROFILES%VTOR
    COREPROF(1)%vtor%ddrho                   = PROFILES%DVTOR !AF - 25.Sep.2014
    COREPROF(1)%vtor%flux%flux_dv            = PROFILES%FLUX_MTOR 
    COREPROF(1)%vtor%boundary%value          = PROFILES%VTOR_BND
    COREPROF(1)%vtor%boundary%type           = PROFILES%VTOR_BND_TYPE
    COREPROF(1)%vtor%boundary%rho_tor        = PROFILES%VTOR_BND_RHO
    COREPROF(1)%vtor%transp_coef%diff        = PROFILES%DIFF_VTOR
    COREPROF(1)%vtor%transp_coef%vconv       = PROFILES%VCONV_VTOR
    COREPROF(1)%vtor%source_term%value       = PROFILES%SOURCE_MTOR
    COREPROF(1)%vtor%source_term%integral    = PROFILES%INT_SOURCE_MTOR
   
    COREPROF(1)%profiles1d%jtot%value        = PROFILES%CURR_PAR
    COREPROF(1)%profiles1d%jni%value         = PROFILES%JNI
    COREPROF(1)%profiles1d%jphi%value        = PROFILES%CURR_TOR
    COREPROF(1)%profiles1d%joh%value         = PROFILES%JOH
    COREPROF(1)%profiles1d%vloop%value       = PROFILES%VLOOP
    COREPROF(1)%profiles1d%sigmapar%value    = PROFILES%SIGMA
    COREPROF(1)%profiles1d%qoh%value         = PROFILES%QOH
    COREPROF(1)%profiles1d%qoh%integral      = PROFILES%INT_QOH
    COREPROF(1)%profiles1d%eparallel%value   = PROFILES%E_PAR
    COREPROF(1)%profiles1d%q%value           = PROFILES%QSF
    COREPROF(1)%profiles1d%shear%value       = PROFILES%SHEAR
    COREPROF(1)%profiles1d%bpol%value        = PROFILES%BPOL

    COREPROF(1)%profiles1d%pe%value          = PROFILES%TE * PROFILES%NE * itm_ev
    COREPROF(1)%profiles1d%pi%value          = PROFILES%TI * PROFILES%NI * itm_ev
    COREPROF(1)%profiles1d%pi_tot%value      = SUM(COREPROF(1)%profiles1d%pi%value, dim=2)
    COREPROF(1)%profiles1d%pr_th%value       = COREPROF(1)%profiles1d%pe%value + SUM(COREPROF(1)%profiles1d%pi%value, dim=2)
    COREPROF(1)%profiles1d%pr_perp%value     = COREPROF(1)%profiles1d%pr_th%value
    COREPROF(1)%profiles1d%pr_parallel%value = COREPROF(1)%profiles1d%pr_th%value



    RETURN


  END SUBROUTINE CONVERT_ETS_TO_CPO_TYPES


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + 








! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!-------------------------------------------------------!
!-------------------------------------------------------!
     SUBROUTINE CONVERT_NEOCLASSIC2CORETRANSP(NEOCLASSIC, CORETRANSP)
!-------------------------------------------------------!
!     This routine converts transport data from         !
!     NEOCLASSIC to CORETRANSP CPO                      !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     should be udated after NCLASS       !
!                   is available to the ETS             !
!                                                       !
!-------------------------------------------------------!


     USE EUITM_SCHEMAS
     USE ITM_CONSTANTS
     USE COPY_STRUCTURES
     USE DEALLOCATE_STRUCTURES
     USE ALLOCATE_DEALLOCATE

     IMPLICIT NONE

     TYPE (type_neoclassic),  POINTER :: NEOCLASSIC(:)
     TYPE (type_coretransp),  POINTER :: CORETRANSP(:)
     TYPE (TYPE_COMPOSITIONS_TYPE)    :: COMPOSITIONS

     INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
     INTEGER                          :: NRHO
     INTEGER                          :: NNUCL
     INTEGER                          :: NION, IION
     INTEGER                          :: NIMP, IIMP, IZIMP      
     INTEGER,             ALLOCATABLE :: NZIMP(:)
     INTEGER                          :: NNEUT
     INTEGER,             ALLOCATABLE :: NCOMP(:)
     INTEGER,             ALLOCATABLE :: NTYPE(:)



!-------------------------------------------------------!


     NRHO   =            SIZE  (NEOCLASSIC(1)%rho_tor)
     CALL GET_COMP_DIMENSIONS  (NEOCLASSIC(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
     



     ALLOCATE                  (CORETRANSP(1))        
     ALLOCATE                  (CORETRANSP(1)%VALUES(1))        



! ++ Toroidal flux coordinate:
     IF (ASSOCIATED(NEOCLASSIC(1)%rho_tor)) THEN
        ALLOCATE               (CORETRANSP(1)%VALUES(1)%rho_tor(NRHO))
        CORETRANSP(1)%VALUES(1)%rho_tor  =  NEOCLASSIC(1)%rho_tor
     END IF



! ++ Compositions:
     IF (ASSOCIATED(NEOCLASSIC(1)%compositions%nuclei)) THEN
        CALL DEALLOCATE_CPO    (CORETRANSP(1)%compositions)
        CALL COPY_CPO          (NEOCLASSIC(1)%compositions, CORETRANSP(1)%compositions)
     ELSE        
        ALLOCATE               (NZIMP(NIMP))
        ALLOCATE               (NCOMP(NNEUT))
        ALLOCATE               (NTYPE(NNEUT))
        CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)
        CALL DEALLOCATE_CPO    (CORETRANSP(1)%compositions)
        CALL COPY_CPO          (COMPOSITIONS, CORETRANSP(1)%compositions)
     END IF



! ++ sigma
     IF (ASSOCIATED(NEOCLASSIC(1)%sigma)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%sigma(NRHO))
        CALL L3INTERP(NEOCLASSIC(1)%sigma, NEOCLASSIC(1)%rho_tor, NRHO,  &
                      CORETRANSP(1)%VALUES(1)%sigma, CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
     END IF



! ++ ni
     IF (ASSOCIATED(NEOCLASSIC(1)%ni_neo%diff_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(NRHO,NION,3))
        CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff   =  0.0_R8
        DO IION=1,NION
           CALL L3INTERP(NEOCLASSIC(1)%ni_neo%diff_eff(:,IION),      NEOCLASSIC(1)%rho_tor, NRHO,  &
                         CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(:,IION,3), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
        END DO
     END IF

     IF (ASSOCIATED(NEOCLASSIC(1)%ni_neo%vconv_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff(NRHO,NION,3))
        CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff  =  0.0_R8
        DO IION=1,NION
           CALL L3INTERP(NEOCLASSIC(1)%ni_neo%vconv_eff(:,IION),      NEOCLASSIC(1)%rho_tor, NRHO,  &
                         CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff(:,IION,3), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
        END DO
     END IF
       


! ++ ne
     IF (ASSOCIATED(NEOCLASSIC(1)%ne_neo%diff_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(NRHO,3))
        CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff   =  0.0_R8
        CALL L3INTERP(NEOCLASSIC(1)%ne_neo%diff_eff,         NEOCLASSIC(1)%rho_tor, NRHO,  &
                      CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,3), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
     END IF

     IF (ASSOCIATED(NEOCLASSIC(1)%ne_neo%vconv_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff(NRHO,3))
        CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff=0.0_R8
        CALL L3INTERP(NEOCLASSIC(1)%ne_neo%vconv_eff,         NEOCLASSIC(1)%rho_tor, NRHO,  &
                      CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff(:,3), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
     END IF
       


! ++ Ti
     IF (ASSOCIATED(NEOCLASSIC(1)%ti_neo%diff_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(NRHO,NION))
        DO IION=1,NION
           CALL L3INTERP(NEOCLASSIC(1)%ti_neo%diff_eff(:,IION),    NEOCLASSIC(1)%rho_tor, NRHO,  &
                         CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(:,IION), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
        END DO
     END IF

     IF (ASSOCIATED(NEOCLASSIC(1)%Ti_neo%vconv_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%Ti_transp%vconv_eff(NRHO,NION))
        DO IION=1,NION
           CALL L3INTERP(NEOCLASSIC(1)%Ti_neo%vconv_eff(:,IION),    NEOCLASSIC(1)%rho_tor, NRHO,  &
                         CORETRANSP(1)%VALUES(1)%Ti_transp%vconv_eff(:,IION), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
        END DO
     END IF



! ++ Te    
     IF (ASSOCIATED(NEOCLASSIC(1)%te_neo%diff_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%te_transp%diff_eff(NRHO))
        CALL L3INTERP(NEOCLASSIC(1)%te_neo%diff_eff,    NEOCLASSIC(1)%rho_tor, NRHO,  &
                      CORETRANSP(1)%VALUES(1)%te_transp%diff_eff, CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
     END IF

     IF (ASSOCIATED(NEOCLASSIC(1)%Te_neo%vconv_eff)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%Te_transp%vconv_eff(NRHO))
        CALL L3INTERP(NEOCLASSIC(1)%Te_neo%vconv_eff,    NEOCLASSIC(1)%rho_tor, NRHO,  &
                      CORETRANSP(1)%VALUES(1)%Te_transp%vconv_eff, CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
     END IF



! ++ vtor/mtor  



! ++ nz
     IF (ASSOCIATED(NEOCLASSIC(1)%nz_neo)) THEN
        ALLOCATE(CORETRANSP(1)%VALUES(1)%nz_transp(NIMP))
        DO IIMP = 1, NIMP
           IF (ASSOCIATED(NEOCLASSIC(1)%nz_neo(IIMP)%diff_eff)) THEN
              ALLOCATE(CORETRANSP(1)%VALUES(1)%nz_transp(IIMP)%diff_eff(NRHO,NZIMP(IIMP)))
              CORETRANSP(1)%VALUES(1)%nz_transp(IIMP)%diff_eff   =  0.0_R8
              DO IZIMP=1,NZIMP(IIMP)
                 CALL L3INTERP(NEOCLASSIC(1)%nz_neo(IIMP)%diff_eff(:,IZIMP),      NEOCLASSIC(1)%rho_tor, NRHO,  &
                               CORETRANSP(1)%VALUES(1)%nz_transp(IIMP)%diff_eff(:,IZIMP), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
              END DO
           END IF

           IF (ASSOCIATED(NEOCLASSIC(1)%nz_neo(IIMP)%vconv_eff)) THEN
              ALLOCATE(CORETRANSP(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff(NRHO,NZIMP(IIMP)))
              CORETRANSP(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff   =  0.0_R8
              DO IZIMP=1,NZIMP(IIMP)
                 CALL L3INTERP(NEOCLASSIC(1)%nz_neo(IIMP)%vconv_eff(:,IZIMP),      NEOCLASSIC(1)%rho_tor, NRHO,  &
                               CORETRANSP(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff(:,IZIMP), CORETRANSP(1)%VALUES(1)%rho_tor, NRHO)
              END DO
           END IF
        END DO
     END IF




!+++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (CORETRANSP(1)%VALUES(1)%transportid%id(1))
    ALLOCATE            (CORETRANSP(1)%VALUES(1)%transportid%description(1))
    CORETRANSP(1)%VALUES(1)%transportid%id          = 'neoclassical'
    CORETRANSP(1)%VALUES(1)%transportid%flag        = 2
    CORETRANSP(1)%VALUES(1)%transportid%description = 'Neoclassical'
     




    RETURN     


    END SUBROUTINE CONVERT_NEOCLASSIC2CORETRANSP
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  












! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!-------------------------------------------------------!
!-------------------------------------------------------!
     SUBROUTINE CONVERT_NEOCLASSIC2CORESOURCE(NEOCLASSIC, CORESOURCE)
!-------------------------------------------------------!
!     This routine converts sourceort data from         !
!     NEOCLASSIC to CORESOURCE CPO                      !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     should be udated after NCLASS       !
!                   is available to the ETS             !
!                                                       !
!-------------------------------------------------------!


     USE EUITM_SCHEMAS
     USE ITM_CONSTANTS
     USE COPY_STRUCTURES
     USE DEALLOCATE_STRUCTURES
     USE ALLOCATE_DEALLOCATE

     IMPLICIT NONE

     TYPE (type_neoclassic),  POINTER :: NEOCLASSIC(:)
     TYPE (type_coresource),  POINTER :: CORESOURCE(:)
     TYPE (TYPE_COMPOSITIONS_TYPE)    :: COMPOSITIONS

     INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
     INTEGER                          :: NRHO
     INTEGER                          :: NNUCL  = 1
     INTEGER                          :: NION   = 1
     INTEGER                          :: NIMP   = 0      
     INTEGER,             ALLOCATABLE :: NZIMP(:)
     INTEGER                          :: NNEUT  = 0 
     INTEGER,             ALLOCATABLE :: NCOMP(:)
     INTEGER,             ALLOCATABLE :: NTYPE(:)




!-------------------------------------------------------!


     NRHO   =  SIZE(NEOCLASSIC(1)%rho_tor)
     CALL GET_COMP_DIMENSIONS  (NEOCLASSIC(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)



     ALLOCATE                  (CORESOURCE(1))        
     ALLOCATE                  (CORESOURCE(1)%VALUES(1))        



! ++ Toroidal flux coordinate:
     IF (ASSOCIATED(NEOCLASSIC(1)%rho_tor)) THEN
        ALLOCATE               (CORESOURCE(1)%VALUES(1)%rho_tor(NRHO))
        CORESOURCE(1)%VALUES(1)%rho_tor  =  NEOCLASSIC(1)%rho_tor
     END IF



! ++ Compositions:
     IF (ASSOCIATED(NEOCLASSIC(1)%compositions%nuclei)) THEN
        CALL DEALLOCATE_CPO    (CORESOURCE(1)%compositions)
        CALL COPY_CPO          (NEOCLASSIC(1)%compositions, CORESOURCE(1)%compositions)
     ELSE        
        ALLOCATE               (NZIMP(NIMP))
        ALLOCATE               (NCOMP(NNEUT))
        ALLOCATE               (NTYPE(NNEUT))
        CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)
        CALL DEALLOCATE_CPO    (CORESOURCE(1)%compositions)
        CALL COPY_CPO          (COMPOSITIONS, CORESOURCE(1)%compositions)
     END IF



! ++ Bootstrap current:
     IF (ASSOCIATED(NEOCLASSIC(1)%jboot)) THEN
        ALLOCATE(CORESOURCE(1)%VALUES(1)%j(NRHO))
        CORESOURCE(1)%VALUES(1)%j         =  NEOCLASSIC(1)%jboot
     END IF





! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (CORESOURCE(1)%VALUES(1)%sourceid%id(1))
    ALLOCATE            (CORESOURCE(1)%VALUES(1)%sourceid%description(1))
    CORESOURCE(1)%VALUES(1)%sourceid%id          = 'neoclassical'
    CORESOURCE(1)%VALUES(1)%sourceid%flag        = 32
    CORESOURCE(1)%VALUES(1)%sourceid%description = 'Neoclassical'



    RETURN     


    END SUBROUTINE CONVERT_NEOCLASSIC2CORESOURCE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE CHANGERADII (EQUILIBRIUM,    &
                          COREPROF,       &
                          CORETRANSP,     &
                          CORESOURCE,     &
                          COREIMPUR,      &
                          CORENEUTRALS,   & 
                          NEOCLASSIC) 

! +++ Declaration of variables: 
    USE ITM_TYPES
    USE EUITM_SCHEMAS
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM),  POINTER  :: EQUILIBRIUM(:)      !input CPO

    TYPE (TYPE_COREPROF),     POINTER  :: COREPROF(:)         !input CPO  
    TYPE (TYPE_CORETRANSP),   POINTER  :: CORETRANSP(:)       !input CPO  
    TYPE (TYPE_CORESOURCE),   POINTER  :: CORESOURCE(:)       !input CPO  
    TYPE (TYPE_COREIMPUR),    POINTER  :: COREIMPUR(:)        !input CPO  
    TYPE (TYPE_CORENEUTRALS), POINTER  :: CORENEUTRALS(:)     !input CPO
    TYPE (TYPE_NEOCLASSIC),   POINTER  :: NEOCLASSIC(:)       !input CPO

    REAL(R8)                           :: RHO_TOR_RESCALE
    INTEGER(ITM_I4)                    :: NEQ


    NEQ                                  = SIZE(EQUILIBRIUM(1)%profiles_1d%rho_tor)

! +++ Copy input CPOs to output CPOs:
    RHO_TOR_RESCALE                      = 1.0_R8
    IF (ASSOCIATED(COREPROF(1)%rho_tor)) THEN
       IF (MAXVAL(COREPROF(1)%rho_tor).GE.EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ)*0.5_R8) &
       RHO_TOR_RESCALE                   = EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ) / &
                                           COREPROF(1)%rho_tor(SIZE(COREPROF(1)%rho_tor))
       COREPROF(1)%rho_tor               = COREPROF(1)%rho_tor * RHO_TOR_RESCALE
    END IF


    RHO_TOR_RESCALE                      = 1.0_R8
    IF (ASSOCIATED(CORETRANSP(1)%VALUES)) THEN
    IF (ASSOCIATED(CORETRANSP(1)%VALUES(1)%rho_tor)) THEN
       IF (MAXVAL(CORETRANSP(1)%VALUES(1)%rho_tor).GE.EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ)*0.5_R8) &
       RHO_TOR_RESCALE                   = EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ) / &
                                           CORETRANSP(1)%VALUES(1)%rho_tor(SIZE(CORETRANSP(1)%VALUES(1)%rho_tor))
       CORETRANSP(1)%VALUES(1)%rho_tor   = CORETRANSP(1)%VALUES(1)%rho_tor * RHO_TOR_RESCALE
    END IF
    END IF


    RHO_TOR_RESCALE                      = 1.0_R8
    IF (ASSOCIATED(CORESOURCE(1)%VALUES)) THEN
    IF (ASSOCIATED(CORESOURCE(1)%VALUES(1)%rho_tor)) THEN
       IF (MAXVAL(CORESOURCE(1)%VALUES(1)%rho_tor).GE.EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ)*0.5_R8) &
       RHO_TOR_RESCALE                   = EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ) / &
                                           CORESOURCE(1)%VALUES(1)%rho_tor(SIZE(CORESOURCE(1)%VALUES(1)%rho_tor))
       CORESOURCE(1)%VALUES(1)%rho_tor   = CORESOURCE(1)%VALUES(1)%rho_tor * RHO_TOR_RESCALE
    END IF
    END IF


    RHO_TOR_RESCALE                      = 1.0_R8
    IF (ASSOCIATED(COREIMPUR(1)%rho_tor)) THEN
       IF (MAXVAL(COREIMPUR(1)%rho_tor).GE.EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ)*0.5_R8) &
       RHO_TOR_RESCALE                   = EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ) / &
                                           COREIMPUR(1)%rho_tor(SIZE(COREIMPUR(1)%rho_tor))
       COREIMPUR(1)%rho_tor              = COREIMPUR(1)%rho_tor * RHO_TOR_RESCALE
    END IF

    RHO_TOR_RESCALE                      = 1.0_R8
    IF(ASSOCIATED(CORENEUTRALS(1)%rho_tor)) THEN
       IF (MAXVAL(CORENEUTRALS(1)%rho_tor).GE.EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ)*0.5_R8) &
       RHO_TOR_RESCALE                   = EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ) / &
                                           CORENEUTRALS(1)%rho_tor(SIZE(CORENEUTRALS(1)%rho_tor))
       CORENEUTRALS(1)%rho_tor           = CORENEUTRALS(1)%rho_tor * RHO_TOR_RESCALE
    ENDIF

    RHO_TOR_RESCALE                      = 1.0_R8
    IF(ASSOCIATED(NEOCLASSIC(1)%rho_tor)) THEN
       IF (MAXVAL(NEOCLASSIC(1)%rho_tor).GE.EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ)*0.5_R8) &
       RHO_TOR_RESCALE                   = EQUILIBRIUM(1)%profiles_1d%rho_tor(NEQ) / &
                                           NEOCLASSIC(1)%rho_tor(SIZE(NEOCLASSIC(1)%rho_tor))
       NEOCLASSIC(1)%rho_tor             = NEOCLASSIC(1)%rho_tor * RHO_TOR_RESCALE
    END IF


    RETURN 

    END SUBROUTINE CHANGERADII 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


END MODULE CONVERT
