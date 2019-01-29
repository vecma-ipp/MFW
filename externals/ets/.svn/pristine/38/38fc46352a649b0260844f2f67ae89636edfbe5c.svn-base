! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
MODULE ETS_START 


CONTAINS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE ETSSTART                                                       &
!PARAMETERS & CPOs_IN:
                      (SOLVER,           EQUILIBRIUM_IN,   COREPROF_IN,     &
!CPOs_OUT:
                       COREPROF_OUT,     CORETRANSP_OUT,   CORESOURCE_OUT,  &
                       COREIMPUR_OUT,    CORENEUTRALS_OUT, NEOCLASSIC_OUT,  &
                       EQUILIBRIUM_OUT,  TOROIDFIELD_OUT,                   &
!BOUNDARY_CONDITIONS:
                       PSI_BND_TYPE,   NE_BND_TYPE,     NI_BND_TYPE,        &
                       TI_BND_TYPE,    TE_BND_TYPE,     VTOR_BND_TYPE,      &
                       IMP_BND_TYPE,   N0_BND_TYPE,     T0_BND_TYPE,        &
!
                       PSI_BND_VALUE,  NE_BND_VALUE,    NI_BND_VALUE,       &
                       TI_BND_VALUE,   TE_BND_VALUE,    VTOR_BND_VALUE,     &
                       IMP_BND_VALUE,  N0_BND_VALUE,    T0_BND_VALUE,       &
!SPACE_RESOLUTION:
                       NRHO, NPSI, NDIM1, NDIM2, NPOINTS) 

!-------------------------------------------------------!
!     This routine generates the RHO grid for ETS       !
!     and defines the number of ion species and saves   !
!     it in COREPROF CPO.                               !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


! +++ Declaration of variables: 
    USE ALLOCATE_DEALLOCATE
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
 

    IMPLICIT NONE


! +++ CPO derived types:
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_IN(:)    
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_IN(:)    

    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_OUT(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_OUT(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE_OUT(:)    
    TYPE (TYPE_COREIMPUR),   POINTER :: COREIMPUR_OUT(:)   
    TYPE (TYPE_CORENEUTRALS),POINTER :: CORENEUTRALS_OUT(:)   
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OUT(:)    
    TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD_OUT(:)    
!    TYPE (TYPE_COMPOSITIONC),POINTER :: COMPOSITIONC_OUT(:)    
    TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC_OUT(:)    

! +++ General settings:
    REAL (R8)                        :: RHOB                  !boundary value of RHO
    INTEGER                          :: SOLVER


! +++ Dimensions:
    INTEGER                          :: NRHO,     IRHO
    INTEGER                          :: NNUCL,    INUCL       !number of nuclei species
    INTEGER                          :: NION,     IION        !number of ion species
    INTEGER                          :: NIMP,     IIMP        !number of impurity species
    INTEGER,             ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
    INTEGER                          ::           IZIMP       
    INTEGER                          :: NNEUT,    INEUT       !number of neutrals species
    INTEGER,             ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
    INTEGER                          ::           ICOMP       
    INTEGER,             ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
    INTEGER                          ::           ITYPE       
    INTEGER                          :: NPSI                  !number of points for equilibrium 1-D arrays
    INTEGER                          :: NDIM1                 !number of points for equilibrium 2-D arrays, first dimension
    INTEGER                          :: NDIM2                 !number of points for equilibrium 2-D arrays, second dimension
    INTEGER                          :: NPOINTS               !number of points for equilibrium boundary 
 

! +++ Local:
    INTEGER                          :: PSI_BND_TYPE
    REAL (R8)                        :: PSI_BND_VALUE(3)
    INTEGER                          :: TE_BND_TYPE
    REAL (R8)                        :: TE_BND_VALUE(3)
    INTEGER                          :: NE_BND_TYPE
    REAL (R8)                        :: NE_BND_VALUE(3)
    INTEGER,             ALLOCATABLE :: NI_BND_TYPE(:)
    REAL (R8),           ALLOCATABLE :: NI_BND_VALUE(:,:)
    INTEGER,             ALLOCATABLE :: TI_BND_TYPE(:)
    REAL (R8),           ALLOCATABLE :: TI_BND_VALUE(:,:)
    INTEGER,             ALLOCATABLE :: VTOR_BND_TYPE(:)
    REAL (R8),           ALLOCATABLE :: VTOR_BND_VALUE(:,:)

    INTEGER,             ALLOCATABLE :: IMP_BND_TYPE(:,:)
    REAL (R8),           ALLOCATABLE :: IMP_BND_VALUE(:,:,:)

    INTEGER,             ALLOCATABLE :: N0_BND_TYPE(:,:)
    REAL (R8),           ALLOCATABLE :: N0_BND_VALUE(:,:,:)
    INTEGER,             ALLOCATABLE :: T0_BND_TYPE(:,:)
    REAL (R8),           ALLOCATABLE :: T0_BND_VALUE(:,:,:)

    REAL (R8),           ALLOCATABLE :: RHO(:)
    REAL (R8),           ALLOCATABLE :: RHON(:)


    INTEGER,              PARAMETER  :: NSLICE = 1         
    INTEGER                          :: I, NEUT_FLAG



    

    !----------------------------------------------------------------------!
    !     Allocation of output CPOs                                        !
    !----------------------------------------------------------------------!
    CALL GET_COMP_DIMENSIONS          (COREPROF_IN(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

    CALL ALLOCATE_COREPROF_CPO        (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREPROF_OUT    )
    CALL ALLOCATE_CORETRANSP_CPO      (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP_OUT  )        
    CALL ALLOCATE_CORESOURCE_CPO      (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE_OUT  )
    CALL ALLOCATE_CORENEUTRALS_CPO    (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORENEUTRALS_OUT)
    CALL ALLOCATE_COREIMPUR_CPO       (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREIMPUR_OUT   )        
    CALL ALLOCATE_NEOCLASSIC_CPO      (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, NEOCLASSIC_OUT  )        
    CALL ALLOCATE_TOROIDFIELD_CPO     (NSLICE,                                                          TOROIDFIELD_OUT )         
    ALLOCATE                          (EQUILIBRIUM_OUT(1))
    CALL COPY_CPO                     (EQUILIBRIUM_IN(1),                                               EQUILIBRIUM_OUT(1))

    !----------------------------------------------------------------------!
    !     Set up starting time for all CPOs                                !
    !----------------------------------------------------------------------!
     COREPROF_OUT(1)%time                           = EQUILIBRIUM_IN(1)%time
     CORETRANSP_OUT(1)%time                         = EQUILIBRIUM_IN(1)%time
     CORESOURCE_OUT(1)%time                         = EQUILIBRIUM_IN(1)%time
     IF(NIMP.GT.0) COREIMPUR_OUT(1)%time            = EQUILIBRIUM_IN(1)%time
     IF(NNEUT.GT.0) CORENEUTRALS_OUT(1)%time        = EQUILIBRIUM_IN(1)%time
     TOROIDFIELD_OUT(1)%time                        = EQUILIBRIUM_IN(1)%time



    !----------------------------------------------------------------------!
    !     Set up equidistant rho grid for all CPOs                         !
    !----------------------------------------------------------------------!
     ALLOCATE (RHO(NRHO))
     ALLOCATE (RHON(NRHO))

     RHOB                                           = EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(SIZE(EQUILIBRIUM_IN(1)%profiles_1d%rho_tor))

     RHO_LOOP1: DO IRHO=1,NRHO
        RHO(IRHO)                                   = RHOB/(NRHO-1)*(IRHO-1)  
        RHON(IRHO)                                  = (IRHO-1)/(NRHO-1)  
        IF (SOLVER.EQ.4.AND.IRHO.NE.1.AND.IRHO.NE.NRHO) THEN
           RHON(IRHO)                               = 1.0_R8/(NRHO-2)*(IRHO-2)+0.5_R8/(NRHO-2)
           RHO(IRHO)                                = RHON(IRHO) * RHOB  
        END IF 
     END DO RHO_LOOP1

     COREPROF_OUT(1)%rho_tor                        = RHO        
     CORETRANSP_OUT(1)%VALUES(1)%rho_tor            = RHO        
     CORESOURCE_OUT(1)%VALUES(1)%rho_tor            = RHO        
     IF(NIMP.GT.0) COREIMPUR_OUT(1)%rho_tor         = RHO    
     IF(NNEUT.GT.0) CORENEUTRALS_OUT(1)%rho_tor     = RHO 
     ALLOCATE (NEOCLASSIC_OUT(1)%rho_tor(NRHO))
     NEOCLASSIC_OUT(1)%rho_tor                      = RHO          


     COREPROF_OUT(1)%rho_tor_norm                   = RHON        
     CORETRANSP_OUT(1)%VALUES(1)%rho_tor_norm       = RHON        
     CORESOURCE_OUT(1)%VALUES(1)%rho_tor_norm       = RHON        
     IF(NIMP.GT.0) COREIMPUR_OUT(1)%rho_tor_norm    = RHON    
     IF(NNEUT.GT.0) CORENEUTRALS_OUT(1)%rho_tor_norm= RHON          
     ALLOCATE (NEOCLASSIC_OUT(1)%rho_tor_norm(NRHO))
     NEOCLASSIC_OUT(1)%rho_tor_norm                 = RHON          





    !----------------------------------------------------------------------!
    !     Set up PLASMA composition                                        !
    !----------------------------------------------------------------------!
     CALL DEALLOCATE_CPO   (COREPROF_OUT(1)%COMPOSITIONS)
     CALL COPY_CPO         (COREPROF_IN(1)%COMPOSITIONS,  COREPROF_OUT(1)%COMPOSITIONS)
     CALL DEALLOCATE_CPO   (CORETRANSP_OUT(1)%COMPOSITIONS)
     CALL COPY_CPO         (COREPROF_IN(1)%COMPOSITIONS,  CORETRANSP_OUT(1)%COMPOSITIONS)
     CALL DEALLOCATE_CPO   (CORESOURCE_OUT(1)%COMPOSITIONS)
     CALL COPY_CPO         (COREPROF_IN(1)%COMPOSITIONS,  CORESOURCE_OUT(1)%COMPOSITIONS)
     IF(NNEUT.GT.0) THEN
        CALL DEALLOCATE_CPO(CORENEUTRALS_OUT(1)%COMPOSITIONS)
        CALL COPY_CPO      (COREPROF_IN(1)%COMPOSITIONS,  CORENEUTRALS_OUT(1)%COMPOSITIONS)
     ENDIF
     IF(NIMP.GT.0) THEN
        CALL DEALLOCATE_CPO(COREIMPUR_OUT(1)%COMPOSITIONS)
        CALL COPY_CPO      (COREPROF_IN(1)%COMPOSITIONS,  COREIMPUR_OUT(1)%COMPOSITIONS)
     ENDIF
     CALL DEALLOCATE_CPO   (NEOCLASSIC_OUT(1)%COMPOSITIONS)
     CALL COPY_CPO         (COREPROF_IN(1)%COMPOSITIONS,  NEOCLASSIC_OUT(1)%COMPOSITIONS)





    !----------------------------------------------------------------------!
    !     Set up Impurity charge states                                    !
    !----------------------------------------------------------------------!
     IF(nimp.GT.0) THEN
        DO IIMP = 1,NIMP
           DO IZIMP = 1,NZIMP(IIMP)
              COREIMPUR_OUT(1)%IMPURITY(IIMP)%z(:,IZIMP)   = (COREIMPUR_OUT(1)%COMPOSITIONS%IMPURITIES(IIMP)%zmin(IZIMP) + &
                                                              COREIMPUR_OUT(1)%COMPOSITIONS%IMPURITIES(IIMP)%zmax(IZIMP) )/2.0_R8
              COREIMPUR_OUT(1)%IMPURITY(IIMP)%zsq(:,IZIMP) =  COREIMPUR_OUT(1)%IMPURITY(IIMP)%z(:,IZIMP)**2
           END DO
        END DO
     ENDIF




    !----------------------------------------------------------------------!
    !     Set up boundary condition type and value for main plasma         !
    !----------------------------------------------------------------------!
    
     COREPROF_OUT(1)%psi%boundary%type              = PSI_BND_TYPE
     COREPROF_OUT(1)%te%boundary%type               = TE_BND_TYPE
     COREPROF_OUT(1)%ne%boundary%type               = NE_BND_TYPE
     COREPROF_OUT(1)%ni%boundary%type               = NI_BND_TYPE
     COREPROF_OUT(1)%ti%boundary%type               = TI_BND_TYPE
     COREPROF_OUT(1)%vtor%boundary%type             = VTOR_BND_TYPE

     COREPROF_OUT(1)%psi%boundary%value             = PSI_BND_VALUE
     COREPROF_OUT(1)%te%boundary%value              = TE_BND_VALUE
     COREPROF_OUT(1)%ne%boundary%value              = NE_BND_VALUE
     COREPROF_OUT(1)%ni%boundary%value              = NI_BND_VALUE
     COREPROF_OUT(1)%ti%boundary%value              = TI_BND_VALUE
     COREPROF_OUT(1)%vtor%boundary%value            = VTOR_BND_VALUE

     COREPROF_OUT(1)%psi%flag                       = 0
     COREPROF_OUT(1)%te%flag                        = 0
     COREPROF_OUT(1)%ne%flag                        = 0
     COREPROF_OUT(1)%ni%flag                        = 0
     COREPROF_OUT(1)%ti%flag                        = 0
     COREPROF_OUT(1)%vtor%flag                      = 0

     IF (PSI_BND_TYPE.GE.1.AND.PSI_BND_TYPE.LE.5)   &
     COREPROF_OUT(1)%psi%flag                       = 2
     IF (PSI_BND_TYPE.EQ.6)                         &
     COREPROF_OUT(1)%psi%flag                       = 3
     IF (PSI_BND_TYPE.EQ.7)                         &
     COREPROF_OUT(1)%psi%flag                       = 1
 
     IF (NE_BND_TYPE.GE.1.AND.NE_BND_TYPE.LE.5)     &
     COREPROF_OUT(1)%ne%flag                        = 2
     IF (NE_BND_TYPE.EQ.6)                          &
     COREPROF_OUT(1)%ne%flag                        = 3
     IF (NE_BND_TYPE.EQ.7)                          &
     COREPROF_OUT(1)%ne%flag                        = 1

     IF (TE_BND_TYPE.GE.1.AND.TE_BND_TYPE.LE.5)     &
     COREPROF_OUT(1)%te%flag                        = 2
     IF (TE_BND_TYPE.EQ.6)                          &
     COREPROF_OUT(1)%te%flag                        = 3
     IF (TE_BND_TYPE.EQ.7)                          &
     COREPROF_OUT(1)%te%flag                        = 1

     DO IION = 1, NION

        IF (NI_BND_TYPE(IION).GE.1.AND.NI_BND_TYPE(IION).LE.5)     &
        COREPROF_OUT(1)%ni%flag(IION)               = 2
        IF (NI_BND_TYPE(IION).EQ.6)                 &
        COREPROF_OUT(1)%ni%flag(IION)               = 3
        IF (NI_BND_TYPE(IION).EQ.7)                 &
        COREPROF_OUT(1)%ni%flag(IION)               = 1

        IF (TI_BND_TYPE(IION).GE.1.AND.TI_BND_TYPE(IION).LE.5)     &
        COREPROF_OUT(1)%ti%flag(IION)               = 2
        IF (TI_BND_TYPE(IION).EQ.6)                 &
        COREPROF_OUT(1)%ti%flag(IION)               = 3
        IF (TI_BND_TYPE(IION).EQ.7)                 &
        COREPROF_OUT(1)%ti%flag(IION)               = 1

        IF (VTOR_BND_TYPE(IION).GE.1.AND.VTOR_BND_TYPE(IION).LE.5) &
        COREPROF_OUT(1)%VTOR%flag(IION)             = 2
        IF (VTOR_BND_TYPE(IION).EQ.6)               &
        COREPROF_OUT(1)%vtor%flag(IION)             = 3
        IF (VTOR_BND_TYPE(IION).EQ.7)               &
        COREPROF_OUT(1)%vtor%flag(IION)             = 1

     END DO

    !----------------------------------------------------------------------!
    !     Set up boundary condition type and value for impurities          !
    !----------------------------------------------------------------------!

     IF(nimp.GE.1) THEN
        DO IIMP = 1,NIMP
           DO IZIMP = 1,NZIMP(IIMP)
              COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%type(IZIMP)    = IMP_BND_TYPE(IIMP,IZIMP)
              COREIMPUR_OUT(1)%IMPURITY(IIMP)%boundary%value(:,IZIMP) = IMP_BND_VALUE(IIMP,:,IZIMP)
           END DO
        END DO
        DEALLOCATE(IMP_BND_TYPE, IMP_BND_VALUE)
     ENDIF
    
     DEALLOCATE(NI_BND_TYPE,  TI_BND_TYPE,  VTOR_BND_TYPE)
     DEALLOCATE(NI_BND_VALUE, TI_BND_VALUE, VTOR_BND_VALUE)


    !----------------------------------------------------------------------!
    !     Set up boundary condition type and value for neutrals            !
    !----------------------------------------------------------------------!

     IF(nneut.GE.1) THEN
        DO INEUT = 1, NNEUT
           DO ITYPE = 1, NTYPE(INEUT)
              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%boundary%type           = N0_BND_TYPE(INEUT,ITYPE)
              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%t0%boundary%type           = T0_BND_TYPE(INEUT,ITYPE)
              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%v0%toroidal%boundary%type  = 0
              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%v0%poloidal%boundary%type  = 0

              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%n0%boundary%value(:)       = N0_BND_VALUE(INEUT,:,ITYPE)
              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%t0%boundary%value(:)       = T0_BND_VALUE(INEUT,:,ITYPE)
              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%v0%toroidal%boundary%value = 0.0_R8
              CORENEUTRALS_OUT(1)%PROFILES(INEUT)%NEUTRALTYPE(ITYPE)%v0%poloidal%boundary%value = 0.0_R8
           ENDDO
        ENDDO
        DEALLOCATE(N0_BND_TYPE, T0_BND_TYPE, N0_BND_VALUE, T0_BND_VALUE)
     ENDIF

     DEALLOCATE(RHO, RHON)


    !----------------------------------------------------------------------!
    !     Synchronise Ip, B0, R0 in different CPOs                         !
    !----------------------------------------------------------------------!

    COREPROF_OUT(1)%globalparam%current_tot         = EQUILIBRIUM_IN(1)%global_param%i_plasma
    COREPROF_OUT(1)%toroid_field%r0                 = EQUILIBRIUM_IN(1)%global_param%toroid_field%r0
    COREPROF_OUT(1)%toroid_field%b0                 = EQUILIBRIUM_IN(1)%global_param%toroid_field%b0
    TOROIDFIELD_OUT(1)%current%value                = EQUILIBRIUM_IN(1)%global_param%i_plasma
    TOROIDFIELD_OUT(1)%r0                           = EQUILIBRIUM_IN(1)%global_param%toroid_field%r0   
    TOROIDFIELD_OUT(1)%bvac_r%value                 = EQUILIBRIUM_IN(1)%global_param%toroid_field%b0

    RETURN

  END SUBROUTINE ETSSTART
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   




END MODULE ETS_START 
