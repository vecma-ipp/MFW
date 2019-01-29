! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FC2K_ETSSTART                                                  &
!PARAMETERS & CPOs_IN:
                      (SOLVER,           EQUILIBRIUM_IN,   COREPROF_IN,     &
!CPOs_OUT:
                       COREPROF_OUT,     CORETRANSP_OUT,   CORESOURCE_OUT,  &
                       COREIMPUR_OUT,    CORENEUTRALS_OUT, NEOCLASSIC_OUT,  &
                       EQUILIBRIUM_OUT,  TOROIDFIELD_OUT,                   &
!BOUNDARY_CONDITIONS:
                       BC_MAIN_INT,      BC_MAIN_REAL,                      &
                       BC_IMP_INT,       BC_IMP_REAL,                       &
                       BC_NEUTR_INT,     BC_NEUTR_N0,       BC_NEUTR_T0,    &
!SPACE_RESOLUTION:
                       RESOLUTIONS                                          ) 

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
    USE ETS_START 
 

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
    TYPE (TYPE_COMPOSITIONC),POINTER :: COMPOSITIONC_OUT(:)    
    TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC_OUT(:)    

! +++ General settings:
    INTEGER                          :: SOLVER

! +++ Boundary conditions:
    INTEGER                          :: BC_MAIN_INT(12)    
    REAL (R8)                        :: BC_MAIN_REAL(36)

    INTEGER                          :: BC_IMP_INT    
    REAL (R8)                        :: BC_IMP_REAL(500)

    INTEGER                          :: BC_NEUTR_INT(2)    
    REAL (R8)                        :: BC_NEUTR_N0(520)
    REAL (R8)                        :: BC_NEUTR_T0(520)

! +++ Dimensions:
    INTEGER                          :: RESOLUTIONS(5)    


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


    INTEGER,              PARAMETER  :: NSLICE = 1         
    INTEGER                          :: I, NEUT_FLAG


    !----------------------------------------------------------------------!
    !     Fill input parameters                                            !
    !----------------------------------------------------------------------!
    NRHO                                             = RESOLUTIONS(1)
    NPSI                                             = RESOLUTIONS(2)
    NDIM1                                            = RESOLUTIONS(3)
    NDIM2                                            = RESOLUTIONS(4)
    NPOINTS                                          = RESOLUTIONS(5)

    CALL GET_COMP_DIMENSIONS       (COREPROF_IN(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

    IF (NION.GT.3) WRITE(*,*)'WARNING "NION > 3": approximations will be applied'
    IF (NIMP.GT.5) WRITE(*,*)'WARNING "NIMP > 5": approximations will be applied'



    !----------------------------------------------------------------------!
    !     Set up boundary condition type and value for main plasma         !
    !----------------------------------------------------------------------!
    
     ALLOCATE (NI_BND_TYPE(NION))
     ALLOCATE (TI_BND_TYPE(NION))
     ALLOCATE (VTOR_BND_TYPE(NION))
    
     ALLOCATE (NI_BND_VALUE(3,NION))
     ALLOCATE (TI_BND_VALUE(3,NION))
     ALLOCATE (VTOR_BND_VALUE(3,NION))


     PSI_BND_TYPE                                   = BC_MAIN_INT(1)
     TE_BND_TYPE                                    = BC_MAIN_INT(2)
     NE_BND_TYPE                                    = BC_MAIN_INT(3)

     DO IION = 1,NION
        TI_BND_TYPE(IION)                           = BC_MAIN_INT(4 +MIN(IION-1,2))
        NI_BND_TYPE(IION)                           = BC_MAIN_INT(7 +MIN(IION-1,2))
        VTOR_BND_TYPE(IION)                         = BC_MAIN_INT(10+MIN(IION-1,2))
     END DO

     DO I = 1,3
        PSI_BND_VALUE(I)                            = BC_MAIN_REAL((i-1)*12+1)
        TE_BND_VALUE(I)                             = BC_MAIN_REAL((i-1)*12+2)
        NE_BND_VALUE(I)                             = BC_MAIN_REAL((i-1)*12+6)
        DO IION = 1,NION
           TI_BND_VALUE(I,IION)                     = BC_MAIN_REAL((i-1)*12+3 +MIN(IION-1,2))
           NI_BND_VALUE(I,IION)                     = BC_MAIN_REAL((i-1)*12+7 +MIN(IION-1,2))
           VTOR_BND_VALUE(I,IION)                   = BC_MAIN_REAL((i-1)*12+10+MIN(IION-1,2))
        END DO
     END DO




    !----------------------------------------------------------------------!
    !     Set up boundary condition type and value for impurities          !
    !----------------------------------------------------------------------!
    IF (NIMP.GE.1) THEN
       ALLOCATE   (IMP_BND_VALUE(NIMP,3,MAXVAL(NZIMP)))
       ALLOCATE   (IMP_BND_TYPE(NIMP,   MAXVAL(NZIMP)))

       DO IIMP = 1,NIMP
          IMP_BND_TYPE(IIMP,:)                          = BC_IMP_INT          
          DO IZIMP = 1,NZIMP(IIMP)
             IMP_BND_VALUE(IIMP,1,IZIMP)                = BC_IMP_REAL(MIN(4,(IIMP-1))*100+IZIMP)
             IMP_BND_VALUE(IIMP,2,IZIMP)                = 0.0_R8
             IMP_BND_VALUE(IIMP,3,IZIMP)                = 0.0_R8
          END DO
       END DO
    END IF


    


    !----------------------------------------------------------------------!
    !     Set up boundary condition type and value for neutrals            !
    !----------------------------------------------------------------------!
     IF (NNEUT.GE.1) THEN
        ALLOCATE (N0_BND_VALUE(NNEUT,3,MAXVAL(NTYPE)))
        ALLOCATE (T0_BND_VALUE(NNEUT,3,MAXVAL(NTYPE)))
        ALLOCATE (N0_BND_TYPE(NNEUT,   MAXVAL(NTYPE)))
        ALLOCATE (T0_BND_TYPE(NNEUT,   MAXVAL(NTYPE)))
        
        DO INEUT = 1, NNEUT          
           DO ITYPE = 1, NTYPE(INEUT)
              NEUT_FLAG                                 = COREPROF_IN(1)%COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%flag
              
              N0_BND_TYPE(INEUT,ITYPE)                  = BC_NEUTR_INT(1)
              T0_BND_TYPE(INEUT,ITYPE)                  = BC_NEUTR_INT(2)

              N0_BND_VALUE(INEUT,1,ITYPE)               = BC_NEUTR_N0(INEUT+NEUT_FLAG*130)
              T0_BND_VALUE(INEUT,1,ITYPE)               = BC_NEUTR_T0(INEUT+NEUT_FLAG*130)

              N0_BND_VALUE(INEUT,2,ITYPE)               = 0._R8
              T0_BND_VALUE(INEUT,2,ITYPE)               = 0._R8
              N0_BND_VALUE(INEUT,3,ITYPE)               = 0._R8
              T0_BND_VALUE(INEUT,3,ITYPE)               = 0._R8
           END DO
        END DO
     END IF

 


    !----------------------------------------------------------------------!
    !     Call CPO allocation                                              !
    !----------------------------------------------------------------------!
     CALL ETSSTART                                                          &
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




    !----------------------------------------------------------------------!
    !     Deallocate internal variables                                    !
    !----------------------------------------------------------------------!
     IF (ALLOCATED(NI_BND_TYPE))   DEALLOCATE (NI_BND_TYPE)
     IF (ALLOCATED(TI_BND_TYPE))   DEALLOCATE (TI_BND_TYPE)
     IF (ALLOCATED(VTOR_BND_TYPE)) DEALLOCATE (VTOR_BND_TYPE)  
     IF (ALLOCATED(NI_BND_VALUE))  DEALLOCATE (NI_BND_VALUE)
     IF (ALLOCATED(TI_BND_VALUE))  DEALLOCATE (TI_BND_VALUE)
     IF (ALLOCATED(VTOR_BND_VALUE))DEALLOCATE (VTOR_BND_VALUE)
     IF (ALLOCATED(IMP_BND_VALUE)) DEALLOCATE (IMP_BND_VALUE)
     IF (ALLOCATED(N0_BND_VALUE))  DEALLOCATE (N0_BND_VALUE)
     IF (ALLOCATED(T0_BND_VALUE))  DEALLOCATE (T0_BND_VALUE)
     IF (ALLOCATED(N0_BND_TYPE))   DEALLOCATE (N0_BND_TYPE)
     IF (ALLOCATED(T0_BND_TYPE))   DEALLOCATE (T0_BND_TYPE)
     IF (ALLOCATED(NZIMP))         DEALLOCATE (NZIMP)
     IF (ALLOCATED(NTYPE))         DEALLOCATE (NTYPE)
     IF (ALLOCATED(NCOMP))         DEALLOCATE (NCOMP)




    RETURN


  END SUBROUTINE FC2K_ETSSTART 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   







