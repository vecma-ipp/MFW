
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FC2K_PLASMA_COMPOSITION (COREPROF_DB, COREPROF_OUT,             &
                                 NION,      NIMP,      NNEUT,           &
                                 AMN_ION,   ZN_ION,    Z_ION,           &
                                 AMN_IMP,   ZN_IMP,    MAXZ_IMP,        &
                                 NCOMP_IN,  NTYPE_IN,                   &
                                 NCOLD,     NTHERMAL,  NFAST,   NNBI) 


! +++ Declaration of variables: 
    use itm_types
    USE EUITM_SCHEMAS
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES
    USE PLASMA_COMPOSITION



    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),     POINTER   :: COREPROF_DB(:)         !input  CPO slice 
    TYPE (TYPE_COREPROF),     POINTER   :: COREPROF_OUT(:)        !output CPO slice 
    TYPE (TYPE_COMPOSITIONC), POINTER   :: COMPOSITIONC(:)

! +++ Input:
    INTEGER,                  PARAMETER  :: NOCUR = 1             !number of CPO ocurancies in the work flow
    INTEGER                              :: NNUCL                 !number of nuclei species
    INTEGER                              :: NION                  !number of ion species
    INTEGER                              :: NIMP                  !number of impurity species
    INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
    INTEGER                              :: NNEUT                 !number of neutrals species
    INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
    INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
 
    INTEGER                              :: NCOLD, NTHERMAL, NFAST, NNBI     

    REAL (R8)                            :: AMN_ION(30),  ZN_ION(30),  Z_ION(30)
    REAL (R8)                            :: AMN_IMP(100), ZN_IMP(100), MAXZ_IMP(100)

    INTEGER                              :: NCOMP_IN(130)
    INTEGER                              :: NTYPE_IN(130)

! +++ Local:
    REAL (R8),              ALLOCATABLE  :: AMN(:),    ZN(:),    ZION(:)
    REAL (R8),              ALLOCATABLE  :: AMNIMP(:), ZNIMP(:), MAXZIMP(:)


    ALLOCATE (AMN(30),     ZN(30),     ZION(30))
    ALLOCATE (AMNIMP(130), ZNIMP(130), MAXZIMP(130))

    AMN      = AMN_ION
    ZN       = ZN_ION
    ZION     = Z_ION
    AMNIMP   = AMN_IMP
    ZNIMP    = ZN_IMP
    MAXZIMP  = MAXZ_IMP



    CALL SET_PLASMA_COMPOSITION (COREPROF_OUT,                          &
                                 NION,      NIMP,      NNEUT,           &
                                 AMN,       ZN,        ZION,            &
                                 AMNIMP,    ZNIMP,     MAXZIMP,         &
                                 NCOMP_IN,  NTYPE_IN,                   &
                                 NCOLD,     NTHERMAL,  NFAST,   NNBI) 



    RETURN 


    END SUBROUTINE FC2K_PLASMA_COMPOSITION
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
