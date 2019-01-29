  SUBROUTINE DATABASE_TRANSPORT(USER, MACHINE, VERSION, SHOT, RUN, INTERPOL, TIME, COREPROF, CORETRANSP_OUT, RHO_INTERPOL)

    USE ALLOCATE_DEALLOCATE
    USE DEALLOCATE_STRUCTURES

    USE ITM_CONSTANTS
    USE EUITM_ROUTINES
    USE EUITM_SCHEMAS
    USE INTERPOLATE_CPO
    USE COPY_STRUCTURES

    IMPLICIT NONE


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ CPOs:
    REAL (R8)                        :: TIME               !Time
    INTEGER                          :: SHOT,  RUN         !shot and run numbers
    INTEGER                          :: IDX                !index (internal)

    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_OUT(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_DB(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_TMP(:)   

    INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO1, NRHO2
    INTEGER                          :: NNUCL1
    INTEGER                          :: NION1
    INTEGER                          :: NIMP1       
    INTEGER,             ALLOCATABLE :: NZIMP1(:)
    INTEGER                          :: NNEUT1
    INTEGER,             ALLOCATABLE :: NCOMP1(:)
    INTEGER,             ALLOCATABLE :: NTYPE1(:)




    INTEGER                          :: INTERPOL           !interpolation index
    INTEGER                          :: RHO_INTERPOL

    CHARACTER(len=10)                :: CPOPATH
    CHARACTER(len=5),     PARAMETER  :: TREENAME = 'euitm'
    CHARACTER(*)                     :: USER
    CHARACTER(*)                     :: MACHINE
    CHARACTER(*)                     :: VERSION
 


! +++ allocate and define grid of output CPO:
    NRHO1                         = SIZE(COREPROF(1)%rho_tor,      DIM=1)
    CALL GET_COMP_DIMENSIONS      (COREPROF(1)%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL ALLOCATE_CORETRANSP_CPO  (NSLICE,      NRHO1, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1, CORETRANSP_OUT)
    call DEALLOCATE_CPO           (CORETRANSP_OUT(1)%COMPOSITIONS)
    CALL COPY_CPO                 (COREPROF(1)%COMPOSITIONS, CORETRANSP_OUT(1)%COMPOSITIONS)
    CORETRANSP_OUT(1)%VALUES(1)%rho_tor   = COREPROF(1)%rho_tor



! +++ Retrieve CPO from the data base:
    CPOPATH                     = 'coretransp' 
    ALLOCATE                      (CORETRANSP_DB(1))

#ifdef UAL
    CALL EUITM_OPEN_ENV           (TREENAME, SHOT, RUN, IDX, USER, MACHINE, VERSION)
    CALL EUITM_GET_SLICE          (IDX, CPOPATH, CORETRANSP_DB(1), TIME, INTERPOL)
#else
    WRITE (6,*) 'ERROR>>> NO CORETRANSP CPO IN THE DATABASE FOR SELECTED SHOT:', SHOT, RUN
    CALL ALLOCATE_CORETRANSP_CPO  (NSLICE,      NRHO1, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1, CORETRANSP_DB)
    call DEALLOCATE_CPO           (CORETRANSP_DB(1)%COMPOSITIONS)
    CALL COPY_CPO                 (COREPROF(1)%COMPOSITIONS, CORETRANSP_DB(1)%COMPOSITIONS)
    CORETRANSP_DB(1)%VALUES(1)%rho_tor   = COREPROF(1)%rho_tor
#endif



! +++ OUTPUT TRANSPORT CPO:
    NRHO2                       = SIZE (CORETRANSP_DB(1)%VALUES(1)%rho_tor, DIM=1)
  
    IF (RHO_INTERPOL.NE.0) &
       CORETRANSP_DB(1)%VALUES(1)%rho_tor =   CORETRANSP_DB(1)%VALUES(1)%rho_tor            &
                                            / CORETRANSP_DB(1)%VALUES(1)%rho_tor(NRHO2)     &
                                            * CORETRANSP_OUT(1)%VALUES(1)%rho_tor(NRHO1) 
 


! +++ Check:
    IF (SIZE(CORETRANSP_DB(1)%VALUES).GT.1) THEN
       ALLOCATE                   (CORETRANSP_TMP(1))
       ALLOCATE                   (CORETRANSP_TMP(1)%VALUES(1))
       CALL COPY_CPO              (CORETRANSP_DB(1)%VALUES(1), CORETRANSP_TMP(1)%VALUES(1))
       CALL DEALLOCATE_CPO        (CORETRANSP_DB(1)%VALUES)
       ALLOCATE                   (CORETRANSP_DB(1)%VALUES(1))
       CALL COPY_CPO              (CORETRANSP_TMP(1)%VALUES(1), CORETRANSP_DB(1)%VALUES(1))       
    END IF


! +++ Interpolate CPO:
    CALL INTERPOLATE_TRANSP       (CORETRANSP_DB(1), CORETRANSP_OUT(1), 0)



! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (CORETRANSP_OUT(1)%VALUES(1)%transportid%id(1))
    ALLOCATE            (CORETRANSP_OUT(1)%VALUES(1)%transportid%description(1))
    CORETRANSP_OUT(1)%VALUES(1)%transportid%id          = 'database'
    CORETRANSP_OUT(1)%VALUES(1)%transportid%flag        = 10
    CORETRANSP_OUT(1)%VALUES(1)%transportid%description = 'Transport specified by a database entry'
     


    CALL DEALLOCATE_CPO           (CORETRANSP_DB)
    IF (ALLOCATED(NZIMP1))        DEALLOCATE (NZIMP1)
    IF (ALLOCATED(NCOMP1))        DEALLOCATE (NCOMP1)
    IF (ALLOCATED(NTYPE1))        DEALLOCATE (NTYPE1)

#ifdef UAL
    CALL EUITM_CLOSE              (IDX, TREENAME, SHOT, RUN)
#endif


    RETURN


  END SUBROUTINE DATABASE_TRANSPORT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






