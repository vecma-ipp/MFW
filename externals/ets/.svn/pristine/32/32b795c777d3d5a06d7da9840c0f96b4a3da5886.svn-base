    SUBROUTINE DATABASE_PROFILES(USER, MACHINE, VERSION, SHOT, RUN, INTERPOL, TIME, COREPROF, COREPROF_OUT, RHO_INTERPOL)

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
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_OUT(:)    
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_DB(:)    

    INTEGER,              PARAMETER  :: NSLICE = 1             !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO1, NRHO2
    INTEGER                          :: NNUCL1
    INTEGER                          :: NION1, IION1
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
    NRHO1                  =  SIZE(COREPROF(1)%rho_tor,      DIM=1)
    CALL GET_COMP_DIMENSIONS      (COREPROF(1)%COMPOSITIONS, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1)
    CALL ALLOCATE_COREPROF_CPO    (NSLICE,      NRHO1, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1, COREPROF_OUT)
    CALL COPY_CPO                 (COREPROF(1), COREPROF_OUT(1))
    COREPROF_OUT(1)%rho_tor     =  COREPROF(1)%rho_tor



! +++ Retrieve CPO from the data base:
    CPOPATH                     = 'COREPROF' 
    ALLOCATE                      (COREPROF_DB(1))

#ifdef UAL
    CALL EUITM_OPEN_ENV           (TREENAME, SHOT, RUN, IDX, USER, MACHINE, VERSION)
    CALL EUITM_GET_SLICE          (IDX, CPOPATH, COREPROF_DB(1), TIME, INTERPOL)
#else
    WRITE (6,*) 'ERROR>>> NO COREPROF CPO IN THE DATABASE FOR SELECTED SHOT:', SHOT, RUN
    CALL ALLOCATE_COREPROF_CPO    (NSLICE,      NRHO1, NNUCL1, NION1,  NIMP1,  NZIMP1, NNEUT1, NTYPE1, NCOMP1, COREPROF_DB)
    call deallocate_cpo(COREPROF_DB(1)%COMPOSITIONS)
    CALL COPY_CPO                 (COREPROF(1)%COMPOSITIONS, COREPROF_DB(1)%COMPOSITIONS)
    COREPROF_DB(1)%rho_tor      =  COREPROF(1)%rho_tor
#endif



! +++ OUTPUT TRANSPORT CPO:
    NRHO2                  = SIZE (COREPROF_DB(1)%rho_tor, DIM=1)
  
    IF (RHO_INTERPOL.NE.0) &
       COREPROF_DB(1)%rho_tor   =  COREPROF_DB(1)%rho_tor              &
                                   / COREPROF_DB(1)%rho_tor(NRHO2)     &
                                   * COREPROF_OUT(1)%rho_tor(NRHO1) 
 


! +++ Interpolate CPO:
    CALL INTERPOLATE_PROF         (COREPROF_DB(1), COREPROF(1))




! +++ Copy profiles:
    IF (COREPROF_OUT(1)%psi%flag .EQ. 1)                                &
        COREPROF_OUT(1)%psi%value              = COREPROF(1)%psi%value
    IF (COREPROF_OUT(1)%ne%flag  .EQ. 1)                                &
        COREPROF_OUT(1)%ne%value               = COREPROF(1)%ne%value
    IF (COREPROF_OUT(1)%te%flag  .EQ. 1)                                &
        COREPROF_OUT(1)%te%value               = COREPROF(1)%te%value

    DO IION1 = 1, NION1

       IF (COREPROF_OUT(1)%ni%flag(IION1)   .EQ. 1)                                &
           COREPROF_OUT(1)%ni%value(:,IION1)   = COREPROF(1)%ni%value(:,IION1)
       IF (COREPROF_OUT(1)%ti%flag(IION1)   .EQ. 1)                                &
           COREPROF_OUT(1)%ti%value(:,IION1)   = COREPROF(1)%ti%value(:,IION1)
       IF (COREPROF_OUT(1)%vtor%flag(IION1) .EQ. 1)                                &
           COREPROF_OUT(1)%vtor%value(:,IION1) = COREPROF(1)%vtor%value(:,IION1)

    END DO

    IF (COREPROF_OUT(1)%ne%flag  .EQ. 3)       THEN
        COREPROF_OUT(1)%ne%boundary%value(1)      = COREPROF(1)%ne%value(NRHO1)
        COREPROF_OUT(1)%ne%boundary%value(2)      = 0.0_R8
        COREPROF_OUT(1)%ne%boundary%value(3)      = 0.0_R8
        COREPROF_OUT(1)%ne%boundary%type          = 1
    END IF
    IF (COREPROF_OUT(1)%te%flag  .EQ. 3)       THEN
        COREPROF_OUT(1)%te%boundary%value(1)      = COREPROF(1)%te%value(NRHO1)
        COREPROF_OUT(1)%te%boundary%value(2)      = 0.0_R8
        COREPROF_OUT(1)%te%boundary%value(3)      = 0.0_R8
        COREPROF_OUT(1)%te%boundary%type          = 1
    END IF

    DO IION1 = 1, NION1

       IF (COREPROF_OUT(1)%ni%flag(IION1)   .EQ. 3) THEN
          COREPROF_OUT(1)%ni%boundary%value(1,IION1)       = COREPROF(1)%ni%value(NRHO1,IION1)
           COREPROF_OUT(1)%ni%boundary%value(2,IION1)      = 0.0_R8
           COREPROF_OUT(1)%ni%boundary%value(3,IION1)      = 0.0_R8
           COREPROF_OUT(1)%ni%boundary%type(IION1)         = 1

       END IF
       IF (COREPROF_OUT(1)%ti%flag(IION1)   .EQ. 3) THEN
           COREPROF_OUT(1)%ti%boundary%value(1,IION1)      = COREPROF(1)%ti%value(NRHO1,IION1)
           COREPROF_OUT(1)%ti%boundary%value(2,IION1)      = 0.0_R8
           COREPROF_OUT(1)%ti%boundary%value(3,IION1)      = 0.0_R8
           COREPROF_OUT(1)%ti%boundary%type(IION1)         = 1

       END IF
       IF (COREPROF_OUT(1)%vtor%flag(IION1) .EQ. 3) THEN
           COREPROF_OUT(1)%vtor%boundary%value(1,IION1)    = COREPROF(1)%vtor%value(NRHO1,IION1)
           COREPROF_OUT(1)%vtor%boundary%value(2,IION1)    = 0.0_R8
           COREPROF_OUT(1)%vtor%boundary%value(3,IION1)    = 0.0_R8
           COREPROF_OUT(1)%vtor%boundary%type(IION1)       = 1

       END IF

    END DO

    


    CALL DEALLOCATE_CPO                  (COREPROF_DB)
    IF (ALLOCATED(NZIMP1)) DEALLOCATE    (NZIMP1)
    IF (ALLOCATED(NCOMP1)) DEALLOCATE    (NCOMP1)
    IF (ALLOCATED(NTYPE1)) DEALLOCATE    (NTYPE1)

#ifdef UAL
    CALL EUITM_CLOSE                     (IDX, TREENAME, SHOT, RUN)
#endif


    RETURN


  END SUBROUTINE DATABASE_PROFILES
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






