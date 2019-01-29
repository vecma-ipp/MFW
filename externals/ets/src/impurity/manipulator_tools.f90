MODULE MANIPULATOR_TOOLS
!--------------------------------------------------------
!-------------------------------------------------------!
!     This module defines types of internal             !
!     variables used by IMPURITY_MANIPULATOR            !
!-------------------------------------------------------!
!     Developers:   D.Kalupin                           !
!     Contacts:     Denis.Kalupin@euro-fusion.org       !
!                                                       !
!     Comments:     created for the ETS                 !
!                                                       !
!-------------------------------------------------------!
!--------------------------------------------------------

CONTAINS
!--------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE OUTPUT_COMPOSITIONS  (PARAM, COREPROF, COREIMPUR, DIAG)

    USE ALLOCATE_DEALLOCATE
    USE ETS_PLASMA
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE MANIPULATOR_TYPE

    IMPLICIT NONE
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)   
    TYPE (TYPE_COREIMPUR),   POINTER :: COREIMPUR(:)
    TYPE (TYPE_COMPOSITIONC),POINTER :: COMPOSITIONC(:)
    TYPE (MANIPULATOR_PARAM)         :: PARAM               
    TYPE (DIAGNOSTIC)                :: DIAG

    REAL (R8),           ALLOCATABLE :: AMN(:)
    REAL (R8),           ALLOCATABLE :: ZN(:)
    INTEGER,             ALLOCATABLE :: NUCINDEX(:)
    INTEGER,             ALLOCATABLE :: NZIMP(:)
    INTEGER,             ALLOCATABLE :: NCOMP(:)
    INTEGER,             ALLOCATABLE :: NTYPE(:)

    INTEGER                          :: NRHO
    INTEGER                          :: NNUCL
    INTEGER                          :: INUCL
    INTEGER                          :: IIMP, IZIMP

    ALLOCATE (AMN(PARAM%NIMP))
    ALLOCATE (ZN(PARAM%NIMP))
    ALLOCATE (NUCINDEX(PARAM%NIMP))
    ALLOCATE (NZIMP(PARAM%NIMP))

    NNUCL   = 0
    DO IIMP = 1,PARAM%NIMP
       IF (PARAM%AMN_IMP(IIMP).GT.0._R8.AND.PARAM%ZN_IMP(IIMP).GT.0._R8) THEN
          IF (NNUCL.GE.1) THEN
             DO INUCL = 1, NNUCL
                IF (ABS(AMN(INUCL)-PARAM%AMN_IMP(IIMP)) .LE. 0.25 .AND. &
                    ABS(ZN(INUCL)-PARAM%ZN_IMP(IIMP)) .LE. 0.25)  THEN   
                    NUCINDEX(IIMP)  = INUCL
                    GOTO 12                                              
                END IF
             ENDDO
          ENDIF
          NNUCL                =  NNUCL + 1                             
          AMN(NNUCL)           =  PARAM%AMN_IMP(IIMP)
          ZN(NNUCL)            =  PARAM%ZN_IMP(IIMP)
          NUCINDEX(IIMP)       =  NNUCL
 12       CONTINUE
       END IF 
       IF (TRIM(ADJUSTL(PARAM%ISTATE(IIMP))).EQ."all") THEN
          NZIMP(IIMP)          =  MAXVAL(PARAM%Z_IMP(IIMP,:))
       ELSE
          NZIMP(IIMP)          =  1
       END IF
    END DO

    NRHO = SIZE(COREPROF(1)%rho_tor)

    CALL ALLOCATE_COREIMPUR_CPO    (1, NRHO,  NNUCL, 0, PARAM%NIMP,  NZIMP, 0, NTYPE, NCOMP, COREIMPUR)        

! +++ Nuclei:
    DO INUCL = 1, NNUCL
       COREIMPUR(1)%compositions%NUCLEI(INUCL)%zn                    = ZN(INUCL)
       COREIMPUR(1)%compositions%NUCLEI(INUCL)%amn                   = AMN(INUCL)
       COREIMPUR(1)%compositions%NUCLEI(INUCL)%label                 = " "
    END DO
    

! +++ Impurities:
    DO IIMP = 1, PARAM%NIMP
       COREIMPUR(1)%compositions%IMPURITIES(IIMP)%nucindex           = NUCINDEX(IIMP)
       COREIMPUR(1)%compositions%IMPURITIES(IIMP)%nzimp              = NZIMP(IIMP)
       IF (TRIM(ADJUSTL(PARAM%ISTATE(IIMP))).EQ."all") THEN
          DO IZIMP = 1, NZIMP(IIMP)
             COREIMPUR(1)%compositions%IMPURITIES(IIMP)%zmin(IZIMP)  = IZIMP
             COREIMPUR(1)%compositions%IMPURITIES(IIMP)%zmax(IZIMP)  = IZIMP
             COREIMPUR(1)%IMPURITY(IIMP)%z(:,IZIMP)                  = IZIMP
             COREIMPUR(1)%IMPURITY(IIMP)%zsq(:,IZIMP)                = IZIMP**2
             COREIMPUR(1)%compositions%IMPURITIES(IIMP)%label(IZIMP) = " "
          END DO
       ELSE
          COREIMPUR(1)%compositions%IMPURITIES(IIMP)%zmin(:)         = MAXVAL(PARAM%Z_IMP(IIMP,:))
          COREIMPUR(1)%compositions%IMPURITIES(IIMP)%zmax(:)         = MAXVAL(PARAM%Z_IMP(IIMP,:))
          COREIMPUR(1)%IMPURITY(IIMP)%z(:,:)                         = MAXVAL(PARAM%Z_IMP(IIMP,:))
          COREIMPUR(1)%IMPURITY(IIMP)%zsq(:,:)                       = MAXVAL(PARAM%Z_IMP(IIMP,:))**2
          COREIMPUR(1)%compositions%IMPURITIES(IIMP)%label(:)        = " "
       END IF

    END DO
    
! +++ grid:
    COREIMPUR(1)%rho_tor                                             = COREPROF(1)%rho_tor                 
    COREIMPUR(1)%rho_tor_norm                                        = COREPROF(1)%rho_tor_norm                

    RETURN

  END SUBROUTINE OUTPUT_COMPOSITIONS
!--------------------------------------------------------
!--------------------------------------------------------



!--------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE CONSTANT_PROFILES  (IIMP, PARAM, COREIMPUR, DIAG)

    USE ETS_PLASMA
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE MANIPULATOR_TYPE

    IMPLICIT NONE
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR(:)
    TYPE (MANIPULATOR_PARAM)         :: PARAM               
    TYPE (DIAGNOSTIC)                :: DIAG

    INTEGER                          :: IIMP, IZIMP

    DO IZIMP = 1, SIZE(COREIMPUR(1)%IMPURITY(IIMP)%nz, DIM=2)
       COREIMPUR(1)%IMPURITY(IIMP)%nz(:,IZIMP) = PARAM%DENS(IIMP,IZIMP)

       COREIMPUR(1)%IMPURITY(IIMP)%boundary%type(IZIMP)       = 0 

       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(1,IZIMP)    = PARAM%DENS(IIMP,IZIMP)
       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(2,IZIMP)    = 0.0_R8
       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(3,IZIMP)    = 0.0_R8

       IF (PARAM%DENS(IIMP,IZIMP).LE.0.0_R8.OR.PARAM%DENS(IIMP,IZIMP).GT.1.E30_R8) THEN
          DIAG%ERROR_MESSAGE  = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//" your settings for density might be wrong."
          DIAG%IERR           = DIAG%IERR+1
       END IF
    END DO

    RETURN

  END SUBROUTINE CONSTANT_PROFILES
!--------------------------------------------------------
!--------------------------------------------------------



!--------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE DERIVE_PROFILES  (IIMP, PARAM, COREPROF, COREIMPUR, DIAG)

    USE ETS_PLASMA
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE MANIPULATOR_TYPE

    IMPLICIT NONE
    TYPE (TYPE_COREPROF),  POINTER   :: COREPROF(:)
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR(:)
    TYPE (MANIPULATOR_PARAM)         :: PARAM               
    TYPE (DIAGNOSTIC)                :: DIAG

    INTEGER                          :: IIMP, IZIMP, IION, INUCL

    REAL (R8),           ALLOCATABLE :: PROF(:)

    ALLOCATE (PROF(SIZE(COREPROF(1)%rho_tor)))
    PROF    = 0.0_R8

    
    IF (PARAM%PROF_SOURCE(IIMP).EQ."electrons") THEN
       PROF = COREPROF(1)%ne%value

    ELSE IF (PARAM%PROF_SOURCE(IIMP).EQ."selected_ion") THEN
       DO IION = 1,SIZE(COREPROF(1)%ni%value, DIM=2) 
          INUCL= COREPROF(1)%COMPOSITIONS%IONS(IION)%nucindex
          IF (INUCL.LE.0 .OR. INUCL.GT.SIZE(COREPROF(1)%COMPOSITIONS%NUCLEI)) GOTO 5

          IF &
             (ABS(COREPROF(1)%COMPOSITIONS%NUCLEI(INUCL)%amn   - PARAM%AMN_ION(IIMP))  .LE. 0.25_R8  .AND.  &
              ABS(COREPROF(1)%COMPOSITIONS%NUCLEI(INUCL)%zn    - PARAM%ZN_ION(IIMP) )  .LE. 0.25_R8  .AND.  &
              ABS(COREPROF(1)%COMPOSITIONS%IONS(IION)%zion     - PARAM%Z_ION(IIMP))    .LE. 0.25_R8) THEN
             PROF(:) = COREPROF(1)%ni%value(:,IION)
             EXIT
          END IF
5         CONTINUE
       END DO

    ELSE IF (PARAM%PROF_SOURCE(IIMP).EQ."ions_total") THEN
       DO IION = 1,SIZE(COREPROF(1)%ni%value, DIM=2) 
          PROF(:) = PROF(:)+COREPROF(1)%ni%value(:,IION)
       END DO
    END IF

    DO IZIMP = 1, SIZE(COREIMPUR(1)%IMPURITY(IIMP)%nz, DIM=2)
       COREIMPUR(1)%IMPURITY(IIMP)%nz(:,IZIMP)                = PROF(:)*PARAM%FRA(IIMP,IZIMP)

       COREIMPUR(1)%IMPURITY(IIMP)%boundary%type(IZIMP)       = 0 

       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(1,IZIMP)    = PROF(SIZE(PROF))*PARAM%FRA(IIMP,IZIMP)
       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(2,IZIMP)    = 0.0_R8
       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(3,IZIMP)    = 0.0_R8

       IF (PARAM%FRA(IIMP,IZIMP).LE.0.0_R8.OR.PARAM%FRA(IIMP,IZIMP).GT.1000_R8) THEN
          DIAG%ERROR_MESSAGE  = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//" your settings for fractions might be wrong."
          DIAG%IERR           = DIAG%IERR+1
       END IF
    END DO
    

    RETURN

  END SUBROUTINE DERIVE_PROFILES
!--------------------------------------------------------
!--------------------------------------------------------



!--------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE CORONAL_PROFILES  (IIMP, PARAM, COREPROF, COREIMPUR, DIAG)

    USE CORONAL
    USE ETS_PLASMA
    USE EUITM_SCHEMAS
    USE ITM_TYPES
    USE MANIPULATOR_TYPE

    IMPLICIT NONE
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)   
    TYPE (TYPE_COREIMPUR),   POINTER :: COREIMPUR(:)
    TYPE (MANIPULATOR_PARAM)         :: PARAM               
    TYPE (DIAGNOSTIC)                :: DIAG

    INTEGER                          :: IIMP, IZIMP, NZIMP, IRHO, NRHO
    REAL (R8),           ALLOCATABLE :: NE(:), TE(:), N_IMPURITY(:,:)   

    NRHO = SIZE(COREIMPUR(1)%rho_tor)
    NZIMP= INT(PARAM%ZN_IMP(IIMP))
    ALLOCATE   (NE(NRHO))
    ALLOCATE   (TE(NRHO))
    ALLOCATE   (N_IMPURITY(NRHO,NZIMP))

    NE = COREPROF(1)%ne%value
    TE = COREPROF(1)%te%value

    CALL CORONAL_DISTRIBUTION (NRHO, NE, TE, PARAM%AMN_IMP(IIMP), PARAM%ZN_IMP(IIMP), NZIMP, N_IMPURITY)
    
    DO IRHO = 1,NRHO
       DO IZIMP = 1,NZIMP
          IF (N_IMPURITY(NRHO,IZIMP).LE.1.0D-200) N_IMPURITY(NRHO,IZIMP) = 0._R8 
       ENDDO
    ENDDO
    
    DO IZIMP = 1,NZIMP
       COREIMPUR(1)%IMPURITY(IIMP)%nz(:,IZIMP)                = N_IMPURITY(:,IZIMP)*PARAM%DENS(IIMP,1) 

       COREIMPUR(1)%IMPURITY(IIMP)%boundary%type(IZIMP)       = 0 

       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(1,IZIMP)    = N_IMPURITY(NRHO,IZIMP)*PARAM%DENS(IIMP,1) 
       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(2,IZIMP)    = 0.0_R8
       COREIMPUR(1)%IMPURITY(IIMP)%boundary%value(3,IZIMP)    = 0.0_R8
    END DO
       
  
    DEALLOCATE   (NE)
    DEALLOCATE   (TE)
    DEALLOCATE   (N_IMPURITY)
      

    RETURN

  END SUBROUTINE CORONAL_PROFILES
!--------------------------------------------------------
!--------------------------------------------------------





END MODULE MANIPULATOR_TOOLS
!--------------------------------------------------------
!--------------------------------------------------------
