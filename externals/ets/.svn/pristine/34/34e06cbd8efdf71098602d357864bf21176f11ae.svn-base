MODULE MANIPULATOR_TYPE
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
  USE ITM_TYPES
  USE ETS_PLASMA

  IMPLICIT NONE

!--------------------------------------------------------
!--------------------------------------------------------
  TYPE MANIPULATOR_PARAM

     INTEGER                 :: NIMP              ! number of impurity elements requested to MANIPULATOR 
     INTEGER                 :: MAX_Z_IMP         ! maximum ionization state over all impurity 

! +++ Arrays:      
     REAL (R8),      POINTER :: AMN_IMP(:),     & ! atomic mass number of IMPURITY components [NIMP]
                                ZN_IMP(:),      & ! nuclear charge of IMPURITY components [NIMP]
                                Z_IMP(:,:),     & ! charge of IMPURITY individual ionization states [NIMP,ZN_IMP]
!                                                 ! ASSUMPTION: MAX_Z_IMP>=ZN_IMP
                                AMN_ION(:),     & ! atomic mass number of reference ION components [NIMP]
                                ZN_ION(:),      & ! nuclear charge of reference ION components [NIMP]
                                Z_ION(:)          ! charge of reference ION components [NIMP]
!
     CHARACTER(132), POINTER :: ISTATE(:),      & ! number of ionization states to be treated [NIMP]
                                PROFILE(:),     & ! number of ionization states to be treated [NIMP]
                                PROF_SOURCE(:)    ! number of ionization states to be treated [NIMP]

     REAL (R8),      POINTER :: FRA(:,:),       & ! fractions of IMPURITY densities, defined with respect to the reference profile [NIMP,ZN_IMP]
                                DENS(:,:)         ! individual densities of IMPURITY components [NIMP,ZN_IMP]

  END TYPE MANIPULATOR_PARAM
!--------------------------------------------------------
!--------------------------------------------------------



CONTAINS
!--------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE ALLOCATE_MANIPULATOR_PARAM (NIMP, MAX_Z_IMP, PARAM, DIAG)

    INTEGER, INTENT (IN)     :: Nimp
    INTEGER, INTENT (IN)     :: MAX_Z_IMP
    INTEGER                  :: ISTAT

    TYPE (MANIPULATOR_PARAM) :: PARAM
    TYPE (DIAGNOSTIC)        :: DIAG              


    ALLOCATE   (PARAM%AMN_IMP(NIMP),         &  
                PARAM%ZN_IMP(NIMP),          & 
                PARAM%Z_IMP(NIMP,MAX_Z_IMP), & 
                PARAM%AMN_ION(NIMP),         & 
                PARAM%ZN_ION(NIMP),          & 
                PARAM%Z_ION(NIMP),           & 
                PARAM%ISTATE(NIMP),          & 
                PARAM%PROFILE(NIMP),         & 
                PARAM%PROF_SOURCE(NIMP),     & 
                PARAM%FRA(NIMP,MAX_Z_IMP),   & 
                PARAM%DENS(NIMP,MAX_Z_IMP),  & 
                STAT=ISTAT)

! +++ Error checking and reporting:
    IF (ISTAT /= 0) THEN
       DIAG%ERROR_MESSAGE  = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//" ALLOCATE_MANIPULATOR_PARAM: Failure to allocate."
       DIAG%IERR           = -1
       RETURN
    END IF


    PARAM%NIMP         = NIMP
    PARAM%AMN_IMP      = 0.0_R8
    PARAM%ZN_IMP       = 0.0_R8          
    PARAM%Z_IMP        = 0.0_R8
    PARAM%AMN_ION      = 0.0_R8         
    PARAM%ZN_ION       = 0.0_R8       
    PARAM%Z_ION        = 0.0_R8         
    PARAM%FRA          = 0.0_R8
    PARAM%DENS         = 0.0_R8
    PARAM%ISTATE       = ""         
    PARAM%PROFILE      = ""       
    PARAM%PROF_SOURCE  = ""

    RETURN

  END SUBROUTINE ALLOCATE_MANIPULATOR_PARAM
!--------------------------------------------------------
!--------------------------------------------------------



!--------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE DEALLOCATE_MANIPULATOR_PARAM (PARAM)

    TYPE (MANIPULATOR_PARAM)      :: PARAM


    IF(ASSOCIATED(PARAM%AMN_IMP))       DEALLOCATE (PARAM%AMN_IMP)  
    IF(ASSOCIATED(PARAM%ZN_IMP))        DEALLOCATE (PARAM%ZN_IMP)  
    IF(ASSOCIATED(PARAM%Z_IMP))         DEALLOCATE (PARAM%Z_IMP)  
    IF(ASSOCIATED(PARAM%AMN_ION))       DEALLOCATE (PARAM%AMN_ION)  
    IF(ASSOCIATED(PARAM%ZN_ION))        DEALLOCATE (PARAM%ZN_ION)  
    IF(ASSOCIATED(PARAM%Z_ION))         DEALLOCATE (PARAM%Z_ION)  
    IF(ASSOCIATED(PARAM%FRA))           DEALLOCATE (PARAM%FRA)  
    IF(ASSOCIATED(PARAM%DENS))          DEALLOCATE (PARAM%DENS)  
    IF(ASSOCIATED(PARAM%ISTATE))        DEALLOCATE (PARAM%ISTATE)
    IF(ASSOCIATED(PARAM%PROFILE))       DEALLOCATE (PARAM%PROFILE)
    IF(ASSOCIATED(PARAM%PROF_SOURCE))   DEALLOCATE (PARAM%PROF_SOURCE)

    RETURN

  END SUBROUTINE DEALLOCATE_MANIPULATOR_PARAM
!--------------------------------------------------------
!--------------------------------------------------------




END MODULE MANIPULATOR_TYPE
!--------------------------------------------------------
!--------------------------------------------------------
