! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE FC2k_READJUST_PROFILES            &
       (PROF_FLAG, Q0_FLAG, COREPROF_IN, EQUILIBRIUM_IN, COREPROF_OUT) 

!-------------------------------------------------------!
!     This routine readjusts consistently               !
!     profiles of psi, q and jparallel                  !
!                                                       !
!     information received in: COREPROF                 !
!                              EQUILIBRIUM.             !
!                                                       !
!     information saved in:    COREPROF                 !
!                                                       !
!     controling parameter:    PROF_FLAG                !
!                                                       !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     created for V&V between ETS, JETTO  !
!                   and ASTRA                           !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE ITM_CONSTANTS
    USE EQUILIBRIUM_WORK

    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !input/output CPO with plasma profiles   


! +++ Dimensions:
    INTEGER                           :: PROF_FLAG           !determines primary profile
    INTEGER                           :: Q0_FLAG             !Flag for positive dq/drho: 0-allowed, >0-cut off



    CALL READJUST_PROFILES            &
       (PROF_FLAG, Q0_FLAG, COREPROF_IN, EQUILIBRIUM_IN, COREPROF_OUT)



    RETURN

  END SUBROUTINE FC2K_READJUST_PROFILES

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
