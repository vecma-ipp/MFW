! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE FC2K_CORRECT_CURRENT_PROF  (J0_FLAG, COREPROF_IN, EQUILIBRIUM, COREPROF_OUT) 
!-------------------------------------------------------!
!     This routine corrects profiles                    !
!     of psi, q and jparallel such, that                !
!     there is no negative current.                     !
!                                                       !
!     information received in: COREPROF_IN              !
!                                                       !
!     information saved in:    COREPROF_OUT             !
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
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !output/output CPO with plasma profiles   
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)      !input CPO with geometry quantities from previous time



! +++ Dimensions:
    INTEGER                           :: J0_FLAG             !flag for negative current, if J0_FLAG>0 negative current will be cut off


    CALL CORRECT_CURRENT_PROF  (J0_FLAG, COREPROF_IN, EQUILIBRIUM, COREPROF_OUT) 


    RETURN


   END SUBROUTINE FC2K_CORRECT_CURRENT_PROF
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE FC2K_NEGATIVE_CURRENT  (J0_FLAG, COREPROF_IN, EQUILIBRIUM, COREPROF_OUT) 
!-------------------------------------------------------!
!     This routine corrects profiles                    !
!     of psi, q and jparallel such, that                !
!     there is no negative current.                     !
!                                                       !
!     information received in: COREPROF_IN              !
!                                                       !
!     information saved in:    COREPROF_OUT             !
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
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !output/output CPO with plasma profiles   
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)      !input CPO with geometry quantities from previous time



! +++ Dimensions:
    INTEGER                           :: J0_FLAG             !flag for negative current, if J0_FLAG>0 negative current will be cut off


    CALL NEGATIVE_CURRENT  (J0_FLAG, COREPROF_IN, EQUILIBRIUM, COREPROF_OUT)


    RETURN


   END SUBROUTINE FC2K_NEGATIVE_CURRENT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   





