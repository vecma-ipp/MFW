! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE  FC2K_PARABOLIC_PROF  (COREPROF_IN,  EQUILIBRIUM_IN, &
                                    COREPROF_OUT, EQUILIBRIUM_OUT)

!-------------------------------------------------------!
!     This routine puts parabolic                       !
!     profiles of psi, q and jparallel                  !
!                                                       !
!     information received in: COREPROF_IN                 !
!                                                       !
!     information saved in:    COREPROF                 !
!                                                       !
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
    USE COPY_STRUCTURES
    USE EQUILIBRIUM_WORK

    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)         !input/output CPO with plasma profiles   
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)        !input/output CPO with plasma profiles   
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)      !input CPO with geometry quantities from previous time
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)     !input CPO with geometry quantities from previous time


    CALL PARABOLIC_PROF  (COREPROF_IN,  EQUILIBRIUM_IN, &
                          COREPROF_OUT, EQUILIBRIUM_OUT)


    RETURN

  END SUBROUTINE FC2K_PARABOLIC_PROF

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

