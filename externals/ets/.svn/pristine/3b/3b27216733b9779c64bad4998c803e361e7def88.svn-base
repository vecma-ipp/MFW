!-------------------------------------------------------!
!-------------------------------------------------------!

  SUBROUTINE FC2K_SPITZER_RESISTIVITY    (COREPROF_IN, CORETRANSP_OUT) 

!-------------------------------------------------------!
!     This routine is the interface used by the         !
!     ETS workflow to the routine calculating           !
!     Spitzer resistivity                               !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     output CORETRANSP CPO is            !
!                   allocated inside the module         !
!                                                       !
!-------------------------------------------------------!

      USE ITM_TYPES
      USE EUITM_SCHEMAS
      USE SPITZER
      

      IMPLICIT NONE

! +++ CPO types:
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)        !input CPO with profiles
      TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP_OUT(:)     !output CPO with transport (SIGMA)

      CALL SPITZER_RESISTIVITY    (COREPROF_IN, CORETRANSP_OUT) 

      RETURN

      END SUBROUTINE FC2K_SPITZER_RESISTIVITY    
!-------------------------------------------------------!
!-------------------------------------------------------!
