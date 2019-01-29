!-------------------------------------------------------!
!-------------------------------------------------------!

  SUBROUTINE FC2K_GAUSIAN_SOURCES    (COREPROF, EQUILIBRIUM, CORESOURCE, code_parameters) 

!-------------------------------------------------------!
!     This routine provides dummy source for the        !
!     ETS workflow.                                     !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     input parameter list is specified   !
!                   in "source_dummy.xml" file.         !
!                                                       !
!                   output CORESOURCE CPO is            !
!                   allocated inside the module         !
!                                                       !
!-------------------------------------------------------!


      USE  ITM_TYPES
      USE  EUITM_SCHEMAS
      USE  GAUSIAN_SRC


      IMPLICIT NONE


      INTEGER                           :: ifail
 

! +++ CPO derived types:
      TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)      !input CPO with geometry quantities from previous time
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF(:)         !input CPO with internal ETS parameters profiles from previous time
      TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE(:)       !output CPO with sources
      TYPE (TYPE_PARAM)                 :: code_parameters


      CALL GAUSIAN_SOURCES    (COREPROF, EQUILIBRIUM, CORESOURCE, code_parameters)
      

      RETURN


      END SUBROUTINE FC2K_GAUSIAN_SOURCES  

!-------------------------------------------------------!
!-------------------------------------------------------!



