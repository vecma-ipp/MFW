! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE FC2K_EMEQ  (EQUILIBRIUM_IN, EQUILIBRIUM_OUT, code_parameters)

    !-------------------------------------------------------!
    !     This routine provides interface between ETS       !
    !     and three moment equilibrium solver.              !
    !-------------------------------------------------------!
    !     Source:       ---                                 !
    !     Developers:   D.Kalupin, G.Pereverzev             !
    !     Kontacts:     D.Kalupin@fz-juelich.de             !
    !                                                       !
    !     Comments:     created for V&V between ETS and     !
    !                   ASTRA                               !
    !                                                       !
    !-------------------------------------------------------!


    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS
    USE EMEQ


    !-------------------------------------------------------!
    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)  !output CPO with geometry quantities from previous iteration
    TYPE (type_param)                 :: code_parameters




    CALL EMEQ_E3M (EQUILIBRIUM_IN, EQUILIBRIUM_OUT, code_parameters)


    RETURN



  END SUBROUTINE FC2K_EMEQ
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  








 
