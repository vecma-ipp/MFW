! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE SCALEPROF (S_psi, S_jpar, S_ne, S_te, S_ni, S_ti, S_vtor, &
                         COREPROF_IN,    COREPROF_OUT) 
  !-------------------------------------------------------!
  !     This routine rescales profiles from               !
  !     the COREPROF CPO                                  !
  !                                                       !
  !     multipliers are expected from the workflow as     !
  !     parameters                                        !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for V&V between ETS and     !
  !                   ASTRA                               !
  !                                                       !
  !-------------------------------------------------------!

! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES
    USE ITM_CONSTANTS

    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_IN(:)        !input CPO  
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_OUT(:)       !output CPO  
 
    REAL (R8)                        ::  S_psi  
    REAL (R8)                        ::  S_jpar 
    REAL (R8)                        ::  S_ne   
    REAL (R8)                        ::  S_te   
    REAL (R8)                        ::  S_ni   
    REAL (R8)                        ::  S_ti   
    REAL (R8)                        ::  S_vtor 



! +++ Allocate output CPOs:
    ALLOCATE(COREPROF_OUT(1))


! +++ Copy input CPOs to output CPOs:
    CALL COPY_CPO     (COREPROF_IN(1),    COREPROF_OUT(1))


! +++ Rescale profiles in output CPOs:
    COREPROF_OUT(1)%psi%value              = COREPROF_IN(1)%psi%value * S_psi
    COREPROF_OUT(1)%profiles1d%jtot%value  = COREPROF_IN(1)%profiles1d%jtot%value * S_jpar

    COREPROF_OUT(1)%ne%value               = COREPROF_IN(1)%ne%value * S_ne
    COREPROF_OUT(1)%te%value               = COREPROF_IN(1)%te%value * S_te

    COREPROF_OUT(1)%ni%value               = COREPROF_IN(1)%ni%value * S_ni
    COREPROF_OUT(1)%ti%value               = COREPROF_IN(1)%ti%value * S_ti

    COREPROF_OUT(1)%vtor%value             = COREPROF_IN(1)%vtor%value * S_vtor


 
    RETURN 


    END SUBROUTINE SCALEPROF
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



