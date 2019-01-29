
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine combines deltas                      !
  !     from dufferent modules and interpolates them      !
  !     on the COREPROF grid                              !
  !-------------------------------------------------------!
  !     Delta:       ---                                  !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for  ETS workflow           !
  !                                                       !
  !-------------------------------------------------------!

    SUBROUTINE FC2K_DELTA_COMBINER                        &
              (COREPROF,                                  &
               COREDELTA1, COREDELTA2, COREDELTA_OUT,     &
               code_parameters)

      USE EUITM_SCHEMAS
      USE DELTA_COMBINER

      
      IMPLICIT NONE



! +++ CPOs  
      TYPE (TYPE_COREPROF),   POINTER :: COREPROF(:)  
      TYPE (TYPE_COREDELTA),  POINTER :: COREDELTA1(:)    
      TYPE (TYPE_COREDELTA),  POINTER :: COREDELTA2(:)    
      TYPE (TYPE_COREDELTA),  POINTER :: COREDELTA_OUT(:)    

      TYPE (TYPE_PARAM)               :: code_parameters



      CALL COMBINE_DELTAS                                 &
              (COREPROF,                                  &
               COREDELTA1, COREDELTA2, COREDELTA_OUT,     &
               code_parameters)

      RETURN


    END SUBROUTINE FC2K_DELTA_COMBINER
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

