! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


  !-------------------------------------------------------!
  !     This routine combines transport coeffients        !
  !     from different modules and interpolates them      !
  !     on the COREPROF grid                              !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for V&V between ETS and     !
  !                   ASTRA                               !
  !                                                       !
  !-------------------------------------------------------!

  SUBROUTINE  FC2K_TRANSPORT_COMBINER                &
              (COREPROF,       CORETRANSP,           &
               CORETRANSP1,    CORETRANSP2,          &
               CORETRANSP3,    CORETRANSP4,          &
               CORETRANSP5,    CORETRANSP_OUT,       &
               AMIX_TR,        code_parameters)

    
    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS
    USE TRANSPORT_COMBINER

    IMPLICIT NONE


! +++ CPOs:
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP(:)    

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP1(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP2(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP3(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP4(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP5(:)    

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_OUT(:)    

    TYPE (TYPE_PARAM)                :: code_parameters

    REAL (R8)                        :: AMIX_TR      




    CALL COMBINE_TRANSPORT                            &
                   (COREPROF,       CORETRANSP,       &
                   CORETRANSP1,     CORETRANSP2,      &
                   CORETRANSP3,     CORETRANSP4,      &
                   CORETRANSP5,     CORETRANSP_OUT,   &
                   AMIX_TR,         code_parameters)


    RETURN

  END SUBROUTINE FC2K_TRANSPORT_COMBINER
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


