
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine combines sources                     !
  !     from dufferent modules and interpolates them      !
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

    SUBROUTINE FC2K_SOURCE_COMBINER                              &
              (COREPROF,       CORESOURCE,                       &
               CORESOURCE1,    CORESOURCE2,    CORESOURCE3,      &
               CORESOURCE4,    CORESOURCE5,    CORESOURCE6,      &
               CORESOURCE7,                                      &
               CORESOURCE_OUT, AMIX_SRC,       code_parameters)

    USE ALLOCATE_DEALLOCATE

    USE ITM_CONSTANTS
    USE EUITM_ROUTINES
    USE EUITM_SCHEMAS
    USE EUITM_XML_PARSER  
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO
    USE SOURCE_COMBINER

    IMPLICIT NONE



! +++ CPOs  
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE(:)    

    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE1(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE2(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE3(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE4(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE5(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE6(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE7(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE_OUT(:)    
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE_ARR(:)
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE_MIX(:)    

    TYPE (TYPE_PARAM)                :: code_parameters

    REAL (R8)                        :: AMIX_SRC      



    CALL COMBINE_SOURCES                                         &
              (COREPROF,       CORESOURCE,                       &
               CORESOURCE1,    CORESOURCE2,    CORESOURCE3,      &
               CORESOURCE4,    CORESOURCE5,    CORESOURCE6,      &
               CORESOURCE7,                                      &
               CORESOURCE_OUT, AMIX_SRC,       code_parameters)


  END SUBROUTINE FC2K_SOURCE_COMBINER
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

