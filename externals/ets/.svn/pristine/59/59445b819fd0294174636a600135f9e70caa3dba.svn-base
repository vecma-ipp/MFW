! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE  FC2K_CORONAL(COREIMPUR_IN, COREPROF_IN, COREIMPUR_OUT, INTERPOL, ICORONAL) 


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE CORONAL


    IMPLICIT NONE

    INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM 
    INTEGER                          :: ICORONAL           !coronal index: "0"-OFF; "1" - replace boundary conditions by coronal; "2" - replace boundary conditions and profiles by coronal 

! +++ CPO derived types:
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_IN(:)    !input CPO with impurity 
    TYPE (TYPE_COREPROF),  POINTER   :: COREPROF_IN(:)     !input CPO with plasma profiles
    TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_OUT(:)   !output CPO with sources uploaded from the data base 

    CALL SET_CORONAL(COREIMPUR_IN, COREPROF_IN, COREIMPUR_OUT, INTERPOL, ICORONAL)


 10 RETURN


    END SUBROUTINE  FC2K_CORONAL 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   





