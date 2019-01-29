MODULE DELTA_COMBINER


CONTAINS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine combines instanteneous changes       !
  !     (DELTAS) from dufferent modules                   !
  !     and interpolates them on the COREPROF grid        !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for ETS workflow            !
  !                                                       !
  !-------------------------------------------------------!

    SUBROUTINE COMBINE_DELTAS                             &
              (COREPROF,      COREDELTA1, COREDELTA2,     &
               COREDELTA_OUT, code_parameters)

    USE ALLOCATE_DEALLOCATE

    USE ITM_CONSTANTS
    USE EUITM_ROUTINES
    USE EUITM_SCHEMAS
    USE EUITM_XML_PARSER  
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO
    USE COPY_STRUCTURES

    IMPLICIT NONE


    INTEGER, PARAMETER               :: num_delta=2


! +++ CPOs  
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  

    TYPE (TYPE_COREDELTA),   POINTER :: COREDELTA1(:)    
    TYPE (TYPE_COREDELTA),   POINTER :: COREDELTA2(:)    
    TYPE (TYPE_COREDELTA),   POINTER :: COREDELTA_OUT(:)    
    TYPE (TYPE_COREDELTA),   POINTER :: COREDELTA_ARR(:)

    TYPE (TYPE_PARAM)                :: code_parameters



! +++ Control parameters:
    REAL (R8), SAVE                  :: C_psi(num_delta)  = 0.0_R8
    REAL (R8), SAVE                  :: C_te(num_delta)   = 0.0_R8
    REAL (R8), SAVE                  :: C_ne(num_delta)   = 0.0_R8
    REAL (R8), SAVE                  :: C_ti(num_delta)   = 0.0_R8
    REAL (R8), SAVE                  :: C_ni(num_delta)   = 0.0_R8
    REAL (R8), SAVE                  :: C_tz(num_delta)   = 0.0_R8
    REAL (R8), SAVE                  :: C_nz(num_delta)   = 0.0_R8
    REAL (R8), SAVE                  :: C_vtor(num_delta) = 0.0_R8

    REAL (R8), ALLOCATABLE           :: RHO_TOR(:)



    INTEGER,              PARAMETER  :: NSLICE = 1            !number of CPO ocurancies in the work flow
    INTEGER                          :: NRHO, IRHO            !number of radial points     (input, determined from COREPROF CPO)
    INTEGER                          :: NNUCL                 !number of nuclei species
    INTEGER                          :: NION, IION            !number of ion species
    INTEGER                          :: NIMP,     IIMP        !number of impurity species
    INTEGER,             ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
    INTEGER                          :: NNEUT,    INEUT       !number of neutrals species
    INTEGER,             ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
    INTEGER,             ALLOCATABLE :: NTYPE(:)              !number of types for each neutral


    INTEGER                          :: INUM, IVAL, IARR
    INTEGER                          :: return_status





    CALL ASSIGN_COMBINER_PARAMETERS(code_parameters, return_status)
    
    IF (return_status /= 0) THEN
       WRITE(*,*) 'ERROR: Could not assign delta multipliers.'
    END IF




! +++ OUTPUT DELTA CPO:
    NRHO                  = SIZE(COREPROF(1)%rho_tor)
    ALLOCATE                    (RHO_TOR(NRHO))

    CALL GET_COMP_DIMENSIONS    (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

    CALL ALLOCATE_COREDELTA_CPO (NSLICE,    NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREDELTA_OUT)
    CALL ALLOCATE_COREDELTA_CPO (NUM_DELTA, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREDELTA_ARR)

    CALL DEALLOCATE_CPO         (COREDELTA_OUT(1)%compositions)
    CALL COPY_CPO               (COREPROF(1)%compositions, COREDELTA_OUT(1)%compositions)


    RHO_TOR                    = COREPROF(1)%rho_tor     
    COREDELTA_OUT(1)%VALUES(1)%rho_tor = RHO_TOR     

    DO INUM=1,NUM_DELTA 
       COREDELTA_ARR(INUM)%VALUES(1)%rho_tor  = COREDELTA_OUT(1)%VALUES(1)%rho_tor 
       CALL DEALLOCATE_CPO      (COREDELTA_ARR(INUM)%compositions)
       CALL COPY_CPO            (COREDELTA_OUT(1)%compositions, COREDELTA_ARR(INUM)%compositions)
    END DO


! +++ Interpolate delta profiles on the output grid:
    CALL INTERPOLATE_DELTA      (COREDELTA1(1), COREDELTA_ARR(1))
    CALL INTERPOLATE_DELTA      (COREDELTA2(1), COREDELTA_ARR(2))



! +++ Combines deltas:
    DO inum=1, num_delta
! psi
       IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%delta_psi) .AND.  C_psi(inum).NE.0.0_R8)                                  &
          COREDELTA_OUT(1)%VALUES(1)%delta_psi     =  COREDELTA_OUT(1)%VALUES(1)%delta_psi +                                  &
                                                      COREDELTA_ARR(inum)%VALUES(1)%delta_psi * C_psi(inum)
! ne
       IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%delta_ne)  .AND.  C_ne(inum).NE.0.0_R8)                                   &
          COREDELTA_OUT(1)%VALUES(1)%delta_ne      =  COREDELTA_OUT(1)%VALUES(1)%delta_ne +                                   &
                                                      COREDELTA_ARR(inum)%VALUES(1)%delta_ne  * C_ne(inum)
! te
       IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%delta_te)  .AND.  C_te(inum).NE.0.0_R8)                                   &
          COREDELTA_OUT(1)%VALUES(1)%delta_te      =  COREDELTA_OUT(1)%VALUES(1)%delta_te +                                   &
                                                      COREDELTA_ARR(inum)%VALUES(1)%delta_te  * C_te(inum)
! ni
       IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%delta_ni)  .AND.  C_ni(inum).NE.0.0_R8)                                   &
          COREDELTA_OUT(1)%VALUES(1)%delta_ni      =  COREDELTA_OUT(1)%VALUES(1)%delta_ni +                                   &
                                                      COREDELTA_ARR(inum)%VALUES(1)%delta_ni  * C_ni(inum)
! ti
       IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%delta_ti)  .AND.  C_ti(inum).NE.0.0_R8)                                   &
          COREDELTA_OUT(1)%VALUES(1)%delta_ti      =  COREDELTA_OUT(1)%VALUES(1)%delta_ti +                                   &
                                                      COREDELTA_ARR(inum)%VALUES(1)%delta_ti  * C_ti(inum)
! vtor
       IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%delta_vtor)  .AND.  C_vtor(inum).NE.0.0_R8)                               &
          COREDELTA_OUT(1)%VALUES(1)%delta_vtor    =  COREDELTA_OUT(1)%VALUES(1)%delta_vtor +                                 &
                                                      COREDELTA_ARR(inum)%VALUES(1)%delta_vtor* C_vtor(inum)
       DO IIMP = 1, NIMP
! nz
          IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%IMPURITY(IIMP)%delta_nz)  .AND.  C_nz(inum).NE.0.0_R8)                 &
               COREDELTA_OUT(1)%VALUES(1)%IMPURITY(IIMP)%delta_nz =  COREDELTA_OUT(1)%VALUES(1)%IMPURITY(IIMP)%delta_nz +     &
                                                                     COREDELTA_ARR(inum)%VALUES(1)%IMPURITY(IIMP)%delta_nz  * C_nz(inum)
! tz
          IF (ASSOCIATED(COREDELTA_ARR(inum)%VALUES(1)%IMPURITY(IIMP)%delta_tz)  .AND.  C_tz(inum).NE.0.0_R8)                 &
               COREDELTA_OUT(1)%VALUES(1)%IMPURITY(IIMP)%delta_tz =  COREDELTA_OUT(1)%VALUES(1)%IMPURITY(IIMP)%delta_tz +     &
                                                                     COREDELTA_ARR(inum)%VALUES(1)%IMPURITY(IIMP)%delta_tz  * C_tz(inum)
       END DO
    END DO






! +++ Deallocation of internal variables:
    IF(ALLOCATED(RHO_TOR)) DEALLOCATE ( RHO_TOR )
    IF(ALLOCATED(NZIMP))   DEALLOCATE ( NZIMP   )
    IF(ALLOCATED(NCOMP))   DEALLOCATE ( NCOMP   )
    IF(ALLOCATED(NTYPE))   DEALLOCATE ( NTYPE   ) 
    CALL DEALLOCATE_CPO               (COREDELTA_ARR)



! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (COREDELTA_OUT(1)%VALUES(1)%deltaid%id(1))
    ALLOCATE            (COREDELTA_OUT(1)%VALUES(1)%deltaid%description(1))
    COREDELTA_OUT(1)%VALUES(1)%deltaid%id          = 'combined'
    COREDELTA_OUT(1)%VALUES(1)%deltaid%flag        = 4
    COREDELTA_OUT(1)%VALUES(1)%deltaid%description = 'Combined coredelta'
     



! +++ COPY INDIVIDUAL INPUT DELTAS TO OUTPUT CPO:
    ALLOCATE            (COREDELTA_ARR(NUM_DELTA+1))
    CALL COPY_CPO       (COREDELTA_OUT(1), COREDELTA_ARR(1))
    CALL COPY_CPO       (COREDELTA1(1),    COREDELTA_ARR(2))
    CALL COPY_CPO       (COREDELTA2(1),    COREDELTA_ARR(3))
    CALL DEALLOCATE_CPO (COREDELTA_OUT(1)%VALUES)
    ALLOCATE            (COREDELTA_OUT(1)%VALUES(1+SIZE(COREDELTA1(1)%VALUES)+SIZE(COREDELTA2(1)%VALUES)))
    

    IVAL = 1
    DO INUM = 1, NUM_DELTA+1
       DO IARR = 1, SIZE(COREDELTA_ARR(INUM)%VALUES)
          CALL COPY_CPO (COREDELTA_ARR(INUM)%VALUES(IARR), COREDELTA_OUT(1)%VALUES(IVAL))
          IVAL = IVAL + 1
       END DO
    END DO


    CALL DEALLOCATE_CPO (COREDELTA_ARR)


    RETURN


  CONTAINS


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    SUBROUTINE ASSIGN_COMBINER_PARAMETERS(codeparameters, return_status)

  !-------------------------------------------------------!
  !     This subroutine calls the XML parser for          !
  !     the combiner parameters and assign the            !
  !     resulting values to the corresponding variables   !
  !-------------------------------------------------------!
  !     Delta:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for V&V between ETS and     !
  !                   ASTRA                               !
  !                                                       !
  !-------------------------------------------------------!
  
      USE ITM_TYPES
      USE EUITM_SCHEMAS
      USE EUITM_XML_PARSER  

      IMPLICIT NONE


      TYPE(type_param)                  :: codeparameters
      INTEGER(ITM_I4)                   :: return_status 

      TYPE(tree)                        :: parameter_list
      TYPE(element),        POINTER     :: temp_pointer
      INTEGER(ITM_I4)                   :: i, nparm, n_values
      CHARACTER(len = 132)              :: cname
      CHARACTER(len = 132)              :: code_param_name
      CHARACTER(len = 132)              :: parameter_value
      REAL (R8), SAVE                   :: VALUE(1)  = 0.0_R8

      INTEGER                           :: n_data

      return_status   = 0      

      C_psi           = 0.0_R8
      C_te            = 0.0_R8
      C_ne            = 0.0_R8
      C_ti            = 0.0_R8
      C_ni            = 0.0_R8
      C_tz            = 0.0_R8
      C_nz            = 0.0_R8
      C_vtor          = 0.0_R8

!-- parse xml-string codeparameters%parameters
      CALL EUITM_XML_PARSE  (codeparameters, nparm, parameter_list)

      temp_pointer => parameter_list%first
      code_param_name = 'parameters/PSI/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_psi(1) = VALUE(1)
      END IF
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/PSI/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_psi(2) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/NE/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_ne(1) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/NE/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_ne(2) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/TE/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_te(1) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/TE/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_te(2) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/NI/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_ni(1) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/NI/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_ni(2) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/TI/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_ti(1) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/TI/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_ti(2) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/VTOR/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_vtor(1) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/VTOR/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_vtor(2) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/NZ/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_nz(1) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/NZ/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_nz(2) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/PTZ/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_pellet'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_TZ(1) = VALUE(1)
      END IF 
      temp_pointer => parameter_list%first
      code_param_name = 'parameters/TZ/COMBINE_CONTRIBUTIONS_WITH_WEIGHTS/from_sawtooth'
      CALL find_parameter(code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         CALL scan_str2real(parameter_value, VALUE, n_data)
         C_tz(2) = VALUE(1)
      END IF 


       
      
!-- destroy tree
      CALL DESTROY_XML_TREE(parameter_list)


      RETURN

    END SUBROUTINE ASSIGN_COMBINER_PARAMETERS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


  END SUBROUTINE COMBINE_DELTAS



END MODULE DELTA_COMBINER
