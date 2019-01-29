!----------------------------------------------------------
!----------------------------------------------------------
   SUBROUTINE  IMPURITY_MANIPULATOR(COREPROF, COREIMPUR, code_parameters) 
!----------------------------------------------------------
  !-------------------------------------------------------!
  !     This routine generates analythical profiles       !
  !     for impurity ion densities                        !
  !     (No atomic processes are included!!!)             !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Contacts:     Denis.Kalupin@euro-fusion.org       !
  !                                                       !
  !                                                       !
  !     Comments:     created for the ETS                 !
  !                                                       !
  !-------------------------------------------------------!
!----------------------------------------------------------


! +++ Declaration of variables: 
     USE CORONAL
     USE ETS_PLASMA
     USE EUITM_SCHEMAS
     USE ITM_TYPES
     USE MANIPULATOR_TYPE
     USE MANIPULATOR_TOOLS


     IMPLICIT NONE

! +++ CPO derived types:
     TYPE (TYPE_COREPROF),  POINTER   :: COREPROF(:)         !input CPO with plasma profiles
     TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR(:)        !output CPO with sources uploaded from the data base 

! +++ configuration parameters:
     TYPE (type_param)                :: code_parameters
     TYPE (MANIPULATOR_PARAM)         :: PARAM               

! +++ warnings & errors:
     TYPE (DIAGNOSTIC)                :: DIAG                !diagnostic output 

     INTEGER                          :: IIMP

     DIAG%ERROR_MESSAGE               = ""
     DIAG%IERR                        = 0


!----------------------------------------------------------
!Assign manipulator data:
    CALL ASSIGN_CODE_PARAMETERS       (code_parameters, PARAM, DIAG)   
    IF (DIAG%IERR .LT. 0) GOTO 112

!Set output CPO and Compositions:
    CALL OUTPUT_COMPOSITIONS          (PARAM, COREPROF, COREIMPUR, DIAG)
    IF (DIAG%IERR .LT. 0) GOTO 112

!Set output Profiles:
    DO IIMP = 1, PARAM%NIMP
       IF(TRIM(ADJUSTL(PARAM%PROFILE(IIMP))).EQ."constant") THEN
          CALL CONSTANT_PROFILES  (IIMP, PARAM, COREIMPUR, DIAG)
          IF (DIAG%IERR .LT. 0) GOTO 112
       END IF
       
       IF(TRIM(ADJUSTL(PARAM%PROFILE(IIMP))).EQ."derived") THEN
          CALL DERIVE_PROFILES  (IIMP, PARAM, COREPROF, COREIMPUR, DIAG)
          IF (DIAG%IERR .LT. 0) GOTO 112
       END IF

       IF(TRIM(ADJUSTL(PARAM%PROFILE(IIMP))).EQ."coronal") THEN
          CALL CORONAL_PROFILES  (IIMP, PARAM, COREPROF, COREIMPUR, DIAG)
          IF (DIAG%IERR .LT. 0) GOTO 112
       END IF
    END DO


112 ALLOCATE     (COREIMPUR(1)%codeparam%codename(1))
    ALLOCATE     (COREIMPUR(1)%codeparam%codeversion(1))
    ALLOCATE     (COREIMPUR(1)%codeparam%output_diag(1))

    COREIMPUR(1)%codeparam%codename            = 'IMPURITY MANIPULATOR'
    COREIMPUR(1)%codeparam%codeversion         = 'IMPURITY MANIPULATOR_4.10b.10'
    COREIMPUR(1)%codeparam%output_flag         =  DIAG%IERR
    COREIMPUR(1)%codeparam%output_diag(1)      = "IMPURITY MANIPULATOR: "//TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))


    RETURN

  CONTAINS
!----------------------------------------------------------
!----------------------------------------------------------
    SUBROUTINE ASSIGN_CODE_PARAMETERS(code_parameters, PARAM, DIAG)

      USE EUITM_XML_PARSER  
 
      IMPLICIT NONE

      TYPE (MANIPULATOR_PARAM)       :: PARAM               
      TYPE (DIAGNOSTIC)              :: DIAG                

      TYPE(type_param)               :: code_parameters
 
      TYPE(tree)                     :: parameter_list
      TYPE(element),         POINTER :: temp_pointer
      INTEGER(ITM_I4)                :: nparam
      CHARACTER(len = 132)           :: parameter_value
      CHARACTER(len = 264)           :: code_param_name
      CHARACTER(len = 6)             :: cvalue(10)
      REAL (R8)                      :: value(10)
      INTEGER                        :: nval

      INTEGER                        :: NIMP
      INTEGER                        :: MAX_Z_IMP
      LOGICAL                        :: IMP_ELEMENT(3) = .false.

      INTEGER                        :: I, IIMP, IZ
      INTEGER                        :: length
      CHARACTER(len = 132)           :: PREFIX

      DIAG%IERR            = 0                   
      DIAG%ERROR_MESSAGE   = " "                 

!-- parse xml-string codeparameters%parameters
      CALL EUITM_XML_PARSE  (code_parameters, nparam, parameter_list)

!-- check the number of impurities and their highest ionization state:  
      NIMP                 = 0
      MAX_Z_IMP            = 0

      temp_pointer => parameter_list%first
      code_param_name       = 'parameters/IMPURITY1/Define_impurity/zn'
      CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         NIMP       = NIMP + 1
         IMP_ELEMENT(1) = .true.
         CALL scan_str2real    (parameter_value, value ,nval)
         MAX_Z_IMP  = MAX(MAX_Z_IMP, INT(value(1)))
      END IF

      temp_pointer => parameter_list%first
      code_param_name       = 'parameters/IMPURITY2/Define_impurity/zn'
      CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         NIMP       = NIMP + 1
         IMP_ELEMENT(2) = .true.
         CALL scan_str2real    (parameter_value, value ,nval)
         MAX_Z_IMP  = MAX(MAX_Z_IMP, INT(value(1)))
      END IF

      temp_pointer => parameter_list%first
      code_param_name       = 'parameters/IMPURITY3/Define_impurity/zn'
      CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
      IF (LEN(TRIM(parameter_value)).GE.1) THEN
         NIMP       = NIMP + 1
         IMP_ELEMENT(3) = .true.
         CALL scan_str2real    (parameter_value, value ,nval)
         MAX_Z_IMP  = MAX(MAX_Z_IMP, INT(value(1)))
      END IF

      IF (NIMP.GE.0) THEN
         CALL ALLOCATE_MANIPULATOR_PARAM (NIMP, MAX_Z_IMP, PARAM, DIAG)
      ELSE
         DIAG%ERROR_MESSAGE  = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//" ASSIGN_PARAM: no impurities are defined."
         DIAG%IERR           = DIAG%IERR+1
         RETURN
      END IF


!-- check configuration for each impurity:
      IIMP = 0
      DO I = 1, 3
         IF (IMP_ELEMENT(I).EQ. .true.) THEN
            IIMP = IIMP + 1
            WRITE (PREFIX, "(a19,i1,a17)") "parameters/IMPURITY",I,"/Define_impurity/"
!           composition:
            code_param_name   =  TRIM(ADJUSTL(PREFIX))//"amn"
            CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               CALL scan_str2real    (parameter_value, value ,nval) 
               PARAM%AMN_IMP(IIMP) = value(1)
            END IF

            code_param_name   =  TRIM(ADJUSTL(PREFIX))//"zn"
            CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               CALL scan_str2real    (parameter_value, value ,nval) 
               PARAM%ZN_IMP(IIMP)  = value(1)
            END IF



            
            code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/all/Profiles/all_radially_constant/Densities"
            CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               PARAM%PROFILE(IIMP) = "constant"
               PARAM%ISTATE(IIMP)  = "all"
               CALL scan_str2real    (parameter_value, value ,nval) 
               PARAM%DENS(IIMP,:)      = value(:)
               IF (nval.LT.PARAM%ZN_IMP(IIMP)) THEN
                  DIAG%ERROR_MESSAGE  = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//" you did not specify densities for all ionization states."
                  DIAG%IERR           = DIAG%IERR+1
               END IF
            END IF

            code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/all/Profiles/coronal_distribution/Total_Density"
            CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               PARAM%PROFILE(IIMP) = "coronal"
               PARAM%ISTATE(IIMP)  = "all"
               CALL scan_str2real    (parameter_value, value ,nval) 
               PARAM%DENS(IIMP,1)      = value(1)
            END IF


            code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/all/Profiles/all_proportional_to_source_profile/Fractions"
            CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               PARAM%PROFILE(IIMP) = "derived"
               PARAM%ISTATE(IIMP)  = "all"
               CALL scan_str2real    (parameter_value, value ,nval) 
               PARAM%FRA(IIMP,:)       = value(:)
               IF (nval.LT.PARAM%ZN_IMP(IIMP)) THEN
                  DIAG%ERROR_MESSAGE  = TRIM(ADJUSTL(DIAG%ERROR_MESSAGE))//" you did not specify fractions for all ionization states."
                  DIAG%IERR           = DIAG%IERR+1
               END IF

               code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/all/Profiles/all_proportional_to_source_profile/Source_profile/ions/extrapolate_from/selected_ion/amn"
               CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
               IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                  PARAM%PROF_SOURCE(IIMP) = "selected_ion"
                  CALL scan_str2real      (parameter_value, value ,nval) 
                  PARAM%AMN_ION(IIMP)     = value(1)
                  code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/all/Profiles/all_proportional_to_source_profile/Source_profile/ions/extrapolate_from/selected_ion/zn"
                  CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
                  IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                     CALL scan_str2real      (parameter_value, value ,nval) 
                     PARAM%ZN_ION(IIMP)   = value(1)
                  END IF
                  code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/all/Profiles/all_proportional_to_source_profile/Source_profile/ions/extrapolate_from/selected_ion/zion"
                  CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
                  IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                     CALL scan_str2real      (parameter_value, value ,nval) 
                     PARAM%Z_ION(IIMP)   = value(1)
                  END IF
               ELSE
                  PARAM%PROF_SOURCE(IIMP) = "ions_total"
               END IF

               code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/all/Profiles/all_proportional_to_source_profile/Source_profile/electrons/NOTE"
               CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
               IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                  PARAM%PROF_SOURCE(IIMP) = "electrons"
               END IF
            END IF



            code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/fully_stripped_state_only/Profile/radially_constant/Density"
            CALL find_parameter   (code_param_name, parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               PARAM%PROFILE(IIMP)   = "constant"
               PARAM%ISTATE(IIMP)    = "fully_stipped"
               CALL scan_str2real    (parameter_value, value ,nval) 
               PARAM%DENS(IIMP,1)    = value(1)
            END IF
            
            code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/fully_stripped_state_only/Profile/proportional_to_source_profile/Fraction"
            CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               PARAM%PROFILE(IIMP)   = "derived"
               PARAM%ISTATE(IIMP)    = "fully_stipped"
               CALL scan_str2real    (parameter_value, value ,nval) 
               PARAM%FRA(IIMP,1)     = value(1)
               
               code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/fully_stripped_state_only/Profile/proportional_to_source_profile/Source_profile/ions/extrapolate_from/selected_ion/amn"
               CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
               IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                  PARAM%PROF_SOURCE(IIMP) = "selected_ion"
                  CALL scan_str2real      (parameter_value, value ,nval) 
                  PARAM%AMN_ION(IIMP)     = value(1)
                  code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/fully_stripped_state_only/Profile/proportional_to_source_profile/Source_profile/ions/extrapolate_from/selected_ion/zn"
                  CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
                  IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                     CALL scan_str2real      (parameter_value, value ,nval) 
                     PARAM%ZN_ION(IIMP)   = value(1)
                  END IF
                  code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/fully_stripped_state_only/Profile/proportional_to_source_profile/Source_profile/ions/extrapolate_from/selected_ion/zion"
                  CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
                  IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                     CALL scan_str2real      (parameter_value, value ,nval) 
                     PARAM%Z_ION(IIMP)   = value(1)
                  END IF
               ELSE
                  PARAM%PROF_SOURCE(IIMP) = "ions_total"
               END IF
               
               code_param_name   =  TRIM(ADJUSTL(PREFIX))//"ionization_states/fully_stripped_state_only/Profile/proportional_to_source_profile/Source_profile/electrons/NOTE"
               CALL find_parameter_test   (code_param_name, parameter_value, temp_pointer)
               IF (LEN(TRIM(parameter_value)).GE.1) THEN                  
                  PARAM%PROF_SOURCE(IIMP) = "electrons"
               END IF

            END IF

            IF (TRIM(ADJUSTL(PARAM%ISTATE(IIMP))).EQ."all") THEN
               DO IZ=1,INT(PARAM%ZN_IMP(IIMP))
                  PARAM%Z_IMP(IIMP,IZ)  = IZ
               END DO
            ELSE IF (TRIM(ADJUSTL(PARAM%ISTATE(IIMP))).EQ."fully_stipped") THEN
               PARAM%Z_IMP(IIMP,1)      = INT(PARAM%ZN_IMP(IIMP))
            END IF


          END IF
      END DO
     

     RETURN

   END SUBROUTINE ASSIGN_CODE_PARAMETERS
!----------------------------------------------------------
!----------------------------------------------------------

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
!TEST ROUTINES FROM MICHAL: TO BE REMOVED AFTER
! INTEGRATING THEM INTO XMLLIB !!!!
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
SUBROUTINE find_parameter_test(mko_str, mko_value, mko_parameters_ptr)
  use string_manipulation_tools
  CHARACTER(len=264), INTENT(in)     :: mko_str
  CHARACTER(len=132), INTENT(out)    :: mko_value
  TYPE(element), POINTER             :: mko_temp_pointer
  TYPE(element), POINTER             :: mko_found_element
  TYPE(element), POINTER, INTENT(in) :: mko_parameters_ptr
  INTEGER :: mko_pos1 = 1, mko_pos2=10000, mko_n = 0, mko_i
  ! there are few strong assumptions
  ! 1. we assume that no parameter will be longer than 132
  ! 2. we assume that the deepth of the tree will be not bigger than 10 levels down
  CHARACTER(len=264)   :: mko_word(30)

  mko_value = ''
  mko_word  = ''
  mko_n     = 0
  mko_pos1 = 1
  mko_pos2 = 10000

  mko_temp_pointer => mko_parameters_ptr

  DO
     mko_pos2 = INDEX(mko_str(mko_pos1:), "/")
     IF (mko_pos2 == 0) THEN
        mko_n = mko_n + 1
	mko_word(mko_n) = mko_str(mko_pos1:)
        EXIT
     END IF
     mko_n = mko_n + 1
     mko_word(mko_n) = mko_str(mko_pos1:mko_pos1+mko_pos2-2)
     mko_pos1 = mko_pos2+mko_pos1
  END DO

! we have the whole tree here, now we have to traverse the elements

  DO mko_i = 1, mko_n
     ! at each level, we have to check whether we have correct name
     CALL find_element_test(mko_temp_pointer, mko_found_element, mko_word(mko_i) )
     IF(ASSOCIATED(mko_found_element) .EQV. .FALSE.) THEN
        mko_value = ''
       RETURN
    ELSE
       IF ( mko_i == mko_n) THEN
          mko_value = char2str(mko_found_element%cvalue)
          RETURN
       ELSE
          mko_temp_pointer => mko_found_element%child
       END IF
     END IF
  END DO
  mko_value = ''
END SUBROUTINE find_parameter_test
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
SUBROUTINE find_element_test(mko_ptr_to_element, mko_return_ptr, mko_cname)
  use string_manipulation_tools
  CHARACTER(len=264),     INTENT(in)  :: mko_cname
  CHARACTER(len=264)                  :: mko_tmp_cname
  TYPE(element), POINTER, INTENT(in)  :: mko_ptr_to_element
  TYPE(element), POINTER, INTENT(out) :: mko_return_ptr
  TYPE(element), POINTER              :: mko_tmp_ptr

  mko_tmp_ptr => mko_ptr_to_element

  DO WHILE( ASSOCIATED(mko_tmp_ptr) )
     mko_tmp_cname = char2str(mko_tmp_ptr%cname)
     IF ( mko_cname .EQ. mko_tmp_cname) THEN
        mko_return_ptr => mko_tmp_ptr
        RETURN
     END IF
     mko_tmp_ptr => mko_tmp_ptr%sibling
   END DO
   mko_return_ptr => mko_tmp_ptr
 END SUBROUTINE find_element_test
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +


   END SUBROUTINE IMPURITY_MANIPULATOR
!----------------------------------------------------------
!----------------------------------------------------------





