MODULE SOURCE_COMBINER


CONTAINS
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

    SUBROUTINE COMBINE_SOURCES                                   &
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
    USE COPY_STRUCTURES

    IMPLICIT NONE


    INTEGER, PARAMETER               :: num_source=7


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



! +++ Control parameters:
    REAL (R8)                        :: AMIX_SRC      

    REAL (R8), SAVE                  :: C_j_exp(num_source)  = 0.0_R8
    REAL (R8), SAVE                  :: C_sigma(num_source)  = 0.0_R8
    REAL (R8), SAVE                  :: C_Se_exp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Se_imp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Si_exp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Si_imp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Sz_exp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Sz_imp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Qe_exp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Qe_imp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Qi_exp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Qi_imp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Qz_exp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Qz_imp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Ui_exp(num_source) = 0.0_R8
    REAL (R8), SAVE                  :: C_Ui_imp(num_source) = 0.0_R8

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
       WRITE(*,*) 'ERROR: Could not assign source multipliers.'
    END IF




! +++ OUTPUT SOURCE CPO:
    NRHO                                       = SIZE(COREPROF(1)%rho_tor)
    ALLOCATE                                    (RHO_TOR(NRHO))

    CALL GET_COMP_DIMENSIONS                    (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

    CALL ALLOCATE_CORESOURCE_CPO                (NSLICE,      NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE_OUT)
    CALL ALLOCATE_CORESOURCE_CPO                (NUM_SOURCE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE_ARR)

    CALL DEALLOCATE_CPO                         (CORESOURCE_OUT(1)%compositions)
    CALL COPY_CPO                               (COREPROF(1)%compositions, CORESOURCE_OUT(1)%compositions)


    RHO_TOR                                    = COREPROF(1)%rho_tor     
    CORESOURCE_OUT(1)%VALUES(1)%rho_tor        = RHO_TOR     

    DO INUM=1,NUM_SOURCE 
       CORESOURCE_ARR(INUM)%VALUES(1)%rho_tor  = CORESOURCE_OUT(1)%VALUES(1)%rho_tor 
       CALL DEALLOCATE_CPO                      (CORESOURCE_ARR(INUM)%compositions)
       CALL COPY_CPO                            (CORESOURCE_OUT(1)%compositions, CORESOURCE_ARR(INUM)%compositions)
    END DO


! +++ Interpolate source profiles on the output grid:
    CALL INTERPOLATE_SOURCE                     (CORESOURCE1(1), CORESOURCE_ARR(1))
    CALL INTERPOLATE_SOURCE                     (CORESOURCE2(1), CORESOURCE_ARR(2))
    CALL INTERPOLATE_SOURCE                     (CORESOURCE3(1), CORESOURCE_ARR(3))
    CALL INTERPOLATE_SOURCE                     (CORESOURCE4(1), CORESOURCE_ARR(4))
    CALL INTERPOLATE_SOURCE                     (CORESOURCE5(1), CORESOURCE_ARR(5))
    CALL INTERPOLATE_SOURCE                     (CORESOURCE6(1), CORESOURCE_ARR(6))
    CALL INTERPOLATE_SOURCE                     (CORESOURCE7(1), CORESOURCE_ARR(7))



! +++ Combines sources:
    DO inum=1, num_source
! j
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%j) .AND.  C_j_exp(inum).NE.0.0_R8)                                    &
          CORESOURCE_OUT(1)%VALUES(1)%j             =  CORESOURCE_OUT(1)%VALUES(1)%j +                                     &
                                             CORESOURCE_ARR(inum)%VALUES(1)%j * C_j_exp(inum) 

! sigma 
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%sigma) .AND.  C_sigma(inum).NE.0.0_R8)                                &
          CORESOURCE_OUT(1)%VALUES(1)%sigma         = CORESOURCE_OUT(1)%VALUES(1)%sigma +                                  &
                                             C_sigma(inum) * CORESOURCE_ARR(inum)%VALUES(1)%sigma 

! Se
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Se%exp) .AND. C_Se_exp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Se%exp        = CORESOURCE_OUT(1)%VALUES(1)%Se%exp +                                  &
                                            C_Se_exp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Se%exp 
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Se%imp) .AND. C_Se_imp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Se%imp        = CORESOURCE_OUT(1)%VALUES(1)%Se%imp +                                  &
                                            C_Se_imp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Se%imp
! Qe    
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Qe%exp) .AND. C_Qe_exp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Qe%exp        = CORESOURCE_OUT(1)%VALUES(1)%Qe%exp +                                  &
                                            C_Qe_exp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Qe%exp
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Qe%imp) .AND. C_Qe_imp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Qe%imp        = CORESOURCE_OUT(1)%VALUES(1)%Qe%imp +                                  &
                                            C_Qe_imp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Qe%imp


! Si
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Si%exp) .AND. C_Si_exp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Si%exp        = CORESOURCE_OUT(1)%VALUES(1)%Si%exp +                                  &
                                            C_Si_exp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Si%exp
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Si%imp) .AND. C_Si_imp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Si%imp        = CORESOURCE_OUT(1)%VALUES(1)%Si%imp +                                  &
                                            C_Si_imp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Si%imp

! Qi
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Qi%exp) .AND. C_Qi_exp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Qi%exp        = CORESOURCE_OUT(1)%VALUES(1)%Qi%exp +                                  &
                                            C_Qi_exp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Qi%exp
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Qi%imp) .AND. C_Qi_imp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Qi%imp        = CORESOURCE_OUT(1)%VALUES(1)%Qi%imp +                                  &
                                            C_Qi_imp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Qi%imp

! Ui
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Ui%exp) .AND. C_Ui_exp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Ui%exp        = CORESOURCE_OUT(1)%VALUES(1)%Ui%exp +                                  &
                                            C_Ui_exp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Ui%exp
       IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Ui%imp) .AND. C_Ui_imp(inum).NE.0.0_R8) &
          CORESOURCE_OUT(1)%VALUES(1)%Ui%imp        = CORESOURCE_OUT(1)%VALUES(1)%Ui%imp +                                  &
                                            C_Ui_imp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Ui%imp 


! Sz
       DO IIMP = 1, NIMP
          IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Sz(IIMP)%exp) .AND. C_Sz_exp(inum).NE.0.0_R8)                       &
             CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%exp        = CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%exp +                   &
                                               C_Sz_exp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Sz(IIMP)%exp
          IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Sz(IIMP)%imp) .AND. C_Sz_imp(inum).NE.0.0_R8)                       &
             CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%imp        = CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%imp +                   &
                                               C_Sz_imp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Sz(IIMP)%imp
! Qz 
          IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Qz(IIMP)%exp) .AND. C_Qz_exp(inum).NE.0.0_R8)                       &
             CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%exp        = CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%exp +                   &
                                               C_Qz_exp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Qz(IIMP)%exp 
          IF (ASSOCIATED(CORESOURCE_ARR(inum)%VALUES(1)%Qz(IIMP)%imp) .AND. C_Qz_imp(inum).NE.0.0_R8)                       &
             CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%imp        = CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%imp +                   &
                                               C_Qz_imp(inum) * CORESOURCE_ARR(inum)%VALUES(1)%Qz(IIMP)%imp
       END DO

    END DO 



! +++ MIXING OF SOURCE PROFILES:
    CALL ALLOCATE_CORESOURCE_CPO    (NSLICE,      NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE_MIX)
    CALL DEALLOCATE_CPO             (CORESOURCE_MIX(1)%VALUES(1)%rho_tor)
    CALL COPY_CPO                   (CORESOURCE_OUT(1)%VALUES(1)%rho_tor,         CORESOURCE_MIX(1)%VALUES(1)%rho_tor)     
    CALL DEALLOCATE_CPO             (CORESOURCE_MIX(1)%compositions)
    CALL COPY_CPO                   (CORESOURCE_OUT(1)%compositions,              CORESOURCE_MIX(1)%compositions)     
    CALL INTERPOLATE_SOURCE         (CORESOURCE(1), CORESOURCE_MIX(1))
 
! J
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%j))                                                     &
      CORESOURCE_OUT(1)%VALUES(1)%j          = CORESOURCE_OUT(1)%VALUES(1)%j * AMIX_SRC +              &
                                               CORESOURCE_MIX(1)%VALUES(1)%J * (1.0_R8 - AMIX_SRC)             

! sigma 
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%sigma))                                                 &
       CORESOURCE_OUT(1)%VALUES(1)%sigma     = CORESOURCE_OUT(1)%VALUES(1)%sigma * AMIX_SRC +          &
                                               CORESOURCE_MIX(1)%VALUES(1)%sigma * (1.0_R8 - AMIX_SRC)

! Si
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Si%exp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Si%exp    = CORESOURCE_OUT(1)%VALUES(1)%Si%exp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Si%exp * (1.0_R8 - AMIX_SRC)
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Si%imp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Si%imp    = CORESOURCE_OUT(1)%VALUES(1)%Si%imp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Si%imp * (1.0_R8 - AMIX_SRC)
! Se
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Se%exp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Se%exp    = CORESOURCE_OUT(1)%VALUES(1)%Se%exp * AMIX_SRC  +        &
                                               CORESOURCE_MIX(1)%VALUES(1)%Se%exp * (1.0_R8 - AMIX_SRC)
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Se%imp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Se%imp    = CORESOURCE_OUT(1)%VALUES(1)%Se%imp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Se%imp * (1.0_R8 - AMIX_SRC)
! Qi
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Qi%exp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Qi%exp    = CORESOURCE_OUT(1)%VALUES(1)%Qi%exp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Qi%exp * (1.0_R8 - AMIX_SRC)
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Qi%imp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Qi%imp    = CORESOURCE_OUT(1)%VALUES(1)%Qi%imp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Qi%imp * (1.0_R8 - AMIX_SRC)
! Qe    
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Qe%exp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Qe%exp    = CORESOURCE_OUT(1)%VALUES(1)%Qe%exp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Qe%exp * (1.0_R8 - AMIX_SRC)
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Qe%imp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Qe%imp    = CORESOURCE_OUT(1)%VALUES(1)%Qe%imp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Qe%imp * (1.0_R8 - AMIX_SRC)
! Ui
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Ui%exp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Ui%exp    = CORESOURCE_OUT(1)%VALUES(1)%Ui%exp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Ui%exp * (1.0_R8 - AMIX_SRC)
    IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Ui%imp))                                                &
       CORESOURCE_OUT(1)%VALUES(1)%Ui%imp    = CORESOURCE_OUT(1)%VALUES(1)%Ui%imp * AMIX_SRC +         &
                                               CORESOURCE_MIX(1)%VALUES(1)%Ui%imp * (1.0_R8 - AMIX_SRC)

! Sz
    DO IIMP = 1, NIMP
       IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Sz(IIMP)%exp))                                                &
          CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%exp = CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%exp * AMIX_SRC +      &
                                                  CORESOURCE_MIX(1)%VALUES(1)%Sz(IIMP)%exp * (1.0_R8 - AMIX_SRC)
       IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Sz(IIMP)%imp))                                                &
          CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%imp = CORESOURCE_OUT(1)%VALUES(1)%Sz(IIMP)%imp * AMIX_SRC +      &
                                                  CORESOURCE_MIX(1)%VALUES(1)%Sz(IIMP)%imp * (1.0_R8 - AMIX_SRC)

! Qz 
       IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Qz(IIMP)%exp))                                                &
          CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%exp = CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%exp * AMIX_SRC +      &
                                                  CORESOURCE_MIX(1)%VALUES(1)%Qz(IIMP)%exp * (1.0_R8 - AMIX_SRC)
       IF (ASSOCIATED(CORESOURCE_MIX(1)%VALUES(1)%Qz(IIMP)%imp))                                                &
          CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%imp = CORESOURCE_OUT(1)%VALUES(1)%Qz(IIMP)%imp * AMIX_SRC +      &
                                                  CORESOURCE_MIX(1)%VALUES(1)%Qz(IIMP)%imp * (1.0_R8 - AMIX_SRC)
    END DO



! +++ Deallocation of internal variables:
    IF(ALLOCATED(RHO_TOR)) DEALLOCATE ( RHO_TOR )
    IF(ALLOCATED(NZIMP))   DEALLOCATE ( NZIMP   )
    IF(ALLOCATED(NCOMP))   DEALLOCATE ( NCOMP   )
    IF(ALLOCATED(NTYPE))   DEALLOCATE ( NTYPE   ) 
    CALL DEALLOCATE_CPO               (CORESOURCE_ARR)
    CALL DEALLOCATE_CPO               (CORESOURCE_MIX)



! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (CORESOURCE_OUT(1)%VALUES(1)%sourceid%id(1))
    ALLOCATE            (CORESOURCE_OUT(1)%VALUES(1)%sourceid%description(1))
    CORESOURCE_OUT(1)%VALUES(1)%sourceid%id          = 'combined'
    CORESOURCE_OUT(1)%VALUES(1)%sourceid%flag        = 30
    CORESOURCE_OUT(1)%VALUES(1)%sourceid%description = 'Combined source'
     



! +++ COPY INDIVIDUAL INPUT SOURCES TO OUTPUT CPO:
    ALLOCATE            (CORESOURCE_ARR(num_source))
    CALL COPY_CPO       (CORESOURCE1(1), CORESOURCE_ARR(1))
    CALL COPY_CPO       (CORESOURCE2(1), CORESOURCE_ARR(2))
    CALL COPY_CPO       (CORESOURCE3(1), CORESOURCE_ARR(3))
    CALL COPY_CPO       (CORESOURCE4(1), CORESOURCE_ARR(4))
    CALL COPY_CPO       (CORESOURCE5(1), CORESOURCE_ARR(5))
    CALL COPY_CPO       (CORESOURCE6(1), CORESOURCE_ARR(6))
    CALL COPY_CPO       (CORESOURCE7(1), CORESOURCE_ARR(7))
    
    IARR = 1
    DO INUM = 1, NUM_SOURCE
       IARR = IARR + SIZE(CORESOURCE_ARR(INUM)%VALUES)
    END  DO
    ALLOCATE            (CORESOURCE_MIX(1))
    ALLOCATE            (CORESOURCE_MIX(1)%VALUES(IARR))

    CALL COPY_CPO       (CORESOURCE_OUT(1)%VALUES(1), CORESOURCE_MIX(1)%VALUES(1))
    CALL DEALLOCATE_CPO (CORESOURCE_OUT(1)%VALUES)
    ALLOCATE            (CORESOURCE_OUT(1)%VALUES(IARR))
    CALL COPY_CPO       (CORESOURCE_MIX(1)%VALUES(1), CORESOURCE_OUT(1)%VALUES(1))
 

    IVAL = 2
    DO INUM = 1, NUM_SOURCE
       DO IARR = 1, SIZE(CORESOURCE_ARR(INUM)%VALUES)
          CALL COPY_CPO (CORESOURCE_ARR(INUM)%VALUES(IARR), CORESOURCE_OUT(1)%VALUES(IVAL))
          IF (.NOT.ASSOCIATED(CORESOURCE_OUT(1)%VALUES(IVAL)%sourceid%id)) THEN
             ALLOCATE   (CORESOURCE_OUT(1)%VALUES(IVAL)%sourceid%id(1))
             CORESOURCE_OUT(1)%VALUES(IVAL)%sourceid%id             = 'unspecified'
             CORESOURCE_OUT(1)%VALUES(IVAL)%sourceid%flag           = 0
             IF (.NOT.ASSOCIATED(CORESOURCE_OUT(1)%VALUES(IVAL)%sourceid%description)) THEN
                ALLOCATE(CORESOURCE_OUT(1)%VALUES(IVAL)%sourceid%description(1))
                CORESOURCE_OUT(1)%VALUES(IVAL)%sourceid%description = 'Unspecified source type'
             END IF
          END IF
          IVAL = IVAL + 1
       END DO
    END DO

    CALL DEALLOCATE_CPO (CORESOURCE_ARR)
    CALL DEALLOCATE_CPO (CORESOURCE_MIX)

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
  !     Source:       ---                                 !
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

      CHARACTER(len = 132)              :: prefix


      return_status          = 0      

      C_j_exp                = 0.0_R8
      C_sigma                = 0.0_R8
      C_Se_exp               = 0.0_R8
      C_Se_imp               = 0.0_R8
      C_Si_exp               = 0.0_R8
      C_Si_imp               = 0.0_R8
      C_Sz_exp               = 0.0_R8
      C_Sz_imp               = 0.0_R8
      C_Qe_exp               = 0.0_R8
      C_Qe_imp               = 0.0_R8
      C_Qi_exp               = 0.0_R8
      C_Qi_imp               = 0.0_R8
      C_Qz_exp               = 0.0_R8
      C_Qz_imp               = 0.0_R8
      C_Ui_exp               = 0.0_R8
      C_Ui_imp               = 0.0_R8


      prefix = 'parameters/CURRENT/jni'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_j_exp)
      prefix = 'parameters/CURRENT/conductivity'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_sigma)
      prefix = 'parameters/NE/explicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Se_exp)
      prefix = 'parameters/NE/implicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Se_imp)
      prefix = 'parameters/NI/explicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Si_exp)
      prefix = 'parameters/NI/implicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Si_imp)
      prefix = 'parameters/NZ/explicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Sz_exp)
      prefix = 'parameters/NZ/implicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Sz_imp)
      prefix = 'parameters/TE/explicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Qe_exp)
      prefix = 'parameters/TE/implicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Qe_imp)
      prefix = 'parameters/TI/explicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Qi_exp)
      prefix = 'parameters/TI/implicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Qi_imp)
      prefix = 'parameters/TZ/explicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Qz_exp)
      prefix = 'parameters/TZ/implicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Qz_imp)
      prefix = 'parameters/VTOR/explicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Ui_exp)
      prefix = 'parameters/VTOR/implicit_source'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_Ui_imp)
 
      
      RETURN

    END SUBROUTINE ASSIGN_COMBINER_PARAMETERS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    SUBROUTINE ASSIGN_MULTIPLIERS(prefix,codeparameters, return_status, multipliers)
  
      USE ITM_TYPES
      USE EUITM_XML_PARSER  

      IMPLICIT NONE

      TYPE(type_param)                  :: codeparameters
      INTEGER(ITM_I4)                   :: return_status 

      CHARACTER(len = 132)              :: prefix
      CHARACTER(len = 132)              :: multiplier_path(num_source)
      CHARACTER(len = 132)              :: parameter_value
      REAL(R8)                          :: multipliers(num_source)
      REAL(R8)                          :: value(1)

      TYPE(element),        POINTER     :: temp_pointer
      TYPE(tree)                        :: parameter_list

      INTEGER(ITM_I4)                   :: i_src, nval
      INTEGER(ITM_I4)                   :: nparm

      multiplier_path(1) = TRIM(ADJUSTL(prefix))//"/from_input_CPOs/Multipliers_for_contributions_from/Data_Base"
      multiplier_path(2) = TRIM(ADJUSTL(prefix))//"/from_input_CPOs/Multipliers_for_contributions_from/Gaussian"
      multiplier_path(3) = TRIM(ADJUSTL(prefix))//"/from_input_CPOs/Multipliers_for_contributions_from/Synchrotron"
      multiplier_path(4) = TRIM(ADJUSTL(prefix))//"/from_input_CPOs/Multipliers_for_contributions_from/HCD"
      multiplier_path(5) = TRIM(ADJUSTL(prefix))//"/from_input_CPOs/Multipliers_for_contributions_from/Neutrals"
      multiplier_path(6) = TRIM(ADJUSTL(prefix))//"/from_input_CPOs/Multipliers_for_contributions_from/Impurity"
      multiplier_path(7) = TRIM(ADJUSTL(prefix))//"/from_input_CPOs/Multipliers_for_contributions_from/Neoclassical"
      
!-- parse xml-string codeparameters%parameters
      CALL EUITM_XML_PARSE  (codeparameters, nparm, parameter_list)

      DO i_src = 1, num_source
         temp_pointer => parameter_list%first
         CALL find_parameter       (multiplier_path(i_src), parameter_value, temp_pointer)
         IF (LEN(TRIM(parameter_value)).GE.1) THEN
             CALL scan_str2real    (parameter_value, value ,nval)  
             multipliers(i_src)   = value(1)
         END IF
       END DO

!-- destroy tree
      CALL DESTROY_XML_TREE(parameter_list)

      RETURN

    END SUBROUTINE ASSIGN_MULTIPLIERS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



  END SUBROUTINE COMBINE_SOURCES
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



END MODULE SOURCE_COMBINER
