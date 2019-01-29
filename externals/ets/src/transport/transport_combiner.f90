MODULE TRANSPORT_COMBINER


CONTAINS
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

  SUBROUTINE  COMBINE_TRANSPORT                                  &
              (COREPROF,       CORETRANSP,                       &
               CORETRANSP1,    CORETRANSP2,       CORETRANSP3,   &
               CORETRANSP4,    CORETRANSP5,       CORETRANSP_OUT,&
               AMIX_TR,        code_parameters)

    
    USE ALLOCATE_DEALLOCATE

    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EUITM_ROUTINES
    USE EUITM_SCHEMAS
    USE EUITM_XML_PARSER  
    USE DEALLOCATE_STRUCTURES
    USE INTERPOLATE_CPO
    USE COPY_STRUCTURES

    IMPLICIT NONE


    INTEGER, PARAMETER               :: num_transp=5


! +++ CPOs:
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP(:)    

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP1(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP2(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP3(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP4(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP5(:)    

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_OUT(:)    

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_ARR(:) 
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_MIX(:)    

    TYPE (TYPE_PARAM)                :: code_parameters



    REAL (R8)                        :: AMIX_TR      

    REAL (R8), SAVE                  :: C_sigma(num_transp)      = 0.0_R8
    REAL (R8), SAVE                  :: C_ne_diff(num_transp)    = 0.0_R8
    REAL (R8), SAVE                  :: C_ne_vconv(num_transp)   = 0.0_R8
    REAL (R8), SAVE                  :: C_ni_diff(num_transp)    = 0.0_R8
    REAL (R8), SAVE                  :: C_ni_vconv(num_transp)   = 0.0_R8
    REAL (R8), SAVE                  :: C_te_diff(num_transp)    = 0.0_R8
    REAL (R8), SAVE                  :: C_te_vconv(num_transp)   = 0.0_R8
    REAL (R8), SAVE                  :: C_ti_diff(num_transp)    = 0.0_R8
    REAL (R8), SAVE                  :: C_ti_vconv(num_transp)   = 0.0_R8
    REAL (R8), SAVE                  :: C_vtor_diff(num_transp)  = 0.0_R8
    REAL (R8), SAVE                  :: C_vtor_vconv(num_transp) = 0.0_R8
    REAL (R8), SAVE                  :: C_nz_diff(num_transp)    = 0.0_R8
    REAL (R8), SAVE                  :: C_nz_vconv(num_transp)   = 0.0_R8
    REAL (R8), SAVE                  :: C_tz_diff(num_transp)    = 0.0_R8
    REAL (R8), SAVE                  :: C_tz_vconv(num_transp)   = 0.0_R8

    INTEGER                          :: negative_diff            = 0
    INTEGER                          :: ne_conv                  = 0
    INTEGER                          :: te_conv                  = 0
    INTEGER                          :: ni_conv                  = 0
    INTEGER                          :: ti_conv                  = 0
    INTEGER                          :: vtor_conv                = 0
    INTEGER                          :: nz_conv                  = 0
    INTEGER                          :: tz_conv                  = 0

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


    INTEGER                          :: INUM, IARR, IVAL
    INTEGER                          :: return_status








    CALL ASSIGN_COMBINER_PARAMETERS(code_parameters, return_status)
       
    IF (return_status /= 0) THEN
       WRITE(*,*) 'ERROR: Could not assign transport multipliers.'
    END IF


    CALL  DEALLOCATE_CPO (CORETRANSP_OUT)

! +++ OUTPUT TRANSPORT CPO:
    NRHO                      = SIZE(COREPROF(1)%rho_tor)
    ALLOCATE                        (RHO_TOR(NRHO))

    CALL GET_COMP_DIMENSIONS        (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

    CALL ALLOCATE_CORETRANSP_CPO    (NSLICE,      NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP_OUT)
    CALL ALLOCATE_CORETRANSP_CPO    (NUM_TRANSP, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP_ARR)

    CALL DEALLOCATE_CPO             (CORETRANSP_OUT(1)%compositions)
    CALL COPY_CPO                   (COREPROF(1)%compositions, CORETRANSP_OUT(1)%compositions)


    RHO_TOR                             = COREPROF(1)%rho_tor     
    CORETRANSP_OUT(1)%VALUES(1)%rho_tor = RHO_TOR     

    DO INUM=1,NUM_TRANSP 
       CORETRANSP_ARR(INUM)%VALUES(1)%rho_tor  = CORETRANSP_OUT(1)%VALUES(1)%rho_tor 
       CALL DEALLOCATE_CPO          (CORETRANSP_ARR(INUM)%compositions)
       CALL COPY_CPO                (CORETRANSP_OUT(1)%compositions, CORETRANSP_ARR(INUM)%compositions)
    END DO



! +++ Interpolate transport profiles on the output grid:
    CALL INTERPOLATE_TRANSP         (CORETRANSP1(1), CORETRANSP_ARR(1), negative_diff)
    CALL INTERPOLATE_TRANSP         (CORETRANSP2(1), CORETRANSP_ARR(2), negative_diff)
    CALL INTERPOLATE_TRANSP         (CORETRANSP3(1), CORETRANSP_ARR(3), negative_diff)
    CALL INTERPOLATE_TRANSP         (CORETRANSP4(1), CORETRANSP_ARR(4), negative_diff)
    CALL INTERPOLATE_TRANSP         (CORETRANSP5(1), CORETRANSP_ARR(5), negative_diff)



! +++ Combines transport coefficients:

    DO inum=1, num_transp
! sigma
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%sigma) .AND. C_sigma(inum).NE.0.0_R8)                              &
            CORETRANSP_OUT(1)%VALUES(1)%sigma                 = CORETRANSP_OUT(1)%VALUES(1)%sigma                      &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%sigma                   &
                                                              * C_sigma(inum) 
! ne
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ne_transp%diff_eff) .AND. C_ne_diff(inum).NE.0.0_R8)               &
            CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff    = CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff         &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ne_transp%diff_eff      &
                                                              * C_ne_diff(inum) 
       IF(ne_conv.EQ.0) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ne_transp%vconv_eff) .AND. C_ne_vconv(inum).NE.0.0_R8)             &
            CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ne_transp%vconv_eff     &
                                                              * C_ne_vconv(inum) 
       ELSE IF(ne_conv.EQ.1) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ne_transp%diff_eff) .AND. C_ne_vconv(inum).NE.0.0_R8)              &
            CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ne_transp%diff_eff      &
                                                              * C_ne_vconv(inum) 
       END IF
! te
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%te_transp%diff_eff) .AND. C_te_diff(inum).NE.0.0_R8)               &
            CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff    = CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff         &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%te_transp%diff_eff      &
                                                              * C_te_diff(inum) 
       IF(te_conv.EQ.0) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%te_transp%vconv_eff) .AND. C_te_vconv(inum).NE.0.0_R8)             &
            CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%te_transp%vconv_eff     &
                                                              * C_te_vconv(inum) 
       ELSE IF(te_conv.EQ.1) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%te_transp%diff_eff) .AND. C_te_vconv(inum).NE.0.0_R8)              &
            CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%te_transp%diff_eff      &
                                                              * C_te_vconv(inum) 
       END IF
! ni
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ni_transp%diff_eff) .AND. C_ni_diff(inum).NE.0.0_R8)               &
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff    = CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff         &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ni_transp%diff_eff      &
                                                              * C_ni_diff(inum) 
       IF(ni_conv.EQ.0) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ni_transp%vconv_eff) .AND. C_ni_vconv(inum).NE.0.0_R8)             &
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ni_transp%vconv_eff     &
                                                              * C_ni_vconv(inum) 
       ELSE IF(ni_conv.EQ.1) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ni_transp%diff_eff) .AND. C_ni_vconv(inum).NE.0.0_R8)              &
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ni_transp%diff_eff      &
                                                              * C_ni_vconv(inum)
       END IF
! ti
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ti_transp%diff_eff) .AND. C_ti_diff(inum).NE.0.0_R8)               &
            CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff    = CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff         &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ti_transp%diff_eff      &
                                                              * C_ti_diff(inum) 
       IF(ti_conv.EQ.0) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ti_transp%vconv_eff) .AND. C_ti_vconv(inum).NE.0.0_R8)             &
            CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ti_transp%vconv_eff     &
                                                              * C_ti_vconv(inum) 
       ELSE IF(ti_conv.EQ.1) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%ti_transp%diff_eff) .AND. C_ti_vconv(inum).NE.0.0_R8)              &
            CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff        &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%ti_transp%diff_eff      &
                                                              * C_ti_vconv(inum) 
       END IF
! vtor
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%vtor_transp%diff_eff) .AND. C_vtor_diff(inum).NE.0.0_R8)           &
            CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff  = CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff       &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%vtor_transp%diff_eff    &
                                                              * C_vtor_diff(inum)
       IF(vtor_conv.EQ.0) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%vtor_transp%vconv_eff) .AND. C_vtor_vconv(inum).NE.0.0_R8)         &
            CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff = CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff      &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%vtor_transp%vconv_eff   &
                                                              * C_vtor_vconv(inum) 
       ELSE IF(vtor_conv.EQ.1) THEN
       IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%vtor_transp%diff_eff) .AND. C_vtor_vconv(inum).NE.0.0_R8)          &
            CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff    &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%vtor_transp%diff_eff    &
                                                              * C_vtor_vconv(inum) 
       END IF


       DO IIMP = 1, NIMP
! nz
          IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%nz_transp(IIMP)%diff_eff) .AND. C_nz_diff(inum).NE.0.0_R8)            &
            CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%diff_eff    = CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%diff_eff   &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%nz_transp(IIMP)%diff_eff      &
                                                              * C_nz_diff(inum) 
          IF(nz_conv.EQ.0) THEN
          IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%nz_transp(IIMP)%vconv_eff) .AND. C_nz_vconv(inum).NE.0.0_R8)          &
            CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff  &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%nz_transp(IIMP)%vconv_eff     &
                                                              * C_nz_vconv(inum) 
          ELSE IF(nz_conv.EQ.1) THEN
          IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%nz_transp(IIMP)%diff_eff) .AND. C_nz_vconv(inum).NE.0.0_R8)           &
            CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff  &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%nz_transp(IIMP)%diff_eff      &
                                                              * C_nz_vconv(inum)
          END IF
! tz
          IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%tz_transp(IIMP)%diff_eff) .AND. C_tz_diff(inum).NE.0.0_R8)            &
            CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%diff_eff    = CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%diff_eff   &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%tz_transp(IIMP)%diff_eff      &
                                                              * C_tz_diff(inum)
          IF(tz_conv.EQ.0) THEN
          IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%tz_transp(IIMP)%vconv_eff) .AND. C_tz_vconv(inum).NE.0.0_R8)          &
            CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff  &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%tz_transp(IIMP)%vconv_eff     &
                                                              * C_tz_vconv(inum) 
          ELSE IF(tz_conv.EQ.1) THEN
          IF(ASSOCIATED(CORETRANSP_ARR(inum)%VALUES(1)%tz_transp(IIMP)%diff_eff) .AND. C_tz_vconv(inum).NE.0.0_R8)           &
            CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff   = CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff  &
                                                              + CORETRANSP_ARR(inum)%VALUES(1)%tz_transp(IIMP)%diff_eff      &
                                                              * C_tz_vconv(inum) 
          END IF
       END DO

    END DO



! +++ MIXING OF TRANSPORT PROFILES:
    CALL ALLOCATE_CORETRANSP_CPO    (NSLICE,      NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP_MIX)
    CALL DEALLOCATE_CPO             (CORETRANSP_MIX(1)%VALUES(1)%rho_tor)
    CALL COPY_CPO                   (CORETRANSP_OUT(1)%VALUES(1)%rho_tor,   CORETRANSP_MIX(1)%VALUES(1)%rho_tor)
    CALL DEALLOCATE_CPO             (CORETRANSP_MIX(1)%compositions)
    CALL COPY_CPO                   (CORETRANSP_OUT(1)%compositions,        CORETRANSP_MIX(1)%compositions)
    CALL INTERPOLATE_TRANSP         (CORETRANSP(1),                         CORETRANSP_MIX(1),    negative_diff)
 
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%sigma))                                                                         &
         CORETRANSP_OUT(1)%VALUES(1)%sigma                    = CORETRANSP_OUT(1)%VALUES(1)%sigma * AMIX_TR                   &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%sigma * (1.0_R8 - AMIX_TR)             
! ni
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%ni_transp%diff_eff))                                                            &
         CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff       = CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff * AMIX_TR      &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%ni_transp%diff_eff * (1.0_R8 - AMIX_TR)      

    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%ni_transp%vconv_eff))                                                           &
         CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff      = CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff * AMIX_TR     &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%ni_transp%vconv_eff * (1.0_R8 - AMIX_TR)     
! ne
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%ne_transp%diff_eff))                                                            &
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff       = CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff * AMIX_TR      &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%ne_transp%diff_eff * (1.0_R8 - AMIX_TR)    
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%ne_transp%vconv_eff))                                                           &
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff      = CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff * AMIX_TR     &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%ne_transp%vconv_eff * (1.0_R8 - AMIX_TR)     
! ti
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%ti_transp%diff_eff))                                                            &
         CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff       = CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff * AMIX_TR      &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%ti_transp%diff_eff * (1.0_R8 - AMIX_TR)     
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%ti_transp%vconv_eff))                                                           &
         CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff      = CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff * AMIX_TR     &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%ti_transp%vconv_eff * (1.0_R8 - AMIX_TR)    
! te
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%te_transp%diff_eff))                                                            &
         CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff       = CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff * AMIX_TR      &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%te_transp%diff_eff * (1.0_R8 - AMIX_TR)      
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%te_transp%vconv_eff))                                                           &
         CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff      = CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff * AMIX_TR     &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%te_transp%vconv_eff * (1.0_R8 - AMIX_TR)     
! vtor
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%vtor_transp%diff_eff))                                                          &
         CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff     = CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff * AMIX_TR    &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%vtor_transp%diff_eff * (1.0_R8 - AMIX_TR)    
    IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%vtor_transp%vconv_eff))                                                         &
         CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff    = CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff * AMIX_TR   &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%vtor_transp%vconv_eff * (1.0_R8 - AMIX_TR)  

    DO IIMP = 1, NIMP
! nz
       IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%nz_transp(IIMP)%diff_eff))                                                   &
         CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%diff_eff = CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%diff_eff * AMIX_TR      &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%nz_transp(IIMP)%diff_eff * (1.0_R8 - AMIX_TR)    
       IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff))                                                  &
         CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff= CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff * AMIX_TR     &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff * (1.0_R8 - AMIX_TR)  
! tz
       IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%tz_transp(IIMP)%diff_eff))                                                   &
         CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%diff_eff = CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%diff_eff * AMIX_TR      &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%tz_transp(IIMP)%diff_eff * (1.0_R8 - AMIX_TR)    
       IF(ASSOCIATED(CORETRANSP_MIX(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff))                                                  &
         CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff= CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff * AMIX_TR     &
                                                              + CORETRANSP_MIX(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff * (1.0_R8 - AMIX_TR)  
    END DO

! +++ Deallocation of internal variables:
    IF(ALLOCATED(RHO_TOR)) DEALLOCATE ( RHO_TOR )
    IF(ALLOCATED(NZIMP))   DEALLOCATE ( NZIMP   )
    IF(ALLOCATED(NCOMP))   DEALLOCATE ( NCOMP   )
    IF(ALLOCATED(NTYPE))   DEALLOCATE ( NTYPE   ) 
    CALL DEALLOCATE_CPO               (CORETRANSP_ARR)
    CALL DEALLOCATE_CPO               (CORETRANSP_MIX)

! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (CORETRANSP_OUT(1)%VALUES(1)%transportid%id(1))
    ALLOCATE            (CORETRANSP_OUT(1)%VALUES(1)%transportid%description(1))
    CORETRANSP_OUT(1)%VALUES(1)%transportid%id               = 'combined'
    CORETRANSP_OUT(1)%VALUES(1)%transportid%flag             = 12
    CORETRANSP_OUT(1)%VALUES(1)%transportid%description      = 'Derived from a number of contributions'
     



! +++ COPY INDIVIDUAL INPUT TRANSPORT CONTRIBUTIONS TO OUTPUT CPO:
    ALLOCATE            (CORETRANSP_ARR(6))
    CALL COPY_CPO       (CORETRANSP1(1), CORETRANSP_ARR(1))
    CALL COPY_CPO       (CORETRANSP2(1), CORETRANSP_ARR(2))
    CALL COPY_CPO       (CORETRANSP3(1), CORETRANSP_ARR(3))
    CALL COPY_CPO       (CORETRANSP4(1), CORETRANSP_ARR(4))
    CALL COPY_CPO       (CORETRANSP5(1), CORETRANSP_ARR(5))
    
    IARR = 1
    DO INUM = 1, NUM_TRANSP
       IARR = IARR + SIZE(CORETRANSP_ARR(INUM)%VALUES)
    END  DO
    ALLOCATE            (CORETRANSP_MIX(1))
    ALLOCATE            (CORETRANSP_MIX(1)%VALUES(IARR))

    CALL COPY_CPO       (CORETRANSP_OUT(1)%VALUES(1), CORETRANSP_MIX(1)%VALUES(1))
    CALL DEALLOCATE_CPO (CORETRANSP_OUT(1)%VALUES)
    ALLOCATE            (CORETRANSP_OUT(1)%VALUES(IARR))
    CALL COPY_CPO       (CORETRANSP_MIX(1)%VALUES(1), CORETRANSP_OUT(1)%VALUES(1))
 

    IVAL = 2
    DO INUM = 1, NUM_TRANSP
       DO IARR = 1, SIZE(CORETRANSP_ARR(INUM)%VALUES)
          CALL COPY_CPO (CORETRANSP_ARR(INUM)%VALUES(IARR), CORETRANSP_OUT(1)%VALUES(IVAL))
          IF (.NOT.ASSOCIATED(CORETRANSP_OUT(1)%VALUES(IVAL)%transportid%id)) THEN
             ALLOCATE   (CORETRANSP_OUT(1)%VALUES(IVAL)%transportid%id(1))
             CORETRANSP_OUT(1)%VALUES(IVAL)%transportid%id             = 'unspecified'
             CORETRANSP_OUT(1)%VALUES(IVAL)%transportid%flag           = 0
             IF (.NOT.ASSOCIATED(CORETRANSP_OUT(1)%VALUES(IVAL)%transportid%description)) THEN
                ALLOCATE(CORETRANSP_OUT(1)%VALUES(IVAL)%transportid%description(1))
                CORETRANSP_OUT(1)%VALUES(IVAL)%transportid%description = 'Unspecified transport type'
             END IF
          END IF
          IVAL = IVAL + 1
       END DO
    END DO

    CALL DEALLOCATE_CPO (CORETRANSP_ARR)
    CALL DEALLOCATE_CPO (CORETRANSP_MIX)



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
      INTEGER(ITM_I4)                   :: vod 
      INTEGER(ITM_I4)                   :: nparm

      CHARACTER(len = 132)              :: prefix

      TYPE(element),        POINTER     :: temp_pointer
      TYPE(tree)                        :: parameter_list
      CHARACTER(len = 132)              :: parameter_value
      CHARACTER(len = 4)                :: parameter_char


      return_status            = 0      

      C_sigma                  = 0.0_R8
      C_ne_diff                = 0.0_R8
      C_ne_vconv               = 0.0_R8
      C_ni_diff                = 0.0_R8
      C_ni_vconv               = 0.0_R8
      C_te_diff                = 0.0_R8
      C_te_vconv               = 0.0_R8
      C_ti_diff                = 0.0_R8
      C_ti_vconv               = 0.0_R8
      C_vtor_diff              = 0.0_R8
      C_vtor_vconv             = 0.0_R8
      C_nz_diff                = 0.0_R8
      C_nz_vconv               = 0.0_R8
      C_tz_diff                = 0.0_R8
      C_tz_vconv               = 0.0_R8

      negative_diff            = 0


      prefix = 'parameters/CURRENT/conductivity'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_sigma,      vod)
      prefix = 'parameters/NE/diffusion'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_ne_diff,    vod)
      prefix = 'parameters/NE/convective_velocity'
      ne_conv    = 0
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_ne_vconv,   ne_conv)
      prefix = 'parameters/NI/diffusion'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_ni_diff,    vod)
      prefix = 'parameters/NI/convective_velocity'
      ni_conv    = 0
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_ni_vconv,   ni_conv)
      prefix = 'parameters/NZ/diffusion'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_nz_diff,    vod)
      prefix = 'parameters/NZ/convective_velocity'
      nz_conv    = 0
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_nz_vconv,   nz_conv)
      prefix = 'parameters/TE/diffusion'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_te_diff,    vod)
      prefix = 'parameters/TE/convective_velocity'
      te_conv    = 0
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_te_vconv,   te_conv)
      prefix = 'parameters/TI/diffusion'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_ti_diff,    vod)
      prefix = 'parameters/TI/convective_velocity'
      ti_conv    = 0
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_ti_vconv,   ti_conv)
      prefix = 'parameters/TZ/diffusion'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_tz_diff,    vod)
      prefix = 'parameters/TZ/convective_velocity'
      tz_conv    = 0
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_tz_vconv,   tz_conv)
      prefix = 'parameters/VTOR/diffusion'
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_vtor_diff,  vod)
      prefix = 'parameters/VTOR/convective_velocity'
      vtor_conv    = 0
      CALL ASSIGN_MULTIPLIERS(prefix,codeparameters,return_status,    C_vtor_vconv, vtor_conv)
 
      CALL EUITM_XML_PARSE  (codeparameters, nparm, parameter_list)
      temp_pointer => parameter_list%first
      prefix = 'parameters/CHECKs/remove_negative_diffusion'
      CALL find_parameter(prefix, parameter_value, temp_pointer)
      parameter_char = TRIM(ADJUSTL(parameter_value))
      IF (parameter_char.EQ."true") negative_diff = 1
      CALL DESTROY_XML_TREE(parameter_list)

      RETURN

    END SUBROUTINE ASSIGN_COMBINER_PARAMETERS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    SUBROUTINE ASSIGN_MULTIPLIERS(prefix,codeparameters, return_status, multipliers, vod)
  
      USE ITM_TYPES
      USE EUITM_XML_PARSER  

      IMPLICIT NONE

      TYPE(type_param)                  :: codeparameters
      INTEGER(ITM_I4)                   :: return_status 

      CHARACTER(len = 132)              :: prefix
      CHARACTER(len = 132)              :: inter_path(3)
      CHARACTER(len = 132)              :: multiplier_path(num_transp)
      CHARACTER(len = 132)              :: parameter_value
      REAL(R8)                          :: multipliers(num_transp)
      REAL(R8)                          :: value(1)

      TYPE(element),        POINTER     :: temp_pointer
      TYPE(tree)                        :: parameter_list

      INTEGER(ITM_I4)                   :: i_transp, nval, i_int
      INTEGER(ITM_I4)                   :: nparm,    vod

      vod                = 0

!-- parse xml-string codeparameters%parameters
      CALL EUITM_XML_PARSE  (codeparameters, nparm, parameter_list)

      inter_path(1)         = TRIM(ADJUSTL(prefix))//"/Multipliers_for_conductivity_from"
      inter_path(2)         = TRIM(ADJUSTL(prefix))//"/Multipliers_for_contributions_from"
      inter_path(3)         = TRIM(ADJUSTL(prefix))//"/V_over_D_ratio_for_contributions_from"

      DO i_int = 1,3
         multiplier_path(1) = TRIM(ADJUSTL(inter_path(i_int)))//"/Data_Base"
         multiplier_path(2) = TRIM(ADJUSTL(inter_path(i_int)))//"/Anomalous"
         multiplier_path(3) = TRIM(ADJUSTL(inter_path(i_int)))//"/Neoclassical"
         multiplier_path(4) = TRIM(ADJUSTL(inter_path(i_int)))//"/Background"
         multiplier_path(5) = TRIM(ADJUSTL(inter_path(i_int)))//"/Spitzer"
         
         DO i_transp = 1, num_transp
            temp_pointer => parameter_list%first
            CALL find_parameter   (multiplier_path(i_transp), parameter_value, temp_pointer)
            IF (LEN(TRIM(parameter_value)).GE.1) THEN
               CALL scan_str2real    (parameter_value, value ,nval)  
               multipliers(i_transp)   = value(1)
            END IF
         END DO

         IF (i_int.EQ.3.AND.ABS(MAXVAL(multipliers)).NE.0.0_R8) vod = 1

      END DO

 


!-- destroy tree
      CALL DESTROY_XML_TREE(parameter_list)

      RETURN

    END SUBROUTINE ASSIGN_MULTIPLIERS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


  END SUBROUTINE COMBINE_TRANSPORT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



END MODULE TRANSPORT_COMBINER
