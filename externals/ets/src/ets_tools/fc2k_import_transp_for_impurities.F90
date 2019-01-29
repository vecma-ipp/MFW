! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


  !-------------------------------------------------------!
  !     This routine imports transport coeffients         !
  !     from main ions to impurities if CONTROL=1         !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for V&V between ETS and     !
  !                   ASTRA                               !
  !                                                       !
  !-------------------------------------------------------!

  SUBROUTINE  FC2K_IMPORT_TRANSPORT_FOR_IMPURITIES (CORETRANSP_IN, CORETRANSP_OUT, CONTROL)

    
    USE ALLOCATE_DEALLOCATE

    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES

    IMPLICIT NONE



! +++ CPOs:
    TYPE (TYPE_CORETRANSP),   POINTER :: CORETRANSP_IN(:)    
    TYPE (TYPE_CORETRANSP),   POINTER :: CORETRANSP_OUT(:)    

    INTEGER                           :: CONTROL
    INTEGER,               PARAMETER  :: NSLICE = 1          !number of CPO ocurancies in the work flow
    INTEGER                           :: NRHO                !number of radial points     (input, determined from COREPROF CPO)
    INTEGER                           :: NION, IION          !number of ion species       (input, determined from COREPROF CPO)
    INTEGER                           :: NIMP, IIMP          !number of impurities                (input)
    INTEGER                           :: NNUCL               !number of nuclei species
    INTEGER,              ALLOCATABLE :: NZIMP(:)            !number of ionization states for each impurity
    INTEGER                           ::       IZIMP         !
    INTEGER                           :: NNEUT               !number of neutrals species
    INTEGER,              ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
    INTEGER,              ALLOCATABLE :: NTYPE(:)            !number of types for each neutral



  
! +++ OUTPUT TRANSPORT CPO:
    NRHO                          = SIZE(CORETRANSP_IN(1)%VALUES(1)%rho_tor)
    CALL GET_COMP_DIMENSIONS       (CORETRANSP_IN(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
    CALL ALLOCATE_CORETRANSP_CPO   (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  CORETRANSP_OUT)
    call deallocate_cpo(CORETRANSP_OUT(1)%COMPOSITIONS)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%COMPOSITIONS,            CORETRANSP_OUT(1)%COMPOSITIONS)
    call deallocate_cpo(CORETRANSP_OUT(1)%datainfo)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%datainfo,                CORETRANSP_OUT(1)%datainfo)
    call deallocate_cpo(CORETRANSP_OUT(1)%codeparam)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%codeparam,               CORETRANSP_OUT(1)%codeparam)
    call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%sigma)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%VALUES(1)%sigma,         CORETRANSP_OUT(1)%VALUES(1)%sigma)
    call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%ni_transp)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%VALUES(1)%ni_transp,     CORETRANSP_OUT(1)%VALUES(1)%ni_transp)
    call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%ne_transp)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%VALUES(1)%ne_transp,     CORETRANSP_OUT(1)%VALUES(1)%ne_transp)
    call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%ti_transp)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%VALUES(1)%ti_transp,     CORETRANSP_OUT(1)%VALUES(1)%ti_transp)
    call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%te_transp)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%VALUES(1)%te_transp,     CORETRANSP_OUT(1)%VALUES(1)%te_transp)
    call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%vtor_transp)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%VALUES(1)%vtor_transp,   CORETRANSP_OUT(1)%VALUES(1)%vtor_transp)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%VALUES(1)%transportid,   CORETRANSP_OUT(1)%VALUES(1)%transportid)

    CORETRANSP_OUT(1)%VALUES(1)%rho_tor_norm                      = CORETRANSP_IN(1)%VALUES(1)%rho_tor_norm
    CORETRANSP_OUT(1)%VALUES(1)%rho_tor                           = CORETRANSP_IN(1)%VALUES(1)%rho_tor
    CORETRANSP_OUT(1)%time                                        = CORETRANSP_IN(1)%time

! +++ Transport coefficients for impurity:
    IF (CONTROL.EQ.1) THEN
       DO IIMP = 1, NIMP
          DO IZIMP = 1, NZIMP(IIMP)
! nz
            CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%diff_eff(:,IZIMP)    =    &
                     CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff(:,1,1)         &
                   + CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff(:,1,2)         &
                   + CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff(:,1,3)      
                                                
            CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff(:,IZIMP)   =    &
                     CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff(:,1,1)        &
                   + CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff(:,1,2)        &
                   + CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff(:,1,3)      
                                                
! tz
            CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%diff_eff(:, IZIMP)    =   &
                     CORETRANSP_IN(1)%VALUES(1)%ti_transp%diff_eff(:,1)        
                                                
            CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff(:, IZIMP)   =   &
                     CORETRANSP_IN(1)%VALUES(1)%ti_transp%vconv_eff(:,1)       

         END DO  
      END DO

    ELSE IF (CONTROL.EQ.2) THEN
       DO IIMP = 1, NIMP
          DO IZIMP = 1, NZIMP(IIMP)
! nz
            CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%diff_eff(:,IZIMP)    =    &
                     CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff(:,1)           &
                   + CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff(:,2)           &
                   + CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff(:,3)      
                                                
            CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff(:,IZIMP)   =    &
                     CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff(:,1)          &
                   + CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff(:,2)          &
                   + CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff(:,3)      
                                                
! tz
            CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%diff_eff(:, IZIMP)    =   &
                     CORETRANSP_IN(1)%VALUES(1)%te_transp%diff_eff(:)        
                                                
            CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff(:, IZIMP)   =   &
                     CORETRANSP_IN(1)%VALUES(1)%te_transp%vconv_eff(:)       

         END DO  
      END DO


    ELSE IF (CONTROL.EQ.0) THEN

       call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%nz_transp)
       CALL COPY_CPO      (CORETRANSP_IN(1)%VALUES(1)%nz_transp,     CORETRANSP_OUT(1)%VALUES(1)%nz_transp)
       call deallocate_cpo(CORETRANSP_OUT(1)%VALUES(1)%tz_transp)
       CALL COPY_CPO      (CORETRANSP_IN(1)%VALUES(1)%tz_transp,     CORETRANSP_OUT(1)%VALUES(1)%tz_transp)

    END IF

     

    RETURN


  END SUBROUTINE FC2K_IMPORT_TRANSPORT_FOR_IMPURITIES
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




