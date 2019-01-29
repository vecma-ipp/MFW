! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE CHECK_TRANSPORT_BOUND(COREPROF, CORETRANSP_IN, CORETRANSP_OUT)
  !-------------------------------------------------------!
  !     This subroutine checks the boundary values of     !
  !     transport coeffients                              !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for use of GEM in ETS       !
  !                                                       !
  !-------------------------------------------------------!
    
    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES
    USE ALLOCATE_DEALLOCATE
    USE DEALLOCATE_STRUCTURES
    
    IMPLICIT NONE
    
    
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ CPOs:
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)  
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_IN(:)    
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_OUT(:)    

    INTEGER,              PARAMETER  :: NSLICE = 1         
    INTEGER                          :: NRHO_PROF
    INTEGER                          :: NRHO_TR
    INTEGER                          :: NNUCL_TR
    INTEGER                          :: NION_TR
    INTEGER                          :: NIMP_TR, IIMP
    INTEGER,             ALLOCATABLE :: NZIMP_TR(:)
    INTEGER                          :: NNEUT_TR
    INTEGER,             ALLOCATABLE :: NCOMP_TR(:)
    INTEGER,             ALLOCATABLE :: NTYPE_TR(:)

    INTEGER                          :: FLAG_BND(2)
    INTEGER                          :: ISTART, IEND
    INTEGER                          :: IRHO




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ OUTPUT TRANSPORT CPO:
    NRHO_PROF                     = SIZE(COREPROF(1)%rho_tor)
    NRHO_TR                       = SIZE(CORETRANSP_IN(1)%VALUES(1)%rho_tor)


    CALL GET_COMP_DIMENSIONS       (CORETRANSP_IN(1)%COMPOSITIONS, NNUCL_TR, NION_TR,  NIMP_TR,  NZIMP_TR, NNEUT_TR, NTYPE_TR, NCOMP_TR)



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    FLAG_BND(:)                   = 0
    ISTART                        = 0
    IEND                          = 0

    IF ((COREPROF(1)%rho_tor(1).GE.CORETRANSP_IN(1)%VALUES(1)%rho_tor(1)).AND.    &
        (COREPROF(1)%rho_tor(NRHO_PROF).LE.CORETRANSP_IN(1)%VALUES(1)%rho_tor(NRHO_TR))) THEN
       ALLOCATE                    (CORETRANSP_OUT(1))
       CALL COPY_CPO               (CORETRANSP_IN(1), CORETRANSP_OUT(1))
       GOTO 100
    END IF


    IF ((COREPROF(1)%rho_tor(NRHO_PROF).GT.CORETRANSP_IN(1)%VALUES(1)%rho_tor(NRHO_TR))) THEN
       NRHO_TR                    = NRHO_TR + 1
       FLAG_BND(2)                = 1
    END IF
    IF ((COREPROF(1)%rho_tor(1).LT.CORETRANSP_IN(1)%VALUES(1)%rho_tor(1))) THEN
       NRHO_TR                    = NRHO_TR + 1
       FLAG_BND(1)                = 1
    END IF



    CALL ALLOCATE_CORETRANSP_CPO   (NSLICE,  NRHO_TR,  NNUCL_TR, NION_TR,  NIMP_TR,  NZIMP_TR, NNEUT_TR, NTYPE_TR, NCOMP_TR,  CORETRANSP_OUT)
    CALL COPY_CPO                  (CORETRANSP_IN(1)%COMPOSITIONS, CORETRANSP_OUT(1)%COMPOSITIONS)
      



    IF (FLAG_BND(1).EQ.1) THEN
       ISTART = 1
       CORETRANSP_OUT(1)%VALUES(1)%rho_tor(1)                                            = COREPROF(1)%rho_tor(1)
       CORETRANSP_OUT(1)%VALUES(1)%rho_tor_norm(1)                                       = COREPROF(1)%rho_tor(1)/COREPROF(1)%rho_tor(NRHO_PROF)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff(1,:,:)               = CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff(1,:,:)  
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff(1,:,:)              = CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff(1,:,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff(1,:)                 = CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff(1,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff(1,:)                = CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff(1,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ti_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff(1,:)                 = CORETRANSP_IN(1)%VALUES(1)%ti_transp%diff_eff(1,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ti_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff(1,:)                = CORETRANSP_IN(1)%VALUES(1)%ti_transp%vconv_eff(1,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%te_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff(1)                   = CORETRANSP_IN(1)%VALUES(1)%te_transp%diff_eff(1)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%te_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff(1)                  = CORETRANSP_IN(1)%VALUES(1)%te_transp%vconv_eff(1)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%vtor_transp%diff_eff))  &
                     CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff(1,:)               = CORETRANSP_IN(1)%VALUES(1)%vtor_transp%diff_eff(1,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%vtor_transp%vconv_eff)) &
                     CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff(1,:)              = CORETRANSP_IN(1)%VALUES(1)%vtor_transp%vconv_eff(1,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%sigma))                 &
                     CORETRANSP_OUT(1)%VALUES(1)%sigma(1)                                = CORETRANSP_IN(1)%VALUES(1)%sigma(1)

       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP)) THEN 
         DO IIMP=1, NIMP_TR
           IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff))  &
                         CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(1,:)       = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(1,:)  
           IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff)) &
                         CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(1,:)      = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(1,:)  
          END DO
       END IF

       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP)) THEN 
          DO IIMP=1, NIMP_TR
           IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff))  &
                         CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(1,:)       = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(1,:)  
           IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff)) &
                         CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(1,:)      = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(1,:)  
          END DO
       END IF

    END IF

      
    IF (FLAG_BND(2).EQ.1) THEN
       IEND = 1
       CORETRANSP_OUT(1)%VALUES(1)%rho_tor(NRHO_TR)                                      = COREPROF(1)%rho_tor(NRHO_PROF)
       CORETRANSP_OUT(1)%VALUES(1)%rho_tor_norm(NRHO_TR)                                 = COREPROF(1)%rho_tor(NRHO_PROF)/COREPROF(1)%rho_tor(NRHO_PROF)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff(NRHO_TR,:,:)         = CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff(NRHO_TR-ISTART-IEND,:,:)  
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff(NRHO_TR,:,:)        = CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff(NRHO_TR-ISTART-IEND,:,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff(NRHO_TR,:)           = CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff(NRHO_TR-ISTART-IEND,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff(NRHO_TR,:)          = CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff(NRHO_TR-ISTART-IEND,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ti_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff(NRHO_TR,:)           = CORETRANSP_IN(1)%VALUES(1)%ti_transp%diff_eff(NRHO_TR-ISTART-IEND,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ti_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff(NRHO_TR,:)          = CORETRANSP_IN(1)%VALUES(1)%ti_transp%vconv_eff(NRHO_TR-ISTART-IEND,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%te_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff(NRHO_TR)             = CORETRANSP_IN(1)%VALUES(1)%te_transp%diff_eff(NRHO_TR-ISTART-IEND)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%te_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff(NRHO_TR)            = CORETRANSP_IN(1)%VALUES(1)%te_transp%vconv_eff(NRHO_TR-ISTART-IEND)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%vtor_transp%diff_eff))  &
                     CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff(NRHO_TR,:)         = CORETRANSP_IN(1)%VALUES(1)%vtor_transp%diff_eff(NRHO_TR-ISTART-IEND,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%vtor_transp%vconv_eff)) &
                     CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff(NRHO_TR,:)        = CORETRANSP_IN(1)%VALUES(1)%vtor_transp%vconv_eff(NRHO_TR-ISTART-IEND,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%sigma))                 &
                     CORETRANSP_OUT(1)%VALUES(1)%sigma(NRHO_TR)                          = CORETRANSP_IN(1)%VALUES(1)%sigma(NRHO_TR-ISTART-IEND)

       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP)) THEN 
         DO IIMP=1, NIMP_TR
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff))  &
                        CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(NRHO_TR,:)  = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(NRHO_TR-ISTART-IEND,:)  
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff)) &
                        CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(NRHO_TR,:) = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(NRHO_TR-ISTART-IEND,:)  
         END DO
       END IF

       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP)) THEN 
         DO IIMP=1, NIMP_TR
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff))  &
                        CORETRANSP_OUT(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff(NRHO_TR,:)  = CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff(NRHO_TR-ISTART-IEND,:)  
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff)) &
                        CORETRANSP_OUT(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff(NRHO_TR,:) = CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff(NRHO_TR-ISTART-IEND,:)  
         END DO
       END IF

    END IF

    DO IRHO = ISTART+1, NRHO_TR-IEND 
       CORETRANSP_OUT(1)%VALUES(1)%rho_tor(IRHO)                                         = CORETRANSP_IN(1)%VALUES(1)%rho_tor(IRHO-ISTART)
       CORETRANSP_OUT(1)%VALUES(1)%rho_tor_norm(IRHO)                                    = CORETRANSP_IN(1)%VALUES(1)%rho_tor(IRHO-ISTART)/COREPROF(1)%rho_tor(NRHO_PROF)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff(IRHO,:,:)            = CORETRANSP_IN(1)%VALUES(1)%ni_transp%diff_eff(IRHO-ISTART,:,:)  
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff(IRHO,:,:)           = CORETRANSP_IN(1)%VALUES(1)%ni_transp%vconv_eff(IRHO-ISTART,:,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff(IRHO,:)              = CORETRANSP_IN(1)%VALUES(1)%ne_transp%diff_eff(IRHO-ISTART,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff(IRHO,:)             = CORETRANSP_IN(1)%VALUES(1)%ne_transp%vconv_eff(IRHO-ISTART,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ti_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff(IRHO,:)              = CORETRANSP_IN(1)%VALUES(1)%ti_transp%diff_eff(IRHO-ISTART,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%ti_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff(IRHO,:)             = CORETRANSP_IN(1)%VALUES(1)%ti_transp%vconv_eff(IRHO-ISTART,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%te_transp%diff_eff))    &
                     CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff(IRHO)                = CORETRANSP_IN(1)%VALUES(1)%te_transp%diff_eff(IRHO-ISTART)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%te_transp%vconv_eff))   &
                     CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff(IRHO)               = CORETRANSP_IN(1)%VALUES(1)%te_transp%vconv_eff(IRHO-ISTART)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%vtor_transp%diff_eff))  &
                     CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff(IRHO,:)            = CORETRANSP_IN(1)%VALUES(1)%vtor_transp%diff_eff(IRHO-ISTART,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%vtor_transp%vconv_eff)) &
                     CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff(IRHO,:)           = CORETRANSP_IN(1)%VALUES(1)%vtor_transp%vconv_eff(IRHO-ISTART,:)
       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%sigma))                 &
                     CORETRANSP_OUT(1)%VALUES(1)%sigma(IRHO)                             = CORETRANSP_IN(1)%VALUES(1)%sigma(IRHO-ISTART)

       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP)) THEN 
         DO IIMP=1, NIMP_TR
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff))  &
                        CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(IRHO,:)     = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(IRHO-ISTART,:)  
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff)) &
                        CORETRANSP_OUT(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(IRHO,:)    = CORETRANSP_IN(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(IRHO-ISTART,:)  
         END DO
       END IF

       IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP)) THEN 
         DO IIMP=1, NIMP_TR
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff))  &
                        CORETRANSP_OUT(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff(IRHO,:)     = CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff(IRHO-ISTART,:)  
          IF(ASSOCIATED(CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff)) &
                        CORETRANSP_OUT(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff(IRHO,:)    = CORETRANSP_IN(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff(IRHO-ISTART,:)  
         END DO
       END IF

    END DO



    IF(ALLOCATED  (NZIMP_TR)) THEN
       DEALLOCATE (NZIMP_TR)
    END IF
    IF(ALLOCATED  (NCOMP_TR)) THEN
       DEALLOCATE (NCOMP_TR)
    END IF
    IF(ALLOCATED  (NTYPE_TR)) THEN
       DEALLOCATE (NTYPE_TR)
    END IF


     

 100   RETURN


  END SUBROUTINE CHECK_TRANSPORT_BOUND
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
 
 
