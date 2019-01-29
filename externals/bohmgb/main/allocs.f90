  SUBROUTINE ALLOCATE_CORETRANSP_CPO (NSLICE, NRHO, NION, CORETRANSP)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates CORETRANSP CPO             !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


    USE EUITM_SCHEMAS
    USE ITM_TYPES

    IMPLICIT NONE

    INTEGER                          :: NSLICE, ISLICE      !number of slices and slice index
    INTEGER                          :: NRHO                !number of radial points    (input)
    INTEGER                          :: NION                !number of ion species      (input)

    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP(:)       !CPO with transport coefficients



    ALLOCATE (CORETRANSP(NSLICE))


    DO ISLICE = 1,NSLICE

! +++ Radial coordinate:
       ALLOCATE (CORETRANSP(ISLICE)%rho_tor(NRHO))


! +++ Profiles of transport coefficients:
       ALLOCATE (CORETRANSP(ISLICE)%sigma(NRHO))
       ALLOCATE (CORETRANSP(ISLICE)%ne_transp%diff_eff(NRHO,3))
       ALLOCATE (CORETRANSP(ISLICE)%ne_transp%vconv_eff(NRHO,3))
       ALLOCATE (CORETRANSP(ISLICE)%ni_transp%diff_eff(NRHO,NION,3))
       ALLOCATE (CORETRANSP(ISLICE)%ni_transp%vconv_eff(NRHO,NION,3))
       ALLOCATE (CORETRANSP(ISLICE)%ti_transp%diff_eff(NRHO,NION))
       ALLOCATE (CORETRANSP(ISLICE)%ti_transp%vconv_eff(NRHO,NION))
       ALLOCATE (CORETRANSP(ISLICE)%te_transp%diff_eff(NRHO))
       ALLOCATE (CORETRANSP(ISLICE)%te_transp%vconv_eff(NRHO))
       ALLOCATE (CORETRANSP(ISLICE)%vtor_transp%diff_eff(NRHO,NION))
       ALLOCATE (CORETRANSP(ISLICE)%vtor_transp%vconv_eff(NRHO,NION))


       CORETRANSP(ISLICE)%sigma(:)                       = 0.0_R8
       CORETRANSP(ISLICE)%ne_transp%diff_eff(:,:)        = 0.0_R8
       CORETRANSP(ISLICE)%ne_transp%vconv_eff(:,:)       = 0.0_R8
       CORETRANSP(ISLICE)%ni_transp%diff_eff(:,:,:)      = 0.0_R8
       CORETRANSP(ISLICE)%ni_transp%vconv_eff(:,:,:)     = 0.0_R8
       CORETRANSP(ISLICE)%ti_transp%diff_eff(:,:)        = 0.0_R8
       CORETRANSP(ISLICE)%ti_transp%vconv_eff(:,:)       = 0.0_R8
       CORETRANSP(ISLICE)%te_transp%diff_eff(:)          = 0.0_R8
       CORETRANSP(ISLICE)%te_transp%vconv_eff(:)         = 0.0_R8
       CORETRANSP(ISLICE)%vtor_transp%diff_eff(:,:)      = 0.0_R8
       CORETRANSP(ISLICE)%vtor_transp%vconv_eff(:,:)     = 0.0_R8

    END DO

    RETURN

  END SUBROUTINE ALLOCATE_CORETRANSP_CPO
