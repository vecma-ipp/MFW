! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This module contains routines for allocation/deallocation if CPOs used in ETS
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE ALLOCATE_DEALLOCATE
!-------------------------------------------------------!  
!                                                       !
!     This module contains routines for                 !
!     allocation/deallocation if CPOs used in ETS       !
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

CONTAINS

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates COREPROF CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ALLOCATE_COREPROF_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  COREPROF)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COREPROF CPO               !
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
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE, ISLICE      !number of slices and slice index
      INTEGER                              :: NRHO                !number of radial points    (input)
      INTEGER                              :: NNUCL,    INUCL     !number of nuclei species
      INTEGER                              :: NION,     IION      !number of ion species
      INTEGER                              :: NIMP,     IIMP      !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)            !number of ionization states for each impurity
      INTEGER                              ::           IZIMP       
      INTEGER                              :: NNEUT,    INEUT     !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)            !number of types for each neutral
      INTEGER                              ::           ITYPE       

      TYPE (TYPE_COREPROF),        POINTER :: COREPROF(:)         !CPO with internal ETS parameters profiles 
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS

!    write(*,*) '*** ALLOCATE_COREPROF_CPO CALLED ***'

      ALLOCATE (COREPROF(NSLICE))
      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)


      DO ISLICE = 1,NSLICE

         CALL deallocate_cpo(COREPROF(ISLICE)%compositions)
         CALL COPY_CPO(COMPOSITIONS, COREPROF(ISLICE)%compositions)


! +++ Codeparam:
         ALLOCATE (COREPROF(ISLICE)%codeparam%codename(1))
         ALLOCATE (COREPROF(ISLICE)%codeparam%codeversion(1))
         ALLOCATE (COREPROF(ISLICE)%codeparam%output_diag(1))


! +++ Radial coordinate:
         ALLOCATE (COREPROF(ISLICE)%rho_tor(NRHO))
         ALLOCATE (COREPROF(ISLICE)%rho_tor_norm(NRHO))



! +++ Plasma primary profiles:
         ALLOCATE (COREPROF(ISLICE)%psi%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ni%value(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ne%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ti%value(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%te%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%vtor%value(NRHO,NION))


! +++ Derivatives of plasma primary profiles: (AF - 25.Sep.2014)
       ALLOCATE (COREPROF(ISLICE)%psi%ddrho(NRHO))
       ALLOCATE (COREPROF(ISLICE)%ni%ddrho(NRHO,NION))
       ALLOCATE (COREPROF(ISLICE)%ne%ddrho(NRHO))
       ALLOCATE (COREPROF(ISLICE)%ti%ddrho(NRHO,NION))
       ALLOCATE (COREPROF(ISLICE)%te%ddrho(NRHO))
       ALLOCATE (COREPROF(ISLICE)%vtor%ddrho(NRHO,NION))


! +++ Boundary conditions:
         ALLOCATE (COREPROF(ISLICE)%ni%flag(NION))
         ALLOCATE (COREPROF(ISLICE)%ti%flag(NION))
         ALLOCATE (COREPROF(ISLICE)%vtor%flag(NION))

         ALLOCATE (COREPROF(ISLICE)%ni%boundary%type(NION))
         ALLOCATE (COREPROF(ISLICE)%ti%boundary%type(NION))
         ALLOCATE (COREPROF(ISLICE)%vtor%boundary%type(NION))

         ALLOCATE (COREPROF(ISLICE)%ni%boundary%rho_tor(NION))
         ALLOCATE (COREPROF(ISLICE)%ti%boundary%rho_tor(NION))
         ALLOCATE (COREPROF(ISLICE)%vtor%boundary%rho_tor(NION))

         ALLOCATE (COREPROF(ISLICE)%psi%boundary%value(3))
         ALLOCATE (COREPROF(ISLICE)%ni%boundary%value(3,NION))
         ALLOCATE (COREPROF(ISLICE)%ne%boundary%value(3))
         ALLOCATE (COREPROF(ISLICE)%ti%boundary%value(3,NION))
         ALLOCATE (COREPROF(ISLICE)%te%boundary%value(3))
         ALLOCATE (COREPROF(ISLICE)%vtor%boundary%value(3,NION))


! +++ Profiles of transport coefficients:
         ALLOCATE (COREPROF(ISLICE)%psi%sigma_par%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ni%transp_coef%diff(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ne%transp_coef%diff(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ti%transp_coef%diff(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%te%transp_coef%diff(NRHO))
         ALLOCATE (COREPROF(ISLICE)%vtor%transp_coef%diff(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ni%transp_coef%vconv(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ne%transp_coef%vconv(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ti%transp_coef%vconv(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%te%transp_coef%vconv(NRHO))
         ALLOCATE (COREPROF(ISLICE)%vtor%transp_coef%vconv(NRHO,NION))


! +++ Flux profiles:
         ALLOCATE (COREPROF(ISLICE)%ni%flux%flux_dv(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ne%flux%flux_dv(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ti%flux%flux_dv(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%te%flux%flux_dv(NRHO))
         ALLOCATE (COREPROF(ISLICE)%vtor%flux%flux_dv(NRHO,NION))


! +++ Source profiles:
         ALLOCATE (COREPROF(ISLICE)%psi%jni%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ni%source_term%value(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ni%source_term%integral(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ne%source_term%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ne%source_term%integral(NRHO))
         ALLOCATE (COREPROF(ISLICE)%ti%source_term%value(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%ti%source_term%integral(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%te%source_term%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%te%source_term%integral(NRHO))
         ALLOCATE (COREPROF(ISLICE)%vtor%source_term%value(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%vtor%source_term%integral(NRHO,NION))


! +++ Plasma profiles 1-D:
         ALLOCATE (COREPROF(ISLICE)%profiles1d%pr_th%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%pr_perp%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%pr_parallel%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%pe%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%pi_tot%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%pi%value(NRHO,NION))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%jtot%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%jni%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%jphi%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%joh%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%q%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%vloop%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%sigmapar%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%qoh%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%qoh%integral(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%eparallel%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%shear%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%bpol%value(NRHO))
         ALLOCATE (COREPROF(ISLICE)%profiles1d%zeff%value(NRHO))


! +++ Zero out arrays
         COREPROF(ISLICE)%codeparam%codename              = '-999999999'
         COREPROF(ISLICE)%codeparam%codeversion           = '-999999999'
         COREPROF(ISLICE)%codeparam%output_diag           = '-999999999'
         COREPROF(ISLICE)%codeparam%output_flag           = -999999999

         COREPROF(ISLICE)%rho_tor(:)                      = 0.0_R8
         COREPROF(ISLICE)%rho_tor_norm(:)                 = 0.0_R8

         COREPROF(ISLICE)%psi%flag                        = 0
         COREPROF(ISLICE)%ne%flag                         = 0
         COREPROF(ISLICE)%te%flag                         = 0
         COREPROF(ISLICE)%ni%flag(:)                      = 0
         COREPROF(ISLICE)%ti%flag(:)                      = 0
         COREPROF(ISLICE)%vtor%flag(:)                    = 0

         COREPROF(ISLICE)%psi%boundary%type               = 0
         COREPROF(ISLICE)%ne%boundary%type                = 0
         COREPROF(ISLICE)%te%boundary%type                = 0
         COREPROF(ISLICE)%ni%boundary%type(:)             = 0
         COREPROF(ISLICE)%ti%boundary%type(:)             = 0
         COREPROF(ISLICE)%vtor%boundary%type(:)           = 0

         COREPROF(ISLICE)%psi%boundary%rho                = 0.0_R8
         COREPROF(ISLICE)%ni%boundary%rho_tor             = 0.0_R8
         COREPROF(ISLICE)%ne%boundary%rho_tor             = 0.0_R8
         COREPROF(ISLICE)%ti%boundary%rho_tor             = 0.0_R8
         COREPROF(ISLICE)%te%boundary%rho_tor             = 0.0_R8
         COREPROF(ISLICE)%vtor%boundary%rho_tor           = 0.0_R8


         COREPROF(ISLICE)%psi%boundary%value(:)           = 0.0_R8
         COREPROF(ISLICE)%ni%boundary%value(:,:)          = 0.0_R8
         COREPROF(ISLICE)%ne%boundary%value(:)            = 0.0_R8
         COREPROF(ISLICE)%ti%boundary%value(:,:)          = 0.0_R8
         COREPROF(ISLICE)%te%boundary%value(:)            = 0.0_R8
         COREPROF(ISLICE)%vtor%boundary%value(:,:)        = 0.0_R8


         COREPROF(ISLICE)%psi%value(:)                    = 0.0_R8
         COREPROF(ISLICE)%ni%value(:,:)                   = 0.0_R8
         COREPROF(ISLICE)%ne%value(:)                     = 0.0_R8
         COREPROF(ISLICE)%ti%value(:,:)                   = 0.0_R8
         COREPROF(ISLICE)%te%value(:)                     = 0.0_R8
         COREPROF(ISLICE)%vtor%value(:,:)                 = 0.0_R8


         COREPROF(ISLICE)%psi%sigma_par%value             = 0.0_R8
         COREPROF(ISLICE)%profiles1d%q%value(:)           = 0.0_R8
         COREPROF(ISLICE)%profiles1d%jtot%value(:)        = 0.0_R8
         COREPROF(ISLICE)%profiles1d%jni%value(:)         = 0.0_R8
         COREPROF(ISLICE)%profiles1d%jphi%value(:)        = 0.0_R8
         COREPROF(ISLICE)%profiles1d%joh%value(:)         = 0.0_R8
         COREPROF(ISLICE)%profiles1d%vloop%value(:)       = 0.0_R8
         COREPROF(ISLICE)%profiles1d%sigmapar%value(:)    = 0.0_R8
         COREPROF(ISLICE)%profiles1d%qoh%value(:)         = 0.0_R8
         COREPROF(ISLICE)%profiles1d%qoh%integral(:)      = 0.0_R8
         COREPROF(ISLICE)%profiles1d%eparallel%value(:)   = 0.0_R8
         COREPROF(ISLICE)%profiles1d%pr_th%value(:)       = 0.0_R8
         COREPROF(ISLICE)%profiles1d%pr_perp%value(:)     = 0.0_R8
         COREPROF(ISLICE)%profiles1d%pr_parallel%value(:) = 0.0_R8
         COREPROF(ISLICE)%profiles1d%pe%value(:)          = 0.0_R8
         COREPROF(ISLICE)%profiles1d%pi_tot%value(:)      = 0.0_R8
         COREPROF(ISLICE)%profiles1d%pi%value(:,:)        = 0.0_R8
         COREPROF(ISLICE)%ni%flux%flux_dv(:,:)            = 0.0_R8
         COREPROF(ISLICE)%ne%flux%flux_dv(:)              = 0.0_R8
         COREPROF(ISLICE)%ti%flux%flux_dv(:,:)            = 0.0_R8
         COREPROF(ISLICE)%te%flux%flux_dv(:)              = 0.0_R8
         COREPROF(ISLICE)%vtor%flux%flux_dv(:,:)          = 0.0_R8
         COREPROF(ISLICE)%ni%transp_coef%diff(:,:)        = 0.0_R8
         COREPROF(ISLICE)%ne%transp_coef%diff(:)          = 0.0_R8
         COREPROF(ISLICE)%ti%transp_coef%diff(:,:)        = 0.0_R8
         COREPROF(ISLICE)%te%transp_coef%diff(:)          = 0.0_R8
         COREPROF(ISLICE)%vtor%transp_coef%diff(:,:)      = 0.0_R8
         COREPROF(ISLICE)%ni%transp_coef%vconv(:,:)       = 0.0_R8
         COREPROF(ISLICE)%ne%transp_coef%vconv(:)         = 0.0_R8
         COREPROF(ISLICE)%ti%transp_coef%vconv(:,:)       = 0.0_R8
         COREPROF(ISLICE)%te%transp_coef%vconv(:)         = 0.0_R8
         COREPROF(ISLICE)%vtor%transp_coef%vconv(:,:)     = 0.0_R8
         COREPROF(ISLICE)%profiles1d%bpol%value(:)        = 0.0_R8
         COREPROF(ISLICE)%profiles1d%zeff%value(:)        = 0.0_R8


         COREPROF(ISLICE)%psi%jni%value(:)               = 0.0_R8
         COREPROF(ISLICE)%ni%source_term%value(:,:)      = 0.0_R8
         COREPROF(ISLICE)%ni%source_term%integral(:,:)   = 0.0_R8
         COREPROF(ISLICE)%ne%source_term%value(:)        = 0.0_R8
         COREPROF(ISLICE)%ne%source_term%integral(:)     = 0.0_R8
         COREPROF(ISLICE)%ti%source_term%value(:,:)      = 0.0_R8
         COREPROF(ISLICE)%ti%source_term%integral(:,:)   = 0.0_R8
         COREPROF(ISLICE)%te%source_term%value(:)        = 0.0_R8
         COREPROF(ISLICE)%te%source_term%integral(:)     = 0.0_R8
         COREPROF(ISLICE)%vtor%source_term%value(:,:)    = 0.0_R8
         COREPROF(ISLICE)%vtor%source_term%integral(:,:) = 0.0_R8


      END DO


      CALL DEALLOCATE_CPO(COMPOSITIONS)


      RETURN

    END SUBROUTINE ALLOCATE_COREPROF_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates CORETRANSP CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    SUBROUTINE ALLOCATE_CORETRANSP_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  CORETRANSP)

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
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE, ISLICE      !number of slices and slice index
      INTEGER                              :: NRHO                !number of radial points    (input)
      INTEGER                              :: NNUCL,    INUCL     !number of nuclei species
      INTEGER                              :: NION,     IION      !number of ion species
      INTEGER                              :: NIMP,     IIMP      !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)            !number of ionization states for each impurity
      INTEGER                              ::           IZIMP       
      INTEGER                              :: NNEUT,    INEUT     !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)            !number of types for each neutral
      INTEGER                              ::           ITYPE       

      TYPE (TYPE_CORETRANSP),      POINTER :: CORETRANSP(:)       !CPO with transport coefficients
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS

      ALLOCATE (CORETRANSP(NSLICE))
      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)



      DO ISLICE = 1,NSLICE

         CALL deallocate_cpo(CORETRANSP(ISLICE)%compositions)
         CALL COPY_CPO      (COMPOSITIONS, CORETRANSP(ISLICE)%compositions)


         ALLOCATE(CORETRANSP(ISLICE)%values(1))



! +++ Codeparam:
         ALLOCATE (CORETRANSP(ISLICE)%codeparam%codename(1))
         ALLOCATE (CORETRANSP(ISLICE)%codeparam%codeversion(1))
         ALLOCATE (CORETRANSP(ISLICE)%codeparam%output_diag(1))


! +++ Radial coordinate:
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%rho_tor(NRHO))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%rho_tor_norm(NRHO))

         CORETRANSP(ISLICE)%VALUES(1)%rho_tor(:)      = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%rho_tor_norm(:) = 0.0_R8




! +++ Profiles of transport coefficients:
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%sigma(NRHO))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%ne_transp%diff_eff(NRHO,3))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%ne_transp%vconv_eff(NRHO,3))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%ni_transp%diff_eff(NRHO,NION,3))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%ni_transp%vconv_eff(NRHO,NION,3))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%ti_transp%diff_eff(NRHO,NION))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%ti_transp%vconv_eff(NRHO,NION))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%te_transp%diff_eff(NRHO))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%te_transp%vconv_eff(NRHO))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%vtor_transp%diff_eff(NRHO,NION))
         ALLOCATE (CORETRANSP(ISLICE)%values(1)%vtor_transp%vconv_eff(NRHO,NION))


         CORETRANSP(ISLICE)%codeparam%codename                       = '-999999999'
         CORETRANSP(ISLICE)%codeparam%codeversion                    = '-999999999'
         CORETRANSP(ISLICE)%codeparam%output_diag                    = '-999999999'
         CORETRANSP(ISLICE)%codeparam%output_flag                    =  -999999999

         CORETRANSP(ISLICE)%VALUES(1)%sigma(:)                       = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%ne_transp%diff_eff(:,:)        = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%ne_transp%vconv_eff(:,:)       = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%ni_transp%diff_eff(:,:,:)      = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%ni_transp%vconv_eff(:,:,:)     = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%ti_transp%diff_eff(:,:)        = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%ti_transp%vconv_eff(:,:)       = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%te_transp%diff_eff(:)          = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%te_transp%vconv_eff(:)         = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%vtor_transp%diff_eff(:,:)      = 0.0_R8
         CORETRANSP(ISLICE)%VALUES(1)%vtor_transp%vconv_eff(:,:)     = 0.0_R8

         IF(NIMP.GE.1) THEN
            ALLOCATE (CORETRANSP(ISLICE)%values(1)%nz_transp(NIMP))
            ALLOCATE (CORETRANSP(ISLICE)%values(1)%tz_transp(NIMP))

            DO iimp=1, NIMP
               ALLOCATE (CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%diff_eff(NRHO,nzimp(iimp)))
               ALLOCATE (CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%vconv_eff(NRHO,nzimp(iimp)))
               ALLOCATE (CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%diff_eff(NRHO,nzimp(iimp)))
               ALLOCATE (CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%vconv_eff(NRHO,nzimp(iimp)))


               CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%diff_eff(:,:) = 0.0_R8
               CORETRANSP(ISLICE)%values(1)%nz_transp(iimp)%vconv_eff(:,:) = 0.0_R8
               CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%diff_eff(:,:) = 0.0_R8
               CORETRANSP(ISLICE)%values(1)%tz_transp(iimp)%vconv_eff(:,:) = 0.0_R8
            ENDDO
         ENDIF

      END DO


      CALL DEALLOCATE_CPO(COMPOSITIONS)


      RETURN

  END SUBROUTINE ALLOCATE_CORETRANSP_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates CORESOURCE CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    SUBROUTINE ALLOCATE_CORESOURCE_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  CORESOURCE)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates CORESOURCE CPO             !
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
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE, ISLICE      !number of slices and slice index
      INTEGER                              :: NRHO                !number of radial points    (input)
      INTEGER                              :: NNUCL,    INUCL     !number of nuclei species
      INTEGER                              :: NION,     IION      !number of ion species
      INTEGER                              :: NIMP,     IIMP      !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)            !number of ionization states for each impurity
      INTEGER                              ::           IZIMP      
      INTEGER                              :: NNEUT,    INEUT     !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)            !number of types for each neutral
      INTEGER                              ::           ITYPE       

      TYPE (TYPE_CORESOURCE),      POINTER :: CORESOURCE(:)       !CPO with sources
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS



      ALLOCATE (CORESOURCE(NSLICE))
      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)


      DO ISLICE = 1,NSLICE

         CALL deallocate_cpo(CORESOURCE(ISLICE)%compositions)
         CALL COPY_CPO(COMPOSITIONS, CORESOURCE(ISLICE)%compositions)


         ALLOCATE(CORESOURCE(ISLICE)%values(1))

! +++ Codeparam:
         ALLOCATE (CORESOURCE(ISLICE)%codeparam%codename(1))
         ALLOCATE (CORESOURCE(ISLICE)%codeparam%codeversion(1))
         ALLOCATE (CORESOURCE(ISLICE)%codeparam%output_diag(1))


! +++ Radial coordinate:
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%rho_tor(NRHO))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%rho_tor_norm(NRHO))

         CORESOURCE(ISLICE)%VALUES(1)%rho_tor(:)      = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%rho_tor_norm(:) = 0.0_R8


! +++ Profiles of sources:
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sigma(NRHO))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%j(NRHO))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%se%exp(NRHO))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%se%imp(NRHO))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qe%exp(NRHO))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qe%imp(NRHO))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%si%exp(NRHO,NION))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%si%imp(NRHO,NION)) 
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qi%exp(NRHO,NION))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qi%imp(NRHO,NION))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%ui%exp(NRHO,NION))
         ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%ui%imp(NRHO,NION))


         CORESOURCE(ISLICE)%codeparam%codename        = '-999999999'
         CORESOURCE(ISLICE)%codeparam%codeversion     = '-999999999'
         CORESOURCE(ISLICE)%codeparam%output_diag     = '-999999999'
         CORESOURCE(ISLICE)%codeparam%output_flag     =  -999999999

         CORESOURCE(ISLICE)%VALUES(1)%sigma(:)        = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%j(:)            = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%se%exp(:)       = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%se%imp(:)       = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%qe%exp(:)       = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%qe%imp(:)       = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%si%exp(:,:)     = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%si%imp(:,:)     = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%qi%exp(:,:)     = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%qi%imp(:,:)     = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%ui%exp(:,:)     = 0.0_R8
         CORESOURCE(ISLICE)%VALUES(1)%ui%imp(:,:)     = 0.0_R8


         IF(NIMP.GE.1) THEN
            ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sz(NIMP))
            ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qz(NIMP))

            DO iimp=1, NIMP
               ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sz(iimp)%exp(NRHO,nzimp(iimp)))
               ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%sz(iimp)%imp(NRHO,nzimp(iimp)))
               ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qz(iimp)%exp(NRHO,nzimp(iimp)))
               ALLOCATE (CORESOURCE(ISLICE)%VALUES(1)%qz(iimp)%imp(NRHO,nzimp(iimp)))


               CORESOURCE(ISLICE)%VALUES(1)%sz(iimp)%exp(:,:)   = 0.0_R8
               CORESOURCE(ISLICE)%VALUES(1)%sz(iimp)%imp(:,:)   = 0.0_R8
               CORESOURCE(ISLICE)%VALUES(1)%qz(iimp)%exp(:,:)   = 0.0_R8
               CORESOURCE(ISLICE)%VALUES(1)%qz(iimp)%imp(:,:)   = 0.0_R8
            ENDDO

         ENDIF

      END DO


      CALL DEALLOCATE_CPO(COMPOSITIONS)


      RETURN

    END SUBROUTINE ALLOCATE_CORESOURCE_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates COREIMPUR CPO
!>
!> \author I.Ivanova-Stanik
!>
!> \version "$Id: allocate_deallocate.f90 471 2010-07-05"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

      SUBROUTINE ALLOCATE_COREIMPUR_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREIMPUR)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COREIMPUR CPO              !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   I.Ivanova-Stanik                    !
!     Kontacts:     irena@ifpilm.waw.pl                 !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


      USE EUITM_SCHEMAS
      USE ITM_TYPES
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE, ISLICE        !number of slices and slice index
      INTEGER                              :: NRHO                  !number of radial points              (input)
      INTEGER                              :: NNUCL,    INUCL       !number of nuclei species
      INTEGER                              :: NION,     IION        !number of ion species
      INTEGER                              :: NIMP,     IIMP        !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                              ::           IZIMP       
      INTEGER                              :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
      INTEGER                              ::           ITYPE       

      TYPE (TYPE_COREIMPUR),       POINTER :: COREIMPUR(:)          !CPO with impurities
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS



      ALLOCATE (COREIMPUR(NSLICE))
      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)



      DO ISLICE = 1,NSLICE

         CALL deallocate_cpo (COREIMPUR(ISLICE)%compositions)
         CALL COPY_CPO       (COMPOSITIONS, COREIMPUR(ISLICE)%compositions)


         ALLOCATE (COREIMPUR(ISLICE)%codeparam%codename(1))
         ALLOCATE (COREIMPUR(ISLICE)%codeparam%codeversion(1))
         ALLOCATE (COREIMPUR(ISLICE)%codeparam%output_diag(1))

         COREIMPUR(ISLICE)%codeparam%codename      = '-999999999'
         COREIMPUR(ISLICE)%codeparam%codeversion   = '-999999999'
         COREIMPUR(ISLICE)%codeparam%output_diag   = '-999999999'
         COREIMPUR(ISLICE)%codeparam%output_flag   =  -999999999


         ALLOCATE (COREIMPUR(ISLICE)%rho_tor(NRHO))
         ALLOCATE (COREIMPUR(ISLICE)%rho_tor_norm(NRHO))

         COREIMPUR(ISLICE)%rho_tor(:)              = 0.0_R8
         COREIMPUR(ISLICE)%rho_tor_norm(:)         = 0.0_R8

      END DO

      IF(NIMP.EQ.0) RETURN

      DO ISLICE = 1,NSLICE

         ALLOCATE (COREIMPUR(ISLICE)%impurity(NIMP))

         DO IIMP = 1, NIMP

            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%z(NRHO,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%zsq(NRHO,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%nz(NRHO,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%boundary%value(3,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%boundary%type(NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%boundary%rho(NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%transp_coef%diff(NRHO,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%transp_coef%vconv(NRHO,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%source_term%value(NRHO,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%flux%flux_dv(NRHO,NZIMP(IIMP)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%flux%flux_interp(NRHO,NZIMP(IIMP)))


            COREIMPUR(ISLICE)%impurity(iimp)%z(:,:)                = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%zsq(:,:)              = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%nz(:,:)               = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%boundary%value(:,:)   = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%boundary%type(:)      = 0
            COREIMPUR(ISLICE)%impurity(iimp)%boundary%rho(:)       = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%transp_coef%diff(:,:) = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%transp_coef%vconv(:,:)= 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%source_term%value(:,:)= 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%flux%flux_dv(:,:)     = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%flux%flux_interp(:,:) = 0.0_R8


            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%line_rad%profile(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%line_rad%integral(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%brem_radrec%profile(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%brem_radrec%integral(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%sum%profile(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%sum%integral(nrho,nzimp(iimp)))


            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%line_rad%profile(:,:)     = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%line_rad%integral(:,:)    = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%brem_radrec%profile(:,:)  = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%brem_radrec%integral(:,:) = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%sum%profile(:,:)          = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%radiation%sum%integral(:,:)         = 0.0_R8
           

            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%ionization%profile(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%ionization%integral(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%recombin%profile(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%recombin%integral(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%sum%profile(nrho,nzimp(iimp)))
            ALLOCATE (COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%sum%integral(nrho,nzimp(iimp)))


            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%ionization%profile(:,:)  = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%ionization%integral(:,:) = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%recombin%profile(:,:)    = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%recombin%integral(:,:)   = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%sum%profile(:,:)         = 0.0_R8
            COREIMPUR(ISLICE)%impurity(iimp)%diagnostic%energy%sum%integral(:,:)        = 0.0_R8

         ENDDO

        ALLOCATE (COREIMPUR(ISLICE)%diagnostic%radiation%line_rad%profile(NRHO,NIMP))
        ALLOCATE (COREIMPUR(ISLICE)%diagnostic%radiation%line_rad%integral(NRHO,NIMP))
	ALLOCATE (COREIMPUR(ISLICE)%diagnostic%radiation%brem_radrec%profile(NRHO,NIMP))
        ALLOCATE (COREIMPUR(ISLICE)%diagnostic%radiation%brem_radrec%integral(NRHO,NIMP))
	ALLOCATE (COREIMPUR(ISLICE)%diagnostic%radiation%sum%profile(NRHO,NIMP))
        ALLOCATE (COREIMPUR(ISLICE)%diagnostic%radiation%sum%integral(NRHO,NIMP))


	COREIMPUR(ISLICE)%diagnostic%radiation%line_rad%profile(:,:)     = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%radiation%line_rad%integral(:,:)    = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%radiation%brem_radrec%profile(:,:)  = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%radiation%brem_radrec%integral(:,:) = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%radiation%sum%profile(:,:)          = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%radiation%sum%integral(:,:)         = 0.0_R8

	
	ALLOCATE (COREIMPUR(ISLICE)%diagnostic%energy%ionization%profile(NRHO,NIMP))
        ALLOCATE (COREIMPUR(ISLICE)%diagnostic%energy%ionization%integral(NRHO,NIMP))
	ALLOCATE (COREIMPUR(ISLICE)%diagnostic%energy%recombin%profile(NRHO,NIMP))
        ALLOCATE (COREIMPUR(ISLICE)%diagnostic%energy%recombin%integral(NRHO,NIMP))
	ALLOCATE (COREIMPUR(ISLICE)%diagnostic%energy%sum%profile(NRHO,NIMP))
        ALLOCATE (COREIMPUR(ISLICE)%diagnostic%energy%sum%integral(NRHO,NIMP))


	COREIMPUR(ISLICE)%diagnostic%energy%ionization%profile(:,:)  = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%energy%ionization%integral(:,:) = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%energy%recombin%profile(:,:)    = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%energy%recombin%integral(:,:)   = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%energy%sum%profile(:,:)         = 0.0_R8
	COREIMPUR(ISLICE)%diagnostic%energy%sum%integral(:,:)        = 0.0_R8

	
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%radiation%line_rad%profile(NRHO))
        ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%radiation%line_rad%integral(NRHO))
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%radiation%brem_radrec%profile(NRHO))
        ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%radiation%brem_radrec%integral(NRHO))
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%radiation%sum%profile(NRHO))
        ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%radiation%sum%integral(NRHO))


	COREIMPUR(ISLICE)%diagnosticsum%radiation%line_rad%profile(:)     = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%radiation%line_rad%integral(:)    = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%radiation%brem_radrec%profile(:)  = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%radiation%brem_radrec%integral(:) = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%radiation%sum%profile(:)          = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%radiation%sum%integral(:)         = 0.0_R8

	
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%energy%ionization%profile(NRHO))
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%energy%ionization%integral(NRHO))
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%energy%recombin%profile(NRHO))
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%energy%recombin%integral(NRHO))
	ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%energy%sum%profile(NRHO))
        ALLOCATE (COREIMPUR(ISLICE)%diagnosticsum%energy%sum%integral(NRHO))


	COREIMPUR(ISLICE)%diagnosticsum%energy%ionization%profile(:)  = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%energy%ionization%integral(:) = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%energy%recombin%profile(:)    = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%energy%recombin%integral(:)   = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%energy%sum%profile(:)         = 0.0_R8
	COREIMPUR(ISLICE)%diagnosticsum%energy%sum%integral(:)        = 0.0_R8
 



      END DO


      CALL DEALLOCATE_CPO(COMPOSITIONS)


      RETURN

    END SUBROUTINE ALLOCATE_COREIMPUR_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

      SUBROUTINE ALLOCATE_CORENEUTRALS_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORENEUTRALS)
!-------------------------------------------------------!  
!                                                       !
!     This routine allocates CORENEUTRALS CPO           !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   I.Ivanova-Stanik,R.Stankiewicz      !
!                   D.Kalupin                           !
!     Kontacts:     irena@ifpilm.waw.pl                 !
!                   romsta@ifpilm.waw.pl                !
!                   denis.kalupin@efda.org              !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


      USE EUITM_SCHEMAS
      USE ITM_TYPES
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE, ISLICE        !number of slices and slice index
      INTEGER                              :: NRHO                  !number of radial points              (input)
      INTEGER                              :: NNUCL,    INUCL       !number of nuclei species
      INTEGER                              :: NION,     IION        !number of ion species
      INTEGER                              :: NIMP,     IIMP        !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                              ::           IZIMP       
      INTEGER                              :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
      INTEGER                              ::           ITYPE       

      TYPE (TYPE_CORENEUTRALS),    POINTER :: CORENEUTRALS(:)       !CPO with impurities
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS





      ALLOCATE (CORENEUTRALS(NSLICE))
      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)




      DO ISLICE = 1,NSLICE

         CALL deallocate_cpo(CORENEUTRALS(ISLICE)%compositions)
         CALL COPY_CPO(COMPOSITIONS, CORENEUTRALS(ISLICE)%compositions)

         ALLOCATE (CORENEUTRALS(ISLICE)%codeparam%codename(1))
         ALLOCATE (CORENEUTRALS(ISLICE)%codeparam%codeversion(1))
         ALLOCATE (CORENEUTRALS(ISLICE)%codeparam%output_diag(1))

         CORENEUTRALS(ISLICE)%codeparam%codename      = '-999999999'
         CORENEUTRALS(ISLICE)%codeparam%codeversion   = '-999999999'
         CORENEUTRALS(ISLICE)%codeparam%output_diag   = '-999999999'
         CORENEUTRALS(ISLICE)%codeparam%output_flag   =  -999999999


         ALLOCATE (CORENEUTRALS(ISLICE)%rho_tor(NRHO))
         ALLOCATE (CORENEUTRALS(ISLICE)%rho_tor_norm(NRHO))

         CORENEUTRALS(ISLICE)%rho_tor(:)                                 = 0.0_R8	
         CORENEUTRALS(ISLICE)%rho_tor_norm(:)                            = 0.0_R8	

      END DO


      IF(NNEUT.LE.0) RETURN

      DO ISLICE = 1,NSLICE

         IF(NNEUT.GT.0) THEN
            ALLOCATE (CORENEUTRALS(ISLICE)%profiles(NNEUT))
            DO ineut = 1, NNEUT
               ALLOCATE (CORENEUTRALS(ISLICE)%profiles(INEUT)%neutraltype(NTYPE(INEUT)))
               ALLOCATE (CORENEUTRALS(ISLICE)%profiles(INEUT)%prad0(NRHO))

               CORENEUTRALS(ISLICE)%profiles(ineut)%prad0                                               = 0.0_R8


               DO itype = 1, ntype(ineut)
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%value(NRHO))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%flux(NRHO))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%value(3))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%value(NRHO))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%flux(NRHO))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%value(3))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%value(NRHO))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%value(3))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%value(NRHO))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%value(3))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%value(NRHO))
                  ALLOCATE (CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%value(3))


                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%value(:)                   = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%flux(:)                    = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%value(:)          = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%type              = 0
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%n0%boundary%rho_tor           = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%value(:)                   = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%flux(:)                    = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%value(:)          = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%type              = 0
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%t0%boundary%rho_tor           = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%value(:)          = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%value(:) = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%type     = 0
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%toroidal%boundary%rho_tor  = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%value(:)          = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%value(:) = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%type     = 0
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%poloidal%boundary%rho_tor  = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%value(:)            = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%value(:)   = 0.0_R8
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%type       = 0
                  CORENEUTRALS(ISLICE)%profiles(ineut)%neutraltype(itype)%v0%radial%boundary%rho_tor    = 0.0_R8
               ENDDO
            ENDDO
         ENDIF
      END DO

      CALL DEALLOCATE_CPO (COMPOSITIONS)

      RETURN

      END SUBROUTINE ALLOCATE_CORENEUTRALS_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates COREDELTA CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    SUBROUTINE ALLOCATE_COREDELTA_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  COREDELTA)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COREDELTA CPO              !
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
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE, ISLICE        !number of slices and slice index
      INTEGER                              :: NRHO                  !number of radial points    (input)
      INTEGER                              :: NNUCL,    INUCL       !number of nuclei species
      INTEGER                              :: NION,     IION        !number of ion species
      INTEGER                              :: NIMP,     IIMP        !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                              ::           IZIMP       
      INTEGER                              :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
      INTEGER                              ::           ITYPE       

      TYPE (TYPE_COREDELTA),       POINTER :: COREDELTA(:)          !CPO with deltas
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS



      ALLOCATE (COREDELTA(NSLICE))
      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)


      DO ISLICE = 1,NSLICE

         CALL DEALLOCATE_CPO(COREDELTA(ISLICE)%compositions)
         CALL COPY_CPO      (COMPOSITIONS, COREDELTA(ISLICE)%compositions)


         ALLOCATE(COREDELTA(ISLICE)%VALUES(1))

! +++ Codeparam:
         ALLOCATE (COREDELTA(ISLICE)%codeparam%codename(1))
         ALLOCATE (COREDELTA(ISLICE)%codeparam%codeversion(1))
         ALLOCATE (COREDELTA(ISLICE)%codeparam%output_diag(1))


! +++ Radial coordinate:
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%rho_tor(NRHO))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%rho_tor_norm(NRHO))

         COREDELTA(ISLICE)%VALUES(1)%rho_tor(:)      = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%rho_tor_norm(:) = 0.0_R8


! +++ Profiles of deltas:
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%psi(NRHO))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%volume(NRHO))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%area(NRHO))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%delta_psi(NRHO))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%delta_te(NRHO))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%delta_ne(NRHO))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%delta_ti(NRHO,NION))
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%delta_ni(NRHO,NION)) 
         ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%delta_vtor(NRHO,NION))
         IF (NIMP.GE.1) THEN
            ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%IMPURITY(NIMP))
            DO IIMP = 1, NIMP
               ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%IMPURITY(IIMP)%delta_nz(NRHO,NZIMP(IIMP)))
               ALLOCATE (COREDELTA(ISLICE)%VALUES(1)%IMPURITY(IIMP)%delta_tz(NRHO,NZIMP(IIMP)))
               COREDELTA(ISLICE)%VALUES(1)%IMPURITY(IIMP)%delta_nz = 0.0_R8
               COREDELTA(ISLICE)%VALUES(1)%IMPURITY(IIMP)%delta_tz = 0.0_R8
            END DO
         END IF


         COREDELTA(ISLICE)%codeparam%codename        = '-999999999'
         COREDELTA(ISLICE)%codeparam%codeversion     = '-999999999'
         COREDELTA(ISLICE)%codeparam%output_diag     = '-999999999'
         COREDELTA(ISLICE)%codeparam%output_flag     =  -999999999

         COREDELTA(ISLICE)%VALUES(1)%psi             = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%volume          = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%area            = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%delta_psi       = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%delta_te        = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%delta_ne        = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%delta_ti        = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%delta_ni        = 0.0_R8
         COREDELTA(ISLICE)%VALUES(1)%delta_vtor      = 0.0_R8


      END DO


      CALL DEALLOCATE_CPO(COMPOSITIONS)


      RETURN

    END SUBROUTINE ALLOCATE_COREDELTA_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates EQUILIBRIUM CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ALLOCATE_EQUILIBRIUM_CPO (NSLICE, NPSI, NDIM1, NDIM2, NPOINTS,   EQUILIBRIUM)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates EQUILIBRIUM CPO            !
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
    INTEGER                          :: NPSI                !number of points for equilibrium 1-D arrays
    INTEGER                          :: NDIM1               !number of points for equilibrium 2-D arrays, first dimension
    INTEGER                          :: NDIM2               !number of points for equilibrium 2-D arrays, second dimension
    INTEGER                          :: NPOINTS             !number of points for equilibrium boundary 

    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)      !CPO with geometry quantities



    ALLOCATE (EQUILIBRIUM(NSLICE))


    DO ISLICE = 1,NSLICE

! +++ PROFILES_1D:
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%rho_tor(NPSI)) 
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%q(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%pressure(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%jparallel(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%jphi(NPSI))

       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm1(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm2(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm3(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm4(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm5(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm6(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm7(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm8(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%gm9(NPSI))

       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%volume(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%vprime(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%area(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%aprime(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%F_dia(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%pprime(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%ffprime(NPSI))

       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%elongation(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%tria_upper(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%tria_lower(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%r_inboard(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%r_outboard(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%dpsidrho_tor(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%beta_pol(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%li(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%dvdrho(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%surface(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%ftrap(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%b_av(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%b_min(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%b_max(NPSI))

       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%phi(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%psi(NPSI))
       ALLOCATE (EQUILIBRIUM(ISLICE)%profiles_1d%rho_vol(NPSI))


       EQUILIBRIUM(ISLICE)%profiles_1d%rho_tor(:)          = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%q(:)                = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%pressure(:)         = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%jparallel(:)        = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%jphi(:)             = 0.0_R8

       EQUILIBRIUM(ISLICE)%profiles_1d%phi(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%psi(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%rho_vol(:)          = 0.0_R8

       EQUILIBRIUM(ISLICE)%profiles_1d%gm1(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm2(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm3(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm4(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm5(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm6(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm7(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm8(:)              = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%gm9(:)              = 0.0_R8

       EQUILIBRIUM(ISLICE)%profiles_1d%volume(:)           = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%vprime(:)           = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%area(:)             = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%aprime(:)           = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%F_dia(:)            = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%pprime(:)           = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%ffprime(:)          = 0.0_R8

       EQUILIBRIUM(ISLICE)%profiles_1d%elongation(:)       = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%tria_upper(:)       = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%tria_lower(:)       = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%r_inboard(:)        = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%r_outboard(:)       = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%dpsidrho_tor(:)     = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%beta_pol(:)         = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%li(:)               = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%dvdrho(:)           = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%surface(:)          = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%ftrap(:)            = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%b_av(:)             = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%b_min(:)            = 0.0_R8
       EQUILIBRIUM(ISLICE)%profiles_1d%b_max(:)            = 0.0_R8


! +++ PROFILES_2D:
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%grid%dim1(NDIM1))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%grid%dim2(NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%r(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%z(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%psi(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%theta(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%jphi(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%jpar(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%br(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%bz(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%bphi(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%vphi(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%vtheta(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%rho_mass(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%pressure(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%profiles_2d(1)%temperature(NDIM1, NDIM2))

       EQUILIBRIUM(1)%profiles_2d(1)%grid%dim1(:)           = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%grid%dim2(:)           = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%r(:,:)                 = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%z(:,:)                 = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%psi(:,:)               = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%theta(:,:)             = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%jphi(:,:)              = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%jpar(:,:)              = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%br(:,:)                = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%bz(:,:)                = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%bphi(:,:)              = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%vphi(:,:)              = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%vtheta(:,:)            = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%rho_mass(:,:)          = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%pressure(:,:)          = 0.0_R8
       EQUILIBRIUM(1)%profiles_2d(1)%temperature(:,:)       = 0.0_R8



! +++ COORD_SYS:
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%grid%dim1(NDIM1))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%grid%dim2(NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%g_11(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%g_12(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%g_13(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%g_22(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%g_23(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%g_33(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%position%R(NDIM1, NDIM2))
       ALLOCATE(EQUILIBRIUM(1)%coord_sys%position%Z(NDIM1, NDIM2))

       EQUILIBRIUM(1)%coord_sys%grid%dim1(:)               = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%grid%dim2(:)               = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%g_11(:,:)                  = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%g_12(:,:)                  = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%g_13(:,:)                  = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%g_22(:,:)                  = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%g_23(:,:)                  = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%g_33(:,:)                  = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%position%R(:,:)            = 0.0_R8
       EQUILIBRIUM(1)%coord_sys%position%Z(:,:)            = 0.0_R8



! +++ EQGEOMETRY:
       ALLOCATE(EQUILIBRIUM(1)%eqgeometry%boundary(1))
       ALLOCATE(EQUILIBRIUM(1)%eqgeometry%boundary(1)%r(NPOINTS))  
       ALLOCATE(EQUILIBRIUM(1)%eqgeometry%boundary(1)%z(NPOINTS))  

       EQUILIBRIUM(1)%eqgeometry%boundary(1)%r(:)         = 0.0_R8 
       EQUILIBRIUM(1)%eqgeometry%boundary(1)%z(:)         = 0.0_R8  



    END DO

    RETURN

  END SUBROUTINE ALLOCATE_EQUILIBRIUM_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates TOROIDFIELD CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ALLOCATE_TOROIDFIELD_CPO (NSLICE, TOROIDFIELD)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates TOROIDFIELD CPO            !
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

    TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD(:)      !CPO with geometry quantities



    ALLOCATE (TOROIDFIELD(NSLICE))



    RETURN

  END SUBROUTINE ALLOCATE_TOROIDFIELD_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine allocates NEOCLASSIC CPO
!>
!> \author D.Kalupin
!>
!> \version "$Id: allocate_deallocate.f90 1756 2016-06-13 17:09:10Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
    SUBROUTINE ALLOCATE_NEOCLASSIC_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  NEOCLASSIC)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates NEOCLASSIC CPO             !
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
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE, ISLICE      !number of slices and slice index
      INTEGER                              :: NRHO                !number of radial points    (input)
      INTEGER                              :: NNUCL,    INUCL       !number of nuclei species
      INTEGER                              :: NION,     IION        !number of ion species
      INTEGER                              :: NIMP,     IIMP        !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                              ::           IZIMP       
      INTEGER                              :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
      INTEGER                              ::           ITYPE       

      TYPE (TYPE_NEOCLASSIC),      POINTER :: NEOCLASSIC(:)       !CPO with transport coefficients
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS

      ALLOCATE (NEOCLASSIC(NSLICE))
      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)



      DO ISLICE = 1,NSLICE

         CALL DEALLOCATE_CPO    (NEOCLASSIC(ISLICE)%compositions)
         CALL COPY_CPO          (COMPOSITIONS, NEOCLASSIC(ISLICE)%compositions)



! +++ Codeparam:
         ALLOCATE (NEOCLASSIC(ISLICE)%codeparam%codename(1))
         ALLOCATE (NEOCLASSIC(ISLICE)%codeparam%codeversion(1))
         ALLOCATE (NEOCLASSIC(ISLICE)%codeparam%output_diag(1))


! +++ Radial coordinate:
         ALLOCATE (NEOCLASSIC(ISLICE)%rho_tor(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%rho_tor_norm(NRHO))

         NEOCLASSIC(ISLICE)%rho_tor(:)      = 0.0_R8
         NEOCLASSIC(ISLICE)%rho_tor_norm(:) = 0.0_R8




! +++ Profiles of transport coefficients:
         ALLOCATE (NEOCLASSIC(ISLICE)%sigma(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%jboot(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%er(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%vpol(NRHO,NION))
         ALLOCATE (NEOCLASSIC(ISLICE)%ne_neo%diff_eff(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%ne_neo%vconv_eff(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%ni_neo%diff_eff(NRHO,NION))
         ALLOCATE (NEOCLASSIC(ISLICE)%ni_neo%vconv_eff(NRHO,NION))
         ALLOCATE (NEOCLASSIC(ISLICE)%ti_neo%diff_eff(NRHO,NION))
         ALLOCATE (NEOCLASSIC(ISLICE)%ti_neo%vconv_eff(NRHO,NION))
         ALLOCATE (NEOCLASSIC(ISLICE)%te_neo%diff_eff(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%te_neo%vconv_eff(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%mtor_neo%diff_eff(NRHO))
         ALLOCATE (NEOCLASSIC(ISLICE)%mtor_neo%vconv_eff(NRHO))


         NEOCLASSIC(ISLICE)%codeparam%codename                       = '-999999999'
         NEOCLASSIC(ISLICE)%codeparam%codeversion                    = '-999999999'
         NEOCLASSIC(ISLICE)%codeparam%output_diag                    = '-999999999'
         NEOCLASSIC(ISLICE)%codeparam%output_flag                    =  -999999999

         NEOCLASSIC(ISLICE)%sigma(:)                                 = 0.0_R8
         NEOCLASSIC(ISLICE)%jboot(:)                                 = 0.0_R8
         NEOCLASSIC(ISLICE)%er(:)                                    = 0.0_R8
         NEOCLASSIC(ISLICE)%vpol(:,:)                                = 0.0_R8
         NEOCLASSIC(ISLICE)%ne_neo%diff_eff(:)                       = 0.0_R8
         NEOCLASSIC(ISLICE)%ne_neo%vconv_eff(:)                      = 0.0_R8
         NEOCLASSIC(ISLICE)%ni_neo%diff_eff(:,:)                     = 0.0_R8
         NEOCLASSIC(ISLICE)%ni_neo%vconv_eff(:,:)                    = 0.0_R8
         NEOCLASSIC(ISLICE)%ti_neo%diff_eff(:,:)                     = 0.0_R8
         NEOCLASSIC(ISLICE)%ti_neo%vconv_eff(:,:)                    = 0.0_R8
         NEOCLASSIC(ISLICE)%te_neo%diff_eff(:)                       = 0.0_R8
         NEOCLASSIC(ISLICE)%te_neo%vconv_eff(:)                      = 0.0_R8
         NEOCLASSIC(ISLICE)%mtor_neo%diff_eff(:)                     = 0.0_R8
         NEOCLASSIC(ISLICE)%mtor_neo%vconv_eff(:)                    = 0.0_R8

         IF(NIMP.GT.0) THEN
            ALLOCATE (NEOCLASSIC(ISLICE)%nz_neo(NIMP))
            ALLOCATE (NEOCLASSIC(ISLICE)%tz_neo(NIMP))

            DO iimp=1, NIMP
               ALLOCATE (NEOCLASSIC(ISLICE)%nz_neo(iimp)%diff_eff(NRHO,nzimp(iimp)))
               ALLOCATE (NEOCLASSIC(ISLICE)%nz_neo(iimp)%vconv_eff(NRHO,nzimp(iimp)))
               ALLOCATE (NEOCLASSIC(ISLICE)%tz_neo(iimp)%diff_eff(NRHO,nzimp(iimp)))
               ALLOCATE (NEOCLASSIC(ISLICE)%tz_neo(iimp)%vconv_eff(NRHO,nzimp(iimp)))


               NEOCLASSIC(ISLICE)%nz_neo(iimp)%diff_eff(:,:)         = 0.0_R8
               NEOCLASSIC(ISLICE)%nz_neo(iimp)%vconv_eff(:,:)        = 0.0_R8
               NEOCLASSIC(ISLICE)%tz_neo(iimp)%diff_eff(:,:)         = 0.0_R8
               NEOCLASSIC(ISLICE)%tz_neo(iimp)%vconv_eff(:,:)        = 0.0_R8
            ENDDO
         ENDIF

      END DO


      CALL DEALLOCATE_CPO(COMPOSITIONS)


      RETURN

  END SUBROUTINE ALLOCATE_NEOCLASSIC_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

      SUBROUTINE ALLOCATE_COMPOSITIONC_CPO (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONC)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COMPOSITIONC CPO           !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   Denis Kalupin                       !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


      USE EUITM_SCHEMAS
      USE ITM_TYPES
      USE COPY_STRUCTURES
      USE DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                              :: NSLICE,   ISLICE      !number of slices and slice index
      INTEGER                              :: NNUCL                 !number of nuclei species
      INTEGER                              :: NION                  !number of ion species
      INTEGER                              :: NIMP                  !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                              :: NNEUT                 !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
      
      TYPE (TYPE_COMPOSITIONC),    POINTER :: COMPOSITIONC(:)       !CPO with impurities
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS


      ALLOCATE (COMPOSITIONC(NSLICE))


      CALL ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)


      DO ISLICE = 1,NSLICE

         CALL DEALLOCATE_CPO     (COMPOSITIONC(ISLICE)%compositions)
         CALL COPY_CPO           (COMPOSITIONS, COMPOSITIONC(ISLICE)%compositions)

      END DO


      CALL DEALLOCATE_CPO        (COMPOSITIONS)

      RETURN

      END SUBROUTINE ALLOCATE_COMPOSITIONC_CPO

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

      SUBROUTINE ALLOCATE_COMPOSITIONS (NSLICE, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONS)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COMPOSITIONC CPO           !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   Denis Kalupin                       !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


      USE EUITM_SCHEMAS
      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER                              :: NSLICE,   ISLICE      !number of slices and slice index
      INTEGER                              :: NNUCL,    INUCL       !number of nuclei species
      INTEGER                              :: NION,     IION        !number of ion species
      INTEGER                              :: NIMP,     IIMP        !number of impurity species
      INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                              ::           IZIMP       
      INTEGER                              :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER                              ::           ICOMP       
      INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
      INTEGER                              ::           ITYPE       
      
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS


! +++ Nuclei:
         ALLOCATE (COMPOSITIONS%NUCLEI(NNUCL))
         DO INUCL = 1, NNUCL
            ALLOCATE(COMPOSITIONS%NUCLEI(INUCL)%label(1))
            COMPOSITIONS%NUCLEI(INUCL)%zn                    = 0.0_R8
            COMPOSITIONS%NUCLEI(INUCL)%amn                   = 0.0_R8
            COMPOSITIONS%NUCLEI(INUCL)%label                 = " "
         END DO



! +++ Ions:
         ALLOCATE (COMPOSITIONS%IONS(NION))
         DO IION = 1, NION
            ALLOCATE(COMPOSITIONS%IONS(IION)%label(1))
            COMPOSITIONS%IONS(IION)%nucindex                 = 0
            COMPOSITIONS%IONS(IION)%zion                     = 0.0_R8
            COMPOSITIONS%IONS(IION)%imp_flag                 = 0
            COMPOSITIONS%IONS(IION)%label                    = " "
         END DO



! +++ Impurities:
         IF (NIMP.GE.1) THEN
            ALLOCATE (COMPOSITIONS%IMPURITIES(NIMP))
            DO IIMP = 1, NIMP
               COMPOSITIONS%IMPURITIES(IIMP)%nucindex           = 0
               COMPOSITIONS%IMPURITIES(IIMP)%i_ion              = 0
               COMPOSITIONS%IMPURITIES(IIMP)%nzimp              = NZIMP(IIMP)
               ALLOCATE (COMPOSITIONS%IMPURITIES(IIMP)%zmin(NZIMP(IIMP)))
               ALLOCATE (COMPOSITIONS%IMPURITIES(IIMP)%zmax(NZIMP(IIMP)))
               ALLOCATE (COMPOSITIONS%IMPURITIES(IIMP)%label(NZIMP(IIMP)))
               DO IZIMP = 1, NZIMP(IIMP)
                  COMPOSITIONS%IMPURITIES(IIMP)%zmin(IZIMP)     = 0.0_R8
                  COMPOSITIONS%IMPURITIES(IIMP)%zmax(IZIMP)     = 0.0_R8
                  COMPOSITIONS%IMPURITIES(IIMP)%label(IZIMP)    = " "
               END DO
            END DO
         END IF



! +++ Neutrals:
         IF (NNEUT.GE.1) THEN
            ALLOCATE (COMPOSITIONS%NEUTRALSCOMP(NNEUT))
            DO INEUT = 1, NNEUT
               ALLOCATE (COMPOSITIONS%NEUTRALSCOMP(INEUT)%NEUTCOMP(NCOMP(INEUT)))
               ALLOCATE (COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE(NTYPE(INEUT)))
               DO ICOMP = 1, NCOMP(INEUT)
                  COMPOSITIONS%NEUTRALSCOMP(INEUT)%NEUTCOMP(ICOMP)%nucindex     = 0
                  COMPOSITIONS%NEUTRALSCOMP(INEUT)%NEUTCOMP(ICOMP)%multiplicity = 0
               END DO
               DO ITYPE = 1, NTYPE(INEUT)
                  ALLOCATE(COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%id(1))
                  ALLOCATE(COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%description(1))
                  COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%id               = " " 
                  COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%flag             = 0
                  COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%description      = " "
               END DO
            END DO
         END IF



      RETURN

      END SUBROUTINE ALLOCATE_COMPOSITIONS

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

      SUBROUTINE GET_COMP_DIMENSIONS (COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

!-------------------------------------------------------!  
!                                                       !
!     This routine allocates COMPOSITIONC CPO           !
!                                                       !
!-------------------------------------------------------!  
!                                                       !
!     Source:       ---                                 !
!     Developers:   Denis Kalupin                       !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!  


      USE EUITM_SCHEMAS
      USE ITM_TYPES

      IMPLICIT NONE

      INTEGER, INTENT(OUT)                 :: NNUCL                 !number of nuclei species
      INTEGER, INTENT(OUT)                 :: NION                  !number of ion species
      INTEGER, INTENT(OUT)                 :: NIMP                  !number of impurity species
      INTEGER, INTENT(OUT),    ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER, INTENT(OUT)                 :: NNEUT                 !number of neutrals species
      INTEGER, INTENT(OUT),    ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER, INTENT(OUT),    ALLOCATABLE :: NTYPE(:)              !number of types for each neutral

      INTEGER                              :: IIMP        
      INTEGER                              :: INEUT        
     
      TYPE (TYPE_COMPOSITIONS_TYPE)        :: COMPOSITIONS


! +++ Nuclei:
      IF(ASSOCIATED(COMPOSITIONS%NUCLEI)) THEN
         NNUCL                  = SIZE (COMPOSITIONS%NUCLEI)
      ELSE
         NNUCL                  = 0
      ENDIF

! +++ Ions:
      IF(ASSOCIATED(COMPOSITIONS%IONS)) THEN
         NION                   = SIZE (COMPOSITIONS%IONS)
      ELSE
         NION                   = 0
      ENDIF

! +++ Impurities:
      IF(ASSOCIATED(COMPOSITIONS%IMPURITIES)) THEN
         NIMP                   = SIZE (COMPOSITIONS%IMPURITIES)
         ALLOCATE                (NZIMP(NIMP))
         NZIMP                  = 0
         DO IIMP = 1, NIMP
            NZIMP(IIMP)         = COMPOSITIONS%IMPURITIES(IIMP)%nzimp
         ENDDO
      ELSE
         NIMP                   = 0
      ENDIF

! +++ Neutrals:
      IF(ASSOCIATED(COMPOSITIONS%NEUTRALSCOMP)) THEN
         NNEUT                  = SIZE (COMPOSITIONS%NEUTRALSCOMP)
         ALLOCATE                (NTYPE(NNEUT))
         ALLOCATE                (NCOMP(NNEUT))
         NTYPE                  = 0
         NCOMP                  = 0
         DO INEUT = 1, NNEUT
            NCOMP(INEUT)        = SIZE(COMPOSITIONS%NEUTRALSCOMP(INEUT)%NEUTCOMP)
            NTYPE(INEUT)        = SIZE(COMPOSITIONS%NEUTRALSCOMP(INEUT)%TYPE)
         ENDDO
      ELSE
         NNEUT                  = 0
      ENDIF

      RETURN

      END SUBROUTINE GET_COMP_DIMENSIONS 

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
END MODULE ALLOCATE_DEALLOCATE
