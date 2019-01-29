!-------------------------------------------------------!
!-------------------------------------------------------!

  SUBROUTINE BACKGROUND_TRANSPORT        (COREPROF,              &
                                          SIGMA,                 &
                                          DIFF_NI,   VCONV_NI,   &
                                          DIFF_NE,   VCONV_NE,   &
                                          DIFF_TI,   VCONV_TI,   &
                                          DIFF_TE,   VCONV_TE,   & 
                                          DIFF_VTOR, VCONV_VTOR, &
                                          DIFF_NZ,   VCONV_NZ,   &
                                          DIFF_TZ,   VCONV_TZ,   & 
                                          CORETRANSP) 

!-------------------------------------------------------!
!     This routine is used by the ETS workflow          !
!     to fill background transport coefficients         !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     output CORETRANSP CPO is            !
!                   allocated inside the module         !
!                                                       !
!-------------------------------------------------------!

      USE  EUITM_SCHEMAS
      USE  EUITM_ROUTINES
      USE  ALLOCATE_DEALLOCATE
      USE  COPY_STRUCTURES
      USE  ITM_TYPES
      USE  DEALLOCATE_STRUCTURES

      IMPLICIT NONE

      INTEGER                           :: ifail

! +++ CPO derived types:
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF(:)         !input CPO with internal ETS parameters profiles from previous time
      TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP(:)       !output CPO with transport


! +++ Dimensions:
      INTEGER,               PARAMETER  :: NSLICE = 1          !number of CPO ocurancies in the work flow
      INTEGER                           :: NRHO                !number of radial points     (input, determined from COREPROF CPO)
      INTEGER                           :: NION, IION          !number of ion species       (input, determined from COREPROF CPO)
      INTEGER                           :: NIMP, IIMP          !number of impurities                (input)
      INTEGER                           :: NNUCL               !number of nuclei species
      INTEGER,              ALLOCATABLE :: NZIMP(:)            !number of ionization states for each impurity
      INTEGER                           :: NNEUT               !number of neutrals species
      INTEGER,              ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
      INTEGER,              ALLOCATABLE :: NTYPE(:)            !number of types for each neutral

      REAL(R8)                          :: SIGMA
      REAL(R8)                          :: DIFF_NI(30),   VCONV_NI(30)
      REAL(R8)                          :: DIFF_TI(30),   VCONV_TI(30)
      REAL(R8)                          :: DIFF_NE,       VCONV_NE
      REAL(R8)                          :: DIFF_TE,       VCONV_TE
      REAL(R8)                          :: DIFF_VTOR(30), VCONV_VTOR(30)
      REAL(R8)                          :: DIFF_NZ(100),  VCONV_NZ(100)
      REAL(R8)                          :: DIFF_TZ(100),  VCONV_TZ(100)

! +++ Set dimensions:
      NRHO                    = SIZE (COREPROF(1)%rho_tor, DIM=1)

! +++ Allocate output CPO:
      CALL GET_COMP_DIMENSIONS       (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
      CALL ALLOCATE_CORETRANSP_CPO   (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  CORETRANSP)
      call deallocate_cpo(CORETRANSP(1)%COMPOSITIONS)
      CALL COPY_CPO                  (COREPROF(1)%COMPOSITIONS, CORETRANSP(1)%COMPOSITIONS)



! +++ Save output in CPO:
      CORETRANSP(1)%time                                        =  COREPROF(1)%time     !time    [s]

      CORETRANSP(1)%VALUES(1)%rho_tor                           =  COREPROF(1)%rho_tor  !rho     [m]
      CORETRANSP(1)%VALUES(1)%rho_tor_norm                      =  COREPROF(1)%rho_tor/COREPROF(1)%rho_tor(NRHO)

      CORETRANSP(1)%VALUES(1)%sigma                             =  SIGMA
      CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,1)           =  DIFF_NE
      CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff(:,1)          =  VCONV_NE
      CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,2)           =  0.0_R8
      CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff(:,2)          =  0.0_R8
      CORETRANSP(1)%VALUES(1)%ne_transp%diff_eff(:,3)           =  0.0_R8
      CORETRANSP(1)%VALUES(1)%ne_transp%vconv_eff(:,3)          =  0.0_R8
      CORETRANSP(1)%VALUES(1)%te_transp%diff_eff(:)             =  DIFF_TE
      CORETRANSP(1)%VALUES(1)%te_transp%vconv_eff(:)            =  VCONV_TE

      DO IION = 1, NION
         CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(:,IION,1)   =  DIFF_NI(IION)
         CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff(:,IION,1)  =  VCONV_NI(IION)
         CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(:,IION,2)   =  0.0_R8
         CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff(:,IION,2)  =  0.0_R8
         CORETRANSP(1)%VALUES(1)%ni_transp%diff_eff(:,IION,3)   =  0.0_R8
         CORETRANSP(1)%VALUES(1)%ni_transp%vconv_eff(:,IION,3)  =  0.0_R8
         CORETRANSP(1)%VALUES(1)%ti_transp%diff_eff(:,IION)     =  DIFF_TI(IION)
         CORETRANSP(1)%VALUES(1)%ti_transp%vconv_eff(:,IION)    =  VCONV_TI(IION)
         CORETRANSP(1)%VALUES(1)%vtor_transp%diff_eff(:,IION)   =  DIFF_VTOR(IION)
         CORETRANSP(1)%VALUES(1)%vtor_transp%vconv_eff(:,IION)  =  VCONV_VTOR(IION)
      END DO

      DO IIMP = 1, NIMP
         CORETRANSP(1)%VALUES(1)%NZ_TRANSP(IIMP)%diff_eff(:,:)  =  DIFF_NZ(IIMP)
         CORETRANSP(1)%VALUES(1)%NZ_TRANSP(IIMP)%vconv_eff(:,:) =  VCONV_NZ(IIMP)
         CORETRANSP(1)%VALUES(1)%TZ_TRANSP(IIMP)%diff_eff(:,:)  =  DIFF_TZ(IIMP)
         CORETRANSP(1)%VALUES(1)%TZ_TRANSP(IIMP)%vconv_eff(:,:) =  VCONV_TZ(IIMP)
      END DO



! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
      ALLOCATE            (CORETRANSP(1)%VALUES(1)%transportid%id(1))
      ALLOCATE            (CORETRANSP(1)%VALUES(1)%transportid%description(1))
      CORETRANSP(1)%VALUES(1)%transportid%id          = 'background'
      CORETRANSP(1)%VALUES(1)%transportid%flag        = 11
      CORETRANSP(1)%VALUES(1)%transportid%description = 'Background transport level'




      RETURN

      END SUBROUTINE BACKGROUND_TRANSPORT   
!-------------------------------------------------------!
!-------------------------------------------------------!
