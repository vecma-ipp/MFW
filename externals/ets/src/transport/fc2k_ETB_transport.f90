!-------------------------------------------------------!
!-------------------------------------------------------!

  SUBROUTINE ETB_TRANSPORT    (CORETRANSP_IN, CORETRANSP_OUT,&
                               DIFF_NI,       VCONV_NI,      &
                               DIFF_NE,       VCONV_NE,      &
                               DIFF_TI,       VCONV_TI,      &
                               DIFF_TE,       VCONV_TE,      & 
                               DIFF_VTOR,     VCONV_VTOR,    & 
                               DIFF_NZ,       VCONV_NZ,      &
                               DIFF_TZ,       VCONV_TZ,      &
                               RHO_ETB) 

!-------------------------------------------------------!
!     This routine is used by the ETS workflow          !
!     to reduce transport inside ETB                    !
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
      USE  COPY_STRUCTURES
      USE  ITM_TYPES
      USE  ALLOCATE_DEALLOCATE
      

      IMPLICIT NONE

      INTEGER                           :: ifail

      TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP_IN(:)      !input CPO with transport
      TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP_OUT(:)     !output CPO with transport


! +++ Dimensions:
      INTEGER,               PARAMETER  :: NSLICE = 1            !number of CPO ocurancies in the work flow
      INTEGER                           :: NRHO, IRHO            !number of radial points     (input, determined from COREPROF CPO)
      INTEGER                           :: NNUCL                 !number of nuclei species
      INTEGER                           :: NION, IION            !number of ion species
      INTEGER                           :: NIMP, IIMP            !number of impurity species
      INTEGER,              ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                           :: IZIMP
      INTEGER                           :: NNEUT,    INEUT       !number of neutrals species
      INTEGER,              ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER,              ALLOCATABLE :: NTYPE(:)              !number of types for each neutral


      REAL(R8)                          :: RHO_ETB
      REAL(R8)                          :: DIFF_NI(30),   VCONV_NI(30)
      REAL(R8)                          :: DIFF_TI(30),   VCONV_TI(30)
      REAL(R8)                          :: DIFF_NE,       VCONV_NE
      REAL(R8)                          :: DIFF_TE,       VCONV_TE
      REAL(R8)                          :: DIFF_VTOR(30), VCONV_VTOR(30)
      REAL(R8)                          :: DIFF_NZ(100),  VCONV_NZ(100)
      REAL(R8)                          :: DIFF_TZ(100),  VCONV_TZ(100)

! +++ Set dimensions:
      NRHO                   = SIZE (CORETRANSP_IN(1)%VALUES(1)%rho_tor,                  DIM=1)
      CALL GET_COMP_DIMENSIONS      (CORETRANSP_IN(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)



! +++ Allocate output CPO:
      ALLOCATE      (CORETRANSP_OUT(1))
      CALL COPY_CPO (CORETRANSP_IN(1),   CORETRANSP_OUT(1))


! +++ Save output in CPO:
      DO IRHO = 1, NRHO

         IF (CORETRANSP_OUT(1)%VALUES(1)%rho_tor(IRHO)/CORETRANSP_OUT(1)%VALUES(1)%rho_tor(NRHO) .GT. RHO_ETB)  THEN

         CORETRANSP_OUT(1)%VALUES(1)%te_transp%diff_eff(IRHO)                =  DIFF_TE
         CORETRANSP_OUT(1)%VALUES(1)%te_transp%vconv_eff(IRHO)               =  VCONV_TE
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff(IRHO,1)              =  DIFF_NE
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff(IRHO,1)             =  VCONV_NE
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff(IRHO,2)              =  0.0_R8
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff(IRHO,2)             =  0.0_R8
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%diff_eff(IRHO,3)              =  0.0_R8
         CORETRANSP_OUT(1)%VALUES(1)%ne_transp%vconv_eff(IRHO,3)             =  0.0_R8

         DO IION = 1, NION
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff(IRHO,IION,1)      =  DIFF_NI(IION)
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff(IRHO,IION,1)     =  VCONV_NI(IION)
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff(IRHO,IION,2)      =  0.0_R8
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff(IRHO,IION,2)     =  0.0_R8
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%diff_eff(IRHO,IION,3)      =  0.0_R8
            CORETRANSP_OUT(1)%VALUES(1)%ni_transp%vconv_eff(IRHO,IION,3)     =  0.0_R8
            CORETRANSP_OUT(1)%VALUES(1)%ti_transp%diff_eff(IRHO,IION)        =  DIFF_TI(IION)
            CORETRANSP_OUT(1)%VALUES(1)%ti_transp%vconv_eff(IRHO,IION)       =  VCONV_TI(IION)
            CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%diff_eff(IRHO,IION)      =  DIFF_VTOR(IION)
            CORETRANSP_OUT(1)%VALUES(1)%vtor_transp%vconv_eff(IRHO,IION)     =  VCONV_VTOR(IION)
         END DO

         IF (NIMP > 0) THEN
            DO IIMP = 1, NIMP
              DO IZIMP = 1, NZIMP(IIMP)          
               CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%diff_eff(IRHO,IZIMP) =  DIFF_NZ(IIMP)
               CORETRANSP_OUT(1)%VALUES(1)%nz_transp(IIMP)%vconv_eff(IRHO,IZIMP)=  VCONV_NZ(IIMP)
               CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%diff_eff(IRHO,IZIMP) =  DIFF_TZ(IIMP)
               CORETRANSP_OUT(1)%VALUES(1)%tz_transp(IIMP)%vconv_eff(IRHO,IZIMP)=  VCONV_TZ(IIMP)
              END DO
            END DO
        ENDIF

         END IF

      END DO



      RETURN

      END SUBROUTINE ETB_TRANSPORT    
!-------------------------------------------------------!
!-------------------------------------------------------!
