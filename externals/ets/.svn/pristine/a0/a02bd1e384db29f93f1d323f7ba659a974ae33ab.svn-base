MODULE SPITZER

CONTAINS
!-------------------------------------------------------!
!-------------------------------------------------------!

  SUBROUTINE SPITZER_RESISTIVITY    (COREPROF_IN, CORETRANSP_OUT) 

!-------------------------------------------------------!
!     This routine is used by the ETS workflow          !
!     to calculate Spitzer resistivity                  !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     output CORETRANSP CPO is            !
!                   allocated inside the module         !
!                                                       !
!-------------------------------------------------------!

      USE ITM_TYPES
      USE ITM_CONSTANTS
      USE EUITM_SCHEMAS
      USE ALLOCATE_DEALLOCATE
      USE PLASMA_COLLISIONALITY
      

      IMPLICIT NONE

      INTEGER                           :: ifail

! +++ CPO types:
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)        !input CPO with profiles
      TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP_OUT(:)     !output CPO with transport (SIGMA)

! +++ Dimensions:
      INTEGER,                PARAMETER :: NSLICE = 1            !number of CPO ocurancies in the work flow
      INTEGER                           :: NRHO, IRHO            !number of radial points     (input, determined from COREPROF CPO)
      INTEGER                           :: NNUCL                 !number of nuclei species
      INTEGER                           :: NION                  !number of ion species
      INTEGER                           :: NIMP                  !number of impurity species
      INTEGER,              ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
      INTEGER                           :: NNEUT                 !number of neutrals species
      INTEGER,              ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
      INTEGER,              ALLOCATABLE :: NTYPE(:)              !number of types for each neutral

! +++ Internal variables:
      REAL (R8)                         :: CLOG                  !Coulomb logarithm      
      REAL (R8),            ALLOCATABLE :: TE(:)
      REAL (R8),            ALLOCATABLE :: NE(:)
      REAL (R8),            ALLOCATABLE :: TAU_E(:)
      REAL (R8),            ALLOCATABLE :: SIGMA(:)

! +++ Constants:
      REAL (R8),              PARAMETER :: ME  = itm_me*1e3_R8   !electron mass              [g]
      REAL (R8),              PARAMETER :: E   = itm_qe*3e9_R8   !elementary charge,         [esu]
      REAL (R8),              PARAMETER :: CN  = 1.E-6_R8        !density convergence        from [m^-3] to [cm^-3]
      REAL (R8),              PARAMETER :: CS  = 9.E9_R8         !conductivity convergence   from [(Ohm*m)^-1] to [s^-1]


! +++ Set dimensions:
      NRHO                   = SIZE (COREPROF_IN(1)%rho_tor)
      CALL GET_COMP_DIMENSIONS      (COREPROF_IN(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

! +++ Allocate output CPO:
      CALL ALLOCATE_CORETRANSP_CPO  (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP_OUT)        
      CORETRANSP_OUT(1)%VALUES(1)%rho_tor = COREPROF_IN(1)%rho_tor

! +++ Set up profiles:
      ALLOCATE (TE(NRHO))
      ALLOCATE (NE(NRHO))
      ALLOCATE (TAU_E(NRHO))
      ALLOCATE (SIGMA(NRHO))
      RHO_LOOP1: DO IRHO =1,NRHO
         TE(IRHO)           = COREPROF_IN(1)%TE%value(IRHO)
         NE(IRHO)           = COREPROF_IN(1)%NE%value(IRHO)


! +++ Electron collisions:
!       determination of Coulomb logarithm:
         IF(TE(IRHO).GE.10) CLOG = 24.E0_R8 - 1.15E0_R8*LOG10(NE(IRHO)*CN) + 2.30E0_R8*LOG10(TE(IRHO))
         IF(TE(IRHO).LT.10) CLOG = 23.E0_R8 - 1.15E0_R8*LOG10(NE(IRHO)*CN) + 3.45E0_R8*LOG10(TE(IRHO))


!       electron collision time:
         TAU_E(IRHO)          = (SQRT(2.D0*ME)*(Te(IRHO))**1.5) / 1.8D-19 / (NE(IRHO)*CN) / CLOG


!       Plasma electrical conductivity:
         SIGMA(IRHO)          = 1.96E0_R8 * E**2 *NE(IRHO)*CN * TAU_E(IRHO) /ME /CS

      END DO RHO_LOOP1

      CORETRANSP_OUT(1)%VALUES(1)%sigma = SIGMA

! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
      ALLOCATE            (CORETRANSP_OUT(1)%VALUES(1)%transportid%id(1))
      ALLOCATE            (CORETRANSP_OUT(1)%VALUES(1)%transportid%description(1))
      CORETRANSP_OUT(1)%VALUES(1)%transportid%id          = 'spitzer'
      CORETRANSP_OUT(1)%VALUES(1)%transportid%flag        = 14
      CORETRANSP_OUT(1)%VALUES(1)%transportid%description = 'Spitzer Resistivity'


      IF (ALLOCATED(TE))    DEALLOCATE (TE)
      IF (ALLOCATED(NE))    DEALLOCATE (NE)
      IF (ALLOCATED(TAU_E)) DEALLOCATE (TAU_E)
      IF (ALLOCATED(SIGMA)) DEALLOCATE (SIGMA)

      RETURN

      END SUBROUTINE SPITZER_RESISTIVITY    
!-------------------------------------------------------!
!-------------------------------------------------------!


END MODULE SPITZER
