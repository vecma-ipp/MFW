!-------------------------------------------------------!
!-------------------------------------------------------!

  SUBROUTINE IGNORE_SOURCES    (COREPROF, CORESOURCE) 

!-------------------------------------------------------!
!     This routine is used by the ETS workflow          !
!     in case when sources should be ignored            !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     output CORESOURCE CPO is            !
!                   allocated inside the module         !
!                                                       !
!-------------------------------------------------------!


      USE  EUITM_SCHEMAS
      USE  EUITM_ROUTINES
      USE  ALLOCATE_DEALLOCATE
      USE  ITM_TYPES
      USE  COPY_STRUCTURES
      USE  DEALLOCATE_STRUCTURES


      IMPLICIT NONE


      INTEGER                           :: ifail
 

! +++ CPO derived types:
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF(:)         !input CPO with internal ETS parameters profiles from previous time
      TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE(:)       !output CPO with sources


! +++ Dimensions:
      INTEGER,               PARAMETER  :: NSLICE = 1          !number of CPO ocurancies in the work flow
      INTEGER                           :: NRHO                !number of radial points     (input, determined from COREPROF CPO)
      INTEGER                           :: NION                !number of ion species       (input, determined from COREPROF CPO)
      INTEGER                           :: NIMP                !number of impurities                (input)
      INTEGER                           :: NNUCL               !number of nuclei species
      INTEGER,              ALLOCATABLE :: NZIMP(:)            !number of ionization states for each impurity
      INTEGER                           :: NNEUT               !number of neutrals species
      INTEGER,              ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
      INTEGER,              ALLOCATABLE :: NTYPE(:)            !number of types for each neutral



! +++ Set dimensions:
      NRHO                     = SIZE (COREPROF(1)%rho_tor, DIM=1)

      CALL GET_COMP_DIMENSIONS        (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
! +++ Allocate output CPO:
      CALL ALLOCATE_CORESOURCE_CPO    (NSLICE,      NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE)
      call deallocate_cpo(CORESOURCE(1)%COMPOSITIONS)
      CALL COPY_CPO                   (COREPROF(1)%COMPOSITIONS, CORESOURCE(1)%COMPOSITIONS)


! +++ Save output in CPO:
      CORESOURCE(1)%time                     =  COREPROF(1)%time     !time    [s]
      CORESOURCE(1)%VALUES(1)%rho_tor        =  COREPROF(1)%rho_tor  !rho     [m]
      CORESOURCE(1)%VALUES(1)%rho_tor_norm   =  COREPROF(1)%rho_tor/COREPROF(1)%rho_tor(NRHO)  !rho_norm [m]




! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
      ALLOCATE            (CORESOURCE(1)%VALUES(1)%sourceid%id(1))
      ALLOCATE            (CORESOURCE(1)%VALUES(1)%sourceid%description(1))
      CORESOURCE(1)%VALUES(1)%sourceid%id          = 'not_provided'
      CORESOURCE(1)%VALUES(1)%sourceid%flag        = 31
      CORESOURCE(1)%VALUES(1)%sourceid%description = 'No data provided'



      RETURN


      END SUBROUTINE IGNORE_SOURCES  

!-------------------------------------------------------!
!-------------------------------------------------------!
