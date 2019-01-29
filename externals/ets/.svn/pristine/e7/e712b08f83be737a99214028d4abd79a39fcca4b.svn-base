MODULE SYNCHROTRON

CONTAINS


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + SYNCHROTRON + + + + + + + + + +   

  SUBROUTINE SYNCHROTRON_RADIATION(COREPROF, CORESOURCE)

!-------------------------------------------------------!
!     This routine calculate sybchrotron radiation      !
!                                                       !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   I.M.Ivanova-Stanik                  !
!     Kontacts:     irena.ivanova-stanik@ifpilm.pl      !
!                                                       !
!     Comments:     might change after the ITM          !
!                   data stucture is finalized          !
!                                                       !
!-------------------------------------------------------!


    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES
    USE ALLOCATE_DEALLOCATE
      
      
    IMPLICIT NONE
 
! +++ CPO derived types:
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)          !input CPO main plasma
    TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE(:)        !output CPO with syncrhotron radiation

      

! +++ Dimensions:
    INTEGER                          :: NRHO                 !number of radial points     (input, determined from COREPROF CPO)
    INTEGER                          :: NNUCL                !number of ion species       (input, determined from COREPROF CPO)
    INTEGER                          :: NION                 !number of ion species       (input, determined from COREPROF CPO)
    INTEGER                          :: NIMP                 !number of impurity species  (input, determined from COREIMPUR CPO)
    INTEGER                          :: NNEUT                !number of neutrals species                (input)
    INTEGER,             ALLOCATABLE :: NTYPE(:)             !number of impurity ionization states (input)     
    INTEGER,             ALLOCATABLE :: NCOMP(:)             !max_number of distinct atoms enter the composition-"1" wich is neutral
    INTEGER,             ALLOCATABLE :: NZIMP(:)
  
    INTEGER,               PARAMETER :: NOCUR = 1            !number of CPO ocurancies in the work flow
  
     

! +++ Set dimensions:
    NRHO                   = SIZE (COREPROF(1)%rho_tor)
    CALL GET_COMP_DIMENSIONS      (COREPROF(1)%COMPOSITIONS, NNUCL, NION, NIMP, NZIMP, NNEUT, NTYPE, NCOMP)

! +++ Allocate output CPOs:
    CALL ALLOCATE_CORESOURCE_CPO  (NOCUR, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE)
    CALL COPY_CPO                 (COREPROF(1)%COMPOSITIONS, CORESOURCE(1)%COMPOSITIONS)
     

    CORESOURCE(1)%datainfo%cocos           = 13
    CORESOURCE(1)%time                     = COREPROF(1)%time
    CORESOURCE(1)%VALUES(1)%rho_tor        = COREPROF(1)%rho_tor
    CORESOURCE(1)%VALUES(1)%rho_tor_norm   = COREPROF(1)%rho_tor/COREPROF(1)%rho_tor(NRHO)


! +++ Syncrhotron radiation    
    CORESOURCE(1)%VALUES(1)%qe%exp = 0.01_R8*6.2E-20_R8*COREPROF(1)%toroid_field%B0**2.0*COREPROF(1)%ne%value*COREPROF(1)%te%value


! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (CORESOURCE%VALUES(1)%sourceid%id(1))
    ALLOCATE            (CORESOURCE(1)%VALUES(1)%sourceid%description(1))
    CORESOURCE(1)%VALUES(1)%sourceid%id          = 'syncrotronradiation'
    CORESOURCE(1)%VALUES(1)%sourceid%flag        = 17
    CORESOURCE(1)%VALUES(1)%sourceid%description = 'Source from syncrotron radiation'


    IF(ALLOCATED(NTYPE)) DEALLOCATE (NTYPE)
    IF(ALLOCATED(NCOMP)) DEALLOCATE (NCOMP)
    IF(ALLOCATED(NZIMP)) DEALLOCATE (NZIMP)



    RETURN


    END SUBROUTINE SYNCHROTRON_RADIATION
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +




END MODULE SYNCHROTRON
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
