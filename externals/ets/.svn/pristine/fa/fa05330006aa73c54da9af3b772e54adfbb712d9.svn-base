
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine converts two coreprof CPOs           !
  !     to a single coredelta CPO by taking               !
  !     difference between them                           !
  !-------------------------------------------------------!
  !     Delta:       ---                                  !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for  ETS workflow           !
  !                                                       !
  !-------------------------------------------------------!

    SUBROUTINE COREPROF2COREDELTA                         &
              (COREPROF_PIVOT, COREPROF_NEW, COREDELTA)

      USE ITM_CONSTANTS
      USE EUITM_SCHEMAS
      USE COPY_STRUCTURES
      USE INTERPOLATE_CPO
      USE ALLOCATE_DEALLOCATE
      USE DEALLOCATE_STRUCTURES

      
      IMPLICIT NONE



! +++ CPOs  
      TYPE (TYPE_COREPROF),   POINTER :: COREPROF_PIVOT(:)  
      TYPE (TYPE_COREPROF),   POINTER :: COREPROF_NEW(:)  
      TYPE (TYPE_COREPROF),   POINTER :: COREPROF_INTERP(:)  
      TYPE (TYPE_COREDELTA),  POINTER :: COREDELTA(:)    

      INTEGER                         :: NRHO
      INTEGER,              PARAMETER :: NSLICE = 1             
      INTEGER                         :: NRHO1, NRHO2
      INTEGER                         :: NNUCL
      INTEGER                         :: NION, IION
      INTEGER                         :: NIMP       
      INTEGER,            ALLOCATABLE :: NZIMP(:)
      INTEGER                         :: NNEUT
      INTEGER,            ALLOCATABLE :: NCOMP(:)
      INTEGER,            ALLOCATABLE :: NTYPE(:)


! +++ Interpolate pivot CPO to the grid of new CPO  
      ALLOCATE                    (COREPROF_INTERP(1))
      CALL COPY_CPO               (COREPROF_NEW(1),   COREPROF_INTERP(1))
      CALL INTERPOLATE_PROF       (COREPROF_PIVOT(1), COREPROF_INTERP(1))
      NRHO                  = SIZE(COREPROF_NEW(1)%rho_tor)


! +++ Allocate output CPO and internal derived types:
      CALL GET_COMP_DIMENSIONS    (COREPROF_NEW(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
      CALL ALLOCATE_COREDELTA_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  COREDELTA)
      CALL COPY_CPO               (COREPROF_NEW(1)%COMPOSITIONS,   COREDELTA(1)%COMPOSITIONS)


! +++ Fill output CPO with data:
      COREDELTA(1)%VALUES(1)%rho_tor         = COREPROF_NEW(1)%rho_tor 
      COREDELTA(1)%VALUES(1)%rho_tor_norm    = COREPROF_NEW(1)%rho_tor_norm
      COREDELTA(1)%VALUES(1)%psi             = COREPROF_NEW(1)%psi%value
      COREDELTA(1)%VALUES(1)%delta_psi       = COREPROF_NEW(1)%psi%value  - COREPROF_INTERP(1)%psi%value
      COREDELTA(1)%VALUES(1)%delta_te        = COREPROF_NEW(1)%te%value   - COREPROF_INTERP(1)%te%value
      COREDELTA(1)%VALUES(1)%delta_ne        = COREPROF_NEW(1)%ne%value   - COREPROF_INTERP(1)%ne%value
      COREDELTA(1)%VALUES(1)%delta_ti        = COREPROF_NEW(1)%ti%value   - COREPROF_INTERP(1)%ti%value
      COREDELTA(1)%VALUES(1)%delta_ni        = COREPROF_NEW(1)%ni%value   - COREPROF_INTERP(1)%ni%value
      COREDELTA(1)%VALUES(1)%delta_vtor      = COREPROF_NEW(1)%vtor%value - COREPROF_INTERP(1)%vtor%value


! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
      ALLOCATE                                (COREDELTA(1)%VALUES(1)%deltaid%id(1))
      ALLOCATE                                (COREDELTA(1)%VALUES(1)%deltaid%description(1))     
      COREDELTA(1)%VALUES(1)%deltaid%id          = 'unspecified'
      COREDELTA(1)%VALUES(1)%deltaid%flag        = 0
      COREDELTA(1)%VALUES(1)%deltaid%description = 'Unspecified coredelta'


10    IF(ALLOCATED(NZIMP)) DEALLOCATE         (NZIMP)
      IF(ALLOCATED(NCOMP)) DEALLOCATE         (NCOMP)
      IF(ALLOCATED(NTYPE)) DEALLOCATE         (NTYPE)

      CALL DEALLOCATE_CPO                     (COREPROF_INTERP)
 

      RETURN


    END SUBROUTINE COREPROF2COREDELTA
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

