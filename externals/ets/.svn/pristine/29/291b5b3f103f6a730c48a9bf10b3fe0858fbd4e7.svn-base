
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine adds values from coredelta CPO       !
  !     to a coreprof CPO                                 !
  !-------------------------------------------------------!
  !     Delta:       ---                                  !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for  ETS workflow           !
  !                                                       !
  !-------------------------------------------------------!

    SUBROUTINE COREDELTA2COREPROF                         &
              (COREPROF_PIVOT, COREDELTA, COREPROF_NEW)

      USE ITM_CONSTANTS
      USE EUITM_SCHEMAS
      USE COPY_STRUCTURES
      USE INTERPOLATE_CPO
      USE DEALLOCATE_STRUCTURES
      USE ALLOCATE_DEALLOCATE

      
      IMPLICIT NONE



! +++ CPOs  
      TYPE (TYPE_COREPROF),   POINTER :: COREPROF_PIVOT(:)  
      TYPE (TYPE_COREPROF),   POINTER :: COREPROF_NEW(:)  
      TYPE (TYPE_COREDELTA),  POINTER :: COREDELTA_INTERP(:)  
      TYPE (TYPE_COREDELTA),  POINTER :: COREDELTA(:)    
      TYPE (TYPE_COREDELTA),  POINTER :: COREDELTA_V1(:)    

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
 

! +++ Interpolate coredelta CPO to the grid of pivot coreprof CPO  

      NRHO                       = SIZE(COREPROF_PIVOT(1)%rho_tor)
      CALL GET_COMP_DIMENSIONS    (COREPROF_PIVOT(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)
      CALL ALLOCATE_COREDELTA_CPO (NSLICE, NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP,  COREDELTA_INTERP)
      CALL COPY_CPO               (COREPROF_PIVOT(1)%COMPOSITIONS,   COREDELTA_INTERP(1)%COMPOSITIONS)
      COREDELTA_INTERP(1)%VALUES(1)%rho_tor = COREPROF_PIVOT(1)%rho_tor



      ALLOCATE                    (COREDELTA_V1(1))
      ALLOCATE                    (COREDELTA_V1(1)%VALUES(1))
      CALL COPY_CPO               (COREDELTA(1)%COMPOSITIONS,   COREDELTA_V1(1)%COMPOSITIONS)
      CALL COPY_CPO               (COREDELTA(1)%VALUES(1),      COREDELTA_V1(1)%VALUES(1))


      CALL INTERPOLATE_DELTA      (COREDELTA_V1(1),     COREDELTA_INTERP(1))



! +++ Allocate output CPO and internal derived types:
      ALLOCATE                    (COREPROF_NEW(1))
      CALL COPY_CPO               (COREPROF_PIVOT(1),   COREPROF_NEW(1))



! +++ Fill output CPO with data:
      COREPROF_NEW(1)%psi%value  = COREPROF_PIVOT(1)%psi%value  + COREDELTA_INTERP(1)%VALUES(1)%delta_psi
      COREPROF_NEW(1)%te%value   = COREPROF_PIVOT(1)%te%value   + COREDELTA_INTERP(1)%VALUES(1)%delta_te
      COREPROF_NEW(1)%ne%value   = COREPROF_PIVOT(1)%ne%value   + COREDELTA_INTERP(1)%VALUES(1)%delta_ne
      COREPROF_NEW(1)%ti%value   = COREPROF_PIVOT(1)%ti%value   + COREDELTA_INTERP(1)%VALUES(1)%delta_ti
      COREPROF_NEW(1)%ni%value   = COREPROF_PIVOT(1)%ni%value   + COREDELTA_INTERP(1)%VALUES(1)%delta_ni
      COREPROF_NEW(1)%vtor%value = COREPROF_PIVOT(1)%vtor%value + COREDELTA_INTERP(1)%VALUES(1)%delta_vtor


      CALL DEALLOCATE_CPO         (COREDELTA_INTERP)
      CALL DEALLOCATE_CPO         (COREDELTA_V1)
 

      RETURN


    END SUBROUTINE COREDELTA2COREPROF
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

