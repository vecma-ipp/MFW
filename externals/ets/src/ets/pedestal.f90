! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module provides the interface between (external) CPO and internal ETS derived types.
!>
!> \author D.Kalupin
!>
!> \version "$Id: ets.f90 1490 2013-04-03 14:57:06Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE PEDESTAL

CONTAINS

   
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE MAIN_PLASMA_PEDESTAL (FUN, RHO, RHO_PED, PSI, NRHO) 

!-------------------------------------------------------!
!     This routine provides artificial pedestal         !
!     in profiles of plasma parameters which are not    !
!     solved up to the separatrix.                      !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Ref.:         `Snyder, PPCF 46 (2004), A131`      !
!                                                       !
!-------------------------------------------------------!


    USE ITM_TYPES
    USE ETS_PLASMA

    IMPLICIT NONE

! +++ Internal ETS derived types:
    TYPE (MAGNETIC_GEOMETRY)          :: GEOMETRY       !contains all geometry quantities
    TYPE (PLASMA_PROFILES)            :: PROFILES       !contains profiles of plasma parameters


! +++ Dimensions:
    INTEGER                           :: NRHO,IRHO      !number of radial points     
    INTEGER                           :: NPED


! +++ Parameters:
    REAL (R8),          INTENT(INOUT) :: FUN(NRHO)
    REAL (R8),          INTENT(IN)    :: RHO(NRHO)
    REAL (R8),          INTENT(IN)    :: PSI(NRHO)
    REAL (R8),          INTENT(IN)    :: RHO_PED
    REAL (R8)                         :: PSI_MID
    REAL (R8)                         :: DELTA
!-------------------------------------------------------!

! +++ Determine the index of pedestal top
    DO IRHO = 1, NRHO
       IF (RHO(IRHO).GT.RHO_PED) EXIT
       NPED = IRHO
    END DO

    PSI_MID =    (PSI(NPED) + PSI(NRHO)) / 2.0_R8
    DELTA   = ABS(PSI(NPED) - PSI(NRHO))

! +++ Determine the index of pedestal top
    DO IRHO = NPED+1, NRHO
       FUN(IRHO) = FUN(NPED) + (FUN(NPED)-FUN(NRHO)) *  &
                   (TANH(2.0_R8*(1.0_R8-PSI_MID)/DELTA)-TANH(2.0_R8*(PSI(IRHO)-PSI_MID)/DELTA))
    END DO

    RETURN



  END SUBROUTINE MAIN_PLASMA_PEDESTAL
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





END MODULE PEDESTAL

