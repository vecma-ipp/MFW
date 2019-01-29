!*DECK C2SX04
!*CALL PROCESS
SUBROUTINE SUBSZ
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SX04 SHIFT Z POINTS OF BOUNDARY SO THAT SYMMETRY PLANE OF         *
  !        EQUILIBRIUM IS Z=0                                           *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J1
  DO J1=1,NBPS
     RZBPS(J1) = RZBPS(J1) - RZ0
  END DO
  !
  RETURN
END SUBROUTINE SUBSZ
