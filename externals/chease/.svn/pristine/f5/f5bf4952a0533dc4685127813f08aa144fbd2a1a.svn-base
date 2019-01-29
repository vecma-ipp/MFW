!*DECK C2SD07
!*CALL PROCESS
SUBROUTINE AWAY(KD,KB1,KB2)
  !        ###########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SD07 PERFORMS FOLLOWING LINE-ROW OPERATIONS ON MATRIX A:          *
  !                                                                     *
  !        1) SET LINE KD TO 0                                          *
  !        2) SET ROW KD TO 0                                           *
  !        3) SET DIAGONAL TERM A(1,KD) TO 1                            *
  !                                                                     *
  !        KB1 AND KB2 ARE THE 2 EXTREMAL INDICES OF THE BAND MATRIX.   *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  INCLUDE 'BNDIND.inc'
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  INTEGER          ::     IAROW
  INTEGER          ::     KD
  INTEGER          ::     IACOL
  INTEGER          ::     KB2
  INTEGER          ::     KB1
  INTEGER          ::     J1
  !
  DO J1=KB1,KB2
     !
     IACOL = INDCOL(J1,KD)
     IAROW = INDROW(J1,KD)
     !
     A(IACOL,IAROW) = 0
     !
  END DO
  !
  A(1,KD) = 1
  !
  RETURN
END SUBROUTINE AWAY
