!*DECK C2S05
!*CALL PROCESS
SUBROUTINE MATRIX
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2S05 SET UP MATRIX A AND PERFORMS L * D * LT DECOMPOSITION OF A    *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !
  INTEGER          ::     ISGN
  CALL SETUPA
  CALL ALDLT(A,RC1M14,N4NSNT,NBAND,NPBAND,ISGN)
  !
  IF (ISGN .EQ. -1) STOP
  !
  RETURN
END SUBROUTINE MATRIX
