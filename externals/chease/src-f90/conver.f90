!*DECK C2SE06
!*CALL PROCESS
SUBROUTINE CONVER(K,KCON)
  !        #########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SE06 CONVERGENCE TEST : KCON = 0  NO CONVERGENCE                  *
  !                           KCON = 1  CONVERGENCE OVER NON-           *
  !                                     LINEARITY                       *
  !                           KCON = 2  CONVERGENCE OVER CURRENT        *
  !                                     PROFILE                         *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  INTEGER          ::     KCON
  INTEGER          ::     K
  GOTO (1,2) K
  !
1 CONTINUE
  !
  !**********************************************************************
  !                                                                     *
  ! 1. CONVERGENCE TEST ON RESIDU OF ITERATION OVER NONLINEARITY        *
  !                                                                     *
  !**********************************************************************
  !
  IF (RESIDU .LT. CEPS) KCON = 1
  !
  RETURN
  !
2 CONTINUE
  !
  !**********************************************************************
  !                                                                     *
  ! 2. CONVERGENCE TEST ON RESIDU OF ITERATION OVER MAPPING             *
  !                                                                     *
  !**********************************************************************
  !
  IF (RESMAP .LT. 100._RKIND * CEPS) THEN
     !%OS         IF (RESMAP .LT. 3. * CEPS) THEN
     !
     KCON = 2
     RETURN
     !
  ENDIF
  !
  KCON   = 0
  !
  RETURN
END SUBROUTINE CONVER
