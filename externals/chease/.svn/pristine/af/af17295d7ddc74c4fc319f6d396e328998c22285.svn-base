!*DECK C0S01
!*CALL PROCESS
SUBROUTINE MASTER
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C0S01 CONTROLS THE RUN                                              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     STIME
  COMMON /COMTIM/ STIME
  !
  !----*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  CALL RUNTIM
  !
  !**********************************************************************
  !                                                                     *
  ! 1. SET UP THE CASE                                                  *
  !                                                                     *
  !**********************************************************************
  !
  CALL LABRUN
  !TTM         CALL CLEAR
  CALL PRESET
  CALL DATA
  CALL globals_init
  CALL TCASE
  CALL COTROL
  CALL AUXVAL
  !
  !**********************************************************************
  !                                                                     *
  ! 3. STEP ON THE CALCULATION                                          *
  !                                                                     *
  !**********************************************************************
  !
  CALL STEPON
  CALL RUNTIM
  !
  RETURN
END SUBROUTINE MASTER
