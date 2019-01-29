!*DECK C2SP07
!*CALL PROCESS
SUBROUTINE COPYAPP(PAP,PA0,PA1,PB0,PB1,PB2,PB3,PC0,PC1,PC2, &
     &                          PD0,PD1,PD2,PD3,PE0,PE1,PF0,PF1,PF2)
  !        ##########################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP07  EVALUATE POLYNOMIAL COEFFICIENTS IF P' IS                  *
  !          PRESCRIBED AS POLYNOMIALS IN SEVERALS SECTIONS             *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  REAL(RKIND)      ::     PF0
  REAL(RKIND)      ::     PF1
  REAL(RKIND)      ::     PF2
  REAL(RKIND)      ::     ZF5
  REAL(RKIND)      ::     PE1
  REAL(RKIND)      ::     PE0
  REAL(RKIND)      ::     PD0
  REAL(RKIND)      ::     PD1
  REAL(RKIND)      ::     PD2
  REAL(RKIND)      ::     PD3
  REAL(RKIND)      ::     ZP3
  REAL(RKIND)      ::     PC0
  REAL(RKIND)      ::     PC1
  REAL(RKIND)      ::     PC2
  REAL(RKIND)      ::     PB0
  REAL(RKIND)      ::     PB1
  REAL(RKIND)      ::     PB2
  REAL(RKIND)      ::     PB3
  REAL(RKIND)      ::     ZF2
  REAL(RKIND)      ::     ZF1
  REAL(RKIND)      ::     PA1
  REAL(RKIND)      ::     PA0
  REAL(RKIND)      ::     PAP
  DIMENSION &
       &   PAP(*)
  !
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !**********************************************************************
  !                                                                     *
  !  FIRST SECTION                                                      *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAP(1) .NE. 0._RKIND) THEN
     !
     PA0 = PAP(6)
     PA1 = PAP(7)
     !
  ELSE IF (PAP(1) .EQ. 0._RKIND) THEN
     !
     PA0 = 0._RKIND
     PA1 = 0._RKIND
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  SECOND SECTION                                                     *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAP(1) .NE. PAP(2)) THEN
     !
     ZF1 = PA0 + PA1 * PAP(1)
     ZF2 = PAP(8)
     PB3 = FC3(PAP(1),ZF1,PAP(7),PAP(2),ZF2,PAP(9))
     PB2 = FC2(PAP(1),ZF1,PAP(7),PAP(2),ZF2,PAP(9))
     PB1 = FC1(PAP(1),ZF1,PAP(7),PAP(2),ZF2,PAP(9))
     PB0 = FC0(PAP(1),ZF1,PAP(7),PAP(2),ZF2,PAP(9))
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  THIRD SECTION                                                      *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAP(2) .NE. PAP(3)) THEN
     !
     PC2 = FD2(PAP(2),PAP(8),PAP(9),PAP(3),PAP(10))
     PC1 = FD1(PAP(2),PAP(8),PAP(9),PAP(3),PAP(10))
     PC0 = FD0(PAP(2),PAP(8),PAP(9),PAP(3),PAP(10))
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  FOURTH SECTION                                                     *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAP(3) .NE. PAP(4)) THEN
     !
     ZP3 = FQDQ1(PAP(2),PAP(8),PAP(9),PAP(3),PAP(10),PAP(3))
     PD3 = FC3(PAP(3),PAP(10),ZP3,PAP(4),PAP(11),PAP(12))
     PD2 = FC2(PAP(3),PAP(10),ZP3,PAP(4),PAP(11),PAP(12))
     PD1 = FC1(PAP(3),PAP(10),ZP3,PAP(4),PAP(11),PAP(12))
     PD0 = FC0(PAP(3),PAP(10),ZP3,PAP(4),PAP(11),PAP(12))
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  FIFTH SECTION                                                      *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAP(4) .NE. PAP(5)) THEN
     !
     PE0 = PAP(11) - PAP(12) * PAP(4)
     PE1 = PAP(12)
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  SIXTH SECTION                                                      *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAP(5) .NE. RC1P) THEN
     !
     ZF5 = PAP(11) + (PAP(5) - PAP(4)) * PAP(12)
     PF2 = FD2(PAP(5),ZF5,PAP(12),RC1P,PAP(13))
     PF1 = FD1(PAP(5),ZF5,PAP(12),RC1P,PAP(13))
     PF0 = FD0(PAP(5),ZF5,PAP(12),RC1P,PAP(13))
     !
  ENDIF
  !
  RETURN
END SUBROUTINE COPYAPP
