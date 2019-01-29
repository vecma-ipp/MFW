!*DECK C2SP05
!*CALL PROCESS
SUBROUTINE COPYAP(PAP,PA0,PA1,PB0,PB1,PB2,PB3,PC0,PC1,PC2)
  !        #########################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP05  EVALUATE POLYNOMIAL COEFFICIENTS IF P' IS                  *
  !          PRESCRIBED AS POLYNOMIALS IN SEVERALS SECTIONS             *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
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
     PA0 = PAP(5)
     PA1 = PAP(4)
     !
  ELSE IF (PAP(1) .EQ. 0._RKIND) THEN
     !
     PA0 = PAP(5)
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
    ZF2 = PAP(3)
    PB3 = FC3(PAP(1),ZF1,PAP(4),PAP(2),ZF2,PAP(7))
    PB2 = FC2(PAP(1),ZF1,PAP(4),PAP(2),ZF2,PAP(7))
    PB1 = FC1(PAP(1),ZF1,PAP(4),PAP(2),ZF2,PAP(7))
    PB0 = FC0(PAP(1),ZF1,PAP(4),PAP(2),ZF2,PAP(7))
    !
  ELSE
    ZF1 = PA0 + PA1 * PAP(1)
    ZF2 = ZF1
    PB3 = 0._RKIND
    PB2 = 0._RKIND
    PB1 = 0._RKIND
    PB0 = ZF1
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  THIRD SECTION                                                      *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAP(2) .NE. RC1P) THEN
    !
    PC2 = FD2(PAP(2),PAP(3),PAP(7),RC1P,PAP(6))
    PC1 = FD1(PAP(2),PAP(3),PAP(7),RC1P,PAP(6))
    PC0 = FD0(PAP(2),PAP(3),PAP(7),RC1P,PAP(6))
    !
  ELSE
    PC0 = PAP(6)
    PC1 = 0._RKIND
    PC2 = 0._RKIND
  ENDIF
  !
  RETURN
END SUBROUTINE COPYAP
