!*DECK C2SI03
!*CALL PROCESS
SUBROUTINE COPYAT(PAT,PA0,PA1,PA2,PB0,PB1,PB2,PB3,PC0,PC1)
  !        ##########################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SI03  EVALUATE POLYNOMIAL COEFFICIENTS IF TT', I* OR I_PARALLEL  *
  !          ARE PRESCRIBED AS POLYNOMIALS IN SEVERALS SECTIONS         *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  REAL(RKIND)      ::     PC0
  REAL(RKIND)      ::     PC1
  REAL(RKIND)      ::     PB0
  REAL(RKIND)      ::     PB1
  REAL(RKIND)      ::     PB2
  REAL(RKIND)      ::     PB3
  REAL(RKIND)      ::     ZF2
  REAL(RKIND)      ::     ZF1
  REAL(RKIND)      ::     PA2
  REAL(RKIND)      ::     PA1
  REAL(RKIND)      ::     PA0
  REAL(RKIND)      ::     PAT
  DIMENSION &
       &   PAT(*)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !**********************************************************************
  !                                                                     *
  !  FIRST SECTION                                                      *
  !                                                                     *
  !**********************************************************************
  IF (PAT(1) .NE. 0._RKIND) THEN
     !
     PA0 = PAT(3)
     PA1 = PAT(4)
     PA2 = .5_RKIND * (PAT(5) - PAT(4)) / PAT(1)
     !
  ELSE IF (PAT(1).EQ.0.0_RKIND) THEN
     !
     PA0 = PAT(3)
     PA1 = 0._RKIND
     PA2 = 0._RKIND
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  SECOND SECTION                                                     *
  !                                                                     *
  !**********************************************************************
  !
  IF (PAT(1) .NE. PAT(2)) THEN
     !
     ZF1 = PA0 + PA1 * PAT(1) + PA2 * PAT(1) * PAT(1)
     ZF2 = PAT(6) - PAT(7) + PAT(7) * PAT(2)
     PB3 = FC3(PAT(1),ZF1,PAT(5),PAT(2),ZF2,PAT(7))
     PB2 = FC2(PAT(1),ZF1,PAT(5),PAT(2),ZF2,PAT(7))
     PB1 = FC1(PAT(1),ZF1,PAT(5),PAT(2),ZF2,PAT(7))
     PB0 = FC0(PAT(1),ZF1,PAT(5),PAT(2),ZF2,PAT(7))
     !
   ELSE
     ZF1 = PA0 + PA1 * PAT(1) + PA2 * PAT(1) * PAT(1)
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
  IF (PAT(2) .LT. 1._RKIND) THEN
    PC0 = PAT(6) - PAT(7)
    PC1 = PAT(7)
  else
    PC0 = PAT(6)
    PC1 = 0._RKIND
  end IF
  !
  RETURN
END SUBROUTINE COPYAT
