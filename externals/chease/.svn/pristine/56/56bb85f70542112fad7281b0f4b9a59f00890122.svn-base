!*DECK C2SX03
!*CALL PROCESS
SUBROUTINE BNDSPL
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SX03 INITIALIZE QUANTITIES USED FOR SPLINE INTERPOLATION OF       *
  !        BOUNDARY GIVEN BY EXPERIMENTAL POINTS                        *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZA1
  INTEGER          ::     J4
  INTEGER          ::     J2
  INTEGER          ::     IBP
  INTEGER          ::     JWKSP
  REAL(RKIND)      ::     ZWKSP
  REAL(RKIND)      ::     ZDZ
  REAL(RKIND)      ::     ZDR
  INTEGER          ::     J1
  DIMENSION &
       &   ZA1(NPBPS), ZB1(NPBPS), ZC1(NPBPS), &
       &   ZWKSP(NPBPS)
  DIMENSION &
       &   JWKSP(NPBPS)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !   COMPUTE THETA(R,Z) OF LINES PASSING THROUGH (BPS(1),BPS(12)) AND
  !   THE (R,Z) POINTS KNOWN ON THE PLASMA BOUNDARY
  !
  DO J1=1,NBPS-1
     ZDR = RRBPS(J1) - BPS( 1)
     ZDZ = RZBPS(J1) - BPS(12)
     !
     IF (ZDR .EQ. 0._RKIND) THEN
        IF (ZDZ .GE. 0._RKIND) TETBPS(J1) =  .5_RKIND * CPI
        IF (ZDZ .LT. 0._RKIND) TETBPS(J1) = 1.5_RKIND * CPI
     ELSE
        TETBPS(J1) = ATAN2(ZDZ,ZDR)
     ENDIF
     ! 
     IF (TETBPS(J1) .LT. 0._RKIND) TETBPS(J1) = TETBPS(J1) + 2._RKIND * CPI
  END DO
  !
  !        SORT TETBPS, RRBPS AND RZBPS SO THAT ELEMENTS OF TETBPS ARE IN 
  !        INCREASING ORDER
  !
  CALL SORT3(NBPS-1,TETBPS,RRBPS,RZBPS,ZWKSP,JWKSP)
  !SYM
  !     SYMMETRIZE BOUNDARY SO THAT LOWER HALF = UPPER HALF
  !
  IF (NSYM.EQ.1) THEN
     IBP = 1
     DO J2=1,NBPS-1
        IF (TETBPS(J2).GT.CPI) GOTO 3
        IBP = IBP+1
     END DO
3    CONTINUE 
     !
     NBPS = 2 * (IBP - 1) + 1
     IF (NBPS.GT.NPBPS) THEN
        PRINT*,'NPBPS SMALLER THAN NBPS'
        STOP
     ENDIF
     !
     DO J4=IBP,NBPS-1
        TETBPS(J4) = 2*CPI-TETBPS(NBPS-J4)
        RRBPS(J4)  = RRBPS(NBPS-J4)
        RZBPS(J4)  = -RZBPS(NBPS-J4)
     END DO
  ENDIF
  !
  !        COMPUTE SPLINE COEFFICIENTS OF RRBPS AND RZBPS VERSUS TETBPS
  !        WITH CYCLIC BOUNDARY CONDITIONS
  !
  CALL SPLCY(TETBPS,RRBPS,NBPS-1,RC2PI,D2RBPS,ZA1,ZB1,ZC1)
  CALL SPLCY(TETBPS,RZBPS,NBPS-1,RC2PI,D2ZBPS,ZA1,ZB1,ZC1)
  !
  TETBPS(NBPS) = TETBPS(1) + 2._RKIND * CPI
  RRBPS(NBPS)  = RRBPS(1)
  RZBPS(NBPS)  = RZBPS(1)
  D2RBPS(NBPS) = D2RBPS(1)
  D2ZBPS(NBPS) = D2ZBPS(1)
  !
  RETURN
END SUBROUTINE BNDSPL
