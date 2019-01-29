!*DECK NUMREC04
!*CALL PROCESS
SUBROUTINE FOUR1(DATA,NN,ISIGN)
  !     -------------------------------
  !
  !     FROM NUMERICAL RECIPES
  !
  USE prec_const
  IMPLICIT NONE
  !%OS      REAL WR,WI,WPR,WPI,WTEMP,THETA
  REAL(RKIND)      ::     WTEMP
  REAL(RKIND)      ::     WI
  REAL(RKIND)      ::     WR
  REAL(RKIND)      ::     WPI
  REAL(RKIND)      ::     WPR
  INTEGER          ::     ISIGN
  REAL(RKIND)      ::     THETA
  INTEGER          ::     ISTEP
  INTEGER          ::     MMAX
  INTEGER          ::     M
  REAL(RKIND)      ::     TEMPI
  REAL(RKIND)      ::     DATA
  REAL(RKIND)      ::     TEMPR
  INTEGER          ::     I
  INTEGER          ::     J
  INTEGER          ::     NN
  INTEGER          ::     N
  DIMENSION DATA(*)
  N=2*NN
  J=1
  DO I=1,N,2
     IF(J.GT.I)THEN
        TEMPR=DATA(J)
        TEMPI=DATA(J+1)
        DATA(J)=DATA(I)
        DATA(J+1)=DATA(I+1)
        DATA(I)=TEMPR
        DATA(I+1)=TEMPI
     ENDIF
     M=N/2
1    IF ((M.GE.2).AND.(J.GT.M)) THEN
        J=J-M
        M=M/2
        GO TO 1
     ENDIF
     J=J+M
  END DO
  MMAX=2
2 IF (N.GT.MMAX) THEN
     ISTEP=2*MMAX
     THETA=6.28318530717959_RKIND/(ISIGN*MMAX)
     WPR=-2._RKIND* SIN(0.5_RKIND*THETA)**2
     WPI= SIN(THETA)
     WR=1.0_RKIND
     WI=0.0_RKIND
     DO M=1,MMAX,2
        DO I=M,N,ISTEP
           J=I+MMAX
           TEMPR=    (WR)*DATA(J)-    (WI)*DATA(J+1)
           TEMPI=    (WR)*DATA(J+1)+    (WI)*DATA(J)
           DATA(J)=DATA(I)-TEMPR
           DATA(J+1)=DATA(I+1)-TEMPI
           DATA(I)=DATA(I)+TEMPR
           DATA(I+1)=DATA(I+1)+TEMPI
        END DO
        WTEMP=WR
        WR=WR*WPR-WI*WPI+WR
        WI=WI*WPR+WTEMP*WPI+WI
     END DO
     MMAX=ISTEP
     GO TO 2
  ENDIF
  RETURN
END SUBROUTINE FOUR1
