!*DECK MSP05
!*CALL PROCESS
SUBROUTINE SPLCYP(X,Y,N,XPERIOD,YPERIOD,YP2,A,B,C)
  !        ##################################################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! MSP05  CUBIC SPLINE INTERPOLATION, X AND Y PERIODIC.                *
  !        X = DISCRETIZATION GRID                                      *
  !        Y = FUNCTION TO BE INTERPOLATED                              *
  !        N = NUMBER OF X GRID POINTS                                  *
  !        XPERIOD = X+XPERIOD = X                                      *
  !        YPERIOD = Y(X+YPERIOD) = Y(X)                                *
  !        YP2 = SECOND DERIVATIVE OF Y WITH RESPECT TO X               *
  !        A,B,C = WORK ARRAYS                                          *
  !                                                                     *
  !**********************************************************************
  !
  USE globals, except_A => A, except_B => B
  IMPLICIT NONE
  REAL(RKIND)      ::     YPERIOD
  REAL(RKIND)      ::     XPERIOD
  REAL(RKIND)      ::     Y
  REAL(RKIND)      ::     YP2
  REAL(RKIND)      ::     C
  REAL(RKIND)      ::     B
  REAL(RKIND)      ::     X
  REAL(RKIND)      ::     A
  INTEGER          ::     J2
  INTEGER          ::     N
  DIMENSION A(N), B(N), C(N), &
       &             X(N), Y(N), YP2(N)
  !
  DO J2=2,N-1
     !
     A(J2)   = (X(J2+1) - X(J2-1)) / 3._RKIND
     B(J2)   = (X(J2+1) - X(J2  )) / 6._RKIND
     C(J2)   = 0._RKIND
     YP2(J2) = (Y(J2+1)-Y(J2)) / (X(J2+1)-X(J2))- &
          &                (Y(J2)-Y(J2-1)) / (X(J2)-X(J2-1))
     !
  END DO
  !
  A(1)   = (XPERIOD + X(2) - X(N)) / 3._RKIND
  B(1)   = (X(2) - X(1)) / 6._RKIND
  C(1)   = (XPERIOD + X(1) - X(N)) / 6._RKIND
  YP2(1) = (Y(2) - Y(1)) / (X(2) - X(1)) - &
       &            (YPERIOD + Y(1) - Y(N)) / (XPERIOD + X(1) - X(N))
  C(N-1) = B(N-1)
  C(N)   = (XPERIOD + X(1) - X(N-1)) / 3._RKIND
  YP2(N) = (YPERIOD + Y(1) - Y(N)) / (XPERIOD + X(1) - X(N)) - &
       &            (Y(N) - Y(N-1)) / (X(N) - X(N-1))
  !
  CALL TRICYC(A,B,C,YP2,N,RC1M14)
  !
  RETURN
END SUBROUTINE SPLCYP
