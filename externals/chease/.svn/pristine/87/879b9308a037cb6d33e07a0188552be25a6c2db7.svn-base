!*DECK MSP03
!*CALL PROCESS
SUBROUTINE SPLCY(X,Y,N,PERIOD,YP2,A,B,C)
  !        ########################################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! MSP03  CUBIC SPLINE INTERPOLATION, Y PERIODIC.                      *
  !        X = DISCRETIZATION GRID                                      *
  !        Y = FUNCTION TO BE INTERPOLATED                              *
  !        N = NUMBER OF X GRID POINTS                                  *
  !        PERIOD = Y(X+PERIOD) = Y(X)                                  *
  !        YP2 = SECOND DERIVATIVE OF Y WITH RESPECT TO X               *
  !        A,B,C = WORK ARRAYS                                          *
  !                                                                     *
  !**********************************************************************
  !
  USE globals, except_A => A, except_B => B
  IMPLICIT NONE
  REAL(RKIND)   	::  	PERIOD 	! <splcy.f90>
  REAL(RKIND)   	::  	Y 	! <splcy.f90>
  REAL(RKIND)   	::  	YP2 	! <splcy.f90>
  REAL(RKIND)   	::  	C 	! <splcy.f90>
  REAL(RKIND)   	::  	B 	! <splcy.f90>
  REAL(RKIND)   	::  	X 	! <splcy.f90>
  REAL(RKIND)   	::  	A 	! <splcy.f90>
  INTEGER       	::  	J2 	! <splcy.f90>
  INTEGER       	::  	N 	! <splcy.f90>
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
  A(1)   = (PERIOD + X(2) - X(N)) / 3._RKIND
  B(1)   = (X(2) - X(1)) / 6._RKIND
  C(1)   = (PERIOD + X(1) - X(N)) / 6._RKIND
  YP2(1) = (Y(2) - Y(1)) / (X(2) - X(1)) - &
       &            (Y(1) - Y(N)) / (PERIOD + X(1) - X(N))
  C(N-1) = B(N-1)
  C(N)   = (PERIOD + X(1) - X(N-1)) / 3._RKIND
  YP2(N) = (Y(1) - Y(N)) / (PERIOD + X(1) - X(N))- &
       &            (Y(N) - Y(N-1)) / (X(N) - X(N-1))
  !
  CALL TRICYC(A,B,C,YP2,N,RC1M14)
  !
  RETURN
END SUBROUTINE SPLCY
