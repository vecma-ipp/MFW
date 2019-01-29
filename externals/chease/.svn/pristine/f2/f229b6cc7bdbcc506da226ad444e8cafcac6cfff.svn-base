!*DECK MSP04
!*CALL PROCESS
SUBROUTINE MSPLCY(X,Y,N,MD,M,PERIOD,YP2,A,B,C,WORK)
  !        ###################################################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! MSP04  SOLVE M CUBIC SPLINE INTERPOLATIONS IN PARALLEL OF PERIODIC  *
  !        FUNCTINS.                                                    *
  !        X = DISCRETIZATION GRID                                      *
  !        Y = FUNCTION TO BE INTERPOLATED                              *
  !        N = NUMBER OF X GRID POINTS                                  *
  !        MD = LEADING DIMENSION OF ARRAYS                             *
  !        M = NUMBER OF SPLINES TO BE SOLVED                           *
  !        PERIOD = Y(M,X+PERIOD) = Y(M,X)                              *
  !        YP2 = SECOND DERIVATIVE OF Y WITH RESPECT TO X               *
  !        A,B,C,WORK = WORK ARRAYS                                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals, except_a => a, except_b => b
  IMPLICIT NONE
  REAL(RKIND)      ::     WORK
  REAL(RKIND)      ::     PERIOD
  INTEGER          ::     J3
  REAL(RKIND)      ::     Y
  REAL(RKIND)      ::     YP2
  REAL(RKIND)      ::     C
  REAL(RKIND)      ::     B
  REAL(RKIND)      ::     X
  REAL(RKIND)      ::     A
  INTEGER          ::     M
  INTEGER          ::     J1
  INTEGER          ::     J2
  INTEGER          ::     N
  INTEGER          ::     MD
  DIMENSION A(MD,N), B(MD,N), C(MD,N), WORK(MD), &
       &             X(MD,N), Y(MD,N), YP2(MD,N)
  !
  DO J2=2,N-1
     DO J1=1,M
        !
        A(J1,J2)   = (X(J1,J2+1) - X(J1,J2-1)) / 3._RKIND
        B(J1,J2)   = (X(J1,J2+1) - X(J1,J2  )) / 6._RKIND
        C(J1,J2)   = 0._RKIND
        YP2(J1,J2) = (Y(J1,J2+1)-Y(J1,J2)) / &
             &                      (X(J1,J2+1)-X(J1,J2)) - &
             &                      (Y(J1,J2)-Y(J1,J2-1)) / &
             &                      (X(J1,J2)-X(J1,J2-1))
        !
     END DO
  END DO
  !
  DO J3=1,M
     !
     A(J3,1)   = (PERIOD + X(J3,2) - X(J3,N)) / 3._RKIND
     B(J3,1)   = (X(J3,2) - X(J3,1)) / 6._RKIND
     C(J3,1)   = (PERIOD + X(J3,1) - X(J3,N)) / 6._RKIND
     YP2(J3,1) = (Y(J3,2)-Y(J3,1)) / (X(J3,2)-X(J3,1)) - &
          &                  (Y(J3,1)-Y(J3,N)) / (PERIOD+X(J3,1)-X(J3,N))
     C(J3,N-1) = B(J3,N-1)
     C(J3,N)   = (PERIOD + X(J3,1) - X(J3,N-1)) / 3._RKIND
     YP2(J3,N) = (Y(J3,1)-Y(J3,N)) / (PERIOD+X(J3,1)-X(J3,N))- &
          &                  (Y(J3,N)-Y(J3,N-1)) / (X(J3,N)-X(J3,N-1))
     !
  END DO
  !
  CALL TRICYCM(A,B,C,YP2,WORK,N,MD,M,RC1M14)
  !
  RETURN
END SUBROUTINE MSPLCY
