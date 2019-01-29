!*DECK MSP01
!*CALL PROCESS
SUBROUTINE SPLINE(N,X,Y,Y2,WORK)
  !        ################################      
  !
  ! ALMOST NUMERICAL RECIPES ROUTINE. THE FIRST DERIVATIVES 
  ! REQUIRED AT THE FIRST AND THE LAST NODE OF THE SPLINE
  ! INTERPOLATION ARE INTERPOLATED WITH CUBIC LAGRANGE FUNCTIONS.
  !
  ! DOES NOT ASSUME ANY SPECIFIC X MESH
  !
  USE prec_const
  USE interpol
  IMPLICIT NONE
  INTEGER          ::     K
  REAL(RKIND)      ::     UN
  REAL(RKIND)      ::     QN
  REAL(RKIND)      ::     P
  REAL(RKIND)      ::     SIG
  INTEGER          ::     I
  REAL(RKIND)      ::     WORK
  REAL(RKIND)      ::     Y2
  REAL(RKIND)      ::     YPN
  REAL(RKIND)      ::     X
  REAL(RKIND)      ::     Y
  REAL(RKIND)      ::     YP1
  INTEGER          ::     N
  DIMENSION X(N),Y(N),Y2(N),WORK(N)
  !
  !
  ! !!!! FORE PRECISE DOCUMENTATION SEE NUMERICAL RECIPES, $3.3, P.86
  !
  !        N : # OF TRIDIAGONAL EQUATIONS TO BE SOLVED
  !
  !        X & Y : ARRAYS CONTAINING TABULATED FUNCTION SUCH THAT
  !                Y(I) = F(X(I)), I=1,...,N AND X(I) < X(I+1),
  !                I = 1,...,N-1
  !
  !        Y2 : OUTPUT ARRAY. CONTAINS SECOND DERIVATIVES SUCH THAT
  !             Y2(I) = F''(X(I)), I=1,...,N AND X(I) < X(I+1),
  !             I = 1,...,N-1
  !
  !        WORK : WORK SPACE REQUIRED FOR RESOLUTION
  !
  ! COMPUTE FIRST DERIVATIVES OF Y AT X(1) AND X(N) USING A CUBIC LAGRANGE
  ! INTERPOLATION. 
  !
  YP1 = FCCCC1(Y(1),Y(2),Y(3),Y(4),X(1),X(2),X(3),X(4),X(1))
  YPN = FCCCC1(Y(N-3),Y(N-2),Y(N-1),Y(N), &
       &                 X(N-3),X(N-2),X(N-1),X(N),X(N))
  !
  ! LOWER BOUNDARY IS SET TO HAVE A SPECIFIED FIRST DERIVATIVE YP1
  !
  Y2(1)   = - 0.5_RKIND
  WORK(1) = (3._RKIND/(X(2)-X(1))) * ((Y(2)-Y(1))/(X(2)-X(1)) - YP1)
  !
  ! CONSTRUCTION AND DECOMPOSITION OF TRIDIAGONAL MATRIX
  !
  DO I=2,N-1
     !
     SIG     = (X(I)-X(I-1)) / (X(I+1)-X(I-1))
     P       = SIG * Y2(I-1) + 2._RKIND
     Y2(I)   = (SIG - 1._RKIND) / P
     WORK(I) = (6._RKIND*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))/ &
          &         (X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*WORK(I-1))/P
     !
  END DO
  !
  ! UPPER BOUNDARY IS SET TO HAVE A SPECIFIED FIRST DERIVATIVE YPN
  !
  QN = 0.5_RKIND
  UN = (3._RKIND/(X(N)-X(N-1))) * (YPN - (Y(N)-Y(N-1))/(X(N)-X(N-1)))
  !
  Y2(N) = (UN - QN*WORK(N-1)) / (QN*Y2(N-1)+1._RKIND)
  !
  ! BACKSUBSTITUTION
  !
  DO K=N-1,1,-1
     !
     Y2(K) = Y2(K) * Y2(K+1) + WORK(K)
     !
  END DO
  !
  RETURN
END SUBROUTINE SPLINE
