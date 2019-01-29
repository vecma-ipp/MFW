!*DECK MRD03
!*CALL PROCESS
SUBROUTINE TRICYC(A,B,C,R,N,EPS)
  !        ################################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! MRD03 PERFORM L*U DECOMPOSITION AND BACK-SUBSTITUTION OF 1          *
  !       TRIDIAGONAL SYSTEMS WITH PERIODIC BOUNDARY CONDITIONS         *
  !                                                                     *
  !**********************************************************************
  !
  !         USE globals
  USE prec_const
  IMPLICIT NONE
  INTEGER          ::     J11
  INTEGER          ::     J9
  REAL(RKIND)      ::     TOP
  REAL(RKIND)      ::     R
  REAL(RKIND)      ::     C
  REAL(RKIND)      ::     CTOP
  REAL(RKIND)      ::     B
  REAL(RKIND)      ::     BTOP
  REAL(RKIND)      ::     EPS
  REAL(RKIND)      ::     A
  REAL(RKIND)      ::     DIAG
  INTEGER          ::     ITOP
  INTEGER          ::     J4
  INTEGER          ::     N
  DIMENSION A(N),B(N),C(N),R(N)
  !
  ! DECOMPOSE AND FORWARD SUBSTITUTION
  !
  DO J4=2,N-1
     !
     ITOP = J4 - 1
     DIAG = A(ITOP)
     !
     IF (DIAG .LT. EPS) THEN
        !
        WRITE(*,*) ' ZERO PIVOT I = ',J4
        STOP
        !
     ENDIF
     !
     BTOP = B(ITOP)
     CTOP = C(ITOP)
     !
     B(ITOP) = B(ITOP) / DIAG
     C(ITOP) = C(ITOP) / DIAG
     R(ITOP) = R(ITOP) / DIAG
     !
     A(J4) = A(J4) - BTOP * B(ITOP)
     C(J4) = C(J4) - BTOP * C(ITOP)
     R(J4) = R(J4) - BTOP * R(ITOP)
     !
     C(N) = C(N) - CTOP * C(ITOP)
     R(N) = R(N) - CTOP * R(ITOP)
     !
  END DO
  !
  ITOP = N - 1
  DIAG = A(ITOP)
  !
  IF (DIAG .LT. EPS) THEN
     !
     WRITE(*,*) ' ZERO PIVOT I = ',N-1
     STOP
     !
  ENDIF
  !
  TOP  = C(ITOP)
  !
  C(ITOP) = C(ITOP) / DIAG
  R(ITOP) = R(ITOP) / DIAG
  C(N)    = C(N) - TOP * C(ITOP)
  R(N)    = R(N) - TOP * R(ITOP)
  !
  ! CHECK LAST PIVOT
  !
  IF (C(N) .LT. EPS) THEN
     !
     WRITE(*,*) ' ZERO PIVOT I = ',N
     STOP
     !
  ENDIF
  !
  ! BACKSUBSTITUTION
  !
  R(N) = R(N) / C(N)
  !
  DO J9=1,N-1
     !
     R(J9) = R(J9) - C(J9) * R(N)
     !
  END DO
  !
  DO J11=N-2,1,-1
     !
     R(J11) = R(J11) - B(J11) * R(J11+1)
     !
  END DO
  !
  RETURN
END SUBROUTINE TRICYC
