!*DECK MRD04
!*CALL PROCESS
SUBROUTINE TRICYCM(A,B,C,R,DIAG,N,MD,M,EPS)
  !        ###########################################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! MRD02 PERFORM L*U DECOMPOSITION AND BACK-SUBSTITUTION OF M          *
  !       TRIDIAGONAL SYSTEMS                                           *
  !                                                                     *
  !**********************************************************************
  !
  !         USE globals
  USE prec_const
  IMPLICIT NONE
  INTEGER          ::     J10
  INTEGER          ::     J11
  INTEGER          ::     J8
  INTEGER          ::     J9
  INTEGER          ::     J7
  REAL(RKIND)      ::     TOP
  INTEGER          ::     J6
  INTEGER          ::     J5
  REAL(RKIND)      ::     R
  REAL(RKIND)      ::     C
  REAL(RKIND)      ::     CTOP
  REAL(RKIND)      ::     B
  REAL(RKIND)      ::     BTOP
  INTEGER          ::     J3
  REAL(RKIND)      ::     EPS
  INTEGER          ::     ISAMIN
  INTEGER          ::     IMN
  REAL(RKIND)      ::     A
  REAL(RKIND)      ::     DIAG
  INTEGER          ::     M
  INTEGER          ::     J2
  INTEGER          ::     ITOP
  INTEGER          ::     J4
  INTEGER          ::     N
  INTEGER          ::     MD
  DIMENSION A(MD,N),B(MD,N),C(MD,N),R(MD,N),DIAG(MD)
  !
  ! DECOMPOSE AND FORWARD SUBSTITUTION
  !
  DO J4=2,N-1
     !
     ITOP = J4 - 1
     !
     DO J2=1,M
        !
        DIAG(J2) = A(J2,ITOP)
        !
     END DO
     !
     IMN = ISAMIN(M,DIAG,1)
     !
     IF (DIAG(IMN) .LT. EPS) THEN

        !
        WRITE(*,*) ' ZERO PIVOT I = ',J4,', M = ',IMN
        STOP
        !
     ENDIF

     !
     DO J3=1,M
        !
        BTOP = B(J3,ITOP)
        CTOP = C(J3,ITOP)
        !
        B(J3,ITOP) = B(J3,ITOP) / DIAG(J3)
        C(J3,ITOP) = C(J3,ITOP) / DIAG(J3)
        R(J3,ITOP) = R(J3,ITOP) / DIAG(J3)
        !
        A(J3,J4) = A(J3,J4) - BTOP * B(J3,ITOP)
        C(J3,J4) = C(J3,J4) - BTOP * C(J3,ITOP)
        R(J3,J4) = R(J3,J4) - BTOP * R(J3,ITOP)
        !
        C(J3,N) = C(J3,N) - CTOP * C(J3,ITOP)
        R(J3,N) = R(J3,N) - CTOP * R(J3,ITOP)

        !
     END DO
  END DO
  !
  ITOP = N - 1
  !
  DO J5=1,M
     !
     DIAG(J5) = A(J5,ITOP)
     !
  END DO
  !
  IMN = ISAMIN(M,DIAG,1)
  !
  IF (DIAG(IMN) .LT. EPS) THEN
     !
     WRITE(*,*) ' ZERO PIVOT I = ',N-1,', M = ',IMN
     STOP
     !
  ENDIF
  !
  DO J6=1,M
     !
     TOP  = C(J6,ITOP)
     !
     C(J6,ITOP) = C(J6,ITOP) / DIAG(J6)
     R(J6,ITOP) = R(J6,ITOP) / DIAG(J6)
     C(J6,N)    = C(J6,N) - TOP * C(J6,ITOP)
     R(J6,N)    = R(J6,N) - TOP * R(J6,ITOP)
     !
  END DO
  !
  ! CHECK LAST PIVOT
  !
  IMN = ISAMIN(M,C(1,N),1)
  !
  IF (C(IMN,N) .LT. EPS) THEN
     !
     WRITE(*,*) ' ZERO PIVOT I = ',N,', M = ',IMN
     STOP
     !
  ENDIF
  !
  ! BACKSUBSTITUTION
  !
  DO J7=1,M
     !
     R(J7,N) = R(J7,N) / C(J7,N)

     !
  END DO
  !
  DO J9=1,N-1
     DO J8=1,M
        !
        R(J8,J9) = R(J8,J9) - C(J8,J9) * R(J8,N)
        !
     END DO
  END DO
  !
  DO J11=N-2,1,-1
     DO J10=1,M
        !
        R(J10,J11) = R(J10,J11) - B(J10,J11) * R(J10,J11+1)
        !
     END DO
  END DO
  !
  RETURN
END SUBROUTINE TRICYCM
