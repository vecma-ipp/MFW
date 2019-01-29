!*DECK MRD02
!*CALL PROCESS
SUBROUTINE TRIDAGM(A,B,R,DIAG,N,MD,M,EPS)
  !        #########################################
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
  INTEGER          ::     J6
  INTEGER          ::     J10
  INTEGER          ::     J5
  REAL(RKIND)      ::     R
  REAL(RKIND)      ::     B
  REAL(RKIND)      ::     TOP
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
  DIMENSION A(MD,N),B(MD,N),R(MD,N),DIAG(MD)
  !
  ! DECOMPOSE AND FORWARD SUBSTITUTION
  !
  DO J4=2,N
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
        TOP  = B(J3,ITOP)
        !
        B(J3,ITOP) = B(J3,ITOP) / DIAG(J3)
        R(J3,ITOP) = R(J3,ITOP) / DIAG(J3)
        !
        A(J3,J4) = A(J3,J4) - TOP * B(J3,ITOP)
        R(J3,J4) = R(J3,J4) - TOP * R(J3,ITOP)
        ! 
     END DO
  END DO
  !
  ! CHECK LAST PIVOT
  !
  IMN = ISAMIN(M,A(1,N),1)
  !
  IF (A(IMN,N) .LT. EPS) THEN
     !
     WRITE(*,*) ' ZERO PIVOT I = ',N,', M = ',IMN
     STOP
     !
  ENDIF
  !
  ! BACKSUBSTITUTION
  !
  DO J5=1,M
     !
     R(J5,N) = R(J5,N) / A(J5,N)
     !
  END DO
  !
  DO J10=N-1,1,-1
     DO J6=1,M
        !
        R(J6,J10) = R(J6,J10) - B(J6,J10) * R(J6,J10+1)
        !
     END DO
  END DO
  !
  RETURN
END SUBROUTINE TRIDAGM
