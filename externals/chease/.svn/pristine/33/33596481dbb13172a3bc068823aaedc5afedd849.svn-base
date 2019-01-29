!
SUBROUTINE NTRIDG(A,KDIMA,ND1,ND2,N)
  !        #########################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**************************************************************
  ! PERFORM L*U DECOMPOSITION FOR ND2 - ND1 TRIDIAGONAL SYMETRIC
  ! REAL MATRIXES IN PARALLEL.
  !***************************************************************
  !
  USE globals, except_A => A
  IMPLICIT NONE
  !
  INTEGER          ::     KDIMA, N, ND1, ND2
  REAL(RKIND)      ::     A(KDIMA,2,N)
  !
  INTEGER          ::     ITOP, J2, J3, J4
  REAL(RKIND)      ::     TOP
  REAL(RKIND)      ::     DIAG(ND2)
  !
  DO J4=2,N
    ITOP = J4 - 1
    !
    DO J2=ND1,ND2
      DIAG(J2) = A(J2,1,ITOP)
    END DO
    !
    DO J3=ND1,ND2
      TOP    = A(J3,2,ITOP)
      A(J3,2,ITOP) = A(J3,2,ITOP) / DIAG(J3)
      A(J3,1,J4)   = A(J3,1,J4) - TOP * A(J3,2,ITOP)
    END DO
  END DO
  !
  RETURN
END SUBROUTINE NTRIDG
