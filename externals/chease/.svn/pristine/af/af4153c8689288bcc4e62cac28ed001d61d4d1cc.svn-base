!*DECK C2S08
!*CALL PROCESS
SUBROUTINE CHECK
  !        ################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2S08 : CHECKS THE SOLUTION. IT COMPUTES :                          *
  !                                                                     *
  !         SQRT(SASUM((A(I,J)*CPSI(J) - B(I))**2) / N4NSNT, I=1,N4NSNT *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     SSUM
  INTEGER          ::     IROW
  INTEGER          ::     ICOL
  INTEGER          ::     J2
  INTEGER          ::     I2
  INTEGER          ::     I1
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZRES
  DIMENSION &
       &   ZRES(NP4NST)
  !
  INCLUDE 'BNDIND.inc'
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  CALL VZERO(ZRES,N4NSNT)
  CALL SETUPA
  CALL SETUPB
  !
  DO J3=1,N4NSNT
     !
     I1 = MAX(1,J3-NBAND+1)
     I2 = MIN(N4NSNT,J3+NBAND-1)
     !
     DO J2=I1,I2
        !
        ICOL = INDCOL(J2,J3)
        IROW = INDROW(J2,J3)
        !
        ZRES(J3) = ZRES(J3) + A(ICOL,IROW) * CPSILI(J2)
        !
     END DO
     !
     ZRES(J3) = (ZRES(J3) - B(J3)) * (ZRES(J3) - B(J3))
     !
  END DO
  !
  SCHECK = SQRT(SSUM(N4NSNT,ZRES,1)) / REAL(N4NSNT,RKIND)
  !
  RETURN
END SUBROUTINE CHECK
