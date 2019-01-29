!*DECK NUMREC01
!*CALL PROCESS
SUBROUTINE SORT3(N,RA,RB,RC,WKSP,IWKSP)
  !       ---------------------------------------
  !
  !       NUMERICAL RECIPES SORTING ROUTINE.
  !  
  !         USE globals
  USE prec_const
  IMPLICIT NONE
  REAL(RKIND)      ::     RC
  REAL(RKIND)      ::     RB
  REAL(RKIND)      ::     WKSP
  INTEGER          ::     J
  INTEGER          ::     IWKSP
  REAL(RKIND)      ::     RA
  INTEGER          ::     N
  DIMENSION RA(N),RB(N),RC(N),WKSP(N),IWKSP(N)
  CALL INDEXX(N,RA,IWKSP)
  DO J=1,N
     WKSP(J)=RA(J)
  END DO
  DO J=1,N
     RA(J)=WKSP(IWKSP(J))
  END DO
  DO J=1,N
     WKSP(J)=RB(J)
  END DO
  DO J=1,N
     RB(J)=WKSP(IWKSP(J))
  END DO
  DO J=1,N
     WKSP(J)=RC(J)
  END DO
  DO J=1,N
     RC(J)=WKSP(IWKSP(J))
  END DO
  RETURN
END SUBROUTINE SORT3
