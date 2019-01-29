!*DECK C3SB06
!*CALL PROCESS
SUBROUTINE GENOUT(VECTOR,ALPHA,NUNIT,NCRAY,NR,MSMAX,NSMAX,RM,RN)
  !     ================================================================
  !
  !                                        AUTHORS:
  !                                        G. VLAD,  ENEA FRASCATI
  !
  !.....GENERAL  INPUT ROUTINE:...........................................
  !.....                       VECTOR        :  VECTOR....................
  !.....                       ALPHA         :  ALPHANUMERIC STRING 6 CH..
  !.....                       NUNIT         :  INPUT  UNIT NUMBER........
  !.....                       NCRAY         :  RADIAL VECTOR DIMENSION...
  !.....                       NR,MSMAX,NSMAX:  CURRENT VECTOR DIM........
  !.....                       RM,RN         :  POLOIDAL AND TOROIDAL # ..
  !
  !...!!IF NSMAX .GT. 1, MSMAX MUST BE THE TRUE DIMENSION OF THE VECTORS..
  !.....TO PRESERVE THE CORRECT ORDERING OF THE VECTOR ELEMENTS...........
  !
  !
  !         USE globals, except_MSMAX => MSMAX, except_NSMAX => NSMAX, except_RN => RN, except_RM => RM
  USE prec_const
  USE globals, ONLY: NVERBOSE
  IMPLICIT NONE
  INTEGER          ::     NR
  INTEGER          ::     NCRAY
  INTEGER          ::     I
  REAL(RKIND)      ::     RN
  REAL(RKIND)      ::     RM
  INTEGER          ::     NUNIT
  INTEGER          ::     MSMAX
  INTEGER          ::     MS
  INTEGER          ::     NSMAX
  INTEGER          ::     NS
  COMPLEX(CKIND)    VECTOR
  DIMENSION &
       &   VECTOR(*)
  !
  DIMENSION &
       &   RM(*),   RN(*)
  !
  CHARACTER*6      ALPHA
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  DO NS=1,NSMAX
    DO MS=1,MSMAX
      IF (NVERBOSE .GE. 1) WRITE(NUNIT,1010) ALPHA,RM(MS+(NS-1)*MSMAX),RN(NS)
      IF (NVERBOSE .GE. 1) WRITE(NUNIT,1020) &
        &         (VECTOR(I+(MS-1)*NCRAY+(NS-1)*MSMAX*NCRAY),I=1,NR)
    END DO
  END DO
  !
1010 FORMAT(//,1X,A,2F20.0)
1020 FORMAT(2D30.15)
  !
  RETURN
END SUBROUTINE GENOUT
