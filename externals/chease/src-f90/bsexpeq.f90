!*DECK C3SB07
!*CALL PROCESS
SUBROUTINE BSEXPEQ(KOPT)
  !        ########################
  !                                        AUTHORS:
  !                                        O. SAUTER,  CRPP-EPFL
  !                                        H. LUTJENS, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SB07 READ/WRITE PROFILE ARRAYS OF T_E, N_E, ZEFF AND/OR T_I FROM  *
  !        AND ON UNIT NXPQTNZ, DEPENDING ON VALUE OF NBSEXPQ             *
  !        THESE PROFILES ARE USED FOR THE BOOTSTRAP CURRENT            *
  !        ASSUMES PROFILES VS S AND T IN [EV], N IN [M-3]              *
  !                                                                     *
  !     KOPT = 1 --> READ                                               *
  !     KOPT = 2 --> WRITE                                              *
  !                                                                     *
  !     NBSEXPQ: DETERMINES IF T_E, N_E, AND/OR ZEFF ARE GIVEN IN EXPEQ *
  !              IN THE SAME WAY AS P' AND/OR TT' CAN BE GIVEN AS ARRAYS*
  !              ARRAYS ON EXPEQ SHOULD BE IN THIS ORDER:T_E, N_E, ZEFF *
  !              NBSEXPQ = 0    --> NONE GIVEN AS ARRAY                 *
  !                                 AND NONE WRITTEN ON EXPEQ.OUT       *
  !              NBSEXPQ =    1 --> ONLY T_E GIVEN (IN BSTEMPE)         *
  !              NBSEXPQ =   10 --> ONLY N_E GIVEN (IN BSDENSE)         *
  !              NBSEXPQ =  100 --> ONLY ZEFF GIVEN (IN BSZEFF)         *
  !              NBSEXPQ = 1000 --> ONLY T_I GIVEN (IN BSTEMPE)         *
  !              NBSEXPQ =   11 --> T_E + N_E GIVEN                     *
  !              NBSEXPQ =  101 --> T_E + ZEFF GIVEN                    *
  !              (etc. up to )                                          *
  !              NBSEXPQ = 1111 --> ALL 4 PROFILES GIVEN                *
  !              NBSEXPQ < 0    --> NOT READ ON EXPEQ, BUT WRITTEN ON   *
  !                                 EXPEQ.OUT ACCORDING TO -NBSEXPQ     *
  !                                                                     *
  !     IF NUMBER OF POINTS NEGATIVE => ASSUME FUNCTION OF PSI          *
  !     .                               RATHER THAN SQRT(PSI)           *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZPEOP
  REAL(RKIND)      ::     ZTIP(1)
  REAL(RKIND)      ::     ZTI
  REAL(RKIND)      ::     ZZEFF
  REAL(RKIND)      ::     ZNEP(1)
  REAL(RKIND)      ::     ZNE, ZNI
  REAL(RKIND)      ::     ZTEP(1)
  REAL(RKIND)      ::     ZTE
  REAL(RKIND)      ::     ZWORK
  INTEGER          ::     I
  REAL(RKIND)      ::     ZPSIMN
  REAL(RKIND)      ::     ZDPSI
  INTEGER          ::     L
  INTEGER          ::     INTNZPT1
  INTEGER          ::     NTNZPT1
  INTEGER          ::     ITI
  INTEGER          ::     IZEFF
  INTEGER          ::     INE
  INTEGER          ::     ITE
  INTEGER          ::     KOPT
  DIMENSION ZTE(NPBPS), ZNE(NPBPS), ZNI(NPBPS), ZZEFF(NPBPS), ZTI(NPBPS), &
       &     ZWORK(NPBPS), ZPEOP(NPBPS)
  CHARACTER TEXT*4
  !-----------------------------------------------------------------------
  !
  GO TO (100,200) KOPT
  !
  !     1. DETERMINE PROFILES
  !
100 CONTINUE 
  !
  WRITE(TEXT,'(I4.4)') NBSEXPQ
  READ(TEXT(1:1),'(I1)') ITE
  READ(TEXT(2:2),'(I1)') INE
  READ(TEXT(3:3),'(I1)') IZEFF
  READ(TEXT(4:4),'(I1)') ITI
  !
  !     1.1 READ PROFILES FROM EXPEQ
  !
  !     1.1.1 NUMBER OF POINTS AND CS MESH ARE THE SAME AS FOR P' AND TT'
  !     IF NOT YET READ, DO IT HERE
  !
  IF (NBSEXPQ .GT. 0) THEN
     !
     OPEN(UNIT=NXPQTNZ,ACCESS='SEQUENTIAL',FORM='FORMATTED', &
          &       FILE='EXPTNZ')
     REWIND NXPQTNZ
     !
     !     if NTNZPT1<0, MEANS VERSUS PSI RATHER THAN S AS IN EXPEQ
     !     => CHANGE TO S = SQRT((PSI-PSI0)/(PSIEDGE-PSI0))
     READ(NXPQTNZ,*) NTNZPT1
     INTNZPT1 = NTNZPT1
     NTNZPT1 = ABS(NTNZPT1)
     IF (NTNZPT1 .GT. NPBPS) THEN
       IF (NVERBOSE .GE. 0) WRITE(0,*) ' IN EXPTNZ: NTNZPT1= ',NTNZPT1, &
         &         ' TOO LARGE. SHOULD BE .LE. NPBPS= ',NPBPS
       STOP 'BSEXPEQ'
     ENDIF
     READ(NXPQTNZ,*) (FCSMTNZ(L), L=1,NTNZPT1)
     NTNZPT = NTNZPT1 - 1
     !     CHANGE FROM PSI TO SQRT(PSI) IN [0,1] IF NTNZPT1 WAS NEGATIVE
     IF (INTNZPT1 .LT. 0) THEN
        ZDPSI = FCSMTNZ(NTNZPT1) - FCSMTNZ(1)
        ZPSIMN = FCSMTNZ(1)
        FCSMTNZ(1) = 0.0_RKIND
        DO I=2,NTNZPT1
           FCSMTNZ(I) = SQRT((FCSMTNZ(I)-ZPSIMN)/ZDPSI)
        END DO
     ENDIF
     !
     !     1.1.2 READ THE PROFILES IN THE FOLLOWING ORDER:
     !     .             T_E [EV], N_E [1/M**3], ZEFF, T_I [EV]
     !
     IF (ITE   .EQ. 1) READ(NXPQTNZ,*) (BSTEMPE(L), L=1,NTNZPT+1)
     IF (INE   .EQ. 1) READ(NXPQTNZ,*) (BSDENSE(L), L=1,NTNZPT+1)
     IF (IZEFF .EQ. 1) READ(NXPQTNZ,*) (BSZEFF (L), L=1,NTNZPT+1)
     IF (ITI   .EQ. 1) READ(NXPQTNZ,*) (BSTEMPI(L), L=1,NTNZPT+1)
     ! stop if same s values
     do i=1,NTNZPT
       if ((FCSMTNZ(i+1)-FCSMTNZ(i)) .eq. 0) then
         IF (NVERBOSE .GE. 0) WRITE(0,*) ' s values for point ',i,' and ',i+1,' are the same in bsexpeq'
         stop 'bsexpeq'
       endif
     end do

  ENDIF
  !
  !     1.2 PREPARE SPLINE
  !
  IF (ITE   .EQ. 1)CALL SPLINE(NTNZPT+1,FCSMTNZ,BSTEMPE,BSD2TE , &
       &     ZWORK)
  IF (INE   .EQ. 1)CALL SPLINE(NTNZPT+1,FCSMTNZ,BSDENSE,BSD2NE , &
       &     ZWORK)
  IF (IZEFF .EQ. 1)CALL SPLINE(NTNZPT+1,FCSMTNZ,BSZEFF ,BSD2ZEF, &
       &     ZWORK)
  IF (ITI   .EQ. 1)CALL SPLINE(NTNZPT+1,FCSMTNZ,BSTEMPI,BSD2TI , &
       &     ZWORK)
  !
  CLOSE(NXPQTNZ)
  RETURN
  !
  !     2. WRITE PROFILES ACCORDING TO |NBSEXPQ| (SAME MEANING AS FOR INPUT)
  !
200 CONTINUE 
  !
  IF (NVERBOSE .GE. 1) THEN
    OPEN(UNIT=NXPQTNZ,ACCESS='SEQUENTIAL',FORM='FORMATTED', &
      &       FILE='EXPTNZ.OUT')
    REWIND NXPQTNZ
  END IF
  !
  CALL BSTNZPRO(ZTE,ZTEP,ZNE,ZNEP,ZZEFF,ZTI,ZTIP,ZPEOP,SMISOP1, &
    &       NISO1EFF+1,eqchease_out(1)%profiles_1d%pressure, &
    & eqchease_out(1)%profiles_1d%pressure(1),eqchease_out(1)%profiles_1d%pprime,0)
  ! assumes z_imp=6
  zni=(6._RKIND-ZZEFF(1:NISO1EFF+1))/(6._RKIND-1._RKIND) * ZNE(1:NISO1EFF+1)
  !
  IF (NVERBOSE .GE. 1) THEN 
    WRITE(NXPQTNZ,9210) NISO1EFF+1,'   rhopsi, Te, ne, Zeff, Ti, ni profiles'
    WRITE(NXPQTNZ,9211) (SMISOP1(L),L=1,NISO1EFF+1)
    WRITE(NXPQTNZ,9211) (ZTE(L),  L=1,NISO1EFF+1)
    WRITE(NXPQTNZ,9211) (ZNE(L),  L=1,NISO1EFF+1)
    WRITE(NXPQTNZ,9211) (ZZEFF(L),L=1,NISO1EFF+1)
    WRITE(NXPQTNZ,9211) (ZTI(L),  L=1,NISO1EFF+1)
    WRITE(NXPQTNZ,9211) (ZNI(L),  L=1,NISO1EFF+1)
    CLOSE(NXPQTNZ)
  END IF
  !
  RETURN
9210 FORMAT(I5,A)
9211 FORMAT(1PE18.8)
  !
END SUBROUTINE BSEXPEQ
