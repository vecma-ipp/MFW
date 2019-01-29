!*DECK C2SU04
!*CALL PROCESS
SUBROUTINE RMRAD(KN,PSIAXE,PSIBND,PT,PAR,PSIG,PTET,KINC)
  !        ########################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SU04  COMPUTE INTERSECTIONS OF CONSTANT FLUX SURFACE WITH        *
  !          Z = RZMAG LINE.
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER       	::  	ISSUM 	! <rmrad.f90>
  INTEGER       	::  	ITEST 	! <rmrad.f90>
  REAL(RKIND)   	::  	PAR 	! <rmrad.f90>
  REAL(RKIND)   	::  	PSIG 	! <rmrad.f90>
  INTEGER       	::  	KINC 	! <rmrad.f90>
  REAL(RKIND)   	::  	PTET 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZTEST 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZFMID 	! <rmrad.f90>
  INTEGER       	::  	J9 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZPSI 	! <rmrad.f90>
  INTEGER       	::  	J8 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZPCEL 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZF 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZT2 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZT1 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZS2 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZS1 	! <rmrad.f90>
  INTEGER       	::  	J7 	! <rmrad.f90>
  INTEGER       	::  	IS0 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZSIGMA 	! <rmrad.f90>
  INTEGER       	::  	JS 	! <rmrad.f90>
  INTEGER       	::  	IT0 	! <rmrad.f90>
  INTEGER       	::  	JG 	! <rmrad.f90>
  INTEGER       	::  	JT 	! <rmrad.f90>
  INTEGER       	::  	IC1 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZBND 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZTETA 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZRHO 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZZ 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZR 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZRTMID 	! <rmrad.f90>
  INTEGER       	::  	J3 	! <rmrad.f90>
  INTEGER       	::  	IC 	! <rmrad.f90>
  REAL(RKIND)   	::  	PSIBND 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZFMAX 	! <rmrad.f90>
  REAL(RKIND)   	::  	PSIAXE 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZFMIN 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZRTMAX 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZRTMIN 	! <rmrad.f90>
  INTEGER       	::  	J1 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZBNDT0 	! <rmrad.f90>
  REAL(RKIND)   	::  	PT 	! <rmrad.f90>
  REAL(RKIND)   	::  	ZEPS 	! <rmrad.f90>
  INTEGER       	::  	KN 	! <rmrad.f90>
  DIMENSION &
       &   IS0(KN),       IT0(KN), &
       &   IC(KN),        IC1(KN), &
       &   PAR(KN),                PSIG((KN-1)*KINC+1), &
       &   PTET((KN-1)*KINC+1),    ZBND(KN), &
       &   ZF(KN,16),     ZFMIN(KN), &
       &   ZFMAX(KN),     ZPCEL(KN,16), &
       &   ZPSI(KN),      ZRHO(KN), &
       &   ZRTMID(KN),    ZRTMIN(KN), &
       &   ZRTMAX(KN),    ZSIGMA(KN), &
       &   ZS1(KN),       ZS2(KN), &
       &   ZTETA(KN), &
       &   ZT1(KN), &
       &   ZT2(KN)     
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  ZEPS = RC1M13
  !
  BPS( 1) = RMAG
  BPS(12) = RZMAG
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  CALL BOUND(1,PT,ZBNDT0)
  !
  DO J1=1,KN
     !
     ZRTMIN(J1) = 0._RKIND
     ZRTMAX(J1) = 1._RKIND
     ZFMIN(J1)  = PSIAXE - PSIISO(J1)
     ZFMAX(J1)  = PSIBND - PSIISO(J1)
     IC(J1)     = 1
     !
     IF (ZFMIN(J1) .EQ. 0._RKIND) THEN
        !
        ZRTMAX(J1) = 0._RKIND
        ZFMAX(J1)  = ZFMIN(J1)
        !
     ELSE IF (ZFMAX(J1) .EQ. 0._RKIND) THEN
        !
        ZRTMIN(J1) = 1._RKIND
        ZFMIN(J1)  = ZFMAX(J1)
        !
     ENDIF
     !
  END DO
  !
  BPS( 1) = R0
  BPS(12) = RZ0
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
2 CONTINUE 
  !
  DO J3=1,KN
     !
     ZRTMID(J3) = .5_RKIND * (ZRTMAX(J3) + ZRTMIN(J3))
     ZR         = ZRTMID(J3) * ZBNDT0 * COS(PT) + RMAG
     ZZ         = ZRTMID(J3) * ZBNDT0 * SIN(PT) + RZMAG
     ZRHO(J3)   = SQRT((ZR - R0)**2 + (ZZ - RZ0)**2)
     ZTETA(J3)  = ATAN2(ZZ - RZ0,ZR - R0)
     !
     IF (ZTETA(J3) .LT. CT(1)) ZTETA(J3) = ZTETA(J3) + 2._RKIND * CPI
     !
  END DO
  !
  CALL BOUND(KN,ZTETA,ZBND)
  !
  CALL RESETI(IC1,KN,1)
  DO JT = 1,NT1
     DO JG=1,KN
        IF (IC1(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (ZTETA(JG).LE.CT(JT)) IC1(JG) = 0
        ENDIF
     ENDDO
  ENDDO
  CALL RESETI(IC1,KN,1)
  DO JS = 1,NS1
     DO JG=1,KN
        IF (IC1(JG).EQ.1) THEN
           ZSIGMA(JG) = ZRHO(JG) / ZBND(JG)
           IS0(JG) = JS-1
           IF (ZSIGMA(JG).LE.CSIG(JS)) IC1(JG) = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J7=1,KN
     IF (IS0(J7) .GT. NS) IS0(J7) = NS
     IF (IS0(J7) .LT. 1)  IS0(J7) = 1
     IF (IT0(J7) .GT. NT) IT0(J7) = NT
     IF (IT0(J7) .LT. 1)  IT0(J7) = 1
     !
     ZS1(J7) = CSIG(IS0(J7))
     ZS2(J7) = CSIG(IS0(J7)+1)
     ZT1(J7) = CT(IT0(J7))
     ZT2(J7) = CT(IT0(J7)+1)
  END DO
  !
  CALL BASIS1(KN,KN,ZS1,ZS2,ZT1,ZT2,ZSIGMA,ZTETA,ZF)
  CALL PSICEL(IS0,IT0,KN,KN,ZPCEL,CPSICL)
  !
  DO J8=1,KN
     !
     ZPSI(J8) = ZPCEL(J8, 1) * ZF(J8, 1) + &
          &              ZPCEL(J8, 2) * ZF(J8, 2) + &
          &              ZPCEL(J8, 3) * ZF(J8, 3) + &
          &              ZPCEL(J8, 4) * ZF(J8, 4) + &
          &              ZPCEL(J8, 5) * ZF(J8, 5) + &
          &              ZPCEL(J8, 6) * ZF(J8, 6) + &
          &              ZPCEL(J8, 7) * ZF(J8, 7) + &
          &              ZPCEL(J8, 8) * ZF(J8, 8) + &
          &              ZPCEL(J8, 9) * ZF(J8, 9) + &
          &              ZPCEL(J8,10) * ZF(J8,10) + &
          &              ZPCEL(J8,11) * ZF(J8,11) + &
          &              ZPCEL(J8,12) * ZF(J8,12) + &
          &              ZPCEL(J8,13) * ZF(J8,13) + &
          &              ZPCEL(J8,14) * ZF(J8,14) + &
          &              ZPCEL(J8,15) * ZF(J8,15) + &
          &              ZPCEL(J8,16) * ZF(J8,16)
     !
  END DO
  !
  DO J9=1,KN
     !
     IF (IC(J9) .NE. 0) THEN
        !
        IF (ZFMIN(J9) .NE. 0._RKIND) THEN
           !
           ZFMID = ZPSI(J9) - PSIISO(J9)
           !
        ELSE
           !
           ZFMID = 0._RKIND
           !
        ENDIF
        !
        ZTEST = .5_RKIND * ABS(ZRTMAX(J9) - ZRTMIN(J9))
        !
        IF (ZTEST .LE. ZEPS) THEN
           !
           IC(J9)               = 0
           PTET((J9-1)*KINC+1)  = ZTETA(J9)
           PSIG((J9-1)*KINC+1)  = ZSIGMA(J9)
           PAR(J9)              = ZRTMID(J9)
           !
        ELSE IF (ZTEST .GT. ZEPS) THEN
           !
           IF (ZFMIN(J9) * ZFMID .LE. 0._RKIND) THEN
              !
              ZRTMAX(J9) = ZRTMID(J9)
              ZFMAX(J9)  = ZFMID
              !
           ELSE
              !
              ZRTMIN(J9) = ZRTMID(J9)
              ZFMIN(J9)  = ZFMID
              !
           ENDIF
        ENDIF
        !
     ENDIF
  END DO
  !
  ITEST = ISSUM(KN,IC,1)
  !
  IF (ITEST .GE. 1) GOTO 2
  !
  RETURN
  !
END SUBROUTINE RMRAD
