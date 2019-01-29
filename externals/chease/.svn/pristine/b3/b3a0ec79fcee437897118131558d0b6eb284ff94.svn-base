!*DECK C2SM17
!*CALL PROCESS
SUBROUTINE JNOVAW(KP,N,PTETCP,PSIGCP,PR,PZ,PJAC)
  !        ################################################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM17 EVALUATE JACOBIAN (EQ. (14) OF PUBLICATION) AT (S,CHI) NODES *
  !        REQUIRED BY NOVA-W AND PEST                                  *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     KP
  REAL(RKIND)      ::     PJAC
  REAL(RKIND)      ::     PZ
  REAL(RKIND)      ::     PR
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZSINT
  REAL(RKIND)      ::     ZCOST
  REAL(RKIND)      ::     ZGRADP
  REAL(RKIND)      ::     ZFP
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  REAL(RKIND)      ::     ZDRSDT
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZDBDT
  REAL(RKIND)      ::     ZDBDS
  REAL(RKIND)      ::     ZPCEL
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     ZS
  REAL(RKIND)      ::     ZT
  INTEGER          ::     J4
  REAL(RKIND)      ::     PSIGCP
  INTEGER          ::     IS0
  INTEGER          ::     JS
  INTEGER          ::     IT0
  INTEGER          ::     JG
  INTEGER          ::     JT
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     PTETCP
  REAL(RKIND)      ::     ZTETA
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     N
  DIMENSION &
       &   IS0(NPCHI+3),       IT0(NPCHI+3),       IC(NPCHI+3), &
       &   PSIGCP(N),          PTETCP(N), &
       &   PR(N), &
       &   PJAC(N),            PZ(N), &
       &   ZBND(NPCHI+3,5),    ZDBDS(NPCHI+3,16), &
       &   ZDBDT(NPCHI+3,16),  ZPCEL(NPCHI+3,16), &
       &   ZS(NPCHI+3),        ZS1(NPCHI+3), &
       &   ZS2(NPCHI+3),       ZTETA(NPCHI+3,5), &
       &   ZT(NPCHI+3),        ZT1(NPCHI+3), &
       &   ZT2(NPCHI+3)
  !
  ZEPS  = 1.E-3_RKIND
  !
  DO J1=1,N
     !
     ZTETA(J1,1) = PTETCP(J1)
     ZTETA(J1,2) = PTETCP(J1) - 2._RKIND * ZEPS
     ZTETA(J1,3) = PTETCP(J1) -      ZEPS
     ZTETA(J1,4) = PTETCP(J1) +      ZEPS
     ZTETA(J1,5) = PTETCP(J1) + 2._RKIND * ZEPS
     !
  END DO
  !
  CALL BOUND(N,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(N,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(N,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(N,ZTETA(1,4),ZBND(1,4))
  CALL BOUND(N,ZTETA(1,5),ZBND(1,5))
  !
  CALL RESETI(IC,N,1)
  DO JT = 1,NT1
     DO JG=1,N
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (PTETCP(JG).LE.CT(JT)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  CALL RESETI(IC,N,1)
  DO JS = 1,NS1
     DO JG=1,N
        IF (IC(JG).EQ.1) THEN
           IS0(JG) = JS-1
           IF (PSIGCP(JG).LE.CSIG(JS)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J4=1,N
     !
     ZT(J4) = PTETCP(J4)
     ZS(J4) = PSIGCP(J4)
     !
     IF (IS0(J4) .GT. NS) IS0(J4) = NS
     IF (IS0(J4) .LT. 1)  IS0(J4) = 1
     IF (IT0(J4) .GT. NT) IT0(J4) = NT
     IF (IT0(J4) .LT. 1)  IT0(J4) = 1
     !
     ZS1(J4) = CSIG(IS0(J4))
     ZS2(J4) = CSIG(IS0(J4)+1)
     ZT1(J4) = CT(IT0(J4))
     ZT2(J4) = CT(IT0(J4)+1)
     !
  END DO
  !
  CALL PSICEL(IS0,IT0,N,NPCHI+3,ZPCEL,CPSICL)
  CALL BASIS2(N,NPCHI+3,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT)
  !
  DO J5=1,N
     !
     ZDRSDT = (ZBND(J5,2) + 8*(ZBND(J5,4) - ZBND(J5,3)) - &
          &             ZBND(J5,5)) / (12._RKIND * ZEPS)
     !
     ZDPDS = ZDBDS(J5, 1) * ZPCEL(J5, 1) + &
          &           ZDBDS(J5, 2) * ZPCEL(J5, 2) + &
          &           ZDBDS(J5, 3) * ZPCEL(J5, 3) + &
          &           ZDBDS(J5, 4) * ZPCEL(J5, 4) + &
          &           ZDBDS(J5, 5) * ZPCEL(J5, 5) + &
          &           ZDBDS(J5, 6) * ZPCEL(J5, 6) + &
          &           ZDBDS(J5, 7) * ZPCEL(J5, 7) + &
          &           ZDBDS(J5, 8) * ZPCEL(J5, 8) + &
          &           ZDBDS(J5, 9) * ZPCEL(J5, 9) + &
          &           ZDBDS(J5,10) * ZPCEL(J5,10) + &
          &           ZDBDS(J5,11) * ZPCEL(J5,11) + &
          &           ZDBDS(J5,12) * ZPCEL(J5,12) + &
          &           ZDBDS(J5,13) * ZPCEL(J5,13) + &
          &           ZDBDS(J5,14) * ZPCEL(J5,14) + &
          &           ZDBDS(J5,15) * ZPCEL(J5,15) + &
          &           ZDBDS(J5,16) * ZPCEL(J5,16)
     !
     ZDPDT = ZDBDT(J5, 1) * ZPCEL(J5, 1) + &
          &           ZDBDT(J5, 2) * ZPCEL(J5, 2) + &
          &           ZDBDT(J5, 3) * ZPCEL(J5, 3) + &
          &           ZDBDT(J5, 4) * ZPCEL(J5, 4) + &
          &           ZDBDT(J5, 5) * ZPCEL(J5, 5) + &
          &           ZDBDT(J5, 6) * ZPCEL(J5, 6) + &
          &           ZDBDT(J5, 7) * ZPCEL(J5, 7) + &
          &           ZDBDT(J5, 8) * ZPCEL(J5, 8) + &
          &           ZDBDT(J5, 9) * ZPCEL(J5, 9) + &
          &           ZDBDT(J5,10) * ZPCEL(J5,10) + &
          &           ZDBDT(J5,11) * ZPCEL(J5,11) + &
          &           ZDBDT(J5,12) * ZPCEL(J5,12) + &
          &           ZDBDT(J5,13) * ZPCEL(J5,13) + &
          &           ZDBDT(J5,14) * ZPCEL(J5,14) + &
          &           ZDBDT(J5,15) * ZPCEL(J5,15) + &
          &           ZDBDT(J5,16) * ZPCEL(J5,16)
     !
     ZFP    = (ZDPDS**2 + (ZDPDT / PSIGCP(J5) - ZDPDS * ZDRSDT / &
          &             ZBND(J5,1))**2) / ZBND(J5,1)**2
     ZGRADP = SQRT(ZFP)
     !
     ZCOST = COS(ZTETA(J5,1))
     ZSINT = SIN(ZTETA(J5,1))
     !
     ZRHO   = PSIGCP(J5) * ZBND(J5,1)
     PR(J5) = ZRHO * ZCOST + R0
     PZ(J5) = ZRHO * ZSINT + RZ0
     !
     PJAC(J5) = CP(KP) * PR(J5)**NER * ZGRADP**NEGP
     !
  END DO
  !
  RETURN
END SUBROUTINE JNOVAW
