!*DECK C2SM15
!*CALL PROCESS
SUBROUTINE OUTNVW
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM15 COMPUTE EQ'S FOR NOVA-W AND PEST (SEE SECTION 5.4.3 OF       *
  !        PUBLICATION                                                  *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     M
  INTEGER          ::     L
  REAL(RKIND)      ::     ZRMJ
  INTEGER          ::     ISMAX
  INTEGER          ::     IMX
  INTEGER          ::     ISMIN
  INTEGER          ::     IMN
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  INTEGER          ::     J7
  REAL(RKIND)      ::     ZBND
  INTEGER          ::     J6
  REAL(RKIND)      ::     ZDT
  INTEGER          ::     IBND
  REAL(RKIND)      ::     ZTMF
  REAL(RKIND)      ::     ZDQ
  REAL(RKIND)      ::     ZQ
  REAL(RKIND)      ::     ZCPPR
  REAL(RKIND)      ::     ZCPR
  REAL(RKIND)      ::     AXX
  INTEGER          ::     NXX
  REAL(RKIND)      ::     SSUM
  REAL(RKIND)      ::     ZJMAG
  REAL(RKIND)      ::     ZJACM0
  REAL(RKIND)      ::     ZJAC0
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZFBP
  REAL(RKIND)      ::     ZFB
  REAL(RKIND)      ::     ZTP
  REAL(RKIND)      ::     ZPSIM
  REAL(RKIND)      ::     ZPSI
  REAL(RKIND)      ::     ZJACM
  REAL(RKIND)      ::     ZZCPM
  REAL(RKIND)      ::     ZRCPM
  REAL(RKIND)      ::     ZJAC
  REAL(RKIND)      ::     ZZCP
  REAL(RKIND)      ::     ZRCP
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZTETCPM
  REAL(RKIND)      ::     ZSIGCPM
  INTEGER          ::     IT0
  REAL(RKIND)      ::     ZTETCP
  REAL(RKIND)      ::     ZSIGCP
  REAL(RKIND)      ::     ZD2TET
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZD2SIG
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZTET
  INTEGER          ::     I3
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZCHIM
  REAL(RKIND)      ::     ZCHI
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZDCHI
  DIMENSION &
       &   NXX(3),                IT0(NPCHI+3), &
       &   ZA1(NTP2),             ZBND(12*NPT), &
       &   ZB1(NTP2),             ZC1(NTP2), &
       &   AXX(5),                ZCHI(NPCHI+3), &
       &   ZCHIM(NPCHI+3),        ZCPR(NPPSNVW), &
       &   ZCPPR(NPPSNVW),        ZDQ(NPPSNVW), &
       &   ZD2SIG(NTP2),          ZD2TET(NTP2), &
       &   ZFB(NPPSNVW),          ZFBP(NPPSNVW), &
       &   ZJAC(NPCHI+3,NPPSNVW), ZJAC0(NPCHI+3), &
       &   ZJACM(NPCHI+3,NPPSNVW),ZJACM0(NPCHI+3), &
       &   ZPSI(NPPSNVW),         ZPSIM(NPPSNVW), &
       &   ZQ(NPPSNVW),           ZR(12*NPT), &
       &   ZRCP(NPCHI+3,NPPSNVW), ZRCPM(NPCHI+3), &
       &   ZSIGCP(NPCHI+3),       ZSIGCPM(NPCHI+3), &
       &   ZTET(NTP2+12*NPT),     ZTETCP(NPCHI+3), &
       &   ZTETCPM(NPCHI+3),      ZTMF(NPPSNVW), &
       &   ZTP(NPPSNVW),          ZZ(12*NPT), &
       &   ZZCP(NPCHI+3,NPPSNVW), ZZCPM(NPCHI+3)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (NSYM .EQ. 1) THEN
     !
     ZDCHI = CPI / REAL(NTNOVA-1)
     !
  ELSE
     !
     ZDCHI = 2._RKIND * CPI / REAL(NTNOVA)
     ! 
  ENDIF
  !
  DO J1=1,NTNOVA+3
     !
     ZCHI(J1)  = (J1 - 3._RKIND)  * ZDCHI
     ZCHIM(J1) = (J1 - 3.5_RKIND) * ZDCHI
     !
  END DO
  !
  ZCHI(1)  = ZCHI(1)  + 2._RKIND * CPI
  ZCHI(2)  = ZCHI(2)  + 2._RKIND * CPI
  ZCHIM(1) = ZCHIM(1) + 2._RKIND * CPI
  ZCHIM(2) = ZCHIM(2) + 2._RKIND * CPI
  ZCHIM(3) = ZCHIM(3) + 2._RKIND * CPI
  !
  DO J4=2,2*NPSI,2
     !
     I3 = J4 / 2  + 1
     !
     CALL DCOPY(NT2,TETMAP(1,J4),1,ZTET,1)
     !
     DO J2=2,NT2
        !
        IF (ZTET(J2) .LT. ZTET(J2-1)) THEN
           !
           ZTET(J2) = ZTET(J2) + 2._RKIND * CPI * (1._RKIND + &
                &                 INT(.5_RKIND * ABS(ZTET(J2) - ZTET(J2-1)) / CPI))       
           !
        ENDIF
        !
     END DO
     !
     CALL SPLCY(CHIN(1,J4),SIGMAP(1,J4),NT1,RC2PI, &
          &              ZD2SIG,ZA1,ZB1,ZC1)
     CALL SPLCYP(CHIN(1,J4),ZTET,NT1,RC2PI,RC2PI, &
          &               ZD2TET,ZA1,ZB1,ZC1)
     !
     ZD2SIG(NT2) = ZD2SIG(1) 
     ZD2TET(NT2) = ZD2TET(1) 
     !
     CALL STCHPS(J4,NTNOVA+3,ZCHI ,ZTET,ZD2TET,SIGMAP(1,J4), &
          &               ZD2SIG, ZSIGCP, ZTETCP, IT0)
     CALL STCHPS(J4,NTNOVA+3,ZCHIM,ZTET,ZD2TET,SIGMAP(1,J4), &
          &               ZD2SIG,ZSIGCPM,ZTETCPM, IT0)
     !
     DO J3=1,NTNOVA+3
        !
        IF (ZTETCP(J3) .GT.CT(NT1)) ZTETCP(J3)  = ZTETCP(J3)  - 2._RKIND*CPI
        IF (ZTETCPM(J3).GT.CT(NT1)) ZTETCPM(J3) = ZTETCPM(J3) - 2._RKIND*CPI
        !
     END DO
     !
     CALL JNOVAW(J4,NTNOVA+3,ZTETCP, ZSIGCP,ZRCP(1,I3),ZZCP(1,I3), &
          &               ZJAC(1,I3))
     CALL JNOVAW(J4,NTNOVA+3,ZTETCPM,ZSIGCPM,ZRCPM,ZZCPM, &
          &               ZJACM(1,I3))
     !
     ZPSI(I3)    = PSIISO(J4)   - CPSRF
     ZPSIM(I3-1) = PSIISO(J4-1) - CPSRF
     ZTP(I3)     = TTP(J4) / TMF(J4)
     ZFB(I3)     = TMF(J4) / QPSI(J4)
     ZFBP(I3)    = (TTP(J4) / (TMF(J4) * QPSI(J4)) - &
          &                 TMF(J4) * CDQ(J4) / QPSI(J4)**2)
     !
  END DO
  !
  CALL RESETR(ZRCP(1,1),NTNOVA+3,RMAG)
  CALL RESETR(ZZCP(1,1),NTNOVA+3,RZMAG)
  !
  DO J5=1,NTNOVA+3
     !
     ZJAC0(J5)  = FCCCC0(ZJAC(J5,2),ZJAC(J5,3), &
          &                       ZJAC(J5,4),ZJAC(J5,5), &
          &                       CS(2),CS(3),CS(4),CS(5),RC0P)
     !
     ZJACM0(J5) = FCCCC0(ZJACM(J5,2),ZJACM(J5,3), &
          &                       ZJACM(J5,4),ZJACM(J5,5), &
          &                       CS(2),CS(3),CS(4),CS(5),RC0P)
     !
  END DO
  !
  ZJMAG = .5_RKIND*(SSUM(NTNOVA+3,ZJAC0,1) + SSUM(NTNOVA+3,ZJACM0,1)) / &
       &           REAL(NTNOVA+3)
  !
  CALL RESETR(ZJAC(1,1), NTNOVA+3,ZJMAG)
  CALL RESETR(ZJACM(1,1),NTNOVA+3,ZJMAG)
  !
  NXX(1) = NTNOVA
  NXX(2) = NPSI1
  NXX(3) = NSYM
  AXX(1) = ZDCHI
  AXX(2) = SQRT(2._RKIND * CPI) / REAL(NPSI,RKIND)
  AXX(3) = RMAG
  AXX(4) = RZMAG
  AXX(5) = TMF(2*NPSI)
  !
  CALL DCOPY(NPSI,CPR(1) ,2,ZCPR,1)
  CALL DCOPY(NPSI,CPPR(2),2,ZCPPR(2),1)
  CALL DCOPY(NPSI,QPSI(2),2,ZQ(2),1)
  CALL DCOPY(NPSI,CDQ(2),2,ZDQ(2),1)
  CALL DCOPY(NPSI,TMF(1),2,ZTMF(1),1)
  ! 
  ZCPPR(1) = DPDP0
  ZQ(1)    = Q0
  ZDQ(1)   = DQDP0
  ZTP(1)   = DTTP0 / T0
  ZFB(1)   = FCCCC0(ZFB(2),ZFB(3),ZFB(4),ZFB(5), &
       &                     CS(2),CS(3),CS(4),CS(5),RC0P)
  ZFBP(1)  = FCCCC0(ZFBP(2),ZFBP(3),ZFBP(4),ZFBP(5), &
       &                     CS(2),CS(3),CS(4),CS(5),RC0P)
  ZPSI(1)  = - CPSRF
  !
  ZTMF(NPSI1) = TMF(2*NPSI)
  !
  IBND = 12 * NT
  !
  ZDT  = 2._RKIND* CPI / REAL(IBND-1,RKIND)
  !
  DO J6=1,IBND
     !
     ZTET(J6) = (J6 - 1._RKIND) * ZDT
     !
  END DO
  !
  CALL BOUND(IBND,ZTET,ZBND)
  !
  DO J7=1,IBND
     !
     ZR(J7) = R0  + ZBND(J7) * COS(ZTET(J7))
     ZZ(J7) = RZ0 + ZBND(J7) * SIN(ZTET(J7))
     !
  END DO
  !
  IMN = ISMIN(IBND,ZR,1)
  IMX = ISMAX(IBND,ZR,1)
  !
  ZRMJ = .5_RKIND * (ZR(IMN) + ZR(IMX))
  !
  CALL DSCAL(NPSI1,ZRMJ,ZFB,1)
  CALL DSCAL(NPSI1,ZRMJ,ZFBP,1)
  !
  OPEN(INP1,FILE='INP1',FORM='UNFORMATTED')
  !
  WRITE(INP1) (NXX(L),L=1,3)
  WRITE(INP1) (AXX(L),L=1,5)
  WRITE(INP1) (ZCPR(L),L=1,NPSI)
  WRITE(INP1) (ZCPPR(L),L=1,NPSI1)
  WRITE(INP1) (ZQ(L),L=1,NPSI1)
  WRITE(INP1) (ZDQ(L),L=1,NPSI1)
  WRITE(INP1) (ZTMF(L),L=1,NPSI1)
  WRITE(INP1) (ZTP(L),L=1,NPSI1)
  WRITE(INP1) (ZFB(L),L=1,NPSI1)
  WRITE(INP1) (ZFBP(L),L=1,NPSI1)
  WRITE(INP1) (ZPSI(L),L=1,NPSI1)
  WRITE(INP1) (ZPSIM(L),L=1,NPSI)
  WRITE(INP1) ((ZRCP(L,M),L=1,NTNOVA+3),M=1,NPSI1)
  WRITE(INP1) ((ZZCP(L,M),L=1,NTNOVA+3),M=1,NPSI1)
  WRITE(INP1) ((ZJACM(L,M),L=1,NTNOVA+3),M=1,NPSI1)
  WRITE(INP1) ((ZJAC(L,M),L=1,NTNOVA+3),M=1,NPSI1)
  !
  CLOSE(INP1,STATUS='KEEP')
  !
  RETURN
END SUBROUTINE OUTNVW
