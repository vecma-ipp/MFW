SUBROUTINE OUTELIT
  !       ###################
  !
  !                                        AUTHOR O. SAUTER, CRPP-EPFL
  !****************************************************************************
  !       INTERFACE FOR CODE ELITE BY H. WILSON, COMPUTING EDGE STABILITY     *
  !                                                                           *
  !       NIDEAL = 8 (ASSUMES SAME MESH AS NIDEAL=6, THUS ERATO MAPPING MESH) *
  !       THUS CAN PACK MESH POINTS. SHOULD USE NER=1, NEGP=-1 FOR EQUAL ARC LENGTH *
  !****************************************************************************
  !
  USE GLOBALS
  USE interpos_module
  IMPLICIT NONE
  !
  INTEGER         ::     I, JS, JSURF, JG, JT, J1, J2, J3, J4, J5, IC, IS0, IT0
  REAL(RKIND)    :: ZCOST, ZDPDS, ZDPDT, ZDRSDT, ZEPS, ZFP, ZGRADP, ZR, ZRHO, ZS, ZS1, &
       &  ZS2, ZSINT, ZT, ZT1, ZT2, ZTETA, ZZ, ZBPOL, ZCOF, ZPSIEDGE, ZDBDS, ZDBDT, ZDBDST, &
       &  ZD2BS2, ZD2BT2, ZPCEL, ZBND
  REAL(RKIND)    :: ZPPRIME, ZD2PDPSI, ZFPSI, ZFFP, ZDFFPDP, ZQCS, ZD2TMP, ZWORK
  REAL(RKIND)    :: ZDUMPSI(NPPSI1), ZDUMPSI2(NPPSI1)
  DIMENSION &
       &   IS0(NPCHI1),       IT0(NPCHI1),       IC(NPCHI1), &
       &   ZBND(NPCHI,5),    ZDBDS(NPCHI,16), &
       &   ZDBDT(NPCHI,16),  ZDBDST(NPCHI,16), ZD2BS2(NPCHI,16), &
       &   ZD2BT2(NPCHI,16), ZPCEL(NPCHI,16),  ZS(NPCHI1), &
       &   ZS1(NPCHI1),       ZS2(NPCHI1),       ZTETA(NPCHI,5), &
       &   ZT(NPCHI1),        ZT1(NPCHI1),       ZT2(NPCHI1)
  DIMENSION & 
       &   ZR(NPPSI1, NPCHI1),  ZZ(NPPSI1, NPCHI1), ZBPOL(NPPSI1, NPCHI1), &
       &   ZPPRIME(NPPSI1), ZD2PDPSI(NPPSI1), ZFPSI(NPPSI1), ZFFP(NPPSI1), &
       &   ZDFFPDP(NPPSI1), ZQCS(NPPSI1), ZD2TMP(NPPSI1), ZWORK(NPPSI1)
  !****************************************************************************
  !
  ! PSI-MESH: NPSI1 POINTS, CHI MESH: NCHI1 = NCHI + 1
  !
  ZEPS  = 1.E-3_RKIND
  !
  ! We are on SMISO(jsurf) thus SMISOP1(jsurf+1)
  DO JSURF=1,NISO1EFF
     !
     ! FIRST COMPUTE VALUES ON CHI, PSI MESH IN THE SAME WAY AS ERDATA (JUST COPIED)
     !
     DO J1=1,NCHI
        !
        ZTETA(J1,1) = TETCHI(J1,JSURF)
        ZTETA(J1,2) = TETCHI(J1,JSURF) - 2._RKIND * ZEPS
        ZTETA(J1,3) = TETCHI(J1,JSURF) -      ZEPS
        ZTETA(J1,4) = TETCHI(J1,JSURF) +      ZEPS
        ZTETA(J1,5) = TETCHI(J1,JSURF) + 2._RKIND * ZEPS
        !
     END DO
     !
     CALL BOUND(NCHI,ZTETA(1,1),ZBND(1,1))
     CALL BOUND(NCHI,ZTETA(1,2),ZBND(1,2))
     CALL BOUND(NCHI,ZTETA(1,3),ZBND(1,3))
     CALL BOUND(NCHI,ZTETA(1,4),ZBND(1,4))
     CALL BOUND(NCHI,ZTETA(1,5),ZBND(1,5))
     !
     CALL RESETI(IC,NCHI,1)
     DO JT = 1,NT1
        DO JG=1,NCHI
           IF (IC(JG).EQ.1) THEN
              IT0(JG) = JT-1
              IF (TETCHI(JG,JSURF).LE.CT(JT)) IC(JG)  = 0
           ENDIF
        ENDDO
     ENDDO
     CALL RESETI(IC,NCHI,1)
     DO JS = 1,NS1
        DO JG=1,NCHI
           IF (IC(JG).EQ.1) THEN
              IS0(JG) = JS-1
              IF (SIGCHI(JG,JSURF).LE.CSIG(JS)) IC(JG)  = 0
           ENDIF
        ENDDO
     ENDDO
3    CONTINUE
!!$            CALL RESETI(IC,NCHI,1)
!!$            DO JT = 1,NT1
!!$               DO JG=1,NCHI
!!$                  IF (IC(JG).EQ.1) THEN
!!$                     IT0(JG) = JT-1
!!$                     IF (TETCHI(JG,JSURF).LE.CT(JT)) IC(JG)  = 0
!!$                  ENDIF 
!!$2              END DO
!!$            END DO
!!$            if (jsurf .eq. npsi1) then
!!$               write(6,'(1p10e12.4)') (TETCHI(JG,JSURF),JG=1,NCHI)
!!$               write(6,'(1p10e12.4)') (CT(JT),JT=1,NT1)
!!$               write(6,'(10I6)') (IC(JG),JG=1,NCHI)
!!$               write(6,'(10I6)') (IT0(JG),JG=1,NCHI)
!!$            endif
!!$            CALL RESETI(IC,NCHI,1)
!!$            DO JS = 1,NS1
!!$               DO JG=1,NCHI
!!$                  IF (IC(JG).EQ.1) THEN
!!$                     IS0(JG) = JS-1
!!$                     IF (SIGCHI(JG,JSURF).LE.CSIG(JS)) IC(JG)  = 0
!!$                  ENDIF
!!$3              END DO
!!$            END DO
!!$            if (jsurf .eq. npsi1) then
!!$               write(6,'(1p10e12.4)') (SIGCHI(JG,JSURF),JG=1,NCHI)
!!$               write(6,'(1p10e12.4)') (CSIG(JT),JT=1,NS1)
!!$               write(6,'(10I6)') (IC(JG),JG=1,NCHI)
!!$               write(6,'(10I6)') (IS0(JG),JG=1,NCHI)
!!$            endif
     !
     DO J4=1,NCHI
        !
        ZT(J4) = TETCHI(J4,JSURF)
        ZS(J4) = SIGCHI(J4,JSURF)
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
     CALL PSICEL(IS0,IT0,NCHI,NPCHI,ZPCEL,CPSICL)
     CALL BASIS3(NCHI,NPCHI,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT, &
          &               ZDBDST,ZD2BS2,ZD2BT2)
     !
     DO J5=1,NCHI
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
        ZFP    = (ZDPDS**2 + (ZDPDT / SIGCHI(J5,JSURF) - ZDPDS * ZDRSDT / &
             &             ZBND(J5,1))**2) / ZBND(J5,1)**2
        ZGRADP = SQRT(ZFP)
        !
        ZCOST        = COS(ZTETA(J5,1))
        ZSINT        = SIN(ZTETA(J5,1))
        ZRHO         = SIGCHI(J5,JSURF) * ZBND(J5,1)
        ZR(JSURF+1,J5) = ZRHO * ZCOST + R0
        ZZ(JSURF+1,J5) = ZRHO * ZSINT + RZ0
        !
        ZBPOL(JSURF+1,J5) = ZGRADP / ZR(JSURF+1,J5)
        !
        ! (NOT NEEDED YET BUT KEPT FOR INFORMATION)               ZJAC   = CP(JSURF) * ZR**NER * ZGRADP**NEGP
        !

        ! J5 END DO
     END DO
     ! COPY SAME POINT AROUND
     ZR(JSURF+1,NCHI1) = ZR(JSURF+1,1)
     ZZ(JSURF+1,NCHI1) = ZZ(JSURF+1,1)
     ZBPOL(JSURF+1,NCHI1) = ZBPOL(JSURF+1,1)
     ! JSURF END DO 
  END DO

  !
  ! WRITE DATA TO FILE NELITE
  !
  ! IN MKSA UNITS: M, T, NM**2, ETC, THUS USE R0EXP AND B0EXP TO TRANSFORM CHEASE UNITS
  !
  OPEN(NELITE,FILE='NELITE',FORM='FORMATTED')
  !
  ! |PSI_EDGE - PSI_AXIS| =  ABS(SPSIM)*B0EXP*R0EXP**2
  !
  ZPSIEDGE = ABS(SPSIM)*B0EXP*R0EXP**2
  WRITE(NELITE,9100) 'PSIA-PSI0'
  WRITE(NELITE,9200) (SMISOP1(I)*SMISOP1(I) * ZPSIEDGE,I=1,NISO1EFF1)
  !
  !  MU0 PPRIME = MU0 P /PSI = MU0 P_CHEASE B0^2/MU0 / (PSI_CHEASE B0 R0^2)
  !  MU0 PPRIME = PPRIME_CHEASE B0 / R0^2
  !
  ZCOF = B0EXP / R0EXP**2
  WRITE(NELITE,9100) 'MU0_PPRIME'
  WRITE(NELITE,9200) (eqchease_out(1)%profiles_1d%pprime(I) * ZCOF,I=1,NISO1EFF1)
  !
  !  MU0 D2P/DPSI^2 = MU0 PPRIME / PSI = PPRIME_CHEASE B0 / R0^2  / (PSI_CHEASE B0 R0^2)
  !  MU0 D2P/DPSI^2 = D2P/DPSI^2_CHEASE / R0^4
  !
  ZDUMPSI=SMISOP1(1:NISO1EFF1)**2 * ABS(SPSIM)
  CALL INTERPOS(ZDUMPSI,eqchease_out(1)%profiles_1d%pprime,nin=NISO1EFF1,TENSION=-0.01_RKIND,YOUTP=ZD2PDPSI, &
       & nbc=(/2, 2/), ybc=(/ eqchease_out(1)%profiles_1d%pprime(1), eqchease_out(1)%profiles_1d%pprime(NISO1EFF1) /))
  ZCOF = 1._RKIND / R0EXP**4
  WRITE(NELITE,9100) 'MU0_D^2P/DPSI^2'
  WRITE(NELITE,9200) (ZD2PDPSI(I) * ZCOF,I=1,NISO1EFF1)
  !
  !  G = G_CHEASE * R0 * B0
  !
  ZCOF = R0EXP * B0EXP
  WRITE(NELITE,9100) 'F(PSI)=R*BPHI'
  WRITE(NELITE,9200) (eqchease_out(1)%profiles_1d%f_dia(I) * ZCOF,I=1,NISO1EFF1)
  !
  !  G GPRIME = GGPRIME_CHEASE * B0
  !
  ZCOF = B0EXP
  WRITE(NELITE,9100) 'F(PSI)DF/DPSI'
  WRITE(NELITE,9200) (eqchease_out(1)%profiles_1d%ffprime(I) * ZCOF,I=1,NISO1EFF1)
  !
  !  D (G GPRIME) / DPSI = [D (G GPRIME) / DPSI]_CHEASE / R0^2
  !
  CALL INTERPOS(ZDUMPSI,eqchease_out(1)%profiles_1d%ffprime,nin=NISO1EFF1,TENSION=-0.01_RKIND,YOUTP=ZDFFPDP, &
       & nbc=(/2, 2/), ybc=(/ eqchease_out(1)%profiles_1d%ffprime(1), eqchease_out(1)%profiles_1d%ffprime(NISO1EFF1) /))
  ZCOF = 1._RKIND / R0EXP**2
  WRITE(NELITE,9100) 'D[F(PSI)*DF/DPSI]/DPSI'
  WRITE(NELITE,9200) (ZDFFPDP(I) * ZCOF,I=1,NISO1EFF1)
  !
  !  Q = Q_CHEASE
  !
  WRITE(NELITE,9100) 'Q(PSI)'
  WRITE(NELITE,9200) (eqchease_out(1)%profiles_1d%q,I=1,NISO1EFF1)
  !
  !  R(PSI,CHI) = R_CHEASE * R0EXP
  !
  ZCOF = R0EXP
  WRITE(NELITE,9100) 'R(PSI,CHI)'
  ZR(1,:) = RMAG
  ZZ(1,:) = RZMAG
  DO I=1,NCHI1
     WRITE(NELITE,9200) (ZR(JS,I)*ZCOF,JS=1,NISO1EFF1)
  END DO
  !
  !  Z(PSI,CHI) = Z_CHEASE * R0EXP
  !
  ZCOF = R0EXP
  WRITE(NELITE,9100) 'Z(PSI,CHI)'
  DO I=1,NCHI1
     WRITE(NELITE,9200) (ZZ(JS,I)*ZCOF,JS=1,NISO1EFF1)
  END DO
  !
  !  BP(PSI,CHI) = |GRAD_PSI|/R_CHEASE * B0EXP*R0EXP**2 / R0EXP^2
  !  BP(PSI,CHI) = |GRAD_PSI|/R_CHEASE * B0EXP
  !
  ZCOF = B0EXP
  ZBPOL(1,:) = 0._RKIND
  WRITE(NELITE,9100) 'BPOL(PSI,CHI)'
  DO I=1,NCHI1
     WRITE(NELITE,9200) (ZBPOL(JS,I)*zcof,JS=1,NISO1EFF1)
  END DO
  !
  ! CHI MESH
  !
  WRITE(NELITE,9100) 'CHI_MESH'
  WRITE(NELITE,9200) (CHI(I),I=1,NCHI1)
  !
  CLOSE(NELITE,STATUS='KEEP')
  !
  RETURN
  !
9100 FORMAT(A)
9200 FORMAT(1P5E20.12)
  !
END SUBROUTINE OUTELIT
