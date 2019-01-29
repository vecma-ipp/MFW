!*DECK C2SM13
!*CALL PROCESS
SUBROUTINE VACUUM(KM)
  !        #####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM13 COMPUTE VACUUM EQ'S FOR MARS (SEE SECTION 5.4.2 AND TABLE 2  *
  !        IN PUBLICATION)                                              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZJAC1
  REAL(RKIND)      ::     G33L
  REAL(RKIND)      ::     G12L
  REAL(RKIND)      ::     G22L
  REAL(RKIND)      ::     G11L
  INTEGER          ::     KM
  REAL(RKIND)      ::     ZARG
  REAL(RKIND)      ::     ZPWGT
  REAL(RKIND)      ::     ZDZDC
  REAL(RKIND)      ::     ZDRDC
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     IG
  INTEGER          ::     J7
  INTEGER          ::     J8
  INTEGER          ::     J9
  REAL(RKIND)      ::     ZDZ1DC
  REAL(RKIND)      ::     ZDR1DC
  REAL(RKIND)      ::     ZDPDZ
  REAL(RKIND)      ::     ZDPDR
  REAL(RKIND)      ::     ZDTDZ
  REAL(RKIND)      ::     ZDSDZ
  REAL(RKIND)      ::     ZDTDR
  REAL(RKIND)      ::     ZDSDR
  REAL(RKIND)      ::     ZZ1
  REAL(RKIND)      ::     ZR1
  REAL(RKIND)      ::     ZDCDT
  REAL(RKIND)      ::     ZJAC
  REAL(RKIND)      ::     ZDRSDT
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  REAL(RKIND)      ::     ZGP
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZBNDT
  REAL(RKIND)      ::     ZSINT
  REAL(RKIND)      ::     ZCOST
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZTETA
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     IEDGE
  DIMENSION &
       &   ZBND(NPMGS*NTP1,4), ZDCDT(NPMGS*NTP1), &
       &   ZDR1DC(NPMGS*NTP1), ZDZ1DC(NPMGS*NTP1), &
       &   ZR1(NPMGS*NTP1),    ZZ1(NPMGS*NTP1), &
       &   ZS(2*NPV1),         ZTETA(NPMGS*NTP1,4)
  !
  COMPLEX(ckind)     ZCG11,ZCG22,ZCG33,ZCG12,ZCDGIJ,ZIW
  !        COMPLEX(ckind)     ZCGJA
  DIMENSION &
       &   ZCG11(2*NPV1),      ZCG22(2*NPV1),     ZCG33(2*NPV1), &
       &   ZCG12(2*NPV1),      ZCDGIJ(5)
  !        DIMENSION           ZCGJA(2*NPV1)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IEDGE = 2*NPSI
  ZEPS = 1.E-3_RKIND
  !
  DO J1=1,NV
     !
     ZS(2*(J1-1)+1) = CSV(J1)
     ZS(2*J1      ) = CSMV(J1)
     !
  END DO
  !
  ZS(2*NV+1) = CSV(NV1)
  !
  DO J2=1,NMGAUS*NT1
     !
     ZTETA(J2,1) = TETPSI(J2,IEDGE) - 2._RKIND * ZEPS
     ZTETA(J2,2) = TETPSI(J2,IEDGE) -      ZEPS
     ZTETA(J2,3) = TETPSI(J2,IEDGE) +      ZEPS
     ZTETA(J2,4) = TETPSI(J2,IEDGE) + 2._RKIND * ZEPS
     !
  END DO
  !
  CALL BOUND(NMGAUS*NT1,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(NMGAUS*NT1,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(NMGAUS*NT1,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(NMGAUS*NT1,ZTETA(1,4),ZBND(1,4))
  !
  DO J3=1,NMGAUS*NT1
     !
     ZCOST = COS(TETPSI(J3,IEDGE))
     ZSINT = SIN(TETPSI(J3,IEDGE))
     ZBNDT = BNDISO(J3,IEDGE)
     ZRHO  = ZBNDT
     ZR    = RRISO(J3,IEDGE)
     ZZ    = RZISO(J3,IEDGE)
     ZGP   = GPISO(J3,IEDGE)
     ZDPDS = DPSISO(J3,IEDGE)
     ZDPDT = 0._RKIND
     !
     ZDRSDT = (ZBND(J3,1) + 8*(ZBND(J3,3) - ZBND(J3,2)) - &
          &             ZBND(J3,4)) / (12._RKIND * ZEPS)
     ZJAC   = CP(IEDGE) * ZR**NER * ZGP**NEGP
     !
     ZDCDT(J3) = ZRHO * ZBNDT * ZR / (ZJAC * ZDPDS)
     ZR1(J3)   = ZR - R0W
     ZZ1(J3)   = ZZ - RZ0W
     !
     ZDSDR = (ZDRSDT * ZSINT + ZBNDT * ZCOST) / ZBNDT**2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBNDT * ZSINT - ZDRSDT * ZCOST) / ZBNDT**2
     ZDTDZ = ZCOST / ZRHO
     !
     ZDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
     ZDR1DC(J3) = - ZJAC * ZDPDZ / ZR
     ZDZ1DC(J3) = ZJAC * ZDPDR / ZR
     !
  END DO
  !
  CALL CVZERO(ZCG11,2*NV1)
  CALL CVZERO(ZCG22,2*NV1)
  CALL CVZERO(ZCG33,2*NV1)
  CALL CVZERO(ZCG12,2*NV1)
  !        CALL CVZERO(ZCGJA,2*NV1)
  !
  DO J9=1,2*NV+1
     !
     DO J8=1,NT1
        !
        CALL CVZERO(ZCDGIJ,5)
        !
        DO J7=1,NMGAUS
           !
           IG = (J8-1)*NMGAUS+J7
           !
           ZR    = ZR1(IG)
           ZZ    = ZZ1(IG)
           ZS1   = ZS(J9)
           ZDRDC = ZDR1DC(IG)
           ZDZDC = ZDZ1DC(IG)
           ZPWGT = WGTPSI(IG,IEDGE)
           ZARG  = REAL(KM,RKIND) * CHIISO(IG)
           ZIW   = (0.5_RKIND/CPI * ZDCDT(IG) * ZPWGT) &
                &         *  CMPLX(COS(ZARG),-SIN(ZARG))
           !
           G11L =           ZR**2 + ZZ**2
           G22L = ZS1**2 * (ZDRDC**2 + ZDZDC**2)
           G12L =   ZS1  * (ZR * ZDRDC + ZZ * ZDZDC)
           G33L =          (R0W + ZS1 * ZR)**2
           ZJAC1 = ZS1 * (R0W + ZS1 * ZR) * (ZR * ZDZDC - ZZ * ZDRDC)
           !
           ZCDGIJ(1) = ZCDGIJ(1) + ZIW * G11L / ZJAC1
           ZCDGIJ(2) = ZCDGIJ(2) + ZIW * G22L / ZJAC1
           ZCDGIJ(3) = ZCDGIJ(3) + ZIW * G33L / ZJAC1
           ZCDGIJ(4) = ZCDGIJ(4) + ZIW * G12L / ZJAC1
           !        ZCDGIJ(5) = ZCDGIJ(5) + ZIW * ZJAC1
           !
        END DO
        !
        ZCG11(J9) = ZCG11(J9) + ZCDGIJ(1)
        ZCG22(J9) = ZCG22(J9) + ZCDGIJ(2)
        ZCG33(J9) = ZCG33(J9) + ZCDGIJ(3)
        ZCG12(J9) = ZCG12(J9) + ZCDGIJ(4)
        !        ZCGJA(J9) = ZCGJA(J9) + ZCDGIJ(5)
        !
     END DO
  END DO
  !
  CALL CCOPY(NV1,ZCG11(1),2,DG11LV(1,KM+1),1)
  CALL CCOPY(NV,ZCG11(2),2,DG11LMV(1,KM+1),1)
  CALL CCOPY(NV1,ZCG22(1),2,DG22LV(1,KM+1),1)
  CALL CCOPY(NV,ZCG22(2),2,DG22LMV(1,KM+1),1)
  CALL CCOPY(NV1,ZCG33(1),2,DG33LV(1,KM+1),1)
  CALL CCOPY(NV,ZCG33(2),2,DG33LMV(1,KM+1),1)
  CALL CCOPY(NV1,ZCG12(1),2,DG12LV(1,KM+1),1)
  CALL CCOPY(NV,ZCG12(2),2,DG12LMV(1,KM+1),1)
  !        CALL CCOPY(NV1,ZCGJA(1),2,VJACOB(1,KM+1),1)
  !        CALL CCOPY(NV,ZCGJA(2),2,VJACOM (1,KM+1),1)
  !
  RETURN
END SUBROUTINE VACUUM
