!*DECK C2SM23
!*CALL PROCESS
SUBROUTINE VACUFFT(KMMAX)
  !        #########################
  !
  !                                        AUTHOR O. SAUTER, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM23  COMPUTE EQV TERMS FOR MARS USING FAST FFT TRANSFORM ON AN   *
  !         2**L EQUIDISTANT CHI-MESH. CUBIC SPLINE INTERPOLATION OF THE*
  !        TERMS FROM GAUSSIAN THETA-MESH TO EQUIDISTANT CHI-MESH.      *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZFFTJAC
  INTEGER          ::     IM2P
  INTEGER          ::     IM2
  INTEGER          ::     KMMAX
  INTEGER          ::     IMP1
  INTEGER          ::     IV
  REAL(RKIND)      ::     ZFFTG12
  REAL(RKIND)      ::     ZFFTG33
  REAL(RKIND)      ::     ZFFTG22
  REAL(RKIND)      ::     ZFFTG11
  REAL(RKIND)      ::     ZWORK
  REAL(RKIND)      ::     ZD2FUN
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J9
  REAL(RKIND)      ::     ZN
  REAL(RKIND)      ::     ZJAC1
  REAL(RKIND)      ::     ZG33
  REAL(RKIND)      ::     ZG12
  REAL(RKIND)      ::     ZG22
  REAL(RKIND)      ::     ZG11
  REAL(RKIND)      ::     ZZW
  REAL(RKIND)      ::     ZRW
  REAL(RKIND)      ::     ZDZDC
  REAL(RKIND)      ::     ZDRDC
  REAL(RKIND)      ::     ZDPDZ
  REAL(RKIND)      ::     ZDPDR
  REAL(RKIND)      ::     ZDTDZ
  REAL(RKIND)      ::     ZDSDZ
  REAL(RKIND)      ::     ZDTDR
  REAL(RKIND)      ::     ZDSDR
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
  REAL(RKIND)      ::     ZD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  REAL(RKIND)      ::     ZA
  REAL(RKIND)      ::     ZH
  INTEGER          ::     ICHIISO
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     ICHISO
  INTEGER          ::     IGCHISO
  REAL(RKIND)      ::     ZCHIFFT
  INTEGER          ::     I
  REAL(RKIND)      ::     ZDCHI
  INTEGER          ::     INCHI
  REAL(RKIND)      ::     ZLOG2
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     IEDGE
  DIMENSION &
       &     ZBND(NPMGS*NTP1,4), ZS(2*NPV1), ZTETA(NPMGS*NTP1,4), &
       &     ICHIISO(2*NPCHI1), ZCHIFFT(2*NPCHI1), ZD2FUN(NPMGS*NTP1), &
       &     ZWORK(NPMGS*NTP1,3), &
       &     ZA(2*NPCHI1), ZB(2*NPCHI1), ZC(2*NPCHI1), ZD(2*NPCHI1), &
       &     ZFFTG11(2*NPCHI1), ZFFTG22(2*NPCHI1), ZFFTG33(2*NPCHI1), &
       &     ZFFTG12(2*NPCHI1), ZFFTJAC(2*NPCHI1), &
       &     ZG11(NPMGS*NTP1), ZG22(NPMGS*NTP1), ZG33(NPMGS*NTP1), &
       &     ZG12(NPMGS*NTP1), ZJAC1(NPMGS*NTP1)
  !
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IEDGE = 2*NPSI
  ZEPS = 1.E-3_RKIND
  !
  !     PREPARE COEFFICIENTS FOR FFT AND CUBIC SPLINE
  !
  ZLOG2 = LOG(1._RKIND*NCHI)/LOG(2._RKIND)
  INCHI = 2**NINT(ZLOG2)
  IF (INCHI .GE. 2*NPCHI1) INCHI = INCHI/2
  ZDCHI = 2._RKIND * CPI / REAL(INCHI,RKIND)
  DO I=1,INCHI+1
     ZCHIFFT(I) = REAL(I-1,RKIND)*ZDCHI
  ENDDO
  !
  IGCHISO = NMGAUS*NT1
  !     ASSUMES THAT CHIISO HAS ALREADY ARRAY AT BOUNDARY (FROM CALL FOURFFT)
  !     (OTHERWISE, ADD CALL GCHI(2*NPSI))
  DO I=1,INCHI
     ICHISO = ISRCHFGE(IGCHISO,CHIISO,1,ZCHIFFT(I)) - 1
     !
     IF (ICHISO .LT. 1) THEN
        ICHISO = 1
     ELSE IF (ICHISO .GT. IGCHISO) THEN
        ICHISO = IGCHISO
     ENDIF
     ICHIISO(I) = ICHISO
     !
     ZH = CHIISO(ICHISO+1) - CHIISO(ICHISO)
     ZA(I) = (CHIISO(ICHISO+1) - ZCHIFFT(I)) / ZH
     ZB(I) = (ZCHIFFT(I) - CHIISO(ICHISO)) / ZH
     ZC(I) = (ZA(I) + 1) * (ZA(I) - 1) * ZH * &
          &       (CHIISO(ICHISO+1) - ZCHIFFT(I)) / 6._RKIND
     ZD(I) = (ZB(I) + 1) * (ZB(I) - 1) * ZH * &
          &       (ZCHIFFT(I) - CHIISO(ICHISO)) / 6._RKIND
     !
  ENDDO
  !
  !     PRECOMPUTE SOME ARRAYS
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
  !     PRECOMPUTE TERMS NON-DEPENDING ON VACUUM S VALUE
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
     ZDCDT = ZRHO * ZBNDT * ZR / (ZJAC * ZDPDS)
     !
     ZDSDR = (ZDRSDT * ZSINT + ZBNDT * ZCOST) / ZBNDT**2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBNDT * ZSINT - ZDRSDT * ZCOST) / ZBNDT**2
     ZDTDZ = ZCOST / ZRHO
     !
     ZDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
     ZDRDC = - ZJAC * ZDPDZ / ZR
     ZDZDC = ZJAC * ZDPDR / ZR
     !
     ZRW = ZR - R0W
     ZZW = ZZ - RZ0W
     ZG11(J3) = ZRW**2 + ZZW**2
     ZG22(J3) = ZDRDC**2 + ZDZDC**2
     ZG12(J3) = ZRW * ZDRDC + ZZW * ZDZDC
     ZG33(J3) = ZRW
     ZJAC1(J3)= ZRW * ZDZDC - ZZW * ZDRDC
     !
  END DO
  !
  !     AT EACH S VACUUM LOCATION: COMPUTE TERMS ON GAUSSIAN MESH, INTERPOLATE
  !     THEM ON EQUIDISTANT CHI-MESH AND COMPUTE FOURIER TRANSFORM
  !
  ZN = 1._RKIND / REAL(INCHI,RKIND)
  DO J9=1,2*NV+1
     ZS1   = ZS(J9)
     !
     !     USE ZBND AND ZTETA MEMORY SPACE
     DO J8=1,NT1*NMGAUS
        !
        ZTETA(J8,1) = ZS1 * (R0W + ZS1 * ZG33(J8)) * ZJAC1(J8)
        ZBND (J8,1) = ZG11(J8) / ZTETA(J8,1)
        ZBND (J8,2) = ZS1**2 * ZG22(J8) / ZTETA(J8,1)
        ZBND (J8,3) = (R0W + ZS1 * ZG33(J8))**2 / ZTETA(J8,1)
        ZBND (J8,4) = ZS1  * ZG12(J8) / ZTETA(J8,1)
        !
     END DO
     !
     !     CUBIC SPLINE AND FFT
     !
     CALL SPLIFFT(CHIISO,ZBND (1,1),IGCHISO,RC2PI,ZD2FUN, &
          &     ZWORK,ZFFTG11,INCHI,ICHIISO,ZA,ZB,ZC,ZD)
     CALL SPLIFFT(CHIISO,ZBND (1,2),IGCHISO,RC2PI,ZD2FUN, &
          &     ZWORK,ZFFTG22,INCHI,ICHIISO,ZA,ZB,ZC,ZD)
     CALL SPLIFFT(CHIISO,ZBND (1,3),IGCHISO,RC2PI,ZD2FUN, &
          &     ZWORK,ZFFTG33,INCHI,ICHIISO,ZA,ZB,ZC,ZD)
     CALL SPLIFFT(CHIISO,ZBND (1,4),IGCHISO,RC2PI,ZD2FUN, &
          &     ZWORK,ZFFTG12,INCHI,ICHIISO,ZA,ZB,ZC,ZD)
     !C         CALL SPLIFFT(CHIISO,ZTETA(1,1),IGCHISO,RC2PI,ZD2FUN,
     !C     +     ZWORK,ZFFTJAC,INCHI,ICHIISO,ZA,ZB,ZC,ZD)
     !
     !     COPY SPECTRUM FOR M=0,KMMAX
     !
     IF (MOD(J9,2) .EQ. 1) THEN
        IV = (J9 + 1) / 2
        DO IMP1=1,KMMAX+1
           IM2 = 2*IMP1 - 1
           IM2P = IM2 + 1
           !
           DG11LV(IV,IMP1) = CMPLX(ZFFTG11(IM2),-ZFFTG11(IM2P)) * ZN
           DG22LV(IV,IMP1) = CMPLX(ZFFTG22(IM2),-ZFFTG22(IM2P)) * ZN
           DG33LV(IV,IMP1) = CMPLX(ZFFTG33(IM2),-ZFFTG33(IM2P)) * ZN
           DG12LV(IV,IMP1) = CMPLX(ZFFTG12(IM2),-ZFFTG12(IM2P)) * ZN
           !C             VJACOB(IV,IMP1) = CMPLX(ZFFTJAC(IM2),-ZFFTJAC(IM2P)) * ZN
        ENDDO
     ELSE IF (MOD(J9,2) .EQ. 0) THEN
        IV = J9 / 2
        DO IMP1=1,KMMAX+1
           IM2 = 2*IMP1 - 1
           IM2P = IM2 + 1
           !
           DG11LMV(IV,IMP1) = CMPLX(ZFFTG11(IM2),-ZFFTG11(IM2P)) * ZN
           DG22LMV(IV,IMP1) = CMPLX(ZFFTG22(IM2),-ZFFTG22(IM2P)) * ZN
           DG33LMV(IV,IMP1) = CMPLX(ZFFTG33(IM2),-ZFFTG33(IM2P)) * ZN
           DG12LMV(IV,IMP1) = CMPLX(ZFFTG12(IM2),-ZFFTG12(IM2P)) * ZN
           !C             VJACOM(IV,IMP1)  = CMPLX(ZFFTJAC(IM2),-ZFFTJAC(IM2P)) * ZN
        ENDDO
     ENDIF
     !
  END DO
  !
  RETURN
END SUBROUTINE VACUFFT
