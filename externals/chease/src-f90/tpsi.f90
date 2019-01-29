!*DECK C2SM20
!*CALL PROCESS
SUBROUTINE TPSI(NPMAX,KP,KCASE)
  !        #######################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM20 COMPUTE (SIGMA,THETA) COODINATES OF ALL QUADRATURE AND MESH  *
  !        POINTS REQUIRED BY PENN (SEE SECTION 5.4.5 OF PUBLICATION)   *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     NPMAX
  REAL(RKIND)      ::     ZD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  REAL(RKIND)      ::     ZA
  REAL(RKIND)      ::     ZH
  INTEGER          ::     IT
  INTEGER          ::     J5
  INTEGER          ::     JG
  INTEGER          ::     JT
  INTEGER          ::     IGMAX
  REAL(RKIND)      ::     ZD2TET
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZD2SIG
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZTETN
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZBND
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZTET
  INTEGER          ::     KP
  INTEGER          ::     KCASE
  INTEGER, DIMENSION(:), ALLOCATABLE      :: IC
  INTEGER, DIMENSION(:), ALLOCATABLE      :: IT0
  REAL(RKIND), DIMENSION(:), ALLOCATABLE  :: ZTETA
  DIMENSION &
       &   ZD2SIG(NTP2), ZD2TET(NTP2), ZTET(NTP2),   ZTETN(NTP2), &
       &   ZA1(NTP2),    ZB1(NTP2),    ZC1(NTP2),    ZBND(NTP2)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (KCASE==0) THEN
     IGMAX = NCHI*(NMGAUS+1)
     ALLOCATE(ZTETA(IGMAX),IC(IGMAX),IT0(IGMAX))
     CALL DCOPY(IGMAX,CTPEN,1,ZTETA,1)
  ELSE IF (KCASE==1) THEN
     IGMAX = NTNOVA
     ALLOCATE(ZTETA(IGMAX),IC(IGMAX),IT0(IGMAX))
     CALL DCOPY(IGMAX,CTXT,1,ZTETA,1)
  ELSE IF (KCASE==2) THEN
     IGMAX = NTNOVA*MDT
     ALLOCATE(ZTETA(IGMAX),IC(IGMAX),IT0(IGMAX))
     CALL DCOPY(IGMAX,CTXT_REFINED,1,ZTETA,1)
  ELSE
     PRINT*,'TPSI REQUIRES KCASE=0,1 OR 2'
     STOP
  ENDIF
  !
  CALL DCOPY(NT2,TETMAP(1,KP),1,ZTET,1)
  !
  DO J1=2,NT2
     !
     IF (ZTET(J1) .LT. ZTET(J1-1)) THEN
        !
        ZTET(J1) = ZTET(J1) + 2._RKIND * CPI * (1._RKIND + &
             &                 INT(.5_RKIND * ABS(ZTET(J1) - ZTET(J1-1)) / CPI))       
        !
     ENDIF
     !
  END DO
  !
  CALL BOUND(NT2,ZTET,ZBND)
  !
  DO J2=1,NT2
     !
     ZR        = SIGMAP(J2,KP) * ZBND(J2) * COS(ZTET(J2)) + R0
     ZZ        = SIGMAP(J2,KP) * ZBND(J2) * SIN(ZTET(J2)) + RZ0
     ZTETN(J2) = ATAN2(ZZ - RZMAG,ZR - RMAG)
     !
  END DO
  !
  DO J3=2,NT2
     !
     IF (ZTETN(J3) .LT. ZTETN(J3-1)) THEN
        !
        ZTETN(J3) = ZTETN(J3) + 2._RKIND * CPI * (1._RKIND + &
             &                 INT(.5_RKIND * ABS(ZTETN(J3) - ZTETN(J3-1)) / CPI))       
        !
     ENDIF
     !
  END DO
  !
  CALL SPLCY(ZTETN,SIGMAP(1,KP),NT1,RC2PI,ZD2SIG, &
       &              ZA1,ZB1,ZC1)
  CALL SPLCYP(ZTETN,ZTET,NT1,RC2PI,RC2PI,ZD2TET, &
       &               ZA1,ZB1,ZC1)
  !
  ZD2SIG(NT2) = ZD2SIG(1) 
  ZD2TET(NT2) = ZD2TET(1) 
  !
  CALL RESETI(IC,IGMAX,1)
  DO JT=1,NT2
     DO JG = 1,IGMAX
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (ZTETA(JG).LE.ZTETN(JT)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J5=1,IGMAX
     !
     IT = IT0(J5)
     !
     IF (IT .LT. 1)   IT = 1
     IF (IT .GT. NT1) IT = NT1
     !
     ZH = ZTETN(IT+1) - ZTETN(IT)
     ZA = (ZTETN(IT+1) - ZTETA(J5)) / ZH
     ZB = (ZTETA(J5) - ZTETN(IT)) / ZH
     ZC = (ZA+1) * (ZA-1) * ZH * (ZTETN(IT+1) - ZTETA(J5)) / 6._RKIND
     ZD = (ZB+1) * (ZB-1) * ZH * (ZTETA(J5) - ZTETN(IT)) / 6._RKIND
     ! 
     TETPEN(J5) = ZA*ZTET(IT)   + ZB*ZTET(IT+1) + &
          &                ZC*ZD2TET(IT) + ZD*ZD2TET(IT+1)
     !
     IF (TETPEN(J5) .LT. CT(1)) &
          &                   TETPEN(J5) = TETPEN(J5) + 2._RKIND*CPI
     IF (TETPEN(J5) .GT. CT(NT1)) &
          &                   TETPEN(J5) = TETPEN(J5) - 2._RKIND*CPI
     !
     IF (KP .EQ. NPMAX) THEN
        !
        SIGPEN(J5) = 1._RKIND
        !
     ELSE
        !
        SIGPEN(J5) = ZA*SIGMAP(IT,KP) + ZB*SIGMAP(IT+1,KP) + &
             &                  ZC*ZD2SIG(IT)    + ZD*ZD2SIG(IT+1)
        !
     ENDIF
     !
  END DO
  !
  DEALLOCATE(ZTETA,IC,IT0)
  !
  RETURN
END SUBROUTINE TPSI
