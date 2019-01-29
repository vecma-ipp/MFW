!*DECK C2SM07
!*CALL PROCESS
SUBROUTINE GCHI(K)
  !        ##################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM07 INTERPOLATE CHI(S,THETA) AND BETA_CHI(S,THETA) AT GAUSSIAN   *
  !        QUADRATURE POINTS ALONG CONSTANT POLOIDAL FLUX SURFACES.     *
  !        THESE QUANTITIES ARE REQUIRED FOR THE EVALUATION OF THE EQ'S *
  !        FOR MARS                                                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  REAL(RKIND)      ::     ZA
  REAL(RKIND)      ::     ZH
  INTEGER          ::     J4
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZD2BCN
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZD2CHN
  INTEGER          ::     IT
  INTEGER          ::     J2
  INTEGER          ::     J3
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZTET1
  REAL(RKIND)      ::     ZTET
  INTEGER          ::     K
  DIMENSION &
       &   ZA1(NTP2),         ZB1(NTP2),    ZC1(NTP2), &
       &   ZD2BCN(NTP2),      ZD2CHN(NTP2), ZTET(NTP2), &
       &   ZTET1(NTP2*NPMGS)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !
  CALL DCOPY(NT2,TETMAP(1,K),1,ZTET,1)
  CALL DCOPY(NMGAUS*NT1,TETPSI(1,K),1,ZTET1,1)
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
  DO J3=1,NMGAUS
     !
     DO J2=1,NT1
        !
        IT = (J2 - 1) * NMGAUS + J3
        !
        IF (ZTET1(IT) .LT. ZTET(J2)) THEN
           !
           ZTET1(IT) = ZTET1(IT) + 2._RKIND * CPI * (1._RKIND + &
                &                  INT(.5_RKIND * ABS(ZTET1(IT) - ZTET(J2)) / CPI))       
           !
        ELSE IF (ZTET1(IT) .GT. ZTET(J2+1)) THEN
           !
           ZTET1(IT) = ZTET1(IT) - 2._RKIND * CPI * (1._RKIND + &
                &                  INT(.5_RKIND * ABS(ZTET1(IT) - ZTET(J2+1)) / CPI))       
           !
        ENDIF
        !
     END DO
  END DO
  !
  CALL SPLCYP(ZTET,CHIN(1,K),NT1,RC2PI,RC2PI, &
       &               ZD2CHN,ZA1,ZB1,ZC1)
  CALL SPLCY(ZTET,BCHIN(1,K),NT1,RC2PI, &
       &              ZD2BCN,ZA1,ZB1,ZC1)
  !
  ZD2CHN(NT2) = ZD2CHN(1)
  ZD2BCN(NT2) = ZD2BCN(1) 
  !
  DO J5=1,NMGAUS
     !
     DO J4=1,NT1
        !
        IT = (J4 - 1) * NMGAUS + J5
        !
        ZH = ZTET(J4+1) - ZTET(J4)
        ZA = (ZTET(J4+1) - ZTET1(IT)) / ZH
        ZB = (ZTET1(IT) - ZTET(J4)) / ZH
        ZC = (ZA + 1) * (ZA - 1) * ZH * &
             &        (ZTET(J4+1) - ZTET1(IT)) / 6._RKIND
        ZD = (ZB + 1) * (ZB - 1) * ZH * &
             &        (ZTET1(IT) - ZTET(J4)) / 6._RKIND
        ! 
        CHIISO(IT) = ZA * CHIN(J4,K)  + ZB * CHIN(J4+1,K) + &
             &                ZC * ZD2CHN(J4)  + ZD * ZD2CHN(J4+1)
        BCHISO(IT) = ZA * BCHIN(J4,K) + ZB * BCHIN(J4+1,K) + &
             &                ZC * ZD2BCN(J4)  + ZD * ZD2BCN(J4+1)
        !
     END DO
  END DO
  !
  RETURN
END SUBROUTINE GCHI
