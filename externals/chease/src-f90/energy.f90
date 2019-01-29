!*DECK C2SE04
!*CALL PROCESS
SUBROUTINE ENERGY
  !        #################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SE04  COMPUTE AVERAGED POLOIDAL MAGNETIC FIELD ENERGY OF TORUS    *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZJAC
  REAL(RKIND)      ::     ZW
  REAL(RKIND)      ::     ZGRAD2
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZDBDT
  REAL(RKIND)      ::     ZDBDS
  REAL(RKIND)      ::     ZT
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J3
  INTEGER          ::     J7
  REAL(RKIND)      ::     ZPCEL
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     IS
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  INTEGER          ::     IT
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZIBP2
  DIMENSION &
       &   IS(NPT),           IT(NPT), &
       &   ZPCEL(NPT,16), &
       &   ZDBDS(NPT,16),     ZDBDT(NPT,16),     ZS(NPT), &
       &   ZS1(NPT),          ZS2(NPT),          ZT(NPT), &
       &   ZT1(NPT),          ZT2(NPT)        
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !**********************************************************************
  !                                                                     *
  ! 1. INITIALIZATION                                                   *
  !                                                                     *
  !**********************************************************************
  !
  ZIBP2 = 0._RKIND
  !
  DO J1=1,NT
     !
     IT(J1)  = J1
     ZT1(J1) = CT(J1)
     ZT2(J1) = CT(J1+1)
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 2. SCAN OVER ALL CELLS                                              *
  !                                                                     *
  !**********************************************************************
  !
  DO J8=1,NS
     !
     CALL RESETI(IS,NT,J8)
     CALL RESETR(ZS1,NT,CSIG(J8))
     CALL RESETR(ZS2,NT,CSIG(J8+1))
     !
     !**********************************************************************
     !                                                                     *
     ! 2.1. COMPUTE ALL QUANTITIES TO DEFINE PSI ON CELL                   *
     !                                                                     *
     !**********************************************************************
     !
     CALL PSICEL(IS,IT,NT,NPT,ZPCEL,CPSICL)
     !
     !**********************************************************************
     !                                                                     *
     ! 2.2 COMPUTE PSI, R AND SOURCE TERM ON INTEGRATION POINTS            *
     !                                                                     *
     !**********************************************************************
     !
     DO J7=1,NWGAUS
        !
        DO J3=1,NT
           !
           ZS(J3) = RSINT(J8,J7)
           ZT(J3) = RTINT(J3,J7)
           !
        END DO
        !
        CALL BASIS2(NT,NPT,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT)
        !
        DO J4=1,NT
           !
           ZDPDS = ZPCEL(J4, 1) * ZDBDS(J4, 1) + &
                &           ZPCEL(J4, 2) * ZDBDS(J4, 2) + &
                &           ZPCEL(J4, 3) * ZDBDS(J4, 3) + &
                &           ZPCEL(J4, 4) * ZDBDS(J4, 4) + &
                &           ZPCEL(J4, 5) * ZDBDS(J4, 5) + &
                &           ZPCEL(J4, 6) * ZDBDS(J4, 6) + &
                &           ZPCEL(J4, 7) * ZDBDS(J4, 7) + &
                &           ZPCEL(J4, 8) * ZDBDS(J4, 8) + &
                &           ZPCEL(J4, 9) * ZDBDS(J4, 9) + &
                &           ZPCEL(J4,10) * ZDBDS(J4,10) + &
                &           ZPCEL(J4,11) * ZDBDS(J4,11) + &
                &           ZPCEL(J4,12) * ZDBDS(J4,12) + &
                &           ZPCEL(J4,13) * ZDBDS(J4,13) + &
                &           ZPCEL(J4,14) * ZDBDS(J4,14) + &
                &           ZPCEL(J4,15) * ZDBDS(J4,15) + &
                &           ZPCEL(J4,16) * ZDBDS(J4,16)
           !
           ZDPDT = ZPCEL(J4, 1) * ZDBDT(J4, 1) + &
                &           ZPCEL(J4, 2) * ZDBDT(J4, 2) + &
                &           ZPCEL(J4, 3) * ZDBDT(J4, 3) + &
                &           ZPCEL(J4, 4) * ZDBDT(J4, 4) + &
                &           ZPCEL(J4, 5) * ZDBDT(J4, 5) + &
                &           ZPCEL(J4, 6) * ZDBDT(J4, 6) + &
                &           ZPCEL(J4, 7) * ZDBDT(J4, 7) + &
                &           ZPCEL(J4, 8) * ZDBDT(J4, 8) + &
                &           ZPCEL(J4, 9) * ZDBDT(J4, 9) + &
                &           ZPCEL(J4,10) * ZDBDT(J4,10) + &
                &           ZPCEL(J4,11) * ZDBDT(J4,11) + &
                &           ZPCEL(J4,12) * ZDBDT(J4,12) + &
                &           ZPCEL(J4,13) * ZDBDT(J4,13) + &
                &           ZPCEL(J4,14) * ZDBDT(J4,14) + &
                &           ZPCEL(J4,15) * ZDBDT(J4,15) + &
                &           ZPCEL(J4,16) * ZDBDT(J4,16) 
           !
           ZR     = RSINT(J8,J7) * YRST(J4,J7) * COS(RTINT(J4,J7)) + R0
           ZGRAD2 = (ZDPDS**2 + (ZDPDT / RSINT(J8,J7) - &
                &             ZDPDS * YDRSDT(J4,J7) / YRST(J4,J7))**2) / &
                &             YRST(J4,J7)**2
           ZW     = CW(J7) * (CSIG(J8+1) - CSIG(J8)) * &
                &                     (CT(J4+1) - CT(J4))
           ZJAC   = YRST(J4,J7)**2 * RSINT(J8,J7)
           ZIBP2  = ZIBP2 + ZW * ZJAC * ZGRAD2 / ZR
           !
        END DO
     END DO
  END DO
  !
  WMAGP = .5_RKIND  * ZIBP2
  !
  RETURN
END SUBROUTINE ENERGY
