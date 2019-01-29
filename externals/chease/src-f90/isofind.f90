!*DECK C2SU01
!*CALL PROCESS
SUBROUTINE ISOFIND(K1,K2,PSIGMA,PTETA,PGWGT,PSIAXE,PSIBND)
  !        ###########################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SU01  LEAD TRACING OF CONSTANT POLOIDAL FLUX SURFACES            *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     PSIGMA
  REAL(RKIND)      ::     ZSIG
  REAL(RKIND)      ::     PSIBND
  REAL(RKIND)      ::     PSIAXE
  INTEGER          ::     NROOT
  REAL(RKIND)      ::     ZTET
  INTEGER          ::     JPLEN
  INTEGER          ::     JK2
  INTEGER          ::     JK1
  INTEGER          ::     JPSTART
  INTEGER          ::     IKTET
  REAL(RKIND)      ::     PGWGT
  REAL(RKIND)      ::     PTETA
  INTEGER          ::     IG
  REAL(RKIND)      ::     ZTADD
  REAL(RKIND)      ::     ZTDIF
  INTEGER          ::     JG
  REAL(RKIND)      ::     ZPISO
  INTEGER          ::     IT
  REAL(RKIND)      ::     ZC
  INTEGER          ::     IT0
  INTEGER          ::     JP
  INTEGER          ::     JT
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZWGT
  REAL(RKIND)      ::     ZRAC
  INTEGER          ::     K1
  INTEGER          ::     K2
  INTEGER          ::     KP
  DIMENSION &
       &   IC(K2),                  IT0(K2), &
       &   PSIGMA(NPMGS*NTP1,NPISOEFF),   PTETA(NPMGS*NTP1,NPISOEFF), &
       &   PGWGT(NPMGS*NTP1,NPISOEFF),    ZSIG(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   ZTET(2*NTP1*(NPMGS+1)*NPPSCUB), ZPISO(K2), &
       &   ZRAC(NPMGS+1),                ZWGT(NPMGS+1)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !
  KP = K2 - K1 + 1
  !
  CALL GAUSS(NMGAUS,ZRAC,ZWGT)
  !     
  CALL RESETI(IC(K1),KP,1)
  DO JT=1,NT1
     DO JP=K1,K2
        IF (IC(JP).EQ.1) THEN
           IT0(JP) = JT
           IF (TETMAP(1,JP).LT.CT(JT)) IC(JP)  = 0
        ENDIF
     END DO
  END DO
  !
  !     TETMAP STARTS FROM TETMAP(1) AND THEN FOLLOWS THE VALUES
  !     OF CT AROUND. IF TETMAP(1) IN [CT(I-1),CT(I)[ THEN (IT0=I):
  !     TETMAP(JT=2)     = CT(I)
  !     TETMAP   (3)     = CT(I+1)
  !     :                :  :
  !     TETMAP(NT+1-I+2) = CT(NT+1) = CT(1) - 2.*CPI
  !     TETMAP(NT+4-I)   = CT(NT+4-I+I-2 - NT) = CT(2)
  !     :                :  :
  !     TETMAP(NT+1) = TETMAP(NT+4-I+(I-3)) = CT(2+I-3) = CT(I-1)
  !
  !     THUS ONE HAS THE VALUES OF CT PLUS ONE EXTRA VALUE AT TETMAP(1)
  !     => NT+1 POINTS
  !
  !     NOTE THAT THE INDEX OF THE JUMP OF 2*PI IS CHANGED BELOW
  !
  DO JT=2,NT1
     DO JP=K1,K2
        ZC = 0._RKIND
        IT = IT0(JP)+JT-2
        IF (IT.GT.NT1) THEN
           IT = IT-NT
           ZC = 1._RKIND
        ENDIF
        TETMAP(JT,JP) = CT(IT) + 2._RKIND * ZC * CPI
     END DO
  END DO
  !
  !     IF TETMAP(1)=CT(I-1) THEN TETMAP(NT+1)=TETMAP(1), WHICH IS BAD,
  !     THUS WE MODIFY TETMAP(NT+1) TO BE IN BETWEEN CT(I-2) AND CT(I-1),
  !     I.E IN BETWEEN TETMAP(NT) AND TETMAP(NT+1)
  !
  DO JP=K1,K2
     !%OS           IF (TETMAP(1,JP) .EQ. CT(IC(JP)-1))
     IF (TETMAP(1,JP) .EQ. CT(IT0(JP)-1)) &
          &       TETMAP(NT+1,JP) = 0.5_RKIND*(TETMAP(NT,JP) + TETMAP(NT+1,JP))
  END DO
  !
  !     SET TETMAP(NT+2) = TETMAP(1) + 2.*CPI
  !     => TETPSI=PTETA IS DEFINED BETWEEN TETMAP(1) AND TETMAP(NT+2)
  !     BUT WITH THE JUMP OF 2.*PI ALWAYS AT CT(NT+1)=CT(1)+2*PI
  !     THUS AT TETMAP(NT+1-I+2) (NORMALLY, I=1 TO 3)
  !
  DO JP=K1,K2
     ZPISO(JP-K1+1) = PSIISO(JP)
     TETMAP(NT2,JP) = TETMAP(1,JP) + 2._RKIND * CPI  
  END DO
  !           
  DO JG=1,NMGAUS
     DO JT=1,NT1
        DO JP=K1,K2
           ZTDIF = TETMAP(JT+1,JP) - TETMAP(JT,JP)
           ZTADD = TETMAP(JT+1,JP) + TETMAP(JT,JP)
           IG = NMGAUS*(JT-1)+JG
           PTETA(IG,JP) = .5_RKIND*(ZTADD + ZTDIF*ZRAC(JG))
           IF (PTETA(IG,JP) .GE. CT(NT1)) PTETA(IG,JP) = &
                &                             PTETA(IG,JP) - 2._RKIND * CPI
           IF (PTETA(IG,JP) .LT. CT(1)) PTETA(IG,JP) = &
                &                             PTETA(IG,JP) + 2._RKIND * CPI
           PGWGT(IG,JP) = ZWGT(JG) * ZTDIF
        END DO
     END DO
  END DO
  !
  !-----------------------------------------------------------------------
  !     COMPUTE ROOTS FOR NPPSCUB ISO-PSI SURFACES AT A TIME (FOR MEMORY)
  !
  IKTET = NT1 * (NMGAUS+1)
  DO JPSTART=K1,K2,NPPSCUB
     JK1 = JPSTART
     JK2 = MIN(JPSTART+NPPSCUB-1,K2)
     JPLEN = JK2 - JK1 + 1
     !
     DO JT=1,NT1
        DO JP=JK1,JK2
           !
           !     TETMAP: RESET THE JUMP OF 2.*CPI TO BE AT CT(NT1) => JT+1=NT+1-I+2
           !
           IF (TETMAP(JT+1,JP) .GE. CT(NT1)) &
                &                TETMAP(JT+1,JP) = TETMAP(JT+1,JP) - 2._RKIND*CPI
           ZTET(((JP-JK1)*NT1+JT)*(NMGAUS+1)) = TETMAP(JT+1,JP)
        END DO
        DO JG=1,NMGAUS
           DO JP=JK1,JK2
              ZTET(((JP-JK1)*NT1+JT-1)*(NMGAUS+1)+JG) = &
                   &                                  PTETA(NMGAUS*(JT-1)+JG,JP)
           END DO
        END DO
     END DO
     !     
     NROOT = JPLEN * IKTET
     !C         ISTART = (JK1-JK1)*IKTET + 1
     CALL CUBRT(NROOT,PSIAXE,PSIBND,ZPISO(JK1-K1+1),ZTET,ZSIG)
     !
     DO JT=1,NT1
        DO JP=JK1,JK2
           SIGMAP(JT+1,JP) = ZSIG(((JP-JK1)*NT1+JT)*(NMGAUS+1))
        END DO
        DO JG=1,NMGAUS
           DO JP=JK1,JK2
              PSIGMA(NMGAUS*(JT-1)+JG,JP) = &
                   &               ZSIG(((JP-JK1)*NT1+JT-1)*(NMGAUS+1)+JG)
           END DO
        END DO
     END DO
     !
  END DO
  !
  DO JP=K1,K2
     SIGMAP(NT2,JP) = SIGMAP(1,JP)
  END DO
  !     
END SUBROUTINE ISOFIND
