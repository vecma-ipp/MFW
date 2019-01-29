!----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
!
!*DECK C3SB04
!*CALL PROCESS
SUBROUTINE SURFRZ(K,PSIGMA,PTETA)
  !        #################################
  !                                        AUTHORS:
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SB04  COMPUTE AND SAVE R,Z COORDINATES OF FLUX SURFACE Q = QSHAVE *
  !                                                                     *
  !**********************************************************************
  !
  !     WRITE R,Z COORDINATES OF K:TH FLUX SURFACE ON FILE RZPEEL
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  USE globals
  IMPLICIT NONE
  !
!!$         PARAMETER (NPGMAP=NPMGS*NTP1)
  !
  INTEGER          ::     J
  INTEGER          ::     K
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     PSIGMA
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZSINT
  REAL(RKIND)      ::     ZCOST
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     PTETA
  REAL(RKIND)      ::     ZTETA
  INTEGER          ::     J1
  INTEGER          ::     IGMAX
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     NPGMAP
  DIMENSION &
       &   PSIGMA(*),         PTETA(*), &
       &   ZBND(npmgs*ntp1,5),    ZTETA(npmgs*ntp1,5)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  NPGMAP=NPMGS*NTP1
  !
  ZEPS = 1.E-3_RKIND
  !
  IGMAX = NMGAUS * NT1
  !
  DO J1=1,IGMAX
     !
     ZTETA(J1,1) = PTETA(J1)
     ZTETA(J1,2) = PTETA(J1) - 2._RKIND * ZEPS
     ZTETA(J1,3) = PTETA(J1) -      ZEPS
     ZTETA(J1,4) = PTETA(J1) +      ZEPS
     ZTETA(J1,5) = PTETA(J1) + 2._RKIND * ZEPS
     !
  END DO
  !
  CALL BOUND(IGMAX,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(IGMAX,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(IGMAX,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(IGMAX,ZTETA(1,4),ZBND(1,4))
  CALL BOUND(IGMAX,ZTETA(1,5),ZBND(1,5))
  !
  DO J4=1,IGMAX
     ! 
     ZCOST  = COS(PTETA(J4))
     ZSINT  = SIN(PTETA(J4))
     !
     ZRHO    = PSIGMA(J4) * ZBND(J4,1)
     ZR      = ZRHO * ZCOST + R0
     ZZ      = ZRHO * ZSINT + RZ0
     !
     RRISO(J4,K)  = ZR
     RZISO(J4,K)  = ZZ
     !
  END DO
  !
  OPEN(NRZPEL,FILE='RZPEEL',FORM='FORMATTED')
  WRITE(NRZPEL,1000) IGMAX
  WRITE(NRZPEL,1010) (RRISO(J,K),RZISO(J,K),J=1,IGMAX)
1000 FORMAT(I5)
1010 FORMAT(2E18.8)
  CLOSE(NRZPEL)
  RETURN
END SUBROUTINE SURFRZ
