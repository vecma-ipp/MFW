!*DECK C1S07
!*CALL PROCESS
SUBROUTINE TCASE
  !        ################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C1S07 SET UP TEST CASES                                             *
  !       NTCASE = 0 ----> USER DEFINED EQUILIBRIUM                     *
  !       NTCASE = 1 ----> SOLOVEV EQUILIBRIUM GIVEN IN PRESET          *
  !       NTCASE = 2 ----> JET EQUILIBRIUM WITH SPECIFIED TT' AND P'    *
  !       NTCASE = 3 ----> NET EQUILIBRIUM WITH SPECIFIED I* AND P'     *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  IF (NTCASE .EQ. 0 .OR. NTCASE .EQ. 1) RETURN
  !
  IF (NTCASE .EQ. 2) THEN
     !
     AP(1)     = 0
     AP(2)     = -0.22_RKIND
     NPP=3
     ASPCT     = 0.423_RKIND
     AT(1)     = 0
     AT(2)     = 9.E-3_RKIND
     NIPR=1
     CURRT     = 0.65_RKIND
     TRIANG    = 0.3_RKIND
     ELONG     = 1.68_RKIND
     EPSLON    = 1.E-10_RKIND
     SOLPDA    = 0.5_RKIND
     !
     NSTTP     = 1
     NFUNC     = 1
     NPPFUN    = 1
     NSOUR     = 2
     NMESHA    = 0
     NSURF     = 2
     !
  ELSE IF (NTCASE .EQ. 3) THEN
     !
     AP(1)     = 0.05_RKIND
     AP(2)     = 0.6_RKIND
     AP(3)     = 0.7_RKIND
     AP(6)     = 0.5_RKIND
     APLACE(1) = 0.986_RKIND
     APLACE(2) = 0.999_RKIND
     ASPCT     = 0.27027_RKIND
     AT(1)     = 0.8_RKIND
     AT(2)     = 0.999_RKIND
     AT(3)     = 1._RKIND
     AWIDTH(1) = 0.03_RKIND
     AWIDTH(2) = 0.03_RKIND
     CURRT     = 0.73977_RKIND
     TRIANG    = 0.6_RKIND
     ELONG     = 2._RKIND
     EPSLON    = 1.E-10_RKIND
     SOLPDA    = 0.55_RKIND
     !
     NSTTP     = 2
     NFUNC     = 2
     NPPFUN    = 2
     NIPR      = 1
     NPP       = 1
     NMESHA    = 1
     NPOIDA    = 2
     NSURF     = 2
     !
  ELSE IF (NTCASE .EQ. 4) THEN
     !
     AP(1)     = 0.3_RKIND
     AP(2)     = 0.5_RKIND
     AP(3)     = 0.4_RKIND
     AP(4)     = 0._RKIND
     AP(5)     = 0.4_RKIND
     AP(6)     = 0._RKIND
     AP(7)     = 0._RKIND
     AP(8)     = 0._RKIND
     ASPCT     = 0.274_RKIND
     AT(1)     = 0.16_RKIND
     AT(2)     = 1._RKIND
     AT(3)     = 1._RKIND
     AT(4)     = -1.1_RKIND
     AT(5)     = -1.1_RKIND
     AT(6)     = 0._RKIND
     AT(7)     = 0._RKIND
     AT(8)     = 0._RKIND
     CSSPEC    = 0.33_RKIND
     CURRT     = 0.22_RKIND
     DELTA     = 0.5_RKIND
     DPLACE(1) = -.5_RKIND*CPI
     DWIDTH(1) = .05_RKIND*CPI
     ELONG     = 1.35_RKIND
     EPSLON    = 1.E-8_RKIND
     QSPEC     = 1._RKIND
     RNU       = 0.45_RKIND
     SGMA      = 1.2_RKIND
     SOLPDD    = 0.6_RKIND
     THETA0    = -.5_RKIND*CPI
     TRIANG    = 0._RKIND
     XI        = 8.E-4_RKIND
     !
     NBAL      = 1
     NBLC0     = 1
     NCHI      = 100
     NDIFPS    = 0
     NEGP      = -1
     NER       = 1
     NFUNC     = 2
     NIDEAL    = 0
     NINMAP    = 50
     NINSCA    = 50
     NIPR      = 1
     NISO      = 100
     NOPT      = 0
     NPLOT     = 1
     NPP       = 1
     NPPFUN    = 2
     NPSI      = 15
     NDIFT     = 1
     NCSCAL    = 1
     NRSCAL    = 0
     NSYM      = 0
     NTMF0     = 1
     NS        = 30
     NSTTP     = 2
     NSURF     = 4
     NT        = 30

     NFUNC     = 2
     NPPFUN    = 2
     NIPR      = 1
     NPP       = 1
     NMESHA    = 0
     NMESHD    = 1
     NPOIDD    = 1

     !
  ENDIF
  !
  RETURN
END SUBROUTINE TCASE
