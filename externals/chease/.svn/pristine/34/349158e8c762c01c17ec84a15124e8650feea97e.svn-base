SUBROUTINE OUTMKSA(KUNIT,KOPT)
  !        ##############################
  !
  !                                        AUTHORS:
  !                                        O.SAUTER,  CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SA03 OUTPUT OF SOME EQUILIBRIUM VALUES AND THEIR MKSA VALUES      *
  !        USING R0EXP [M] AND B0EXP [T] FOR CONVERSION                 *
  !                                                                     *
  !        KUNIT: DISK UNIT ON WHICH TO WRITE                           *
  !        KOPT = 1: NO HEADER                                          *
  !        KOPT = 2: WITH HEADER                                        *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !----------------------------------------------------------------------
  !
  !L       1. WRITE SOME VALUES IN MKSA USING R0EXP [M] AND B0EXP [T]
  !
  REAL(RKIND)      ::     ZRMAX
  REAL(RKIND)      ::     ZRMIN
  INTEGER          ::     IZMIN
  INTEGER          ::     IZMAX
  INTEGER          ::     ISMIN
  INTEGER          ::     IRMIN
  INTEGER          ::     ISMAX
  INTEGER          ::     IRMAX
  REAL(RKIND)      ::     ZMU0
  REAL(RKIND)      ::     Z2PI
  INTEGER          ::     KOPT
  INTEGER          ::     KUNIT
  INTEGER          ::     IUNIT
  IUNIT = KUNIT
  !
  IF (KOPT .EQ. 2) THEN
     WRITE(IUNIT,9100)
  ENDIF
  WRITE(IUNIT,*)
  Z2PI = 2._RKIND*CPI
  ZMU0 = 4.E-07_RKIND * CPI
  WRITE(IUNIT,9102) ABS(SPSIM), ' abs(PSI-AXIS) --> [T M**2] ', &
       &     SPSIM*B0EXP*R0EXP**2
  WRITE(IUNIT,9102) RMAG,' R OF MAGAXE --> [M]   ',RMAG*R0EXP
  WRITE(IUNIT,9102) RZMAG,' Z OF MAGAXE --> [M]   ',RZMAG*R0EXP
  WRITE(IUNIT,9102) RZ0,' Z0 --> [M]   ',RZ0*R0EXP
  IF (NRSCAL .EQ. 0) THEN
     WRITE(IUNIT,9101) R0EXP,' R0 [M] USED FOR CONVERTING TO MKSA'
     WRITE(IUNIT,9101) B0EXP,' B0 [T] USED FOR CONVERTING TO MKSA'
  ELSE
     WRITE(IUNIT,9103) R0EXP,' R0 [M] USED FOR CONVERTING TO MKSA' &
          &       ,' (CHANGED TO RMAG_EXP)'
     WRITE(IUNIT,9103) B0EXP,' B0 [T] USED FOR CONVERTING TO MKSA' &
          &       ,' (CHANGED TO BMAG_EXP)'
  ENDIF
  WRITE(IUNIT,9102) SIGNB0XP,' SIGN OF B0 IN EXPERIMENT (CHEASE ASSUMES 1.0) '
  WRITE(IUNIT,9102) RITOT, ' TOTAL CURRENT --> [A] ', &
       &     RITOT*R0EXP*B0EXP/ZMU0
  WRITE(IUNIT,9102) SIGNIPXP,' SIGN OF IP IN EXPERIMENT (CHEASE ASSUMES 1.0) '
  WRITE(IUNIT,9101) (1._RKIND+RELL(NISO1EFF))/(1._RKIND-RELL(NISO1EFF)),' b/a'
  WRITE(IUNIT,9102) Q0,' Q_ZERO, USING SIGNS OF IP AND B0, WOULD GIVE: ', &
       &                     SIGNB0XP*SIGNIPXP*Q0
  WRITE(IUNIT,9102) QPSI(NISO1EFF),' Q_EDGE, USING SIGNS OF IP AND B0, WOULD GIVE: ', &
       &                     SIGNB0XP*SIGNIPXP*QPSI(NISO1EFF)
  WRITE(IUNIT,9101) BETAP,' POLOIDAL BETA'
  WRITE(IUNIT,9101) BETAX,' BETA_EXP=<P>*2*MU0/B0**2'
  WRITE(IUNIT,9101) eqchease_out(1)%profiles_1d%li(NISO1EFF1),' LI'
  WRITE(IUNIT,9104) CP0, ' PRESSURE ON AXIS --> [Pa] ', &
       &     CP0*B0EXP*B0EXP/ZMU0,'  --> [10**19 M**-3 KEV]: ', &
       &     CP0*B0EXP*B0EXP/ZMU0/1.602E-16_RKIND/1.E+19_RKIND
  WRITE(IUNIT,9101) BETA,' BETA'
  WRITE(IUNIT,9101) BETAS,' BETA* (SQRT(<P**2>))'
  WRITE(IUNIT,9101) 2._rkind*cp0*( eqchease_out_add_1d(NISO1EFF1,iirgeo)/T0 )**2,' BETA-AXIS'
  WRITE(IUNIT,9102) SPSIM, ' PSI-AXIS --> [T M**2] ', &
       &     SPSIM*B0EXP*R0EXP**2
  WRITE(IUNIT,9102) Z2PI*SPSIM,' 2*PI*PSI-AXIS -->     ', &
       &     Z2PI*SPSIM*B0EXP*R0EXP**2
  WRITE(IUNIT,9102) SIGNIPXP*SPSIM, ' IP_SIGN*PSI-AXIS --> [T M**2] ', &
       &     SIGNIPXP*SPSIM*B0EXP*R0EXP**2
  WRITE(IUNIT,9102) SIGNIPXP*Z2PI*SPSIM,' IP_SIGN*2*PI*PSI-AXIS -->     ', &
       &     SIGNIPXP*Z2PI*SPSIM*B0EXP*R0EXP**2
  WRITE(IUNIT,9102) ARATIO(NISO1EFF1),' ASPECT RATIO ; a/R= ', &
       &     1._RKIND/ARATIO(NISO1EFF1)
  WRITE(IUNIT,9102) VOLUME, ' VOLUME -> ',VOLUME*R0EXP**3
  WRITE(IUNIT,9102) AREA,   ' AREA   -> ',AREA*R0EXP**2
  WRITE(IUNIT,9102) eqchease_out(1)%profiles_1d%surface(NISO1EFF1),   ' SURFACE   -> ', &
       & eqchease_out(1)%profiles_1d%surface(NISO1EFF1)*R0EXP**2
  WRITE(IUNIT,9102)RLENG(NISO1EFF), ' LENGTH -> ',RLENG(NISO1EFF)*R0EXP
  IRMAX = ISMAX(NBPSOUT,RRBPSOU,1)
  IRMIN = ISMIN(NBPSOUT,RRBPSOU,1)
  IZMAX = ISMAX(NBPSOUT,RZBPSOU,1)
  IZMIN = ISMIN(NBPSOUT,RZBPSOU,1)
  ZRMIN = RRBPSOU(IRMIN)
  ZRMAX = RRBPSOU(IRMAX)
  WRITE(IUNIT,9102) ZRMIN,' RMIN -> RMIN [m] ',ZRMIN*R0EXP
  WRITE(IUNIT,9102) ZRMAX,' RMAX -> RMAX [m] ',ZRMAX*R0EXP
  WRITE(IUNIT,9102) RZBPSOU(IZMIN),' ZMIN -> ZMIN [m] ', &
       &       RZBPSOU(IZMIN)*R0EXP
  WRITE(IUNIT,9102) RZBPSOU(IZMAX),' ZMAX -> ZMAX [m] ', &
       &       RZBPSOU(IZMAX)*R0EXP
  WRITE(IUNIT,9102) 0.5_RKIND*(ZRMIN+ZRMAX),' RGEOM -> RGEOM [m] ', &
       &       0.5_RKIND*(ZRMIN+ZRMAX)*R0EXP
  WRITE(IUNIT,9102) 0.5_RKIND*(ZRMAX-ZRMIN),' MINOR RADIUS -> A [m] ', &
       &       0.5_RKIND*(ZRMAX-ZRMIN)*R0EXP
  !
  RETURN
9100 FORMAT(/,1X,'*************************************', &
       &         //,1X,'SOME QUANTITIES AND THEIR MKSA VALUES', &
       &         //,1X,'*************************************',/)
9101 FORMAT(1PE18.8,A)
9102 FORMAT(1PE18.8,A,E18.8)
9103 FORMAT(1PE18.8,2A)
9104 FORMAT(1PE18.8,2(A,E18.8))
  !
END SUBROUTINE OUTMKSA
