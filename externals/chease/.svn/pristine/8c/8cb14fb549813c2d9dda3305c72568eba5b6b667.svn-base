!
SUBROUTINE PSIBOX(KPSI1)
  !       #########################
  !                                        AUTHOR O. SAUTER, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SY06 EVALUATE PSI ON (R(I),Z(J)), I=1,NRBOX, J=1,NZBOX EQUIDISTANT*
  !        MESH SUCH THAT R(1) = RBOXLFT , R(NRBOX) = RBOXLFT + RBOXLEN *
  !        Z(1) = ZBOXMID-ZBOXLEN/2. , Z(NZBOX) = ZBOXMID+ZBOXLEN/2.    *
  !        Z mesh could be shifted DEPENDING ON NEQDZMG
  !            (NEQDZMG DEFINED FROM NEQDXTPO in AUXVAL)
  !                                                                     *
  !        ASSUME THAT KPSI1 ISO-SURFACE HAVE BEEN CALCULATED BEFORE    *
  !                                                                     *
  !        USE SAME ALGORITHM AS IN ROUTINE EVLATE FOR INTERIOR POINTS  *
  !        FOR OUTSIDE POINTS, USE CUBIC EXTRAPOLATION WITH PSI AND     *
  !        DPSI/DSIGMA AT CSIG(NS) AND CSIG(NS1)                        *
  !                                                                     *
  ! NOTE: RBOXLFT, RBOXLEN, ZBOXMID, ZBOXLEN ARE IN SI UNITS
  !       ZRBOXLFT, ZRBOXLEN, ZZBOXMID, ZZBOXLEN (LOCAL VALUES) ARE IN CHEASE UNITS
  !**********************************************************************
  !
  USE globals
  USE interpol
  USE interpos_module
  IMPLICIT NONE
  !
  INTEGER          ::     KPSI1
  !
  REAL(RKIND)      ::     ZPSINSM2
  REAL(RKIND)      ::     ZSNSM2
  REAL(RKIND)      ::     ZPSINSM1
  REAL(RKIND)      ::     ZDPDS1
  REAL(RKIND)      ::     ZPSI1
  REAL(RKIND)      ::     ZSIG1
  REAL(RKIND)      ::     ZPSIM3
  REAL(RKIND)      ::     ZPSIM2
  REAL(RKIND)      ::     ZDPDSM1
  REAL(RKIND)      ::     ZDFDT
  REAL(RKIND)      ::     ZDFDS
  REAL(RKIND)      ::     ZPSIM1
  REAL(RKIND)      ::     ZSIGM3
  REAL(RKIND)      ::     ZSIGM2
  REAL(RKIND)      ::     ZSIGM1
  REAL(RKIND)      ::     DDOT
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZCPSI
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     IS
  REAL(RKIND)      ::     ZSIG
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     IT
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZTET
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZR(NPBPS)
  INTEGER          ::     I
  REAL(RKIND)      ::     ZZ(NPBPS)
  INTEGER          ::     J
  REAL(RKIND)      ::     ZPSEDGMN
  REAL(RKIND)      ::     ZDZ, TENSION1D
  REAL(RKIND)      ::     ZDR
  REAL(RKIND)      ::     ZBOTTOM
  REAL(RKIND)      ::     ZZZLEN
  REAL(RKIND)      ::     ZSHIFTZ, ZSHIFTDZ
  REAL(RKIND)      ::     ZSHIFTDR
  REAL(RKIND)      ::     ZNBDEL, ZDUM
  INTEGER          ::     IFIXBOUN, IFIXWALL
  REAL(RKIND)      ::     ZZMAXEF
  REAL(RKIND)      ::     ZZMINEF
  REAL(RKIND)      ::     ZZBOXMID
  REAL(RKIND)      ::     ZZBOXLEN
  REAL(RKIND)      ::     ZRBOXLEN
  REAL(RKIND)      ::     ZRBOXLFT
  REAL(RKIND)      ::     ZZMAX
  REAL(RKIND)      ::     ZZMIN
  REAL(RKIND)      ::     ZRMAX
  REAL(RKIND)      ::     ZRMIN
  INTEGER          ::     IZMIN
  INTEGER          ::     IZMAX
  INTEGER          ::     ISMIN
  INTEGER          ::     IRMIN
  INTEGER          ::     ISMAX
  INTEGER          ::     IRMAX
  INTEGER          ::     IGMAX, nbprofiles1d
  INTEGER          ::     iRZ_1d, NRZBOX, ipsi, i3nchi
  REAL(rkind)           ::      YY1LIN,YY2LIN,XX1LIN,XX2LIN,XXXLIN
  DIMENSION &
       &      ZCPSI(1,16),     ZDFDS(1,16),    ZDFDT(1,16), &
       &      ZF(1,16)
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE :: PSIZ_1, DPSIDZ_1, DPSIDR_1, pprime_RZ, ffprime_RZ, f_dia_RZ, xout_profRZ, &
       & zsigRZ_1d_psi, zchiRZ_1d_psi, chi_RZ, smiso_RZ
  REAL(RKIND), DIMENSION(:), ALLOCATABLE :: ztetRZ_1d, zrhoboundRZ_1d, zsigRZ_1d, tmp_theta, tmp_chim
  !
  !-----------------------------------------------------------------------
  !
  !   CHECK ARRAY DIMENSIONS
  !
  IF (NRBOX .GT. NPBPS) THEN
    IF (NVERBOSE .GE. 1) PRINT *,' WARNING: NRBOX= ',NRBOX,' TOO LARGE, CHANGED TO', &
         &       ' NPBPS= ',NPBPS
    NRBOX = NPBPS
  ENDIF
  IF (NZBOX .GT. NPBPS) THEN
    IF (NVERBOSE .GE. 1) PRINT *,' WARNING: NZBOX= ',NZBOX,' TOO LARGE, CHANGED TO', &
         &       ' NPBPS= ',NPBPS
    NZBOX = NPBPS
  ENDIF
  !
  !        CHECK THAT PLASMA BOUNDARY IS INSIDE BOX
  !        NOTE THAT BOX DIMENSIONS ARE IN MKSA
  !
  IGMAX = NMGAUS * NT1
  IRMAX = ISMAX(IGMAX,RRISO(1,KPSI1),1)
  IRMIN = ISMIN(IGMAX,RRISO(1,KPSI1),1)
  IZMAX = ISMAX(IGMAX,RZISO(1,KPSI1),1)
  IZMIN = ISMIN(IGMAX,RZISO(1,KPSI1),1)
  ZRMIN = RRISO(IRMIN,KPSI1)
  ZRMAX = RRISO(IRMAX,KPSI1)
  ZZMIN = RZISO(IZMIN,KPSI1)
  ZZMAX = RZISO(IZMAX,KPSI1)
  ! Transform to CHEASE units
  ! ZBOXMID WILL CONTAIN THE VERTICAL SHIFTS ALREADY, BUT NOT R,Z OTHERWISE
  IF (NVERBOSE .GE. 3) print *,'ZBOXMID, ZBOXLEN, RBOXLFT, RBOXLEN= ',ZBOXMID, ZBOXLEN, RBOXLFT, RBOXLEN
  IF (ZBOXMID .LE. -1.e3_RKIND) THEN
    ! RBOXLFT, RBOXLEN, ZBOXLEN, ZBOXMID not yet defined from iodisk and read eqdsk
    ! so define defaults from RZMAG
    ZBOXMID = RZMAG * R0EXP
    ZBOXLEN = (2.1_rkind*max(ZZMAX-ZBOXMID,ZBOXMID-ZZMIN)) * R0EXP
    RBOXLFT = (ZRMIN - 0.05_rkind * (ZRMAX-ZRMIN)) * R0EXP
    RBOXLEN = 1.1_rkind * (ZRMAX-ZRMIN) * R0EXP
    IF (NVERBOSE .GE. 3) print *,'ZBOXMID, ZBOXLEN, RBOXLFT, RBOXLEN= ',ZBOXMID, ZBOXLEN, RBOXLFT, RBOXLEN
    !
  ELSE
    ! defined from eqdsk in iodisk or from namelist (in si units)
  END IF
  ZRBOXLFT = RBOXLFT / R0EXP
  ZRBOXLEN = RBOXLEN / R0EXP
  ZZBOXLEN = ZBOXLEN / R0EXP
  ZZBOXMID = ZBOXMID / R0EXP
  !
  !     DETERMINE EFFECTIVE ZMAG FOR EQDSK FILE, DEPENDING ON
  !     CENTER OF Z BOX
  !
  ZSHIFTZ = 0.0_RKIND
  ! ZBOXMID WILL CONTAIN THE VERTICAL SHIFTS ALREADY, BUT NOT R,Z OTHERWISE
  ! SINCE ZBOXMID CAN BE INDEPENDENT OF SHIFT
  IF (NEQDZMG .EQ. 0) THEN
    !  SHIFT TO MAGNETIC AXIS
    ZSHIFTZ = RZMAG
    ! SHIFT IN Z OF EQDSK ONLY THROUGH ZBOXMID
    ZZBOXMID = ZZBOXMID - ZSHIFTZ
  ELSE IF (NEQDZMG .EQ. 2) THEN
    !  SHIFT Z TO GEOMETRICAL CENTER
    ZSHIFTZ = 0.5_RKIND * (ZZMIN + ZZMAX)
    ! SHIFT IN Z OF EQDSK ONLY THROUGH ZBOXMID
    ZZBOXMID = ZZBOXMID - ZSHIFTZ
  ELSE IF (NEQDZMG .EQ. 3) THEN
    !  SHIFT Z TO MAGNETIC AXIS AND BOX SUCH THAT ZBOXMID = 0
    ZSHIFTZ = RZMAG
    ZZBOXLEN = ZZBOXLEN + 2._RKIND*ABS(ZZBOXMID)
    ZZBOXMID = 0._RKIND
  ENDIF
  IF (NVERBOSE .GE. 3) THEN
    print *,'NEQDZMG= ',NEQDZMG
    print *,'ZBOXMID = ',ZBOXMID
  END IF
  !
  IFIXBOUN = 0
  IF (ZRBOXLEN.LE.0._RKIND .OR. ZZBOXLEN.LE.0._RKIND) IFIXBOUN = 1
  IF (ZRMIN.LE.ZRBOXLFT .OR. ZRMAX.GE.ZRBOXLFT+ZRBOXLEN .OR. &
       &       ZZMIN.LE.ZZBOXMID-ZZBOXLEN/2._RKIND+ZSHIFTZ .OR. ZZMAX.GE.ZZBOXMID+ZZBOXLEN/2._RKIND+ZSHIFTZ &
       &       .OR. IFIXBOUN.EQ.1) THEN
    IF (NVERBOSE .GE. 3) THEN
      WRITE(6,'(/,A)') ' ***********************************'
      PRINT *,' BAD VALUES FOR RBOXLFT, RBOXLEN OR ZBOXLEN'
      PRINT *,' THEY HAVE BEEN CHANGED FROM:'
      PRINT *,' RBOXLFT = ',RBOXLFT
      PRINT *,' RBOXLEN = ',RBOXLEN
      PRINT *,' ZBOXLEN = ',ZBOXLEN
      PRINT *,' ZBOXMID = ',ZBOXMID
      PRINT *,' TO:'
      PRINT *,' '
    END IF
    !
    !     AT LEAST (ZNBDEL-1) MESH POINTS OUTSIDE PLASMA BOUNDARY
    !
    ZNBDEL = 5._RKIND
    ZSHIFTDR = ZNBDEL * (ZRMAX-ZRMIN)/REAL(NRBOX,RKIND)
    ZSHIFTDZ = ZNBDEL * (ZZMAX-ZZMIN)/REAL(NZBOX,RKIND)
    IF (ZRMIN-ZSHIFTDR.LE.ZRBOXLFT .OR. IFIXBOUN.EQ.1) &
         &        ZRBOXLFT = ZRMIN - ZSHIFTDR
    !
    IF (ZRMAX+ZSHIFTDR.GE.ZRBOXLFT+ZRBOXLEN .OR. IFIXBOUN.EQ.1) &
         &        ZRBOXLEN = ZRMAX + ZSHIFTDR - ZRBOXLFT
    !
    ZZZLEN = 2._RKIND*MAX(ABS(ZZMAX-ZZBOXMID),ABS(ZZMIN-ZZBOXMID))
    !
    IF (ZZZLEN+2._RKIND*ZSHIFTDZ .GE. ZZBOXLEN .OR. IFIXBOUN.EQ.1) &
         &          ZZBOXLEN = ZZZLEN + 2._RKIND*ZSHIFTDZ
    !
  ENDIF
  !
  ! VALUE NEEDED TO COMPUTE PSI WITH RESPECT TO PRESENT MESH
  ZBOTTOM = ZZBOXMID - 0.5_RKIND*ZZBOXLEN + ZSHIFTZ
  !
  ! ZSHIFTZ CONTAINED IN SHIFT OF RZMAG
  RZMGEQD = RZMAG - ZSHIFTZ
  !
  ZBOXMID = ZZBOXMID * R0EXP
  RBOXLFT = ZRBOXLFT * R0EXP
  RBOXLEN = ZRBOXLEN * R0EXP
  ZBOXLEN = ZZBOXLEN * R0EXP
  !
  ! FIX WALL IN CASE WAS NOT CORRECTLY DEFINED (DEFINED IN IODISK IF NOT DEFINED AT ALL)
  IF (NWALLPOS .GT. 0 .AND. NFIXWALL.EQ.1) THEN
    IFIXWALL=0
    DO I=1,NWALLPOS
      ! Simple check: Check if a limiter point is inside (Rmin,Rmax,Zmin,Zmax) box
      IF ((WALLPOSR(I).GE.ZRMIN*R0EXP) .AND. (WALLPOSR(I).LE.ZRMAX*R0EXP) .AND. &
           & (WALLPOSZ(I).GE.ZZMIN*R0EXP) .AND. (WALLPOSZ(I).LE.ZZMAX*R0EXP)) THEN
        print *,'I, WALLPOSR(I), WALLPOSZ(I), ZRMIN*R0EXP, ZRMAX*R0EXP, ZZMIN*R0EXP, ZZMAX*R0EXP= ', &
             & I, WALLPOSR(I), WALLPOSZ(I), ZRMIN*R0EXP, ZRMAX*R0EXP, ZZMIN*R0EXP, ZZMAX*R0EXP
        IFIXWALL=1
      END IF
    END DO
    IF (IFIXWALL .EQ. 1) THEN
      IF (NVERBOSE .GE. 3) PRINT *,' CORRECT LIMITER POSITION TO AVOID CUTTING PLASMA'
      NWALLPOS = 5
      ! NOTE: FOR TORAY WALL NEEDS TO BE INSIDE RMESH,ZMESH, SO SHIFT BY ZDUM TO MAKE SURE
      ZDUM = 1.0E-05_RKIND
      WALLPOSR(1) = RBOXLFT * (1._RKIND + ZDUM)
      WALLPOSZ(1) = ZBOXMID + ZSHIFTZ*R0EXP - ZBOXLEN/2._RKIND*(1._RKIND-ZDUM)
      WALLPOSR(2) = RBOXLFT + RBOXLEN*(1._RKIND-ZDUM)
      WALLPOSZ(2) = WALLPOSZ(1)
      WALLPOSR(3) = WALLPOSR(2)
      WALLPOSZ(3) = ZBOXMID + ZSHIFTZ*R0EXP + ZBOXLEN/2._RKIND*(1._RKIND-ZDUM)
      WALLPOSR(4) = WALLPOSR(1)
      WALLPOSZ(4) = WALLPOSZ(3)
      WALLPOSR(5) = WALLPOSR(1)
      WALLPOSZ(5) = WALLPOSZ(1)
    END IF
  END IF
  !
  IF (NVERBOSE .GE. 3) THEN
    PRINT *,' RBOXLFT (* R0EXP) = ',RBOXLFT
    PRINT *,' RBOXLEN (* R0EXP) = ',RBOXLEN
    PRINT *,' ZBOXMID (* R0EXP) = ',ZBOXMID
    PRINT *,' ZBOXLEN (* R0EXP) = ',ZBOXLEN
    PRINT *,' ZBOTTOM * R0EXP = ',ZBOTTOM * R0EXP
    PRINT *,' ZTOP * R0EXP = ',(ZZBOXMID + 0.5_RKIND*ZZBOXLEN) * R0EXP
    PRINT *,' PLASMA EDGES:'
    PRINT *,' ZRMIN * R0EXP= ',ZRMIN * R0EXP
    PRINT *,' ZRMAX * R0EXP= ',ZRMAX * R0EXP
    PRINT *,' ZZMIN * R0EXP= ',ZZMIN * R0EXP
    PRINT *,' ZZMAX * R0EXP= ',ZZMAX * R0EXP
    PRINT *,' AND WILL SHIFT Z WITH ZSHIFT * R0EXP = ',ZSHIFTZ * R0EXP
    !
    PRINT *,' '
    PRINT *,' RMAG * R0EXP=  ',RMAG * R0EXP
    PRINT *,' RZMAG * R0EXP= ',RZMAG * R0EXP
    PRINT *,' RZMAG_EFF * R0EXP= ',(RZMAG-ZSHIFTZ) * R0EXP
    PRINT *,' '
    PRINT *,' VALUES USED FOR MESH IN PSIBOX (CHEASE UNITS): ZRBOXLEN= ',ZRBOXLEN,' ZZBOXLEN= ',ZZBOXLEN
    PRINT *,' VALUES USED FOR MESH IN PSIBOX (SI     UNITS): ZRBOXLEN= ',ZRBOXLEN*R0EXP,' ZZBOXLEN= ',ZZBOXLEN*R0EXP
    PRINT *,' '
  END IF
  !
  !-----------------------------------------------------------------------
  !L       2. COMPUTE PSI VALUE. NOTE THAT CPSICL WAS SHIFTED BY CPSRF
  !        SO SHIFT IT BACK
  !
  ZDR = ZRBOXLEN / REAL(NRBOX-1,RKIND)
  ZDZ = ZZBOXLEN / REAL(NZBOX-1,RKIND)
  IF (NVERBOSE .GE. 3) THEN
    print *,' r0, rz0, rmag, rzmag= ',r0, rz0, rmag, rzmag
    print *,' bps(1),bps(12) = ',bps(1),bps(12),'  r0exp= ',r0exp
  END IF
  bps(1) = r0
  bps(12) = rz0
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  ZPSEDGMN = 1.E-05_RKIND
  allocate(eqchease_out(1)%profiles_2d(1)%grid%dim1(NRBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%grid%dim2(NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%theta(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%vtheta(NRBOX,NZBOX)) ! put rho, polar, of RZ points in vtheta
  ! Use TETCHI_SORTED(NCHI,NISO1EFF), SIGCHI(NCHI,NISO1EFF) points on chim, smiso surfaces to calculate chi(R,Z) points
  ! Needs theta(R,Z) points in 1D array (to do only one interpos call)
  ! Needs sigma values at each NISO1EFF surface for all theta(R,Z) points
  allocate(ztetRZ_1d(NRBOX*NZBOX))
  allocate(zrhoboundRZ_1d(NRBOX*NZBOX))
  allocate(zsigRZ_1d(NRBOX*NZBOX))
  DO J=1,NZBOX
    ZZ(J) = ZBOTTOM + REAL(J-1,RKIND)*ZDZ
    eqchease_out(1)%profiles_2d(1)%grid%dim2(j) = ZZ(j)
    !
    DO I=1,NRBOX
      iRZ_1d = (J-1)*NRBOX + I
      ZR(I) = ZRBOXLFT + REAL(I-1,RKIND)*ZDR
      eqchease_out(1)%profiles_2d(1)%grid%dim1(I) = ZR(I)
      !
      ZRHO = SQRT((ZR(I) - BPS(1))**2 + (ZZ(J) - BPS(12))**2)
      ZTET = ATAN2(ZZ(J) - BPS(12),ZR(I) - BPS(1))
      eqchease_out(1)%profiles_2d(1)%theta(I,J) = ZTET
      ztetRZ_1d(iRZ_1d) = ZTET
      eqchease_out(1)%profiles_2d(1)%vtheta(I,J) = ZRHO
      IF (ZTET .LT. CT(1)) ZTET = ZTET + 2._RKIND * CPI
      eqchease_out(1)%profiles_2d(1)%theta(I,J) = ztet
      IF (eqchease_out(1)%profiles_2d(1)%theta(I,J) .LT. RC0P) &
           & eqchease_out(1)%profiles_2d(1)%theta(I,J) = eqchease_out(1)%profiles_2d(1)%theta(I,J) + 2._RKIND * CPI
      !
      CALL BOUND(1,ZTET,ZBND)
      IT = ISRCHFGE(NT1,CT,1,ZTET) - 1
      IF (IT .LT. 1)  IT = 1
      IF (IT .GT. NT) IT = NT
      ZT1 = CT(IT)
      ZT2 = CT(IT+1)
      !
      ZSIG = ZRHO / ZBND
      zsigRZ_1d(iRZ_1d) = ZSIG
      zrhoboundRZ_1d(iRZ_1d) = ZBND
      !
      IF (ZSIG .LE. 1.0_RKIND) THEN
        !        INSIDE POINT
        IS = ISRCHFGE(NS1,CSIG,1,ZSIG)  - 1
        IF (IS .LT. 1)  IS = 1
        IF (IS .GT. NS) IS = NS
        !
        ZS1 = CSIG(IS)
        ZS2 = CSIG(IS+1)
        !
        CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
        CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG,ZTET,ZF)
        !
        EQDSPSI(I,J) = DDOT(16,ZF,1,ZCPSI,1) - CPSRF
        !
      ELSE IF (ZSIG .GT. 1.0_RKIND) THEN
        !        OUTSIDE POINT
        ZSIGM1 = CSIG(NS)
        ZSIGM2 = ZSIGM1 + 0.33_RKIND*(1._RKIND-ZSIGM1)
        ZSIGM3 = ZSIGM1 + 0.67_RKIND*(1._RKIND-ZSIGM1)
        IS = NS
        ZS1 = CSIG(IS)
        ZS2 = CSIG(IS+1)
        !
        !        EVALUATE PSI AT SIGMA = ZSIGM1
        CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
        CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIGM1,ZTET,ZF)
        ZPSIM1 = DDOT(16,ZF,1,ZCPSI,1)
        !        EVALUATE DPSI/DSIGMA AT SIGMA = ZSIGM1
        CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIGM1,ZTET,ZDFDS, &
             &                ZDFDT)
        ZDPDSM1 = DDOT(16,ZDFDS,1,ZCPSI,1)
        !
        !        EVALUATE PSI AT SIGMA = ZSIGM2
        CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
        CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIGM2,ZTET,ZF)
        ZPSIM2 = DDOT(16,ZF,1,ZCPSI,1)
        !
        !        EVALUATE PSI AT SIGMA = ZSIGM3
        CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
        CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIGM3,ZTET,ZF)
        ZPSIM3 = DDOT(16,ZF,1,ZCPSI,1)
        !
        !        EVALUATE PSI AT SIGMA = 1.0
        ZSIG1 = 1.0_RKIND
        CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
        CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG1,ZTET,ZF)
        ZPSI1 = DDOT(16,ZF,1,ZCPSI,1)
        !
        !        EVALUATE DPSI/DSIGMA AT SIGMA = 1.0
        CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG1,ZTET,ZDFDS, &
             &                ZDFDT)
        ZDPDS1 = DDOT(16,ZDFDS,1,ZCPSI,1)
        !
        !     EXTRAPOLATE ACCORDING TO NEQDXTPO:
        !     NEQDXTPO = 0 => CONSTANT
        !     NEQDXTPO = 1 => LINEAR
        !     NEQDXTPO = 2 => QUADRATIC USING ZPSI1-2, ZPSI1-1 ZPSI1 (DEFAULT)
        !     NEQDXTPO = 3 => CUBIC USING ZPSIM1, ZPSIM2, ZPSIM3, ZPSI1
        !     NEQDXTPO = 4 => CUBIC USING ZPSIM1 AND ZPSI1 WITH DERIVATIVES
        !     NEQDXTPO = 5 => QUADRATIC USING ZPSIM1, ZDPDSM1 AND ZPSI1
        !     NEQDXTPO = 6 => QUADRATIC USING ZPSIM1, ZPSI1 AND ZDPDS1
        !     NEQDXTPO = 7 => QUADRATIC USING ZPSIM2, ZPSIM3 AND ZPSI1
        !     NEQDXTPO = 8 => CUBIC USING NS-2, NS-1, NS, AND NS+1
        !
        IF (NEQDXTPO .EQ. 0) THEN
          !     ASSUME PSIEDGE = 0. AND PSIAXIS < 0.
          EQDSPSI(I,J) = ZPSEDGMN
        ELSE IF (NEQDXTPO .EQ. 1) THEN
          EQDSPSI(I,J) = FLINEAR(ZSIGM1,ZSIG1,ZPSIM1,ZPSI1, &
               &                ZSIG) - CPSRF
        ELSE IF (NEQDXTPO .EQ. 2) THEN
          !     EVALUATE PSI AT ZSIG WITH QUADRATIC USING NS-1, NS, NS+1
          !     EVALUATE PSI AT SIGMA = ZSIG(NS-1)
          IS = NS - 1
          ZS1 = CSIG(IS)
          ZS2 = CSIG(IS+1)
          CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
          CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZS1,ZTET,ZF)
          ZPSINSM1 = DDOT(16,ZF,1,ZCPSI,1)
          EQDSPSI(I,J) = FQQQ0(ZPSINSM1,ZPSIM1,ZPSI1, &
               &                ZS1,ZSIGM1,ZSIG1,ZSIG) - CPSRF
        ELSE IF (NEQDXTPO .EQ. 3) THEN
          !     EVALUATE PSI AT ZSIG WITH CUBIC USING FUNCTION VALUES
          EQDSPSI(I,J) = FCCCC0(ZPSIM1,ZPSIM2,ZPSIM3,ZPSI1, &
               &                ZSIGM1,ZSIGM2,ZSIGM3,ZSIG1,ZSIG) - CPSRF
        ELSE IF (NEQDXTPO .EQ. 4) THEN
          !     EVALUATE PSI AT ZSIG WITH CUBIC USING DERIVATIVES
          EQDSPSI(I,J) = FCDCD0(ZSIGM1,ZPSIM1,ZDPDSM1, &
               &                ZSIG1,ZPSI1,ZDPDS1,ZSIG) - CPSRF
        ELSE IF (NEQDXTPO .EQ. 5) THEN
          !     EVALUATE PSI AT ZSIG WITH QUADRATIC USING DERIVATIVE AT EDGE-1
          EQDSPSI(I,J) = FQDQ0(ZSIGM1,ZPSIM1,ZDPDSM1, &
               &                ZSIG1,ZPSI1,ZSIG) - CPSRF
        ELSE IF (NEQDXTPO .EQ. 6) THEN
          !     EVALUATE PSI AT ZSIG WITH QUADRATIC USING DERIVATIVE AT EDGE
          EQDSPSI(I,J) = FQDQ0(ZSIG1,ZPSI1,ZDPDS1, &
               &                ZSIGM1,ZPSIM1,ZSIG) - CPSRF
        ELSE IF (NEQDXTPO .EQ. 7) THEN
          !     EVALUATE PSI AT ZSIG WITH QUADRATIC
          EQDSPSI(I,J) = FQQQ0(ZPSIM2,ZPSIM3,ZPSI1, &
               &                ZSIGM2,ZSIGM3,ZSIG1,ZSIG) - CPSRF
        ELSE IF (NEQDXTPO .EQ. 8) THEN
          !     EVALUATE PSI AT ZSIG WITH CUBIC USING NS-2, NS-1, NS, NS+1
          !     EVALUATE PSI AT SIGMA = ZSIG(NS-2) AND SIGMA = ZSIG(NS-1)
          IS = NS - 2
          ZS1 = CSIG(IS)
          ZS2 = CSIG(IS+1)
          CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
          CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZS1,ZTET,ZF)
          ZSNSM2 = ZS1
          ZPSINSM2 = DDOT(16,ZF,1,ZCPSI,1)
          IS = NS - 1
          ZS1 = CSIG(IS)
          ZS2 = CSIG(IS+1)
          CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
          CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZS1,ZTET,ZF)
          ZPSINSM1 = DDOT(16,ZF,1,ZCPSI,1)
          EQDSPSI(I,J) = FCCCC0(ZPSINSM2,ZPSINSM1,ZPSIM1,ZPSI1 &
               &                ,ZSNSM2,ZS1,ZSIGM1,ZSIG1,ZSIG) - CPSRF
        ELSE
          PRINT *,' ERROR, OPTION NEQDXTPO= ',NEQDXTPO, &
               &                ' NOT DEFINED YET'
          STOP 'NEQDXTPO'
        ENDIF
        !
        !     KEEP PSI VALUE POSITIVE (>PSILIM)
        IF (EQDSPSI(I,J) .LE. 0.0_RKIND) EQDSPSI(I,J) = ZPSEDGMN
        !
      ENDIF
    END DO
  END DO
  ! Compute BR, BZ and Bphi on R,Z mesh from EQDSPSI
  ! smooth EQDSPSI with twice spline at the same time
  ! compute pprime, ffprime and f on R,Z needed for jphi and bphi
  TENSION1D=-0.03
  allocate(PSIZ_1(NRBOX,NZBOX))
  allocate(DPSIDZ_1(NRBOX,NZBOX))
  DO I=1,NRBOX
    call INTERPOS(ZZ,EQDSPSI(I,:),nin=NZBOX,nout=NZBOX,tension=TENSION1D,yout=PSIZ_1(I,1:NZBOX),youtp=DPSIDZ_1(I,1:NZBOX));
  END DO
  allocate(DPSIDR_1(NRBOX,NZBOX))
  allocate(pprime_RZ(NRBOX,NZBOX))
  allocate(ffprime_RZ(NRBOX,NZBOX))
  allocate(f_dia_RZ(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%phi(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%jphi(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%jpar(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%br(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%bz(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%bphi(NRBOX,NZBOX))
  allocate(eqchease_out(1)%profiles_2d(1)%pressure(NRBOX,NZBOX))
  nbprofiles1d=size(eqchease_out(1)%profiles_1d%psi)
  allocate(xout_profRZ(NRBOX,NZBOX))
  xout_profRZ(1:NRBOX,1:NZBOX) = EQDSPSI(1:NRBOX,1:NZBOX) + eqchease_out(1)%profiles_1d%psi(nbprofiles1d)
  IF (NVERBOSE .GE. 3) THEN
    write(0,*) 'eqchease_out(1)%profiles_1d%psi(1)= ',eqchease_out(1)%profiles_1d%psi(1)
    write(0,*) 'eqchease_out(1)%profiles_1d%psi(nbprofiles1d)= ',eqchease_out(1)%profiles_1d%psi(nbprofiles1d)
    write(0,*) 'minval(EQDSPSI)= ',minval(EQDSPSI)
    write(0,*) 'maxval(EQDSPSI)= ',maxval(EQDSPSI)
  END IF
  DO J=1,NZBOX
    CALL INTERPOS(ZR,PSIZ_1(:,J),NIN=NRBOX,NOUT=NRBOX,TENSION=TENSION1D,YOUT=EQDSPSI(:,J),YOUTP=DPSIDR_1(:,J));
    CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out(1)%profiles_1d%pprime,nin=nbprofiles1d,nout=NRBOX, &
         & xout=xout_profRZ(:,J),yout=pprime_RZ(:,J),nbc=(/2, 2 /), &
         & ybc=(/eqchease_out(1)%profiles_1d%pprime(1), eqchease_out(1)%profiles_1d%pprime(nbprofiles1d) /),option=-63)
    CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out(1)%profiles_1d%ffprime,nin=nbprofiles1d,nout=NRBOX, &
         & xout=xout_profRZ(:,J),yout=ffprime_RZ(:,J),nbc=(/2, 2 /), &
         & ybc=(/eqchease_out(1)%profiles_1d%ffprime(1), eqchease_out(1)%profiles_1d%ffprime(nbprofiles1d) /),option=-63)
    CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out(1)%profiles_1d%F_dia,nin=nbprofiles1d,nout=NRBOX, &
         & xout=xout_profRZ(:,J),yout=f_dia_RZ(:,J),nbc=(/2, 2 /), &
         & ybc=(/eqchease_out(1)%profiles_1d%f_dia(1), eqchease_out(1)%profiles_1d%f_dia(nbprofiles1d) /),option=63)
    CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out(1)%profiles_1d%phi,nin=nbprofiles1d,nout=NRBOX, &
         & xout=xout_profRZ(:,J),yout=eqchease_out(1)%profiles_2d(1)%phi(1:nrbox,J),nbc=(/2, 2 /), &
         & ybc=(/eqchease_out(1)%profiles_1d%phi(1), eqchease_out(1)%profiles_1d%phi(nbprofiles1d) /),option=63)
    eqchease_out(1)%profiles_2d(1)%br(1:nrbox,J) = - dpsidZ_1(1:nrbox,j) / ZR(1:nrbox)
    eqchease_out(1)%profiles_2d(1)%bz(1:nrbox,J) = + dpsidr_1(1:nrbox,j) / ZR(1:nrbox)
    eqchease_out(1)%profiles_2d(1)%bphi(1:nrbox,J) = f_dia_RZ(1:nrbox,j) / ZR(1:nrbox)
    eqchease_out(1)%profiles_2d(1)%jphi(1:nrbox,J) = -ZR(1:nrbox)*pprime_RZ(1:nrbox,J) - ffprime_RZ(1:nrbox,J) / ZR(1:nrbox)
    CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out(1)%profiles_1d%pressure,nin=nbprofiles1d,nout=NRBOX, &
         & xout=xout_profRZ(:,J),yout=eqchease_out(1)%profiles_2d(1)%pressure(:,J),nbc=(/2, 2 /), &
         & ybc=(/eqchease_out(1)%profiles_1d%pressure(1), eqchease_out(1)%profiles_1d%pressure(nbprofiles1d) /),option=63)
    CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out(1)%profiles_1d%jparallel,nin=nbprofiles1d,nout=NRBOX, &
         & xout=xout_profRZ(:,J),yout=eqchease_out(1)%profiles_2d(1)%jpar(:,J),nbc=(/2, 2 /), &
         & ybc=(/eqchease_out(1)%profiles_1d%jparallel(1), eqchease_out(1)%profiles_1d%jparallel(nbprofiles1d) /),option=63)
  END DO
  !
  ! Calculate chi(R,Z) (and psi(R,Z) with same method in option for testing)
  ! Use tetchi_sorted,sigchi calculated in chipsimetrics on each (smiso(1:niso1eff),chim(1:nchi)) points
  ! Method:
  ! Use thetaRZ_1d as a 1D array with all theta values of all (R,Z) points at which to find chi
  ! From rho(R,Z) and rho_bound(thetaRZ), find sigma(R,Z)
  ! First find sigmaRZ(thetaRZ_1d) and chiRZ(thetaRZ_1d) on each niso1eff surfaces from sigchi(tetchi_sorted) evaluated at thetaRZ_1d
  ! At each thetaRZ_1d, from the array of chiRZ(sigmaRZ) find value of chi at sigma(R,Z). Same for SMISO
  !
  nrzbox=nrbox*nzbox
  allocate(zsigRZ_1d_psi(NRBOX*NZBOX,niso1eff))
  allocate(zchiRZ_1d_psi(NRBOX*NZBOX,niso1eff))
  allocate(tmp_theta(3*nchi))
  allocate(tmp_chim(3*nchi))
  i3nchi = 3*nchi
  tmp_chim(1:i3nchi) = (/ chim(1:nchi)-twopi, chim(1:nchi), chim(1:nchi)+twopi /)
  do ipsi=1,niso1eff-1
    call interpos(tetchi_sorted(1:nchi,ipsi),sigchi(1:nchi,ipsi),nchi,nout=nrzbox,xout=ztetRZ_1d,tension=-0.001_rkind, &
         & yout=zsigRZ_1d_psi(:,ipsi),nbc=-1,ybc=TWOPI)
    ! To interpolate chim, since not periodic, repeat chim(theta) at -2pi and +2pi to avoid influence of boundary conditions, since both increase by 2pi every 2pi
    tmp_theta(1:i3nchi) = (/ tetchi_sorted(1:nchi,ipsi)-twopi, tetchi_sorted(1:nchi,ipsi), tetchi_sorted(1:nchi,ipsi)+twopi /)
    call interpos(tmp_theta(1:i3nchi),tmp_chim(1:i3nchi),i3nchi,nout=nrzbox,xout=ztetRZ_1d,tension=-0.001_rkind, &
         & yout=zchiRZ_1d_psi(:,ipsi))
  end do
  ipsi=niso1eff
  zsigRZ_1d_psi(1:nrzbox,ipsi) = 1._RKIND
  tmp_theta(1:i3nchi) = (/ tetchi_sorted(1:nchi,ipsi)-twopi, tetchi_sorted(1:nchi,ipsi), tetchi_sorted(1:nchi,ipsi)+twopi /)
  call interpos(tmp_theta(1:i3nchi),tmp_chim(1:i3nchi),i3nchi,nout=nrzbox,xout=ztetRZ_1d,tension=-0.001_rkind, &
       & yout=zchiRZ_1d_psi(:,ipsi))
  !
  ! Compute chi value at (theta(R,Z), sigma(R,Z)) from (zsigRZ_1d_psi(ij,:),zchiRZ_1d_psi(ij,:)), ij being index for theta(R,Z) line
  ! Each theta(R,Z) being different, one needs an interpos for each case
  ! Outside plasma boundary, use chi=cst. For psi, extrapolate with quadratic or use above option
  !
  allocate(smiso_RZ(NRBOX,NZBOX))
  do J=1,NZBOX
    do I=1,NRBOX
      iRZ_1d = (J-1)*NRBOX + I
      call interpos((/0._rkind, zsigRZ_1d_psi(iRZ_1d,1:niso1eff)/),(/zchiRZ_1d_psi(iRZ_1d,1), zchiRZ_1d_psi(iRZ_1d,1:niso1eff)/), &
           & n=niso1eff1,tension=-0.1_rkind,xscal=zsigRZ_1d(iRZ_1d),yscal=EQCHEASE_OUT_ADD_2D_RZ(I,J,iiRZ_chi), &
           & nbcscal=(/0, 0/), ybcscal=(/0._rkind, 0._rkind /), option=63)
      ! Un-comment next line if psi obtained in this way is requested (for tests in particular)
!!$      call interpos((/0._rkind, zsigRZ_1d_psi(iRZ_1d,1:niso1eff)/),(/0._rkind, smiso(1:niso1eff)/),n=niso1eff1,tension=-0.1_rkind, &
!!$           & xscal=zsigRZ_1d(iRZ_1d),yscal=smiso_RZ(I,J),nbcscal=(/2, 2/), ybcscal=(/0._rkind, 1._rkind /), option=-23)
    end do
  end do

  ! tested with WK/TESTCASES/debug/debug_fort43.m, keep lines commented
  !OS write(43,*) nrbox, nzbox, CPSRF
  !OS write(43,*) ((EQCHEASE_OUT_ADD_2D_RZ(i,j,iiRZ_chi),i=1,nrbox),j=1,nzbox)
  !OS write(43,*) ((smiso_RZ(i,j),i=1,nrbox),j=1,nzbox)

  RETURN
END SUBROUTINE PSIBOX
