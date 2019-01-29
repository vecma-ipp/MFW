SUBROUTINE IODISK(KOPT)
  !
  ! PERFORM DISK OPERATIONS
  !
  USE globals
  USE interpol
  USE interpos_module
  IMPLICIT NONE
  INCLUDE 'COMDAT.inc'
  interface
    SUBROUTINE bndfit(RIN,ZIN,KIN,RFIT,ZFIT,NBFIT,TENSION,R0,RZ0,KOPTION)
      !
      ! PERFORM DISK OPERATIONS
      !
      USE prec_const
      USE interpos_module
      IMPLICIT NONE
      INTEGER, intent(in) :: KIN, NBFIT
      INTEGER, optional :: KOPTION
      REAL(RKIND), intent(in) :: RIN(KIN), ZIN(KIN)
      REAL(RKIND), intent(out) :: RFIT(NBFIT), ZFIT(NBFIT)
      REAL(RKIND), intent(in), optional :: TENSION, R0, RZ0
    end SUBROUTINE bndfit
    SUBROUTINE COCOS(KCOCOS,Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
      !
      ! return values of exp_Bp, sigma_Bp, sigma_rhothetaphi, sign_q_pos, sign_pprime_pos
      ! from the input value KCOCOS and according to O. Sauter and S. Yu. Medvevdev paper and Table I
      ! (see paper in chease directory)
      !
      IMPLICIT NONE
      INTEGER, intent(in) :: KCOCOS
      INTEGER, intent(out) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
      !
      ! cocos=i or 10+i have similar coordinate conventions except psi/2pi for cocos=i and psi for cocos=10+i
      !
    end SUBROUTINE COCOS
  end interface
  !
  INTEGER, INTENT(IN) :: KOPT
  !
  REAL(RKIND) :: ZTEMP(NPISOEFF+NPBPS), ZTEMPP(NPISOEFF+NPBPS), ZD2TMP(NPISOEFF+NPBPS), &
       & ZR(12*NPT+1), ZZ(12*NPT+1), ZT(12*NPT+1), ZBND(12*NPT+1), &
       & ZWORK(NPISOEFF+NPBPS)
  REAL(RKIND), ALLOCATABLE :: ZPSI(:), ZTMF(:), ZTTP(:), ZCPR(:), ZCPPR(:), ZCID0(:), ZCID2(:)
  REAL(RKIND), ALLOCATABLE :: ZSTEMP(:), FCSM_eff(:), ZTEMP_P(:)
  INTEGER, ALLOCATABLE :: iFCSM(:)
  REAL(RKIND) :: ZSHIFTZ, ZCOF, ZCOFP, ZCOFT, ZCOFPP, ZCOFTTP, ZCOFJ, ZMU0, &
       & ZPSINORM, ZDUM, ZPSIAX, ZPSIEDGE, ZRMAGXP, ZZMAGXP, ZR0EXP, ZB0EXP, &
       & ZDT, X2, F1, ZRGEOM, ZZGEOM, ZAMINOR, ZDX, ZSIGN_G, zpsi_signfactor
  REAL(RKIND), SAVE :: SIGNB0XP_IN, SIGNIPXP_IN
  !
  INTEGER :: I, J, IBND, IS, L, INUM, IPPRIMEMAX(1)
  INTEGER :: IDATA, IDCHSE, IRMIN, IZMIN, IRMAX, IZMAX, INTMF0, INZBOX, INRBOX, &
       & IDUM, IMX, IMN, NPPF1, I0, I1, NPPF1_eff, Ippfun, ipropt_abs
  INTEGER :: iexp_Bp,isigma_Bp,isigma_RphiZ,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos,isigma_RphiZ_out
  !
  CHARACTER  ZDATE*8, FILECOCOS*20
  !
  ! FUNCTIONS
  INTEGER :: ISMIN, ISMAX, ISRCHFGE
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  GO TO (10,20,30,40,50,60,70,80,90,100,110,120,130) KOPT
  !
  !----------------------------------------------------------------------
  !  1. READ THE EQUILIBRIUM
  !
10 CONTINUE
  !
  OPEN(UNIT=NIN,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',STATUS='OLD',FILE='NIN')
  REWIND(NIN)
  !
  IF (NOPT.GE.-2 .AND. NOPT.LE.-1) GO TO 12
  !
  !-----------------------------------------------------------------------
  !     1.1 READ FULL EQUILIBRIUM QUANTITIES
  !
  READ(NIN) NSURF,THETA0,ASPCT,BEANS,CETA,DELTA,ELONG,RNU, &
       &             SGMA,TRIANG,TRIPLT,XI,ZR0EXP,ZB0EXP
  !     USE NAMELIST VALUES IF NEGATIVE
  IF (R0EXP .GE. RC0P) R0EXP = ZR0EXP
  IF (B0EXP .GE. RC0P) B0EXP = ZB0EXP
  R0EXP = ABS(R0EXP)
  B0EXP = ABS(B0EXP)
  !
  READ(NIN) NS,NT,NS1,NT1,NSTMAX,N4NSNT,NSOUR,NPP,NSTTP,NIPR, &
       &             NISO,NDIFT,NPPR,NBLOPT,CFBAL,NFUNC,NPPFUN,NBSFUN, &
       &             NBSOPT,NBSTRP

  IF (NISO .GE. NPISOEFF) THEN
    PRINT *,'NISO IN NIN TOO LARGE, INCREASE NPSI IN INPUT OR SET NOPT=-2'
    PRINT *,'NISO = ',NISO
    PRINT *,'NPISOEFF = ',NPISOEFF
    IF (NITMOPT .EQ. 22) RETURN
    STOP 'IODISK 10'
  END IF
  READ(NIN) CUROLD,SCALAC,BSFRAC,RZION,ETAEI,PREDGE
  READ(NIN) (CSIG(J),J=1,NS1)
  READ(NIN) (CT(J),J=1,NT1)
  READ(NIN) (BPS(J),J=1,12)
  IF (NSURF .NE. 6) RC = BPS(2)
  IF (NSURF .EQ. 7) RZ0C = BPS(6)
  !
  READ(NIN) (CSIPR(J),J=1,NISO)
  READ(NIN) (CSIPRI(J),J=1,NISO)
  READ(NIN) (CPSICL(J),J=1,N4NSNT)
  READ(NIN) SPSIM,RMAG,RZMAG,R0,RZ0
  READ(NIN) (CPPR(J),J=1,NISO)
  READ(NIN) (CID0(J),J=1,NISO)
  READ(NIN) (CID2(J),J=1,NISO)
  READ(NIN) (CIDRTOR(J),J=1,NISO)
  READ(NIN) (D2CID0(J),J=1,NISO)
  READ(NIN) (D2CID2(J),J=1,NISO)
  READ(NIN) (D2CIDRTOR(J),J=1,NISO)
  READ(NIN) (D2CPPR(J),J=1,NISO)
  !
  READ(NIN) (TTP(J),J=1,NISO)
  READ(NIN) (TMF(J),J=1,NISO)
  CALL DCOPY(NISO,TMF,1,TMFO,1)
  READ(NIN) (D2TMF(J),J=1,NISO)
  !
  READ(NIN) (AT(J),J=1,10)
  READ(NIN) (AT2(J),J=1,10)
  READ(NIN) (AT3(J),J=1,10)
  READ(NIN) (AT4(J),J=1,10)
  READ(NIN) (AP(J),J=1,10)
  READ(NIN) (AP2(J),J=1,10)
  READ(NIN) (AFBS(J),J=1,10)
  READ(NIN) (AFBS2(J),J=1,10)
  READ(NIN) NMESHB,NMESHC,NMESHD,SOLPDB,SOLPDC,SOLPDD, &
       &             NPOIDB,NPOIDC,NPOIDD
  READ(NIN) (BPLACE(J),J=1,10)
  READ(NIN) (BWIDTH(J),J=1,10)
  READ(NIN) (CPLACE(J),J=1,10)
  READ(NIN) (CWIDTH(J),J=1,10)
  READ(NIN) (DPLACE(J),J=1,10)
  READ(NIN) (DWIDTH(J),J=1,10)
  !
  IF (NBLOPT .EQ. 2 .OR. NBSOPT .EQ. 2)  THEN
    !
    READ(NIN) (PCS(J),J=1,NPPR+1)
    READ(NIN) (PCSM(J),J=1,NPPR+1)
    READ(NIN) (RPRM(J),J=1,NPPR+1)
    READ(NIN) (D2RPRM(J),J=1,NPPR+1)
    !
  ENDIF
  !
  IF (NSURF .EQ. 6) THEN
    !
    READ(NIN) NBPS
    READ(NIN) (TETBPS(L),L=1,NBPS)
    READ(NIN) (RRBPS(L),L=1,NBPS)
    READ(NIN) (RZBPS(L),L=1,NBPS)
    READ(NIN) (D2RBPS(L),L=1,NBPS)
    READ(NIN) (D2ZBPS(L),L=1,NBPS)
    !
  ELSE IF (NSURF .EQ. 7) THEN
    READ(NIN) NFOURPB
    READ(NIN) ALZERO, RC
    READ(NIN) (BPSCOS(L),BPSSIN(L),L=1,NFOURPB)
    !
  ENDIF
  !
  IF (NFUNC .EQ. 4 .OR. NPPFUN .EQ. 4 .OR. NPPFUN .EQ. 8) THEN
    !
    READ(NIN) NPPF1
    NPPF = NPPF1 - 1
    READ(NIN) (FCSM(L),L=1,NPPF1)
    READ(NIN) (RPPF(L),L=1,NPPF1)
    READ(NIN) (RFUN(L),L=1,NPPF1)
    READ(NIN) (D2RPPF(L),L=1,NPPF1)
    READ(NIN) (D2RFUN(L),L=1,NPPF1)
    !
  ENDIF
  !
  CLOSE(UNIT=NIN,STATUS='KEEP')
  RETURN
  !
  !-----------------------------------------------------------------------
  !     1.2 READ ONLY PSI AND RELATED QUATITIES FOR FIRST GUESS (RESTART)
  !
12 CONTINUE
  !
  READ(NIN) IDUM,ZDUM,ZDUM,ZDUM,ZDUM,ZDUM,ZDUM,ZDUM, &
       &             ZDUM,ZDUM,ZDUM,ZDUM,ZDUM,ZDUM
  READ(NIN) NS,NT,NS1,NT1,IDUM,N4NSNT,IDUM,IDUM,IDUM,IDUM, &
       &             NISO,IDUM,IDUM,IDUM,ZDUM,IDUM,IDUM,IDUM, &
       &             IDUM,IDUM
  READ(NIN) ZDUM,ZDUM,ZDUM,ZDUM,ZDUM,ZDUM
  READ(NIN) (CSIG(J),J=1,NS1)
  READ(NIN) (CT(J),J=1,NT1)
  !     DO NOT READ BPS, TO ALLOW NAMELIST VALUES TO OVERRIDE
  READ(NIN) (CSIPR(J),J=1,12)
  READ(NIN) (CSIPR(J),J=1,NISO)
  READ(NIN) (CSIPRI(J),J=1,NISO)
  READ(NIN) (CPSICL(J),J=1,N4NSNT)
  READ(NIN) SPSIM,RMAG,RZMAG,R0,RZ0
  !
  READ(NIN) (CPPR(J),J=1,NISO)
  READ(NIN) (CID0(J),J=1,NISO)
  READ(NIN) (CID2(J),J=1,NISO)
  READ(NIN) (CIDRTOR(J),J=1,NISO)
  READ(NIN) (D2CID0(J),J=1,NISO)
  READ(NIN) (D2CID2(J),J=1,NISO)
  READ(NIN) (D2CIDRTOR(J),J=1,NISO)
  READ(NIN) (D2CPPR(J),J=1,NISO)
  !
  READ(NIN) (TTP(J),J=1,NISO)
  READ(NIN) (TMF(J),J=1,NISO)
  CALL DCOPY(NISO,TMF,1,TMFO,1)
  READ(NIN) (D2TMF(J),J=1,NISO)
  !
  CLOSE(UNIT=NIN,STATUS='KEEP')
  RETURN
  !
  !----------------------------------------------------------------------
  !  2. STORE THE EQUILIBRIUM ON NOUT
  !
20 CONTINUE
  !
  OPEN(UNIT=NOUT,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',FILE='NOUT')
  REWIND NOUT
  WRITE(NOUT) NSURF,THETA0,ASPCT,BEANS,CETA,DELTA,ELONG,RNU, &
       &               SGMA,TRIANG,TRIPLT,XI,R0EXP,B0EXP
  WRITE(NOUT) NS,NT,NS1,NT1,NSTMAX,N4NSNT,NSOUR,NPP,NSTTP,NIPR, &
       &               NISO,NDIFT,NPPR,NBLOPT,CFBAL,NFUNC,NPPFUN,NBSFUN, &
       &               NBSOPT,NBSTRP
  WRITE(NOUT) CUROLD,SCALAC,BSFRAC,RZION,ETAEI,PREDGE
  WRITE(NOUT) (CSIG(J),J=1,NS1)
  WRITE(NOUT) (CT(J),J=1,NT1)
  WRITE(NOUT) (BPS(J),J=1,12)
  WRITE(NOUT) (CSIPR(J),J=1,NISO)
  WRITE(NOUT) (CSIPRI(J),J=1,NISO)
  WRITE(NOUT) (CPSICL(J),J=1,N4NSNT)
  WRITE(NOUT) SPSIM,RMAG,RZMAG,R0,RZ0
  WRITE(NOUT) (CPPR(J),J=1,NISO)
  WRITE(NOUT) (CID0(J),J=1,NISO)
  WRITE(NOUT) (CID2(J),J=1,NISO)
  WRITE(NOUT) (CIDRTOR(J),J=1,NISO)
  WRITE(NOUT) (D2CID0(J),J=1,NISO)
  WRITE(NOUT) (D2CID2(J),J=1,NISO)
  WRITE(NOUT) (D2CIDRTOR(J),J=1,NISO)
  WRITE(NOUT) (D2CPPR(J),J=1,NISO)
  !
  WRITE(NOUT) (TTP(J),J=1,NISO)
  WRITE(NOUT) (TMF(J),J=1,NISO)
  WRITE(NOUT) (D2TMF(J),J=1,NISO)
  !
  WRITE(NOUT) (AT(J),J=1,10)
  WRITE(NOUT) (AT2(J),J=1,10)
  WRITE(NOUT) (AT3(J),J=1,10)
  WRITE(NOUT) (AT4(J),J=1,10)
  WRITE(NOUT) (AP(J),J=1,10)
  WRITE(NOUT) (AP2(J),J=1,10)
  WRITE(NOUT) (AFBS(J),J=1,10)
  WRITE(NOUT) (AFBS2(J),J=1,10)
  WRITE(NOUT) NMESHB,NMESHC,NMESHD,SOLPDB,SOLPDC,SOLPDD, &
       &               NPOIDB,NPOIDC,NPOIDD
  WRITE(NOUT) (BPLACE(J),J=1,10)
  WRITE(NOUT) (BWIDTH(J),J=1,10)
  WRITE(NOUT) (CPLACE(J),J=1,10)
  WRITE(NOUT) (CWIDTH(J),J=1,10)
  WRITE(NOUT) (DPLACE(J),J=1,10)
  WRITE(NOUT) (DWIDTH(J),J=1,10)
  !
  IF (NBLOPT .EQ. 2 .OR. NBSOPT .EQ. 2)  THEN
    !
    WRITE(NOUT) (PCS(J),J=1,NPPR+1)
    WRITE(NOUT) (PCSM(J),J=1,NPPR+1)
    WRITE(NOUT) (RPRM(J),J=1,NPPR+1)
    WRITE(NOUT) (D2RPRM(J),J=1,NPPR+1)
    !
  ENDIF
  !
  IF (NSURF .EQ. 6) THEN
    !
    WRITE(NOUT) NBPS
    WRITE(NOUT) (TETBPS(L),L=1,NBPS)
    WRITE(NOUT) (RRBPS(L),L=1,NBPS)
    WRITE(NOUT) (RZBPS(L),L=1,NBPS)
    WRITE(NOUT) (D2RBPS(L),L=1,NBPS)
    WRITE(NOUT) (D2ZBPS(L),L=1,NBPS)
    !
  ELSE IF (NSURF .EQ. 7) THEN
    WRITE(NOUT) NFOURPB
    WRITE(NOUT) ALZERO, RC
    WRITE(NOUT) (BPSCOS(L),BPSSIN(L),L=1,NFOURPB)
    !
  ENDIF
  !
  IF (NFUNC .EQ. 4 .OR. NPPFUN .EQ. 4 .OR. NPPFUN .EQ. 8) THEN
    !
    NPPF1 = NPPF + 1
    WRITE(NOUT) NPPF1
    WRITE(NOUT) (FCSM(L),L=1,NPPF1)
    WRITE(NOUT) (RPPF(L),L=1,NPPF1)
    WRITE(NOUT) (RFUN(L),L=1,NPPF1)
    WRITE(NOUT) (D2RPPF(L),L=1,NPPF1)
    WRITE(NOUT) (D2RFUN(L),L=1,NPPF1)
    !
  ENDIF
  !
  CLOSE(UNIT=NOUT,STATUS='KEEP')
  RETURN
  !
  !----------------------------------------------------------------------
  !  3.   READ EXPERIMENTAL EQUILIBRIUM ON EXPEQ
  !       IF (NSURF=6 .AND. NEQDSK=1) EXPEQ IS IN EQDSK FORMAT (MKSA)
  !       AND VARIABLES ARE NORMALIZED
  !       IF MOD(NITMOPT,10)>=1, I.E.NITMOPT=1 OR 11 OR 22, read exp. equilibrium as structure
  !
30 CONTINUE
  !
  if (nsurf.eq.6 .and. MOD(NITMOPT,10).GE.1) GO TO 35
  !
  OPEN(UNIT=NXIN,ACCESS='SEQUENTIAL',FORM='FORMATTED', FILE='EXPEQ')
  REWIND(NXIN)
  !
  IF (NSURF.EQ.6 .AND. NEQDSK.GE.1) GO TO 32
  !
  !       1. NORMAL EXPEQ FILE
  !
  READ(NXIN,*) ASPCT
  READ(NXIN,*) RZ0C
  READ(NXIN,*) PREDGE
  IF (RZ0.EQ.RC0P .AND. RZ0C.NE.RC0P) RZ0 = RZ0C
  !
  IF (NSURF .EQ. 6) THEN
    READ(NXIN,*) NBPS
    IF (NBPS .GT. NPBPS) THEN
      PRINT*,'NBPS LARGER THAN NPBPS.RECOMPILE WITH LARGER NPBPS'
      IF (NITMOPT .EQ. 22) RETURN
      STOP
    ENDIF
    READ(NXIN,*) (RRBPS(L),  RZBPS(L), L=1,NBPS)
    !
    ! Use interpos with periodic boundary conditions to smooth boundary, otherwise gives bad resolution for CHEASE
    !
    IF (TENSBND .NE. 0._RKIND) THEN
      IDUM = MIN(2*NBPS,NPBPS)
      call bndfit(RRBPS,RZBPS,NBPS,ZTEMP,ZTEMPP,IDUM,TENSBND)
      NBPS = IDUM
      RRBPS = ZTEMP(1:NBPS)
      RZBPS = ZTEMPP(1:NBPS)
    END IF
  ENDIF
  !
  IF (NSURF .EQ. 7) THEN
    READ(NXIN,*) NFOURPB
    !     NEEDS VALUES OF RC AND RZ0C TO CORRECTLY RECONSTRUCT PLASMA BOUNDARY
    READ(NXIN,*) ALZERO, RC
    READ(NXIN,*) (BPSCOS(L),BPSSIN(L),L=1,NFOURPB)
  END IF
  !
  IF (NFUNC.EQ.4 .AND. (NPPFUN.EQ.4 .OR. NPPFUN.EQ.8)) THEN
    !    BOTH PROFILES GIVEN AS AN ARRAY OF NPPF1 POINTS VS S
    !    NPPFUN=8: P(S) GIVEN
    READ(NXIN,*) NPPF1
    NRHOMESH = 0
    IF (NFUNRHO .EQ. 0) THEN
      READ(NXIN,*) NSTTP
    ELSE
      !    NRHOMESH=0 (S-MESH AS WITH NFUNRHO=0), =1 (RHO_TOR MESH)
      READ(NXIN,*) NSTTP,NRHOMESH
    ENDIF
    IF (NVERBOSE .GE. 1) PRINT *,'NFUNRHO, NRHOMESH= ',NFUNRHO, NRHOMESH
    !
    IF (NPPF1 .GT. NPBPS) THEN
      PRINT*,'NPPF1 LARGER THAN NPBPS. RECOMPILE WITH LARGER NPBPS'
      IF (NITMOPT .EQ. 22) RETURN
      STOP
    ENDIF
    !
    READ(NXIN,*) (FCSM(L), L=1,NPPF1)
    IF (NFUNRHO.EQ.1 .AND. NRHOMESH.EQ.1) THEN
      ! input s is rho_tor, use only rho_tor_norm, so renormalize to make sure
      IF (FCSM(NPPF1) .NE. 0._rkind) THEN
        FCSM(1:NPPF1) = FCSM(1:NPPF1) / FCSM(NPPF1)
        FCSM(NPPF1) = 1._RKIND
      ELSE
        write(6,*) 'Bad input, s(end) = 0., should have rho_tor with 0 in center'
        flush(6)
        if (NITMOPT .NE. 22) STOP 'iodisk'
        return
      END IF
    END IF
    ! check that there are not 2 points too close  in rho: now 1e-5 from average dx
    zdx=(FCSM(NPPF1)-FCSM(1)) / real(NPPF1,rkind)
    NPPF1_eff = NPPF1
    allocate(FCSM_eff(NPPF1_eff))
    allocate(iFCSM(NPPF1_eff))
    i=1
    iFCSM(i)=1
    FCSM_eff(1)=FCSM(1)
    do j=2,NPPF1_eff
      if (abs(FCSM(j)-FCSM_eff(i)) .gt. 1e-5_rkind*zdx) then
        i=i+1;
        FCSM_eff(i) = FCSM(j)
        iFCSM(i)=j
      end if
    end do
    NPPF1_eff = i
    IF (NPPF1_eff .NE. NPPF1) &
      & WRITE(0,*) 'THERE WERE POINTS WITH SAME X VALUES: NEW NPPF1_eff=',NPPF1_eff,' , NPPF1=',NPPF1
    FCSM(1:NPPF1_eff) = FCSM_eff(1:NPPF1_eff)

    READ(NXIN,*) (RPPF(L), L=1,NPPF1)
    RPPF(1:NPPF1_eff) = RPPF(iFCSM(1:NPPF1_eff))
    allocate(ZTEMP_P(NPPF1_eff))
    ZTEMP_P(1:NPPF1_eff) = RPPF(1:NPPF1_eff)
    IF ((NPPFUN .NE. 8) .AND. (TENSPROF .NE. 0._RKIND)) THEN
      ! pressure done later to get derivative, so avoid smoothing twice
      call interpos(FCSM(1:NPPF1_eff),RPPF(1:NPPF1_eff),NPPF1_eff,tension=TENSPROF, &
        & yout=ZTEMP(1:NPPF1_eff),nbc=(/1,2/),ybc=(/RC0P, RPPF(NPPF1_eff)/))
      RPPF(1:NPPF1_eff) = ZTEMP(1:NPPF1_eff)
    END IF
    READ(NXIN,*) (RFUN(L), L=1,NPPF1)
    RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    IF (TENSPROF .NE. 0._RKIND) THEN
      call interpos(FCSM(1:NPPF1_eff),RFUN(1:NPPF1_eff),NPPF1_eff,tension=TENSPROF, &
        & yout=ZTEMP(1:NPPF1_eff),nbc=(/1,2/),ybc=(/RC0P, RFUN(NPPF1_eff)/))
      RFUN(1:NPPF1_eff) = ZTEMP(1:NPPF1_eff)
        IF (NVERBOSE .GE. 1) print *,'Added smoothing of RFUNprofiles with TENSPROF = ',TENSPROF
    END IF
    !
    NPPF1 = NPPF1_eff
    NPPF = NPPF1 - 1
    !
    IF (NPPFUN .EQ. 8) THEN
      ! DEFINE PEDGE, make sure p is positive
      RPPF(1:NPPF1) = ABS(RPPF(1:NPPF1))
      PREDGE = RPPF(NPPF1)
    END IF
    !
  ELSE IF (NFUNC.EQ.1 .AND.NPPFUN.EQ.1) THEN
    !
    !    BOTH PROFILES GIVEN AS POLYNOMIAL OF PSI/PSIAXIS OF DEGREE NSOUR-1
    !
    READ(NXIN,*) NSOUR,NSTTP
    !
    IF (NSOUR .GT. 10) THEN
      PRINT *,' NSOUR TOO LARGE IN EXPEQ'
      IF (NITMOPT .EQ. 22) RETURN
      STOP
    ENDIF
    !
    READ(NXIN,*) (AP(L),L=1,NSOUR)
    READ(NXIN,*) (AT(L),L=1,NSOUR)
    !
  ELSE
    !        MIXED PROFILES AS POLYNOMIAL AND ARRAY
    !
    !      P-PRIME PROFILE
    IF (NPPFUN .EQ. 4 .OR. NPPFUN .EQ. 8) THEN
      READ(NXIN,*) NPPF1
      READ(NXIN,*) NSTTP
      IF (NPPF1 .GT. NPBPS) THEN
        PRINT*,'NPPF1 LARGER THAN NPBPS. RECOMPILE WITH LARGER NPBPS'
        IF (NITMOPT .EQ. 22) RETURN
        STOP
      ENDIF
      READ(NXIN,*) (FCSM(L), L=1,NPPF1)
      READ(NXIN,*) (RPPF(L), L=1,NPPF1)
      IF ((NPPFUN .NE. 8) .AND. (TENSPROF .NE. 0._RKIND)) THEN
        ! pressure done later to get derivative, so avoid smoothing twice
        call interpos(FCSM(1:NPPF1),RPPF(1:NPPF1),NPPF1,tension=TENSPROF, &
          & yout=ZTEMP(1:NPPF1),nbc=(/1,2/),ybc=(/RC0P, RPPF(NPPF1)/))
        RPPF(1:NPPF1) = ZTEMP(1:NPPF1)
          IF (NVERBOSE .GE. 1) print *,'Added smoothing of RPPF profile with TENSPROF = ',TENSPROF
      END IF
      NPPF = NPPF1 - 1
      IF (NPPFUN .EQ. 8) THEN
        ! DEFINE PEDGE, make sure p is positive
        RPPF(1:NPPF1) = ABS(RPPF(1:NPPF1))
        PREDGE = RPPF(NPPF1)
      END IF
    ELSE IF (NPPFUN .EQ. 1) THEN
      READ(NXIN,*) NSTTP
      READ(NXIN,*) NSOUR
      READ(NXIN,*) (AP(L),L=1,NSOUR)
    ENDIF
    !
    !      TT-PRIME OR I-PRIME OR .. PROFILE
    IF (NFUNC .EQ. 4) THEN
      READ(NXIN,*) NPPF1
      READ(NXIN,*) NSTTP
      IF (NPPF1 .GT. NPBPS) THEN
        PRINT*,'NPPF1 LARGER THAN NPBPS. RECOMPILE WITH LARGER NPBPS'
        IF (NITMOPT .EQ. 22) RETURN
        STOP
      ENDIF
      READ(NXIN,*) (FCSM(L), L=1,NPPF1)
      READ(NXIN,*) (RFUN(L), L=1,NPPF1)
      IF (TENSPROF .NE. 0._RKIND) THEN
        call interpos(FCSM(1:NPPF1),RFUN(1:NPPF1),NPPF1,tension=TENSPROF, &
          & yout=ZTEMP(1:NPPF1),nbc=(/1,2/),ybc=(/RC0P, RFUN(NPPF1)/))
        RFUN(1:NPPF1) = ZTEMP(1:NPPF1)
          IF (NVERBOSE .GE. 1) print *,'Added smoothing of RFUNprofile with TENSPROF = ',TENSPROF
      END IF
      NPPF = NPPF1 - 1
    ELSE IF (NFUNC .EQ. 1) THEN
      READ(NXIN,*) NSTTP
      READ(NXIN,*) NSOUR
      READ(NXIN,*) (AT(L),L=1,NSOUR)
    ENDIF
  ENDIF
  !
  ! CONSTRUCT PPRIME FROM P INPUT PROFILE
  !
  IF (NPPFUN .EQ. 8) THEN
    IF (NFUNRHO.EQ.0 .AND. NRHOMESH.EQ.0) THEN
      IF (TENSPROF .NE. 0._RKIND) THEN
        call interpos(FCSM(1:NPPF1)*FCSM(1:NPPF1),RPPF,NPPF1,tension=TENSPROF, &
          & YOUTP=ZTEMPP,nbc=(/2,2/),ybc=(/RPPF(1), RPPF(NPPF1)/))
!        call interpos(FCSM(1:NPPF1)*FCSM(1:NPPF1),RPPF,NPPF1,tension=TENSPROF, &
!          & YOUTP=ZTEMPP,nbc=(/1,2/),ybc=(/RC0P, RPPF(NPPF1)/))
        IF (NVERBOSE .GE. 1) print *,'Added smoothing of p profile with TENSPROF = ',TENSPROF
      ELSE
        call interpos(FCSM(1:NPPF1)*FCSM(1:NPPF1),RPPF,NPPF1,tension=TENSPROF, &
          & YOUTP=ZTEMPP,nbc=(/0,0/),ybc=(/RC0P, RC0P/))
      END IF
      RPPF(1:NPPF1)=ZTEMPP(1:NPPF1) ! RPPF = dp/dpsi_normalized
    ELSEIF (NFUNRHO.EQ.1 .AND. NRHOMESH.EQ.1) THEN
      ! P(RHOTOR) GIVEN, could compute here dp/drhotor but then dp/dpsi(0) can never be non-zero
      ! So has to compute dp/dpsi or dp/dpsi_norm on psi at each interation
      ! RPPF(1:NPPF1) = p(drho_tor_in_norm)
    ELSE
      WRITE(6,*) 'OPTION NOT YET IMPLMENTED: NPPFUN=8 AND NFUNRHO= ',NFUNRHO,' , NRHOMESH= ',NRHOMESH
      FLUSH(6)
      IF (NITMOPT .NE. 22) STOP 'IODISK'
      RETURN
    END IF
  END IF
  !
  ! MODIFY I-PRIME PROFILE IN CENTRAL REGION (MAKE IT MORE FLAT TO KEEP Q0  AWAY FROM 1)
  ! ARGUMENTS OF FQDQ0 (SEE INTERPOL.F90):
  ! FQDQ0(X1,F1,SLOPE1,X2,F2,X) WHERE F1=F(X1), SLOPE1=DF/DX(X1), F2=F(X2)
  ! TO GET THE DESIRED Q PROFILE, PLAY WITH X2 AND F1
  !
  CALL RARRAY('FCSM',FCSM,NPPF1)
  CALL RARRAY('RFUN',RFUN,NPPF1)
  !
  IF (BENTQPROFILE==1) THEN
    X2=BENTRADIUS
    IS=ISRCHFGE(NPPF1,FCSM,1,X2)
    F1=BENTAXIS*RFUN(IS)
    DO I=1,NPPF1
      IF (FCSM(I)<X2) THEN
        RFUN(I)=FQDQ0(0._RKIND,F1,0._RKIND,FCSM(IS),RFUN(IS),FCSM(I))
      END IF
    ENDDO
    IF (NVERBOSE .GE. 1) THEN
      PRINT*, 'WARNING: I* OR J// AND THEREFORe Q HAS BEEN MODIFIED IN CENTRAL PLASMA'
      PRINT*, 'IN SUBROUTINE IODISK, BENTQPROFILE==1 AFTER READING PROFILES IN EXPEQ'
      CALL RARRAY('RFUN',RFUN,NPPF1)
    END IF
  ENDIF
  !
  ! Write equivalent expeq file in order to be able to test outside kepler if there is a problem
  IF (NVERBOSE .GE. 1) THEN
    OPEN(UNIT=NXPQOUT,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='EXPEQ_EXPEQ.IN')
    REWIND(NXPQOUT)
    WRITE(NXPQOUT,1303) ASPCT,RZ0/R0EXP,PREDGE
    WRITE(NXPQOUT,1301) NBPS
    WRITE(NXPQOUT,1302) (RRBPS(L),  RZBPS(L), L=1,NBPS)
    WRITE(NXPQOUT,1304) NPPF1,nppfun
    WRITE(NXPQOUT,1304) nsttp,NRHOMESH
    WRITE(NXPQOUT,1303) (FCSM(L), L=1,NPPF1)
    WRITE(NXPQOUT,1303) (rppf(L), L=1,NPPF1)
    WRITE(NXPQOUT,1303) (rfun(L), L=1,NPPF1)
    IF (NPPFUN .EQ. 8) THEN
      WRITE(NXPQOUT,1303) (ZTEMP_P(L), L=1,NPPF1_eff)
    END IF
    CLOSE(NXPQOUT)
  END IF
  !
  GO TO 39
  !
  !-----------------------------------------------------------------------
  !        3.2 EQDSK TYPE OF EXPEQ
32 CONTINUE
  !        READ MKSA VALUES
  !
  IF (NVERBOSE .GE. 1) PRINT *,'READS FILE EXPEQ ASSUMING A FORMAT ALA EQDSK ASSUMING COCOS = ',COCOS_IN
  READ(NXIN,'(48X)',ADVANCE='NO')
  READ(NXIN,*) IDUM,INRBOX,INZBOX
  IF (NVERBOSE .GE. 1) THEN
    PRINT *,' INRBOX= ',INRBOX
    PRINT *,' INZBOX= ',INZBOX
  END IF
  IF (INRBOX .GT. NPBPS) THEN
    PRINT *,' NRBOX LARGER THAN NPBPS IN EQDSK, SHOULD .LE. ',NPBPS
    PRINT *,' RECOMPILE CHEASE WITH LARGER NPBPS'
    IF (NITMOPT .EQ. 22) RETURN
    STOP
  ENDIF
  IF (INRBOX*INZBOX .GT. NPBPS*NPBPS) THEN
    PRINT *,' NRBOX*NZBOX .GT. NPBPS*NPBPS: NRBOX= ',INRBOX, &
         &  '  NZBOX= ',INZBOX,'  NPBPS= ',NPBPS
    PRINT *,' RECOMPILE CHEASE WITH LARGER NPBPS'
    IF (NITMOPT .EQ. 22) RETURN
    STOP
  ENDIF
  !
  ZR0EXP = R0EXP         ! NAMELIST VALUE
  READ(NXIN,9381) RBOXLEN_IN,ZBOXLEN_IN,R0EXP,RBOXLFT_IN,ZBOXMID_IN
  IF (RBOXLEN .LE. 0._RKIND) THEN
    ! box for eqdsk out not defined in namelist so copy from input
    ! Note: Chease does not need in input the R,Z mesh, thus RBOXLEN_IN not used as such
    RBOXLEN = RBOXLEN_IN
    RBOXLFT = RBOXLFT_IN
  END IF
  IF (ZBOXLEN .LE. 0._RKIND) THEN
    ZBOXLEN = ZBOXLEN_IN
    ZBOXMID = ZBOXMID_IN
  END IF
  READ(NXIN,9381) ZRMAGXP,ZZMAGXP,ZPSIAX,ZPSIEDGE,B0EXP
  READ(NXIN,9381) CURRT
  ! SIGNB0XP OR SIGNIPXP FROM NAMELIST SHOULD BE THE OUTPUT VALUES EXCEPT IF <-1
  ! THEN USE EQDSK VALUES AND FOLLOW COCOS_IN -> _OUT TRANSFORMATION
  ! EQDSK INPUT VALUES PROVIDE INPUT VALUES
  SIGNB0XP_IN = SIGN(RC1P,B0EXP)
  SIGNIPXP_IN = SIGN(RC1P,CURRT)

  READ(NXIN,9381) ZDUM
  !
  ! SET MESH AXIS TO MAGNETIC AXIS
  R0 = ZRMAGXP / R0EXP
  RZ0 = ZZMAGXP / R0EXP
  !
  NPPF1 = INRBOX
  NPPF = NPPF1 -1
  !     G
  READ(NXIN,9381) (RFUN(L), L=1,NPPF1)
  !
  !     NORMALIZATION OF EQDSK USES R0EXP AND B0EXP. BY DEFAULT, ONE USES
  !     THE VALUES FROM EQDSK. BUT THIS IS NOT THE CASE IF:
  !
  !     IF (ROEXP(NAMELIST) < 0.) THEN
  !
  !     ROEXP = ABS(R0EXP(NAMELIST))
  !     B0EXP = G(1-NTMF0) / R0EXP  (G(0)=G AT CENTER; G(1)=G AT EDGE)
  !
  !     ELSE IF (NEQDSK .EQ. 2) THEN
  !
  !     R0EXP = R0EXP(EQDSK)
  !     BOEXP = G(1-NTMF0) / R0EXP, NTMFO FROM NAMELIST
  !
  !     ENDIF
  !
  !     CHECK IF NEED TO RENORMALIZE ACCORDING TO NTMF0 OR R0EXP(NAMELIST)
  !
  ZSIGN_G = SIGN(RC1P,RFUN(1))
  INTMF0 = 0
  IF (ABS(RFUN(1)-R0EXP*B0EXP) .LE. 1.E-04_RKIND*(ABS(RFUN(1)))) THEN
    INTMF0 = 1
    IF (NVERBOSE .GE. 1) THEN
      WRITE(6,'(/,"   EQDSK HAS T(1) = R0*B0")')
      WRITE(6,'("   RFUN(1)= ",1PE12.4,"  R0EXP*B0EXP= ",E12.4,/)') &
        &       RFUN(1),R0EXP*B0EXP
    END IF
  ENDIF
  !
  IF (ZR0EXP .LT. RC0P) THEN
    IF (NVERBOSE .GE. 1) WRITE(6,'(/," R0EXP_EQDSK=",1PE12.4, &
      &       " CHANGED TO R0EXP_NAMELIST=",E12.4)') R0EXP,-ZR0EXP
    R0EXP = ABS(ZR0EXP)
    IF (NTMF0 .EQ. 0) ZB0EXP = RFUN(NPPF1) / R0EXP
    IF (NTMF0 .EQ. 1) ZB0EXP = RFUN(1    ) / R0EXP
    IF (NVERBOSE .GE. 1) WRITE(6,'(" B0EXP_EQDSK=",1PE12.4, &
      &       " CHANGED TO B0EXP=",E12.4)') B0EXP,ZB0EXP
    B0EXP = ZB0EXP
    IF (NVERBOSE .GE. 1) WRITE(6,'(" R0*B0=",1PE12.4," G(CENTER)=",E12.4, &
      &       " G(EDGE)=",E12.4,/)') R0EXP*B0EXP,RFUN(1),RFUN(NPPF1)
  ELSE IF (NEQDSK.EQ.2 .AND. INTMF0.NE.NTMF0) THEN
    IF (NTMF0 .EQ. 0) ZB0EXP = RFUN(NPPF1) / R0EXP
    IF (NTMF0 .EQ. 1) ZB0EXP = RFUN(1    ) / R0EXP
    IF (NVERBOSE .GE. 1) WRITE(6,'(" B0EXP_EQDSK=",1PE12.4, &
      &       " CHANGED TO B0EXP=",E12.4)') B0EXP,ZB0EXP
    B0EXP = ZB0EXP
    IF (NVERBOSE .GE. 1) WRITE(6,'(" R0*B0=",1PE12.4," G(CENTER)=",E12.4, &
      &       " G(EDGE)=",E12.4,/)') R0EXP*B0EXP,RFUN(1),RFUN(NPPF1)
  ELSE IF (INTMF0 .NE. NTMF0) THEN
    IF (NVERBOSE .GE. 1) WRITE(6,'(/,"  WARNING: EQDSK HAS NOT SAME NORMALIZATION AS", &
      &       " ASK BY NTMF0: => EQUIL. IS DIFFERENT")')
  ENDIF
  !     P
  READ(NXIN,9381) (RPPF(L), L=1,NPPF1)
  PREDGE = RPPF(NPPF1)
  IF (PREDGE .LT. RC0P) THEN
    IF (NVERBOSE .GE. 1) WRITE(6,'(//,"   WARNING, PREDGE < 0 IN EXPEQ: PREDGE SET", &
      &       " TO 0.0_RKIND",/)')
    PREDGE = 0.0_RKIND
  ENDIF
  !     GG'
  READ(NXIN,9381) (RFUN(L), L=1,NPPF1)
  !     P'
  READ(NXIN,9381) (RPPF(L), L=1,NPPF1)
  !
  READ(NXIN,9381) ((EQDSPSI(I,J),I=1,INRBOX),J=1,INZBOX)
  !
  READ(NXIN,9381) (QPSIIN(I),I=1,INRBOX)
  IF (NVERBOSE .GE. 1) THEN
    PRINT *,' Q(0)= ', QPSIIN(1)
    PRINT *,' Q(EDGE)= ',QPSIIN(INRBOX)
  END IF
  !
  READ(NXIN,*) NBPS, NWALLPOS
  IF (NBPS .GT. NPBPS) THEN
    PRINT *,'NBPS= ',NBPS,' LARGER THAN NPBPS=',NPBPS,'.RECOMPILE WITH LARGER NPBPS'
    IF (NITMOPT .EQ. 22) RETURN
    STOP
  ENDIF
  !
  READ(NXIN,9381) (RRBPS(L),  RZBPS(L), L=1,NBPS)
  !
  ! Use interpos with periodic boundary conditions to smooth boundary, otherwise gives bad resolution for CHEASE
  !
  IF (abs(TENSBND) .GE. 1.E-12_RKIND) THEN
    IDUM = MIN(2*NBPS,NPBPS)
    call bndfit(RRBPS,RZBPS,NBPS,ZTEMP,ZTEMPP,IDUM,TENSBND)
    NBPS = IDUM
    RRBPS = ZTEMP(1:NBPS)
    RZBPS = ZTEMPP(1:NBPS)
  END IF
  !
  READ(NXIN,9381) (WALLPOSR(L),  WALLPOSZ(L), L=1,NWALLPOS)
  !
  !        AUXILIARY PARAMETERS
  !        (COMMENT NEXT TWO LINES IF WANT TO CHANGE QSPEC IN NAMELIST)
  IF (NVERBOSE .GE. 3) THEN
    PRINT *,' QSPEC= ',QSPEC
    PRINT *,' EQDSPSI(INRBOX,INZBOX)= ',EQDSPSI(INRBOX,INZBOX)
    PRINT *,' QPSIIN= ',QPSIIN(1:INRBOX)
  END IF
  IF (QSPEC .GT. RC0P) THEN
    QSPEC = ABS(QPSIIN(1))
    CSSPEC = 0.0_RKIND
  ELSE
    QSPEC = ABS(QSPEC)
  ENDIF
  IF (NVERBOSE .GE. 3) THEN
    PRINT *,' QSPEC= ',QSPEC
    PRINT *,' NCSCAL= ',NCSCAL
  END IF
  !
  !        EQUIDISTANT MESH IN PSI
  !
  ZCOF = 1._RKIND / REAL(NPPF1-1,RKIND)
  DO I=1,NPPF1
    FCSM(I) = SQRT(REAL(I-1,RKIND)*ZCOF)
  END DO
  !
  IF (TENSPROF .NE. 0._RKIND) THEN
    call interpos(FCSM(1:NPPF1),RPPF(1:NPPF1),NPPF1,tension=TENSPROF, &
      & yout=ZTEMP(1:NPPF1),nbc=(/1,2/),ybc=(/RC0P, RPPF(NPPF1)/))
    RPPF(1:NPPF1) = ZTEMP(1:NPPF1)
    call interpos(FCSM(1:NPPF1),RFUN(1:NPPF1),NPPF1,tension=TENSPROF, &
      & yout=ZTEMP(1:NPPF1),nbc=(/1,2/),ybc=(/RC0P, RFUN(NPPF1)/))
    RFUN(1:NPPF1) = ZTEMP(1:NPPF1)
    IF (NVERBOSE .GE. 1) print *,'Added smoothing of RFUNprofiles with TENSPROF = ',TENSPROF
  END IF
  !
  !     NORMALIZATION AND SIGNS, USE COCOS_IN INDEX
  !
  CALL COCOS(COCOS_OUT,iexp_Bp,isigma_Bp,isigma_RphiZ_out,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)
  CALL COCOS(COCOS_IN,iexp_Bp,isigma_Bp,isigma_RphiZ,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)
  !
  ! If set to -9 in namelist, use input with input cocos as signs, otherwise force to namelist values
  IF (SIGNIPXP < -1) SIGNIPXP = SIGNIPXP_IN * isigma_RphiZ * isigma_RphiZ_out
  IF (SIGNB0XP < -1) SIGNB0XP = SIGNB0XP_IN * isigma_RphiZ * isigma_RphiZ_out
  !
  ! Check some inputs
  IF (sign(RC1P,QPSIIN(INRBOX))*SIGNIPXP_IN*SIGNB0XP_IN*isigma_rhothetaphi .LT. 0.) THEN
    IF (NVERBOSE .GE. 0) print *,'Input EQDSK is not consistent, q should be have sign(q)=', &
         & SIGNIPXP_IN*SIGNB0XP_IN*isigma_rhothetaphi,' with COCOS_IN = ',COCOS_IN
  END IF
  IF (sign(RC1P,ZSIGN_G)*SIGNB0XP_IN .LT. 0) THEN
    print *,'Input EQDSK is not consistent, SIGN(B0)=',SIGNB0XP_IN,' IS NOT EQUAL TO SIGN(G)=',sign(RC1P,ZSIGN_G)
!!$    IF (NITMOPT .NE. 22 .AND. NVERBOSE .LE. 1) STOP 'COCOS IN IODISK'
  END IF
  IPPRIMEMAX = MAXLOC(abs(RPPF(1:NPPF1)))
  IF (sign(RC1P,RPPF(ipprimemax(1))) .NE. -SIGNIPXP_IN*isigma_Bp) THEN
    print *,'Sign of pprime is not correct with respect to COCOS_IN = ',COCOS_IN
    print *,'Sign of pprime = ',sign(RC1P,RPPF(ipprimemax(1)))
    print *,'-SIGNIPXP_IN*isigma_Bp = ',-SIGNIPXP_IN*isigma_Bp
!    IF (NITMOPT .NE. 22) STOP 'COCOS IN IODISK'
  END IF
  IF ((ZPSIAX .GT. ZPSIEDGE) .and. (SIGNIPXP_IN*isigma_Bp .GT. 0)) THEN
    print *,'Input EQDSK is not consistent, psi should be increasing with COCOS_IN = ',COCOS_IN,' and sign(Ip) = ',SIGNIPXP_IN
    IF (NVERBOSE .GE. 1) print *,'Note that dp/dpsi should be positive'
!!$    IF (NITMOPT .NE. 22 .AND. NVERBOSE .LE. 1) STOP 'COCOS IN IODISK'
  ELSEIF ((ZPSIAX .LT. ZPSIEDGE) .and. (SIGNIPXP_IN*isigma_Bp .LT. 0)) THEN
    print *,'Input EQDSK is not consistent, psi should be decreasing with COCOS_IN = ',COCOS_IN,' and sign(Ip) = ',SIGNIPXP_IN
    IF (NVERBOSE .GE. 1) print *,'Note that dp/dpsi should be negative'
!!$    IF (NITMOPT .NE. 22 .AND. NVERBOSE .LE. 1) STOP 'COCOS IN IODISK'
  END IF
  
  ZPSINORM = ABS(R0EXP*R0EXP * B0EXP)
  ZMU0 = 4.E-07_RKIND * CPI
  CURRT = ABS(CURRT)*ZMU0/R0EXP/ABS(B0EXP)
  ZCOFTTP = SIGNIPXP_IN * isigma_Bp * (twopi)**iexp_Bp / ABS(B0EXP)
  ZCOFPP = SIGNIPXP_IN * isigma_Bp * (twopi)**iexp_Bp * ZMU0 * R0EXP**2 / ABS(B0EXP)
  ZCOFT = SIGNB0XP_IN / R0EXP / B0EXP
  ZCOFP = ZMU0 / B0EXP**2
  IF (NVERBOSE .GE. 1) PRINT *,'ZPSINORM, CURRT, ZCOFTTP, ZCOFPP, ZCOFT, ZCOFP = ', &
    & ZPSINORM, CURRT, ZCOFTTP, ZCOFPP, ZCOFT, ZCOFP
  !
  !     NOW CAN IMPOSE B0EXP POSITIVE
  B0EXP = ABS(B0EXP)
  !
  !        NORMALIZE USING R0EXP AND B0EXP
  !        NOTE: R0 = R0EXP / R0EXP = 1. MAY NOT BE MIDDLE OF PLASMA
  !
  ZCOF = 1._RKIND / R0EXP
  DO I=1,NBPS
    RRBPS(I) = RRBPS(I) * ZCOF
    RZBPS(I) = RZBPS(I) * ZCOF
  END DO
  !
  DO I=1,NPPF1
    RFUN(I) = RFUN(I) * ZCOFTTP
    RPPF(I) = RPPF(I) * ZCOFPP
  END DO
  PREDGE = PREDGE * ZCOFP
  !
  !        FIND PLASMA LIMITS
  IRMAX = ISMAX(NBPS,RRBPS,1)
  IRMIN = ISMIN(NBPS,RRBPS,1)
  IZMAX = ISMAX(NBPS,RZBPS,1)
  IZMIN = ISMIN(NBPS,RZBPS,1)
  RZ0 = 0.5_RKIND * (RZBPS(IZMIN) + RZBPS(IZMAX))
  ASPCT = (RRBPS(IRMAX) - RRBPS(IRMIN)) &
       &     / (RRBPS(IRMAX) + RRBPS(IRMIN))
  IF (NVERBOSE .GE. 1) WRITE(6,'(/,3(A,1PE13.4),/)') ' FROM EQDSK: ,R0GEOM= ', &
    &  0.5_RKIND * (RRBPS(IRMIN) + RRBPS(IRMAX)), &
    &  ' Z0GEOM= ',RZ0,' INV. ASPECT RATIO= ',ASPCT
  !
  ! Write equivalent expeq file in order to be able to test outside kepler if there is a problem
  IF (NVERBOSE .GE. 1) THEN
    OPEN(UNIT=NXPQOUT,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='EXPEQ_EQDSK.IN')
    REWIND(NXPQOUT)
    WRITE(NXPQOUT,1303) ASPCT,RZ0/R0EXP,PREDGE
    WRITE(NXPQOUT,1301) NBPS
    WRITE(NXPQOUT,1302) (RRBPS(L),  RZBPS(L), L=1,NBPS)
    WRITE(NXPQOUT,1304) NPPF1,nppfun
    I0=0
    WRITE(NXPQOUT,1304) nsttp,I0
    WRITE(NXPQOUT,1303) (FCSM(L), L=1,NPPF1)
    WRITE(NXPQOUT,1303) (rppf(L), L=1,NPPF1)
    WRITE(NXPQOUT,1303) (rfun(L), L=1,NPPF1)
    CLOSE(NXPQOUT)
  END IF
  !
  GOTO 39
  !-----------------------------------------------------------------------
  !       3.5 READ FROM EUITM DATABASE
  !
35 CONTINUE
  !        READ MKSA VALUES
      IF (NVERBOSE .GE. 1) PRINT *,'READS FILE EXPEQ ASSUMING AN EUITM STRUCTURE AND COCOS_IN = ',COCOS_IN
  !
  ! for all cases:
  !
  ZR0EXP = R0EXP         ! NAMELIST VALUES
  ZB0EXP = B0EXP         !
  CALL RVAR2('NAMELIST R0EXP',R0EXP,' B0EXP',B0EXP)
  !
  ! Now machine values are usualy given and they should be the ones used for R0EXP/B0EXP, so first look at GLOBAL_PARAM%TOROID_FIELD
  if (EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%R0 .GT. -1e+40_rkind) THEN
    R0EXP = EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%R0
    IF (NVERBOSE .GE. 3) write(*,*) 'R0EXP taken from GLOBAL_PARAM%TOROID_FIELD%R0 = ',R0EXP
  else
    if (EQCHEASE_IN(1)%EQGEOMETRY%GEOM_AXIS%R .GT. -1e+40_rkind) THEN
      R0EXP = EQCHEASE_IN(1)%EQGEOMETRY%GEOM_AXIS%R
      IF (NVERBOSE .GE. 3) write(*,*) 'since global_param/toroid_field/r0 not filled in, R0EXP taken from ', &
        & 'EQGEOMETRY%GEOM_AXIS%R = ',R0EXP
    ELSE
      ! use mean of plasma boundary extrema
      if (associated(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R)) then
        R0EXP = 0.5_rkind*(maxval(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R) + minval(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R))
      IF (NVERBOSE .GE. 3) write(*,*) 'since global_param/toroid_field/r0 and EQGEOMETRY%GEOM_AXIS%R not filled in, ', &
        & 'R0EXP taken from (min+max(EQGEOMETRY%BOUNDARY(1)%R))/2 = ',R0EXP
      else
        print *,'R0EXP could not be determined, should not go through here, check with olivier.sauter@epfl.ch'
      end if
    end if
  end if
  if (EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%B0 .GT. -1e+40_rkind) THEN
    if (EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%R0 .GT. -1e+40_rkind) then
      ! Assume R0EXP from GLOBAL_PARAM%TOROID_FIELD%R0
      B0EXP = EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%B0
      IF (NVERBOSE .GE. 3) write(*,*) 'B0EXP taken from GLOBAL_PARAM%TOROID_FIELD%B0 = ',B0EXP
    else
      ! B0 given but not R0 in GLOBAL_PARAM%TOROID_FIELD, so take B0 and assume R0EXP provided consistently, write an NVERBOSE=0 message
      B0EXP = EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%B0
      IF (NVERBOSE .GE. 0) write(0,*) 'WARNING: EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%B0 .GT. -1e+40_rkind is provided', &
        & ' but EQCHEASE_IN(1)%GLOBAL_PARAM%TOROID_FIELD%R0 is not, so do not know about B0EXP*R0EXP accuracy'
    endif
  else
    IF (NVERBOSE .GE. 0) write(0,*) 'B0 not given in GLOBAL_PARAM%TOROID_FIELD%B0, take namelist value'
  endif
  if (EQCHEASE_IN(1)%GLOBAL_PARAM%MAG_AXIS%POSITION%R .GT. -1e+40_rkind) THEN
    ZRMAGXP = EQCHEASE_IN(1)%GLOBAL_PARAM%MAG_AXIS%POSITION%R
  else
    ZRMAGXP = R0EXP
  endif
  if (EQCHEASE_IN(1)%GLOBAL_PARAM%MAG_AXIS%POSITION%Z .GT. -1e+40_rkind) THEN
    ZZMAGXP = EQCHEASE_IN(1)%GLOBAL_PARAM%MAG_AXIS%POSITION%Z
  else
    ZZMAGXP = 0._rkind
  endif
  ! SET MESH AXIS TO MAGNETIC AXIS
  R0 = ZRMAGXP / R0EXP
  RZ0 = ZZMAGXP / R0EXP
  SIGNB0XP_IN = SIGN(RC1P,B0EXP)
  B0EXP = ABS(B0EXP)
  !
  if (associated(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R) .AND. &
       & associated(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%Z)) then
    NBPS = SIZE(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R)
    IF (NBPS .GT. NPBPS) THEN
      PRINT *,'NBPS= ',NBPS,' LARGER THAN NPBPS=',NPBPS,'.RECOMPILE WITH LARGER NPBPS'
      return
    ENDIF
    RRBPS(1:NBPS) = EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R(1:NBPS)
    RZBPS(1:NBPS) = EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%Z(1:NBPS)
  else
    print *,'EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R  or EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%Z not associated'
    print *,'EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%NPOINTS= ',size(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R)
    print *,'EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R= ',EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R
    print *,'EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%Z= ',EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%Z
    print *,'SIZE(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R)= ',SIZE(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R)
    NBPS=size(EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R)
    RRBPS(1:NBPS) = EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%R(1:NBPS)
    RZBPS(1:NBPS) = EQCHEASE_IN(1)%EQGEOMETRY%BOUNDARY(1)%Z(1:NBPS)
    stop 'not associated'
  end if
  !
  IF (TENSBND .NE. 0._RKIND) THEN
    IDUM = NBPS
    call bndfit(RRBPS,RZBPS,NBPS,ZTEMP,ZTEMPP,IDUM,TENSBND)
    ! NBPS = IDUM
    RRBPS = ZTEMP(1:NBPS)
    RZBPS = ZTEMPP(1:NBPS)
  END IF
  !
  ZRGEOM = (MINVAL(RRBPS(1:NBPS)) + MAXVAL(RRBPS(1:NBPS)))/2._RKIND
  ZZGEOM = (MINVAL(RZBPS(1:NBPS)) + MAXVAL(RZBPS(1:NBPS)))/2._RKIND
  RZ0C = ZZGEOM
  ASPCT = (MAXVAL(RRBPS(1:NBPS)) - MINVAL(RRBPS(1:NBPS))) / 2._RKIND / ZRGEOM
  !
  ! Prepare for input transformation according to cocos paper, sec. IV, using COCOS_IN
  ! (need isigma_RphiZ_out in case wants to follow signs of Ip and B0 from cocos_in-_out transformation (setting signs=-9 in namelist)
  CALL COCOS(COCOS_OUT,iexp_Bp,isigma_Bp,isigma_RphiZ_out,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)
  CALL COCOS(COCOS_IN,iexp_Bp,isigma_Bp,isigma_RphiZ,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)  
  !
  ! Need sign of Ip to check equilibrium consistency
  if (EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA .GT. -1e+40_rkind)  THEN
    CURRT = EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA
  ELSE
    ! Ip not given, use jphi or jpar if present
    if (nsttp .eq. 2) then
      if ( associated(eqchease_in(1)%profiles_1d%jphi) ) then
        if (eqchease_in(1)%profiles_1d%jphi(1) .GT. -1e+40_rkind) &
          & CURRT = abs(CURRT)*sign(RC1P,eqchease_in(1)%profiles_1d%jphi(1))
      elseif ( associated(eqchease_in(1)%profiles_1d%jparallel) ) then
        if (eqchease_in(1)%profiles_1d%jparallel(1) .GT. -1e+40_rkind) &
          & CURRT = abs(CURRT)*sign(RC1P,eqchease_in(1)%profiles_1d%jparallel(1))
      end if
    else
      if ( associated(eqchease_in(1)%profiles_1d%jparallel) ) then
        if (eqchease_in(1)%profiles_1d%jparallel(1) .GT. -1e+40_rkind) &
          & CURRT = abs(CURRT)*sign(RC1P,eqchease_in(1)%profiles_1d%jparallel(1))
      elseif ( associated(eqchease_in(1)%profiles_1d%jphi) ) then
        if (eqchease_in(1)%profiles_1d%jphi(1) .GT. -1e+40_rkind) &
          & CURRT = abs(CURRT)*sign(RC1P,eqchease_in(1)%profiles_1d%jphi(1))
      end if
    END if
  end if
  ! if sign(currt) could not be extracted, leave it as positive by default
  !
  SIGNIPXP_IN = SIGN(RC1P,CURRT)
  CURRT = ABS(CURRT)
  !
  ! If set to -9 in namelist, use input with input cocos as signs, otherwise force to namelist values
  IF (SIGNIPXP < -1) SIGNIPXP = SIGNIPXP_IN * isigma_RphiZ * isigma_RphiZ_out
  IF (SIGNB0XP < -1) SIGNB0XP = SIGNB0XP_IN * isigma_RphiZ * isigma_RphiZ_out
  IF (NVERBOSE .GE. 3) THEN
    write(*,*) 'SIGNIPXP = ',SIGNIPXP,' SIGNIPXP_IN = ',SIGNIPXP_IN
    write(*,*) 'SIGNB0XP = ',SIGNB0XP,' SIGNB0XP_IN = ',SIGNB0XP_IN
  END IF
  !
  if ( associated(eqchease_in(1)%profiles_1d%q) ) then
    QPSIIN(1:size(eqchease_in(1)%profiles_1d%q)) = SIGNIPXP_IN * SIGNB0XP_IN * isigma_rhothetaphi * &
         & eqchease_in(1)%profiles_1d%q(1:size(eqchease_in(1)%profiles_1d%q))
    IF (NVERBOSE .GE. 1) THEN
      PRINT *,' Q(0)= ', QPSIIN(1)
      PRINT *,' Q(EDGE)= ',QPSIIN(size(eqchease_in(1)%profiles_1d%q))
    END IF
  else
    QPSIIN(1)=1._rkind
  end if
  !
  !        s mesh or rho_tor mesh
  !
  if ((EQCHEASE_IN(1)%GLOBAL_PARAM%PSI_AX .gt. -1e+40_rkind) .AND. (EQCHEASE_IN(1)%GLOBAL_PARAM%PSI_BOUND .gt. -1e+40_rkind)) THEN
    ZPSIAX = EQCHEASE_IN(1)%GLOBAL_PARAM%PSI_AX
    ZPSIEDGE = EQCHEASE_IN(1)%GLOBAL_PARAM%PSI_BOUND
  else
    ! use 0, 1 with sign according to cocos: sign(dpsi)=sigma_Ip * sigma_Bp (assuming sigma_Ip is ok)
    ZPSIAX = 0.0_rkind
    ZPSIEDGE = sign(RC1P,isigma_Bp*SIGNIPXP_IN)
    IF (NVERBOSE .GE. 1) print *,'psiaxis and psiedge not given, choose from cocos_in convention and Ip sign'
  end if
  IF (NVERBOSE .GE. 1) print *,' ZPSIAX= ',ZPSIAX,' ; ZPSIEDGE= ',ZPSIEDGE
  IF ((ZPSIAX .GT. ZPSIEDGE) .and. (SIGNIPXP_IN*isigma_Bp .GT. 0)) THEN
    print *,'Input EQDSK is not consistent, psi should be increasing with COCOS_IN = ',COCOS_IN
    IF (NVERBOSE .GE. 1) print *,'dp/dpsi should be positive'
!!$    IF (NITMOPT .NE. 22 .AND. NVERBOSE .LE. 1) STOP 'COCOS IN IODISK'
  ELSEIF ((ZPSIAX .LT. ZPSIEDGE) .and. (SIGNIPXP_IN*isigma_Bp .LT. 0)) THEN
    print *,'Input EQDSK is not consistent, psi should be decreasing with COCOS_IN = ',COCOS_IN
    IF (NVERBOSE .GE. 1) print *,'dp/dpsi should be negative'
!!$    IF (NITMOPT .NE. 22 .AND. NVERBOSE .LE. 1) STOP 'COCOS IN IODISK'
  END IF

  ZPSIAX = SIGNIPXP_IN * isigma_Bp / twopi**iexp_Bp * ZPSIAX / R0EXP**2 / B0EXP
  ZPSIEDGE = SIGNIPXP_IN * isigma_Bp / twopi**iexp_Bp * ZPSIEDGE / R0EXP**2 / B0EXP

  IF (nfunrho.EQ.0) THEN
    if ( associated(eqchease_in(1)%profiles_1d%psi) ) then
      NPPF1 = SIZE(eqchease_in(1)%profiles_1d%psi)
      fcsm(1:NPPF1)=SIGNIPXP_IN * isigma_Bp / twopi**iexp_Bp * eqchease_in(1)%profiles_1d%psi(1:NPPF1) / R0EXP**2 / B0EXP
      ZPSIAX = fcsm(1)
      ZPSIEDGE = fcsm(nppf1)
      IF (NVERBOSE .GE. 1) print *,' ZPSIAX_CHEASE= ',ZPSIAX,' ; ZPSIEDGE_CHEASE= ',ZPSIEDGE
    else
      print *,'psi mesh not given, stop'
      IF (NITMOPT .EQ. 22) RETURN
      stop
    end if
    ! s is sqrt(psi_norm)
    DO i=2,NPPF1
      FCSM(I) = SQRT((fcsm(i)-fcsm(1))/(fcsm(nppf1)-fcsm(1)))
    END DO
    fcsm(1)=0._rkind
  ELSEIF ((NFUNRHO.EQ.1) .AND. NRHOMESH.EQ.1) THEN
    ! s-mesh input is rho_tor, so take it from rho_tor
    if ( associated(eqchease_in(1)%profiles_1d%rho_tor) ) then
      NPPF1 = SIZE(eqchease_in(1)%profiles_1d%rho_tor)
      ! nrhomesh=1 is rhotornorm
      fcsm(1:NPPF1) = eqchease_in(1)%profiles_1d%rho_tor(1:NPPF1)
      if (fcsm(NPPF1) .EQ. 0) THEN
        write(6,*) 'fcsm(NPPF1) = 0 from eqchease_in(1)%profiles_1d%rho_tor. Cannot continue'
        flush(6)
        IF (NITMOPT .EQ. 22) RETURN
        stop
      else
        fcsm(1:NPPF1) = fcsm(1:NPPF1) / fcsm(NPPF1)
        fcsm(NPPF1) = 1._rkind
      end if
      IF (NVERBOSE .GE. 3) print *,' fcsm(1)= ',fcsm(1),' ; fcsm(end)= ',fcsm(NPPF1)
    else
      print *,'rho_tor mesh not given, stop'
      IF (NITMOPT .EQ. 22) RETURN
      stop
    end if
  ELSE
    write(6,*) 'Option NFUNRHO= ',NFUNRHO,' and NRHOMESH= ',NRHOMESH,' not yet implemented, ask O. Sauter'
    flush(6)
    IF (NITMOPT .EQ. 22) RETURN
    stop
  END IF
  ! check that there are not 2 points too close  in rho: now 1e-5 from average dx
  zdx=(FCSM(NPPF1)-FCSM(1)) / real(NPPF1,rkind)
  NPPF1_eff = NPPF1
  allocate(FCSM_eff(NPPF1_eff))
  allocate(iFCSM(NPPF1_eff))
  i=1
  iFCSM(i)=1
  FCSM_eff(1)=FCSM(1)
  do j=2,NPPF1_eff
    if (abs(FCSM(j)-FCSM_eff(i)) .gt. 1e-5_rkind*zdx) then
      i=i+1;
      FCSM_eff(i) = FCSM(j)
      iFCSM(i)=j
    end if
  end do
  NPPF1_eff = i
  IF (NPPF1_eff .NE. NPPF1) &
    & WRITE(0,*) 'THERE WERE POINTS WITH SAME X VALUES: NEW NPPF1_eff=',NPPF1_eff,' , NPPF1=',NPPF1
  FCSM(1:NPPF1_eff) = FCSM_eff(1:NPPF1_eff)
  NPPF = NPPF1_eff -1
  !
  ! prepare so that can normalize on the fly the input quantities
  !
  IF (NVERBOSE .GE. 1) PRINT *,' R0EXP, ZRMAGXP, ZPSIAX, ZPSIEDGE, B0EXP, CURRT, COCOS_IN= ', &
    & R0EXP, ZRMAGXP, ZPSIAX, ZPSIEDGE, B0EXP, CURRT, COCOS_IN
  INTMF0 = 0
  IF (NVERBOSE .GE. 1) print *,'R0EXP, B0EXP= ',R0EXP, B0EXP
  !
  !     NORMALIZATION AND SIGNS, USE COCOS PAPER
  !
  ZPSINORM = R0EXP*R0EXP * B0EXP
  !
  ZMU0 = 4.E-07_RKIND * CPI
  CURRT = CURRT*ZMU0/R0EXP/B0EXP
  ZCOFTTP = 1._RKIND / B0EXP
  ZCOFPP = ZMU0 * R0EXP**2 / B0EXP
  ZCOFT = 1._RKIND / R0EXP / B0EXP
  ZCOFP = ZMU0 / B0EXP**2
  ZCOFJ = ZMU0*R0EXP/B0EXP
  IF (NVERBOSE .GE. 1) print *,'ZPSINORM, CURRT, ZCOFTTP, ZCOFPP, ZCOFT, ZCOFP = ', &
    & ZPSINORM, CURRT, ZCOFTTP, ZCOFPP, ZCOFT, ZCOFP
  !
  !     NORMALIZE USING R0EXP AND B0EXP
  !
  !     NOTE: R0 = R0EXP / R0EXP = 1. MAY NOT BE MIDDLE OF PLASMA
  !
  ZCOF = 1._RKIND / R0EXP
  DO I=1,NBPS
    RRBPS(I) = RRBPS(I) * ZCOF
    RZBPS(I) = RZBPS(I) * ZCOF
  END DO
  !
  ! Write equivalent expeq file in order to be able to test outside kepler if there is a problem
  IF (NVERBOSE .GE. 1) THEN
    OPEN(UNIT=NXPQOUT,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='EXPEQ_KEPLER.IN')
    REWIND(NXPQOUT)
    WRITE(NXPQOUT,1303) ASPCT,RZ0C/R0EXP,PREDGE
    WRITE(NXPQOUT,1301) NBPS
    WRITE(NXPQOUT,1302) (RRBPS(L),  RZBPS(L), L=1,NBPS)
    WRITE(NXPQOUT,1304) NPPF1_eff,nppfun
    WRITE(NXPQOUT,1304) nsttp,nrhomesh
    WRITE(NXPQOUT,1303) (FCSM(L), L=1,NPPF1_eff)
  END IF
  !
  ! specific inputs to be used
  !
  if (nppfun .eq. 4) then
    !     P'
    if (associated(eqchease_in(1)%profiles_1d%pprime) ) then
      rppf(1:NPPF1) = SIGNIPXP_IN * isigma_Bp * twopi**iexp_Bp * eqchease_in(1)%profiles_1d%pprime(1:NPPF1) * ZCOFPP
      RPPF(1:NPPF1_eff) = RPPF(iFCSM(1:NPPF1_eff))      
      IF (TENSPROF .NE. 0._RKIND) THEN
        call interpos(FCSM(1:NPPF1_eff),RPPF(1:NPPF1_eff),NPPF1_eff,tension=TENSPROF, &
          & yout=ZTEMP(1:NPPF1_eff),nbc=(/1,2/),ybc=(/RC0P, RPPF(NPPF1_eff)/))
        rppf(1:NPPF1_eff) = ZTEMP(1:NPPF1_eff)
        IF (NVERBOSE .GE. 1) print *,'Added smoothing of RPPF profile with TENSPROF = ',TENSPROF
      END IF
      IF (NVERBOSE .GE. 1) WRITE(NXPQOUT,1303) (rppf(L), L=1,NPPF1_eff)
    else
      write(6,*) 'pprime not associated. stop'
      flush(6)
      pause 10
      IF (NITMOPT .EQ. 22) RETURN
      stop
    end if
  elseif (nppfun .eq. 8) then
    !     P
    if ( associated(eqchease_in(1)%profiles_1d%pressure) ) then
      rppf(1:NPPF1) = eqchease_in(1)%profiles_1d%pressure(1:NPPF1) * ZCOFP
      RPPF(1:NPPF1_eff) = RPPF(iFCSM(1:NPPF1_eff))      
      ! make sure pressure is positive
      rppf(1:NPPF1_eff) = abs(rppf(1:NPPF1_eff))
      PREDGE = RPPF(NPPF1_eff)
    else
      print *,' need p, eqchease_in(1)%profiles_1d%pressure not associated '
      RETURN
    end if
    IF (PREDGE .LT. RC0P) THEN
      IF (NVERBOSE .GE. 1) WRITE(6,'(//,"   WARNING, PREDGE < 0 IN EXPEQ: PREDGE SET", &
        &       " TO 0.0_RKIND",/)')
      PREDGE = 0.0_RKIND
    ENDIF

    IF (NVERBOSE .GE. 1) WRITE(NXPQOUT,1303) (rppf(L), L=1,NPPF1_eff)

    IF (NFUNRHO.EQ.0 .AND. NRHOMESH.EQ.0) THEN
      ! compute dp/dpsi_norm
      IF (TENSPROF .NE. 0._RKIND) THEN
        call interpos(FCSM(1:NPPF1_eff)*FCSM(1:NPPF1_eff),RPPF(1:NPPF1_eff),NPPF1_eff,tension=TENSPROF, &
          & YOUTP=ZTEMPP,nbc=(/2,2/),ybc=(/RPPF(1), RPPF(NPPF1_eff)/))
        IF (NVERBOSE .GE. 1) print *,'Added smoothing of p profile with TENSPROF = ',TENSPROF
      ELSE
        call interpos(FCSM(1:NPPF1_eff)*FCSM(1:NPPF1_eff),RPPF(1:NPPF1_eff),NPPF1_eff,tension=TENSPROF, &
          & YOUTP=ZTEMPP,nbc=(/0,0/),ybc=(/RC0P, RC0P/))
      END IF
      RPPF(1:NPPF1_eff)=ZTEMPP(1:NPPF1_eff) ! RPPF = dp/dpsi_normalized
    ELSEIF (NFUNRHO.EQ.1 .AND. NRHOMESH.EQ.1) THEN
      ! P(RHOTOR) GIVEN, could compute here dp/drhotor but then dp/dpsi(0) can never be non-zero
      ! So has to compute dp/dpsi or dp/dpsi_norm on psi at each interation
      ! RPPF(1:NPPF1) = p(drho_tor_in_norm)
    ELSE
      WRITE(6,*) 'OPTION NOT YET IMPLMENTED: NPPFUN=8 AND NFUNRHO= ',NFUNRHO,' , NRHOMESH= ',NRHOMESH
      FLUSH(6)
      IF (NITMOPT .NE. 22) STOP 'IODISK'
      RETURN
    END IF
    !
  else
    write(6,*) 'should have nppfun=4 or 8 if reading from array inputeqchease_in(1)%profiles_1d...'
    flush(6)
    pause 10
    IF (NITMOPT .EQ. 22) RETURN
    stop
  end if
  if (nsttp .eq. 1) then
    ! ffprime
    !     GG'
    if ( associated(eqchease_in(1)%profiles_1d%ffprime) ) then
      rfun(1:NPPF1) = SIGNIPXP_IN * isigma_Bp * twopi**iexp_Bp * eqchease_in(1)%profiles_1d%ffprime(1:NPPF1) * ZCOFTTP
      RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    else
      print *,'ffprime not associated. stop'
      return
    end if
!!$     !     G to use G(end) to check r0*b0
!!$     IF ( ASSOCIATED(EQCHEASE_IN(1)%PROFILES_1D%F_DIA) ) THEN
!!$        RFUN = EQCHEASE_IN(1)%PROFILES_1D%F_DIA
!!$        PRINT *,'F_DIA ASSOCIATED'
!!$     ELSE
!!$        RFUN(1:NPPF1) = -1
!!$        PRINT *,'F_DIA NOT ASSOCIATED'
!!$     END IF
    !
  elseif (nsttp .eq. 2) then
    !
    !   jphi, so far use Istar defined as jphi I think
    if ( associated(eqchease_in(1)%profiles_1d%jphi) ) then
      ! Cannot do abs(j) because j can change sign near the edge for example
      rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jphi(1:NPPF1) * zcofj
      RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    elseif ( associated(eqchease_in(1)%profiles_1d%jparallel) ) then
      print *,'******************************************************'
      print *,'jphi not associated, but jparallel is, so use it at this stage'
      print *,'******************************************************'
      rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jparallel(1:NPPF1) * zcofj
      RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    else
      print *,'jphi not associated, nor jparallel. stop'
      return
    end if
    !
    if (rfun(1) .lt. 0._rkind) then
      print *,' Istar<0? wrong sign ?'
      print *,'eqchease_in(1)%profiles_1d%jparallel(1) = ',eqchease_in(1)%profiles_1d%jparallel(1)
      print *,'eqchease_in(1)%profiles_1d%jphi(1) = ',eqchease_in(1)%profiles_1d%jparallel(1)
      print *,'EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA= ', EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA
      print *,'use abs(rfun)'
      RFUN(1:NPPF1_eff) = abs(RFUN(1:NPPF1_eff))
    end if
  elseif (nsttp .eq. 3) then
    !
    !   jpar, Ipar not defined as ITM jpar=<j.B>/B0
    ! Ipar=mu0 jpar / (Fdia <1/R**2>) in chease units, jpar, Fdia and <1/R**2> in SI units
    if ( associated(eqchease_in(1)%profiles_1d%jparallel) .AND. associated(eqchease_in(1)%profiles_1d%F_dia) &
         & .AND. associated(eqchease_in(1)%profiles_1d%gm1) ) then
      rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jparallel(1:NPPF1) * B0EXP / R0EXP &
           & / (SIGNB0XP_IN*eqchease_in(1)%profiles_1d%F_dia(1:NPPF1) * eqchease_in(1)%profiles_1d%gm1(1:NPPF1)) * zcofj
      RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    elseif ( associated(eqchease_in(1)%profiles_1d%jparallel) .AND. associated(eqchease_in(1)%profiles_1d%F_dia) ) then
      ! gm1 not defined use 1/R0EXP^2 
      rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jparallel(1:NPPF1) * B0EXP / R0EXP &
           & / (SIGNB0XP_IN*eqchease_in(1)%profiles_1d%F_dia(1:NPPF1) * (RC1P/R0EXP/R0EXP)) * zcofj
      RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    elseif ( associated(eqchease_in(1)%profiles_1d%jparallel) ) then
      ! gm1 not defined use 1/R0EXP^2 and F_dia not defined, use R0EXP*B0EXP
      rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jparallel(1:NPPF1) * B0EXP / R0EXP &
           & / (R0EXP*B0EXP * (RC1P/R0EXP/R0EXP)) * zcofj
      RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    else
      print *,'jpar or F_dia or gm1 not associated use jphi'
      if ( associated(eqchease_in(1)%profiles_1d%jphi) ) then
        rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jphi(1:NPPF1) * zcofj
        RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
      else
        print *,'jphi not associated. stop'
        return
      end if
    end if
    !
    if (rfun(1) .lt. 0._rkind) then
      print *,' Istar<0? wrong sign ?'
      print *,'eqchease_in(1)%profiles_1d%jparallel(1) = ',eqchease_in(1)%profiles_1d%jparallel(1)
      print *,'eqchease_in(1)%profiles_1d%jphi(1) = ',eqchease_in(1)%profiles_1d%jparallel(1)
      print *,'EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA= ', EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA
      print *,'eqchease_in(1)%profiles_1d%f_dia(1) = ',eqchease_in(1)%profiles_1d%f_dia(1)
      print *,'use abs(rfun)'
      RFUN(1:NPPF1_eff) = abs(RFUN(1:NPPF1_eff))
    end if
    !
  elseif (nsttp .eq. 4) then
    !
    !   jpar defined as ITM jpar=< mu0 j.B>/B0 = mu0 R0/B0 [jphys]
    if ( associated(eqchease_in(1)%profiles_1d%jparallel) ) then
      rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jparallel(1:NPPF1) *  zcofj
      RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
    else
      print *,'jpar not associated use jphi'
      if ( associated(eqchease_in(1)%profiles_1d%jphi) ) then
        rfun(1:NPPF1) = SIGNIPXP_IN * eqchease_in(1)%profiles_1d%jphi(1:NPPF1) * zcofj
        RFUN(1:NPPF1_eff) = RFUN(iFCSM(1:NPPF1_eff))
      else
        print *,'jphi not associated. stop'
        if (NITMOPT .EQ. 22) return
        stop 'no j associated'
      end if
    end if
    !
    if (rfun(1) .lt. 0._rkind) then
      print *,' Istar<0? wrong sign ?'
      print *,'eqchease_in(1)%profiles_1d%jparallel(1) = ',eqchease_in(1)%profiles_1d%jparallel(1)
      print *,'eqchease_in(1)%profiles_1d%jphi(1) = ',eqchease_in(1)%profiles_1d%jparallel(1)
      print *,'EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA= ', EQCHEASE_IN(1)%GLOBAL_PARAM%I_PLASMA
      print *,'eqchease_in(1)%profiles_1d%f_dia(1) = ',eqchease_in(1)%profiles_1d%f_dia(1)
      print *,'use abs(rfun)'
      RFUN(1:NPPF1_eff) = abs(RFUN(1:NPPF1_eff))
    end if
    !
  else
    write(0,*) ' case nsttp= ',nsttp,' not defined yet'
    return
    !
  end if
  !
  IF (TENSPROF .NE. 0._RKIND) THEN
    call interpos(FCSM(1:NPPF1_eff),RFUN(1:NPPF1_eff),NPPF1_eff,tension=TENSPROF, &
      & yout=ZTEMP(1:NPPF1_eff),nbc=(/1,2/),ybc=(/RC0P, RFUN(NPPF1_eff)/))
    rfun(1:NPPF1_eff) = ZTEMP(1:NPPF1_eff)
    IF (NVERBOSE .GE. 1) print *,'Added smoothing of RFUNprofile with TENSPROF = ',TENSPROF
  END IF
  IF (NVERBOSE .GE. 1) WRITE(NXPQOUT,1303) (rfun(L), L=1,NPPF1_eff)
  !
  ! Now all input profiles have been fetched, can replace NPPF1 by NPPF1_eff
  NPPF1 = NPPF1_eff
  !
!!$         ZR=eqchease_in(1)%profiles_2d(1)%grid%dim1
!!$    ZZ=eqchease_in(1)%profiles_2d(1)%grid%dim2
!!$!
!!$         RBOXLFT=ZR(1)
!!$    RBOXLEN=ZR(inrbox)-ZR(1)
!!$    ZBOXLEN=ZZ(inzbox)-ZZ(1)
  !
  !        AUXILIARY PARAMETERS
  !        (COMMENT NEXT TWO LINES IF WANT TO CHANGE QSPEC IN NAMELIST)
  IF (NVERBOSE .GE. 3) THEN
    print *,' QSPEC= ',QSPEC
    print *,' QPSIIN= ',QPSIIN(1:nppf1)
  END IF
  IF ((QSPEC .GT. RC0P) .and. (abs(QPSIIN(1)) .GT. RC0P)) THEN
    QSPEC = abs(QPSIIN(1))
    CSSPEC = 0.0_RKIND
  ELSE
    QSPEC = ABS(QSPEC)
  ENDIF
  IF (NVERBOSE .GE. 3) THEN
    print *,' QSPEC= ',QSPEC
    print *,' NCSCAL= ',NCSCAL
  END IF
  IF (NVERBOSE .GE. 1) THEN
    WRITE(NXPQOUT,'(I5,A)') NCSCAL,' = NCSCAL'
    WRITE(NXPQOUT,'(1PE17.7,A)') R0EXP,' = R0EXP'
    WRITE(NXPQOUT,'(1PE17.7,A)') B0EXP,' = B0EXP'
    WRITE(NXPQOUT,'(1PE17.7,A)') QSPEC,' = QSPEC'
    WRITE(NXPQOUT,'(1PE17.7,A)') CSSPEC,' = CSSPEC'
    WRITE(NXPQOUT,'(1PE17.7,A)') PREDGE,' = PREDGE'
    CLOSE(NXPQOUT)
  END IF
  !
  !        FIND PLASMA LIMITS
  !
  IRMAX = ISMAX(NBPS,RRBPS,1)
  IRMIN = ISMIN(NBPS,RRBPS,1)
  IZMAX = ISMAX(NBPS,RZBPS,1)
  IZMIN = ISMIN(NBPS,RZBPS,1)
  RZ0 = 0.5_RKIND * (RZBPS(IZMIN) + RZBPS(IZMAX))
  ASPCT = (RRBPS(IRMAX) - RRBPS(IRMIN)) &
       &     / (RRBPS(IRMAX) + RRBPS(IRMIN))
  IF (NVERBOSE .GE. 1) WRITE(6,'(/,3(A,1PE13.4),/)') ' FROM euitm: ,R0GEOM= ', &
    &  0.5_RKIND * (RRBPS(IRMIN) + RRBPS(IRMAX)), &
    &  ' Z0GEOM= ',RZ0,' INV. ASPECT RATIO= ',ASPCT
  !
39 CONTINUE
  !
  CLOSE(UNIT=NXIN,STATUS='KEEP')
  !
  RETURN
  !
  !----------------------------------------------------------------------
  !  40.   SAVE QUANTITIES FOR PENN CODE ON "PENN", NIDEAL=4
  !        ALSO IN OUTPEN
40 CONTINUE
  !
  OPEN(UNIT=NPENN,ACCESS='SEQUENTIAL',FORM='UNFORMATTED', &
       &        FILE='NPENN')
  REWIND NPENN
  !
  IDCHSE = 222
  IDATA  = 13
  !
  IF (NVERBOSE .GE. 1) THEN
    WRITE(NPENN) IDCHSE
    WRITE(NPENN) IDATA
  END IF
  !
  RETURN
  !
  !----------------------------------------------------------------------
  !  5. SAVE QUANTITIES FOR XTOR INTO "OUTXTOR", NIDEAL = 5
  !
50 CONTINUE
  !
  OPEN(NXTOR,FILE='OUTXTOR',FORM='UNFORMATTED')
  REWIND NXTOR
  !
  WRITE(NXTOR) 2*NPSI, NTNOVA
  WRITE(NXTOR) RC / ASPCT, RMAG, RZMAG
  !
  IF (NOUTXTOR==0) RETURN
  !
  WRITE(NXTOR) NPOPULATIONS
  !
  IF (NPOPULATIONS==0) RETURN
  !
  WRITE(NXTOR) MDT,NCHI
  !
  RETURN
  !
  !----------------------------------------------------------------------
  !  6.   WRITE EQUIL. IN "EXPERIMENTAL" FORMAT ON EXPEQ.OUT
  !       WRITE EXPEQ.OUT.TOR WITH NORMALIZED TOROIDAL RHO INSTEAD OF POLOIDAL RHO
  !
60 CONTINUE
  !
  OPEN(UNIT=NXPQOUT,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='EXPEQ.OUT')
  REWIND(NXPQOUT)
  OPEN(UNIT=NXPQTOR,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='EXPEQ.OUT.TOR')
  REWIND(NXPQTOR)
  !
  WRITE(NXPQOUT,1303) ASPCT,RZ0,PREDGE
  WRITE(NXPQTOR,1303) ASPCT,RZ0,PREDGE
  !
  IF (NSURF .EQ. 7) THEN
    WRITE(NXPQOUT,1301) NFOURPB
    WRITE(NXPQOUT,1302) ALZERO, RC
    WRITE(NXPQOUT,1302) (BPSCOS(L),BPSSIN(L),L=1,NFOURPB)
    WRITE(NXPQTOR,1301) NFOURPB
    WRITE(NXPQTOR,1302) ALZERO, RC
    WRITE(NXPQTOR,1302) (BPSCOS(L),BPSSIN(L),L=1,NFOURPB)
  ELSE
    WRITE(NXPQOUT,1301) NBPSOUT
    WRITE(NXPQOUT,1302) (RRBPSOU(L),  RZBPSOU(L), L=1,NBPSOUT)
    WRITE(NXPQTOR,1301) NBPSOUT
    WRITE(NXPQTOR,1302) (RRBPSOU(L),  RZBPSOU(L), L=1,NBPSOUT)
  END IF
  !
  ! Profiles in EXPEQ.OUT, etc files depend on NPROPT given in input with:
  ! NSTTP_out = abs(NPROPT)
  ! NPPFUN_out = 4 if NPROPT>0 and 8 if NPROPT<0
  !
  ipropt_abs = abs(NPROPT)
  I0 = 0
  I1 = 1
  if (NPROPT .GT. 0) then
    Ippfun = 4
  else
    Ippfun = 8
  end if
  WRITE(NXPQOUT,1304) NISO1EFF+1, Ippfun
  WRITE(NXPQOUT,1304) ipropt_abs,I0
  WRITE(NXPQOUT,1303) (SMISOP1(L), L=1,NISO1EFF+1)
  WRITE(NXPQTOR,1304) NISO1EFF+1, Ippfun
  WRITE(NXPQTOR,1304) ipropt_abs,I1
  WRITE(NXPQTOR,1303) ALZERO,(CSMTOR(L), L=1,NISO1EFF)
  !
  if (Ippfun .EQ. 4) then
    ! pprime
    WRITE(NXPQOUT,1303) DPDP0,(CPPR(L),L=1,NISO1EFF)
    WRITE(NXPQTOR,1303) DPDP0,(CPPR(L),L=1,NISO1EFF)
  elseif (Ippfun .EQ. 8) then
    ! pressure
    WRITE(NXPQOUT,1303) CP0,(CPR(L),L=1,NISO1EFF)
    WRITE(NXPQTOR,1303) CP0,(CPR(L),L=1,NISO1EFF)
  else
    write(6,*) 'error with Ippfun = ',Ippfun
    flush(6)
    if (nitmopt .EQ. 22) return
    stop 'Ippfun'
  end if
  !
  IF (ipropt_abs .EQ. 1) THEN
    ! TTprime
    WRITE(NXPQOUT,1303) DTTP0,(TTP(L),L=1,NISO1EFF)
    WRITE(NXPQTOR,1303) DTTP0,(TTP(L),L=1,NISO1EFF)
  ELSE IF (ipropt_abs .EQ. 2) THEN
    ! I-star
    WRITE(NXPQOUT,1303) RIPR0,(RIPR(L),L=1,NISO1EFF)
    WRITE(NXPQTOR,1303) RIPR0,(RIPR(L),L=1,NISO1EFF)
  ELSE IF (ipropt_abs .EQ. 3) THEN
    ! I-parallel
    WRITE(NXPQOUT,1303) RJDTB0,(RJDOTB(L),L=1,NISO1EFF)
    WRITE(NXPQTOR,1303) RJDTB0,(RJDOTB(L),L=1,NISO1EFF)
  ELSE IF (ipropt_abs .EQ. 4) THEN
    ! j-parallel = <j.B>/B0 = I-parallel * T_chease * <1/R**2>_chease
    WRITE(NXPQOUT,1303) (eqchease_out(1)%profiles_1d%jparallel(L),L=1,NISO1EFF1)
    WRITE(NXPQTOR,1303) (eqchease_out(1)%profiles_1d%jparallel(L),L=1,NISO1EFF1)
  ENDIF
  !
  !L       ADD, IN MKSA, EXTRA VALUES OBTAINED BY CHEASE FOR EASIER COMPARISON
  !        (NO HEADER)
  !
  CALL OUTMKSA(NXPQOUT,1)
  CALL OUTMKSA(NXPQTOR,1)
  !
  CLOSE(UNIT=NXPQOUT,STATUS='KEEP')
  CLOSE(UNIT=NXPQTOR,STATUS='KEEP')
  !
  RETURN
  !
  !----------------------------------------------------------------------
  !        7. SAVE QUANTITIES ON EQDSK IN MKSA USING R0EXP AND B0EXP
  !
70 CONTINUE
  !
  ! WRITE CHEASE STANDARD EQDSK.OUT THUS COCOS=2 and Ip, B0 positive
  OPEN(NUEQDSK,FILE='EQDSK_COCOS_02_POS.OUT',FORM='FORMATTED')
  REWIND(NUEQDSK)
  ! Write also requested COCOS_OUT EQDSK
  WRITE(FILECOCOS,'(A,I2.2,A)') 'EQDSK_COCOS_', COCOS_OUT,'.OUT'
  OPEN(NUEQDSKCOCOS,FILE=FILECOCOS,FORM='FORMATTED')
  REWIND(NUEQDSKCOCOS)
  !
  !  CHEASE NEEDS IP>0 (PSI_AXIS AT MINIMUM) AND B0=1>0 FROM NORMALIZATION
  !  IN ANY CASE, EQUILIBRIUM DEPENDS ON DP/DPSI, D(T^2)/DPSI AND DELTA*(PSI),
  !  SO IT DOES NOT DEPEND ON SIGN OF PSI (THUS OF IP) NOR OF T (THUS OF B)
  !  HOWEVER, FURTHER USE OF EQUILIBRIUM LIKE WAVE-PARTICLE INTERACTION MIGHT
  !  REQUIRE  THE KNOWLEDGE OF THE SIGN OF IP AND B0
  !
  !  THUS, SAVE EQDSK FILE AS BEFORE (COCOS=2 AS CHEASE) WITH IP AND B0 POSITIVE
  !  ALSO SAVE FILECOCOS FILE USING SIGNIPXP (FOR SIGN OF PSI AND THUS D/DPSI)
  !  AND SIGNB0XP FOR SIGN OF T.
  !  NOTE: SINCE Q~DPHI/DPSI, TOROIDAL VS POLOIDAL FLUXES, SIGN(Q)=SIGNB0XP*SIGNIPXP*SIGN_rhothetaphi
  !        CURRT AND B0EXP ARE SUPPOSED TO BE POSITIVE. THE INFORMATION ABOUT THEIR SIGNS
  !                        SHOULD ONLY BE IN SIGNIPXP AND SIGNB0XP
  !
  ! USE TRANSFORMATIONS AS GIVEN IN SEC. III OF COCOS PAPER (Sauter and Medvedev in chease/trunk)
  !
  IF (B0EXP .LT. 0._RKIND) THEN
    PRINT *,'B0EXP SHOULD BE POSITIVE. USE SIGNB0XP FOR SIGN OF B0EXP'
    PRINT *,'STOP IN IODISK FOR EQDSK OUT'
    IF (NITMOPT .EQ. 22) RETURN
    STOP 'IODISK'
  ENDIF
  IF (CURRT .LT. 0._RKIND) THEN
    PRINT *,'CURRT SHOULD BE POSITIVE. USE SIGNIPXP FOR SIGN OF IP'
    PRINT *,'STOP IN IODISK FOR EQDSK OUT'
    IF (NITMOPT .EQ. 22) RETURN
    STOP 'IODISK'
  ENDIF
  !
  ! get relative signs for COCOS_OUT
  !
  CALL COCOS(COCOS_OUT,iexp_Bp,isigma_Bp,isigma_RphiZ,isigma_rhothetaphi,isign_q_pos,isign_pprime_pos)
  !
  !        FIRST LINE: 48 CHAR, DUMMY INTEGER, NRBOX, NZBOX
  !
  CALL DATE_AND_TIME(ZDATE)
  IDUM = 3
  ZSHIFTZ = RZMAG - RZMGEQD
  WRITE(NUEQDSK,9380)    'FROM CHEASE,SI UNITS,COCOS=2, IP,B0>0', &
       &       ZDATE,IDUM,NRBOX,NZBOX
  WRITE(NUEQDSKCOCOS,'(A28,I2.2,A10,A8,3I4)') 'FROM CHEASE BUT COCOS=',COCOS_OUT,',SI UNITS' &
       & ,ZDATE,IDUM,NRBOX,NZBOX
  !
  !        2ND LINE: RBOXLEN, ZBOXLEN, R0, RBOXLFT, ZBOXMID
  !
  WRITE(NUEQDSK,9381) RBOXLEN,ZBOXLEN,R0EXP,RBOXLFT,ZBOXMID
  WRITE(NUEQDSKCOCOS,9381) RBOXLEN,ZBOXLEN,R0EXP,RBOXLFT,ZBOXMID
  !
  !        3RD LINE: RMAG, ZMAG, PSIMAG, PSIEDGE, B0
  !
  !     ZMAG GIVEN BY RZMGEQD, COMPUTED IN PSIBOX AND DEPENDING ON NEQDZMG
  !
  zpsi_signfactor = SIGNIPXP * isigma_Bp * twopi**iexp_Bp
  ZPSIAX = SPSIM * R0EXP**2 * B0EXP
  WRITE(NUEQDSK,9381) RMAG*R0EXP,RZMGEQD*R0EXP, &
       &       ZPSIAX,RC0P,B0EXP
  WRITE(NUEQDSKCOCOS,9381) RMAG*R0EXP,RZMGEQD*R0EXP, &
       &       zpsi_signfactor*ZPSIAX,RC0P,SIGNB0XP*B0EXP
  !
  !        4TH LINE: PLASMA CURRENT, PSIAX1, PSIAX2, RAXIS1, RAXIS2
  !
  ZMU0 = 4.E-07_RKIND * CPI
  IF (R0EXP.EQ.1 .AND. B0EXP.EQ.1) THEN
    PRINT *,'R0EXP=1 AND B0EXP=1. This used to imply mu0=1, but can be misleading on reading the EQDSK file'
    PRINT *,'THUS ZMU0 = 1.0_RKIND IS NOT SET ANYMORE'
    ! ZMU0 = 1.0_RKIND
  END IF
  WRITE(NUEQDSK,9381) RITOT*R0EXP*B0EXP/ZMU0, &
       &       ZPSIAX,RC0P,RMAG*R0EXP,RC0P
  WRITE(NUEQDSKCOCOS,9381) SIGNIPXP*RITOT*R0EXP*B0EXP/ZMU0, &
       &       zpsi_signfactor*ZPSIAX,RC0P,RMAG*R0EXP,RC0P
  !
  !        5TH LINE: ZAXIS1, ZAXIS2, PSI_SEP, R_XPOINT, Z_XPOINT
  !
  WRITE(NUEQDSK,9381) RZMGEQD*R0EXP,RC0P,RC0P,RC0P,RC0P
  WRITE(NUEQDSKCOCOS,9381) RZMGEQD*R0EXP,RC0P,RC0P,RC0P,RC0P
  !
  !     EQUISTANT PSI-MESH FOR PROFILES IN S=SQRT(1.-PSI/PSIMIN)
  !
  ZCOF = ABS(SPSIM) / REAL(NRBOX-1,RKIND)
  ALLOCATE(ZSTEMP(NRBOX))
  DO I=1,NRBOX
    ZSTEMP(I) = SPSIM + REAL(I-1,RKIND)*ZCOF
  ENDDO
  !
  !        6TH ENTRY: T(PSI) (OR G)
  !
  CALL SPLINE(NISO1EFF,SMISO,TMF,ZD2TMP,ZWORK)
  CALL PPSPLN(NRBOX,ZSTEMP,NISO1EFF-1,SMISO,TMF,ZD2TMP,ZTEMP,ZDUM,0)
  ZTEMP(1) = T0
  ZCOF = R0EXP*B0EXP
  WRITE(NUEQDSK,9381) (ZTEMP(I)*ZCOF,I=1,NRBOX)
  WRITE(NUEQDSKCOCOS,9381) (SIGNB0XP*ZTEMP(I)*ZCOF,I=1,NRBOX)
  !
  !        7TH ENTRY: PRESSURE
  !
  CALL SPLINE(NISO1EFF,SMISO,CPR,ZD2TMP,ZWORK)
  CALL PPSPLN(NRBOX,ZSTEMP,NISO1EFF-1,SMISO,CPR,ZD2TMP,ZTEMP,ZDUM,0)
  ZTEMP(1) = CP0
  ZCOF = B0EXP*B0EXP / ZMU0
  WRITE(NUEQDSK,9381) (ZTEMP(I)*ZCOF,I=1,NRBOX)
  WRITE(NUEQDSKCOCOS,9381) (ZTEMP(I)*ZCOF,I=1,NRBOX)
  !
  !        8TH ENTRY: TT' (OR GG')
  !
  CALL SPLINE(NISO1EFF,SMISO,TTP,ZD2TMP,ZWORK)
  CALL PPSPLN(NRBOX,ZSTEMP,NISO1EFF-1,SMISO,TTP,ZD2TMP,ZTEMP,ZDUM,0)
  ZTEMP(1) = DTTP0
  ZCOF = B0EXP
  WRITE(NUEQDSK,9381) (ZTEMP(I)*ZCOF,I=1,NRBOX)
  WRITE(NUEQDSKCOCOS,9381) (SIGNIPXP * isigma_Bp / twopi**iexp_Bp * ZTEMP(I)*ZCOF,I=1,NRBOX)
  !
  !        9TH ENTRY: P'
  !
  CALL SPLINE(NISO1EFF,SMISO,CPPR,ZD2TMP,ZWORK)
  CALL PPSPLN(NRBOX,ZSTEMP,NISO1EFF-1,SMISO,CPPR,ZD2TMP,ZTEMP,ZDUM,0)
  ZTEMP(1) = DPDP0
  ZCOF = B0EXP / ZMU0 / R0EXP / R0EXP
  WRITE(NUEQDSK,9381) (ZTEMP(I)*ZCOF,I=1,NRBOX)
  WRITE(NUEQDSKCOCOS,9381) (SIGNIPXP * isigma_Bp / twopi**iexp_Bp * ZTEMP(I)*ZCOF,I=1,NRBOX)
  !
  !        10TH ENTRY: PSI(I,J)
  !
  ZCOF = R0EXP**2 * B0EXP
  WRITE(NUEQDSK,9381) ((EQDSPSI(I,J)*ZCOF,I=1,NRBOX),J=1,NZBOX)
  WRITE(NUEQDSKCOCOS,9381) ((zpsi_signfactor*EQDSPSI(I,J)*ZCOF,I=1,NRBOX), &
       &                          J=1,NZBOX)
  !
  !        11TH ENTRY: Q PROFILE
  !
  CALL SPLINE(NISO1EFF,SMISO,QPSI,ZD2TMP,ZWORK)
  CALL PPSPLN(NRBOX,ZSTEMP,NISO1EFF-1,SMISO,QPSI,ZD2TMP,ZTEMP,ZDUM,0)
  ZTEMP(1) = Q0
  WRITE(NUEQDSK,9381) (ZTEMP(I),I=1,NRBOX)
  WRITE(NUEQDSKCOCOS,9381) (SIGNIPXP * SIGNB0XP * isigma_rhothetaphi * ZTEMP(I),I=1,NRBOX)
  !
  !        12TH ENTRY: (R,Z) OF PLASMA BOUNDARY AND LIMITER POSITION
  !
  !     LIMITER ARRAY IF NOT DEFINED (FROM EQDSK OR AS DEFAULT)
  IF (NWALLPOS .LE. 0) THEN
    NWALLPOS = 5
    ! NOTE: FOR TORAY WALL NEEDS TO BE INSIDE RMESH,ZMESH, SO SHIFT BY ZDUM TO MAKE SURE
    ZDUM = 1.0E-05_RKIND
    WALLPOSR(1) = RBOXLFT * (1._RKIND + ZDUM)
    WALLPOSZ(1) = ZBOXMID - ZBOXLEN/2._RKIND*(1._RKIND-ZDUM)
    WALLPOSR(2) = RBOXLFT + RBOXLEN*(1._RKIND-ZDUM)
    WALLPOSZ(2) = WALLPOSZ(1)
    WALLPOSR(3) = WALLPOSR(2)
    WALLPOSZ(3) = ZBOXMID + ZBOXLEN/2._RKIND*(1._RKIND-ZDUM)
    WALLPOSR(4) = WALLPOSR(1)
    WALLPOSZ(4) = WALLPOSZ(3)
    WALLPOSR(5) = WALLPOSR(1)
    WALLPOSZ(5) = WALLPOSZ(1)
  ENDIF
  WRITE(NUEQDSK,1204) NBPSOUT, NWALLPOS
  WRITE(NUEQDSKCOCOS,1204) NBPSOUT, NWALLPOS
  ZCOF = R0EXP
  print *,'(RZMAG-RZMGEQD)*ZCOF= ',(RZMAG-RZMGEQD)*ZCOF
  WRITE(NUEQDSK,9381) (RRBPSOU(I)*ZCOF, &
       &     (RZBPSOU(I)-ZSHIFTZ)*ZCOF,I=1,NBPSOUT)
  WRITE(NUEQDSKCOCOS,9381) (RRBPSOU(I)*ZCOF, &
       &     (RZBPSOU(I)-ZSHIFTZ)*ZCOF,I=1,NBPSOUT)
  WRITE(NUEQDSK,9381) (WALLPOSR(I),WALLPOSZ(I)-ZSHIFTZ*ZCOF,I=1,NWALLPOS)
  WRITE(NUEQDSKCOCOS,9381) (WALLPOSR(I),WALLPOSZ(I)-ZSHIFTZ*ZCOF,I=1,NWALLPOS)
  !
  !        LAST LINES: SOME EQUILIBRIUM VALUES
  !        (NO HEADER)
  !
  CALL OUTMKSA(NUEQDSK,1)
  CALL OUTMKSA(NUEQDSKCOCOS,1)
  !
  CLOSE(UNIT=NUEQDSK,STATUS='KEEP')
  DEALLOCATE(ZSTEMP)
  !
  RETURN
  !
  !----------------------------------------------------------------------
  !  8. WRITE EQUILIBRIUM QUANTITIES EQ ON MEQ
  !
80 CONTINUE
  !
  IF (NIDEAL .EQ. 1) THEN
    OPEN(UNIT=MEQ,ACCESS='DIRECT',RECL=8*NDEQ*NCHI,FORM='UNFORMATTED',FILE='MEQ')
    REWIND(MEQ)
    DO  J=1,NISO1EFF1
      WRITE(UNIT=MEQ,REC=J) ((EQ(I,L,J),I=1,NDEQ),L=1,NCHI)
    END DO
  ELSE IF (NIDEAL .EQ. 2) THEN
    OPEN(UNIT=MEQ,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',FILE='MEQ')
    REWIND(MEQ)
    DO  J=1,NISO1EFF1
      WRITE(MEQ) ((EQ(I,L,J),I=1,NDEQ),L=1,NCHI)
    END DO
  ENDIF
  CLOSE(UNIT=MEQ,STATUS='KEEP')
  RETURN
  !
  !----------------------------------------------------------------------
  !  9.1 PLOT QUANTITIES FOR ERATO
  !
90 CONTINUE
  !
  IF (NIDEAL .EQ. 0) THEN
    OPEN(UNIT=NDES,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='NDES')
    REWIND(NDES)
    WRITE(NDES,1015) NISO1EFF1
    WRITE(NDES,1015) NCHI
    WRITE(NDES,1016) R0
    WRITE(NDES,1016) RZ0
  ELSE
    OPEN(UNIT=NDES,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',FILE='NDES')
    REWIND(NDES)
  ENDIF
  !
  DO  J=1,NISO1EFF1
    IF (NIDEAL .NE. 0) THEN
      ! Note LION expects values on CSM(1,NPSI) surfaces
      ! now these are on SMISO thus CS(2:NPSI+1). Should compute back mid-values in LION to be independent on equilibrium
      WRITE(NDES) (CR(I,J),I=1,NCHI)
      WRITE(NDES) (CZ(I,J),I=1,NCHI)
      WRITE(NDES) (CNR1(I,J),I=1,NCHI)
      WRITE(NDES) (CNZ1(I,J),I=1,NCHI)
    ELSE
      WRITE(NDES,1014) (CR(I,J),I=1,NCHI)
      WRITE(NDES,1014) (CZ(I,J),I=1,NCHI)
      WRITE(NDES,1014) (CNR1(I,J),I=1,NCHI)
      WRITE(NDES,1014) (CNZ1(I,J),I=1,NCHI)
      WRITE(NDES,1014) (CNR2(I,J),I=1,NCHI)
      WRITE(NDES,1014) (CNZ2(I,J),I=1,NCHI)
    ENDIF
  END DO
  !
  !  9.2. SURFACE AND LABEL FOR PLOT IN ERATO
  !
  INUM = 12 * NT
  ZDT  = TWOPI / REAL(INUM,RKIND)
  DO  J=1,INUM
    ZT(J) = 0.5_RKIND * REAL(2*J-3,RKIND) * ZDT
  END DO
  !
  CALL BOUND(INUM,ZT,ZBND)
  DO  J=1,INUM
    ZR(J) = R0  + ZBND(J) * COS(ZT(J))
    ZZ(J) = RZ0 + ZBND(J) * SIN(ZT(J))
  END DO
  !
  IF (NIDEAL .NE. 0) THEN
    WRITE(NDES) INUM
    WRITE(NDES) (ZR(J),J=1,INUM),(ZZ(J),J=1,INUM)
    WRITE(NDES) LABEL1,LABEL2
    WRITE(NDES) LABEL2,LABEL3
    WRITE(NDES) LABEL3
    WRITE(NDES) LABEL4
  ELSE
    WRITE(NDES,1015) INUM
    WRITE(NDES,1014) (ZR(J),J=1,INUM),(ZZ(J),J=1,INUM)
  ENDIF
  !
  CLOSE(UNIT=NDES,STATUS='KEEP')
  RETURN
  !
  !----------------------------------------------------------------------
  !  10. VACUM QUANTITIES FOR ERATO
  !
100 CONTINUE
  !
  OPEN(UNIT=NVAC,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',FILE='NVAC')
  REWIND(NVAC)
  WRITE(NVAC) (CR(J,NISO1EFF1),J=1,NCHI),(CZ(J,NISO1EFF1),J=1,NCHI),(CHI(J),J=1,NCHI1)
  WRITE(NVAC) QPSI(NISO1EFF), TMF(NISO1EFF)
  WRITE(NVAC) (CNR1(J,NISO1EFF1),J=1,NCHI),(CNZ1(J,NISO1EFF1),J=1,NCHI)
  !
  !     NOTE: SHOULD BE THE SAME IF STATEMENT AS IN ERDATA, WHICH DECIDES
  !     WHETHER OR NOT THE EQ'S ARE DEFINED
  IF (NDEQ .GE. 25) THEN
    WRITE(NVAC) (EQ(19,J,NISO1EFF1),J=1,NCHI)
    WRITE(NVAC) (EQ(20,J,NISO1EFF1),J=1,NCHI)
  ELSE
    WRITE(6,'(/,"  WARNING IN IODISK: EQ(19) AND EQ(20) NOT", &
         &       " WRITTEN ON NVAC AS NDEQ<25",/)')
  ENDIF
  WRITE(NVAC) (CHIOLD(J),J=1,NCHI1)
  !
  IF (NIDEAL .EQ. 1) RETURN
  !
  WRITE(NVAC) (TETVAC(J),J=1,NCHI1)
  WRITE(NVAC) (RHOVAC(J),J=1,NCHI1)
  WRITE(NVAC) (TETVACM(J),J=1,NCHI1)
  WRITE(NVAC) (RHOVACM(J),J=1,NCHI1)
  WRITE(NVAC) (TETVACI(J,1),J=1,NCHI)
  WRITE(NVAC) (TETVACI(J,2),J=1,NCHI)
  WRITE(NVAC) (TETVACI(J,3),J=1,NCHI)
  WRITE(NVAC) (RHOVACI(J,1),J=1,NCHI)
  WRITE(NVAC) (RHOVACI(J,2),J=1,NCHI)
  WRITE(NVAC) (RHOVACI(J,3),J=1,NCHI)
  WRITE(NVAC) (DRHOPI(J,1),J=1,NCHI)
  WRITE(NVAC) (DRHOPI(J,2),J=1,NCHI)
  WRITE(NVAC) (DRHOPI(J,3),J=1,NCHI)
  !
  CLOSE(UNIT=NVAC,STATUS='KEEP')
  RETURN
  !
  !----------------------------------------------------------------------
  !  11. WRITE QUANTITIES FOR MARS
  !
110 CONTINUE
  !
  OPEN(UNIT=NO,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='EQU01')
  REWIND(NO)
  OPEN(UNIT=NOI,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='EQU02')
  REWIND(NOI)
  !
  WRITE(NO,1001) NPSI1,MSMAX,NSMAX,1._RKIND/ASPCT,Q0
  WRITE(NO,1002) (CS(I),I=1,NPSI1)
  !
  CALL GENOUT(DG11L (1,1),' DG11L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(DG22L (1,1),' DG22L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(DG33L (1,1),' DG33L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(DG12L (1,1),' DG12L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  !
  CALL GENOUT(DG11LM(1,1),'DG11LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(DG22LM(1,1),'DG22LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(DG33LM(1,1),'DG33LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(DG12LM(1,1),'DG12LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  CALL GENOUT(JG11L (1,1),' JG11L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(JG22L (1,1),' JG22L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(JG33L (1,1),' JG33L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(JG12L (1,1),' JG12L',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  !
  CALL GENOUT(JG11LM(1,1),'JG11LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(JG22LM(1,1),'JG22LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(JG33LM(1,1),'JG33LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(JG12LM(1,1),'JG12LM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  CALL GENOUT(JACOBI(1,1),'JACOBI',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(JACOBM(1,1),'JACOBM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  CALL GENOUT( B2U(1,1,1),'   B2U',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT( B3U(1,1,1),'   B3U',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT( J2U(1,1,1),'   J2U',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT( J3U(1,1,1),'   J3U',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT( PRE(1,1,1),'   PRE',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  CALL GENOUT( B2E(1,1,1),'   B2E',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT( B3E(1,1,1),'   B3E',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT( J2E(1,1,1),'   J2E',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT( J3E(1,1,1),'   J3E',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT( PEQ(1,1,1),'   PEQ',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  !
  CALL GENOUT(DPEDS(1,1,1) ,' DPEDS',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(DPEDSM(1,1,1),'DPEDSM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  !        NEW QUANTITIES FOR INERTIA IN ROTATING PLASMA
  !
  CALL GENOUT(GCHDZ (1,1),' GCHDZ',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GSDZ  (1,1),'  GSDZ',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GBZ   (1,1),'   GBZ',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GBR   (1,1),'   GBR',NO,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  !
  CALL GENOUT(GCHDZM(1,1),'GCHDZM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GSDZM (1,1),' GSDZM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GBZM  (1,1),'  GBZM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GBRM  (1,1),'  GBRM',NO,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  CALL GENOUT(IDIY2(1,1) ,' IDIY2',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IDIY2M(1,1),'IDIY2M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IDIY3(1,1) ,' IDIY3',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IDIY3M(1,1),'IDIY3M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IG122(1,1) ,' IG122',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IG122M(1,1),'IG122M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IG123(1,1) ,' IG123',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IG123M(1,1),'IG123M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(INXX(1,1)  ,'  INXX',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(INXXM(1,1) ,' INXXM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(INXY(1,1)  ,'  INXY',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(INXYM(1,1) ,' INXYM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(INYY(1,1)  ,'  INYY',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(INYYM(1,1) ,' INYYM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(INZZ(1,1)  ,'  INZZ',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(INZZM(1,1) ,' INZZM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IJ0QX(1,1) ,' IJ0QX',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IJ0QXM(1,1),'IJ0QXM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IJ0QY(1,1) ,' IJ0QY',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IJ0QYM(1,1),'IJ0QYM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IGPX2(1,1) ,' IGPX2',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IGPX2M(1,1),'IGPX2M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IGPX3(1,1) ,' IGPX3',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IGPX3M(1,1),'IGPX3M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IGPY2(1,1) ,' IGPY2',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IGPY2M(1,1),'IGPY2M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IGPY3(1,1) ,' IGPY3',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IGPY3M(1,1),'IGPY3M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IDRXX(1,1) ,' IDRXX',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IDRXXM(1,1),'IDRXXM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IRXZ(1,1)  ,'  IRXZ',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IRXZM(1,1) ,' IRXZM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IDRYX(1,1) ,' IDRYX',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IDRYXM(1,1),'IDRYXM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IRYX(1,1)  ,'  IRYX',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IRYXM(1,1) ,' IRYXM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IDRZX(1,1) ,' IDRZX',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IDRZXM(1,1),'IDRZXM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IRZY(1,1)  ,'  IRZY',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IRZYM(1,1) ,' IRZYM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(VISXZ(1,1) ,' VISXZ',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(VISXZM(1,1),'VISXZM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(VISYZ(1,1) ,' VISYZ',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(VISYZM(1,1),'VISYZM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IVS11(1,1) ,' IVS11',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IVS11M(1,1),'IVS11M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IVS12(1,1) ,' IVS12',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IVS12M(1,1),'IVS12M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IVS21(1,1) ,' IVS21',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IVS21M(1,1),'IVS21M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IVS22(1,1) ,' IVS22',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IVS22M(1,1),'IVS22M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GSFC(1,1)  ,'  GSFC',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GSFCM(1,1) ,' GSFCM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GSCC(1,1)  ,'  GSCC',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GSCCM(1,1) ,' GSCCM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GSFS(1,1)  ,'  GSFS',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GSFSM(1,1) ,' GSFSM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GSCS(1,1)  ,'  GSCS',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GSCSM(1,1) ,' GSCSM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GCFC(1,1)  ,'  GCFC',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GCFCM(1,1) ,' GCFCM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(GCFS(1,1)  ,'  GCFS',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(GCFSM(1,1) ,' GCFSM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  CALL GENOUT(EQRHO(1,1) ,'   RHO',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(EQRHOM(1,1),'  RHOM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(DRHOS(1,1) ,' DRHOS',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(DRHOSM(1,1),'DRHOSM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(EQROT(1,1) ,'   ROT',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(EQROTM(1,1),'  ROTM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT( DROT(1,1) ,'  DROT',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT( DROTM(1,1),' DROTM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(  FEQ(1,1) ,'   FEQ',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(  FEQM(1,1),'  FEQM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IWSQ1(1,1) ,' IWSQ1',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IWSQ1M(1,1),'IWSQ1M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IWSQ2(1,1) ,' IWSQ2',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IWSQ2M(1,1),'IWSQ2M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IWSQ3(1,1) ,' IWSQ3',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IWSQ3M(1,1),'IWSQ3M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IJ0QZ(1,1) ,' IJ0QZ',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IJ0QZM(1,1),'IJ0QZM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(JACOF(1,1) ,' JACOF',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(JACOFM(1,1),'JACOFM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(  B2F(1,1) ,'   B2F',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(  B2FM(1,1),'  B2FM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(  B3F(1,1) ,'   B3F',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(  B3FM(1,1),'  B3FM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(JACOS(1,1) ,' JACOS',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(JACOSM(1,1),'JACOSM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(IGF22(1,1) ,' IGF22',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(IGF22M(1,1),'IGF22M',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT( B3FC(1,1) ,'  B3FC',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT( B3FCM(1,1),' B3FCM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT( B2FC(1,1) ,'  B2FC',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT( B2FCM(1,1),' B2FCM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  CALL GENOUT(DJCOF(1,1) ,' DJCOF',NOI,NPISOEFF,NPSI1,MSMAX,1,RM,RN)
  CALL GENOUT(DJCOFM(1,1),'DJCOFM',NOI,NPISOEFF,NPSI,MSMAX,1,RM,RN)
  !
  CLOSE(UNIT=NO,STATUS='KEEP')
  CLOSE(UNIT=NOI,STATUS='KEEP')
  RETURN
  !
  !----------------------------------------------------------------------
  !  12. WRITE VACUUM QUANTITIES FOR LINEAR CODE
  !
120 CONTINUE
  !
  OPEN(UNIT=NETVAC,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='ETAVAC')
  REWIND(NETVAC)
  !
  WRITE(NETVAC,1008) NV1,R0W,RZ0W
  WRITE(NETVAC,1002) (CSV(I),I=1,NV1)
  !
  CALL GENOUT(DG11LV(1,1),' VG11L',NETVAC,NPV1,NV1,MSMAX,1,RM,RN)
  CALL GENOUT(DG22LV(1,1),' VG22L',NETVAC,NPV1,NV1,MSMAX,1,RM,RN)
  CALL GENOUT(DG33LV(1,1),' VG33L',NETVAC,NPV1,NV1,MSMAX,1,RM,RN)
  CALL GENOUT(DG12LV(1,1),' VG12L',NETVAC,NPV1,NV1,MSMAX,1,RM,RN)
  !
  CALL GENOUT(DG11LMV(1,1),'VG11LM',NETVAC,NPV,NV,MSMAX,1,RM,RN)
  CALL GENOUT(DG22LMV(1,1),'VG22LM',NETVAC,NPV,NV,MSMAX,1,RM,RN)
  CALL GENOUT(DG33LMV(1,1),'VG33LM',NETVAC,NPV,NV,MSMAX,1,RM,RN)
  CALL GENOUT(DG12LMV(1,1),'VG12LM',NETVAC,NPV,NV,MSMAX,1,RM,RN)
  !       CALL GENOUT(VJACOB (1,1),'VJACOB',NETVAC,NPV1,NV1,MSMAX,1,RM,RN)
  !        CALL GENOUT(VJACOM (1,1),'VJACOM',NETVAC,NPV,NV,MSMAX,1,RM,RN)
  !
  CLOSE(UNIT=NETVAC,STATUS='KEEP')
  RETURN
  !
  !----------------------------------------------------------------------
  !  13.   SAVE QUANTITIES ON "JSOLVER"
  !
130 CONTINUE
  !
  OPEN(UNIT=JSOLVER,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='JSOLVER')
  REWIND(JSOLVER)
  !
  IS = ISRCHFGE(NISO1EFF,PSIISO,1,CPSICL(1))
  IF (IS.LT.1)     IS = 1
  ! SMISO end point is NISO1EFF, so cannot have IS+1 or IS+2 for IS = NISO1EFF
  IF (IS .GT. NISO1EFF-2) THEN
    IS = NISO1EFF - 2
    PRINT *,' OS: SHOULD NEVER be here... so stop ans ask O. Sauter'
    STOP 'iodisk 130'
  END IF
  !
  NSTTP = 3
  DO  J=IS,NISO1EFF
    CALL CINT(J,SIGPSI(1,J),TETPSI(1,J),WGTPSI(1,J))
  END DO
  !
  IF (IS .GT. 1) THEN
    DO  J=1,IS-1
      CID0(J) = FCCCC0(RMAG**2,CID0(IS),CID0(IS+1), &
           &   CID0(IS+2),RC0P,SMISO(IS),SMISO(IS+1),SMISO(IS+2),SMISO(J))
      CID2(J) = FCCCC0(RC0P,CID2(IS),CID2(IS+1), &
           &   CID2(IS+2),RC0P,SMISO(IS),SMISO(IS+1),SMISO(IS+2),SMISO(J))
    END DO
  ENDIF
  !
  ALLOCATE(ZPSI(NISO1EFF+1))
  ALLOCATE(ZTMF(NISO1EFF+1))
  ALLOCATE(ZTTP(NISO1EFF+1))
  ALLOCATE(ZCPR(NISO1EFF+1))
  ALLOCATE(ZCPPR(NISO1EFF+1))
  ALLOCATE(ZCID0(NISO1EFF+1))
  ALLOCATE(ZCID2(NISO1EFF+1))
  DO J=1,NISO1EFF
    ZPSI(J+1) = PSIISO(J)
    ZTMF(J+1) = TMF(J)
    ZTTP(J+1) = TTP(J)
    ZCPR(J+1) = CPR(J)
    ZCPPR(J+1) = CPPR(J)
    ZCID0(J+1) = CID0(J)
    ZCID2(J+1) = CID2(J)
  END DO
  ZPSI(1)  = SPSIM
  ZTMF(1)  = T0
  ZTTP(1)  = DTTP0
  ZCPR(1)  = CP0
  ZCPPR(1) = DPDP0
  ZCID0(1) = RMAG**2
  ZCID2(1) = 0._RKIND
  !
  IBND = 12 * NT
  IF (NSYM .EQ. 1) THEN
    ZDT  = CPI / REAL(IBND-2,RKIND)
    DO  J=1,IBND+1
      ZT(J) = CPI - REAL(J-2,RKIND) * ZDT
    END DO
  ELSE
    ZDT  = TWOPI / REAL(IBND,RKIND)
    DO J=1,IBND+1
      ZT(J) = CPI - REAL(J-2,RKIND) * ZDT
    END DO
  ENDIF
  !
  CALL BOUND(IBND+1,ZT,ZBND)
  !
  DO J=1,IBND+1
    ZR(J) = R0  + ZBND(J) * COS(ZT(J))
    ZZ(J) = RZ0 + ZBND(J) * SIN(ZT(J))
  END DO
  IMN = ISMIN(IBND+1,ZR,1)
  IMX = ISMAX(IBND+1,ZR,1)
  !
  WRITE(JSOLVER,1200) NISO1EFF+1, NSYM, IBND
  WRITE(JSOLVER,1201) RMAG, RZMAG, RITOT
  WRITE(JSOLVER,1201) BETA, BETAP, .5_RKIND * eqchease_out(1)%profiles_1d%li(NISO1EFF1)
  WRITE(JSOLVER,1201) ZPSI(1), ZPSI(NISO1EFF+1), 0.5_RKIND*(ZR(IMN) + ZR(IMX))
  WRITE(JSOLVER,1203) (ZPSI(L),L=1,NISO1EFF+1)
  WRITE(JSOLVER,1203) (ZCPR(L),L=1,NISO1EFF+1)
  WRITE(JSOLVER,1203) (ZCPPR(L),L=1,NISO1EFF+1)
  WRITE(JSOLVER,1203) (ZTMF(L),L=1,NISO1EFF+1)
  WRITE(JSOLVER,1203) (ZTTP(L),L=1,NISO1EFF+1)
  WRITE(JSOLVER,1203) (ZCID0(L),L=1,NISO1EFF+1)
  WRITE(JSOLVER,1203) (ZCID2(L),L=1,NISO1EFF+1)
  WRITE(JSOLVER,1203) (ZR(L),L=1,IBND+1)
  WRITE(JSOLVER,1203) (ZZ(L),L=1,IBND+1)
  !
  CLOSE(UNIT=JSOLVER,STATUS='KEEP')
  !
  RETURN
  !
  !
1001 FORMAT(3I20,2F20.5)
1002 FORMAT(//,' RADIAL (INTEGER) MESH ',/,(2D30.15))
1008 FORMAT(I20,2F20.5)
1014 FORMAT(5(1X,E12.5))
1015 FORMAT(I8)
1016 FORMAT(E12.5)
1200 FORMAT(3I5)
1201 FORMAT(3E15.8)
1203 FORMAT(E15.8)
1204 FORMAT(5I5)
1301 FORMAT(I5)
1302 FORMAT(2E18.8)
1303 FORMAT(E18.8)
1304 FORMAT(2I5)
9380 FORMAT(A40,A8,3I4)
9381 FORMAT(1P,5E16.9)
  !
END SUBROUTINE IODISK
