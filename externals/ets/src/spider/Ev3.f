!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
	SUBROUTINE DURS_DAT( NOUT,   NTER,  NINEV,
     *                     NURS,
     *                     BETAP0, FTOK0, PSIAX0, HELIN0,
     *                     ALF00,  ALF11, ALF22,
     *                     BET00,  BET11, BET22,
     *                     NGAV0 )
C
       include 'double.inc'
C
C....................................................................
        write(fname,'(a,a)') path(1:kname),'durs.dat'
        open(1,file=fname,form='formatted')
          !OPEN(NINEV, FILE='durs.dat')
C
              read(1,*) NURS
              read(1,*) BETAP0
              read(1,*) FTOK0
              read(1,*) PSIAX0
              read(1,*) HELIN0
                               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              read(1,*) ALF00  !!
              read(1,*) ALF11  !! P'=alf0*(1-(1-psi)**alf1)**alf2
              read(1,*) ALF22  !!
                               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              read(1,*) BET00  !!
              read(1,*) BET11  !! FF'=bet0*(1-(1-psi)**bet1)**bet2
              read(1,*) BET22  !!
                               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              read(1,*) NGAV0
C
          CLOSE(1)
C---------------------------------------------------------------
          !WRITE(NOUT,*) '.........................................'
          !WRITE(NOUT,*) 'FROM FILE "durs.dat" BASIC EQUILIBR. DATA'
          !WRITE(NOUT,*) '............................ '
          !WRITE(NOUT,*) '  NURS    =', NURS
          !WRITE(NOUT,*) '............................ '
          !WRITE(NOUT,*) '  FTOK0   =', FTOK0
          !WRITE(NOUT,*) '  BETAP0  =', BETAP0
          !WRITE(NOUT,*) '  PSIAX0  =', PSIAX0
          !WRITE(NOUT,*) '  HELIN0  =', HELIN0
          !WRITE(NOUT,*) '............................ '
          !WRITE(NOUT,*) '  ALF00   =', ALF00
          !WRITE(NOUT,*) '  ALF11   =', ALF11
          !WRITE(NOUT,*) '  ALF22   =', ALF22
          !WRITE(NOUT,*) '  BET00   =', BET00
          !WRITE(NOUT,*) '  BET11   =', BET11
          !WRITE(NOUT,*) '  BET22   =', BET22
          !WRITE(NOUT,*) '............................ '
          !WRITE(NOUT,*) '  NGAV0   =', NGAV0
          !WRITE(NOUT,*) '....................................... '
C---------------------------------------------------------------
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C****************************************************************
C--- DEFINITION MUTUALS INDUCT. AND  SELFINDUCT. MATRIX
C--- FOR "EDDY" CONDUCTORS:  PPIND
C                           ******* 
C---
	SUBROUTINE L_MATR( NOUT,  NTER, NC, NCPFC, 
     *                   NTYPE, RC,   ZC, VC, HC,
     *                   NECON, WECON )
C
       include 'double.inc'
C
      INCLUDE 'prm.inc'
      INCLUDE 'comevl.inc'
C
      INTEGER    NTYPE(NCLIM)
      DIMENSION  RC(NCLIM), ZC(NCLIM), VC(NCLIM), HC(NCLIM)
      INTEGER    NECON(NILIM)
      DIMENSION  WECON(NILIM)
C....................................................................
C--------------------------------------------------------------------
       pi=3.14159265359d0
       amu0=0.4d0*pi
      DO 11 J1=1,NC
      DO 12 J2=1,NC
            IF( J2.LT.J1 ) THEN
              PPIND(J1,J2) = PPIND(J2,J1)
            END IF
            IF( J2.EQ.J1 ) THEN
              PPIND(J1,J2) = SELIND( NTYPE(J1),RC(J1),VC(J1),HC(J1) )
            END IF
            IF( J2.GT.J1 ) THEN
              PPIND(J1,J2) = BETIND( NTYPE(J1),
     *                               RC(J1), ZC(J1), VC(J1), HC(J1),
     *                               NTYPE(J2),
     *                               RC(J2), ZC(J2), VC(J2), HC(J2) )
            END IF
   12    CONTINUE
C -----
C        WRITE(NOUT,*) '**** J1 = ',J1,' ****'
C        WRITE(NOUT,*) 'PPIND(J1,J2) : J2=1,NC,10'
C        WRITE(NOUT,101) (PPIND(J1,J2),J2=1,NC,10)
C -----
   11 CONTINUE


        do j1=1,NC
        do j2=1,NC
          ppind(j1,j2) = ppind(j1,j2)*amu0
        enddo
        enddo


C -----
C          WRITE(NOUT,*) '** SELFINDUCT. OF "EDDY" CONDUCTORS **'
C          WRITE(NOUT,*) '  PPIND(L,L), L=1,NC  : NC =', NC
C          WRITE(NOUT,101) (PPIND(L,L), L=1,NC)
C          WRITE(NTER,*) '** SELFINDUCT. OF "EDDY" CONDUCTORS **'
C          WRITE(NTER,*) '  PPIND(L,L), L=1,NC  : NC =', NC
C          WRITE(NTER,101) (PPIND(L,L), L=1,NC)
C
C********************************************************************
C  TRANSFORM OF "FULL" MATRIX "P" ( "NC*NC" SIZE ) TO
C  "EQUIVALENT" MATRIX "P" ( "NCEQUI*NCEQUI" SIZE ).
C  HERE   NCEQUI = NC - NCPFC + NEQUI.
C
         CALL TRAMAT( PPIND, NJLIM, NC, NCPFC, NEQUI,
     *                NECON, WECON )
C
C     [ PPIND(i,j) PET units ] = [micro*H] * BBB
C       BBB = 10.D0 / (4.D0*PI) / (2.D0*PI) = 0.12665148...
C
C      PPIND(19,19) = 12.2d0     !!! iz bazy dannyh TCV (v edinizah PET)
C      PPIND(19,19) = 27.46153d0 !!! iz nashei analit. formuly
C      PPIND(19,19) = 110.0d0*0.12665148d0
C
         NCEQUI = NC - NCPFC + NEQUI
C -----
C        WRITE(NTER,*) '** SELFINDUCT. OF "EDDY" CONDUCTORS **'
         !WRITE(NOUT,*) '** SELFINDUCT. OF "EDDY" CONDUCTORS **'
         !WRITE(NOUT,*) '  PPIND(L,L), L=1,NCEQUI  : NCEQUI =', NCEQUI
         !WRITE(NOUT,101) (PPIND(L,L), L=1,NCEQUI)
C        WRITE(NTER,*) '  PPIND(L,L), L=1,NCEQUI  : NCEQUI =', NCEQUI
C        WRITE(NTER,101) (PPIND(L,L), L=1,NCEQUI)
C
C        WRITE(NTER,*) '** MUTUALS INDUCT. OF "EDDY" CONDUCTORS **'
         !WRITE(NOUT,*) '** MUTUALS INDUCT. OF "EDDY" CONDUCTORS **'
      !DO L=1,NCEQUI
         !WRITE(NOUT,*) ' PPIND(L,J), L =',L,' J=1,NCEQUI =', NCEQUI
         !WRITE(NOUT,101) (PPIND(L,J), J=1,NCEQUI)
C        WRITE(NTER,*) ' PPIND(L,J), L =',L,' J=1,NCEQUI =', NCEQUI
C        WRITE(NTER,101) (PPIND(L,J), J=1,NCEQUI)
      !END DO
C
C--------------------------------------------------------------------
        !ngra1 = 14
        write(fname,'(a,a)') path(1:kname),'ppind_mat.wr'
        open(1,file=fname,form='formatted')
        !open(ngra1,file='ppind_mat.wr')
           write(1,*) ncequi
           write(1,*) ((ppind(i,j),i=1,ncequi), j=1,ncequi)
        close(1)
C--------------------------------------------------------------------
  101  FORMAT(2X,5E14.7)
C
C--------------------------------------------------------------------
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	SUBROUTINE rd_ppind
C
         include 'double.inc'
C
      INCLUDE 'prm.inc'
      INCLUDE 'comevl.inc'
	common/comeqg/ ncequi
c
        !ngra1 = 14
        write(fname,'(a,a)') path(1:kname),'ppind_mat.wr'
        open(1,file=fname,form='formatted')
        !open(1,file='ppind_mat.wr')
           read(1,*) ncequi
           read(1,*) ((ppind(i,j),i=1,ncequi), j=1,ncequi)
        close(1)
c
      return
      end
C***********************************************************************
C--- INPUT OF POSITIONS OF "PF_PROBE" POINTS:
C
	SUBROUTINE PROPNT( NOUT, NTER, NINFW, NGRA1,
     *                   NPRO, RPRO, ZPRO,  FIPRO )
C
       include 'double.inc'
C
      DIMENSION  RPRO(*), ZPRO(*), FIPRO(*)
C
        write(fname,'(a,a)') path(1:kname),'pf_probe.dat'
        open(1,file=fname,form='formatted')
      !OPEN (NINFW, FILE='pf_probe.dat')
C
        READ(1,*)  NPRO
        !WRITE(NOUT,*) '--------------------------------------'
        !WRITE(NOUT,*) 'NUMBER OF "PROBE" POINTS  NPRO =', NPRO
        !WRITE(NOUT,*) '                                      '
       ! WRITE(NTER,*) '--------------------------------------'
       ! WRITE(NTER,*) 'NUMBER OF "PROBE" POINTS  NPRO =', NPRO
       ! WRITE(NTER,*) '                                      '
C
      IF( NPRO.NE.0 ) THEN
C
        !WRITE(NOUT,*) 'THEIR NUMBER (i), POSITION "RPRO(i), ZPRO(i)"'
        !WRITE(NOUT,*) '   AND ORIENTATION ANGLE   "FIPRO(i)" :      '
        DO 397 L=1,NPRO
           READ(1,*) RPRO(L), ZPRO(L), FIPRO(L)
C       WRITE(NOUT,*) L, RPRO(L), ZPRO(L), FIPRO(L)
  397   CONTINUE
C--------------------------------------------------------------------
C--------------------------------------------------------------------
      END IF
C
      CLOSE (1)
C--------------------------------------------------------------------
        write(fname,'(a,a)') path(1:kname),'propoi.wr'
        open(1,file=fname,form='formatted')
        !open(ngra1,file='propoi.wr')
           write(1,*) npro
           write(1,*) ( rpro(i), i=1,npro)
		 write(1,*) ( zpro(i), i=1,npro)
		 write(1,*) (fipro(i), i=1,npro)
        close(1)
      RETURN
      END
C***********************************************************************
C--- READING OF POSITIONS OF "PF_PROBE" POINTS:
C
	SUBROUTINE rd_prob( NPRO, RPRO, ZPRO,  FIPRO )
C
         include 'double.inc'
C
      DIMENSION  RPRO(*), ZPRO(*), FIPRO(*)
C
        !ngra1 = 14
        write(fname,'(a,a)') path(1:kname),'propoi.wr'
        open(1,file=fname,form='formatted')
        !open(1,file='propoi.wr')
           read(1,*) npro
           read(1,*) ( rpro(i), i=1,npro)
		 read(1,*) ( zpro(i), i=1,npro)
		 read(1,*) (fipro(i), i=1,npro)
        close(1)
C
      RETURN
      END
C***********************************************************************
C--- INPUT OF POSITIONS OF "FL_LOOP" POINTS:
C
      SUBROUTINE LOOPNT( NOUT, NTER, NINFW, NGRA1,
     *                   NLOO, RLOO, ZLOO )
C
       include 'double.inc'
C
      DIMENSION  RLOO(*), ZLOO(*)
 
       NINFW=1
       ngra1=1
 
        write(fname,'(a,a)') path(1:kname),'fl_loop.dat'
        open(NINFW,file=fname,form='formatted')
      !OPEN(NINFW, FILE='fl_loop.dat')
C
        READ(NINFW,*)  NLOO
        !WRITE(NOUT,*) '--------------------------------------'
        !WRITE(NOUT,*) 'NUMBER OF "LOOP" POINTS  NLOO =', NLOO
        !WRITE(NOUT,*) '                                      '
       ! WRITE(NTER,*) '--------------------------------------'
       ! WRITE(NTER,*) 'NUMBER OF "LOOP" POINTS  NLOO =', NLOO
       ! WRITE(NTER,*) '                                      '
C
      IF( NLOO.NE.0 ) THEN
C
        !WRITE(NOUT,*) 'THEIR NUMBER (i), POSITION "RLOO(i), ZLOO(i)"'
C
        DO 397 L=1,NLOO
           READ(NINFW,*) RLOO(L), ZLOO(L)
C       WRITE(NOUT,*) L, RLOO(L), ZLOO(L)
  397   CONTINUE
C--------------------------------------------------------------------
C--------------------------------------------------------------------
      END IF
C
      CLOSE (NINFW)
        write(fname,'(a,a)') path(1:kname),'loopoi.wr'
        open(ngra1,file=fname,form='formatted')
        !open(ngra1,file='loopoi.wr')
           write(ngra1,*) nloo
           write(ngra1,*) (rloo(i), i=1,nloo)
           write(ngra1,*) (zloo(i), i=1,nloo)
        close(ngra1)
C--------------------------------------------------------------------
      RETURN
      END
C***********************************************************************
C--- INPUT OF POSITIONS OF "FL_LOOP" POINTS:
C
      SUBROUTINE rd_loop( NLOO, RLOO, ZLOO )

         include 'double.inc'
         DIMENSION  RLOO(*), ZLOO(*)
C
        ngra1 = 1
        write(fname,'(a,a)') path(1:kname),'loopoi.wr'
        open(1,file=fname,form='formatted')
        !open(ngra1,file='loopoi.wr')
           read(ngra1,*) nloo
           read(ngra1,*) (rloo(i), i=1,nloo)
           read(ngra1,*) (zloo(i), i=1,nloo)
        close(ngra1)
C
      RETURN
      END
C***********************************************************************
C
C.....  TOROIDAL CURRENTS OF PASSIVE CONDUCTOR STRUCTURES 
C
        SUBROUTINE PASCUR( NOUT, NTER, NEQUI, NFW, NBP, NVV,
     *               PJK, fwcurr, bpcurr, vvcurr )
C
       include 'double.inc'
C
        DIMENSION  PJK(*)
C
          fwcurr = 0.d0
          IF( NFW .NE. 0 )  THEN
             DO I=1,NFW
                fwcurr = fwcurr + PJK(NEQUI+I)
             enddo
          END IF
          bpcurr = 0.d0
          IF( NBP .NE. 0 )  THEN
             DO I=1,NBP
                bpcurr = bpcurr + PJK(NEQUI+NFW+I)
             enddo
          END IF
          vvcurr = 0.d0
          IF( NVV .NE. 0 )  THEN
             DO I=1,NVV
                vvcurr = vvcurr + PJK(NEQUI+NFW+NBP+I)
             enddo
          END IF
C
C--------------------------------------------------------------------
      RETURN
      END
C***********************************************************************
C--- PRINTING AND WRITING OF NUMBERS AND POSITIONS OF LIMITER POINTS:
C
        SUBROUTINE PRTLIM( NOUT, NTER, NGRA1 )
C
         include 'double.inc'
C
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'
C
      IF( NCTRL .NE. 0 )  THEN
        !WRITE(NOUT,*) '-----------------------------------------'
       ! WRITE(NTER,*) '-----------------------------------------'
        !WRITE(NOUT,*) 'LIMITER POINTS REGIME:  NCTRL  =', NCTRL
       ! WRITE(NTER,*) 'LIMITER POINTS REGIME:  NCTRL  =', NCTRL
        !WRITE(NOUT,*) 'TOTAL NUMBER OF LIMITER POINTS =', NBLM
       ! WRITE(NTER,*) 'TOTAL NUMBER OF LIMITER POINTS =', NBLM
        !WRITE(NOUT,*) '                                      '
C
        IF( NBLM  .NE. 0 )  THEN
        !WRITE(NOUT,*) 'THEIR NUMBER AND POSITION :'
        DO 603 L=1,NBLM
C          WRITE(NOUT,*) L, RBLM(L), ZBLM(L)
  603   CONTINUE
        !WRITE(NOUT,*) '-----------------------------------------'
        !WRITE(NOUT,*) '                                         '
C
        write(fname,'(a,a)') path(1:kname),'limpoi.wr'
        open(ngra1,file=fname,form='formatted')
        !open(ngra1,file='limpoi.wr')
           write(ngra1,*) nblm
           write(ngra1,*) (rblm(i), i=1,nblm), (zblm(i), i=1,nblm)
        close(ngra1)
C
        END IF
      END IF
C
C-----------------------------------------------------------------------
      RETURN
      END
C***********************************************************************
C--------------------- WRITING IN DISK FILES ---------------------------
C
      SUBROUTINE WRTD1( NOUT,   NTER,   NFRWR1, NVAR,   NGAV1,
     *                  ngra1,  ngra2,  nboun,  nbran,  nprob, nloop,
     *                  KSTEP,  KNEL,   TSTEP,  TIMEV,
     *                  RM,     ZM,     RX0,    ZX0,
     *                  erps  , errcu2, delps2, delaxr,
     *                  tokout, psiout, PSIBOU, PSIDEL,
     *                  betpol, zli3,   alpnew, numlim,
     *                  pvolum, fwcurr, bpcurr, vvcurr,
     *                  NPFC,   NCOP,   NEQUI,  NC,     NCPFC,  NCEQUI,
     *                  NFW,    NBP,    NVV,    NSEGFW, CFWSEG,
     *                  npro,   brpro , bzpro , bpcom , bp_exp,
     *                  nloo,   psloo , ps_exp,
     *                  n_volt, pfvol1, v_full,
     *                  n_cp,   cp_com, cp_exp)
C
       include 'double.inc'
C
       INCLUDE 'param.inc'
C
       common /comus1/ rus1(nbndp2),  zus1(nbndp2), nus1
       common /comus2/ rus2(nbndp2),  zus2(nbndp2), nus2
       common /comlop/  rxb(nbndp2),   zxb(nbndp2), nxb
C
       common /equili/ bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                         zmx,    rrzmx,  zmn,   rrzmn,
     *                         r0cen,  z0cen,  radm,  aspect,
     *                         Eupper, Elower, DELup, DELlw, Bfvakc
       common /volpla/ Vol_pl
C
       DIMENSION  CFWSEG(*)
       DIMENSION  brpro(*),  bzpro(*),  bpcom(*),  psloo(*)
       DIMENSION  bp_exp(*), ps_exp(*)
       DIMENSION  cp_com(*), cp_exp(*)
       DIMENSION  pfvol1(*), v_full(*)
C.......................................................................
C
       pvolum = 2.d0*PI*Vol_pl
C
C----------
       open(ngra1,file='nsteps.wr')
            KSTEP1 = KSTEP + 1
            write(ngra1,*) NVAR, KSTEP1
       close(ngra1)
C----------
       open(ngra2,file='tvalues.wr')
            write(ngra2,*) kstep , timev , tstep , knel  , ngav1,
     *                     erps  , errcu2, delps2, delaxr,
     *                     rm    , zm    , rx0   , zx0   ,
     *                     tokout, psiout, betpol, zli3  ,
     *                     alpnew, numlim, PSIBOU, PSIDEL,
     *                     pvolum, fwcurr, bpcurr, vvcurr
       close(ngra2)
C----------
       open(ngra2,file='bou_geom_t.wr')
          write(ngra2,*) kstep , timev,
     *                   bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                           zmx,    rrzmx,  zmn,   rrzmn,
     *                           r0cen,  z0cen,  radm,  aspect,
     *                           Eupper, Elower, DELup, DELlw
       close(ngra2)
C----------
       open(ngra2,file='z_axis_t.wr')
            write(ngra2,*) timev , zm
       close(ngra2)
C----------
       open(ngra1,file='ncurrs.wr')
            write(ngra1,*)  NPFC, NCOP, NEQUI, NC, NCPFC, NCEQUI,
     *                      NFW, NBP, NVV
       close(ngra1)
C---------------------------------------------------------------
      IF( NFRWR1.NE.0 )  THEN
          label1 = 1
C.........
          open(ngra1,file='n_voltage.wr')
               write(ngra1,*) NVAR, n_volt, label1
          close(ngra1)
          open(ngra2,file='voltage.wr')
              write(ngra2,*)  KSTEP, TIMEV, TSTEP
              write(ngra2,*) (pfvol1(i), i=1,n_volt)
              write(ngra2,*) (v_full(i), i=1,n_volt)
          close(ngra2)
C.........
          open(ngra1,file='n_cp.wr')
               write(ngra1,*) NVAR, n_cp, label1
          close(ngra1)
          open(ngra2,file='cp_com.wr')
              write(ngra2,*)  KSTEP, TIMEV, TSTEP
              write(ngra2,*) (cp_com(i),  i=1,n_cp)
              write(ngra2,*) (cp_exp(i), i=1,n_cp)
          close(ngra2)
C...........
           open(ngra1,file='ncurse.wr')
                write(ngra1,*) NVAR, NSEGFW, label1
           close(ngra1)
           open(ngra2,file='tcurse.wr')
                write(ngra2,*) KSTEP, TIMEV, TSTEP
                write(ngra2,*) (CFWSEG(I), I=1,NSEGFW)
           close(ngra2)
C.........
      if( npro.ne.0 ) then
          open(ngra1,file='n_probf.wr')
               write(ngra1,*) NVAR, npro, label1
          close(ngra1)
       open(nprob,file='probf.wr')
             write(nprob,*)  kstep, timev, tstep
             write(nprob,*) (brpro(i), i=1,npro)
             write(nprob,*) (bzpro(i), i=1,npro)
             write(nprob,*) (bpcom(i), i=1,npro)
             write(nprob,*) (bp_exp(i),i=1,npro)
       close(nprob)
      end if
C.........
      if( nloo.ne.0 ) then
       open(ngra1,file='n_loopf.wr')
             write(ngra1,*) NVAR, nloo, label1
       close(ngra1)
       open(nloop,file='loopf.wr')
             write(nloop,*) kstep, timev, tstep
             write(nloop,*) (psloo(i), i=1,nloo)
             write(nloop,*) (psloo(i), i=1,nloo)
             write(nloop,*) (ps_exp(i),i=1,nloo)
       close(nloop)
      end if
C.........
        open(nboun,file='plbound.wr')
             write(nboun,*) nxb, kstep, timev
             write(nboun,*) rx0, zx0
             do i=1,nxb
                write(nboun,*) rxb(i), zxb(i)
             enddo
C             write(nboun,*) (rxb(i),i=1,nxb)
C             write(nboun,*) (zxb(i),i=1,nxb)
        close(nboun)
C.........
C       open(nbran,file='branchs.wr')
C            write(nbran,*) nus1, nus2, kstep, timev
C            write(nbran,*) (rus1(i),i=1,nus1)
C            write(nbran,*) (zus1(i),i=1,nus1)
C            write(nbran,*) (rus2(i),i=1,nus2)
C            write(nbran,*) (zus2(i),i=1,nus2)
C.........
      END IF
C
C-----------------------------------------------------------------------
      RETURN
      END
C***********************************************************************
C--------------------- WRITING IN DISK FILES ---------------------------
C
      SUBROUTINE WRTD2( NOUT,   NTER,   NFRWR1, NVAR,   NGAV1,
     *                  ngra1,  ngra2,  nboun,  nbran,  nprob, nloop,
     *                  KSTEP,  KNEL,   TSTEP,  TIMEV,
     *                  RM,     ZM,     RX0,    ZX0,
     *                  erps  , errcu2, delps2, delaxr,
     *                  tokout, psiout, PSIBOU, PSIDEL,
     *                  betpol, zli3,   alpnew, numlim,
     *                  pvolum, fwcurr, bpcurr, vvcurr,
     *                  NPFC,   NCOP,   NEQUI,  NC,     NCPFC,  NCEQUI,
     *                  NFW,    NBP,    NVV,    NSEGFW, CFWSEG,
     *                  npro,   brpro , bzpro , bpcom , bp_exp,
     *                  nloo,   psloo , ps_exp,
     *                  n_volt, pfvol2, v_full,
     *                  n_cp,   cp_com, cp_exp )
C
       include 'double.inc'
C
       INCLUDE 'param.inc'
C
       common /comus1/ rus1(nbndp2),  zus1(nbndp2), nus1
       common /comus2/ rus2(nbndp2),  zus2(nbndp2), nus2
       common /comlop/  rxb(nbndp2),   zxb(nbndp2), nxb
C
       common /equili/ bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                         zmx,    rrzmx,  zmn,   rrzmn,
     *                         r0cen,  z0cen,  radm,  aspect,
     *                         Eupper, Elower, DELup, DELlw, Bfvakc
       common /volpla/ Vol_pl
C
       DIMENSION  CFWSEG(*)
       DIMENSION  brpro(*),  bzpro(*),  bpcom(*),  psloo(*)
       DIMENSION  bp_exp(*), ps_exp(*)
       DIMENSION  cp_com(*), cp_exp(*)
       DIMENSION  pfvol2(*), v_full(*)
C.......................................................................
C
         pvolum = 2.d0*PI*Vol_pl
C
C=======================================================================
C----------
       open(ngra1,file='nsteps.wr')
            KSTEP1 = KSTEP + 1
            write(ngra1,*) NVAR, KSTEP1
       close(ngra1)
C.....
       open(ngra2,file='tvalues.wr')
            call endfl(ngra2)
            write(ngra2,*) kstep , timev , tstep , knel  , ngav1,
     *                     erps  , errcu2, delps2, delaxr,
     *                     rm    , zm    , rx0   , zx0   ,
     *                     tokout, psiout, betpol, zli3  ,
     *                     alpnew, numlim, PSIBOU, PSIDEL,
     *                     pvolum, fwcurr, bpcurr, vvcurr
       close(ngra2)
C......
       open(ngra2,file='bou_geom_t.wr')
          call endfl(ngra2)
          write(ngra2,*) kstep , timev,
     *                   bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                           zmx,    rrzmx,  zmn,   rrzmn,
     *                           r0cen,  z0cen,  radm,  aspect,
     *                           Eupper, Elower, DELup, DELlw
       close(ngra2)
C......
       open(ngra2,file='z_axis_t.wr')
            call endfl(ngra2)
            write(ngra2,*) timev , zm
       close(ngra2)
C-----------------------------------------------------------------------
      IF( NFRWR1 .NE. 0 )  THEN
      IF( (KSTEP/NFRWR1)*NFRWR1 .EQ. KSTEP )  THEN
C
           label  = KSTEP/NFRWR1
           label1 = label + 1
C...........
          open(ngra1,file='n_voltage.wr')
               write(ngra1,*) NVAR, n_volt, label1
          close(ngra1)
          open(ngra2,file='voltage.wr')
              call endfl(ngra2)
              write(ngra2,*)  KSTEP, TIMEV, TSTEP
              write(ngra2,*) (pfvol2(i), i=1,n_volt)
              write(ngra2,*) (v_full(i), i=1,n_volt)
          close(ngra2)
C.........
          open(ngra1,file='n_cp.wr')
               write(ngra1,*) NVAR, n_cp, label1
          close(ngra1)
          open(ngra2,file='cp_com.wr')
              call endfl(ngra2)
              write(ngra2,*)  KSTEP, TIMEV, TSTEP
              write(ngra2,*) (cp_com(i),  i=1,n_cp)
              write(ngra2,*) (cp_exp(i), i=1,n_cp)
          close(ngra2)
C...........
           open(ngra1,file='ncurse.wr')
                write(ngra1,*) NVAR, NSEGFW, label1
           close(ngra1)
           open(ngra2,file='tcurse.wr')
                call endfl(ngra2)
                write(ngra2,*) KSTEP, TIMEV, TSTEP
                write(ngra2,*) (CFWSEG(I), I=1,NSEGFW)
           close(ngra2)
C.........
      if( npro.ne.0 ) then
          open(ngra1,file='n_probf.wr')
               write(ngra1,*) NVAR, npro, label1
          close(ngra1)
       open(nprob,file='probf.wr')
             call endfl(nprob)
             write(nprob,*)  kstep, timev, tstep
             write(nprob,*) (brpro(i), i=1,npro)
             write(nprob,*) (bzpro(i), i=1,npro)
             write(nprob,*) (bpcom(i), i=1,npro)
             write(nprob,*) (bp_exp(i),i=1,npro)
       close(nprob)
      end if
C.........
      if( nloo.ne.0 ) then
       open(ngra1,file='n_loopf.wr')
             write(ngra1,*) NVAR, nloo, label1
       close(ngra1)
       open(nloop,file='loopf.wr')
             call endfl(nloop)
             write(nloop,*) kstep, timev, tstep
             write(nloop,*) (psloo(i), i=1,nloo)
             write(nloop,*) (psloo(i), i=1,nloo)
             write(nloop,*) (ps_exp(i),i=1,nloo)
       close(nloop)
      end if
C...........
C   Writing in file = 'plbound.wr'
C
C        open(nboun,file='plbound.wr')
C             call endfl(nboun)
C             write(nboun,*) nxb, kstep, timev
C             write(nboun,*) (rxb(i),i=1,nxb)
C             write(nboun,*) (zxb(i),i=1,nxb)
C        close(nboun)
C...........
C   Writing in file = 'branchs.wr'
C
C            write(nbran,*) nus1, nus2, kstep, timev
C            write(nbran,*) (rus1(i),i=1,nus1)
C            write(nbran,*) (zus1(i),i=1,nus1)
C            write(nbran,*) (rus2(i),i=1,nus2)
C            write(nbran,*) (zus2(i),i=1,nus2)
C.........
C.........
      END IF
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
C***********************************************************************
C--------------------- WRITING IN DISK FILES ---------------------------
C
      SUBROUTINE WRTD11( NOUT,   NTER,    NFRWR1, NVAR,
     *                   ngra1,  ngra2,
     *                   KSTEP,  TSTEP,   TIMEV,
     *                   n_volt, pfc_exp, pfc_ref )
C
       include 'double.inc'
C
       INCLUDE 'prm.inc'
       INCLUDE 'comevl.inc'
C-----------------------------------------------------------------------
C
       DIMENSION  pfc_exp(n_volt_m), pfc_ref(n_volt_m)
       DIMENSION  pfc_ex1(n_volt_m), pfc_re1(n_volt_m)
C-----------------------------------------------------------------------
      IF( NFRWR1.NE.0 )  THEN
          label1 = 1
C.........
          open(ngra1,file='ntcurr.wr')
               write(ngra1,*) NVAR, NPFC, label1
          close(ngra1)
          open(ngra2,file='tcurrs.wr')
              write(ngra2,*)  KSTEP, TIMEV, TSTEP
              write(ngra2,*) (PFCUR1(I), I=1,NPFC)
          close(ngra2)
C.........
          do i=1,n_volt
            pfc_ex1(i)=pfc_exp(i)*0.000001d0
            pfc_re1(i)=pfc_ref(i)*0.000001d0
          end do
C
          open(ngra1,file='n_pfceqw.wr')
               write(ngra1,*) NVAR, n_volt, label1
          close(ngra1)
          open(ngra2,file='pfceqw.wr')
              write(ngra2,*)  KSTEP, TIMEV, TSTEP
              write(ngra2,*) (pfceqw(i),  i=1,n_volt)
              write(ngra2,*) (pfc_ex1(i), i=1,n_volt)
              write(ngra2,*) (pfc_re1(i), i=1,n_volt)
          close(ngra2)
      END IF
C
      RETURN
      END

C***********************************************************************
C--------------------- WRITING IN DISK FILES ---------------------------
C
      SUBROUTINE WRTD22( NOUT,   NTER,    NFRWR1, NVAR,
     *                   ngra1,  ngra2,
     *                   KSTEP,  TSTEP,   TIMEV,
     *                   n_volt, pfc_exp, pfc_ref )
C
       include 'double.inc'
C
       INCLUDE 'prm.inc'
       INCLUDE 'comevl.inc'
C-----------------------------------------------------------------------
C
       DIMENSION  pfc_exp(n_volt_m), pfc_ref(n_volt_m)
       DIMENSION  pfc_ex1(n_volt_m), pfc_re1(n_volt_m)
C-----------------------------------------------------------------------
      IF( NFRWR1 .NE. 0 )  THEN
      IF( (KSTEP/NFRWR1)*NFRWR1 .EQ. KSTEP )  THEN
C
           label  = KSTEP/NFRWR1
           label1 = label + 1
C...........
           open(ngra1,file='ntcurr.wr')
                write(ngra1,*) NVAR, NPFC, label1
           close(ngra1)
           open(ngra2,file='tcurrs.wr')
                call endfl(ngra2)
                write(ngra2,*) KSTEP, TIMEV, TSTEP
                write(ngra2,*) (PFCUR2(I), I=1,NPFC)
           close(ngra2)
C.........
          do i=1,n_volt
            pfc_ex1(i)=pfc_exp(i)*0.000001d0
            pfc_re1(i)=pfc_ref(i)*0.000001d0
          end do
C
          open(ngra1,file='n_pfceqw.wr')
               write(ngra1,*) NVAR, n_volt, label1
          close(ngra1)
          open(ngra2,file='pfceqw.wr')
              call endfl(ngra2)
              write(ngra2,*)  KSTEP, TIMEV, TSTEP
              write(ngra2,*) (pfceqw(i),  i=1,n_volt)
              write(ngra2,*) (pfc_ex1(i), i=1,n_volt)
              write(ngra2,*) (pfc_re1(i), i=1,n_volt)
          close(ngra2)
C.........
      END IF
      END IF
C
      RETURN
      END











