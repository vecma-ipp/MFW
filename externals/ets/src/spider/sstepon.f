!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
       SUBROUTINE  sSTEPON( KLUCH, k_auto, nstep, dt, time,
     *                     voltpf, d_pf_mat,d_tcam_mat,key_dmf )

         use durs_d_modul       

C---------------------------------------------------------------
       INCLUDE 'Descript.inc'
C  ----------
C
       include 'double.inc'
C
       INCLUDE 'prm.inc'
       INCLUDE 'comevl.inc'
       !INCLUDE 'contro.inc'
       !parameter(nstep_p=3000)
C-----------------------------------------------------------------------
       INCLUDE 'comtim.inc'

	 common/comeqg/  ncequi
       common /nostep/ kstep
       common /comhel/ helinp, helout
       
!       common /comsta/ epsro,platok,
!     *                 eqdfn,betplx,tokf,psax,b0,r0,
!     *                 alf0,alf1,alf2,bet0,bet1,bet2,rax,zax,
!     *                 nurs,igdf,n_psi,n_tht,i_bsh,keyctr,i_betp,i_eqdsk
     
       common /comsta/ platok,eqdfn,i_bsh
       
       common /com234/ betpol,tokout,psiout
       common /com_cam/ camtok
	 common /c_kpr/ kpr
       common /key_for_pet/ key_fixbon
       common/com_flag/kastr
C-----------------------------------------------------------------------
C
       DIMENSION  RC(NCLIM),  ZC(NCLIM),  PC(NCLIM), PSIP(NCLIM),
     *            VC(NCLIM),  HC(NCLIM)
C
       DIMENSION  RC1(NCLIM), ZC1(NCLIM), RC2(NCLIM), ZC2(NCLIM),
     *            RC3(NCLIM), ZC3(NCLIM), RC4(NCLIM), ZC4(NCLIM)
C
       INTEGER    NECON(NILIM), NTYPE(NCLIM)
       DIMENSION  WECON(NILIM)
C
       common/comst0/ RES(NJLIM),  VOLK(NJLIM),  VOLKP1(NJLIM)
C
       common/comst1/ PJK(NJLIM),PJKP1(NJLIM),PJKP(NJLIM),PJKD(NJLIM)
       include 'parloo.inc'
       common/comloo/ rloop(nloopp),zloop(nloopp),
     &          rprob(nprobp),zprob(nprobp),fiprob(nprobp),nloop,nprob
       common/comst2/ PSK(NJLIM), PSKP1(NJLIM),PSKP(NJLIM),PSKM1(NJLIM)

       !DIMENSION  cp_com_0(n_cp_m)
C***********************************************************************

       DIMENSION  psiplb(nstep_p),psiexb(nstep_p),psimag(nstep_p),
     *            flu_tor(nstep_p)  

C***********************************************************************

       real*8 voltpf(*)
       real*8 d_pf_mat(*)
       real*8 d_tcam_mat(*)

C***********************************************************************


	 real*4       a_print(200)
	 character*30 apr
       character*40 eqdfn


          ! call put_tim(dt,time)

         kstep = nstep
         timev = time
         tstep = dt


!(((((((((((((((((((((((
!      	i_diag=1
!)))))))))))))))))))))))

         pi  = 3.14159265359d0
         BBB  = 1.d0 / (2.d0*pi)

       SIGM   = 1.d0  !0.50d0

       TSTEPR = TSTEP
       TSTART = 0.00d0

       TSTOP  = 20.0d0
       KSTOP  = 1000

       ENELS  = 5.0d-8
       KNELS  = 500

       NFRPR1 = 5
       NFRWR1 = 0

          NOUT  = 17
          NTER  = 6
          NINFW = 7
          NINEV = 10
          ngra1 = 14
          ngra2 = 15
C


       IF( KLUCH .NE. 0 ) GO TO 1111

 1149     format(a40)
C
      ! OPEN( NOUT, FILE='EVOL0.PRT', FORM='FORMATTED' )
       !write(nout,*) '***********************************************'
      ! write(nout,*) '!               OUTPUT FILE                   !'
      ! write(nout,*) '!                   OF                        !'
      ! write(nout,*) '!  the PET code (Plasma Evolution in Tokamak) !'
      ! write(nout,*) '! ------------------------------------------- !'
      ! write(nout,*) '!  free boundary tokamak equilibrium problem  !'
      ! write(nout,*) '!                    and                      !'
      ! write(nout,*) '!             circuit equations               !'
      ! write(nout,*) '***********************************************'
C
       numwr  = 0
C--------------------------------------------------------------------
C
C======ITER, SOF
C
C=============================================

       KSTEPR = KSTEP
       KEYPRI = 1

C-----------------------------------------------------------------
C --- INPUT OF "BASIC" EQULIBRIUM PARAMETERS FROM FILE "durs.dat"
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(k_auto.eq.0) go to 2005 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(kastr.eq.1) go to 1150 

        write(fname,'(a,a)') path(1:kname),'durs.dat'
        open(1,file=fname)
          !open(1,file='durs.dat')

              read(1,*) n_tht
              read(1,*) n_psi
              read(1,*) igdf
              read(1,*) epsro

              read(1,*) nurs
              read(1,*) keyctr

              read(1,*)    i_eqdsk
              read(1,1149) eqdfn
 
              read(1,*) i_betp
              read(1,*) betplx
              read(1,*) tokf
              read(1,*) psax
              read(1,*) b0,r0
                             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              read(1,*) alf0 !!
              read(1,*) alf1 !! P'=alf0*(1-(1-psi)**alf1)**alf2
              read(1,*) alf2 !!
                             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              read(1,*) bet0 !!
              read(1,*) bet1 !! FF'=bet0*(1-(1-psi)**bet1)**bet2
              read(1,*) bet2 !!
                             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              read(1,*) alw0 !!
              read(1,*) alw1 !! (ro*w**2)'=alw0*(1-(1-psi)**alw1)**alw2
              read(1,*) alw2 !!
                             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              read(1,*) rax
              read(1,*) zax

          close(1)

         if(i_eqdsk.eq.1) then

               call tab_efit(tokf,psax,eqdfn,rax,zax,b0,r0)

               nurs   = -3999
               i_betp = 0

         endif

        write(fname,'(a,a)') path(1:kname),'durs_d.dat'
        open(1,file=fname,form='formatted')
          !open(1,file='durs_d.dat')
              write(1,*) n_tht,n_psi,igdf,nurs,keyctr,i_eqdsk,i_betp
              write(1,*) epsro,betplx,tokf,psax,b0,r0,rax,zax
              write(1,*) alf0,alf1,alf2,bet0,bet1,bet2
          close(1)

 1150   continue


C***********************************************************************
C--- INPUT OF POSITIONS OF "PF_PROBE" POINTS:
C
        CALL PROPNT( NOUT, NTER, NINFW, NGRA1,
     *               NPRO, RPROb, ZPROb, FIPROb )
C***********************************************************************
C--- INPUT OF POSITIONS OF "FL_LOOP" POINTS:
C
        CALL LOOPNT( NOUT, NTER, NINFW, NGRA1,
     *               NLOOp, RLOOp, ZLOOp )
C***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C***********************************************************************
C--- INPUT PARAMETERS OF PFC SYSTEM AND PASSIV CONDUCTORS
C
       CALL CONDUC( NC, NCEQUI, NCPFC, NFW, NBP, NVV,
     *              RC,ZC, PC, VC,HC, NTYPE,
     *              RC1,ZC1,  RC2,ZC2,  RC3,ZC3,  RC4,ZC4,
     *              RES, VOLK, VOLKP1,
     *              NECON, WECON,
     *              NOUT, NTER, NINFW, ngra1 )



C
C***********************************************************************
C--- DEFINITION INDUCT. AND  SELFINDUCT. MATRIX
C--- FOR "EDDY" CONDUCTORS:  "PPIND" from COMMON /PPIDPS/
C                            *******
C
	  CALL L_MATR( NOUT,  NTER, NC, NCPFC,
     *               NTYPE, RC,   ZC, VC, HC,
     *               NECON,WECON )
C
C-----------------------------------------------------------------------
C --- Initial condition (currents) for circuit equations
C

      DO 17 L=1,NCEQUI
	  IF( L.LE.NEQUI ) THEN
           PJK(L)  = PFCEQW(L)
           PJKP(L) = PJK(L)
        ELSE
           PJK(L)  = PC(NCPFC+L-NEQUI)
           PJKP(L) = PJK(L)
        END IF
   17 CONTINUE



        write(fname,'(a,a)') path(1:kname),'currents.wr'
        open(1,file=fname,form='formatted')
       !open(1,file='currents.wr',form='formatted')
        write(1,*) NEQUI,NCEQUI
        write(1,*)(pjk(j),j=1,NCEQUI)
       close(1)

        write(fname,'(a,a)') path(1:kname),'res_mat.wr'
        open(1,file=fname,form='formatted')
       !open(1,file='res_mat.wr')
        write(1,*) (res(j),j=1,ncequi)
       close(1)

C
C.....  TOROIDAL CURRENTS OF PASSIVE CONDUCTOR STRUCTURES 
C
!        CALL PASCUR( NOUT, NTER, NEQUI, NFW, NBP, NVV,
!     *               PJK, fwcurr, bpcurr, vvcurr )
C
C********************************************************************

!         WRITE(NOUT,*) '********************************************'
!         WRITE(NOUT,*) ' NUMBER TIME-LEVEL  KSTEP =', KSTEP
!         WRITE(NOUT,*) '********************************************'

CCCC      include 'printb.inc'

          HELOUT = HELINP
C

!!!!!  WRITE(NTER,*) 'START  OF BASIC FREE BOUNDARY EQUILIBRIUM'

         call wrcoil(nc,ncpfc,rc,zc,pc,necon,wecon)

         ngrid=1

         call auto(rc,zc,pc,nc,nstep,ngrid,
     *                 rc1,zc1, rc2,zc2,
     *                 rc3,zc3, rc4,zc4,
     *                 ntype, necon, wecon )


!(((((((((((((((((((((((((((((((((((((((((
!	if(i_diag.eq.1)then
!	call  diag_params()
!	call diag_MATR(NC, NCPFC, nequi,
!     *                   RC,ZC,NECON,WECON,
!     * NLOO, RLOO, ZLOO ,
!     * NPRO, RPRO, ZPRO, FIPRO
!     *  )
!))))))))))))))))))))))))))))))))))))))))))


!((((((((((((((((((((((((((
!	call  write_diag()
!))))))))))))))))))))))))))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 2005   continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(kastr.eq.0 .AnD. i_eqdsk.eq.0) then 
        write(fname,'(a,a)') path(1:kname),'inpol.dat'
        open(1,file=fname,form='formatted')
          !open(1,file='inpol.dat')
             read(1,*) i_bsh
          close(1)    
         else
             i_bsh=1
         endif

        if(kastr.eq.0 ) then 
        write(fname,'(a,a)') path(1:kname),'durs_d.dat'
        open(1,file=fname,form='formatted')
        !open(1,file='durs_d.dat')
            read(1,*) n_tht,n_psi,igdf,nurs,keyctr,i_eqdsk,i_betp
            read(1,*) epsro,betplx,tokf,psax,b0,r0,rax,zax
            read(1,*) alf0,alf1,alf2,bet0,bet1,bet2
        close(1)
        endif


!!:::::::::::::::::__________________________________:::::::::::::::

        call noauto

        call rd_ppind

        call rd_prob( NPROb, RPROb, ZPROb,  FIPROb )

        call rd_loop( NLOOp, RLOOp, ZLOOp )


        write(fname,'(a,a)') path(1:kname),'currents.wr'
        open(1,file=fname,form='formatted')
       !open(1,file='currents.wr',form='formatted')
        read(1,*) nequi,ncequi
        read(1,*)(pjk(j),j=1,ncequi)
       close(1)

        write(fname,'(a,a)') path(1:kname),'res_mat.wr'
        open(1,file=fname,form='formatted')
       !open(1,file='res_mat.wr')
        read(1,*) (res(j),j=1,ncequi)
       close(1)

       !fr_cam=0.d0
       !do j=nequi+1,ncequi
       ! fr_cam=fr_cam+1.d0/res(j)
       !enddo
       ! fr_cam=1.d0/fr_cam

       do L=1,NCEQUI
           PJKP(L) = PJK(L)
       enddo


!(((((((((((((((((((((((((((((((((((((((((((
!	if(i_diag.eq.1)then 
!		call  diag_params()
!		call  read_diag()
!		call curr_to_dina(ncequi,pjk,tokf)
!	end if
!)))))))))))))))))))))))))))))))))))))))))))

CCCC   voltage for zero time-level
C
       DO i=1,nequi
          VOLK(i) = voltpf(i)*BBB
       END DO
       DO i=nequi+1,NCEQUI
          VOLK(i) = 0.0d0
       END DO



         platok = tokf
         kstep  = 0
	   k_step=0
	   ngav=keyctr



         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             ngav, k_step, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd)



c	call pau()

         ngrid=1


              nursb=nurs
              psicen=psax
              ngav1=0
              ftok=tokf


         call eq_0(  pjk,psip,ncequi,kstep,ngrid,
     *               alf0,alf1,alf2,bet0,bet1,bet2,
     *               betpol,betplx,zli3,
     *                  ngav1,
     *                 ftok, tokout, psiax, psiout, 
     *                 nursb,psi_bnd,alp_b,rax,zax,n_ctrl,b0,r0 )

         call rdexf(ncequi)

         call eq( pjk, psk, ncequi, kstep, ngrid,
     *                 alf0, alf1, alf2, bet0, bet1, bet2,
     *                 betpol, betplx, zli3,
     *                 ngav1,
     *                 ftok, tokout, psiax, psiout, 
     *                 nursb,psi_bnd,alp_b,rax,zax )


!!!!!!!!!!!!!!!!!!!!!!!!!!ttt!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !WRITE(NOUT,*) '    '
      !WRITE(NOUT,*) 'PRINT AFTER BASIC FREE BOUNDARY EQUILIBRIUM'

          CALL eq_par( z0cen, alp, alpnew, qcen, nctrl, numlim, up,
     *                   rm,    zm,  rx0,    zx0    )
C***********************************************************************

C --- FOR diagnostic

          RMAX0  = RM
          ZMAX0  = ZM
          ZCEN0  = Z0CEN
C     ---------------------
          BEOLD  = BETPOL
          ZLOLD  = ZLI3
C     ---------------------
          RAXPR  = RM
          ZAXPR  = ZM
          RMAOLD = RM
          ZMAOLD = ZM
          RXPPR  = RX0
          ZXPPR  = ZX0
          RXPOLD = RX0
          ZXPOLD = ZX0
C-----------------------------------------------------------------------
C --- Definition of input parameters from "basic" equilibrium

          BETPLX = BETPOL
          BETAP0 = BETPOL
          FTOK   = TOKOUT
          PSIAX  = PSIOUT
          HELINP = HELOUT
C ----------------------------------------------------------------------
C  For PSI_axis value time scenario (for NGAV1=2 or 3)

          time_sta = TSTART
          time_fin = TSTOP

          psax_sta = PSIOUT

C***********************************************************************

          PSIBOU = UP
          PSIDEL = PSIOUT - PSIBOU

CCCC      include 'printa.inc' ! ---  PRINTING

C***********************************************************************
C--- PRINTING AND WRITING OF NUMBERS AND POSITIONS OF LIMITER POINTS:

        !CALL PRTLIM( NOUT,  NTER, NGRA1 )
C***********************************************************************
C***********************************************************************
C--------------------- WRITING IN THE DISK FILES -----------------------
                 ! include 'wrt1.inc'
C-----------------------------------------------------------------------
       !WRITE(NOUT,*) '--------------------------------------------'
       !WRITE(NOUT,*) '      THE  END  OF  FILE:  "EVOL0.PRT"      '
       !WRITE(NOUT,*) '--------------------------------------------'

       !CLOSE(NOUT)

C***********************************************************************
CCCC   OPEN (NOUT, FILE='EVOL1.PRT', FORM='FORMATTED')

C --- If we need to compute the "basic" equilibrium only -> KSTOP=0

       IF( KSTOP.EQ.0 ) THEN
           GO TO 222
       END IF
C***********************************************************************
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


1	continue

         i_bsh  = -1
         i_eqdsk  = 0
         kstep  =  0
         keyctr =  0
         e_psi  =  0.d0


         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, kstep, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd)


         call get_par(psi_bnd)

         call bongri
         call psib_pla(pspl_av)
         call psib_ext(psex_av)

         psi0_bnd=pspl_av+psex_av


         ztok_n  = platok
         zpsim_n = psax


         return

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CCCC       WRITE(NOUT,*) ' NEW TIME-LEVEL  K = KSTEP =', KSTEP

 1111   CONTINUE
               !call test_urs

         keyctr = key_dmf
         !keyctr = 500

         call get_par(psi_bnd)
         call bongri
 !2241     continue
         call psib_pla(pspl_av)
         call psib_ext(psex_av)

         psi0_bnd=pspl_av+psex_av


         ztok_n  = platok
         zpsim_n = psax


!  100 KSTEP = KSTEP + 1       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      TIMEV = TIMEV + TSTEP   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       NGAV1 = 0

C********************************************************************
C  DEFINITION OF "KEYPRI" - PARAMETER FOR PRINTING

!       IF((KSTEP/NFRPR1)*NFRPR1 .EQ. KSTEP)  THEN
!           KEYPRI = 1
!       ELSE
           KEYPRI = 0
!       END IF


C====================================================================
C  DEFINITION OF INPUT EQUIL. PARAMETERS: BETPLX, PSIAX, FTOK, HELINP

        IF(NGAV1.EQ.0 .OR. NGAV1.EQ.2 .OR. NGAV1.EQ.4)
     *     BETPLX = BETPOL
        IF( NGAV1.EQ.0 .OR. NGAV1.EQ.1 )  THEN
           PSIAX  = PSIOUT
           HELINP = HELOUT
        END IF
        IF( NGAV1.EQ.2 .OR. NGAV1.EQ.3 )  THEN
           FTOK   = TOKOUT
           HELINP = HELOUT
        END IF
        IF( NGAV1.EQ.4 .OR. NGAV1.EQ.5 )  THEN
           FTOK   = TOKOUT
           PSIAX  = PSIOUT
        END IF
C
C***********************************************************************
C  Computing of "VOLKP1"  voltage  for CIRCUIT EQUATION 
C  for new time level KSTEP
C

         DO i=1,nequi
          VOLKP1(i) = voltpf(i)*BBB
         END DO
         DO i=nequi+1,NCEQUI
            VOLKP1(i) = 0.0d0
         END DO
C-----------------------------------------------------------------------
CCCC        include 'printb.inc' !Printing
C***********************************************************************
              ! call test_urs

         call bongri
         call psib_pla(pspl_av)
         call psib_ext(psex_av)
         call get_flfi(flx_fi)

         psi0_bnd=pspl_av+psex_av
         psi0_ax=pspl_av+psex_av+psax
           ztok=platok
           zpsim=psax
           i_bsh=-1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

               !call test_urs

         n_dmf=3
        if(keyctr.eq.0) n_dmf=1

      do it_dmf=1,n_dmf !!!   it_dmf <<< mag.field diffusion iter. loop


	if(kpr.eq.1)print *,' it_dfm==',it_dmf

        if(keyctr.ne.0) then
         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, kstep, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd )
        endif

c	print *,' psax keyctr platok=',psax,keyctr,platok

c	read (*,*)
               !call test_urs

           call wrb
               !call test_urs

           ftok=platok


        !if(key_fixbon.eq.1) exit


       IF( KSTEP.EQ.1 ) THEN

          NGRID  = 1

          DO L=1,NCEQUI
           PSKP1(L) = PSK(L)
          enddo

          NREG = 0
          NLES = 0
               !call test_urs

          CALL EVSLV( NLES,   NREG,  TSTEP,  TSTEP, SIGM,
     *                NCEQUI, VOLK,  VOLKP1, RES,
     *                PSK,    PSKP1, PJK,    PJKP1, PJKP,
     *                NOUT,   NTER,  KEYPRI, EREVE )

               !call test_urs
       END IF    !!! ===>  FOR  IF( KSTEP.EQ.1 )
C***********************************************************************
       IF(KSTEP.NE.1) THEN

C----------------------------------------------------------------
C  The initial approximation for the case of closed-loop evolution
C
           DO L=1,NCEQUI
             PJKP(L) = PJK(L)
          enddo
C----------------------------------------------------------------
C
           NREG = 0
           NLES = 1
           
               !call test_urs

           CALL EVSLV( NLES,   NREG, TSTEPR, TSTEP, SIGM,
     *                 NCEQUI, VOLK, VOLKP1, RES,
     *                 PSKM1,  PSK,  PJK,    PJKP1, PJKP,
     *                 NOUT,   NTER, KEYPRI, EREVE )


       END IF   !!! ===>  FOR  IF( KSTEP.NE.1 )
C***********************************************************************
               !call test_urs
         CALL EQ_AX( pjkp1, PSkp1, NCequi,   KSTEP, NGRID,
     *               ALF0, ALF1, ALF2, BET0, BET1, BET2,
     *               BETPOL,   BETPLX,   ZLI3,
     *                   NGAV1,
     *               FTOK,     TOKOUT, PSIAX, PSIOUT, 
     *               ENELS,    ERPS,
     *               psi_bnd,alp_b,rax,zax,0 )




!(((((((((((((((((((((((((((((((((((((((((((
!	if(i_diag.eq.1)then
!	call psi_diag()
!	call curr_to_dina(ncequi,pjkp1,ftok)
!	call loopflux()
!	call probefield()
!	end if
!)))))))))))))))))))))))))))))))))))))))))))


         CALL eq_par( z0cen, alp, alpnew, qcen, nctrl, numlim, up,
     *                rm,    zm,  rx0,    zx0   )

C***********************************************************************
C ---  Start of the iteration loop 
C***********************************************************************
 
             KNEL  = 0
 1000        KNEL = KNEL + 1


          DO L=1,NCEQUI
             PSKP(L) = PSKP1(L)
             PJKP(L) = PJKP1(L)
          ENDDO

          NREG = 0
          NLES = 2

          CALL EVSLV( NLES,   NREG,  TSTEP,  TSTEP, SIGM,
     *                NCEQUI, VOLK,  VOLKP1, RES,
     *                PSK,    PSKP1, PJK,    PJKP1, PJKP,
     *                NOUT,   NTER,  KEYPRI, EREVE )




          CALL  DIFFER( PJKP, PJKP1, NCEQUI, ERRCU1, ERRCU2,
     *                  CURMAX, CURMIN, NOUT, NTER )

C         SGMCUR = 1.00D0
          SGMCUR = 0.75D0

          DO L=1,NCEQUI
             PJKP1(L) = SGMCUR*PJKP1(L) + (1.0D0 - SGMCUR)*PJKP(L)
          END DO
C-----------------------------------------------------------------------

          CALL EQ_AX( pjkp1, PSkp1, NCequi,   KSTEP, NGRID,
     *                ALF0, ALF1, ALF2, BET0, BET1, BET2,
     *                BETPOL,   BETPLX,   ZLI3,
     *                    NGAV1,
     *                FTOK,     TOKOUT, PSIAX, PSIOUT, 
     *                ENELS,    ERPS,
     *                psi_bnd,alp_b,rax,zax,0 )

            write(*,*) 'stepon:EQ_AX done, erru=',ERPS 


!(((((((((((((((((((((((((((((((((((((((((
!	if(i_diag.eq.1)then
!	call psi_diag()
!	call curr_to_dina(ncequi,pjkp1,ftok)
!	call loopflux()
!	call probefield()
!	end if
!))))))))))))))))))))))))))))))))))))))))) 


          CALL eq_par( z0cen, alp, alpnew, qcen, nctrl, numlim, up,
     *                 rm,    zm,  rx0,    zx0    )

            !write(*,*) 'stepon:eq_par done, erru=',ERPS 

          IF(NGAV1.EQ.2 .OR. NGAV1.EQ.3)  FTOK = TOKOUT
          IF(NGAV1.EQ.4 .OR. NGAV1.EQ.5)  FTOK = TOKOUT
 
          !DELBET = BETPOL - BEOLD
          !DELZLI = ZLI3   - ZLOLD
          !DELRMB = RM     - RMAOLD
          !DELZMB = ZM     - ZMAOLD
          !DELRXB = RX0    - RXPOLD
          !DELZXB = ZX0    - ZXPOLD
          !DELRMA = RM     - RAXPR
          !DELZMA = ZM     - ZAXPR
          !DELRXP = RX0    - RXPPR
          !DELZXP = ZX0    - ZXPPR
              !dvz=dabs( DELZMB/(DELZMA+1.d-10) )




!          CALL  DIFGEO( RM, ZM, RAXPR, ZAXPR, RMAOLD, ZMAOLD,
!     *                  DELAXA, DELAXR, NOUT, NTER )

            !write(*,*) 'stepon:DIFGEO, erru=',ERPS 

          !BEOLD  = BETPOL
          !ZLOLD  = ZLI3
          !RMAOLD = RM
          !ZMAOLD = ZM
          !RXPOLD = RX0
          !ZXPOLD = ZX0
C
!          CALL  DIFFER( PSKP, PSKP1, NCEQUI, ERRPS1, ERRPS2,
!    *                  PSIMAX, PSIMIN, NOUT, NTER )
!          CALL  DIFTIM( PSK, PSKP, PSKP1, NCEQUI, DELPS1, DELPS2,
!     *                  DELMAX, DELMIN, NOUT, NTER )
 
            !write(*,*) 'stepon:DIFTIM, erru=',ERPS 

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  -----  CONTROL OF EXIT FROM ITERATION LOOP  ------
C         n_pr = 5
C	    a_print(1) = KNEL
C         a_print(2) = ERPS
C	    a_print(3) = DELAXR
C	    a_print(4) = DELZMA
C	    a_print(5) = DELZMB
C	    num = 30
C	    apr = 'KNEL ERPS DELAXR DELZst DELZit'
C	    call out42(n_pr,a_print,num,apr)

C 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000
            write(*,*) 'stepon:erru',ERPS 

               !IF( ERPS .LT. ENELS .AnD. dvz .LT. 5.d-3 ) THEN
               IF( ERPS .LT. ENELS  ) THEN

               !IF( dvz .LT. 5.d-3 ) THEN
                   GO TO 200
               END IF
               IF( KNEL .GE. KNELS ) THEN
                   KEYPRI = 1
            write(*,*) 'spider,sstepon: no covergence ' 
            write(*,*) 'KNEL .GE. KNELS  ',KNEL,KNELS 
            pause 'pause'            
                   GO TO 200
               ELSE
                   GO TO 1000
               END IF

C 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000
 
  200                          CONTINUE

        !call get_par(psi_bnd)

        diftok=dabs(platok-ztok)/(dabs(platok-ztok_n)+1.d-8)
        difpsi=dabs(psax-zpsim)/(dabs(psax-zpsim_n)+1.d-8)


c	print *,' platok ztok=',platok,ztok
c	print *,' psax zpsim=',psax,zpsim

c        diftok=dabs(platok-ztok)/(dabs(platok)+1.d-4)
c        difpsi=dabs(psax-zpsim)/(dabs(psax)+1.d-4)

		
        !write(*,*)'***it_dmf',it_dmf !
        !write(*,*)'diftok=',diftok  ! 
        !write(*,*)'difpsi=',difpsi  ! 

       !if(diftok.lt.3.5d-3 .AnD. it_dmf.gt.2) then
      !if(diftok.lt.5.0d-3 .AnD. difpsi.lt.5.0d-3 .AnD.it_dmf.gt.0) then
       if(diftok.lt.5.0d-3 .AnD. it_dmf.gt.0) then

         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, kstep, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd )
         exit

         endif

         ztok  = platok
         zpsim = psax

        enddo ! it_dmf <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!print *,'stepon:eqb'

             ztok_n=platok
             zpsim_n=psax
	!print *,'ztok_n',ztok_n
	!print *,'zpsim_n',zpsim_n
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C --- Printing of iteration loop accuracy
C
C      IF( KEYPRI.EQ.1 ) THEN
C          WRITE(NOUT,*) '_______________________________________'
C          WRITE(NOUT,1915) KSTEP, KNEL, TIMEV
C          WRITE(NOUT,*) 'ERPS   =', ERPS
C          WRITE(NOUT,*) 'ERRCU1 =', ERRCU1,' ERRCU2 =', ERRCU2
C...      WRITE(NOUT,*) 'CURMAX =', CURMAX,' CURMIN =', CURMIN
C          WRITE(NOUT,*) 'ERRPS1 =', ERRPS1,' ERRPS2 =', ERRPS2
C...      WRITE(NOUT,*) 'PSIMAX =', PSIMAX,' PSIMIN =', PSIMIN
C          WRITE(NOUT,*) 'DELPS1 =', DELPS1,' DELPS2 =', DELPS2
C...      WRITE(NOUT,*) 'DELMAX =', DELMAX,' DELMIN =', DELMIN
C          WRITE(NOUT,*) 'DELAXA =', DELAXA,' DELAXR =', DELAXR
C      END IF
C
C***********************************************************************
C   POLOIDAL mag. field companents Bp_r, Bp_z  and
C   POLOIDAL mag. field projection on direction FIPRO
C   COMPUTE FOR GIVEN "PF_PROBE" POINTS:
C
c       CALL  BP_PROB( NOUT,   NTER,   KEYPRI,
c     *                NPRO,   RPRO,   ZPRO,   FIPRO,
c     *                BRPRO2, BZPRO2, BPCOM2 )
C***********************************************************************
C   Compute full poloidal flux in given "loop" points
C
c       CALL  FL_LOO( NOUT,   NTER,   KEYPRI,
c     *               NLOO,   RLOO,   ZLOO,
c     *               PSLOO2 )
C***********************************************************************
C
C.....  TOROIDAL CURRENTS IN PASSIVE CONDUCTOR STRUCTURES 
C
!        CALL PASCUR( NOUT, NTER, NEQUI, NFW, NBP, NVV,
!     *               PJKP1, fwcurr, bpcurr, vvcurr )
	!print *,'stepon:pascur'
C-----------------------------------------------------------------------
C --- Printing
C 
          PSIBOU = UP
          PSIDEL = PSIOUT - PSIBOU


C
CCCC          include 'printa.inc'
C
C***********************************************************************
C--- TIME LEVEL PARAMETERS ARE WRITTEN IN  DISK  FILES
C
         !!! include 'wrt2.inc'
C***********************************************************************
  222        CONTINUE
C
             ERRCU1 = 0.D0
             ERRCU2 = 0.D0
             ERRPS1 = 0.D0
             ERRPS2 = 0.D0
             DELPS1 = 0.D0
             DELPS2 = 0.D0
             EREVE  = 0.D0
             ERPS   = 0.D0
c
             RAXPR  = RM
             ZAXPR  = ZM
            ! RXPPR  = RX0
            ! ZXPPR  = ZX0
            KSTEPR  = KSTEP
            TSTEPR  = TSTEP
C
          DO 293 L=1,NPFC
             PFCUR1(L) = PFCUR2(L)
             PFCW1(L)  = PFCW2(L)
             PFCD1(L)  = PFCD2(L)
  293     CONTINUE

          do i=1,nequi
             d_pf_mat(i) = PJKP1(i)*1.0d6  !!!  in [A]
          enddo
 
             camtok=0.d0
         do i=nequi+1,ncequi
             d_tcam_mat(i-nequi) = PJKP1(i)*1.0d6  !!!  in [A]
             camtok=camtok+PJKP1(i)
        enddo


          DO 260 L=1,NCEQUI
             PJK(L)   = PJKP1(L)
             VOLK(L)  = VOLKP1(L)
             PSKM1(L) = PSK(L)
             PSK(L)   = PSKP1(L)
  260     CONTINUE
C
C***********************************************************************
        if(kstep.lt.nstep_p)then
           i_tim=kstep

         time_t(i_tim)=time
         torcur(i_tim)=platok
         rm_t(i_tim)=rax
         zm_t(i_tim)=zax
         rxp_t(i_tim)=rxpnt
         zxp_t(i_tim)=zxpnt
         betpol_t(i_tim)=betpol
         bettor_t(i_tim)=betful
         psim_t(i_tim)=psax
         psib_t(i_tim)=psbo
        endif
C-----------------------------------------------------------------------
C For writing for "motion picture"
C
C       psiplb(kstep)=pspl_av
C       psiexb(kstep)=psex_av
C       psimag(kstep)=psi0_ax
C       flu_tor(kstep)=flx_fi
C
C       if(kstep.gt.0 .AnD. kstep.le.2000) then
C       kskw=2
C       if(kstep/kskw*kskw .eq. kstep) then
C         numwr=numwr+1
C         call wrdfmv(numwr,timev)
C         call wrdump(numwr,timev,kstep,psiplb,psiexb,psimag,flu_tor)
C       endif
C       endif
C 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
!!!!                   GO TO 100  
C 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100
C-----------------------------------------------------------------------
  103  FORMAT(2X,8E12.5)
 1915  FORMAT(3X,'LEVEL K =',I3,2X,'ITERATION KNEL =',I3,2X,
     *        'TIME(K) =',E12.5)
C********************************


      RETURN
      END








