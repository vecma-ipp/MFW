!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
       SUBROUTINE  B_STEPON( KLUCH, k_auto, nstep, dt, time,
     *                       r_ax,z_ax ,key_dmf,dpsdt)

         use durs_d_modul       

C---------------------------------------------------------------
       include 'double.inc'
       INCLUDE 'comtim.inc'
C-----------------------------------------------------------------------
C
       common /nostep/ kstep
!                  save epsro,platok,psex_bnd,
!     *                 eqdfn,betplx,tokf,psax,b0,r0,
!     *                 alf0,alf1,alf2,bet0,bet1,bet2,rax,zax,
!     *                 nurs,igdf,n_psi,n_tht,i_bsh,keyctr,i_betp,i_eqdsk
   
!                  save platok,psex_bnd,eqdfn,i_bsh
                  real*8 platok,psex_bnd
                  integer i_bsh
   
	 common /c_kpr/ kpr
       common/com_flag/kastr
       
        common/psi_test/ psi_ext_bon(300)

       character*40 eqdfn
!       save numwr
       integer numwr

         kstep = nstep
         
       IF( KLUCH .NE. 0 ) GO TO 1111

 1149     format(a40)
       numwr  = 0

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

!        write(fname,'(a,a)') path(1:kname),'durs_d.dat'
!        open(1,file=fname,form='formatted')
!          !open(1,file='durs_d.dat')
!              write(1,*) n_tht,n_psi,igdf,nurs,keyctr,i_eqdsk,i_betp
!              write(1,*) epsro,betplx,tokf,psax,b0,r0,rax,zax
!              write(1,*) alf0,alf1,alf2,bet0,bet1,bet2
!          close(1)

         call put_Ipl(tokf)


 1150   continue


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 2005   continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(kastr.eq.0) then 
        write(fname,'(a,a)') path(1:kname),'inpol.dat'
        open(1,file=fname,form='formatted')
          !open(1,file='inpol.dat')
             read(1,*) i_bsh
          close(1)    
         else
             i_bsh=1
         endif


!        write(fname,'(a,a)') path(1:kname),'durs_d.dat'
!        open(1,file=fname,form='formatted')
!        !open(1,file='durs_d.dat')
!            read(1,*) n_tht,n_psi,igdf,nurs,keyctr,i_eqdsk,i_betp
!            read(1,*) epsro,betplx,tokf,psax,b0,r0,rax,zax
!            read(1,*) alf0,alf1,alf2,bet0,bet1,bet2
!        close(1)

         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, nstep, tokf, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd)

         platok = tokf
         call bongri
         call psib_pla(pspl_av)
         psex_bnd=-pspl_av

       return

1111   CONTINUE

          keyctr=key_dmf
         call get_Ipl(platok)
         call bongri
         call psib_pla(pspl_av)
         call get_flfi(flfi_m)
         !call cur_avg
         !psex_bnd=-pspl_av
         psi0_bnd=psex_bnd+pspl_av
         psex_bnd=psex_bnd+dpsdt*dt
         !psex_bnd=psi_ext_bon(nstep)
         psi_bnd=psex_bnd

!         call put_psib0(psi0_bnd)   !?
!         psi_eav=psex_av

         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, nstep, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd)



       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
       SUBROUTINE  f_STEPON( KLUCH, k_auto, nstep, dt, time,
     *                  voltpf, d_pf_mat,d_tcam_mat,r_ax,z_ax ,key_dmf)

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
       INCLUDE 'comtim.inc'
C-----------------------------------------------------------------------
C
	 common/comeqg/  ncequi
       common /nostep/ kstep
       
!       common /comsta/ epsro,platok,
!     *                 eqdfn,betplx,tokf,psax,b0,r0,
!     *                 alf0,alf1,alf2,bet0,bet1,bet2,rax,zax,
!     *                 nurs,igdf,n_psi,n_tht,i_bsh,keyctr,i_betp,i_eqdsk

       common /comsta/ platok,eqdfn,i_bsh
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
       common/comst2/ PSK(NJLIM), PSKP1(NJLIM),PSKP(NJLIM),PSKM1(NJLIM)
       include 'parloo.inc'
       common/comloo/ rloop(nloopp),zloop(nloopp),
     &           rprob(nprobp),zprob(nprobp),fiprob(nprobp),nloop,nprob

       !DIMENSION  cp_com_0(n_cp_m)
C***********************************************************************

!       DIMENSION  psiplb(nstep_p),psiexb(nstep_p),
!     *            flu_tor(nstep_p)  
       real*8 errarr(10)

C***********************************************************************

       real*8 voltpf(*)
       real*8 d_pf_mat(*)
       real*8 d_tcam_mat(*)

C***********************************************************************
       common /key_for_pet/ key_fixbon

	 common /c_kpr/ kpr

	 real*4       a_print(200)
	 character*30 apr
       character*40 eqdfn
!       save numwr
!       save psi_eav_n
       integer numwr
       real*8 psi_eav_n

         kstep = nstep
         timev = time
         tstep = dt


	    !n_pr = 3
	    !a_print(1) = nstep
	    !a_print(2) = time
          !a_print(3) = dt
	    !num = 30
	    !apr = 'STEPON ENTER nstep time dt ='
	    !call out42(n_pr,a_print,num,apr)

!((((((((((((((((((((((
!     	i_diag=1
!))))))))))))))))))))))

         pi  = 3.14159265359d0
         BBB  = 1.d0 / (2.d0*pi)

          NOUT  = 17
          NTER  = 6
          NINFW = 7
          NINEV = 10
          ngra1 = 14
          ngra2 = 15

       SIGM   = 1.0d0  !0.d0

       TSTEPR = TSTEP
       TSTART = 0.00d0

       TSTOP  = 20.0d0
       KSTOP  = 1000

       ENELS  = 5.0d-6
       KNELS  = 150

       NFRPR1 = 5
       NFRWR1 = 0
C=============================================

       KSTEPR = KSTEP
       KEYPRI = 1

       IF( KLUCH .NE. 0 ) GO TO 1111

 1149     format(a40)
C
C
       !OPEN( NOUT, FILE='EVOL0.PRT', FORM='FORMATTED' )
!       write(nout,*) '***********************************************'
!       write(nout,*) '!               OUTPUT FILE                   !'
!       write(nout,*) '!                   OF                        !'
!       write(nout,*) '!  the PET code (Plasma Evolution in Tokamak) !'
!       write(nout,*) '! ------------------------------------------- !'
!       write(nout,*) '!  free boundary tokamak equilibrium problem  !'
!       write(nout,*) '!                    and                      !'
!       write(nout,*) '!             circuit equations               !'
!       write(nout,*) '***********************************************'
C
       numwr  = 0
C--------------------------------------------------------------------
C
C======ITER, SOF
C

C-----------------------------------------------------------------
C --- INPUT OF "BASIC" EQULIBRIUM PARAMETERS FROM FILE "durs.dat"
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(k_auto.eq.0) go to 2005 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        write(fname,'(a,a)') path(1:kname),'durs.dat'
        open(1,file=fname,form='formatted')
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

!        write(fname,'(a,a)') path(1:kname),'durs_d.dat'
!        open(1,file=fname,form='formatted')
!          !open(1,file='durs_d.dat')
!              write(1,*) n_tht,n_psi,igdf,nurs,keyctr,i_eqdsk,i_betp
!              write(1,*) epsro,betplx,tokf,psax,b0,r0,rax,zax
!              write(1,*) alf0,alf1,alf2,bet0,bet1,bet2
!          close(1)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C***********************************************************************
C--- INPUT OF POSITIONS OF "PF_PROBE" POINTS:
C
        CALL PROPNT( NOUT, NTER, NINFW, NGRA1,
     *               NPROb, RPROb, ZPROb, FIPROb )
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

         call wrcoil(nc,ncpfc,rc,zc,pc,necon,wecon)

C
C.....  TOROIDAL CURRENTS OF PASSIVE CONDUCTOR STRUCTURES 
C
!        CALL PASCUR( NOUT, NTER, NEQUI, NFW, NBP, NVV,
!     *               PJK, fwcurr, bpcurr, vvcurr )
C
C********************************************************************


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 2005   continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!        write(fname,'(a,a)') path(1:kname),'durs_d.dat'
!        open(1,file=fname,form='formatted')
!        !open(1,file='durs_d.dat')
!            read(1,*) n_tht,n_psi,igdf,nurs,keyctr,i_eqdsk,i_betp
!            read(1,*) epsro,betplx,tokf,psax,b0,r0,rax,zax
!            read(1,*) alf0,alf1,alf2,bet0,bet1,bet2
!        close(1)

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

         call rdcoil(nc,ncpfc,rc,zc,pc,necon,wecon)

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

       DO i=1,NEQUI
          VOLK(i) = voltpf(i)*BBB
       END DO
       DO i=NEQUI+1,NCEQUI
          VOLK(i) = 0.0d0
       END DO


         kstep = 0
         nursb=nurs
         platok=tokf

         i_eqdsk=0
         i_bsh=-1

         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, nstep, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd )




         call eqa_in(alf0,alf1,alf2,bet0,bet1,bet2,nursb,
     *            keyctr,igdf,kstep,platok,  
     *            pjk,ncequi, b0,r0,
     *            rloop,zloop,nloop, rprob,zprob,nprob,
     *                   necon,wecon,ntype )



!         WRITE(NOUT,*) '********************************************'
!         WRITE(NOUT,*) ' NUMBER TIME-LEVEL  KSTEP =', KSTEP
!         WRITE(NOUT,*) '********************************************'


              if(kpr.eq.1) then
          WRITE(*,*) 'START  OF BASIC FREE BOUNDARY EQUILIBRIUM'
              endif
         call eqa(
     *            keyctr,igdf,kstep,platok, psax,i_betp,betplx, 
     *            rax,zax, rxpnt,zxpnt, psbo, psdel,
     *            rc,zc,nc, pjk,ncequi, psip,
     *            rloop,zloop,nloop, rprob,zprob,nprob,
     *             zli3,betpol,betful,    
     *                     necon,wecon,ntype , nflag, errarr)
            call f_wrd
       !pause 'pause '

            call renet
            call f_bndmat(rc,zc,nc,rloop,zloop,nloop,rprob,zprob,nprob)
            call f_wrd

         call eqa(
     *            keyctr,igdf,kstep,platok, psax,i_betp,betplx, 
     *            rax,zax, rxpnt,zxpnt, psbo, psdel,
     *            rc,zc,nc, pjk,ncequi, psip,
     *            rloop,zloop,nloop, rprob,zprob,nprob,
     *             zli3,betpol,betful,    
     *                     necon,wecon,ntype , nflag, errarr)

       !pause 'pause '


C------------------------------------------------------------------
!          CALL TRAVEC (PSIP, PSK, NC, NCPFC, NEQUI, NECON, WECON)
        do k=1,ncequi
         psk(k)=psip(k)
        enddo
C------------------------------------------------------------------
!!test psip
!         call flux_g(psip,rc,zc,nc)
!         CALL TRAVEC (PSIP, PSK, NC, NCPFC, NEQUI, NECON, WECON)
!!test psip


!((((((((((((((((((((((((((((((((((((((((((
!	if(i_diag.eq.1)then
!
!!	call psi_diag()
!
!      call ext_diag
!
!	call curr_to_dina(ncequi,pjk,tokf)
!	call loopflux()
!	call probefield()
!	end if
!))))))))))))))))))))))))))))))))))))))))))

         istep=nstep
         call bongri
         call f_psib_pla(pspl_av)
         call f_psib_ext(psex_av)

         psi0_bnd=pspl_av+psex_av
         call put_psib0(psi0_bnd)
        call wr_step(numwr,time,istep)

!         return
          go to 757
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


1111   CONTINUE

          keyctr=key_dmf
          !keyctr=0
         call bongri
         call f_psib_pla(pspl_av)
         call f_psib_ext(psex_av)

         psi0_bnd=pspl_av+psex_av
         call put_psib0(psi0_bnd)
         psi_eav=psex_av
                           dpsdt=(psi_eav-psi_eav_n)/dt
         psi_eav_n=psi_eav
C***********************************************************************
C  Computing of "VOLKP1"  voltage  for CIRCUIT EQUATION 
C  for new time level KSTEP
C
         DO i=1,NEQUI
          VOLKP1(i) = voltpf(i)*BBB
         END DO
         DO i=NEQUI+1,NCEQUI
            VOLKP1(i) = 0.0d0
         END DO
C-----------------------------------------------------------------------

!!!!!!! preparations for currrents - equilibrium iteration loop

          !n_pr = 3
	    !a_print(1) = time
	    !a_print(2) = dt
	    !a_print(3) = keypri
	    !num = 30
	    !apr = '-time dt keypri='
	    !call out42(n_pr,a_print,num,apr)

        !write(17,*) 'before kstep.eq.1'

       IF( KSTEP.EQ.1 ) THEN

          do L=1,NCEQUI
            PSKP1(L) = PSK(L)
          enddo

          NREG = 0
          NLES = 0

          CALL EVSLV( NLES,   NREG,  TSTEP,  TSTEP, SIGM,
     *                NCEQUI, VOLK,  VOLKP1, RES,
     *                PSK,    PSKP1, PJK,    PJKP1, PJKP,
     *                NOUT,   NTER,  KEYPRI, EREVE )

       END IF    !!! ===>  FOR  IF( KSTEP.EQ.1 )
C***********************************************************************
       IF(KSTEP.NE.1) THEN

C----------------------------------------------------------------
C  The initial approximation for the case of closed-loop evolution
C
           do L=1,NCEQUI
             PJKP(L) = PJK(L)
          enddo
C----------------------------------------------------------------
C
           NREG = 0
           NLES = 1

           CALL EVSLV( NLES,   NREG, TSTEPR, TSTEP, SIGM,
     *                 NCEQUI, VOLK, VOLKP1, RES,
!     *                 PSK,  PSK,  PJK,    PJKP1, PJKP,
     *                 PSKM1,  PSK,  PJK,    PJKP1, PJKP,
     *                 NOUT,   NTER, KEYPRI, EREVE )

       END IF   !!! ===>  FOR  IF( KSTEP.NE.1 )
C***********************************************************************

         call eqa_ax( dt,time,
     *                  keyctr,igdf,nstep,platok, psax,i_betp,betplx, 
     *                  rax,zax, rxpnt,zxpnt, psbo, psdel,
     *                  rc,zc,nc, pjkp1,ncequi, psip,
     *                  rloop,zloop,nloop, rprob,zprob,nprob,
     *                   zli3,betpol,betful,     
     *                     necon,wecon,ntype , nflag, errarr)
            !call f_wrd

C------------------------------------------------------------------
          !CALL TRAVEC (PSIP, PSKP1, NC, NCPFC, NEQUI, NECON, WECON)
        do k=1,ncequi
         pskp1(k)=psip(k)
        enddo

!((((((((((((((((((((((((((((((((((((((((((((((
!	if(i_diag.eq.1)then
!!	call psi_diag()
!      call ext_diag
!	call curr_to_dina(ncequi,pjkp1,ftok)
!	call loopflux()
!	call probefield()
!	end if
!)))))))))))))))))))))))))))))))))))))))))))))))

C------------------------------------------------------------------

!!!!!!! currrents - equilibrium iteration loop begining
 
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

!          CALL  DIFFER( PJKP, PJKP1, NCEQUI, ERRCU1, ERRCU2,
!     *                  CURMAX, CURMIN, NOUT, NTER )

          !SGMCUR = 1.00D0
          !SGMCUR = 0.75D0
          SGMCUR = 0.5D0

          DO L=1,NCEQUI
             PJKP1(L) = SGMCUR*PJKP1(L) + (1.0D0 - SGMCUR)*PJKP(L)
          END DO
C-----------------------------------------------------------------------

         call eqa_ax( dt,time,
     *                  keyctr,igdf,nstep,platok, psax,i_betp,betplx, 
     *                  rax,zax, rxpnt,zxpnt, psbo, psdel,
     *                  rc,zc,nc, pjkp1,ncequi, psip,
     *                  rloop,zloop,nloop, rprob,zprob,nprob,
     *                   zli3,betpol,betful,     
     *                     necon,wecon,ntype , nflag, errarr)

            !call f_wrd

        !write(17,*) 'after eqa_ax'
C------------------------------------------------------------------
          !CALL TRAVEC (PSIP, PSKP1, NC, NCPFC, NEQUI, NECON, WECON)
        do k=1,ncequi
         pskp1(k)=psip(k)
         !pskp1(k)=0.5d0*(psip(k)+pskp1(k))
        enddo
C------------------------------------------------------------------

!((((((((((((((((((((((((((((((((((((((((((((
!	if(i_diag.eq.1)then
!!	call psi_diag()
!      call ext_diag
!	call curr_to_dina(ncequi,pjkp1,ftok)
!	call loopflux()
!	call probefield()
!	end if
!)))))))))))))))))))))))))))))))))))))))))))))

           erro=errarr(1)

          if(kpr.eq.1) then
             write(*,*) ' '
             write(*,*) ' '
             write(*,*) 'stepon:erro',erro
             write(*,*) ' '
             write(*,*) ' '
          endif
             !write(17,*) ' '
             !write(17,*) ' '
             !write(17,*) 'stepon:erro',erro
             !write(17,*) ' '
             !write(17,*) ' '
               IF( erro .LT. ENELS ) THEN
                   GO TO 200
               END IF

               IF( KNEL .GE. KNELS ) THEN

          if(kpr.eq.1) then
             write(*,*) ' '
             write(*,*) 'stepon:limit of iterations is exceded'
             write(*,*) 'knel=',knel
             write(*,*) ' '
          endif
             !write(17,*) ' '
             !write(17,*) 'stepon:limit of iterations is exceded'
             !write(17,*) 'knel=',knel
             !write(17,*) ' '

                   GO TO 200
               END IF

!!!!!!! currrents - equilibrium iteration loop finish

          go to 1000

C 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000
 
  200            CONTINUE
            call f_wrd

          do i=1,NEQUI
             d_pf_mat(i) = PJKP1(i)*1.0d6  !!!  in [A]
          end do

          do i=NEQUI+1,NCEQUI
             d_tcam_mat(i-NEQUI) = PJKP1(i)*1.0d6  !!!  in [A]
          end do

          do L=1,NCEQUI
             PJK(L)   = PJKP1(L)
             VOLK(L)  = VOLKP1(L)
             PSKM1(L) = PSK(L)
             PSK(L)   = PSKP1(L)
          enddo

         numwr=numwr+1
        call wr_step(numwr,time,kstep)


!!!!!!!!!!time dependent arrays initialization
 757        continue

           i_tim=kstep+1

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

!!!!!!!!!!time dependent arrays initialization





      RETURN
      END









