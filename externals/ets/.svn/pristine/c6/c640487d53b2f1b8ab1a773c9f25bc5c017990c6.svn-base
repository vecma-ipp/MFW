       SUBROUTINE  cf_init( k_auto, nstep, dt, time,
     *                     voltpf, d_pf_mat,d_tcam_mat )

         use durs_d_modul       

         include 'double.inc'
         !include 'param.inc'
         include 'prm.inc'
         include 'comevl.inc'
         include 'parcur.inc'

c   -----------------------------

	 common/comeqg/  ncequi
	 
!       common /comsta/ epsro,platok,
!     *                 eqdfn,betplx,tokf,psax,b0,r0,
!     *                 alf0,alf1,alf2,bet0,bet1,bet2,rax,zax,
!     *                 nurs,igdf,n_psi,n_tht,i_bsh,keyctr,i_betp,i_eqdsk

       common /comsta/ platok,eqdfn,i_bsh
       
 	 common /c_kpr/ kpr
       common/com_flag/ kastr
       common/com_xwx/ kxwx
       common/com_snf/ksnf
C-----------------------------------------------------------------------

       DIMENSION  RC(NCLIM),  ZC(NCLIM),  PC(NCLIM), PSIP(NCLIM),
     *            VC(NCLIM),  HC(NCLIM)

       DIMENSION  RC1(NCLIM), ZC1(NCLIM), RC2(NCLIM), ZC2(NCLIM),
     *            RC3(NCLIM), ZC3(NCLIM), RC4(NCLIM), ZC4(NCLIM)

       INTEGER    NECON(NILIM), NTYPE(NCLIM)
       DIMENSION  WECON(NILIM)

       common/comst0/ RES(NJLIM),  VOLK(NJLIM),  VOLKP1(NJLIM)

       common/comst1/ PJK(NJLIM),PJKP1(NJLIM),PJKP(NJLIM),PJKD(NJLIM)
       common/comst2/ PSK(NJLIM), PSKP1(NJLIM),PSKP(NJLIM),PSKM1(NJLIM)
       include 'parloo.inc'
       common/comloo/ rloop(nloopp),zloop(nloopp),
     &           rprob(nprobp),zprob(nprobp),fiprob(nprobp),nloop,nprob

       DIMENSION   ccurx(nclim),ccury(nclim)
C***********************************************************************

       real*8 voltpf(*)
       real*8 d_pf_mat(*)
       real*8 d_tcam_mat(*)

C***********************************************************************

       character*40 eqdfn
 1149     format(a40)
          kstep = nstep

          ninf=1
          
          kxwx=2  ! = 0 - without prescrib. x-points;
                  ! = 1 - with one prescrib. x-point;
                  ! = 2 - with two prescrib. x-points;
                                
          ksnf=0  ! if ksnf=1,then kxwx must be =1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(k_auto.eq.0) go to 2005 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(kastr.eq.1) go to 1150 

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


 1150   continue

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
        write(*,*) '**'
       DO L=1,NCEQUI
	  IF( L.LE.NEQUI ) THEN
           PJK(L)  = PFCEQW(L)
        ELSE
           PJK(L)  = PC(NCPFC+L-NEQUI)
        END IF
       enddo

        write(fname,'(a,a)') path(1:kname),'currents.wr'
        open(1,file=fname,form='formatted')
       !open(1,file='currents.wr',form='formatted')
        write(1,*) NEQUI,NCEQUI
        write(1,*)(pjk(j),j=1,NCEQUI)
       close(1)

        write(fname,'(a,a)') path(1:kname),'pfcurr.wr'
       open(1,file=fname,form='formatted')
       !open(1,file='currents.wr',form='formatted')
        write(1,*) NEQUI
        write(1,*)(PFCEQW(j),j=1,NEQUI)
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

         call wrcoil(nc,ncpfc,rc,zc,pc,necon,wecon)

         ngrid=1

         call auto(rc,zc,pc,nc,nstep,ngrid,
     *                 rc1,zc1, rc2,zc2,
     *                 rc3,zc3, rc4,zc4,
     *                 ntype, necon, wecon )

 2005   continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

        write(fname,'(a,a)') path(1:kname),'pfcurr.wr'
       open(1,file=fname,form='formatted')
       !open(1,file='currents.wr',form='formatted')
        read(1,*) nequi
        read(1,*)(PFCEQW(j),j=1,nequi)
       close(1)

        write(fname,'(a,a)') path(1:kname),'res_mat.wr'
       open(1,file=fname,form='formatted')
       !open(1,file='res_mat.wr')
        read(1,*) (res(j),j=1,ncequi)
       close(1)

         call rdcoil(nc,ncpfc,rc,zc,pc,necon,wecon)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         platok = tokf
         nstep  = 0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, nstep, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,psi0_bnd)
        !pause 'pause'

!!!!!!!!!!!!!!!!!!!!!!!!!!ttt!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!ttt!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!         nstep  = 1
!         keyctr =1

!         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
!     *             betplx, i_betp,
!     *             keyctr, nstep, platok, rax,zax, b0,r0, psax, igdf,
!     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
!     *              psi_bnd,e_psi)
!
           !psi_bnd=0.d0

         call prefit(rc,zc,ncpfc,NECON,WECON,rax,zax,alp_b,psi_bnd)

        do iq=1,NEQUI
          !curref(iq)=PFCEQW(iq)
          curref(iq)=0.d0
        enddo

         call curfit_(rc,zc,ncpfc,NECON,WECON,psi_bnd)

            do ik=1,nequi
            ccurx(ik)=PFCEQW(ik)
            pjk(ik)=PFCEQW(ik)
            enddo


       ! pause 'pause'

!!!!>>>>>>symmetr.case
              isymm=0
!!!!>>>>>>symmetr.case

              nursb=nurs
              psicen=psax
              ngav1=0
              ftok=tokf
         nstep=0
         ngrid=1


!             call auto(rk,zk,tk,nk,nstep,ngrid,
!     *                 rk1,zk1, rk2,zk2,
!     *                 rk3,zk3, rk4,zk4,
!     *                 ntipe, necon, wecon )
!

             call eq_0(pjk,psk,ncequi,nstep,ngrid,
     *                 alf0, alf1, alf2, bet0, bet1, bet2,
     *                 betpol, betplx, zli3,
     *                  ngav1,
     *                 ftok, tokout, psicen, pscout, 
     *                 nursb,psi_bnd,alp_b,rax,zax,n_ctrl,b0,r0 )

         call rdexf(ncequi)

           call eq_ax( pjk, psk, ncequi, nstep,ngrid,
     *                 alf0, alf1, alf2, bet0, bet1, bet2,
     *                 betpol,  betplx, zli3,
     *                    ngav1,
     *                 ftok,tokout,psicen,pscout,
     *                 EREVE0, ERPS,
     *                 psi_bnd,alp_b,rax,zax,isymm )

         call wrd !

         call prefit(rc,zc,ncpfc,NECON,WECON,rax,zax,alp_b,psi_bnd)

         ngrid=1
         EREVE0=1.d-7


 415        nstep=nstep+1

!!!temporary!!!!only for tin
            !if(ERPS.lt.EREVE0*10) then
            !  iq_cs=5
            !  do iq=1,NEQUI
                !curref(iq)=PFCEQW(iq)
                !if(iq.ne.iq_cs) curref(iq)=0.d0
            !  enddo
            !endif            
!!!temporary!!!!only for tin

          if(kxwx.eq.1)then
           call precal(rc,zc,ncpfc,NECON, WECON )
          elseif(kxwx.eq.0)then
           call precal_wx(rc,zc,ncpfc,NECON, WECON )
          elseif(kxwx.eq.2)then
           call precal(rc,zc,ncpfc,NECON, WECON )
          endif

          if(n_ctrl.eq.1 .OR. kastr.eq.1) then  !limiter point
         call curfit_L(rc,zc,ncpfc,NECON,WECON,psi_bnd)
          else
         call curfit(rc,zc,ncpfc,NECON,WECON,psi_bnd)
         !call curfit_L(rc,zc,ncpfc,NECON,WECON,psi_bnd)
          endif

!!!!!!!!!!!!!!!!!!!!!saturation!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!		     pf_sat=4.d-3 
!         if(nstep/5*5.eq.nstep) then
!            do ik=1,nequi
!          if(dabs(PFCEQW(ik)).gt.pf_sat) then
!             d_wght(ik)=d_wght(ik)*2.0d0
!          endif           
!            !PFCEQW(ik)=(ccurx(ik)+PFCEQW(ik))*0.5d0
!            enddo
!         endif           
!!!!!!!!!!!!!!!!!!!!!saturation!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            do ik=1,nequi
            ccurx(ik)=PFCEQW(ik)
            pjk(ik)=PFCEQW(ik)
            enddo

           call eq_ax( pjk, psk, ncequi, nstep,ngrid,
     *                 alf0, alf1, alf2, bet0, bet1, bet2,
     *                 betpol,  betplx, zli3,
     *                    ngav1,
     *                 ftok,tokout,psicen,pscout,
     *                 EREVE0, ERPS,
     *                 psi_bnd,alp_b,rax,zax,isymm )

          call wrd

            if(ERPS.gt.EREVE0) go to 415
          call wrd

        write(fname,'(a,a)') path(1:kname),'currents.wr'
        open(1,file=fname,form='formatted')
       !open(1,file='currents.wr',form='formatted')
        write(1,*) NEQUI,NCEQUI
        write(1,*)(pjk(j),j=1,NCEQUI)
       close(1)
       
        write(fname,'(a,a)') path(1:kname),'pfc_curr.wr'
       open(1,file=fname,form='formatted')
        write(1,*) 'coil currents [mA]'
        write(1,'(a6,e13.5)') 'PF1',pjk(1) !*1.d3
        write(1,'(a6,e13.5)') 'PF2',pjk(2) !*1.d3
        write(1,'(a6,e13.5)') 'F3,4',pjk(3) !*1.d3
        write(1,'(a6,e13.5)') 'F5,6',pjk(4) !*1.d3
        write(1,'(a6,e13.5)') 'CS',pjk(5) !*1.d3
        !write(1,'(a6,e13.5)') 'CS1',pjk(1) !*1.d3
        !write(1,'(a6,e13.5)') 'CS2U',pjk(2) !*1.d3
        !write(1,'(a6,e13.5)') 'CS2L',pjk(3) !*1.d3
        !write(1,'(a6,e13.5)') 'CS3U',pjk(4) !*1.d3
        !write(1,'(a6,e13.5)') 'CS3L',pjk(5) !*1.d3
        !write(1,'(a6,e13.5)') 'PF1',pjk(6) !*1.d3
        !write(1,'(a6,e13.5)') 'PF2',pjk(7) !*1.d3
        !write(1,'(a6,e13.5)') 'PF3',pjk(8) !*1.d3
        !write(1,'(a6,e13.5)') 'PF4',pjk(9) !*1.d3
        !write(1,'(a6,e13.5)') 'PF5',pjk(10) !*1.d3
        !write(1,'(a6,e13.5)') 'PF6',pjk(11) !*1.d3
        !write(1,'(a6,e13.5)') '   ',pjk(12) !*1.d3
        !write(1,'(a6,e13.5)') '   ',pjk(13) !*1.d3
        !write(1,'(a6,e13.5)') '   ',pjk(14) !*1.d3
        !write(1,'(a6,e13.5)') '   ',pjk(15) !*1.d3
        !write(1,'(a6,e13.5)') '   ',pjk(16) !*1.d3
        !write(1,'(a6,e13.5)') '   ',pjk(17) !*1.d3
        !write(1,'(a6,e13.5)') '   ',pjk(18) !*1.d3
       close(1)

        write(fname,'(a,a)') path(1:kname),'tcurrs.wr'
        open(1,file=fname,form='formatted')
             !open(1,file='tcurrs.wr')
            write(*,*) 'coil currents'

            do ik=1,nequi
	      do j=1,NPFC
	       if(ik .eq. NEPFC(j)) then
            write(*,'(i4,2(1pe13.5))') ik,PFCW1(j),PFCEQW(ik)
	      exit
	       endif
            enddo
		  enddo
       !call output(ngrid,betpol,zli3)
       !call wrdcur
       !call accr_ch
c            write(NOUT,'(8H  li(3)=,1pe13.5)') zli3

            !close(NOUT)
            close(1)

         !!!  stop   !!!!!!!
        pause 'pause'

          i_bsh=-1
          nstep  = 0
          keyctr =0
          i_betp =0
          i_eqdsk =0

         call eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *             betplx, i_betp,
     *             keyctr, nstep, platok, rax,zax, b0,r0, psax, igdf,
     *             n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *             psi_bnd,e_psi)


         !call wr_spik

           return   !!!!!!!!!!
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine aspid_flag(k_astr)
        common/com_flag/kastr
         kastr=k_astr
        return   
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine aspid_ini(k_ini)
        common/com_ini/key_ini
         key_ini=k_ini
        return   
        end