!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine eqb( alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2,
     *                 betplx, i_betp,
     *               keyctr,nstep,platok,rax,zax,b0cen,r0cen,psax,igdf,
     *                 n_tht, n_psi, epsro, nurs, i_eqdsk,i_bsh,
     *                 psi_bnd,psi0_bnd)

      include 'double.inc'
      include 'parurs.inc'
      parameter(nursp4=nursp+4,nursp6=nursp4*6)

      include 'dim.inc'
      include 'compol.inc'

      common /combsh/ rm0,zm0,rc0,zc0,asp0,el_up,el_lw,tr_up,tr_lw,nbsh
      common /comabw/ alf0p,alf1p,alf2p, bet0f,bet1f,bet2f
     *               ,alw0p,alw1p,alw2p

c   -----------------------------------------------------------------

          integer  nstep, ngav
          real*8   alf0,alf1,alf2, bet0,bet1,bet2, alw0,alw1,alw2
          real time_beg,time_end, time_b,time_e, dtim1,dtim2,dtim3
          real*8 q_giv(nrp),q_0(nrp)

          dimension pstab(nursp), qtab(nursp), psian(nrp)
          real*8 rrk(nursp4),cck(nursp4),wrk(nursp6)
          real*8 cwk(4)
          dimension BfJf(nrp)
	common
     *  /c_kpr/kpr

          abs(xx)  = dabs(xx)
          sqrt(xx) = dsqrt(xx)

c--------------------------------------------------------------------

           NiMax=250     
           !NiMax=10     

c--------------------------------------------------------------------

          !call cpu_time(time_beg)

            !!amu0=0.4d0*pi
            ! if(keyctr.ne.0) return
             if(keyctr.ge.100) return

            nt    = n_tht
            iplas = n_psi
            ngav  = keyctr
	      tok   = platok
            nbsh=i_bsh
	   !  qcen  = qax
            b0ax  = b0cen
            r0ax  = r0cen
            psiax = psax
         if(ngav.eq.-1) then
            psibon=psi_bnd
         else
            psi_eav=psi_bnd
         endif
            psibon0=psi0_bnd

            kstep=nstep

c...input initial data

            rm0   = rax
            zm0   = zax 
         
            alf0p = alf0 
            alf1p = alf1 
            alf2p = alf2 

            bet0f = bet0
            bet1f = bet1
            bet2f = bet2

            alw0p = alw0 
            alw1p = alw1 
            alw2p = alw2 
C--------------------------------------------------------------------

            if(i_eqdsk.eq.1) then
               nbsh=1
            endif

            nr     = iplas
            nr1    = nr-1
            nt1    = nt-1
            nr2    = nr-2
            nt2    = nt-2

            iplas1 = iplas-1

            itrmax = 50
            nitmax = 5
            nitdel = 10
            nitbeg = 5  !+ngav*10000
C--------------------------------------------------------------------

      if(nstep.eq.0) then
            if(nbsh.eq.0) then
        write(fname,'(a,a)') path(1:kname),'inpol.dat'
        open(1,file=fname)
        !open(1,file='inpol.dat')
               read(1,*)   dummy_nbsh
               read(1,*) rc0
               read(1,*) zc0
               read(1,*) asp0
               read(1,*) el_up
               read(1,*) el_lw
               read(1,*) tr_up
               read(1,*) tr_lw
        close(1)    ! for "inpol.dat" reading
            endif

!	   if( nbsh.eq.0 ) then
!            write(* ,*) '********************************'
!            write(* ,*) 'nbsh =',nbsh
!            write(* ,*) '"inpol.dat" file has been read: '
!            write(* ,*) '--------------------------------'
!            write(* ,*) '                                '
!            write(17,*) '********************************'
!            write(17,*) 'nbsh =',nbsh
!            write(17,*) '"inpol.dat" file has been read: '
!            write(17,*) '--------------------------------'
!            write(17,*) '                                '
!            write(17,*) 'rc0     =',rc0
!            write(17,*) 'zc0     =',zc0
!            write(17,*) 'asp0    =',asp0
!            write(17,*) 'el_up   =',el_up
!            write(17,*) 'el_lw   =',el_lw
!            write(17,*) 'tr_up   =',tr_up
!            write(17,*) 'tr_lw   =',tr_lw
!            write(17,*) '                                '
!         else
!            write(* ,*) '************************************'
!            write(* ,*) 'nbsh =',nbsh
!            write(* ,*) '"inpol.dat" file has not been read. '
!            write(* ,*) '------------------------------------'
!            write(* ,*) '                                    '
!            write(17,*) '************************************'
!            write(17,*) 'nbsh =',nbsh
!            write(17,*) '"inpol.dat" file has not been read. '
!            write(17,*) '------------------------------------'
!            write(17,*) '                                    '
!         end if

	      igrdf = igdf

	      if(igdf.eq.2) igrdf=1

            !!!++call arc_x_bnd(nt)
           if(nbsh.eq.0.or.nbsh.eq.1) then  
            call grid_0(igrdf,nstep)
            call taburs(0,1.d0,nurs)
           elseif(nbsh.eq.-1) then
            call grid_b(igrdf,nstep)
            !call wrb
           elseif(nbsh.eq.-2) then
            call grid_p0(igrdf,nstep)
            call taburs(0,1.d0,nurs)
           endif

            do i=1,iplas
               dpdpsi(i)=tabp(psia(i))
               dfdpsi(i)=tabf(psia(i))
            enddo

            if(ngav.eq.0) call presol(i_betp,betplx,betpol)

      else                                                   !nstep.ne.0

             !!!++call taburs(0,1.d0,nurs)
             !!!++call arc_x_bnd(nt)
           if(nbsh.eq.0.or.nbsh.eq.1) then  
             call grid_1(igrdf,nstep)
           elseif(nbsh.eq.-1) then
            call grid_b1(igrdf,nstep)
            !call wrb
           elseif(nbsh.eq.-2) then
            call grid_p1(igrdf,nstep)
           endif

!             do i=1,iplas
!                dpdpsi(i)=tabp(psia(i))
!                dfdpsi(i)=tabf(psia(i))
!             enddo
      endif   !nstep.eq.0
      
      if(ngav.lt.0) then
        call bongri
      endif   
!&&&&&&&&&&&&
      !if(ngav.eq.-2.AND.nbsh.eq.-1) then
      if(nbsh.eq.-1) then
        call psib_ext(psi_eav)
      endif   
!&&&&&&&&&&&&

C--------------------------------------------------------------------

!        call taburs(0, 1.d0, nurs)

         cnor=1.d0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         go to 639  !!<<<<<<+**

      if(ngav.gt.0) then

C------------------------------------------------
        write(fname,'(a,a)') path(1:kname),'tab_q.dat'
        open(1,file=fname)
	    !open(1,file='tab_q.dat')
	        read(1,*) nutab
	        do i=1,nutab
                 read(1,*)  pstab(i), qtab(i)
              enddo
          close(1)
C------------------------------------------------

	    do i=1,nutab
             pstab(i)=pstab(i)/pstab(nutab)
          enddo

          call e01baf(nutab,pstab,qtab,rrk,cck,
     *                          nutab+4,wrk,6*nutab+16,ifail)

 2257 continue

          do i=1,iplas1

	        psia05=1.d0-0.5d0*(psia(i)+psia(i+1))

              call e02bcf(nutab+4,rrk,cck,psia05,0,cwk,ifail)

              q_giv(i)=cwk(1)*2.d0*pi
              q_0(i)=q(i)

          enddo

          q_giv0=qtab(1)
          q_giv(iplas)=qtab(nutab)*2.d0*pi

	    do i=1,iplas
             q(i) = q_giv(i)
	    enddo

          if(igdf.eq.2) then

                do i=1,iplas
	           psian(i)=psia(i)
                enddo

	          call grdef(igdf)

                errpsa=0.d0

                do i=1,iplas
                   errpsn=dabs(psian(i)-psia(i))
                   errpsa=dmax1(errpsa,errpsn)
                enddo

                if(errpsa.gt.1.d-9) go to 2257

	    endif

          qax = q_giv0

               do i=1,iplas
                  dpdpsi(i)=tabp(psia(i))
                  dfdpsi(i)=tabf(psia(i))
               enddo                

      endif

!!!!!!!!!!!!!!!!!!!!
  639   continue
!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

             iter  = 0
             itin  = 0
             imov  = 1

 1000   continue

             iter  = iter+1
             itin  = itin+1

         !write(6,*)'iter=',iter,itin

             igrdf = igdf

             !if(igdf.eq.2 .and. itin.lt.4) igrdf=1

           !call cpu_time(time_b)
ch4astra             call metric
             call metrix
           !call cpu_time(time_e)
           !write(*,*) '***metric:time=',time_e-time_b
             call matcof
           !call cpu_time(time_b)
           !write(*,*) '***matcof:time=',time_e-time_b
             call matpla
           !call cpu_time(time_e)
           !write(*,*) '***matpla:time=',time_e-time_b
             call rightg
           !call cpu_time(time_b)
           !w!rite(*,*) '***rightg:time=',time_e-time_b

	       if(i_betp.eq.1) then
	          if(iter.gt.4) call skbetp(betplx,betpol)
             endif

                      !call bt_pol(betpol)
                      !write(6,*)'betpol',betpol,'*********'
                      !call rigbon

	       !if(ngav.le.0 .and. igrdf.ge.2) then
	       if(ngav.le.0 ) then
	          call qst_b
             endif

	       call grdef(igrdf)
             call solint(imov)

                      !write(6,*)'solve(psi)'
                      !call wrb
		            !pause 'wrd after solve'

             call spider_remesh(erro,errpsi,imov)
           !call cpu_time(time_e)
           !write(*,*) '***solve:time=',time_e-time_b

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! if(ngav.eq.1) then
	   ! call wrb
	   ! pause 'wrb after regrid '
	   ! endif

!!!!!!!!!!!! accurasy  test parameters !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         errod = 0.5d0*erro/(dabs(z(iplas,2)-zm)+dabs(r(iplas,2)-rm))
         erru=erro
         !write(6,*) 'errod,epsro',errod,epsro
           if(errod.lt.epsro) go to 2000
          if(itin.ge.NiMax) then 
             write(*,*) 'SPIDER: MAX NUMBER OF ITERATIONS IS EXCEEDED'
             write(*,*) 'ERROD=',errod
             write(*,*) 'ITER=',itin
             go to 2000
          endif             

           go to 1000
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 2000     continue
!        write(*,*) 'OK'

          if(ngav.le.0) then

             call qst_b
                     ! write(6,*)'qst'
!             open(1,file='ddps0.pr')
!                do 567 i=1,iplas
! 567               write(1,*) 'dfdpsi(i), f(i), i', dfdpsi(i),f(i),i
!             close(1)

             psiax=psim

            !call procof(0)

          endif
c--------------------------------------------------------------------

          platok = tokp
          rax    = rm
          zax    = zm
          psax   = psim

          if(ngav.le.0) then
             psax   = psim
             psipla = psim-psip
          endif
          if(ngav.eq.-3) then
            psi_bnd=psi_eav
          endif
             !fvac   = f(iplas)

	    if(ngav.gt.0) qax = q_giv0

          if(ngav.lt.0) call retab_L
          if(ngav.gt.0) call retab_p

          call bt_pol(betpol)
          call bt_tot(bettot)

          if(kpr.eq.1) then
        write(* ,*) '*******************************************'
        write(* ,*) 'Iterations have been converged for epsro =',epsro
        write(* ,*) 'Achieved accuracy ................ errod =',errod
        write(* ,*) 'Number of iterations ............. iter  =',iter
        write(* ,*) 'Magnetic axis coordinates ........ rm    =', rm
        write(* ,*) '                                   zm    =', zm
        write(* ,*) 'Plasma current ................... tokp  =', tokp
        write(* ,*) '                                   cnor  =', cnor
        write(* ,*) 'Magnetic axis PSI value .......... psim  =', psim
        write(* ,*) 'Poloidal beta value ............. betpol =', betpol
        write(* ,*) 'Total    beta value ............. bettot =', bettot
        write(* ,*) 'Plas.boun.poloidal current value... fvac =', fvac
        !write(* ,*) 'Magnetic axis q value ............. qax  =', qax
        write(* ,*) '*******************************************'
          endif
!        write(17,*) '*******************************************'
!        write(17,*) 'Iterations have been converged for epsro =',epsro
!        write(17,*) 'Achieved accuracy ................ errod =',errod
!        write(17,*) 'Number of iterations ............. iter  =',iter 
!        write(17,*) 'Magnetic axis coordinates ........ rm    =', rm
!        write(17,*) '                                   zm    =', zm
!        write(17,*) 'Plasma current ................... tokp  =', tokp
!        write(17,*) '                                   cnor  =', cnor
!        write(17,*) 'Magnetic axis PSI value .......... psim  =', psim 
!        write(17,*) 'Poloidal beta value ............. betpol =', betpol
!        write(17,*) 'Total    beta value ............. bettot =', bettot
!        write(17,*) 'Plas.boun.poloidal current value... fvac =', fvac
!        write(17,*) 'Magnetic axis q value ............. qax  =', qax
!        write(17,*) '*******************************************'


          call wrb
          !call out_b
          !call retab
          !call retab_f
          !if(ngav.eq.0) then
          !call retab_p
          !endif

       !call cpu_time(time_end)
       !write(*,*) 'operation time=',time_end-time_beg
       !write(*,*) 'eqb done'

      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine retab

          include 'double.inc'
          parameter(nursp=4000,nursp4=nursp+4,nursp6=nursp4*6)
          include 'dim.inc'
          include 'compol.inc'

          common/comppp/ ppp(nursp),fff(nursp),www(nursp)
          common/comurs/psit(nursp),purs(nursp),furs(nursp),wurs(nursp),
     *                  nurs
          dimension pstab(nursp),pptab(nursp),fptab(nursp)
          real*8 rrk(nursp4),cck(nursp4),wrk(nursp6)
          real*8 cwk(4)
	common
     *  /c_kpr/kpr

 	    write(*,*) 'retab:recalculation p and ff'

	do i=1,iplas
        pstab(i)=1.d0-psia(i)
	enddo

        call e01baf(iplas,pstab,dfdpsi,rrk,cck,
     *                          iplas+4,wrk,6*iplas+16,ifail)

        if(ifail.ne.0.and.kpr.eq.1) write(*,*) 'ifail=',ifail

           do i=2,nurs-1

              zpsi=1.d0-psit(i)

              call e02bcf(iplas+4,rrk,cck,zpsi,0,cwk,ifail)

              if(ifail.ne.0.and.kpr.eq.1) write(*,*) 'ifail=',ifail

              furs(i)=cwk(1)

           enddo

           furs(1)=dfdpsi(iplas)
           furs(nurs)=dfdpsi(1)

           call e01baf(iplas,pstab,dpdpsi,rrk,cck,
     *                             iplas+4,wrk,6*iplas+16,ifail)

           if(ifail.ne.0.and.kpr.eq.1) write(*,*) 'ifail=',ifail

           do i=2,nurs-1

              zpsi=1.d0-psit(i)

              call e02bcf(iplas+4,rrk,cck,zpsi,0,cwk,ifail)

              if(ifail.ne.0.and.kpr.eq.1) write(*,*) 'ifail=',ifail

              purs(i)=cwk(1)

           enddo

           purs(1)=dpdpsi(iplas)
           purs(nurs)=dpdpsi(1)

          ! do i=1,nurs
	    ! write(6,*) 'p/f,ps:',purs(i)/(furs(i)+1.d-15),psit(i),i
	    ! write(6,*) 'p,f:',purs(i),furs(i),i
	    ! pause ' '
          ! enddo

         dpsi=1.d0/(nurs-1.d0)
         ppp(1)=0.d0
         fff(1)=0.d0

         do 20 i=2,nurs
            ppp(i)=ppp(i-1) +(purs(i-1)+purs(i))*dpsi*0.5d0
            fff(i)=fff(i-1) +(furs(i-1)+furs(i))*dpsi*0.5d0
 20      continue

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine retab_L

          include 'double.inc'
          include 'dim.inc'
          include 'compol.inc'
          include 'urs.inc'

          common/comppp/ ppp(nursp),fff(nursp),www(nursp)
          dimension pstab(nursp),pptab(nursp),fptab(nursp)
	common
     *  /c_kpr/kpr

 	    !write(*,*) 'retab_L:recalculation p and ff'

	do i=1,iplas
        pstab(i)=1.d0-psia(i)
	enddo

           do it=2,nurs-1
              zpsi=1.d0-psit(it)

            do i=1,iplas-1
             if(zpsi.gt.pstab(i) .AND. zpsi.le.pstab(i+1)) then
              ic=i
              exit
             endif
            enddo

              dpsi=pstab(ic+1)-pstab(ic)
              furs(it)=( dfdpsi(ic)*(pstab(ic+1)-zpsi)+
     *                  dfdpsi(ic+1)*(zpsi-pstab(ic)) )/dpsi

              purs(it)=( dpdpsi(ic)*(pstab(ic+1)-zpsi)+
     *                  dpdpsi(ic+1)*(zpsi-pstab(ic)) )/dpsi
           enddo

           furs(1)=dfdpsi(iplas)
           furs(nurs)=dfdpsi(1)

           purs(1)=dpdpsi(iplas)
           purs(nurs)=dpdpsi(1)

         dpsi=1.d0/(nurs-1.d0)
         ppp(1)=0.d0
         fff(1)=0.d0

         do 20 i=2,nurs
            ppp(i)=ppp(i-1) +(purs(i-1)+purs(i))*dpsi*0.5d0
            fff(i)=fff(i-1) +(furs(i-1)+furs(i))*dpsi*0.5d0
 20      continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine retab_L_ast(pstab,pptab,fptab,ntab)

          include 'double.inc'
          !include 'dim.inc'
          !include 'compol.inc'
          include 'urs.inc'

          common/comppp/ ppp(nursp),fff(nursp),www(nursp)
          dimension pstab(*),pptab(*),fptab(*)
	common
     *  /c_kpr/kpr

 	    write(*,*) 'retab_L:recalculation p and ff'


           do it=2,nurs-1
              zpsi=1.d0-psit(it)

            do i=1,ntab-1
             if(zpsi.gt.pstab(i) .AND. zpsi.le.pstab(i+1)) then
              ic=i
              exit
             endif
            enddo

              dpsi=pstab(ic+1)-pstab(ic)
              furs(it)=( fptab(ic)*(pstab(ic+1)-zpsi)+
     *                  fptab(ic+1)*(zpsi-pstab(ic)) )/dpsi

              purs(it)=( pptab(ic)*(pstab(ic+1)-zpsi)+
     *                  pptab(ic+1)*(zpsi-pstab(ic)) )/dpsi
           enddo

           furs(1)=fptab(ntab)
           furs(nurs)=fptab(1)

           purs(1)=pptab(ntab)
           purs(nurs)=pptab(1)

         dpsi=1.d0/(nurs-1.d0)
         ppp(1)=0.d0
         fff(1)=0.d0

         do 20 i=2,nurs
            ppp(i)=ppp(i-1) +(purs(i-1)+purs(i))*dpsi*0.5d0
            fff(i)=fff(i-1) +(furs(i-1)+furs(i))*dpsi*0.5d0
 20      continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine retab_F

          include 'double.inc'
          include 'dim.inc'
          include 'compol.inc'
          include 'urs.inc'

          common/comppp/ ppp(nursp),fff(nursp),www(nursp)
          dimension pstab(nursp),pptab(nursp),fptab(nursp)
	common
     *  /c_kpr/kpr

 	    write(*,*) 'retab_F:linear recalculation ff table'

	do i=1,iplas
        pstab(i)=1.d0-psia(i)
	enddo

           do it=2,nurs-1
              zpsi=1.d0-psit(it)

            do i=1,iplas-1
             if(zpsi.gt.pstab(i) .AND. zpsi.le.pstab(i+1)) then
              ic=i
              exit
             endif
            enddo

              dpsi=pstab(ic+1)-pstab(ic)
              furs(it)=( dfdpsi(ic)*(pstab(ic+1)-zpsi)+
     *                  dfdpsi(ic+1)*(zpsi-pstab(ic)) )/dpsi

!              purs(it)=( dpdpsi(ic)*(pstab(ic+1)-zpsi)+
!     *                  dpdpsi(ic+1)*(zpsi-pstab(ic)) )/dpsi
           enddo

           furs(1)=dfdpsi(iplas)
           furs(nurs)=dfdpsi(1)

!           purs(1)=dpdpsi(iplas)
!           purs(nurs)=dpdpsi(1)

         dpsi=1.d0/(nurs-1.d0)
!         ppp(1)=0.d0
         fff(1)=0.d0

         do 20 i=2,nurs
!            ppp(i)=ppp(i-1) +(purs(i-1)+purs(i))*dpsi*0.5d0
            fff(i)=fff(i-1) +(furs(i-1)+furs(i))*dpsi*0.5d0
 20      continue

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine retab_p

          include 'double.inc'
          include 'dim.inc'
          include 'compol.inc'
          include 'urs.inc'

          common/comppp/ ppp(nursp),fff(nursp),www(nursp)
          dimension pstab(nursp),pptab(nursp),fptab(nursp)
	common
     *  /c_kpr/kpr

 	    write(*,*) 'retab_p:linear recalculation p table'

	do i=1,iplas
        pstab(i)=1.d0-psia(i)
	enddo

           do it=2,nurs-1
              zpsi=1.d0-psit(it)

            do i=1,iplas-1
             if(zpsi.gt.pstab(i) .AND. zpsi.le.pstab(i+1)) then
              ic=i
              exit
             endif
            enddo

              dpsi=pstab(ic+1)-pstab(ic)
!              furs(it)=( dfdpsi(ic)*(pstab(ic+1)-zpsi)+
!     *                  dfdpsi(ic+1)*(zpsi-pstab(ic)) )/dpsi

              purs(it)=( dpdpsi(ic)*(pstab(ic+1)-zpsi)+
     *                  dpdpsi(ic+1)*(zpsi-pstab(ic)) )/dpsi
           enddo

!           furs(1)=dfdpsi(iplas)
!           furs(nurs)=dfdpsi(1)

           purs(1)=dpdpsi(iplas)
           purs(nurs)=dpdpsi(1)

         dpsi=1.d0/(nurs-1.d0)
         ppp(1)=0.d0
!         fff(1)=0.d0

         do 20 i=2,nurs
            ppp(i)=ppp(i-1) +(purs(i-1)+purs(i))*dpsi*0.5d0
!            fff(i)=fff(i-1) +(furs(i-1)+furs(i))*dpsi*0.5d0
 20      continue

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine bt_tot(bettot)

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

              volcen=0.d0

         do j=2,nt1
          volcen=volcen+vol(1,j)*0.5d0
         enddo

          volpl=volcen

              psn=psin(1,2)

             zpres=funppp(psn)
             pintg=zpres*volcen

           do i=2,iplas1
           do j=2,nt1

              psn=psin(i,j)
              zpres=funppp(psn)
	        volk=vol1(i,j)+vol2(i-1,j)+vol3(i-1,j-1)+vol4(i,j-1)
              pintg=pintg+zpres*volk
              volpl=volpl+volk

           enddo
           enddo

           paverg=pintg/volpl

           bettot=2.d0*paverg/(b0ax*b0ax)
           bettot=bettot*(psim-psip)*cnor

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine presol(i_betp,betplx,betpol)

         include 'double.inc'
         include 'dim.inc'
         include 'compol.inc'
	common
     *  /c_kpr/kpr

         imov=0
         iter=0
         itin=0

ch4astra         call metric
         call metrix
         call matcof
         call matpla

C        write(6,*) 'matpla'

 1000 continue

         iter = iter+1
         itin = itin+1
          if(kpr.eq.1) then
         write(*,*)'iter=',iter,itin
          endif
         call rightg

C        write(6,*) 'rightg'

	   if(i_betp.eq.1) then

	      if(iter.gt.4) call skbetp(betplx,betpol)

         endif

                  ! call bt_pol(betpol)
                  ! write(6,*)'betpol',betpol,'*********'

         call solint(imov)

                  ! write(6,*)'solve(psi)'
                  ! call wrb
		        ! pause 'wrd after solve'

         call spider_remesh(erro,errpsi,imov)

          if(kpr.eq.1) then
         write(*,*)'presol: errpsi',errpsi
          endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! if(ngav.eq.1) then
	   ! call wrb
	   ! pause 'wrb after regrid '
	   ! endif

!!!!!!!!!!!! accurasy  test parameters !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         errod = 0.5d0*erro/dabs(z(iplas,2)-zm)

         if(errpsi.lt.1.d-4 .or. iter.ge.50)  go to 2000

         go to 1000

 2000    continue

             psimax=psi(1,2)
	       imax=1
	       jmax=2

             do i=2,iplas1
             do j=2,nt1

                if(psi(i,j).gt.psimax) then
                   psimax=psi(i,j)
			     imax=i
			     jmax=j
		      endif

             enddo
             enddo

          if(kpr.eq.1) then
              write(*,*) 'presol: '
              write(*,*) 'number of iterations ',iter
              write(*,*) 'accuracy ',errpsi
          endif
C--------------------------------------------------------------------

         call prgrid(imax,jmax,erro,1,errpsi)

         platok = tokp
         rax    = rm
         zax    = zm
         psax   = psim

          if(kpr.eq.1) then
              write(*,*) 'rax,zax',rax,zax
              write(*,*) 'plas.current',platok,cnor
              write(*,*) 'psax',psax
          endif

         psax   = psim
         psipla = psim-psip
         fvac   = f(iplas)

         call bt_pol(betpol)
         call wrb
       ! call out_b

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine p_pert

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         ampl=1.0d0 * dpdpsi(1)
         !ampl=0.0d0 
         xa0=0.9999d0
         xw=0.02d0

        do i=1,iplas 
         xa=dfloat(i-1)/dfloat(iplas-1)	  	  
         fun=( 1.d0-dtanh(((xa0-xa)/xw)**2) )
         dpdpsi(i)=dpdpsi(i)+ampl*fun
        enddo

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine put_tim(dt,time)

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         dtim=dt
         ctim=time
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine get_tim(dt,time)

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         dt=dtim
         time=ctim
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine get_www(roomega)

          include 'double.inc'
          include 'dim.inc'
          include 'compol.inc'
          include 'urs.inc'

          common/comppp/ ppp(nursp),fff(nursp),www(nursp)

               roomega=www(nurs)*psim*cnor
              write(*,*) 'roomega',roomega


         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



