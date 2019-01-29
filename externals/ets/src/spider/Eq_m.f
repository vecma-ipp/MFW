!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine auto(rk,zk,tk,nk,nstep,ngrid,
     *                 rk1,zk1, rk2,zk2,
     *                 rk3,zk3, rk4,zk4,
     *                 ntipe, necon, wecon )

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
         include 'urs.inc'

          integer ngrid,nk,nstep
          real*8   tk(*),rk(*),zk(*), wecon(*)

          real*8   rk1(*),zk1(*),rk2(*),zk2(*)
          real*8   rk3(*),zk3(*),rk4(*),zk4(*)

          integer  ntipe(*), necon(*)


c...input initial data

        write(fname,'(a,a)') path(1:kname),'data.dat'
        open(1,file=fname,form='formatted')
        !open(1,file='data.dat')
             read(1,*) icont
             read(1,*) ni
             read(1,*) nj
             read(1,*) rmin
             read(1,*) rmax
             read(1,*) zmin
             read(1,*) zmax
             read(1,*) alp

             read(1,*) nctrl

             read(1,*) qcen
             read(1,*) rx10
             read(1,*) zx10
             read(1,*) rx20
             read(1,*) zx20
             read(1,*) rm0
             read(1,*) zm0
        close(1)

        write(fname,'(a,a)') path(1:kname),'data_d.wr'
        open(1,file=fname,form='formatted')
        !open(1,file='data_d.wr')
             write(1,*) icont,ni,nj,nctrl
             write(1,*) rmin,rmax,zmin,zmax,alp,qcen,
     *                  rx10,zx10,rx20,zx20,rm0,zm0
        close(1)

        n_ctrl=nctrl
        psi_bon=psi_bnd

        rx1=rx10
        zx1=zx10
        rx2=rx20
        zx2=zx20

        write(fname,'(a,a)') path(1:kname),'limpnt.dat'
        open(1,file=fname,form='formatted')
           !open(1,file='limpnt.dat')
                read(1,*) nblm
               do i=1,nblm
                read(1,*) rblm(i),zblm(i)
               enddo
           close(1)

        write(fname,'(a,a)') path(1:kname),'limpnt_d.wr'
        open(1,file=fname,form='formatted')
           !open(1,file='limpnt_d.wr')
                write(1,*) nblm
               do i=1,nblm
                write(1,*) rblm(i),zblm(i)
               enddo
           close(1)

c...index

          call glbind

          call grid

         if(icont.eq.0) then

             call exfmat(rk,zk,tk,nk,
     *                     rk1,zk1,rk2,zk2,
     *                     rk3,zk3,rk4,zk4,ntipe, necon, wecon)
             !call bndint(rk,zk,nk)
             !call wrdbin(nk)

             call cfr_mat(rk,zk,tk,nk,
     *                      NECON, WECON )


         endif

       return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine noauto

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
         include 'urs.inc'


c...input initial data


        write(fname,'(a,a)') path(1:kname),'data_d.wr'
        open(1,file=fname,form='formatted')
        !open(1,file='data_d.wr')
             read(1,*) icont,ni,nj,nctrl
             read(1,*) rmin,rmax,zmin,zmax,alp,qcen,
     *                  rx10,zx10,rx20,zx20,rm0,zm0
        close(1)

        !icont=1

        n_ctrl=nctrl
        psi_bon=psi_bnd

        rx1=rx10
        zx1=zx10
        rx2=rx20
        zx2=zx20

        write(fname,'(a,a)') path(1:kname),'limpnt_d.wr'
        open(1,file=fname,form='formatted')
           !open(1,file='limpnt_d.wr')
                read(1,*) nblm
               do i=1,nblm
                read(1,*) rblm(i),zblm(i)
               enddo
           close(1)

       return
        end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine eq_0(pcequi,psitok,ncequi,nstep,ngrid,
     *                 alf0, alf1, alf2, bet0, bet1, bet2,
     *                 betpol, betplx, zli3,
     *                  ngav1,
     *                 ftok, tokout, psicen, pscout, 
     *                 nursb,psi_bnd,alp_b,rax,zax,n_ctrl,b_0,r_0 )

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
         include 'urs.inc'

          common /comomg/  omega,sigma
          common/comsav/ alf0n
          common /comhel/  helinp, helout

          integer isol,ngrid,nstep,ngav1
          real*8   pcequi(*),psitok(*)

          real*8   alf0,alf1,alf2,bet0,bet1,bet2
          real*8   betpol,zli3,ftok,psicen,tokout,pscout


            alf0p=alf0
            alf1p=alf1
            alf2p=alf2
            bet0f=bet0
            bet1f=bet1
            bet2f=bet2
            b0ax=b_0
            r0ax=r_0
C-----------------------------------------------------------------
                  nnstpp=nstep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!                                            !!!!
!!!!!!!!!                                                !!!
!!!!!!!!                                                  !!!

C      if( nstep.eq.0 .AnD. ngrid.eq.0 ) then
C
C	   call tab_build
C
C      endif

!!!!!!!!                                               !!!!!!
!!!!!!!!!!!                                         !!!!!!!!
!!!!!!!!!!!!!!!!!!!                       !!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ccc+      if(nstep.gt.0) go to 6789

c...input initial data


        n_ctrl=nctrl
        psi_bon=psi_bnd

        if(nstep.gt.0) icont=1

C   -----------------------
          tok  = ftok
          ucen = psicen
C   -----------------------

        write(fname,'(a,a)') path(1:kname),'itpr.dat'
        open(1,file=fname,form='formatted')
        !open(1,file='itpr.dat')
         read(1,*) eps0
         read(1,*) eps
         read(1,*) epsin
         read(1,*) epscrz
         read(1,*) iterbf
         read(1,*) Nitl
         read(1,*) Nitin
         read(1,*) nrun
         read(1,*) nwr
         read(1,*) omg
         read(1,*) sigm
         read(1,*) ceps
        close(1)

c...index

          call glbind

         ix1=1
         jx1=1

         ix2=1
         jx2=1

         call grid
         call geom

         if(icont.eq.0) then
             call bndmat
             call wrdbnd
             icont=1
         else
             call rddbnd
         endif

 7890    continue

             call extfil(pcequi,ncequi)

                        coin=1.d0
         if(nstep.eq.0) then
	   !!!!!!!ien=0
	   ien=1
         call taburs(ien,coin,nursb)
	   endif

              isol  = 0
              nflag = 0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(ngrid.eq.0) then
             Ni=(Ni+1)/2
             Nj=(Nj+1)/2

             alp=alp-0.005d0

             call glbind
             call botlev(nstep)
             call geom
         endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 777         call matrix

             !if(nstep.eq.0 .AND. nflag.eq.0) then

             !call zero(isol)
             call zero_ax(isol)

               clr=0.d0
               clz=0.d0

             !endif

             if(nflag.eq.0) call psiful

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                 tokout = tok          !!
                 pscout = um           !!
                 r_ax = rm             !!
                 z_ax = zm             !!
                 alp_b = alp           !!
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          call wrd

         iter=0
         itin=0

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine eq( pcequi, psicon, ncequi, nstep, ngrid,
     *                 alf0, alf1, alf2, bet0, bet1, bet2,
     *                 betpol, betplx, zli3,
     *                  ngav1,
     *                 ftok, tokout, psicen, pscout, 
     *                 nursb,psi_bnd,alp_b,rax,zax )

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
         include 'urs.inc'

          common /comomg/  omega,sigma
          common/comsav/ alf0n
          common /comhel/  helinp, helout

	common
     *  /c_kpr/kpr

          integer isol,ngrid,ncequi,nstep,ngav1

          real*8   alf0,alf1,alf2,bet0,bet1,bet2
          real*8   betpol,zli3,ftok,psicen,tokout,pscout
          real*8   psicon(*),pcequi(*)

	  real*4 a_print(200)
	  character *30 apr



          if(nstep.eq.0) then
             rm=rax		     
             zm=zax
          endif


        write(fname,'(a,a)') path(1:kname),'itpr.dat'
        open(1,file=fname,form='formatted')
        !open(1,file='itpr.dat')
         read(1,*) eps0
         read(1,*) eps
         read(1,*) epsin
         read(1,*) epscrz
         read(1,*) iterbf
         read(1,*) Nitl
         read(1,*) Nitin
         read(1,*) nrun
         read(1,*) nwr
         read(1,*) omg
         read(1,*) sigm
         read(1,*) ceps
        close(1)

         eps0L=  eps0/3.d0
         epsL =  eps/3.d0
         cepsL = ceps/3.d0
         epsinL= epsin/3.d0
C------------------------------------------------

          if(nstep.gt.0) then
              iterbf=1
              go to 7890
          endif
c...index

          call glbind

         ix1=1
         jx1=1

         ix2=1
         jx2=1

         call grid
         call geom

             call rddbnd

 7890    continue

             call extfil(pcequi,ncequi)

                        coin=1.d0
         if(nstep.eq.0) then
	   !!!ien=0
	   ien=1
         call taburs(ien,coin,nursb)
	   endif

              isol  = 0
              nflag = 0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(ngrid.eq.0) then
             Ni=(Ni+1)/2
             Nj=(Nj+1)/2

             alp=alp-0.005d0

             call glbind
             call botlev(nstep)
             call geom
         endif

 777         call matrix

             if(nstep.eq.0 .AND. nflag.eq.0) then

             !call zero(isol)
             call zero_ax(isol)

               clr=0.d0
               clz=0.d0

             endif

             if(nflag.eq.0) call psiful

               erru=1.d0
               omega=1.d0
               sigma=1.d0
               ich=0
               iflag=0

           call wrd
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         itl=-1

         iter=0
         itin=0

 1000    iter=iter+1
         itin=itin+1

             if(iter.eq.iterbf .or. nstep.gt.0) then
                 rl=rm
                 zl=zm
                      call shab(il,jl,icelm,jcelm)
cccccccc              call wrd
             endif
       !call wrd
       !pause 'pause: wrd'

ccc         if(iter.eq.2) call wrd

            if(itin.le.3) then
                rx1=rx10
                rx2=rx20
            endif

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       !
            if(ich.eq.0) then  !!!!!!!!!!!!!!!!!!!!!!!!!
      !write(6,*) '** 0 **itin=',itin          !
                                                       !
               if(erru.lt.eps .OR. itin.ge.Nitin) then !

                 itin=0                                !
                                                       !
                 omega=1.d0                            !
                 sigma=1.d0                            !

          !write(*,*) 'itl=',itl           !
          !pause 'pause'
                                                       !
                 itl=itl+1                             !

                            !eps=dmax1(eps/1.5,epsl)    !
                           !ceps=dmax1(ceps/1.5,ceps)   !

                 ich=ich+1                             !
                 iflag = 1                             !
                                                       !
                 rl0=rl                                !
                 zl0=zl                                !
                                                       !
                 clr0=clr                              !
                 clz0=clz                              !
                                                       !
                 ddzl=dz(jmax)*ceps                    !
                 zl=zl+ddzl                            !
                      call shab(il,jl,icelm,jcelm)     !
               endif                                   !
                                                       !
            elseif(ich.eq.1) then  !!!!!!!!!!!!!!!!!!!!!
      !write(6,*) '** 1 **itin=',itin           !
                                                       !
               if(erru.lt.epsin .OR. itin.ge.Nitin) then !

                 itin=0                                !
                                                       !
                 omega=1.d0                              !
                 sigma=1.d0                              !
                                                       !
                 ich=ich+1                             !
                                                       !
                 clr1=clr                              !
                 clz1=clz                              !
                                                       !
              !! clr=0.d0                              !
              !! clz=0.d0                              !
                                                       !
                 ddrl=dr(imax)*ceps                    !
                 rl=rl+ddrl                            !
                 zl=zl0                                !
                      call shab(il,jl,icelm,jcelm)     !
               endif                                   !
                                                       !
            elseif(ich.eq.2) then  !!!!!!!!!!!!!!!!!!!!!
      !write(6,*) '** 2 **itin=',itin            !
                                                       !
         if(erru.lt.epsin .OR. itin.ge.Nitin) then      !
                    !!!  call wrd
                     !!!  write(6,*) ' 2 wrd'

                 itin=0                                !
                                                       !
                 omega=1.d0                              !
                 sigma=1.d0                              !
                                                       !
                 dcrdr=(clr-clr0)/ddrl                 !
                 dczdr=(clz-clz0)/ddrl                 !
                                                       !
                 dcrdz=(clr1-clr0)/ddzl                !
                 dczdz=(clz1-clz0)/ddzl                !
                                                       !
                 det=dcrdr*dczdz-dczdr*dcrdz           !
                                                       !
              delrl= (clz0*dcrdz-clr0*dczdz)/det       !
              delzl= (clr0*dczdr-clz0*dcrdr)/det       !
                                                       !
              dll=dsqrt(delrl**2 + delzl**2)           !
                                                       !
              dllim=0.25d0*dr(imax)                      !
                                                       !
              if(dll .gt. dllim) then                  !
                                                       !
                  nstp=dll/dllim
                                                       !
                  ddrr=delrl/nstp                      !
                  ddzz=delzl/nstp
				                                     !
				if(nstp.gt.10) nstp=10               !
                                                       !
                  do 234 istep=1,nstp                  !
                                                       !
                  !write(6,*) 'slow shift',istep,nstp   !
                                                       !
                  rl=rl0+ ddrr*istep                   !
                  zl=zl0+ ddzz*istep                   !
                      call shab(il,jl,icelm,jcelm)     !
                                                       !
 778              continue                             !
                                                       !
                rx1=rx10
                rx2=rx20

             call right0(il,jl,icelm,jcelm,ngav1)
             call solve(isol,g)                        !
             call bound                                !
             !!call right1 !! spar                    !
             call solve(isol,ui)                       !
             call psiful                               !
                                                       !
 234              continue                             !
                                                       !
                                                       !
              else                                     !
                                                       !
                 rl=rl0+ delrl                         !
                 zl=zl0+ delzl                         !
                      call shab(il,jl,icelm,jcelm)     !
              endif                                    !
                                                       !
                      !epsin=dmax1(epsin/2.,epsinl)     !

                      ich=0                            !
                                                       !
                rx1=rx10
                rx2=rx20

               endif                                   !
                                                       !
            endif !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(itl.ge.Nitl) then
            write(*,*) 'limit number of ext. iterations is exeeded'
            write(*,*) 'itl = ',itl,' Nitl = ',Nitl
            go to 1111
         endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if( ngav1.eq.4 .OR. ngav1.eq.5 )  then
             if(itin.ge.5) then
                call gridpl
                call qst(qcen,cnor,b0ax,r0ax)
             endif
         endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         call right0(il,jl,icelm,jcelm,ngav1)
         !write(6,*) 'erru = ',erru

             call solve(isol,g)
             call bound
             !!call right1 !!spar
             call solve(isol,ui)
             call psiful

       if(ngav1.eq.1 .OR. ngav1.eq.3 .OR. ngav1.eq.5) then
          if(itin.ge.3) then

             call btpol( betpol )
             zcoin =  betplx / betpol
C	       write(6,*) 'betplx/betpol',zcoin
             zcoin =  (1.d0/zcoin-1.d0)*tok/(cnor*f_cur)+1.d0
             coin =  1.d0/zcoin
             coin =  (coin-1.d0)*0.33d0+1.d0
             call taburs( 1,coin,nursb)

          endif
       endif

          ! call wrd


       crz=dabs(clr*rm*rm/(um-up))+dabs(clz*(zm-zx0)/(um-up))

       if(erru.le.epsin .AND. crz.lt.epscrz .AND. itl.ge.0
     *   .AND. ich.eq.0) go to 1111

       omega = omg
       sigma = sigm

	if(erru.le.eps.and.kpr.eq.1)then
	write(*,'("iter erru rm zm ", i4,6(1pe12.5))'),
     *  iter,erru,rm,zm

	write(*,'("ich  clr clz ", i4,6(1pe12.5))'),
     *  ich,clr,clz

	end if


       if(iter.le.nrun) go to 1000

 1111  continue

 
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                 tokout = tok          !!
                 pscout = um           !!
                 r_ax = rm             !!
                 z_ax = zm             !!
                 alp_b = alp           !!
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       if(ngrid.eq.1) then
		 call loop
           call wrd
c          call wrdcur
c          call adapol(alf0,alf1,alf2,bet0,bet1,bet2)
           call psi_fil( psicon,ncequi )

coment           call gridpl
coment           call qst(qcen,cnor,b0ax,r0ax)
       endif
       !call output(ngrid,betpol,zli3)
       call btpol(betpol)
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(ngrid.eq.0) then       !
                                   !
             alp=alp+0.00499999d0  !
                                   !
             ni=2*ni-1             !
             nj=2*nj-1             !
                                   !
             imax=2*imax-1         !
             jmax=2*jmax-1         !
                                   !
             ix1=2*ix1-1           !
             jx1=2*jx1-1           !
                                   !
             ix2=2*ix2-1           !
             jx2=2*jx2-1           !
                                   !
             call glbind           !
                                   !
             call grid             !
             call geom             !
                                   !
             call uplev            !
                                   !
             call extfil(pcequi,ncequi)
             call rddbnd       !
                                   !
             call psiful           !
                                   !
             call inter2(imax,jmax)!
                                   !
             call inter2(ix1,jx1)  !
                                   !
             call inter2(ix2,jx2)  !
                                   !
C...         if(nglev.eq.1) return !
                                   !
             iterbf=1              !
                                   !
             !!!!!!!!!!!           !
              Nitl = 15 !           !
             !!!!!!!!!!!           !
                                   !
             ngrid=1               !
             nflag=1               !
             isol=0                !
                                   !
             eps    = 1.0d-7       !
             eps0    = 2.0d-7      !
             epsin  = 1.0d-7       !
             epscrz = 5.0d-7      !
C                                  !
             ceps=0.02d0             !
             omega=1.d0              !
             sigma=1.d0              !
                                   !
           go to 777               !
                                   !
         endif                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         call tabnor(cnor)
         !!!!!!call tabper       !!!<<<---------
	   alf0n=alf0
         write(*,*) '..................................................'
         write(*,*) 'Convergence of free bound. equilibrium iterations:'
         write(*,*) '  iter = ',iter,' nrun = ',nrun
         write(*,*) '  itl  = ',itl ,' Nitl = ',Nitl
         write(*,*) '  erru = ',erru
         write(*,*) '  crz  = ',crz

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
       subroutine eq_ax( pcequi, psicon, ncequi, nstep,ngrid,
     *                 alf0, alf1, alf2, bet0, bet1, bet2,
     *                 betpol,  betplx, zli3,
     *                    ngav1,
     *                 ftok,tokout,psicen,pscout,
     *                 EREVE0, ERPS,
     *                 psi_bnd,alp_b,rax,zax,isymm )
c
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
c
          common /comomg/  omega,sigma
          common /comhel/  helinp, helout
          common/comsav/ alf0n
c
          integer isol,ngrid,nk,nstep,ngav1
          real*8   psicon(*),pcequi(*)
          real*8   alf0,alf1,alf2,bet0,bet1,bet2
          real*8   betpol,zli3,ftok,psicen,tokout,pscout
          real     timeb, timee,t_start, t_finish
C------------------------------------------------------------------
       !return
          if(nstep.gt.0) icont=1
	    nnstpp=nstep
C   -----------------------
          tok  = ftok
          ucen = psicen
          psi_bon = psi_bnd
C   -----------------------
          eps0   = EREVE0
          eps    = 1.d-7
          epsin  = 1.d-7
          epscrz = 1.d-7

          Nitl   = 9
          Nitin  = 61

          nrun   = 1000
          nwr    = 1000

          omg    = 0.99d0
          sigm   = 0.95d0

         iterbf  = 99999999

      	!!coin=alf0/(alf0n+1.d-12)
          !!call taburs( 1,coin,nursb)

C------------------------------------------------
         !call cpu_time(t_start)

             call ext_fil(pcequi,ncequi)

         !call cpu_time(t_finish)
         !write(*,*) 'ext_fil:opertime=',(t_finish-t_start)

             isol  = 1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

               clr=0.d0
               clz=0.d0

              call psiful

               erru=1.d0
               omega=1.d0
               sigma=1.d0
               ich=0
         iter=iter+1
         itin=itin+1

       !call wrd
	 !pause 'wrd'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c            if(itin.le.3) then
c                rx1=rx10
c                rx2=rx20
c            endif

             rl=rm
             zl=zm

             call shab(il,jl,icelm,jcelm)
c------------------------------------------------------------------
         if( ngav1.eq.4 .OR. ngav1.eq.5 )  then
             call gridpl
             call qst(qcen,cnor,b0ax,r0ax)
         endif
c------------------------------------------------------------------

             call right0(il,jl,icelm,jcelm,ngav1)
         !call cpu_time(t_finish)
         !write(*,*) 'right0:opertime=',(t_finish-t_start)

c            write(*,*) 'erru',erru

             call solve(isol,g)
         !call cpu_time(t_finish)
         !write(*,*) 'solv0:opertime=',(t_finish-t_start)
             call bound
             !!call right1 !! spar
             call solve(isol,ui)
         !call cpu_time(t_finish)
         !write(*,*) 'solv1:opertime=',(t_finish-t_start)

!!!!>>>>>>symmetr.case
         if(isymm.eq.1) then             
             call updown
         endif
!!!!>>>>>>symmetr.case 

             call psiful

       if( ngav1.eq.1 .OR. ngav1.eq.3 .OR. ngav1.eq.5 ) then

          call btpol( betpol )
             zcoin =  betplx / betpol
C	 write(*,*) 'betplx/betpol',zcoin
             zcoin =  (1.d0/zcoin-1.d0)*tok/(cnor*f_cur)+1.d0
             coin =  1.d0/zcoin

             coin =  (coin-1.d0)*0.33d0+1.d0

          call taburs( 1,coin,nursb)

       endif

         call psi_fil( psicon,ncequi )
         !call cpu_time(t_finish)
         !write(*,*) '+flux:opertime=',(t_finish-t_start)

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        tokout = tok   !!
                        pscout = um    !!
                 rax = rm              !!
                 zax = zm              !!
                 alp_b = alp           !!
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(erru.le.eps0) then
             call loop
             !call wrd
c             call wrdcur
            !call output(ngrid,betpol,zli3)
            ! call adapol(alf0,alf1,alf2,bet0,bet1,bet2)

coment         call gridpl
coment         call qst(qcen,cnor,b0ax,r0ax)
         !call btpol( betpol )
         !call bttor(bettor)

         endif

         ERPS=ERRU

          call tabnor(cnor)
	    alf0n=alf0
             !call wrd
         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine shab(il,jl,icelm,jcelm)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         do 10 i=1,ni1

         ic=i
         if( rl.ge.r(i) .AND. rl.lt.r(i+1) ) go to 11

 10      continue
 11      continue

         do 20 j=1,nj1

         jc=j
         if( zl.ge.z(j) .AND. zl.lt.z(j+1) ) go to 21

 20      continue
 21      continue

         icelm=ic
         jcelm=jc

          sdmn=rmax

         do k=0,1
           ilp=ic+k
           ri=r(ilp)
         do l=0,1
           jlp=jc+l
           zj=z(jlp)

           ddl=dsqrt( (rl-ri)**2 + (zl-zj)**2 )
          if(ddl.lt.sdmn) then
            sdmn=ddl
            il=ilp
            jl=jlp
          endif

         enddo
         enddo

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
         subroutine glbind
c
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
c
         ni1=ni-1
         nj1=nj-1
c
         ni2=ni-2
         nj2=nj-2

         nbnd=2*(ni1+nj1)

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
         subroutine botlev(nstep)
c
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
c
         do 10 i=1,ni
 10       r(i)=r(2*i-1)

         do 20 j=1,nj
 20       z(j)=z(2*j-1)

         do 100 i=1,ni
         do 110 j=1,nj

          ue(i,j)=ue(2*i-1,2*j-1)

         if(nstep.eq.0) go to 110

          ui(i,j)=ui(2*i-1,2*j-1)

 110     continue
 100     continue

         do 200 ib=1,nbnd
         do 200 ibs=1,nbnd

          binadg(ibs,ib)=binadg(2*ibs-1,2*ib-1)+binadg(2*ibs,2*ib-1)

 200     continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
         subroutine uplev
c
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
c
             nNil=(Ni+1)/2
             nNjl=(Nj+1)/2
c
         do 100 i=nnil,1,-1
         do 100 j=nnjl,1,-1

          ui(2*i-1,2*j-1)=ui(i,j)

 100     continue

         do 200 i=1,ni,2
         do 200 j=2,nj1,2

       ui(i,j)=(ui(i,j-1)*dz(j-1)+ui(i,j+1)*dz(j))/(dz(j-1)+dz(j))

 200     continue

         do 300 i=2,ni1,2
         do 300 j=1,nj

       ui(i,j)=(ui(i-1,j)*dr(i-1)+ui(i+1,j)*dr(i))/(dr(i-1)+dr(i))

 300     continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
         subroutine inter2(ip,jp)
c
         include 'double.inc'
         parameter(nshp=10)
         include 'param.inc'
         include 'comblc.inc'
c
          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)
c
          if(ip.le.2 .OR. ip.gt.ni2 .OR.
     +       jp.le.2 .OR. jp.gt.nj2) return


          nsh=1
          xs(nsh)=r(ip)
          ys(nsh)=z(jp)
         fun(nsh)=u(ip,jp)

        do 200 k=-2,2,2

          i= ip+k

        do 210 l=-2,2,2

          j= jp+l

          if(i.eq.ip .AND. j.eq.jp) go to 210
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)
         fun(nsh)=u(i,j)

 210    continue
 200    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

           rpi=r(ip)
           zpj=z(jp)
           upij=u(ip,jp)

        do 100 k=-2,2

          i= ip+k
          ri=r(i)
        do 110 l=-2,2

          j= jp+l
          zj=z(j)

        u(i,j)=upij + dp(1)*(ri-rpi) + dp(2)*(zj-zpj)
     +              + 0.5*dp(3)*(ri-rpi)*(ri-rpi)
     +              +     dp(4)*(ri-rpi)*(zj-zpj)
     +              + 0.5*dp(5)*(zj-zpj)*(zj-zpj)

 110    continue
 100    continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine psiful

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

          erru=0.d0

         do 10 i=1,ni
         do 10 j=1,nj
	 uold=u(i,j)

       u(i,j)=ui(i,j)+ue(i,j)+clz*z(j)+clr*r(i)*r(i)

         delu=dabs(u(i,j)-uold)
         del_um=dabs(um-up)+1.d-9
         erru=dmax1(delu/del_um,erru)

 10      continue

C        write(6,*)'erru',erru
C        write(6,*)'-------'

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	  subroutine tab_buil(alf0, alf1, alf2, bet0, bet1, bet2)

         include 'double.inc'
	   parameter(ntabp=500)
         real*8 psi(ntabp),q(ntabp),dp(ntabp),df(ntabp)

        write(fname,'(a,a)') path(1:kname),'eoh2.dat'
        open(1,file=fname,form='formatted')
	   !open(1,file='eoh2.DAT')
	        read(1,*) ntab
	        do i=1,ntab
	           read(1,*) psi(i),q(i),dp(i)
              enddo
              dpnor=dp(1)
	        do i=1,ntab
	           dp(i)=alf0*dp(i)/dpnor
	           zpsi=1.d0-psi(i)/psi(ntab)
                 df(i)=funfp(zpsi)
                !dp(i)=funpp(zpsi,alf0,alf1,alf2,bet0,bet1,bet2)
              enddo
         close(1)

        write(fname,'(a,a)') path(1:kname),'tabppf.dat'
        open(1,file=fname,form='formatted')
         !open(1,file='tabppf.dat')
              write(1,*) ntab
              do i=1,ntab
                 write(1,*) psi(i),dp(i),df(i)
              enddo
         close(1)

        write(fname,'(a,a)') path(1:kname),'tab_q.dat'
        open(1,file=fname,form='formatted')
	   !open(1,file='tab_q.dat')
              write(1,*) ntab, (0)
              do i=1,ntab
                 write(1,*) psi(i),q(i)
              enddo
         close(1)

          return
	    end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine tab_build
         include 'double.inc'
	parameter (np=500)
	real*8 ps(np),q(np),p(np),f(np)

       pi=3.14159265358d0

        write(fname,'(a,a)') path(1:kname),'tabpfq.dat'
        open(1,file=fname,form='formatted')
       !open(1,file='tabpfq.dat')
	      read(1,*) n
	      do i=1,n
	         read(1,*) ps(i),q(i),p(i),f(i)
               p(i)=p(i)*4.d-7*pi
            enddo
       close(1)

        write(fname,'(a,a)') path(1:kname),'tabppf.dat'
        open(1,file=fname,form='formatted')
       !open(1,file='tabppf.dat')
	      write(1,*) n
	      do i=1,n
	         write(1,*) ps(i),p(i),f(i)
            enddo
       close(1)

        write(fname,'(a,a)') path(1:kname),'tab_q.dat'
        open(1,file=fname,form='formatted')
	 !open(1,file='tab_q.dat')
            write(1,*) n, (0)
            do i=1,n
               write(1,*) ps(i),q(i)
            enddo
       close(1)

         !write(6,*) 'tab_build: tabppf.dat,tab_q.dat was created'
	 ! pause 'pause'

       return
	end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine q_pert(psizv)

         include'double.inc'

	   parameter (np=500)
	   real*8 ps(np),q(np)

        write(fname,'(a,a)') path(1:kname),'tab_q.dat'
        open(1,file=fname,form='formatted')
	   !open(1,file='tab_q.dat')
              read(1,*) n
              do i=1,n
                 read(1,*) ps(i),q(i)
              enddo
         close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  the first variant of q-perturbation ( psizv = psizv )
!
!         do i=2,n
!            if( ps(i) .gt. psizv ) then
!                izv = i-1
!	           qzv = q(i-1)
!                go to 10
!	    endif
!         enddo
!
! 10      continue
!
!         do i=1,izv
!            q(i)=qzv
!         enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  the second variant of q-perturbation ( psizv = qzv )
!
         qzv   = psizv
         psizv = 0.
         izv   = 0
         do i=1,n
            if( q(i) .lt. qzv ) then
                q(i)  = qzv
                izv   = i
                psizv = ps(i)
            endif
         enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
        write(fname,'(a,a)') path(1:kname),'tab_q.dat'
        open(1,file=fname,form='formatted')
	   !open(1,file='tab_q.dat')
              write(1,*) n, (0)
              do i=1,n
                 write(1,*) ps(i),q(i)
              enddo
         close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine updown

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

	     nj12= (nj+1)/2

         do 10 j=1,nj12
         do 10 i=1,ni

       udown=ui(i,j)
       uup=ui(i,nj-j+1)
	 usym=0.5d0*(uup+udown)
	 ui(i,j)=usym
	 ui(i,nj-j+1)=usym 
 10      continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine get_par(psi_bnd)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
           psi_bnd=up
         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






