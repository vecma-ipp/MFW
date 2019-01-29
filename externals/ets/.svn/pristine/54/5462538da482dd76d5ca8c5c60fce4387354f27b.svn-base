!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine eqa(
     *                  keyctr,igdf,nstep,platok, psax,i_betp,betplx, 
     *            rax,zax, rxpnt,zxpnt, psbo, psdel,
     *            rk,zk,nk, pcequi,ncequi, psitok,
     *            rloop,zloop,nloop,
     *            rprob,zprob,nprob,
     *             zli3,betpol,betful,     
     *                     necon,wecon,ntipe , nflag, errarr)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         ! include 'urs.inc'
         !parameter(nursp4=nursp+4,nursp6=nursp4*6)
         include 'compol.inc'
         include 'compol_add.inc'
c   -----------------------------

          real*8   rk(*),zk(*),psitok(*),wecon(*)
          real*8   rloop(*),zloop(*),rprob(*),zprob(*)
          real*8   pcequi(*)

          real*8   errarr(*)
          integer  ntipe(*),necon(*)
          integer  nk,nloop,nprob,nstep,keyctr

          real*8   alf0,alf1,alf2,bet0,bet1,bet2
          dimension alm(4,4),blm(4),xlm(4),iwrk(4)
          real time_beg,time_end,time_b,time_e,dtim1,dtim2,dtim3
          !dimension pstab(nursp),qtab(nursp)
          !real*8 RRK(nursp4),CCK(nursp4),WRK(nursp6)
           !real*8 CWK(4)
	common
     *  /c_kpr/kpr

          abs(xx)=dabs(xx)
          sqrt(xx)=dsqrt(xx)



             cnor=1.d0
             iplasm=iplas
             nroi=nr
             ntetj=nt
             ngav=keyctr

             epscrz=5.d-6
             epspsm=5.d-7
             epsfpv=5.d-4
             epsro =1.d-7

         rolim=ro(iplas,jrolim)

        if(ngav.ne.2) then
        fpv=0.d0
        endif


             !pause ' '

         !tok=tokf
         !qcen=qc


             itout=0
             iter=0
             itin=0
             ich=0



         call f_bndmat(rk,zk,nk,rloop,zloop,nloop,rprob,zprob,nprob)
                           !write(*,*)'f_bndmat'

 1000     continue

              iter=iter+1
              itin=itin+1

                          !write(*,*)'iter=',iter,itin
                          !write(*,*)'ich=',ich

             call f_metric
                           !write(*,*)'metric'
             call f_matcof
             call matpla 
                            !write(*,*)'matpla'
             call extpol
                           !write(*,*)'ext       '

             call rigext 
                          !write(*,*)'rigext    '

             call solext 

                           !write(*,*)'solext    '
             call f_matrix
                            !write(*,*)'matrix'

             call f_rightg
                          !write(*,*)'rightg'
!%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%
!          if(nstep.eq.1) then
!             call rightg_test
!             call rightp_test
!             call f_solve(0,g)
!             call g_test
!             stop
!          endif
!%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%

	    if(i_betp.eq.1) then
	      if(iter.gt.4) call skbetp(betplx,betpol)
          endif

             call f_solve(0,g)
                           !write(*,*)'solve(g)'

             call f_rightp
                          !write(*,*)'rightp'

             call f_solve(1,psii)

                           !write(*,*)'solve(psii)'

             call f_psiful
                        !call f_wrd
                        !call out
                          !write(*,*)'psiful    '
             call artfil
          if(kpr.eq.1) then
                          write(*,*)'artfil    ',clr,clz
                          write(*,*)'psiax,psim',psiax,psim
          endif
	       !if(ngav.gt.0) then
              !    call f_wrd
	        ! pause 'wrd'
	       !endif


            if(ngav.eq.0 .AND. igdf.eq.2) then
              call qst_b
              call grdef(igdf)
            endif


             call ada(erro)
                !  call f_wrd
	         !pause 'wrd'
          if(kpr.eq.1) then
                          write(*,*)'ada       '
                          write(*,*)'erro=',erro
          endif
!!!!!!!!!!!! accurasy  test parameters !!!!!!!!!!!!!!!

             cab=abs(clr)+abs(clz)

         if(ngav/10*10.eq.ngav) then

             errpsm=0.d0
             errfpv=0.d0

         elseif(ngav.eq.1) then

             errpsm=abs((psiax-psim)/psipla)
             errpsb=abs((psip-psibon)/psipla)
              fvv=sqrt(f(iplas)**2+fpv)
             !errfpv=abs((fvac-fvv)/(f(1)-f(iplas)))
             errfpv=0.d0

         elseif(ngav.eq.2) then

             errpsm=abs((psiax-psim)/psipla)
             errpsb=abs((psip-psibon)/psipla)

              fvv=sqrt(f(iplas)**2+fpv)
             errfpv=abs((fvac-fvv)/(f(1)-f(iplas)))

         endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if(erro.lt.epsro .OR. itin.gt.itrmax) then

          if(ich.ne.0) go to 2000

          if(kpr.eq.1) then
         write(*,*) 'itin',itin
         write(*,*) 'crz',cab
         write(*,*) 'errpsm',errpsm
         write(*,*) 'errpsb',errpsb
         write(*,*) 'errfpv',errfpv
          endif
          if( (cab.lt.epscrz) .AND.
     *        (errpsm.lt.epspsm) .AND.
     *        (errfpv.lt.epsfpv) ) go to 3000

          if( itout.gt.Nitmax ) go to 3000

                 go to 2000

        endif

             go to 1000

 2000     continue
             itin=0

        if(ich.eq.0) then

             cr0=clr
             cz0=clz
             pm0=psim
             fv0=f(iplas)**2+fpv

             rm0=rm
             zm0=zm
             rolim0=rolim
             fpv0=fpv

             ich=1

             drm=0.101*(ro(2,1)-ro(1,1))
             zm=zm+drm

                  !call f_wrd
             call reform
                         !write(*,*)'reform1   '
                  !call f_wrd

                        !pause ' '
             go to 1000

        elseif(ich.eq.1) then

             cr1=clr
             cz1=clz
             pm1=psim
             fv1=f(iplas)**2+fpv


             ich=2
             rm=rm0+drm
             zm=zm0
             rolim=rolim0
             fpv=fpv0

             call reform
                        ! write(6,*)'reform2   '
                        !pause ' '
             go to 1000

        elseif(ich.eq.2 .AND. ngav/10*10.eq.ngav) then

             cr2=clr
             cz2=clz

                 dcrdr=(cr2-cr0)/drm
                 dczdr=(cz2-cz0)/drm

                 dcrdz=(cr1-cr0)/drm
                 dczdz=(cz1-cz0)/drm

                 det=dcrdr*dczdz-dczdr*dcrdz

              delr= (cz0*dcrdz-cr0*dczdz)/det
              delz= (cr0*dczdr-cz0*dcrdr)/det

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              dell=sqrt(delr**2+delz**2)
              if(dell.gt.drm*10.d0) then
              nshift=dell/(drm*10.d0)

              delr=drm*10.d0*delr/dell
              delz=drm*10.d0*delz/dell

	    if(nshift.gt.10) nshift=10

              do 6793 ish=1,nshift

                         !write(6,*)'slow shift',ish,nshift
                  rm=rm0+ delr*ish
                  zm=zm0+ delz*ish

              !write(6,*) 'delr',delr
              !write(6,*) 'delz',delz
             !pause ' '

             call reform
                         !write(6,*)'reform3   '
                      !!!pause ' '
             call f_metric
             call extpol
             call f_matcof
             call matpla
             call rigext
             call solext
             call f_matrix
             call f_rightg
             call f_solve(0,g)
             call f_rightp
             call f_solve(1,psii)
             call f_psiful
             call artfil
                         !write(6,*)'artfil    ',clr,clz
             call ada(erro)
                         !write(6,*)'ada       '
                         !write(6,*)'erro=',erro
                         !write(6,*)'          '

 6793         continue


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              else

                  rm=rm0+ delr
                  zm=zm0+ delz

              !write(6,*) 'delr',delr
              !write(6,*) 'delz',delz
             !pause ' '

             call reform
                         !write(6,*)'reform3   '
                      !!!pause ' '
              endif

             ich=0
             itout=itout+1
             go to 1000

        elseif(ich.eq.2 .AND. ngav.gt.0) then

             cr2=clr
             cz2=clz
             pm2=psim
             fv2=f(iplas)**2+fpv

             ich=3
             rm=rm0
             zm=zm0
             drolim=-drm
             rolim=rolim0+drolim
             fpv=fpv0

             call reform
                        ! write(6,*)'reform3   '
                        !pause ' '
             go to 1000

        elseif(ich.eq.3 .AnD. ngav.eq.1) then

             cr3=clr
             cz3=clz
             pm3=psim

                 dcrdr=(cr2-cr0)/drm
                 dczdr=(cz2-cz0)/drm
                 dpmdr=(pm2-pm0)/drm

                 dcrdz=(cr1-cr0)/drm
                 dczdz=(cz1-cz0)/drm
                 dpmdz=(pm1-pm0)/drm

                 dcrdro=(cr3-cr0)/drolim
                 dczdro=(cz3-cz0)/drolim
                 dpmdro=(pm3-pm0)/drolim

              !write(6,*) 'deriv'


             alm(1,1)=dcrdr
             alm(1,2)=dcrdz
             alm(1,3)=dcrdro

             alm(2,1)=dczdr
             alm(2,2)=dczdz
             alm(2,3)=dczdro

             alm(3,1)=dpmdr
             alm(3,2)=dpmdz
             alm(3,3)=dpmdro
                               do ic=1,3
                               do jc=1,3
                                  !write(6,*)'i,j',ic,jc
                                  !write(6,*)'a(i,j)',Alm(ic,jc)
                               enddo
                               enddo

             blm(1)=-cr0
             blm(2)=-cz0
             blm(3)= psiax-pm0

              !write(6,*) 'mat a'

                 call ge(3,4,alm,blm,xlm,iwrk)

              !write(6,*) 'ge   '

             delr=xlm(1)
             delz=xlm(2)
             delro=xlm(3)

              !write(6,*) 'delr',delr
              !write(6,*) 'delz',delz
              !write(6,*) 'delro',delro
                      !!!pause ' '

                  rm=rm0+ delr
                  zm=zm0+ delz
                  rolim=rolim0+delro

             call reform
                         !write(6,*)'reform*** '
                      !!!pause ' '
             ich=0
             itout=itout+1
             go to 1000

        elseif(ich.eq.3 .AnD. ngav.gt.1) then

             cr3=clr
             cz3=clz
             pm3=psim
             fv3=f(iplas)**2+fpv

             ich=4
             rm=rm0
             zm=zm0
             rolim=rolim0
            !dfpv=0.005d0
             dfpv=(f(1)**2-fvac**2)*1.d-3
             fpv=fpv0+dfpv

             call reform
                        ! write(6,*)'reform4   '
                     !!! pause ' '
             go to 1000

        elseif(ich.eq.4) then

             cr4=clr
             cz4=clz
             pm4=psim
             fv4=f(iplas)**2+fpv

                 dcrdr=(cr2-cr0)/drm
                 dczdr=(cz2-cz0)/drm
                 dpmdr=(pm2-pm0)/drm
                 dfvdr=(fv2-fv0)/drm

                 dcrdz=(cr1-cr0)/drm
                 dczdz=(cz1-cz0)/drm
                 dpmdz=(pm1-pm0)/drm
                 dfvdz=(fv1-fv0)/drm

                 dcrdro=(cr3-cr0)/drolim
                 dczdro=(cz3-cz0)/drolim
                 dpmdro=(pm3-pm0)/drolim
                 dfvdro=(fv3-fv0)/drolim

                 dcrdf=(cr4-cr0)/dfpv
                 dczdf=(cz4-cz0)/dfpv
                 dpmdf=(pm4-pm0)/dfpv
                 dfvdf=(fv4-fv0)/dfpv

             ! write(6,*) 'deriv'


             alm(1,1)=dcrdr
             alm(1,2)=dcrdz
             alm(1,3)=dcrdro
             alm(1,4)=dcrdf

             alm(2,1)=dczdr
             alm(2,2)=dczdz
             alm(2,3)=dczdro
             alm(2,4)=dczdf

             alm(3,1)=dpmdr
             alm(3,2)=dpmdz
             alm(3,3)=dpmdro
             alm(3,4)=dpmdf

             alm(4,1)=dfvdr
             alm(4,2)=dfvdz
             alm(4,3)=dfvdro
             alm(4,4)=dfvdf
                               do ic=1,4
                               do jc=1,4
                                  !write(6,*)'i,j',ic,jc
                                  !write(6,*)'a(i,j)',Alm(ic,jc)
                               enddo
                               enddo

             blm(1)=-cr0
             blm(2)=-cz0
             blm(3)= psiax-pm0
             blm(4)= fvac**2-fv0

              !write(6,*) 'mat a'

                 call ge(4,4,alm,blm,xlm,iwrk)

              !write(6,*) 'ge   '

             delr=xlm(1)
             delz=xlm(2)
             delro=xlm(3)
             delfpv=xlm(4)

              !write(6,*) 'delr',delr
              !write(6,*) 'delz',delz
              !write(6,*) 'delro',delro
              !write(6,*) 'delfv',delfpv
                      !!!pause ' '

                  rm=rm0+ delr
                  zm=zm0+ delz
                  rolim=rolim0+delro
                  fpv=fpv0+delfpv

             call reform
                        ! write(6,*)'reform*** '
                      !!!pause ' '
             ich=0
             itout=itout+1
             go to 1000

         endif

 3000     continue


           nflag=0

           if(itout.gt.nitmax) nflag=1

              errarr(1)=erro    !  out of accuracy tests
              errarr(2)=cab     !
              errarr(3)=errpsm  !
              errarr(4)=errfpv  !



             ! if(ngav.gt.0 .AND. itin.lt.15) go to 1000

             ! if(ngav.eq.1 .AND. itout.lt.2) then
             !  call f_wrd
             !  go to 2000
             ! endif

              !if(ngav.eq.2 .AND. itout.lt.2) then
              ! f_call wrd
              ! go to 2000
              !endif

              if( itout.lt.1) then
              ! f_call wrd
               go to 2000
              endif

          !if(ngav/10*10.eq.ngav) then
          if(ngav.eq.0) then
           call qst_b
           psiax=psim
           psipla=psim-psip
           Fvac = f(iplas)
          endif

!              write(fname,'(a,a)') path(1:kname),'ddps0.pr'
!             open(1,file='ddps0.pr')
!             do 567 i=1,iplas
! 567           write(1,*) dfdpsi(i),f(i),i
!             close(1)
            if(kpr.eq.1) then
             write(*,*) 'iter itout',iter,itout
            endif
c----------------------------------------------------------
         !call f_flux(psitok,rk,zk,nk)
         !call flux_p(psitok,rk,zk,nk)
         !call flux_g(psitok,rk,zk,nk)
         call flux_r(psitok,ncequi)

           platok = tokp
           rax    = rm
           zax    = zm
           rxpnt    = rx0
           zxpnt    = zx0
           psax   = psim
           psbo   = psip
           psdel  = psim - psip

           call bt_pol(betpol)
           call f_wrd
            ! call out(rbnd,zbnd,zli3,betpol,betful,parpla)
C            write(6,*)'wrd '

         iter=0
         itin=0


          return
          end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine eqa_ax( dt,time,
     *                  keyctr,igdf,nstep,platok, psax,i_betp,betplx, 
     *                  rax,zax, rxpnt,zxpnt, psbo, psdel,
     *                  rk,zk,nk, pcequi,ncequi, psitok,
     *                  rloop,zloop,nloop, rprob,zprob,nprob,
     *                   zli3,betpol,betful,     
     *                     necon,wecon,ntipe , nflag, errarr)


         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'
c   -----------------------------

          real*8   rk(*),zk(*),psitok(*),wecon(*)
          real*8   pcequi(*)
          real*8   rloop(*),zloop(*),rprob(*),zprob(*)

          real*8   errarr(*)
          integer  ntipe(*),necon(*)
          integer  nk,nstep,keyctr

          real*8   alf0,alf1,alf2,bet0,bet1,bet2
          real time_beg,time_end,time_b,time_e,dtim1,dtim2,dtim3
	common
     *  /c_kpr/kpr

!          save nstepO
          integer nstepO

          abs(xx)=dabs(xx)
          sqrt(xx)=dsqrt(xx)

             ngav=keyctr
             kstep=nstep
             dtim=dt
             ctim=time

             itrmax=50
             Nitmax=5
             nitdel=7
             nitbeg=5

              cnor=1.d0

             !pause ' '

             call f_ext_fil(pcequi,ncequi)

C                          write(*,*)'extrec'

           if(nstep.ne.nstepO) then 
            itin=0
            erru=0.d0
            call renet
            !call f_wrd
          call f_bndmat(rk,zk,nk,rloop,zloop,nloop,rprob,zprob,nprob)
            !call f_wrd
            !pause 'pause'
           endif

 1000     continue

              iter=iter+1
              itin=itin+1

          if(kpr.eq.1) then
                           write(*,*)' '
                           write(*,*)'iter=',iter,itin
          endif
             call f_metric
C                           write(*,*)'metric'
             call f_matcof
C                           write(*,*)'matcof'
             call matpla
C                        !   write(*,*)'matpla'
             call extpol
C                           write(*,*)'ext       '
             call rigext
C                          ! write(*,*)'rigext    '
             call solext
C                         !  write(*,*)'solext    '
             call f_matrix
C                           write(*,*)'matrix'

             call f_rightg

C                           write(*,*)'rightg'

	    if(i_betp.eq.1) then
	      if(iter.gt.4) call skbetp(betplx,betpol)
          endif


             call f_solve(0,g)
                        !f_call wrd
                        !call out
                        !stop
C                           write(*,*)'solve(g)'
             call f_rightp
C                           write(*,*)'rightp'

             call f_solve(1,psii)

C                           write(*,*)'solve(psii)'

             !call flux_g(psitok,rk,zk,nk)
             !call flux_r(psitok,ncequi)
                          ! write(6,*)'flux'

             call f_psiful
                        !f_call wrd
                        !call out
C                          write(*,*)'psiful    '

            if(ngav.le.0 .AND. igdf.eq.2) then
              call qst_b
              call grdef(igdf)
            endif

             call f_remesh(erro)
                         erru=erro
             !call flux_p(psitok,rk,zk,nk)
             call flux_r(psitok,ncequi)

         if(ngav.eq.0) then

             errpsm=0.d0

         elseif(ngav.eq.1) then

             errpsm=abs((psiax-psim)/psipla)

         endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


C         write(*,*) 'itin',itin
C         write(*,*) 'errpsm',errpsm

 3000     continue


C             write(*,*) 'iter, itout ==', iter, itout
c----------------------------------------------------------

           platok = tokp
           rax    = rm
           zax    = zm
           rxpnt    = rx0
           zxpnt    = zx0
           psax   = psim
           psbo   = psip
           psdel  = psim-psip

           if(ngav.le.0) then
              psiax  = psim
              psipla = psim-psip
              !Fvac   = f(iplas)
           endif
         call bt_pol(betpol)
             !call f_wrd
             !call out(rbnd,zbnd,zli3,betpol,betful,parpla)
             !write(*,*)'f_wrd '

            nflag=0

              errarr(1) = erro    !  out of accuracy tests
              !errarr(2) = cab     !
              !errarr(3) = errpsm  !
              !errarr(4) = errfpv  !

         nstepO=nstep

          if(ngav.lt.0) call retab_L

          return
          end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

