        SUBROUTINE wrb

        INCLUDE 'double.inc' 
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'

        common /compsf/ psf(nrp), sqtor(nrp)
        common/selcon/ psi_d(nrp),fi_d(nrp),f_d(nrp),ri_d(nrp),
     *               ps_pnt(nrp),del_psb,psi_bn1

        common /com_jb/ BJ_av(nrp),curfi_av(nrp)
        common /com_b2/ B2_av(nrp)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       common/savt0/ psi0(nrp),fi0(nrp),f0(nrp),ri0(nrp),q0(nrp),
     *               dpsidt(nrp),dfidt(nrp),rm0,ac0n,skcen0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          common /com_volt/ upls(nrp)
          common /fp_dot/ dfpdt(1000),nna1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       character*10 case(6)

!        dimension psirz(np,np),fpol(np),pres(np),ffprim(np),
!     *            pprime(np),qpsi(np),rbbbs(nbp),zbbbs(nbp),
!     *            rlim(np),zlim(np)
 
        character*10 etitl(5), date
        
	common
     *  /c_kpr/kpr

             if(kpr.lt.0) return

        write(fname,'(a,a)') path(1:kname),'outp.wr'
        open(1,file=fname)
      !open(1,file='outp.wr')
           write(1,*) nr,nt,nr1,nt1,nr2,nt2,iplas
           write(1,*) ((r(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((z(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((cur(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((psi(i,j),i=1,iplas),j=1,nt)
           write(1,*)  (q(i),i=1,iplas)
           write(1,*)  (f(i),i=1,iplas)
      close(1)

        write(fname,'(a,a)') path(1:kname),'ddp.wr'
        open(1,file=fname)
      !open(1,file='ddp.wr')
           write(1,*) iplas
           write(1,*) (q(i),i=1,iplas)
           write(1,*) (f(i),i=1,iplas)
           write(1,*) (dfdpsi(i),i=1,iplas)
           write(1,*) (psia(i),i=1,iplas)
           write(1,*) (sqtor(i),i=1,iplas)
           !write(1,*)  (dpsidt(i),i=1,iplas)
           !write(1,*)  (upls(i),i=1,iplas)
           write(1,*) (dpdpsi(i),i=1,iplas)
           write(1,*) (BJ_av(i),i=1,iplas)
           !write(1,*) (curfi_av(i),i=1,iplas)
           write(1,*) (b2_av(i),i=1,iplas)
           !write(1,*) nna1
           !write(1,*)  (dfpdt(i),i=1,nna1)
      close(1)
        write(fname,'(a,a)') path(1:kname),'tabppf.wr'
        open(1,file=fname)
      !open(1,file='tabppf.wr')
           write(1,*) iplas
       do i=1,iplas
           write(1,*) 1.d0-psia(i),dpdpsi(i),dfdpsi(i)
       enddo
      close(1)

!      open(1,file='dps.wr')
!           do i=1,iplas
!              ddps=psia(i)*psim-psi_d(i)
!              ddfi=flx_fi(i)-fi_d(i)
!              ddf=f(i)-f_d(i)
!              write(1,*) ddps,ddfi,ddf,i
!           enddo
!              write(1,*) ' dpsidt from promat'
!           write(1,*) (ps_pnt(i),i=1,iplas)
!              write(1,*) 'del_psb from promat',del_psb
!
!      close(1)

        write(fname,'(a,a)') path(1:kname),'q.wr'
        open(1,file=fname)
      !open(1,file='q.wr')
           do i=1,iplas
            if(i.ne.iplas) then
          write(1,*) 1.d0-0.5d0*(psia(i)+psia(i+1)),0.5d0*q(i)/pi,i
            else
          write(1,*) 1.d0-psia(i),0.5d0*q(i)/pi,i
            endif
           enddo
      close(1)

      nrr=iplas

        write(fname,'(a,a)') path(1:kname),'efit_comp.wr'
        open(1,file=fname)
      !open(1,file='efit_comp.wr')

         write(1,2022) nrr,nt
         write(1,2020) rm,zm,psim*0.4d0*pi,psip*0.4d0*pi,tok*1.d3
         write(1,2020) (f(i)*0.4d0*pi,i=1,nrr-1)
         write(1,2020) (dpdpsi(i)*1.d7/4.d0/pi,i=1,nrr)
         write(1,2020) (dfdpsi(i)*0.4d0*pi,i=1,nrr)
         write(1,2020) ((r(i,j),i=1,nrr),j=1,nt)
         write(1,2020) ((z(i,j),i=1,nrr),j=1,nt)
         write(1,2020) ((psi(i,j)*0.4d0*pi,i=1,nrr),j=1,nt)
         write(1,2020) (q(i),i=1,nrr-1)
         write(1,2020) (r(nrr,j),z(nrr,j),j=1,nt)

      close(1)

        write(fname,'(a,a)') path(1:kname),'tab_bnd.wr'
        open(1,file=fname)
         !open(1,file='tab_bnd.wr') 
	     write(1,*) nt1 
	      do ib=1,nt1
	        write(1,*) r(iplas,ib),z(iplas,ib) 
	      enddo
         close(1) 

       ! open(1,file='gato_equi.wr')
       !     write (1,1000) date                     
       !     write (1,1000) (etitl(i),i=1,nft)
       ! close(1

 1000 format(6a8)
 1010 format(3i5)
 3000 format(1p4e19.12)

 2000 format(6a8,3i4)
 2020 format(5e16.9)
 2022 format(2i5)

        ! write(*,*) 'wrb:writing iz done'
         !pause 'wrb:pause'
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE rd_step(numwr,dt,time,istep,psex_bnd,psi0_bnd)
C
        INCLUDE 'double.inc'
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
C
        !dimension psiplb(*),psiexb(*)
        character*40 str,dummy

        write(fname,'(a,a)') path(1:kname),'nmwr.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='nmwr.wr',form='formatted')
            read(1,*) numbwr
         close(1)

         if(numwr.lt.10) then
              write(str,'(a,a,i1,a)') path(1:kname),'step',numwr,'.wr'
         elseif(numwr.lt.100) then
              write(str,'(a,a,i2,a)') path(1:kname),'step',numwr,'.wr'
         else
              write(str,'(a,a,i3,a)') path(1:kname),'step',numwr,'.wr'
         endif

         open(1,file=str,form='formatted')
           read(1,*) nr,nt,iplas,istep,dt,time
           read(1,*) psex_bnd,rm,zm,psim,psi0_bnd,platok
           read(1,*) ((r(i,j),i=1,iplas),j=1,nt)
           read(1,*) ((z(i,j),i=1,iplas),j=1,nt)
           read(1,*) ((ro(i,j),i=1,iplas),j=1,nt)
           read(1,*) (teta(j),j=1,nt)
           read(1,*) ((psi(i,j),i=1,iplas),j=1,nt)
           read(1,*) ((psin(i,j),i=1,iplas),j=1,nt)
           read(1,*) (psia(i),i=1,iplas)
           !read(1,*) ((cur(i,j),i=1,iplas),j=1,nt)
           !read(1,*)  (q(i),i=1,iplas)
           !read(1,*)  (f(i),i=1,iplas)
           !read(1,*) (dfdpsi(i),i=1,iplas)
           !read(1,*) (dpdpsi(i),i=1,iplas)
         close(1)

         !open(1,file='wlist.wr',form='formatted')
         ! if(numwr.eq.1) then
         !     write(1,*) str
         ! else
         !  do i=1,numwr-1
         !     read(1,*) dummy
         !  enddo
         !     write(1,*) str
         ! endif
         !close(1)

C---------------------------------------------------------------
            RETURN
            END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE wrb0

        INCLUDE 'double.inc' 
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'

        common /compsf/ psf(nrp), sqtor(nrp)
        common/selcon/ psi_d(nrp),fi_d(nrp),f_d(nrp),ri_d(nrp),
     *               ps_pnt(nrp),del_psb,psi_bn1

        common /com_jb/ BJ_av(nrp),curfi_av(nrp)
        common /com_b2/ B2_av(nrp)

        character*10 case(6)

!        dimension psirz(np,np),fpol(np),pres(np),ffprim(np),
!     *            pprime(np),qpsi(np),rbbbs(nbp),zbbbs(nbp),
!     *            rlim(np),zlim(np)
 
        character*10 etitl(5), date



        write(fname,'(a,a)') path(1:kname),'outp0.wr'
        open(1,file=fname,form='formatted')
      !open(1,file='outp0.wr')
           write(1,*) nr,nt,nr1,nt1,nr2,nt2,iplas
           write(1,*) ((r(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((z(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((cur(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((psi(i,j),i=1,iplas),j=1,nt)
           write(1,*)  (q(i),i=1,iplas)
           write(1,*)  (f(i),i=1,iplas)
      close(1)

        write(fname,'(a,a)') path(1:kname),'ddp0.wr'
        open(1,file=fname,form='formatted')
      !open(1,file='ddp0.wr')
           write(1,*) iplas
           write(1,*) (q(i),i=1,iplas)
           write(1,*) (f(i),i=1,iplas)
           write(1,*) (dfdpsi(i),i=1,iplas)
           write(1,*) (psia(i),i=1,iplas)
           write(1,*) (sqtor(i),i=1,iplas)
           write(1,*) (dpdpsi(i),i=1,iplas)
           write(1,*) (curfi_av(i),i=1,iplas)
           write(1,*) (b2_av(i),i=1,iplas)
      close(1)

        write(fname,'(a,a)') path(1:kname),'dps.wr'
        open(1,file=fname,form='formatted')
      !open(1,file='dps.wr')
           do i=1,iplas
              ddps=psia(i)*psim-psi_d(i)
              ddfi=flx_fi(i)-fi_d(i)
              ddf=f(i)-f_d(i)
              write(1,*) ddps,ddfi,ddf,i
           enddo
              write(1,*) ' dpsidt from promat'
           write(1,*) (ps_pnt(i),i=1,iplas)
              write(1,*) 'del_psb from promat',del_psb

      close(1)

        write(fname,'(a,a)') path(1:kname),'q0.wr'
        open(1,file=fname,form='formatted')
      !open(1,file='q0.wr')
           do i=1,iplas
            if(i.ne.iplas) then
          write(1,*) 1.d0-0.5d0*(psia(i)+psia(i+1)),0.5d0*q(i)/pi,i
            else
          write(1,*) 1.d0-psia(i),0.5d0*q(i)/pi,i
            endif
           enddo
      close(1)

      nrr=iplas

        write(fname,'(a,a)') path(1:kname),'efit_comp.wr'
        open(1,file=fname,form='formatted')
      !open(1,file='efit_comp.wr')

         write(1,2000) nrr,nt
         write(1,2020) rm,zm,psim*0.4d0*pi,psip*0.4d0*pi,tok*1.d3
         write(1,2020) (f(i)*0.4d0*pi,i=1,nrr-1)
         write(1,2020) (dpdpsi(i)*1.d7/4.d0/pi,i=1,nrr)
         write(1,2020) (dfdpsi(i)*0.4d0*pi,i=1,nrr)
         write(1,2020) ((r(i,j),i=1,nrr),j=1,nt)
         write(1,2020) ((z(i,j),i=1,nrr),j=1,nt)
         write(1,2020) ((psi(i,j)*0.4d0*pi,i=1,nrr),j=1,nt)
         write(1,2020) (q(i),i=1,nrr-1)
         write(1,2020) (r(nrr,j),z(nrr,j),j=1,nt)

      close(1)

       ! open(1,file='gato_equi.wr')
       !     write (1,1000) date                     
       !     write (1,1000) (etitl(i),i=1,nft)
       ! close(1

 1000 format(6a8)
 1010 format(3i5)
 3000 format(1p4e19.12)

 2000 format(6a8,3i4)
 2020 format(5e16.9)
 2022 format(2i5)

         write(*,*) 'wrb:writing iz done'
         !pause 'wrb:pause'
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       SUBROUTINE out_b

       INCLUDE 'double.inc'
       INCLUDE 'dim.inc'
       INCLUDE 'compol.inc'

!       write(17,*) '**************************************************'
!       write(17,*) 'Print from "call out_b":'
!      write(17,*) '---------------------'
!       write(17,*) 'Grid size parameters:'
!       write(17,*) '                     '
!       write(17,*) 'iplas =',iplas
!       write(17,*) 'nr    =',nr
!       write(17,*) 'nt    =',nt
!       write(17,*) '---------------------------'
!       write(17,*) 'COMMON /com_pt/ parameters:'
!       write(17,*) '                      '
!       write(17,*) 'Mag. axis coordinates:'	      
!       write(17,*) 'rm    =',rm
!       write(17,*) 'zm    =',zm
!       write(17,*) '       '
!       write(17,*) 'psiax =',psiax 
!       write(17,*) 'psibon=',psibon
!       write(17,*) 'psipla=',psipla
!       write(17,*) 'psip  =',psip
!       write(17,*) 'psim  =',psim
!       write(17,*) '---------------------------'
!       write(17,*) 'COMMON /com_cn/ parameters:'
!       write(17,*) '                      '      
!       write(17,*) 'tok   =',tok
!       write(17,*) 'tokp  =',tokp
!       write(17,*) 'cnor  =',cnor
!       write(17,*) 'qcen  =',qcen
!       write(17,*) 'b0ax  =',b0ax
!       write(17,*) '*************************************************'
!       write(17,*) 'q(i): i=1,iplas =', iplas
!       write(17,*) '                 '
!       write(17,*) (q(i), i=1,iplas)
!       write(17,*) '*************************************************'
!       write(17,*) 'f(i): i=1,iplas =', iplas
!       write(17,*) '                 '
!       write(17,*) (f(i), i=1,iplas)
!       write(17,*) '*************************************************'
!       write(17,*) 'dfdpsi(i): i=1,iplas =', iplas
!       write(17,*) '                      '
!       write(17,*) (dfdpsi(i), i=1,iplas)
!       write(17,*) '*************************************************'
!       write(17,*) 'dpdpsi(i): i=1,iplas =', iplas
!       write(17,*) '                      '
!       write(17,*) (dpdpsi(i), i=1,iplas)
!       write(17,*) '*************************************************'

C       do 10 i=1,iplas,5

C            write(17,*) 'r(i,j): j=1,nt; i=',i
C            write(17,*) (r(i,j),j=1,nt)
C            write(17,*) 'z(i,j): j=1,nt; i=',i
C            write(17,*) (z(i,j),j=1,nt)
C            write(17,*) 'cur(i,j): j=1,nt; i=',i
C            write(17,*) (cur(i,j),j=1,nt)
C            write(17,*) 'psi(i,j): j=1,nt; i=',i
C            write(17,*) (psi(i,j),j=1,nt)

C  10    continue

       RETURN
       END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE wrdump(numwr,time,istep,psiplb,psiexb,psimag,flu_tor)
C
        INCLUDE 'double.inc'
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
C
        dimension psiplb(*),psiexb(*),psimag(*),flu_tor(*)
        character*40 str,dummy

        write(fname,'(a,a)') path(1:kname),'nmwr.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='nmwr.wr',form='formatted')
            write(1,*) numwr
         close(1)

         if(numwr.lt.10) then
               write(str,'(a,a,i1,a)') path(1:kname),'writ',numwr,'.wr'
         else
           if(numwr.lt.100) then
               write(str,'(a,a,i2,a)') path(1:kname),'writ',numwr,'.wr'
         else
               write(str,'(a,a,i3,a)') path(1:kname),'writ',numwr,'.wr'
         endif
         endif

         open(1,file=str,form='formatted')
           write(1,*) nr,nt,nr1,nt1,nr2,nt2,iplas,istep
           write(1,*) ((r(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((z(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((cur(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((psi(i,j),i=1,iplas),j=1,nt)
           write(1,*)  (q(i),i=1,iplas)
           write(1,*)  (f(i),i=1,iplas)
           write(1,*) (dfdpsi(i),i=1,iplas)
           write(1,*) (psia(i),i=1,iplas)
           write(1,*) (dpdpsi(i),i=1,iplas)
           write(1,*) (psiplb(i),i=1,istep)
           write(1,*) (psiexb(i),i=1,istep)
           write(1,*) (psimag(i),i=1,istep)
           write(1,*) (psimag(i),i=1,istep)
           write(1,*) (flu_tor(i),i=1,istep)
         close(1)

        write(fname,'(a,a)') path(1:kname),'wlist.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='wlist.wr',form='formatted')
          if(numwr.eq.1) then
              write(1,*) str
          else
           do i=1,numwr-1
              read(1,*) dummy
           enddo
              write(1,*) str
          endif
         close(1)

C---------------------------------------------------------------
            RETURN
            END


C***************************************************************
!********************************************************************

         subroutine tab_efit( tokf, psax, eqdfn, rax,zax, b0,r0 )
         
         use ppf_modul       
         use bnd_modul       

         include   'double.inc'
	   parameter (np=1000,nbp=np*4)

	   real*8 ps(np),p(np),f(np),q(np)
         character*10 case(6)

         dimension psirz(np,np),
     *             rbbbs(nbp),zbbbs(nbp)

	   common/com_eqd/ fpol(np),pres(np),qpsi(np),
     *                   ffprim(np),pprime(np),
     *                   rlimtr(np),zlimtr(np),case,simag,sibry,
     *                   idum,nw,nh,limitr

!         dimension psirz(np,np),fpol(np),pres(np),ffprim(np),
!     *             pprime(np),qpsi(np),rbbbs(nbp),zbbbs(nbp),
!     *             rlim(np),zlim(np) 

         common/efites/ fcefit,rcentr,iefit
	   common/comefi/ x(np),y(np),u(np,np)
	   common /c_kpr/ kpr
         character*40   eqdfn


C--------------------------------------------------------------------
         iefit=1                  
         pi=3.1415926535898d0
         amu0=0.4d0*pi

      write(*,*) '************************* '
      write(*,*) ' Entry of subr."tab_efit":'
      write(*,*) '------------------------- '

        write(fname,'(a,a40)') path(1:kname),eqdfn
        open(1,file=fname,form='formatted')
         !open(1,file=eqdfn)

	        read(1,2000) (case(i),i=1,6),idum,nw,nh
              write(*,*) idum,nw,nh

              read(1,2020) rdim,zdim,rcentr,rleft,zmid
              read(1,2020) rmaxis,zmzxis,simag,sibry,bcentr
              read(1,2020) current,simag,xdum,rmaxis,xdum
              read(1,2020) zmaxis,xdum,sibry,xdum,xdum
              read(1,2020) (fpol(i),i=1,nw)
              read(1,2020) (pres(i),i=1,nw)
              read(1,2020) (ffprim(i),i=1,nw)
              read(1,2020) (pprime(i),i=1,nw)
              read(1,2020) ((psirz(i,j),i=1,nw),j=1,nh)
              read(1,2020) (qpsi(i),i=1,nw)
              read(1,2022) nbbbs,limitr
              read(1,2020) (rbbbs(i),zbbbs(i),i=1,nbbbs)
              read(1,2020) (rlimtr(i),zlimtr(i),i=1,limitr)

         close(1)

              p_intgr=0.0d0
              
              do i=1,nw
               p_intgr=p_intgr+pprime(i)                           
              enddo             

              if(p_intgr.lt.0.d0) then
               i_sign=-1
              else 
               i_sign= 1
              endif               

          simag=simag*i_sign
          sibry=sibry*i_sign
          bcentr=dabs(bcentr)
          current=dabs(current)
          do i=1,nw
           fpol(i)=fpol(i)*i_sign
           ffprim(i)=ffprim(i)*i_sign
           pprime(i)=pprime(i)*i_sign
           do j=1,nh
            psirz(i,j)=psirz(i,j)*i_sign
           enddo             
          enddo             
          
         rax = rmaxis
         zax = zmaxis

         !b0  = -bcentr*10.d0/4.d0/pi
         b0  = bcentr
         r0  = rcentr
         f0c = b0*rcentr

!      write(*,*) 'efit:b0,fvac,rcentr',b0,f0c,rcentr
        !fvefit=-fpol(nw)*10.d0/4.d0/pi
        fvefit=fpol(nw)
!      write(*,*) 'efit:fvefit',fvefit

 2000   format(6a8,3i4)
 2020   format(5e16.9)
 2022   format(2i5)

C------------------------------------------------
 
       if(allocated(pstab))
     % deallocate( pstab, pptab, fptab )
       allocate( pstab(nw), pptab(nw), fptab(nw) )
     
       nutab=nw
!        write(fname,'(a,a)') path(1:kname),'tabppf.dat'
!        open(1,file=fname,form='formatted')
!         !open(1,file='tabppf.dat')

!	     write(1,*) nw
	     do i=1,nw
            pstab(i)= dfloat(i-1)/dfloat(nw-1)
            pptab(i)= pprime(i)*amu0*1.d-6
            fptab(i)= ffprim(i)
            
!	      !write(1,*) ps(i),-pprime(i)*1.d-6,-ffprim(i)*10.d0/(4.d0*pi)
!	      write(1,*) ps(i),pprime(i)*amu0*1.d-6,ffprim(i)
           enddo

!         close(1)
      !deallocate( pstab, pptab, fptab )


C------------------------------------------------
!        write(fname,'(a,a)') path(1:kname),'tab_q.dat'
!        open(1,file=fname,form='formatted')
!	   !open(1,file='tab_q.dat')
!
!              write(1,*) nw
!              do i=1,nw
!                 write(1,*) ps(i),qpsi(i)
!              enddo
!
!         close(1)
C------------------------------------------------
       if(allocated(rbtab))
     % deallocate( rbtab, zbtab )
       allocate( rbtab(nbbbs), zbtab(nbbbs) )
     
!        write(fname,'(a,a)') path(1:kname),'tab_bnd.dat'
!        open(1,file=fname,form='formatted')
!	    !open(1,file='tab_bnd.dat')

!              write(1,*) nbbbs
!              write(6,*) 'nbbbs',nbbbs

              j=0
              do i=nbbbs,1,-1
                 j=j+1
                 !write(1,*) rbbbs(i),zbbbs(i)
                 rbtab(j)=rbbbs(i)                 
                 zbtab(j)=zbbbs(i)                 
              enddo

          close(1)
          
      !deallocate( rbtab, zbtab )
C------------------------------------------------

!       write(*,*) 'rmaxis, zmaxis == ', rmaxis, zmaxis
!       write(*,*) 'current = ', current
!       write(*,*) 'simag, sibry == ', simag, sibry

!       write(*,*) '------ '
!	 write(*,*) 'Files: '
!       write(*,*) '      "tabppf.dat", "tab_q.dat", "tab_bnd.dat" '
!	 write(*,*) 'have been created'
!       write(*,*) '-----------------'	           
       !stop
C------------------------------------------------
!
!        write(fname,'(a,a)') path(1:kname),'fpol.wr'
!        open(1,file=fname,form='formatted')
!       !open(1,file='fpol.wr')
!            write(1,*) (fpol(i),i=1,nw)
!       close(1)
C------------------------------------------------

       !fcefit = -fpol(nw)*10.d0/4.d0/pi
       fcefit = fpol(nw)
       tokf   = current*1.d-6
       !psax   = (sibry-simag)*10.d0/4.d0/pi
        psax   = -(sibry-simag)

!      write(* ,*) '------------------------- '
!      write(* ,*) ' Exit of subr."tab_efit".'
!      write(* ,*) '************************* '
!      write(17,*) '------------------------- '
!      write(17,*) ' Exit of subr."tab_efit".'
!      write(17,*) '************************* '

       return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE wr_spik

        INCLUDE 'double.inc' 
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'

	common
     *  /c_kpr/kpr

         if(kpr.lt.0) return

        write(fname,'(a,a)') path(1:kname),'spik.wr'
        open(1,file=fname,form='formatted')
      !open(1,file='spik.wr',FORM='UNFORMATTED',STATUS='UNKNOWN')
         nm=iplas*nt1
       write(1,*) iplas,nt1,nm,psim,psibon,1
       !write(1) iplas,nt1,nm,psim,psibon,1
       !write(1,*) 'psn'
       write(1,*)(dsqrt(1.d0-psia(i)),i=1,iplas),
     * (dpdpsi(i),i=1,iplas),
     * (dfdpsi(i),i=1,iplas),
     *  (r(1,j),j=1,nt1),
     *  (z(1,j),j=1,nt1),
     *  (r(iplas,j),j=1,nt1),
     *  (z(iplas,j),j=1,nt1),
     *  ((ro(i,j)/ro(iplas,j),j=1,nt1),i=1,iplas),
     *  (q(i)/2.d0/pi,i=1,iplas),fvac
      close(1)


      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






