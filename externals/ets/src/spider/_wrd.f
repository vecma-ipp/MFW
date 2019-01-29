        SUBROUTINE wrd_tim

        INCLUDE 'double.inc'
        INCLUDE 'comtim.inc'
        common /nostep/ kstep

        write(fname,'(a,a)') path(1:kname),'eq_tim.wr'
        open(1,file=fname)
         !open(1,file='eq_tim.wr')

           kwr=kstep

         write(1,*)  kwr
         write(1,*)  (time_t(i),i=1,kwr)
         write(1,*)  (torcur(i),i=1,kwr)
         write(1,*)  (rm_t(i),i=1,kwr)
         write(1,*)  (zm_t(i),i=1,kwr)
         write(1,*)  (rxp_t(i),i=1,kwr)
         write(1,*)  (zxp_t(i),i=1,kwr)
         write(1,*)  (betpol_t(i),i=1,kwr)
         write(1,*)  (bettor_t(i),i=1,kwr)
         write(1,*)  (psim_t(i),i=1,kwr)
         write(1,*)  (psib_t(i),i=1,kwr)
      
         close(1)
         !write(*,*) 'wrd_tim:writing is done','time=',time_t(kwr)

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE eq_dump

        INCLUDE 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
           include 'compol_add.inc'

        write(fname,'(a,a)') path(1:kname),'eq_dump.wr'
        open(1,file=fname)
         !open(1,file='eq_dump.wr')

         write(1,*) nr,nt,nr1,nt1,nr2,nt2,iplas,iplas1
         write(1,*) rm,zm,psim,psip,rx0,zx0,rx1,zx1,rx2,zx2
         write(1,*) psiax,psibon,ixp1,jxp1,ixp2,jxp2,fvac,fpv
         write(1,*) alp,alpnew,tok,tokp,cnor,qcen,pscen,psipla
         write(1,*) rl,zl,clr,clz,rolim,jrolim,iterbf

         write(1,*) ((r(i,j),i=1,nr),j=1,nt)
         write(1,*) ((z(i,j),i=1,nr),j=1,nt)
         write(1,*) ((ro(i,j),i=1,nr),j=1,nt)
         write(1,*) ((cur(i,j),i=1,nr),j=1,nt)
         write(1,*) ((psi(i,j),i=1,nr),j=1,nt)
         write(1,*) ((ronor(i,j),i=1,nr),j=1,nt)
         write(1,*) ((psie(i,j),i=1,nr),j=1,nt)
         write(1,*) (teta(j),j=1,nt)
         write(1,*)  (q(i),i=1,iplas)
         write(1,*)  (f(i),i=1,iplas)
         write(1,*)  (dpdpsi(i),i=1,iplas)
         write(1,*)  (dfdpsi(i),i=1,iplas)
         write(1,*)  (psia(i),i=1,iplas)
         write(1,*)  (dpsda(i),i=1,iplas)

         close(1)
         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE eq_dump_rd

        INCLUDE 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
           include 'compol_add.inc'

        write(fname,'(a,a)') path(1:kname),'eq_dump.wr'
        open(1,file=fname)
         !open(1,file='eq_dump.wr')

         read(1,*) nr,nt,nr1,nt1,nr2,nt2,iplas,iplas1
         read(1,*) rm,zm,psim,psip,rx0,zx0,rx1,zx1,rx2,zx2
         read(1,*) psiax,psibon,ixp1,jxp1,ixp2,jxp2,fvac,fpv
         read(1,*) alp,alpnew,tok,tokp,cnor,qcen,pscen,psipla
         read(1,*) rl,zl,clr,clz,rolim,jrolim,iterbf

         read(1,*) ((r(i,j),i=1,nr),j=1,nt)
         read(1,*) ((z(i,j),i=1,nr),j=1,nt)
         read(1,*) ((ro(i,j),i=1,nr),j=1,nt)
         read(1,*) ((cur(i,j),i=1,nr),j=1,nt)
         read(1,*) ((psi(i,j),i=1,nr),j=1,nt)
         read(1,*) ((ronor(i,j),i=1,nr),j=1,nt)
         read(1,*) ((psie(i,j),i=1,nr),j=1,nt)
         read(1,*) (teta(j),j=1,nt)
         read(1,*)  (q(i),i=1,iplas)
         read(1,*)  (f(i),i=1,iplas)
         read(1,*)  (dpdpsi(i),i=1,iplas)
         read(1,*)  (dfdpsi(i),i=1,iplas)
         read(1,*)  (psia(i),i=1,iplas)
         read(1,*)  (dpsda(i),i=1,iplas)

         close(1)
         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE f_wrd

        INCLUDE 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
           include 'compol_add.inc'

       common/compsf/ psf(nrp),sqtor(nrp)
       common /com_jb/ BJ_av(nrp),curfi_av(nrp)
       common/com_but/ sigma(nrp),cbut_b(nrp)

        write(fname,'(a,a)') path(1:kname),'outp.wr'
        open(1,file=fname)
         !open(1,file='outp.wr')

         write(1,*) nr,nt,nr1,nt1,nr2,nt2,iplas

         write(1,*) ((r(i,j),i=1,nr),j=1,nt)
         write(1,*) ((z(i,j),i=1,nr),j=1,nt)
         write(1,*) ((g(i,j),i=1,nr),j=1,nt)
         write(1,*) ((cur(i,j),i=1,nr),j=1,nt)
         write(1,*) ((psi(i,j),i=1,nr),j=1,nt)
         write(1,*) ((psii(i,j),i=1,nr),j=1,nt)
         write(1,*) ((psie(i,j),i=1,nr),j=1,nt)
         write(1,*) ((aex(i,j),i=1,nr),j=1,nt)
         write(1,*)  (q(i),i=1,iplas)
         write(1,*)  (f(i),i=1,iplas)
         write(1,*)  rm,zm,rx0,zx0,ctim,kstep
         close(1)

        write(fname,'(a,a)') path(1:kname),'ddp.wr'
        open(1,file=fname)
             !open(1,file='ddp.wr')
               write(1,*) iplas
               write(1,*) (q(i),i=1,iplas)
               write(1,*) (f(i),i=1,iplas)
               write(1,*) (dfdpsi(i),i=1,iplas)
           write(1,*) (psia(i),i=1,iplas)
           write(1,*) (psf(i),i=1,iplas)
           write(1,*) (dpdpsi(i),i=1,iplas)
           write(1,*) (curfi_av(i),i=1,iplas)
           write(1,*) (sigma(i),i=1,iplas)
           write(1,*) (cbut_b(i),i=1,iplas)
           write(1,*)  ctim,kstep
             close(1)

        write(fname,'(a,a)') path(1:kname),'dps.wr'
        open(1,file=fname)
             !open(1,file='dps.wr')
               do i=1,iplas
                 ddps=psi(i,2)-psf(i)
                 write(1,*) ddps,i
               enddo
             close(1)

                 !write(*,*) 'wrd:writting is done'

            RETURN
            END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE rdfb

        INCLUDE 'double.inc' 
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
           include 'compol_add.inc'

        write(fname,'(a,a)') path(1:kname),'outfb.wr'
        open(1,file=fname)
       !open(1,file='outfb.wr')
           !read(1,*) nr,nt,nr1,nt1,nr2,nt2,iplas
           read(1,*) idum,nt,idum,nt1,idum,nt2,iplas
           read(1,*) ((r(i,j),i=1,iplas),j=1,nt)
           read(1,*) ((z(i,j),i=1,iplas),j=1,nt)
           read(1,*) ((ro(i,j),i=1,iplas),j=1,nt)
           read(1,*) ((ronor(i,j),i=1,iplas),j=1,nt)
           read(1,*)  (teta(j),j=1,nt)
           read(1,*) ((cur(i,j),i=1,iplas),j=1,nt)
           read(1,*) ((psin(i,j),i=1,iplas),j=1,nt)
           read(1,*)  (q(i),i=1,iplas)
           read(1,*)  (f(i),i=1,iplas)
           read(1,*)  (dfdpsi(i),i=1,iplas)
           read(1,*)  (dpdpsi(i),i=1,iplas)
           read(1,*)  (psia(i),i=1,iplas)
       close(1)

!       open(1,file='tab_bnd.dat')
!           write(1,*) nt2
!        do j=2,nt1
!           write(1,*) r(iplas,j),z(iplas,j)
!        enddo
!       close(1)

        RETURN
        END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE out(rbnd,zbnd,zli3,betpol,bettot,parpla)

        INCLUDE 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
           include 'compol_add.inc'

         dimension rbnd(*),zbnd(*),press(nrp)
         dimension Bro(nrp,ntp),Btet(nrp,ntp)
         dimension parpla(*)

         common /volpla/ Vol_pl


        write(fname,'(a,a)') path(1:kname),'out.pr'
        open(1,file=fname)
         !open(1,file='out.pr',form='formatted')

           do 100 j=1,nt

            rbnd(j)=r(iplas,j)
            zbnd(j)=z(iplas,j)

 100       continue

           rmx=rm
           rmn=rm

           zmx=zm
           zmn=zm

           do 110 j=2,nt1

            if(rmx.lt.rbnd(j)) then

               rmx = rbnd(j)
               z_rmx = zbnd(j)

            endif

            if(rmn.gt.rbnd(j)) then

               rmn = rbnd(j)
               z_rmn = zbnd(j)

            endif

            if(zmx.lt.zbnd(j)) then

               zmx = zbnd(j)
               r_zmx = rbnd(j)

            endif

            if(zmn.gt.zbnd(j)) then

               zmn = zbnd(j)
               r_zmn = rbnd(j)

            endif

 110       continue

            a_cen=0.5d0*(rmx-rmn)
            R_cen=0.5d0*(rmx+rmn)

            aspect=R_cen/a_cen

            write(1,*) ' right plasma bound:'
            write(1,*) ' Rmax=',rmx,' z(rmax)=',z_rmx

            write(1,*) ' left plasma bound:'
            write(1,*) ' Rmin=',rmn,' z(rmin)=',z_rmn

            write(1,*) ' upper plasma bound:'
            write(1,*) ' Zmax=',Zmx,' r(zmax)=',r_zmx

            write(1,*) ' lower plasma bound:'
            write(1,*) ' Zmin=',Zmn,' r(zmin)=',r_zmn

            write(1,*) ' minor plasma radius:'
            write(1,*) 'a=',a_cen
            write(1,*) ' major plasma radius:'
            write(1,*) 'Rc=',R_cen

c            write(6,*) 'a',a_cen
c            write(6,*) 'Rc',R_cen

            elong=(zmx-zmn)/(rmx-rmn)

            write(1,*) 'plasma elongation:'
            write(1,*) 'el',elong
c            write(6,*) 'el',elong

            triang=(R_cen-R_zmx)/a_cen

            write(1,*) 'plasma triangularity'
            write(1,*) 'tr',triang
c            write(6,*) 'tr',triang

ccc--- pressure determination

         press(iplas)=0.d0

         press(iplas1)=DpDpsi(iplas)*(psi(iplas1,2)-psi(iplas,2))*0.5d0

           do i=iplas-2,1,-1

         press(i)=press(i+1)+DpDpsi(i+1)*(psi(i,2)-psi(i+2,2))*0.5d0

           enddo

cccc---magnetic field determination

           do i=2,nr
            do j=1,nt1

           Bro(i,j)=(psi(i,j+1)-psi(i,j))/Sr(i,j)

            enddo
           enddo

           do i=1,nr1
            do j=1,nt

           Btet(i,j)=(psi(i,j)-psi(i+1,j))/St(i,j)

            enddo
           enddo

                 Pint_s=0.d0
                 Pint_v=0.d0
                 Bint_v=0.d0
                 Vol_pl=0.d0

           do i=1,iplas1
            do j=2,nt1

           pint_s=pint_s+press(i)*s(i,j)
           pint_v=pint_v+press(i)*vol(i,j)
           Vol_pl=Vol_pl+vol(i,j)

          if(i.ne.1) then

           Bp2_1=( Bro(i,j)**2+Btet(i,j)**2+
     +              2.d0*Bro(i,j)*Btet(i,j)*cos1(i,j) )/sin1(i,j)

           Bp2_2=( Bro(i+1,j)**2+Btet(i,j)**2+
     +              2.d0*Bro(i+1,j)*Btet(i,j)*cos2(i,j) )/sin2(i,j)

           Bp2_3=( Bro(i+1,j)**2+Btet(i,j+1)**2+
     +              2.d0*Bro(i+1,j)*Btet(i,j+1)*cos3(i,j) )/sin3(i,j)

           Bp2_4=( Bro(i,j)**2+Btet(i,j+1)**2+
     +              2.d0*Bro(i,j)*Btet(i,j+1)*cos4(i,j) )/sin4(i,j)

           Bp2_v=Bp2_1*vol1(i,j)+Bp2_2*vol2(i,j)+
     +           Bp2_3*vol3(i,j)+Bp2_4*vol4(i,j)

          else

           Bp2_2=( Bro(i+1,j)**2+Btet(i,j)**2+
     +              2.d0*Bro(i+1,j)*Btet(i,j)*cos2(i,j) )/sin2(i,j)

           Bp2_3=( Bro(i+1,j)**2+Btet(i,j+1)**2+
     +              2.d0*Bro(i+1,j)*Btet(i,j+1)*cos3(i,j) )/sin3(i,j)

           Bp2_v= Bp2_3*vol3(i,j)+Bp2_2*vol2(i,j)

          endif

           Bint_v=Bint_v+Bp2_v*2.d0*pi

            enddo
           enddo

                 Paver=pint_v/Vol_pl

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !                                    !
                 BETpol=8.d0*pi*pint_s/tokp**2   !
                                                 !
                 zLi3=2.d0*Bint_v/(R_cen*tokp**2)!
                                                 !
                 BFvac=Fvac/R_cen                !
                                                 !
                 BETtot=2.0d0*Paver/BFvac**2     !
            !                                    !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         write(6,*) 'BETpol',BETpol
c         write(6,*) 'BETtot',BETtot
c         write(6,*) 'Li3   ',zLi3
c         write(6,*) 'Fvac  ',Fvac
c         write(6,*) 'BFvac_cen ',BFvac
c         write(6,*) 'Paver ',Paver
c         write(6,*) 'cnor  ',cnor
c         write(6,*) 'Vol_pl',Vol_pl

         write(1,*) nr,nt,nr1,nt1,nr2,nt2
         write(1,*) 'alp=',alp
         write(1,*) 'rxp=',rx0,'zxp=',zx0
         write(1,*) 'rm =',rm,'zm =',zm
         write(1,*) 'BETpol',BETpol
         write(1,*) 'BETtot',BETtot
         write(1,*) 'Li3   ',zLi3

                  parpla(1)=rmx
                  parpla(2)=Z_rmx
                  parpla(3)=rmn
                  parpla(4)=Z_rmn

                  parpla(5)=zmx
                  parpla(6)=r_zmx
                  parpla(7)=zmn
                  parpla(8)=r_zmn

                  parpla(9)=a_cen
                  parpla(10)=R_cen
                  parpla(11)=aspect
                  parpla(12)=elong
                  parpla(13)=triang
                  parpla(14)=alp

            do 10 i=1,nr

         write(1,*) 'teta'
         write(1,*) (teta(j),j=1,nt)

         write(1,*) 'i=',i
         write(1,*) 'r'
         write(1,*) (r(i,j),j=1,nt)

         write(1,*) 'z'
         write(1,*) (z(i,j),j=1,nt)

         write(1,*) 'g'
         write(1,*) (g(i,j),j=1,nt)

         write(1,*) 'cur'
         write(1,*) (cur(i,j),j=1,nt)

         write(1,*) 'psi'
         write(1,*) (psi(i,j),j=1,nt)

         write(1,*) 'psii'
         write(1,*) (psii(i,j),j=1,nt)

         write(1,*) 'psie'
         write(1,*) (psie(i,j),j=1,nt)

 10      continue

         close(1)

            RETURN
            END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE out_test

        INCLUDE 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
           include 'compol_add.inc'

        write(fname,'(a,a)') path(1:kname),'out_test.pr'
        open(1,file=fname)
         !open(1,file='out_test.pr',form='formatted')

         write(1,*) 'teta'
         write(1,*) (teta(j),j=1,nt)

         write(1,*) 'r'
        do i=1,nr
         write(1,*) 'i=',i
         write(1,*) (r(i,j),j=1,nt)
       enddo

         write(1,*) 'z'
        do i=1,nr
         write(1,*) 'i=',i
         write(1,*) (z(i,j),j=1,nt)
       enddo

         write(1,*) 'g'
        do i=1,nr
         write(1,*) 'i=',i
         write(1,*) (g(i,j),j=1,nt)
       enddo

         write(1,*) 'cur'
        do i=1,nr
         write(1,*) 'i=',i
         write(1,*) (cur(i,j),j=1,nt)
       enddo

         write(1,*) 'psi'
        do i=1,nr
         write(1,*) 'i=',i
         write(1,*) (psi(i,j),j=1,nt)
       enddo

         write(1,*) 'psii'
        do i=1,nr
         write(1,*) 'i=',i
         write(1,*) (psii(i,j),j=1,nt)
       enddo

         write(1,*) 'psie'
        do i=1,nr
         write(1,*) 'i=',i
         write(1,*) (psie(i,j),j=1,nt)
       enddo

         close(1)

            RETURN
            END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE f_rdd

        INCLUDE'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE'dim.inc'
        INCLUDE'compol.inc'
           include 'compol_add.inc'

        write(fname,'(a,a)') path(1:kname),'out.wr'
        open(1,file=fname)
         !open(1,file='out.wr',status='old',form='formatted')

         read(1,*) ni,nj,ni1,nj1,ni2,nj2,nxb

         read(1,*) ((r(i,j),i=1,nr),j=1,nt)
         read(1,*) ((z(i,j),i=1,nr),j=1,nt)
         read(1,*) ((g(i,j),i=1,nr),j=1,nt)
         read(1,*) ((cur(i,j),i=1,nr),j=1,nt)

         close(1)

            RETURN
            END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE rdrec

        INCLUDE 'double.inc'
        INCLUDE 'parrc1.inc'
        INCLUDE 'comrec.inc'


        write(fname,'(a,a)') path(1:kname),'rect.wr'
        open(1,file=fname)
         !open(1,file='rect.wr',status='old',form='formatted')

         read(1,*) ni,nj,ni1,nj1,ni2,nj2,imax,jmax

         read(1,*) (x(i),i=1,ni)
         read(1,*) (y(j),j=1,nj)
         read(1,*) ((u(i,j),i=1,ni),j=1,nj)
         read(1,*) ((ue(i,j),i=1,ni),j=1,nj)
         read(1,*) ((un(i,j),i=1,ni),j=1,nj)
         read(1,*) ((ipr(i,j),i=1,ni),j=1,nj)
         read(1,*) xm,ym,um,xx0,yx0,ux0,up,qcen,b0ax,r0ax
         read(1,*) xx1,yx1,xx2,yx2
         read(1,*) xmax,ymax,xmin,ymin

         close(1)

            RETURN
            END


        SUBROUTINE wr_step(numwr,time,istep)
C
        INCLUDE 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
        INCLUDE 'dim.inc'
        INCLUDE 'compol.inc'
           include 'compol_add.inc'
C
        !dimension psiplb(*),psiexb(*)
        character*40 str,dummy

        write(fname,'(a,a)') path(1:kname),'nmwr.wr'
        open(1,file=fname)
         !open(1,file='nmwr.wr',form='formatted')
            write(1,*) numwr
         close(1)

         if(numwr.lt.10) then
              write(str,'(a,a,i1,a)') path(1:kname),'step',numwr,'.wr'
         elseif(numwr.lt.100) then
              write(str,'(a,a,i2,a)') path(1:kname),'step',numwr,'.wr'
         elseif(numwr.lt.1000) then
              write(str,'(a,a,i3,a)') path(1:kname),'step',numwr,'.wr'
         else
              write(str,'(a,a,i4,a)') path(1:kname),'step',numwr,'.wr'
         endif

         open(1,file=str,form='formatted')
           write(1,*) nr,nt,iplas,istep,dtim,ctim
           write(1,*) ((r(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((z(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((ro(i,j),i=1,iplas),j=1,nt)
           write(1,*) (teta(j),j=1,nt)
           write(1,*) ((psi(i,j),i=1,iplas),j=1,nt)
           write(1,*) ((psin(i,j),i=1,iplas),j=1,nt)
           write(1,*) (psia(i),i=1,iplas)
           !write(1,*) ((cur(i,j),i=1,iplas),j=1,nt)
           write(1,*)  (q(i),i=1,iplas)
           write(1,*)  (f(i),i=1,iplas)
           write(1,*) (dfdpsi(i),i=1,iplas)
           write(1,*) (dpdpsi(i),i=1,iplas)
           write(1,*) psi_eav,rm,zm,psim-psip,psibon0,tok
         close(1)

        write(fname,'(a,a)') path(1:kname),'wlist.wr'
        open(1,file=fname)
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
