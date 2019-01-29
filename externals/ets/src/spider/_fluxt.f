         subroutine flux_p(psitok,rk,zk,nk)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'
       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

         real*8 psitok(*),rk(*),zk(*)
         dimension dg_dn(ntp)

          sqrt(arg)=dsqrt(arg)

          i=iplas        !!!!!!!!!!!!!!!!!!!!!!

       do j=2,nt1

        il=numlin(i,j,nr,nt)

         a1=a13(i-1,j-1)
         a2=a34(i-1,j-1)+a12(i-1,j)
         a3=a24(i-1,j)
         a4=a23(i-1,j-1)
         a6=a23(i-1,j)

       a5=-(a1+a2+a3+a4+a6)


         g1=psi(i-1,j-1)
         g2=psi(i-1,j)
         g3=psi(i-1,j+1)

         g4=psi(i,j-1)
         g5=psi(i,j)
         g6=psi(i,j+1)

         dgdnl=a1*g1+a2*g2+a3*g3+a4*g4+a5*g5+a6*g6-right(il)
         dltk=(dlt(i,j-1)+dlt(i,j))*0.5d0
         dg_dn(j)=dgdnl/dltk

       enddo

         dg_dn(1)=dg_dn(nt1)
         dg_dn(nt)=dg_dn(2)



        do ik=1,nk

          psitok(ik)=0.d0

          rr=rk(ik)
          zz=zk(ik)

         do j=2,nt1

           r0=r(i,j)
           z0=z(i,j)

           r1=r(i,j+1)
           z1=z(i,j+1)

           call bint(rr,zz,R0,Z0,r1,z1,Fint,1)

           psitok(ik)=psitok(ik)-Fint*(dg_dn(j)+dg_dn(j+1))*0.5d0

         enddo

       enddo

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine flux_g(psitok,rk,zk,nk)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'
       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

         real*8 psitok(*),rk(*),zk(*)
         dimension dg_dn(ntp)

          sqrt(arg)=dsqrt(arg)

          i=iplas+2        !!!!!!!!!!!!!!!!!!!!!!

       do j=2,nt1

         a1=a13(i-1,j-1)
         a2=a34(i-1,j-1)+a12(i-1,j)
         a3=a24(i-1,j)
         a4=a23(i-1,j-1)
         a6=a23(i-1,j)

       a5=-(a1+a2+a3+a4+a6)


         g1=g(i-1,j-1)
         g2=g(i-1,j)
         g3=g(i-1,j+1)

         g4=g(i,j-1)
         g5=g(i,j)
         g6=g(i,j+1)

         dgdnl=a1*g1+a2*g2+a3*g3+a4*g4+a5*g5+a6*g6
         dltk=(dlt(i,j-1)+dlt(i,j))*0.5d0
         dg_dn(j)=dgdnl/dltk

       enddo

         dg_dn(1)=dg_dn(nt1)
         dg_dn(nt)=dg_dn(2)



        do ik=1,nk

          psitok(ik)=0.d0

          rr=rk(ik)
          zz=zk(ik)

         do j=2,nt1

           r0=r(i,j)
           z0=z(i,j)
           g0=g(i,j)

           r1=r(i,j+1)
           z1=z(i,j+1)
           g1=g(i,j+1)

           r_05=0.5d0*(r0+r1)
           z_05=0.5d0*(z0+z1)

           d_r=r1-r0
           d_z=z1-z0

       call GRENg(rr,zz, r_05,z_05, fgreen,dGdr,dGdz)

           dGr_dn=(dGdr*d_z-dGdz*d_r)/r_05

       Fint=
     * 0.5d0*( (g0+g1)*dGr_dn - (dg_dn(j)+dg_dn(j+1))*fgreen*dlt(i,j) )
     *           /pi


           psitok(ik)=psitok(ik)+Fint

         enddo

       enddo

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine flux_r(psitok,ncequi)

         include 'double.inc'

         INCLUDE 'prm.inc'
         parameter(nekp=npfc0+nplim)
         parameter(nkp=njlim)
         include 'dimpl1.inc'
         include 'dimpl2.inc'
         include 'parrc1.inc'
         include 'comrec.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         common /comext/ zaindk(nip,njp,nekp)
         real *4 zaindk
         real*8 psitok(*)


          ddx=x(2)-x(1)
          ddy=y(2)-y(1)

          sqcen=0.d0
         do j=2,nt1
          sqcen=sqcen+sq1(1,j)+sq4(1,j)
         enddo

          r0=rm
          z0=zm

          ic=(r0-x(1))/ddx+1
          jc=(z0-y(1))/ddy+1

          r1=x(ic)
          r2=x(ic+1)

          z1=y(jc)
          z2=y(jc+1)

	 do k=1,ncequi

          u1=zaindk(ic,jc,k)
          u2=zaindk(ic+1,jc,k)
          u3=zaindk(ic+1,jc+1,k)
          u4=zaindk(ic,jc+1,k)

         psitok(k)=blin_(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4)*cur(1,2)*sqcen

	 enddo

        do i=2,iplas
         do j=2,nt1

         if(i.ne.iplas) then
          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
         else
          sqk=sq2(i-1,j)+sq3(i-1,j-1)
         endif

          r0=r(i,j)
          z0=z(i,j)

          ic=(r0-x(1))/ddx+1
          jc=(z0-y(1))/ddy+1

          r1=x(ic)
          r2=x(ic+1)

          z1=y(jc)
          z2=y(jc+1)


	 do k=1,ncequi

          u1=zaindk(ic,jc,k)
          u2=zaindk(ic+1,jc,k)
          u3=zaindk(ic+1,jc+1,k)
          u4=zaindk(ic,jc+1,k)

          psitok(k)=psitok(k)+blin_(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4 )
     *   *cur(i,j)*sqk

	 enddo

         enddo
        enddo

       return
       end

         subroutine numcel(rrk,zzk,icell,jcell)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

          sqrt(arg)=dsqrt(arg)

          dr0=rrk-rm
          dz0=zzk-zm

          ro0=sqrt(dr0**2+dz0**2)

           tetp=dacos(dr0/ro0)
          if(dz0.lt.0.d0) then
           tet0=2.d0*pi-tetp
          else
           tet0=tetp
          endif

	   if(tet0.lt.teta(1)) tet0=tet0+2.d0*pi
	   if(tet0.gt.teta(nt)) tet0=tet0-2.d0*pi

	   if(tet0.lt.teta(1)) tet0=tet0+2.d0*pi
	   if(tet0.gt.teta(nt)) tet0=tet0-2.d0*pi

	   if(tet0.lt.teta(1)) pause 'numcel:tet0<teta(1)'
	   if(tet0.gt.teta(nt)) pause 'numcel:tet0>teta(nt)' 

         do j=1,nt1

          if(tet0.ge.teta(j) .AND. tet0.lt.teta(j+1)) jc=j

         enddo

           jcell=jc

          drvb=rrk-r(nr,jc)
          dzvb=zzk-z(nr,jc)

          drcb=r(nr,jc+1)-r(nr,jc)
          dzcb=z(nr,jc+1)-z(nr,jc)

          vecpro=drcb*dzvb-drvb*dzcb


       if(vecpro.le.0.d0) then

          icell=nr
			     
       else

         do i=nr,2,-1

          drv=rrk-r(i,jc)
          dzv=zzk-z(i,jc)

          drc=r(i,jc+1)-r(i,jc)
          dzc=z(i,jc+1)-z(i,jc)

          vecpro=drc*dzv-drv*dzc

         if(vecpro.gt.0.d0) then
           ic=i-1
         else
           go to 100
         endif

         enddo

 100      icell=ic

       endif

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_flux(psitok,rk,zk,nk)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         real*8 psitok(*),rk(*),zk(*)

          sqrt(arg)=dsqrt(arg)

           ik_out=0
        do 100 ik=1,nk
          !  write(6,*) 'ik:',ik
           psitok(ik)=0.d0

          rr=rk(ik)
          zz=zk(ik)

          do 110 j=2,nt1

         psitok(ik)=psitok(ik)
     *             -pinadg(ik,j)*(dgdn(j)+dgdn(j+1))*0.5d0

 110       continue

          if(iprcon(ik).eq.0) then !!!

         call numcel(rr,zz,ic,jc)

           if(ic.ne.nr) then
            write(*,*) 'flux:conductor is in the box',ic,nr
            write(*,*) 'program is terminated'
            write(*,*) 'you need to consult program provider'
            stop
           endif

           ik_out=ik_out+1

!          do 110 j=2,nt1
!
!         psitok(ik)=psitok(ik)
!     *             -pinadg(ik_out,j)*(dgdn(j)+dgdn(j+1))*0.5d0
!
! 110       continue


          else !!!!!!!!!!!!!!!!!!!!!

         call numcel(rr,zz,ic,jc)
            write(*,*) 'ik ic jc',ik,ic,jc

           if(ic.eq.nr) then
            write(*,*) 'flux:conductor is out the box',ic,nr
            write(*,*) 'program is terminated'
            write(*,*) 'you need to consult program provider'
            stop
           endif

          r1=r(ic,jc)
          r2=r(ic+1,jc)
          r3=r(ic+1,jc+1)
          r4=r(ic,jc+1)

          z1=z(ic,jc)
          z2=z(ic+1,jc)
          z3=z(ic+1,jc+1)
          z4=z(ic,jc+1)

          !u1=psii(ic,jc)
          !u2=psii(ic+1,jc)
          !u3=psii(ic+1,jc+1)
          !u4=psii(ic,jc+1)

          u1=g(ic,jc)
          u2=g(ic+1,jc)
          u3=g(ic+1,jc+1)
          u4=g(ic,jc+1)

           call blic_d(rr,zz,r1,r2,r3,r4,z1,z2,z3,z4,
     *                       u1,u2,u3,u4,u0,dudr,dudz)

          !psitok(ik)=u0
          psitok(ik)=psitok(ik)+u0

          endif !!!!!!!!!!!!!!!!!!!!

 100       continue

           if(ik_out.ne.nk_out) then
            write(*,*) 'ik_out.ne.nk_out',ik_out,nk_out
            write(*,*) 'program is terminated'
            write(*,*) 'you need to consult program provider'
            stop
           endif

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       real*8 function blin_tr(tet0,ro0,
     *                        tet1,tet2,ro1,ro2,ro3,ro4,u1,u2,u3,u4)

         include 'double.inc'

         ro14=(ro1*(tet2-tet0)+ro4*(tet0-tet1))/(tet2-tet1)
         ro23=(ro2*(tet2-tet0)+ro3*(tet0-tet1))/(tet2-tet1)

         u14=(u1*(tet2-tet0)+u4*(tet0-tet1))/(tet2-tet1)
         u23=(u2*(tet2-tet0)+u3*(tet0-tet1))/(tet2-tet1)

         blin_tr=(u14*(ro23-ro0)+u23*(ro0-ro14))/(ro23-ro14)

       return
       end

c====================================================================

