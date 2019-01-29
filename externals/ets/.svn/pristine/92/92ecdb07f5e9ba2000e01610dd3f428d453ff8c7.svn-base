
         subroutine qst(qcen,cnor,b0ax,r0ax)

         include 'double.inc'
	 ! include 'param.inc'

         parameter(nshp=10,ntp=128,nrp=64)
	 ! include 'comblc.inc'

         common /complr/ rplr(nrp,ntp),zplr(nrp,ntp),dp(5),
     *                   psia(nrp),psi(nrp),psim,psip,rm,zm,
     *                   fplr(nrp),qplr(nrp),dflufi(nrp),fluxfi(nrp),
     *                   iplas,nt,iplas1,nt1

         common /comhel/ helinp, helout

C*************************************************************************

C      write(6,*) '"qst" => enter'

         pi=3.14159265358d0
             
          Drr=dp(3)
          Drz=dp(4)
          Dzz=dp(5)

          tg2a=2.d0*drz/(drr-dzz)
          cos2a=1.d0/sqrt(1.d0+tg2a**2)
          sin2a=cos2a*tg2a

          Dxx=0.5d0*(Drr+Dzz)+0.5d0*cos2a*(Drr-Dzz)+sin2a*Drz
          Dyy=0.5d0*(Drr+Dzz)+0.5d0*cos2a*(Dzz-Drr)-sin2a*Drz

 1        continue

          bfcen=qcen*dsqrt(dxx*dyy)

          fcen=bfcen*rm
          fvac=b0ax*r0ax

C 	 write(6,*) 'fcen, qcen = ', fcen, qcen
C      pause 'pause: fcen, qcen'

          dpsi=(psia(iplas)-psia(iplas-1))
          ps14=-0.25d0*dpsi
          ffp=tabf(ps14)
           !ffp=dfdpsi(iplas)
          fplr(iplas-1)=sqrt(fvac**2-ffp*cnor*(psim-psip)*dpsi)

          do 200 i=iplas-2,1,-1
           dpsi=0.5d0*(psia(i+2)-psia(i))*(psim-psip)
           ffp=tabf(psia(i+1))
           fplr(i)=sqrt(fplr(i+1)**2-2.d0*ffp*dpsi)
 200    continue

           fplr(iplas)=fvac

c---------------------------------------
c      open(1,file='q_rec.pr')

c      write(1,*) '----------------------'

          do 16 i=1,iplas1

             dflufi(i)=0.

          do 17 j=2,nt1

             r1=rplr(i,j)
             r2=rplr(i+1,j)
             r3=rplr(i+1,j+1)
             r4=rplr(i,j+1)

             z1=zplr(i,j)
             z2=zplr(i+1,j)
             z3=zplr(i+1,j+1)
             z4=zplr(i,j+1)

             r0=(r1+r2+r3+r4)*0.25d0

             sqpol=funsq(r1,r2,r3,r4,z1,z2,z3,z4)

             dflufi(i)=dflufi(i)+sqpol*fplr(i)/r0

 17       continue

          q2pi=-dflufi(i)/((psia(i+1)-psia(i))*(psim-psip))

          qplr(i)=0.5d0*q2pi/pi
!!!!!!       q(i)=q2pi

C      write(1,*) 'qplr(i), dflufi(i), i ', qplr(i), dflufi(i), i
C      write(6,*) 'qplr(i), dflufi(i), i ', qplr(i), dflufi(i), i

coment       if( dflufi(i).lt.0.d0 ) pause 'pause: dflufi(i) < 0'

 16       continue
c...................................................

             fluxfi(1)=0.d0

           do i=2,iplas
              fluxfi(i)=fluxfi(i-1) + dflufi(i-1)
           enddo

c      write(1,*) '----------------------'
c      write(1,*) 'dflufi(i), i=1,iplas =', iplas
c      write(1,*) (dflufi(i), i=1,iplas)
c      write(1,*) '----------------------'
c      write(1,*) '----------------------'
c      write(1,*) 'qplr(i), i=1,iplas =', iplas
c      write(1,*) (qplr(i), i=1,iplas)
c      write(1,*) '----------------------'
c      write(1,*) '----------------------'
c      write(1,*) 'fplr(i), i=1,iplas =', iplas
c      write(1,*) (fplr(i), i=1,iplas)
c      write(1,*) '----------------------'
c      write(1,*) '----------------------'
c      write(1,*) 'fluxfi(i), i=1,iplas =', iplas
c      write(1,*) (fluxfi(i), i=1,iplas)
c      write(1,*) '----------------------'
c      write(1,*) 'psim, psip =', psim, psip
c      write(1,*) '----------------------'
c      write(1,*) 'psi(i), i=1,iplas =', iplas
c      write(1,*) (psi(i), i=1,iplas)
c      write(1,*) '----------------------'
c      write(1,*) '----------------------'
c      write(1,*) 'psia(i), i=1,iplas =', iplas
c      write(1,*) (psia(i), i=1,iplas)
c      write(1,*) '----------------------'
C..................................................

           helout = - fluxfi(iplas)*psip
           
           do i=1,iplas1
              helout = helout + 0.5d0*( psi(i)+psi(i+1) )*dflufi(i) 
           enddo

c      write(1,*) '----------------------'
c      write(6,*) 'helinp, helout =', helinp, helout
c      write(1,*) 'helinp, helout =', helinp, helout
c      write(1,*) '----------------------'
c--------------------------------------------------
c          close(1)
c--------------------------------------------------

c      write(6,*) '"qst" => exit'

           !!!call PET_DIN()

           return
           end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine gridpl

         include 'double.inc'
         include 'param.inc'
         parameter(nshp=10,ntp=128,nrp=64)
         include 'comblc.inc'

         common /complr/ rplr(nrp,ntp),zplr(nrp,ntp),dp(5),
     *                   psia(nrp),psi(nrp),psim,psip,xm,ym, 
     *                   fplr(nrp),qplr(nrp),dflufi(nrp),fluxfi(nrp),
     *                   iplas,nt,iplas1,nt1 

         real*8 ron(ntp),teta(ntp)
         real*8 xs(nshp),ys(nshp),fun(nshp)

         cos(xx) =dcos(xx)
         sin(xx) =dsin(xx)
         sqrt(xx)=dsqrt(xx)

c*******************************************************************

c      write(6,*) '"gridpl" => enter'

             iplas  = 61
             nt     = 95
             iplas1 = iplas-1
             nt1    = nt-1

             ixp1=0
             jxp1=0

             ixp2=0
             jxp2=0

             psim=um
             psip=up

	       xm=rm
	       ym=zm

            dtet=2.d0*pi/(nt-2)

            teta(1)=-dtet

            do j=2,nt
             teta(j) = teta(j-1) + dtet
            enddo

            teta(1)=teta(nt1)
            teta(nt)=teta(2)

            psia(1)=1.d0


             nsh  = 1
          xs(nsh) = r(imax)
          ys(nsh) = z(jmax)
         fun(nsh) = u(imax,jmax)

        do k=-1,1
          ii = imax + k
        do l=-1,1

          jj = jmax + l

          if(ii.ne.imax .OR. jj.ne.jmax) then
                nsh  = nsh+1
             xs(nsh) = r(ii)
             ys(nsh) = z(jj)
            fun(nsh) = u(ii,jj)
	    endif

        enddo
        enddo

          call deriv5(xs,ys,fun,nsh,5,dp)

        ! write(6,*) dp

	     stpx=(r(imax+1)-r(imax))*1.0
	     stpy=(z(jmax+1)-z(jmax))*0.7

c----------------------------------------
        do i=2,iplas

           u0=1.d0-((i-1)/(iplas-1.))  !  **2
             psia(i)=u0
           ur0=up+u0*(um-up) 

        do j=1,nt 

           ttj=teta(j) 
           ro2j=(ur0-um)/( 0.5*dp(3)*cos(ttj)**2
     +                  +     dp(4)*cos(ttj)*sin(ttj)
     +                  + 0.5*dp(5)*sin(ttj)**2 )

           ron(j)=sqrt(ro2j)     


           rplr(i,j)=rm+ron(j)*cos(teta(j))
           zplr(i,j)=zm+ron(j)*sin(teta(j))


        enddo  ! j-loop

	     ibeg=i+1

	     if( ron(2) .GT. dmax1(stpx,stpy) ) go to 2799

        enddo   !i-loop

c------------------------
 2799     continue
c------------------------

c      write(6,*)'ibeg',ibeg,u0
c      pause 'pause: ibeg'

            do 100 i=ibeg,iplas

               u0=(iplas-i)/(iplas-1.d0)
               ! u0=1.d0-((i-1)/(iplas-1.))**2

               psia(i)=u0

               call loop95(teta,nt,ron,u0)

               do 110 j=1,nt
                  rplr(i,j)=rm+ron(j)*cos(teta(j))
                  zplr(i,j)=zm+ron(j)*sin(teta(j))
 110           continue

 100        continue
c-------------------------

            do 120 j=1,nt
               rplr(1,j) = rm
               zplr(1,j) = zm
 120        continue


        do 333 i=1,iplas
             rplr(i,1)=rplr(i,nt1)
             zplr(i,1)=zplr(i,nt1)

             rplr(i,nt)=rplr(i,2)
             zplr(i,nt)=zplr(i,2)
 333    continue

        teta(1)=teta(nt1)-2.d0*pi
        teta(nt)=teta(2)+2.d0*pi

        do i=1,iplas
        do j=1,nt
           psi(i)=psip+psia(i)*(psim-psip)
        enddo
        enddo

c      write(6,*) '"gridpl" => exit'

!         open(1,file='map_bnd.wr')
!           write(1,*) nt-2
!        do j=2,nt1
!           write(1,*) rplr(iplas,j),zplr(iplas,j)
!        enddo
!         close(1)

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine loop95(tetpol,ntet,ro0,u0)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         dimension rxb(nbndp2),zxb(nbndp2)

         real*8 ut(nip,njp)

         real*8 roxb(nbndp2),tetxb(nbndp2)

         real*8 RRK(nbndp4),CCK(nbndp4),WRK(nbndp6)

         real*8 CWK(4),tetpol(1),ro0(1)

         xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1)

        ! write(6,*) '***loop95:enter'
        ! write(6,*) '***loop95:imax,jmax',imax,jmax
        ! write(6,*) '***loop95:rmax,zmax',rm,zm

         if(u0.lt.2.d-5) u0=2.d-5

         imax1=imax-1
         jmax1=jmax-1

         do 10 i=imax1,imax
          if(rm.le.r(i+1) .AND. rm.gt.r(i)) icell=i
 10      continue

         do 20 j=jmax1,jmax
          if(zm.le.z(j+1) .AND. zm.gt.z(j)) jcell=j
 20      continue

        ! write(6,*) 'loop95:icell,jcell',icell,jcell

         i=icell
         j=jcell+1

cccc      u0=0.5
         do 15 ii=1,ni
         do 15 jj=1,nj
          ut(ii,jj)=un(ii,jj)-u0
 15      continue

         ig=1

 888         continue

                do 885 i=imax,ni1

          !   write(6,*) 'un(i,j)',un(i,j),un(i+1,j)

             if(ut(i,j)*ut(i+1,j).le.0.) then

              ic=i
              jc=j

        rxb(ig)=xzer(r(i+1),r(i),ut(i+1,j),ut(i,j))
        zxb(ig)=z(j)

              go to 886
             endif

 885            continue
         ! write(6,*) 'loop95: first point was not find'
            stop

 886            continue

              ic1=ic
              jc1=jc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          lin=1

 100      ig=ig+1

          i=ic
          j=jc

             if(ut(i+1,j)*ut(i,j).le.0. .AND. lin.ne.1) then

              ic=i
              jc=j-1

              lin=3

              rxb(ig)=xzer(r(i+1),r(i),ut(i+1,j),ut(i,j))
              zxb(ig)=z(j)

             elseif(ut(i+1,j+1)*ut(i+1,j).le.0..AND. lin.ne.2) then

              ic=i+1
              jc=j

              lin=4

              rxb(ig)=r(i+1)
              zxb(ig)=xzer(z(j+1),z(j),ut(i+1,j+1),ut(i+1,j))

             elseif(ut(i+1,j+1)*ut(i,j+1).le.0. .AND. lin.ne.3) then

              ic=i
              jc=j+1

              lin=1

              rxb(ig)=xzer(r(i+1),r(i),ut(i+1,j+1),ut(i,j+1))
              zxb(ig)=z(j+1)

             elseif(ut(i,j+1)*ut(i,j).le.0..AND. lin.ne.4) then

              ic=i-1
              jc=j

              lin=2

              rxb(ig)=r(i)
              zxb(ig)=xzer(z(j+1),z(j),ut(i,j+1),ut(i,j))

             endif

c        write(6,*) 'loop:ic,jc',ic,jc,ig

            if(jc.eq.jc1 .AND. ic.eq.ic1) then

              ig=ig+1

              rxb(ig)=rxb(2 )
              zxb(ig)=zxb(2 )

              go to 1212

            endif

             go to 100

 1212        nxb=ig

            do 200 ig=1,nxb

         drx=rxb(ig)-rm
         dzx=zxb(ig)-zm

         tetp=datan(dzx/drx)

       if(ig.ne.1) then
         if(drx.lt.0.) tetp=tetp+pi
         deltet=tetp-tetxb(ig-1)
         if(deltet.lt.0.) tetp=tetp+2.*pi
       endif

         tetxb(ig)=tetp

         roxb(ig)=dsqrt(drx**2+dzx**2)

c         write(6,*) 'ig,ro,tet',ig,roxb(ig),tetxb(ig)

 200        continue


        CALL E01BAF(Nxb,tetxb,roxb,RRK,CCK,nxb+4,WRK,6*nxb+16,IFAIL)

c       write(6,*) 'ifail=',ifail

        DO 210 j=1,Ntet
           tetp=tetpol(j)
           if(tetp.lt.tetxb(1)) then
             tetp=tetp+2.*pi
           elseif(tetp.gt.tetxb(nxb)) then
             tetp=tetp-2.*pi
           endif
          CALL E02BCF(Nxb+4,RRK,CCK,tetp     ,0,CWk,IFAIL)
            ro0(j)=cwk(1)
c       write(6,*) 'ifail=',ifail,j
 210    CONTINUE

           return
           end








