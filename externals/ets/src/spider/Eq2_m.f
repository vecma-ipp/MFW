        SUBROUTINE output(NGRID,betpol,zli3)
c
            include 'double.inc'
            parameter(nshp=10)
            INCLUDE 'param.inc'
            include 'comblc.inc'
c
         common/equili/  bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                           zmx,    rrzmx,  zmn,   rrzmn,
     *                           r0cen,  z0cen,  radm,  aspect,
     *                           Eupper, Elower, DELup, DELlw, bfvakc,
     *                           z_li3

         common/comlop/  rxb(nbndp2),zxb(nbndp2),nxb
         common/combrz/  bmr(nip,njp),bmz(nip,njp)
         common /volpla/ Vol_pl
c
         real*8   xs(nshp),ys(nshp),fun(nshp),dp(5)
c----------------------------------------------------------------------
         if(ngrid.eq.0) go to 999

         rmx=0.
         rmn=rmax

         zmx=zmin
         zmn=zmax

C  KAVIN-----------------------------------------------------------------------
	slen=0.
C------------------------------------------------------------------------------
         do 100 i=1,nxb

C  KAVIN-----------------------------------------------------------------------
	if(i .eq. nxb) then
	rxb1=rxb(1)
	zxb1=zxb(1)
	else
	rxb1=rxb(i+1)
	zxb1=zxb(i+1)
	endif
	slen=slen+dsqrt((rxb(i)-rxb1)**2+(zxb(i)-zxb1)**2)
C------------------------------------------------------------------------------

         if(rxb(i).gt.rmx) then
         rmx=rxb(i)
         zzrmx=zxb(i)
         endif

         if(rxb(i).lt.rmn) then
         rmn=rxb(i)
         zzrmn=zxb(i)
         endif

         if(zxb(i).gt.zmx) then
         zmx=zxb(i)
         rrzmx=rxb(i)
         endif

         if(zxb(i).lt.zmn) then
         zmn=zxb(i)
         rrzmn=rxb(i)
         endif

 100     continue

         r0cen=0.5*(rmn+rmx)
         z0cen=0.5*(zmn+zmx)
         radm=0.5*(rmx-rmn)
         aspect=r0cen/radm
         Eupper=dabs(zmx-zm)/radm
         Elower=dabs(zmn-zm)/radm
         DELup=(r0cen-rrzmx)/radm
         DELlw=(r0cen-rrzmn)/radm

C          write(3,*) '___________________________________'
C          write(3,*) ' max. R on plasma boundary:'
C          write(3,*) ' rmx =  ',rmx
C          write(3,*) ' Z(rmx)=',zzrmx
C          write(3,*) ' min. R on plasma boundary:'
C          write(3,*) ' rmn =  ',rmn
C          write(3,*) ' Z(rmn) ',zzrmn
C          write(3,*) ' max. Z on plasma boundary:'
C          write(3,*) ' zmx    ',zmx
C          write(3,*) ' R(zmx) ',rrzmx
C          write(3,*) ' min. Z on plasma boundary:'
C          write(3,*) ' zmn    ',zmn
C          write(3,*) ' R(zmn) ',rrzmn
C          write(3,*) ' major plasma radius:'
C          write(3,*) ' r0cen=',r0cen
C          write(3,*) ' z0cen=',z0cen
C          write(3,*) ' minor plasma radius:'
C          write(3,*) ' radm= ',radm
C          write(3,*) ' aspect ratio:'
C          write(3,*) ' aspect=',aspect
C          write(3,*) ' upper plasma elongation:'
C          write(3,*) ' Eupper=',Eupper
C          write(3,*) ' lower plasma elongation:'
C          write(3,*) ' Elower=',Elower
C          write(3,*) ' upper plasma triang.:'
C          write(3,*) ' DELup =',DELup
C          write(3,*) ' lower plasma triang.:'
C          write(3,*) ' DELlw =',DELlw

           call magpol
           bpolin=0.
           psres=0.d0
           volpl=0.
           sqpl=0.
           zc=0.

           do 200 i=2,ni1
           do 210 j=2,nj1
             iprr=ipr(i,j)
             if(iprr.ne.1) go to 210
c...         bp2ij=(bmz(i-1,j)**2+bmz(i,j)**2+
c...  +             bmr(i,j-1)**2+bmr(i,j)**2)*0.5
c...         bpolin=bpolin+bp2ij*dri(i)*dzj(j)*r(i)
C KAVIN-----------------------------------------------------
             volpl = volpl +  r(i)*dri(i)*dzj(j)
             sqpl  = sqpl  +       dri(i)*dzj(j)
	     psres = psres + curf(i,j)*u(i,j) *dri(i)*dzj(j)
	     zc    = zc + curf(i,j)*z(j) *dri(i)*dzj(j)/tok
C-----------------------------------------------------------
 210       continue
 200       continue

c...       zmx   = zc
           z0cen = zc
c...       ZLI3  = 4.*pi*bpolin/(r0cen*tok*tok)

C KAVIN-----------------------------------------------------
c	psres=(psres/tok-up)/(0.6283185*r0cen*tok)
	psres=(psres/tok-up)/(0.6283185*tok)*sqpl/volpl
C         write(3,*) ' li(3)  :'
C         write(3,*) 'ZLI3=',ZLI3

          ZLI3=psres*7.8957
          Z_LI3=psres*7.8957
C------------------------------------------------------------
           pintg=0.
           pintv=0.
           volpl=0.

           do 300 i=2,ni1
           do 310 j=2,nj1
            iprr=ipr(i,j)
            if(iprr.ne.1) go to 310
            psi=un(i,j)
            zpres=funppp(psi)
            pintg=pintg+zpres*dri(i)*dzj(j)
            pintv=pintv+zpres*r(i)*dri(i)*dzj(j)
            volpl=volpl+      r(i)*dri(i)*dzj(j)
 310       continue
 300       continue

            Vol_pl = volpl
            paverg = pintv/volpl
            BETpol = 8.*pi*pintg/(tok*tok)
            BETpol = betpol*(um-up)*cnor

c      write(3,*) (paverg*(um-up)*cnor),' li3=', ZLI3, 'LENGTH :',slen
C ----    write(3,*) 'BETPOL  :'
C ----    write(3,*) 'BETpol=',BETpol
C ----    write(3,*) 'cnor,um,up',cnor,um,up

C       write(3,'(3(a,1pe12.4))')
C    >     ' Psmax   =',(7.8957*um),
C    >     ' Psbound =',(7.8957*up),
C    >     ' Psxpt   =',(7.8957*ux0)

          nsh     = 1
          xs(nsh) = r(imax)
          ys(nsh) = z(jmax)
         fun(nsh) = u(imax,jmax)

        do 800 k=-1,1

          i= imax+k

        do 810 l=-1,1

          j= jmax+l

          if(i.eq.imax .AND. j.eq.jmax) go to 810
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)
         fun(nsh)=u(i,j)

 810    continue
 800    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

          Drr=dp(3)
          Drz=dp(4)
          Dzz=dp(5)

          tg2a=2.d0*drz/(drr-dzz)
          cos2a=1.d0/dsqrt(1.d0+tg2a**2)
          sin2a=cos2a*tg2a

          Dxx=0.5d0*(Drr+Dzz)+0.5d0*cos2a*(Drr-Dzz)+sin2a*Drz
          Dyy=0.5d0*(Drr+Dzz)+0.5d0*cos2a*(Dzz-Drr)-sin2a*Drz

          bfcen=qcen*dsqrt(dxx*dyy)

          bettot=2.d0*paverg/(bfcen**2)
c         bettot=bettot*(um-up)*cnor
          bettot=paverg*(um-up)*cnor

          bettot=betpol*slen**2/sqpl/4d0/pi

C         write(6,*) 'bettot ',bettot

C         _______________________________________________

          fcen=bfcen*rm
          fintg=funfff(1.d0)*(um-up)*cnor
          fxcen=dsqrt(fcen**2-2.*fintg)

          bfvakc=fxcen

 999      continue

          return
          end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
         subroutine loop

             include 'double.inc'
             include 'param.inc'
             include 'comblc.inc'

             common/comlop/  rxb(nbndp2),   zxb(nbndp2),  nxb
             common/comus1/  rus1(nbndp2),  zus1(nbndp2), nus1
             common/comus2/  rus2(nbndp2),  zus2(nbndp2), nus2
             dimension       rwork(nbndp2), zwork(nbndp2)
             dimension       us(nip,njp)

         xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1+1.d-8)

C --- **********************************************
C         write(5,*) '*** entrance of subr. loop ***'
C         write(6,*) '   '
C         write(6,*) '*** entrance of subr. loop ***'
C --- **********************************************
C ---    un(i,j)=0 - plasma boundary

	    !delunb=0.00200d0
	    delunb=0.00001d0

         do 2212 i=1,ni
         do 2212 j=1,nj
            us(i,j) = alpnew * (un(i,j) - 1.d0) + 1.d0
            un(i,j) = un(i,j) - delunb
 2212    continue
C----------------------------------------------------------
C------*****----------------------------------------------------
         ig=1
         jc=jmax
         j=jmax

 888         continue

                do 885 i=imax,ni1

             if(un(i,j)*un(i+1,j).le.0.) then

              ic=i

        rxb(ig)=xzer(r(i+1),r(i),un(i+1,j),un(i,j))
        zxb(ig)=z(j)

              go to 886
             endif

 885            continue
          !write(6,*) 'loop: first boundary point was not find'
            stop

 886            continue

              ic1=ic
              jc1=jc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          lin=1
C---------------------------------------------------------------
C
  100      ig=ig+1
C            ----
             i=ic
             j=jc
C            ----
             if(un(i+1,j)*un(i,j).le.0. .AND. lin.ne.1) then

                ic  = i
                jc  = j-1
                lin = 3

                rxb(ig) = xzer(r(i+1),r(i),un(i+1,j),un(i,j))
                zxb(ig) = z(j)

             elseif(un(i+1,j+1)*un(i+1,j).le.0..AND. lin.ne.2) then

                ic  = i+1
                jc  = j
                lin = 4

                rxb(ig) = r(i+1)
                zxb(ig) = xzer(z(j+1),z(j),un(i+1,j+1),un(i+1,j))

             elseif(un(i+1,j+1)*un(i,j+1).le.0. .AND. lin.ne.3) then

                ic = i
                jc = j+1
                lin = 1

                rxb(ig) = xzer(r(i+1),r(i),un(i+1,j+1),un(i,j+1))
                zxb(ig) = z(j+1)

             elseif(un(i,j+1)*un(i,j).le.0..AND. lin.ne.4) then

                ic  = i-1
                jc  = j
                lin = 2

                rxb(ig) = r(i)
                zxb(ig) = xzer(z(j+1),z(j),un(i,j+1),un(i,j))

             endif
C------------------------
C --- control of finish
C
            if( (jc.eq.jc1) .and. (ic.eq.ic1) ) then

                    !ig=ig+1

                    !rxb(ig)=rxb(1 )
                    !zxb(ig)=zxb(1 )

                    go to 1212

            endif
C------------------------
C
            go to 100
C------------------------
C
 1212       nxb=ig
C---------------------------------------------------------------
C     End of plasma boundary treatment
C---------------------------------------------------------------
C***************************************************************

            do i=1,ni
            do j=1,nj
               un(i,j) = un(i,j) + delunb
            enddo
            enddo
           return
           end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c**********************************************************
         subroutine rpoint(ut,rt,zt,dp)

         include 'double.inc'
         parameter(nshp=10)
         include 'param.inc'
         include 'comblc.inc'

          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)

        fps(r_,r0,z_,z0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)=
     *      (ps_0-ps_x)+d_r*(r_-r0)+d_z*(z_-z0)+
     * 0.5d0*d_rr*(r_-r0)**2+0.5d0*d_zz*(z_-z0)**2+d_rz*(z_-z0)*(r_-r0)

        fprim_r(r_,r0,z_,z0,d_r,d_rr,d_rz)=d_r+d_rr*(r_-r0)+d_rz*(z_-z0)

        niter=0
 444    continue

        niter=niter+1
c---definition of cell, containig s-point

        do 300 i=1,ni1
          itx=i
        if( (rt.lt.r(i+1)) .AND. (rt.ge.r(i)) ) go to 301
 300    continue
 301    continue

        do 310 j=1,nj1
          jtx=j
        if( (zt.lt.z(j+1)) .AND. (zt.ge.z(j)) ) go to 311
 310    continue
 311    continue

c         write(*,*) 'icelt,jcelt',itx,jtx,rt,zt

c---define nearest node

         sdmin=1.d6

        do 320 k=0,1
         rr=r(itx+k)
        do 325 l=0,1
         zz=z(jtx+l)
         dlx=dsqrt( (rr-rt)**2+(zz-zt)**2 )
       if(dlx.lt.sdmin) then
        sdmin=dlx
         it=itx+k
         jt=jtx+l
       endif
 325    continue
 320    continue

          numit=0
 555    continue
          numit=numit+1

c         write(*,*) 'it,jt',it,jt
c
c         write(*,*) r(it),z(jt)

          nsh=1
          xs(nsh)=r(it)
          ys(nsh)=z(jt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=u(it,jt)                                   !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        do 400 k=-1,1

          i= it+k

        do 410 l=-1,1

          j= jt+l

          if(k.eq.0  .AND. l.eq.0 ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=u(i,j)                                    !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 410    continue
 400    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

         r_0=xs(1)
         z_0=ys(1)
         rrt=rt
         zzt=zt

         d_r=dp(1)
         d_z=dp(2)
         d_rr=dp(3)
         d_zz=dp(4)
         d_rz=dp(5)
         ps_0=fun(1)
         ps_x=ut



       do inw=1,10

        Rt = rrt - fps(rrt,r_0,zt,z_0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)/
     *             fprim_r(rrt,r_0,zt,z_0,d_r,d_rr,d_rz)
        accr=dabs(Rt-rrt)
        if(accr.lt.1.d-6) exit
        rrt=rt

       enddo


        if(niter.lt.4) go to 444

        return
        end


         subroutine zpoint(ut,rt,zt,dp)

         include 'double.inc'
         parameter(nshp=10)
         include 'param.inc'
         include 'comblc.inc'

          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)

        fps(r_,r0,z_,z0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)=
     *      (ps_0-ps_x)+d_r*(r_-r0)+d_z*(z_-z0)+
     *  0.5d0*d_rr*(r_-r0)**2+0.5d0*d_zz*(z_-z0)**2+d_rz*(z_-z0)*(r_-r0)

        fprim_z(r_,r0,z_,z0,d_z,d_zz,d_rz)=d_z+d_zz*(z_-z0)+d_rz*(r_-r0)

        niter=0
 444    continue

        niter=niter+1
c---definition of cell, containig s-point

        do 300 i=1,ni1
          itx=i
        if( (rt.lt.r(i+1)) .AND. (rt.ge.r(i)) ) go to 301
 300    continue
 301    continue

        do 310 j=1,nj1
          jtx=j
        if( (zt.lt.z(j+1)) .AND. (zt.ge.z(j)) ) go to 311
 310    continue
 311    continue

c         write(*,*) 'icelt,jcelt',itx,jtx,rt,zt

c---define nearest node

         sdmin=Rmax-Rmin+Zmax-Zmin

        do 320 k=0,1
         rr=r(itx+k)
        do 325 l=0,1
         zz=z(jtx+l)
         dlx=dsqrt( (rr-rt)**2+(zz-zt)**2 )
       if(dlx.lt.sdmin) then
        sdmin=dlx
         it=itx+k
         jt=jtx+l
       endif
 325    continue
 320    continue

          numit=0
 555    continue
          numit=numit+1

c         write(*,*) 'it,jt',it,jt
c
c         write(*,*) r(it),z(jt)

          nsh=1
          xs(nsh)=r(it)
          ys(nsh)=z(jt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=u(it,jt)                                   !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        do 400 k=-1,1

          i= it+k

        do 410 l=-1,1

          j= jt+l

          if(k.eq.0  .AND. l.eq.0 ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=u(i,j)                                    !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 410    continue
 400    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

         r_0=xs(1)
         z_0=ys(1)

         rrt=rt
         zzt=zt

         d_r=dp(1)
         d_z=dp(2)
         d_rr=dp(3)
         d_zz=dp(4)
         d_rz=dp(5)
         ps_0=fun(1)
         ps_x=ut



       do inw=1,10

        zt = zzt - fps(rt,r_0,zzt,z_0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)/
     *             fprim_z(rt,r_0,zzt,z_0,d_z,d_zz,d_rz)
        accr=dabs(zt-zzt)
        if(accr.lt.1.d-6) exit
        zzt=zt

       enddo

        if(niter.lt.4) go to 444

        return
        end


c**********************************************************
         subroutine gap(ut,rt,zt,rg,zg)

         include 'double.inc'
         parameter(nshp=10)
         include 'param.inc'
         include 'comblc.inc'

          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)

        fps(r_,r0,z_,z0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)=
     *      (ps_0-ps_x)+d_r*(r_-r0)+d_z*(z_-z0)+
     *  0.5d0*d_rr*(r_-r0)**2+0.5d0*d_zz*(z_-z0)**2+d_rz*(z_-z0)*(r_-r0)

        fprim_z(r_,r0,z_,z0,d_z,d_zz,d_rz)=d_z+d_zz*(z_-z0)+d_rz*(r_-r0)

        fprim_r(r_,r0,z_,z0,d_r,d_rr,d_rz)=d_r+d_rr*(r_-r0)+d_rz*(z_-z0)

        k_iter=0
        
        
 333    continue

         k_iter=k_iter+1

          n_iter=0
          itx_old=0
          jtx_old=0

444      n_iter=n_iter+1
c---definition of cell, containig s-point

        do 300 i=1,ni1
          itx=i
        if( (rt.lt.r(i+1)) .AND. (rt.ge.r(i)) ) go to 301
 300    continue
 301    continue

        do 310 j=1,nj1
          jtx=j
        if( (zt.lt.z(j+1)) .AND. (zt.ge.z(j)) ) go to 311
 310    continue
 311    continue

c         write(*,*) 'icelt,jcelt',itx,jtx,rt,zt

c---define nearest node

         sdmin=1.d6

        do 320 k=0,1
         rr=r(itx+k)
        do 325 l=0,1
         zz=z(jtx+l)
         dlx=dsqrt( (rr-rt)**2+(zz-zt)**2 )
       if(dlx.lt.sdmin) then
        sdmin=dlx
         it=itx+k
         jt=jtx+l
       endif
 325    continue
 320    continue

c         write(*,*) 'it,jt',it,jt
c
c         write(*,*) r(it),z(jt)

          nsh=1
          xs(nsh)=r(it)
          ys(nsh)=z(jt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=u(it,jt)                                   !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        do 400 k=-1,1

          i= it+k

        do 410 l=-1,1

          j= jt+l

          if(k.eq.0  .AND. l.eq.0 ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=u(i,j)                                    !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 410    continue
 400    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

         r_0=xs(1)
         z_0=ys(1)

         rrt=rt
         zzt=zt

         d_r=dp(1)
         d_z=dp(2)
         d_rr=dp(3)
         d_zz=dp(4)
         d_rz=dp(5)
         ps_0=fun(1)
         ps_x=ut

         if(k_iter*n_iter.eq.1) then
          psi_cp=ps_0+d_r*(rg-r_0)+d_z*(zg-z_0)+0.5d0*d_rr*(rg-r_0)**2
     &          +0.5d0*d_zz*(zg-z_0)**2+d_rz*(rg-r_0)*(zg-z_0)
         endif

       do inw=1,10

        dpsidr=fprim_r(rt,r_0,zt,z_0,d_r,d_rr,d_rz)
        dpsidz=fprim_z(rt,r_0,zt,z_0,d_z,d_zz,d_rz)
        grad=dsqrt(dpsidr**2+dpsidz**2)

        Dpsi=-fps(rt,r_0,zt,z_0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)

        D_ro=Dpsi/grad

        Rtp = rt + D_ro*dpsidr/grad
        Ztp = zt + D_ro*dpsidz/grad

        accr=dabs(zt-ztp)+dabs(rt-rtp)
        if(accr.lt.1.d-6) exit
        
        rt = Rtp
        zt = ztp
        
         enddo

        if(itx.eq.itx_old .AnD. jtx.eq.jtx_old) go to 555
        if(n_iter.eq.5) go to 555

          itx_old=itx
          jtx_old=jtx

         go to 444
 555    continue

        taur=-dpsidz/dsqrt(dpsidr**2+dpsidz**2)
        tauz= dpsidr/dsqrt(dpsidr**2+dpsidz**2)

         Dgt_r=rg-rt
         Dgt_z=zg-zt
         scal_pro=Dgt_r*taur+Dgt_z*tauz

         wght=0.8d0

         rt=rt+scal_pro*taur*wght
         zt=zt+scal_pro*tauz*wght

        if(dabs(scal_pro).lt.1.d-6) go to 777
        if(k_iter.eq.5) go to 777
         go to 333

 777    continue
        
         ut=psi_cp       
        
        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine f_gap(ut,rt,zt,rg,zg)

         include 'double.inc'
          parameter(nshp=20)
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)

        fps(r_,r0,z_,z0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)=
     *      (ps_0-ps_x)+d_r*(r_-r0)+d_z*(z_-z0)+
     *  0.5d0*d_rr*(r_-r0)**2+0.5d0*d_zz*(z_-z0)**2+d_rz*(z_-z0)*(r_-r0)

        fprim_z(r_,r0,z_,z0,d_z,d_zz,d_rz)=d_z+d_zz*(z_-z0)+d_rz*(r_-r0)

        fprim_r(r_,r0,z_,z0,d_r,d_rr,d_rz)=d_r+d_rr*(r_-r0)+d_rz*(z_-z0)

        k_iter=0
        
        
 333    continue

         k_iter=k_iter+1

          n_iter=0
          itx_old=0
          jtx_old=0

444      n_iter=n_iter+1
c---definition of cell, containig s-point

         call numcel(rt,zt,itx,jtx)

c         write(*,*) 'icelt,jcelt',itx,jtx,rt,zt
       if(itx.eq.nr) then
       !pause 'f_gap:gap point in bound cell'
       return
       endif
c---define nearest node

         sdmin=1.d6

        do 320 k=0,1
        do 325 l=0,1
         rr=r(itx+k,jtx+l)
         zz=z(itx+k,jtx+l)
         dlx=dsqrt( (rr-rt)**2+(zz-zt)**2 )
       if(dlx.lt.sdmin) then
        sdmin=dlx
         it=itx+k
         jt=jtx+l
       endif
 325    continue
 320    continue

       if(jt.eq.nt) jt=2
       if(jt.eq.1) jt=nt-1
       if(it.eq.nr) it=nr-1

          nsh=1
          xs(nsh)=r(it,jt)
          ys(nsh)=z(it,jt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=psi(it,jt)                                 !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        do 400 k=-1,1

          i= it+k

        do 410 l=-1,1

          j= jt+l

          if(k.eq.0  .AND. l.eq.0 ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i,j)
          ys(nsh)=z(i,j)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                     !!
         fun(nsh)=psi(i,j)                                   !!
      !!                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 410    continue
 400    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

         r_0=xs(1)
         z_0=ys(1)

         rrt=rt
         zzt=zt

         d_r=dp(1)
         d_z=dp(2)
         d_rr=dp(3)
         d_zz=dp(4)
         d_rz=dp(5)
         ps_0=fun(1)
         ps_x=ut

         if(k_iter*n_iter.eq.1) then
          psi_cp=ps_0+d_r*(rg-r_0)+d_z*(zg-z_0)+0.5d0*d_rr*(rg-r_0)**2
     &          +0.5d0*d_zz*(zg-z_0)**2+d_rz*(rg-r_0)*(zg-z_0)
         endif

       do inw=1,10

        dpsidr=fprim_r(rt,r_0,zt,z_0,d_r,d_rr,d_rz)
        dpsidz=fprim_z(rt,r_0,zt,z_0,d_z,d_zz,d_rz)
        grad=dsqrt(dpsidr**2+dpsidz**2)

        Dpsi=-fps(rt,r_0,zt,z_0,d_r,d_z,d_rr,d_zz,d_rz,ps_0,ps_x)

        D_ro=Dpsi/grad

        Rtp = rt + D_ro*dpsidr/grad
        Ztp = zt + D_ro*dpsidz/grad

        accr=dabs(zt-ztp)+dabs(rt-rtp)
        if(accr.lt.1.d-6) exit
        
        rt = Rtp
        zt = ztp
        
         enddo

        if(itx.eq.itx_old .AnD. jtx.eq.jtx_old) go to 555
        if(n_iter.eq.5) go to 555

          itx_old=itx
          jtx_old=jtx

         go to 444
 555    continue

        taur=-dpsidz/dsqrt(dpsidr**2+dpsidz**2)
        tauz= dpsidr/dsqrt(dpsidr**2+dpsidz**2)

         Dgt_r=rg-rt
         Dgt_z=zg-zt
         scal_pro=Dgt_r*taur+Dgt_z*tauz

         wght=0.8d0

         rt=rt+scal_pro*taur*wght
         zt=zt+scal_pro*tauz*wght

        if(dabs(scal_pro).lt.1.d-6) go to 777
        if(k_iter.eq.5) go to 777
         go to 333

 777    continue
 
         ut=psi_cp       
        
        return
        end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           subroutine adapol
c
         include 'double.inc'
         parameter(ntet=66,npsi=33)
         include 'param.inc'
         include 'comblc.inc'
         include 'urs.inc'
c
         common/comlop/ rxb(nbndp2),zxb(nbndp2),nxb
         real*8 teta0(nbndp2),tet(ntet),rpol(ntet),zpol(ntet)
         real*8 psipol(npsi),pppol(npsi),fppol(npsi)
c
          rbmax=0.
          rbmin=rmax

          zbmax=zmin
          zbmin=zmax

          do 77 i=1,nxb-1

          rbmax=dmax1(rxb(i),rbmax)
          rbmin=dmin1(rxb(i),rbmin)

          zbmax=dmax1(zxb(i),zbmax)
          zbmin=dmin1(zxb(i),zbmin)

 77       continue

          rcen=0.5*(rbmax+rbmin)
          zcen=0.5*(zbmax+zbmin)

         do 10 i=1,nxb

         drx=rxb(i)-rcen
         dzx=zxb(i)-zcen

         tetp=datan(dzx/drx)
         if(drx.lt.0) tetp=tetp+pi
         if(tetp.lt.0) tetp=tetp+2.*pi
         teta0(i)=tetp

ccc           write(6,*) 'teta0(i) i',teta0(i),i

 10      continue

         tet(2)=0.
         dtet=2.*pi/(ntet-2.)

         do 100 i=3,ntet-1
         tet(i)=tet(i-1)+dtet

 100     continue

         tet(ntet)=2.*pi
         tet(1)=2.*pi-dtet

         do 200 j=1,ntet
         tetp=tet(j)

ccc           write(6,*) 'tetp j',tetp,j
!!!!!    go to 2222 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           do 2000 i=2,nxb
           tetx1=teta0(i-1)
           tetx2=teta0(i)

            if(tetx1.gt.tetx2) then
              if(tetp.gt.pi) then
              tetx2=tetx2+2.*pi
              elseif(tetp.lt.pi) then
              tetx1=tetx1-2.*pi
              endif
            endif
c        write(6,*) 'tetp tetx1 tetx2'
c        write(6,*)  tetp,tetx1,tetx2

           if(tetp.gt.tetx1 .AND. tetp.le.tetx2) then

ccc      write(6,*) 'tetp tetx1 tetx2'
ccc      write(6,*)  tetp,tetx1,tetx2

           rpol(j)=( (tetp-tetx1)*rxb(i)+(tetx2-tetp)*rxb(i-1) )/
     /              (tetx2-tetx1)

           zpol(j)=((tetp-tetx1)*zxb(i)+(tetx2-tetp)*zxb(i-1))/
     /              (tetx2-tetx1)

           endif

 2000      continue
c2222   !!  rrrr=2.                 !!!!!!!!!!!!
        !!  rpol(j)=rcen+rrrr*dcos(tetp) !!!!!!!!!!!!
        !!  zpol(j)=zcen+rrrr*dsin(tetp) !!!!!!!!!!!!

 200     continue

         do 300 i=1,npsi

         psipol(i)=1.-(i-1)/(npsi-1.)

         pppol(i)=cnor*funpp(psipol(i))
         fppol(i)=cnor*funfp(psipol(i))

 300     continue

        write(fname,'(a,a)') path(1:kname),'adpol.dat'
        open(1,file=fname,form='formatted')
         !open(1,file='adpol.dat',form='formatted')

         write(1,*) npsi,ntet,rm,zm,rcen,zcen

         write(1,*) (rpol(i),i=1,ntet)
         write(1,*) (zpol(i),i=1,ntet)

         write(1,*) (psipol(i),i=1,npsi)
         write(1,*) (pppol(i),i=1,npsi)
         write(1,*) (fppol(i),i=1,npsi)

         close(1)

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
         subroutine magpol
c
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
         common/combrz/ bmr(nip,njp),bmz(nip,njp)
c
          do 10 i=1,ni
          do 10 j=1,nj1

           bmr(i,j)=(u(i,j+1)-u(i,j))/dz(j)/r(i)

 10       continue

          do 20 i=1,ni1
          do 20 j=1,nj

           bmz(i,j)=(u(i+1,j)-u(i,j))/dr(i)/r12(i)

 20       continue

         return
         end
c***************************************************************
c
        subroutine btpol(betpol)
c
           include 'double.inc'
           include 'param.inc'
           include 'comblc.inc'
c
         !!amu0=0.4d0*pi
           pintg=0.d0
           do 300 i=2,ni1
           do 310 j=2,nj1
              iprr=ipr(i,j)
              if(iprr.ne.1) go to 310
              psi=un(i,j)
              zpres=funppp(psi)
              pintg=pintg+zpres*dri(i)*dzj(j)
 310       continue
 300       continue
           BETpol=8.d0*pi*pintg/(tok*tok)
           BETpol=betpol*(um-up)*cnor/amu0**2
c
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine bttor(bettor)
c
           include 'double.inc'
           include 'param.inc'
           include 'comblc.inc'
         common/equili/  bettot, rmx,    zzrmx,  rmn,   zzrmn,
     *                           zmx,    rrzmx,  zmn,   rrzmn,
     *                           r0cen,  z0cen,  radm,  aspect,
     *                           Eupper, Elower, DELup, DELlw, bfvakc,
     *                           z_li3

         !!amu0=0.4d0*pi
           pintg=0.d0
           volpl=0.d0

           do 300 i=2,ni1
           do 310 j=2,nj1
              iprr=ipr(i,j)
              if(iprr.ne.1) go to 310
              psi=un(i,j)
              zpres=funppp(psi)
              pintg=pintg+zpres*dri(i)*dzj(j)*r(i)
              volpl=volpl+      dri(i)*dzj(j)*r(i)
 310       continue
 300       continue

              paverg = pintg*(um-up)*cnor/volpl

              b0_cen=b0ax*r0ax/r0cen

              bettor=2.d0*paverg/(b0_cen*b0_cen)

              tok_nor=tok/(radm*b0_cen)

              bet_nor=bettor/tok_nor*100.d0

        return
        end
C***************************************************************
C
        SUBROUTINE ARCLE ( NBAS, RBAS, ZBAS, ARC, ARCMAX )
C
        INCLUDE 'double.inc'
C
        DIMENSION RBAS(*), ZBAS(*), ARC(*)
C
        SQRT(X) = DSQRT(X)
C
                ARC(1) = 0.D0
                DO 1 I=2,NBAS
                   DS = SQRT( ( RBAS(I) - RBAS(I-1) )**2 +
     *                        ( ZBAS(I) - ZBAS(I-1) )**2   )
    1              ARC(I) = ARC(I-1) + DS
                ARCMAX = ARC(NBAS)
                DO 2 I=1,NBAS
    2           ARC(I) = ARC(I) / ARCMAX
C
        RETURN
        END
C******************************************************************
C-----------------------------------------------------------------------
C
        SUBROUTINE TCONTR(KAS,A,JA,IA,NN,Z,B,IPRT,IGRF,BZ,ERR)
C
        IMPLICIT REAL*8(A-H,O-Z)
C
CCCCCCCCC
      DIMENSION A(*),JA(*),IA(*),Z(*),B(*),BZ(*)
C     IPRT=0, IGRF=0 --- POPTPETA HET
CCCCCCCCC
      ABS(X)=DABS(X)
C
      NOUT = 17
C
C    PPOBEPKA  *DRV
C
      IF(KAS.EQ.1) THEN
               CALL SARVEC(A,JA,IA,NN,Z,B)
      END IF
C
      IF(KAS.EQ.0) THEN
               CALL RARVEC(A,JA,IA,NN,Z,B)
      END IF
C
      ERR=0.
         DO 201 LX=1,NN
      ERRLX=ABS(B(LX)-BZ(LX))
         IF(ERR.LT.ERRLX) ERR=ERRLX
201   CONTINUE
C
C  PRINT MATRIX
C
         IF(IPRT.EQ.1) THEN
C
         !WRITE(NOUT,314)
         !WRITE(NOUT,299) ERR
C        WRITE(NOUT,404)
C             WRITE(NOUT,102)(Z(K),K=1,NN)
C
C        WRITE(NOUT,314)
C        WRITE(NOUT,405)
C             WRITE(NOUT,102)(BZ(K),K=1,NN)
C
C        WRITE(NOUT,314)
C        WRITE(NOUT,406)
C             WRITE(NOUT,102)(B(K),K=1,NN)
C        WRITE(NOUT,314)
C
C+++          CALL APRT(NN,A,IA,JA)
C
        ! WRITE(NOUT,314)
C
      END IF
C
C     POPTPET MATPITSY
C
         IF(IGRF.EQ.1) THEN
C
C+++          CALL AGRF(NN,IA,JA)
C
         END IF
C
299   FORMAT(/5X,'PRINT - TCONTR',2X,'ERROR=',E12.5)
404   FORMAT(/5X,'Z - AFTER *DRV')
405   FORMAT(/5X,'B - BEFORE *DRV')
406   FORMAT(/5X,'B - AFTER R(S)ARVEC')
C
102   FORMAT(8E12.5)
314   FORMAT(2X,'* * * * * * * * * * * * * * * * * * * * * * * * * *')
C
        RETURN
        END
C
C****************************
C
C
      SUBROUTINE RARVEC(A,JA,IA,N,X,Y)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(1),JA(1),IA(1),X(1),Y(1)
      DO 1 I=1,N
      Y(I)=0.0
      K1=IA(I)
      K2=IA(I+1)-1
      DO 1 J=K1,K2
      Y(I)=Y(I)+A(J)*X(JA(J))
1     CONTINUE
C
      RETURN
      END
C****************************
C
C
C****************************
      SUBROUTINE SARVEC(A,JA,IA,N,X,Y)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(1),JA(1),IA(1),X(1),Y(1)
C
      DO 2 I=1,N
2     Y(I)=0.0
C
      DO 1 I=1,N
C
      K1=IA(I)
      K2=IA(I+1)-1
C
      IF(JA(K1).EQ.I) THEN
      Y(I)=Y(I)+A(K1)*X(I)
      K1=K1+1
      END IF
C
      IF(K1.LE.K2) THEN
      DO 3 J=K1,K2
C
      Y(I)=Y(I)+A(J)*X(JA(J))
C
      Y(JA(J))=Y(JA(J))+A(J)*X(I)
3     CONTINUE
      END IF
C
1     CONTINUE
C
      RETURN
      END
C****************************
C
C
      SUBROUTINE APRT(N,A,IA,JA)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(1),IA(1),JA(1)
C
      NOUT = 17
C
C     DO 1 I=1,N
      DO 1 I=1,300
      K1=IA(I)
      K2=IA(I+1)-1
C
      ASUM=0.
         DO 2 K=K1,K2
2     ASUM=ASUM+A(K)
C
         !WRITE(NOUT,102)
         !WRITE(NOUT,100)I,K1
      !WRITE(NOUT,104) ASUM
      !WRITE(NOUT,101)(JA(K),K=K1,K2)
      !WRITE(NOUT,103)(A(K),K=K1,K2)
C
1     CONTINUE
C
100   FORMAT(/5X,'IA(',I7,')=',I10)
101   FORMAT(9I7)
103   FORMAT(9E10.3)
104   FORMAT(/5X,'ASUM=',E12.5)
102   FORMAT(2X,'* * * * * * * * * * * * * * * * * * * * * * * * * *')
C
C
      RETURN
      END
C*****************************
      SUBROUTINE AGRF(N,IA,JA)
C
      INTEGER N, IA(1), JA(1)
      CHARACTER*1 STAR  , BLANC  , STRING(132)
      DATA STAR/'*'/, BLANC/' '/
C
      NOUT = 17
C
      DO 3 I=1,N
        DO 1 J=1,132
 1      STRING(J)=BLANC
        DO 2 J=IA(I),IA(I+1)-1
 2      STRING(JA(J))=STAR
      !WRITE(NOUT,'(132A1)')STRING
 3    continue
      RETURN
      END
C**********************************************************
        SUBROUTINE wrd

        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'
C
        common/comlop/ rxb(nbndp2), zxb(nbndp2), nxb
        common/nostep/ kstep1

C
C---------------------------------------------------------------
C
C++++   keywri = mod( kstep1 , 2 )
        keywri = 0
C
C---------------------------------------------------------------
C
c      if( keywri .eq. 0 ) then
c
        write(fname,'(a,a)') path(1:kname),'out.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='out.wr')

         write(1,*) ni,nj,ni1,nj1,ni2,nj2,nxb

         write(1,*) (r(i),i=1,ni)
         write(1,*) (z(j),j=1,nj)
         write(1,*) ((u(i,j),i=1,ni),j=1,nj)
         write(1,*) ((curf(i,j),i=1,ni),j=1,nj)
         write(1,*) ((ipr(i,j),i=1,ni),j=1,nj)
         write(1,*) rm,zm,um,rx0,zx0,ux0,up
         write(1,*) (rxb(ig),ig=1,nxb)
         write(1,*) (zxb(ig),ig=1,nxb)

         write(1,*) ((ui(i,j),i=1,ni),j=1,nj)
         write(1,*) ((ue(i,j),i=1,ni),j=1,nj)
         write(1,*) ((un(i,j),i=1,ni),j=1,nj)
         close(1)

        write(fname,'(a,a)') path(1:kname),'recbon.wr'
        open(1,file=fname,form='formatted')
        
         write(1,*) nxb
         do ig=1,nxb
         write(1,*) rxb(ig),zxb(ig)
         enddo
        
        close(1)


c      else
C
c         open(1,file='out1.wr')
c
c         write(1,*) ni,nj,ni1,nj1,ni2,nj2,nxb
c
c         write(1,*) (r(i),i=1,ni)
c         write(1,*) (z(j),j=1,nj)
c         write(1,*) ((u(i,j),i=1,ni),j=1,nj)
c         write(1,*) ((curf(i,j),i=1,ni),j=1,nj)
c         write(1,*) ((ipr(i,j),i=1,ni),j=1,nj)
c         write(1,*) rm,zm,um,rx0,zx0,ux0,up
c         write(1,*) (rxb(ig),ig=1,nxb)
c         write(1,*) (zxb(ig),ig=1,nxb)
c
c         write(1,*) ((ui(i,j),i=1,ni),j=1,nj)
c         write(1,*) ((ue(i,j),i=1,ni),j=1,nj)
c         write(1,*) ((un(i,j),i=1,ni),j=1,nj)
c         close(1)
C
c      end if
C---------------------------------------------------------------
            RETURN
            END

        SUBROUTINE wrrec

         INCLUDE 'double.inc'
         INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'

        write(fname,'(a,a)') path(1:kname),'rect.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='rect.wr',form='formatted')

         write(1,*) ni,nj,ni1,nj1,ni2,nj2,imax,jmax

         write(1,*) (r(i),i=1,ni)
         write(1,*) (z(j),j=1,nj)
         write(1,*) ((u(i,j),i=1,ni),j=1,nj)
         write(1,*) ((ue(i,j),i=1,ni),j=1,nj)
         write(1,*) ((un(i,j),i=1,ni),j=1,nj)
         write(1,*) ((ipr(i,j),i=1,ni),j=1,nj)
         write(1,*) rm,zm,um,rx0,zx0,ux0,up,qcen,b0ax,r0ax
         write(1,*) rx1,zx1,rx2,zx2
         write(1,*) rmax,zmax,rmin,zmin

         close(1)
            RETURN
            END


        SUBROUTINE wrdfmv(numwr,time)

        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'

        common/comlop/ rxb(nbndp2), zxb(nbndp2), nxb
        common/nostep/ kstep1
        character*40 str,dummy

        write(fname,'(a,a)') path(1:kname),'nw.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='nw.wr',form='formatted')
            write(1,*) numwr
         close(1)

         if(numwr.lt.10) then
              write(str,'(a,a,i1,a)') path(1:kname),'file',numwr,'.wr'
         else
           if(numwr.lt.100) then
              write(str,'(a,a,i2,a)') path(1:kname),'file',numwr,'.wr'
         else
              write(str,'(a,a,i3,a)') path(1:kname),'file',numwr,'.wr'
         endif
         endif

         open(1,file=str,form='formatted')

         write(1,*) ni,nj,ni1,nj1,ni2,nj2,nxb
         write(1,*) time

         write(1,*) (r(i),i=1,ni)
         write(1,*) (z(j),j=1,nj)
         write(1,*) ((u(i,j),i=1,ni),j=1,nj)
         write(1,*) ((curf(i,j),i=1,ni),j=1,nj)
         write(1,*) ((ipr(i,j),i=1,ni),j=1,nj)
         write(1,*) rm,zm,um,rx0,zx0,ux0,up
         write(1,*) (rxb(ig),ig=1,nxb)
         write(1,*) (zxb(ig),ig=1,nxb)

         !write(1,*) ((ui(i,j),i=1,ni),j=1,nj)
         !write(1,*) ((ue(i,j),i=1,ni),j=1,nj)
         !write(1,*) ((un(i,j),i=1,ni),j=1,nj)
         close(1)

        write(fname,'(a,a)') path(1:kname),'flist.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='flist.wr',form='formatted')
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

C***************************************************************
        SUBROUTINE rdd
C
        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'

        common/comlop/ rxb(nbndp2),zxb(nbndp2),nxb
C
        write(fname,'(a,a)') path(1:kname),'out.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='out.wr')
c         open(1,file='out.wr',status='old',form='formatted')
C
         read(1,*) ni,nj,ni1,nj1,ni2,nj2,nxb
C
         read(1,*) (r(i),i=1,ni)
         read(1,*) (z(j),j=1,nj)
         read(1,*) ((u(i,j),i=1,ni),j=1,nj)
         read(1,*) ((curf(i,j),i=1,ni),j=1,nj)
         read(1,*) ((ipr(i,j),i=1,ni),j=1,nj)
         read(1,*) rm,zm,um,rx0,zx0,ux0,up
         read(1,*) (rxb(ig),ig=1,nxb)
         read(1,*) (zxb(ig),ig=1,nxb)

         read(1,*) ((ui(i,j),i=1,ni),j=1,nj)
         read(1,*) ((ue(i,j),i=1,ni),j=1,nj)
         read(1,*) ((un(i,j),i=1,ni),j=1,nj)
         close(1)
C
            RETURN
            END
C**********************************************************
        SUBROUTINE wrdbnd
 
        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'

        write(fname,'(a,a)') path(1:kname),'bnd.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='bnd.wr',form='formatted')
 
         write(1,*) ((binadg(i,j),i=1,nbnd),j=1,nbnd)

         close(1)

            RETURN
            END
C**********************************************************
        SUBROUTINE wrdbin(nk)

        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'
         integer nk

        write(fname,'(a,a)') path(1:kname),'bndin.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='bndin.wr',form='formatted')

         write(1,*) nkin,nkout
         write(1,*) ((pinadg(ik,j),ik=1,nkout),j=1,nbnd)
         write(1,*) (itok(ik),ik=1,nk)
         write(1,*) (jtok(ik),ik=1,nk)

         close(1)

c         write(6,*)' nkin,nkout',nkin,nkout
c
c         write(6,*) (itok(ik),ik=1,nk)
c         write(6,*) (jtok(ik),ik=1,nk)

            RETURN
            END
C***************************************************************
        SUBROUTINE rddbnd

        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'

        write(fname,'(a,a)') path(1:kname),'bnd.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='bnd.wr',form='formatted')

         read(1,*) ((binadg(i,j),i=1,nbnd),j=1,nbnd)

         close(1)


            RETURN
            END
C**********************************************************
C***************************************************************
        SUBROUTINE rddbin(nk)

        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'
         integer nk

        write(fname,'(a,a)') path(1:kname),'bndin.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='bndin.wr',form='formatted')

         read(1,*) nkin,nkout
         read(1,*) ((pinadg(ik,j),ik=1,nkout),j=1,nbnd)
         read(1,*) (itok(ik),ik=1,nk)
         read(1,*) (jtok(ik),ik=1,nk)

         close(1)

c         write(6,*)' nkin,nkout',nkin,nkout
c
c         write(6,*) (itok(ik),ik=1,nk)
c         write(6,*) (jtok(ik),ik=1,nk)

            RETURN
            END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine eqdsk_rebild

        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'

	   parameter (np=1000,nbp=np*4)

         character*8 case(6)
         dimension rbbbs(nbp),zbbbs(nbp)

         common/efites/ fcefit,rcentr,iefit

	   common/com_eqd/ fpol(np),pres(np),qpsi(np),
     *                   ffprim(np),pprime(np),
     *                   rlimtr(np),zlimtr(np),case,simag,sibry,
     *                   idum,nw,nh,limitr

        common/comlop/ rxb(nbndp2), zxb(nbndp2), nxb


         character*40   eqdfn
C--------------------------------------------------------------------

      write(*,*) '************************* '
      write(*,*) ' Entry of subr."eqdsk_rebild":'
      write(*,*) '------------------------- '


           if(ni.ne.nw .or. nj.ne.nh) then

      write(*,*) ' error:ni.ne.nw .or. nj.ne.nh '
      write(*,*) 'nw,nh',nw,nh
      write(*,*) 'ni,nj',ni,nj
         
           endif

          rdim = rmax-rmin
          zdim = zmax-zmin

          rleft=rmin
          zmid = (zmax+zmin)*0.5d0

          zmaxis=zm
          rmaxis=rm

          current=tok*1.d6
          !simag=um
          !sibry=up

          rcentr=r0ax
          bcentr=b0ax

       f2_bon=fpol(nw)**2
       pres_bon=pres(nw)
       correc=(um-up)/(simag-sibry)

         do i=1,nw
       
       pres(i)=(pres(i)-pres_bon)*correc + pres_bon

         enddo

         do i=1,nw
       
       fpol(i)= dsqrt(  (fpol(i)**2-f2_bon)*correc + f2_bon   )

         enddo

         call get q_spline(qpsi,nw)

         open(1,file='eqdsk_rebild.wr')

	        write(1,2000) (case(i),i=1,6),idum,nw,nh
              write(*,*) idum,nw,nh

              write(1,2020) rdim,zdim,rcentr,rleft,zmid
              write(1,2020) rmaxis,zmaxis,um,up,bcentr
              write(1,2020) current,um,xdum,rmaxis,xdum
              write(1,2020) zmaxis,xdum,up,xdum,xdum
              write(1,2020) (fpol(i),i=1,nw)
              write(1,2020) (pres(i),i=1,nw)
              write(1,2020) (ffprim(i),i=1,nw)
              write(1,2020) (pprime(i),i=1,nw)
              write(1,2020) ((u(i,j),i=1,ni),j=1,nj)
              write(1,2020) (qpsi(i),i=1,nw)
              write(1,2022) nxb,limitr
              write(1,2020) (rxb(i),zxb(i),i=1,nxb)
              write(1,2020) (rlimtr(i),zlimtr(i),i=1,limitr)

         close(1)

 2000   format(6a8,3i4)
 2020   format(5e16.9)
 2022   format(2i5)


       return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine eqdsk_build(eqdfn)

        INCLUDE 'double.inc'
        INCLUDE 'param.inc'
        INCLUDE 'comblc.inc'

	   parameter (np=1000,nbp=np*4)

         character*8 case(6)
         dimension rbbbs(nbp),zbbbs(nbp)
         dimension ps(np)

         common/efites/ fcefit,rcentr,iefit

	   common/com_eqd/ fpol(np),pres(np),qpsi(np),
     *                   ffprim(np),pprime(np),
     *                   rlimtr(np),zlimtr(np),case,simag,sibry,
     *                   idum,nw,nh,limitr

        common/comlop/ rxb(nbndp2), zxb(nbndp2), nxb

        character*40   eqdfn
C--------------------------------------------------------------------

      write(*,*) '************************* '
      write(*,*) ' Entry of subr."eqdsk_build":'
      write(*,*) '------------------------- '

      case(1) = '    KIAM'
      case(2) = '  SPIDER'
      idum = 3

          nw=ni
          nh=nj

          rdim = rmax-rmin
          zdim = zmax-zmin

          rleft=rmin
          zmid = (zmax+zmin)*0.5d0

          zmaxis=zm
          rmaxis=rm

          current=tok*1.d6
          !simag=um
          !sibry=up

          rcentr=r0ax
          bcentr=b0ax

          f_vcm=bcentr*rcentr

	     do i=1,nw
            ps(i)= dfloat(i-1)/dfloat(nw-1)
            pprime(i)=tabp(1.d0-ps(i))*1.d6/amu0
            ffprim(i)=tabf(1.d0-ps(i))
	      !write(1,*) ps(i),pprime(i)*amu0*1.d-6,ffprim(i)
           enddo

         fpol(nw)=f_vcm
         pres(nw)=0.d0
         dpsi=(um-up)/dfloat(nw-1)

	     do i=nw-1,1,-1
            pprim_c = 0.5d0*(pprime(i+1)+pprime(i))
            fprim_c = 0.5d0*(ffprim(i+1)+ffprim(i))
            pres(i)=pres(i+1) + pprim_c*dpsi
            fpol(i)=dsqrt(fpol(i+1)**2 + 2.d0*fprim_c*dpsi)
           enddo

           call inter_q(ps,nw,qpsi)

        write(fname,'(a,a40)') path(1:kname),eqdfn
        open(1,file=fname,form='formatted')
         !open(1,file='eqdsk_rebild.wr')

	        write(1,2000) (case(i),i=1,6),idum,nw,nh
              write(*,*) idum,nw,nh

              write(1,2020) rdim,zdim,rcentr,rleft,zmid
              write(1,2020) rmaxis,zmaxis,um,up,bcentr
              write(1,2020) current,up,xdum,rmaxis,xdum
              write(1,2020) zmaxis,xdum,up,xdum,xdum
              write(1,2020) (fpol(i),i=1,nw)
              write(1,2020) (pres(i),i=1,nw)
              write(1,2020) (ffprim(i),i=1,nw)
              write(1,2020) (pprime(i),i=1,nw)
              write(1,2020) ((u(i,j),i=1,ni),j=1,nj)
              write(1,2020) (0.5d0*qpsi(i)/pi,i=1,nw)
              write(1,2022) nxb,nblm
              write(1,2020) (rxb(i),zxb(i),i=1,nxb)
              write(1,2020) (rblm(i),zblm(i),i=1,nblm)

         close(1)

 2000   format(6a8,3i4)
 2020   format(5e16.9)
 2022   format(2i5)


       return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine get q_spline(qpsi,nw)

        INCLUDE 'double.inc'
        INCLUDE 'dim.inc'
         parameter(nspz=nrp+1,nsp4=nspz+4,nsp6=nsp4*6)
        INCLUDE 'compol.inc'

         dimension qpsi(*)

          real*8 qw(nspz),psiw(nspz)
          real*8 rrk(nsp4),cck(nsp4),wrk(nsp6)
          real*8 cwk(4)

           
!         q_axis and q_bon  definition 

         xa0=1.d0
         xa1=0.5d0*(psia(1)+psia(2))
         xa2=0.5d0*(psia(2)+psia(3))
         xa3=0.5d0*(psia(3)+psia(4))
         
         qx1=q(1)
         qx2=q(2)
         qx3=q(3)
         
       call EXTRP2(xa0,qx0, xa1,xa2,xa3, qx1,qx2,qx3)
         
         xaN=psia(iplas)
         xa1=0.5d0*(psia(iplas-1)+psia(iplas))
         xa2=0.5d0*(psia(iplas-2)+psia(iplas-1))
         xa3=0.5d0*(psia(iplas-3)+psia(iplas-2))
         
         qx1=q(iplas-1)
         qx2=q(iplas-2)
         qx3=q(iplas-3)
         
       call EXTRP2(xaN,qxN, xa1,xa2,xa3, qx1,qx2,qx3)
         
         qw(1)=qx0
         psiw(1)=0.d0
        do i=2,iplas 
         qw(i)=q(i-1)
         psiw(i)=1.d0-0.5d0*(psia(i-1)+psia(i))
        enddo
         qw(iplas+1)=qxN
         psiw(iplas+1)=1.d0
         
         nspl=iplas+1
        
           CALL E01BAF(nspl,psiw,qw,RRK,CCK,
     *                        nspl+4,WRK,6*nspl+16,IFAIL)

              qpsi(1)=qw(1)/(2.d0*pi)
           do i=2,nw-1
              zxx=dfloat((i-1))/dfloat((nw-1))
              CALL E02BCF(nspl+4,RRK,CCK,zxx,0,CWk,IFAIL)
              qpsi(i)=cwk(1)/(2.d0*pi)
           enddo
              qpsi(nw)=qw(iplas+1)/(2.d0*pi)
        
        return
        end         
