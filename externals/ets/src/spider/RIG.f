         subroutine bound

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

          real*8 psib(nbndp)


c--- array dgdn initialization (calculation dg/dn)

        ib=1

        dgdn(ib)=0.d0
        g(1,1)=0.d0

        do 10 i=2,ni1

        ib=ib+1
        dgdn(ib) = g(i,2)/dz(1)/r(i)
        g(i,1)=0.d0

 10     continue

        ib=ib+1
        dgdn(ib)=0.d0
        g(ni,1)=0.d0

        do 20 j=2,nj1

        ib=ib+1
        dgdn(ib) = g(ni1,j)/( dr(ni1)*r12(ni1))
        g(ni,j)=0.d0

 20     continue

        ib=ib+1
        dgdn(ib)=0.d0
        g(ni,nj)=0.d0

        do 30 i=ni1,2,-1

        ib=ib+1
        dgdn(ib) = g(i,nj1)/dz(nj1)/r(i)
        g(i,nj)=0.d0

 30     continue

        ib=ib+1
        dgdn(ib)=0.d0
        g(1,nj)=0.d0

        do 40 j=nj1,2,-1

        ib=ib+1
        dgdn(ib) = g(2,j)/(dr(1)*r12(1))
        g(1,j)=0.d0

 40     continue

        ib=ib+1
        dgdn(ib)=0.d0

        do 100 ib=1,nbnd
        dgdn(ib)= (dgdn(ib)+dgdn(ib+1))*0.5
 100    continue

        do 110 ib=1,nbnd
         zpsi=0.d0
         do 111 ibc=1,nbnd

         zpsi= zpsi+dgdn(ibc)*binadg(ibc,ib)

 111    continue

        psib(ib)=zpsi
 110    continue

c...boundary condition for ui

        ib=0

        do 200 i=1,ni

        ib=ib+1
        ui(i,1)=psib(ib)

 200    continue


        do 300 j=2,nj

        ib=ib+1
        ui(ni,j)=psib(ib)

 300    continue

        do 210 i=ni1,1,-1

        ib=ib+1
        ui(i,nj)=psib(ib)

 210    continue

        do 310 j=nj1,2,-1

        ib=ib+1
        ui(1,j)=psib(ib)

 310    continue

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine right0(ill,jll,icelm,jcelm,ngav1)

          parameter(nshp=10)

          include 'double.inc'
          include 'param.inc'
          include 'comblc.inc'

          common/comomg/ omg,sigm
          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5),dp1(5),dp2(5),clsq(6)
          common/comkjf/ curn,nctrli
          real*8 curn(nip,njp)

          common /comhel/ helinp, helout

	common
     *  /c_kpr/kpr


	 real*4       a_print(200)
	 character*30 apr

c---------------------------------------------------------------
         !!amu0=0.4d0*pi

             nctrli=nctrl

ccc--- right hand side for eq. Lg=Jf in rectangular box.

           do il=1,neqp
            right(il)=0.d0
           enddo
c---definition of Umax- maximum poloidal flux function value on
c   grid in plasma.

          umax=u(imax,jmax)

         if(iter.ge.iterbf) then
          imax=ill
          jmax=jll
          uem=bline(icelm,jcelm,rl,zl)
          go to 2000
         endif

 1000    continue

           ic=imax
           jc=jmax

        do 100 k=-1,1

          imp= imax+k

        do 110 l=-1,1

          jmp= jmax+l

          if( u(imp,jmp).gt.umax) then

            ic=imp
            jc=jmp
           umax=u(imp,jmp)

          endif

 110    continue
 100    continue

          if(ic.ne.imax .OR. jc.ne.jmax) then

            imax=ic
            jmax=jc
            umax=u(imax,jmax)
                             go to 1000

          endif

 2000     continue

          if(kpr.eq.1)write(*,*) 'imax jmax umax',imax,jmax,umax
          if(kpr.eq.1)write(*,*) 'iclm jclm     ',icelm,jcelm

c---definition of Um - poloidal flux function value at magn. axis.

          nsh=1
          xs(nsh)=r(imax)
          ys(nsh)=z(jmax)
         fun(nsh)=u(imax,jmax)

        do 200 k=-1,1

          i= imax+k

        do 210 l=-1,1

          j= jmax+l

          if(i.eq.imax .AND. j.eq.jmax) go to 210
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)
         fun(nsh)=u(i,j)

 210    continue
 200    continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          call lsq_sur6(xs,ys,fun,nsh,clsq,rrmm,zzmm,uumm,dp)
          rmold=rm
          zmold=zm
          rm=rrmm
          zm=zzmm
          um=uumm
          errm=dsqrt( (rm-rmold)**2+(zm-zmold)**2 )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          call deriv5(xs,ys,fun,nsh,5,dp)
!
!        DET = dp(3)*dp(5) - dp(4)**2
!          rmold=rm
!          zmold=zm
!        Rm = Xs(1) + ( dp(2)*dp(4) - dp(1)*dp(5) )/DET
!        Zm = Ys(1) + ( dp(1)*dp(4) - dp(2)*dp(3) )/DET
!
!          errm=dsqrt( (rm-rmold)**2+(zm-zmold)**2 )
!
!           rmi=r(imax)
!           zmj=z(jmax)
!
!       um=fun(1)+ dp(1)*(rm-rmi) + dp(2)*(zm-zmj)
!     +          + 0.5d0*dp(3)*(rm-rmi)*(rm-rmi)
!     +          +     dp(4)*(rm-rmi)*(zm-zmj)
!     +          + 0.5d0*dp(5)*(zm-zmj)*(zm-zmj)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !write(6,*) 'rm zm um',rm,zm,um

        if(iter.lt.iterbf) then
         clr=0.d0
         clz=0.d0
	   rl=rm
	   zl=zm

             call shab(ilma,jlma,icelma,jcelma)
          uem=bline(icelma,jcelma,rl,zl)

         go to 1010
	  endif
c...stabilization by artifical field


!        delcr= -( dp(1) + dp(3)*(rl-rmi) + dp(4)*(zl-zmj) )*0.5d0/rl
!        delcz= -( dp(2) + dp(5)*(zl-zmj) + dp(4)*(rl-rmi) )
        delcr= -(     dp(3)*(rl-rm) + dp(4)*(zl-zm) )*0.5d0/rl
        delcz= -(     dp(5)*(zl-zm) + dp(4)*(rl-rm) )
        clr=clr+delcr
        clz=clz+delcz


        do 250 i=1,ni
        do 250 j=1,nj

         u(i,j)=u(i,j)+delcz*z(j)+delcr*r(i)*r(i)

 250    continue

           delc = dabs(delcr)+dabs(delcz)

        if(delc.lt.1.d-9) go to 1010

          go to 2000

 1010   continue

         if(kpr.eq.1)write(*,*) '*****rm,zm,rl,zl',rm,zm,rl,zl
         !write(6,*) '*****clr,delcr ',clr,delcr
         !write(6,*) '*****clz,delcz ',clz,delcz
         if(kpr.eq.1)write(*,*) '*****clr,clz ',clr,clz


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c---definition ux- poloidal flux function value on separatris.!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          uxold=ux0

         call xpoint(ux1,rx1,zx1,ix1,jx1,dp1,1,kodex1)
         if(kodex1.gt.0) then
                  rx1=rx10
                  zx1=zx10
         endif
         if(kpr.eq.1)write(*,*) 'xpoint1',kodex1
         call xpoint(ux2,rx2,zx2,ix2,jx2,dp2,2,kodex2)
         if(kodex2.gt.0) then
                  rx2=rx20
                  zx2=zx20
         endif
         if(kpr.eq.1)write(*,*) 'xpoint2',kodex2

          kodxp=kodex1*kodex2
          if(kodxp.ne.0) then

         if(kpr.eq.1)write(*,*) ' all x-points out of box'
         if(kpr.eq.1)write(*,*) ' only limiter case '

                  nctrli=1

           !write(6,*) ' program cannot run more '
           !stop

          endif

          if(ux1.gt.ux2) then

         if(kpr.eq.1)write(*,*) 'xpoint=1'
            ux0= ux1

            rx0= rx1
            zx0= zx1

            ix = ix1
            jx = jx1

            rix = r(ix)
            zjx = z(jx)

            uix = u(ix,jx)

            do 1840 is=1,5
 1840         dp(is)=dp1(is)

          else

       if(kpr.eq.1)write(*,*) 'xpoint=2'
            ux0= ux2

            rx0= rx2
            zx0= zx2

            ix = ix2
            jx = jx2

            rix = r(ix)
            zjx = z(jx)

            uix = u(ix,jx)

            do 1940 is=1,5
 1940         dp(is)=dp2(is)

          endif

       if(kpr.eq.1)write(*,*)'rx0,zx0,ux0',rx0,zx0,ux0

            if(uxold.ge.ux0) ux0=(1.d0-sigm)*uxold+sigm*ux0

          zvmax=dmax1(zx1,zx2)
          zvmin=dmin1(zx1,zx2)

         zver=dabs(zm-zx0)

c---definition up- poloidal flux function value on plasma boundary.

             if(kodxp.eq.0) then
            ups=um - alp*(um-ux0)
             else       !!!!<- no x-points
            ups=-1.d12
             endif

        if(nctrli.eq.0 ) then

           up=um - alp*(um-ux0)
           alpnew=alp
           numlim=0
!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        elseif(nctrli.lt.0 ) then
	   
           upp=um - alp*(um-ux0)
           if(psi_bon.lt.ux0) then
		    up=upp
            alpnew=alp
           else
            up=psi_bon
            alpnew=(um-up)/(um-ux0)
           endif
           numlim=0
!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	  
        else   !!!if(nctrli.gt.0 ) <-limiter case

              ulmax1=ups
              ulmax2=ups
              nulim1=0
              nulim2=0
	        neigb=0

         do llim=1,nblm

           if(kodex1.eq.0) then
           rlim1=frlim(dp1,zblm(llim),rx1,zx1,rm,zm)
           rmlim1=frlim(dp1,zm,rx1,zx1,rm,zm)
           else
           rlim1=rmax
           rmlim1=rmax
           endif

           if(kodex2.eq.0) then
           rlim2=frlim(dp2,zblm(llim),rx2,zx2,rm,zm)
           rmlim2=frlim(dp2,zm,rx2,zx2,rm,zm)
           else
           rlim2=rmax
           rmlim2=rmax
           endif


           if( (rblm(llim)-rlim1)*(rm-rmlim1).le.0.d0) then

           go to 1246

           elseif( (rblm(llim)-rlim2)*(rm-rmlim2).le.0.d0) then

           go to 1246

           endif


               ilm=iblm(llim)
               jlm=jblm(llim)
               ublm=blint(iblm(llim),jblm(llim),rblm(llim),zblm(llim))
               iprlm=ipr(ilm,jlm)+ipr(ilm+1,jlm)
     *              +ipr(ilm,jlm+1)+ipr(ilm+1,jlm+1) 

                
	         if(iprlm.gt.0) then

                  if(ublm.gt.ulmax1)then
                       ulmax1=ublm
                       nulim1=llim
	                 neigb=1
                  endif

                else

                  if(ublm.gt.ulmax2)then
                       ulmax2=ublm
                       nulim2=llim
                  endif

                endif

 1246     continue
         enddo

                 if(neigb.eq.1) then
                   ublmax=ulmax1
                   numlim=nulim1
                 elseif(neigb.eq.0) then
                   ublmax=ulmax2
                   numlim=nulim2
                 endif


           up=dmax1(ups,ublmax)

           if(kodxp.eq.0) alpnew=(um-up)/(um-ux0)

        endif

      if(kpr.eq.1) then
       write(*,*) 'up,numlim',up,numlim,neigb
       write(*,*) 'psi_bon',psi_bon
       write(*,*) 'um-up',um-up
       write(*,*) 'alpnew',alpnew
      endif


          
c--- un- normal poloidal flux.

         erru=0.d0

         do 600 i=1,ni
         do 600 j=1,nj

         unold=un(i,j)
         un(i,j)=(u(i,j)-up)/(um-up)
         delun=dabs(un(i,j)-unold)
         erru=dmax1(delun,erru)
 600    continue
      if(kpr.eq.1)write(*,*)'erru',erru

c--- dimension ipr initialization.
c--- if ipr(i,j)=1,then point (i,j) is in plasma.

         do 700 i=1,ni
         do 700 j=1,nj

           ipr(i,j)=0

 700    continue

           ipr(imax-1,jmax-1)=1

         do 800 j=jmax,nj1
cccc+      zlim=zvmax
cccc+      if(z(j).gt.zlim) go to 800

           if(kodex1.eq.0) then
           rlim1=frlim(dp1,z(j),rx1,zx1,rm,zm)
           rmlim1=frlim(dp1,zm,rx1,zx1,rm,zm)
           else
           rlim1=rmax
           rmlim1=rmax
           endif

           if(kodex2.eq.0) then
           rlim2=frlim(dp2,z(j),rx2,zx2,rm,zm)
           rmlim2=frlim(dp2,zm,rx2,zx2,rm,zm)
           else
           rlim2=rmax
           rmlim2=rmax
           endif

         do 801 i=imax,ni1

           if(un(i,j).lt.0.d0) then

           ipr(i,j)=0

           elseif( (r(i)-rlim1)*(rm-rmlim1).le.0.d0) then

           ipr(i,j)=0

           elseif( (r(i)-rlim2)*(rm-rmlim2).le.0.d0) then

           ipr(i,j)=0

           else

           isum=ipr(i-1,j-1)+ipr(i,j-1)+ipr(i+1,j-1)+ipr(i-1,j)
           if(isum.gt.0) ipr(i,j)=1

           endif

 801    continue
 800    continue

         do 810 j=jmax,nj1
ccc+       zlim=zvmax
ccc+       if(z(j).gt.zlim) go to 810

           if(kodex1.eq.0) then
           rlim1=frlim(dp1,z(j),rx1,zx1,rm,zm)
           rmlim1=frlim(dp1,zm,rx1,zx1,rm,zm)
           else
           rlim1=rmin
           rmlim1=rmin
           endif

           if(kodex2.eq.0) then
           rlim2=frlim(dp2,z(j),rx2,zx2,rm,zm)
           rmlim2=frlim(dp2,zm,rx2,zx2,rm,zm)
           else
           rlim2=rmin
           rmlim2=rmin
           endif

         do 811 i=imax,2,-1

           if(un(i,j).lt.0.d0) then

           ipr(i,j)=0

           elseif( (r(i)-rlim1)*(rm-rmlim1).le.0.d0) then

           ipr(i,j)=0

           elseif( (r(i)-rlim2)*(rm-rmlim2).le.0.d0) then

           ipr(i,j)=0

           else

           isum=ipr(i-1,j-1)+ipr(i,j-1)+ipr(i+1,j-1)+ipr(i+1,j)
           if(isum.gt.0.) ipr(i,j)=1

           endif

 811    continue
 810    continue

         do 820 j=jmax,2,-1
ccc+       zlim=zvmin
ccc+       if(z(j).lt.zlim) go to 820

           if(kodex1.eq.0) then
           rlim1=frlim(dp1,z(j),rx1,zx1,rm,zm)
           rmlim1=frlim(dp1,zm,rx1,zx1,rm,zm)
           else
           rlim1=rmax
           rmlim1=rmax
           endif

           if(kodex2.eq.0) then
           rlim2=frlim(dp2,z(j),rx2,zx2,rm,zm)
           rmlim2=frlim(dp2,zm,rx2,zx2,rm,zm)
           else
           rlim2=rmax
           rmlim2=rmax
           endif

         do 821 i=imax,ni1

           if(un(i,j).lt.0.) then

           ipr(i,j)=0

           elseif( (r(i)-rlim1)*(rm-rmlim1).le.0.d0) then

           ipr(i,j)=0

           elseif( (r(i)-rlim2)*(rm-rmlim2).le.0.d0) then

           ipr(i,j)=0

           else

           isum=ipr(i-1,j+1)+ipr(i,j+1)+ipr(i+1,j+1)+ipr(i-1,j)
           if(isum.gt.0.) ipr(i,j)=1

           endif

 821    continue
 820    continue

         do 830 j=jmax,2,-1
ccc+       zlim=zvmin
ccc+       if(z(j).lt.zlim) go to 830

           if(kodex1.eq.0) then
           rlim1=frlim(dp1,z(j),rx1,zx1,rm,zm)
           rmlim1=frlim(dp1,zm,rx1,zx1,rm,zm)
           else
           rlim1=rmin
           rmlim1=rmin
           endif

           if(kodex2.eq.0) then
           rlim2=frlim(dp2,z(j),rx2,zx2,rm,zm)
           rmlim2=frlim(dp2,zm,rx2,zx2,rm,zm)
           else
           rlim2=rmin
           rmlim2=rmin
           endif

         do 831 i=imax,2,-1

           if(un(i,j).lt.0.d0) then

           ipr(i,j)=0

           elseif( (r(i)-rlim1)*(rm-rmlim1).le.0.) then

           ipr(i,j)=0

           elseif( (r(i)-rlim2)*(rm-rmlim2).le.0.) then

           ipr(i,j)=0

           else

           isum=ipr(i-1,j+1)+ipr(i,j+1)+ipr(i+1,j+1)+ipr(i+1,j)
           if(isum.gt.0) ipr(i,j)=1

           endif

 831    continue
 830    continue

         !if(nnstpp.eq.4) then
	   ! call wrd 
	   ! pause 'wrd'
         !endif

c---troidal current density in plasma

         do 999 i=1,ni
         do 999 j=1,nj

           curf(i,j)=0.d0

 999    continue


           tokp=0.d0

         do 900 i=2,ni2

          x1=r(i)
          x2=r(i+1)
          x3=r(i+1)
          x4=r(i)

          x12=0.5d0*(x1+x2)
          x23=0.5d0*(x2+x3)
          x34=0.5d0*(x3+x4)
          x14=0.5d0*(x1+x4)

          x0=0.5d0*(x12+x34)

         do 901 j=2,nj2

          cur1=0.d0
          cur2=0.d0
          cur3=0.d0
          cur4=0.d0

           if(kodex1.ne.0) go to 2080

           if(i.gt.ix1.OR. i.lt.ix1-1) go to 2080
           if(j.gt.jx1  .OR. j.lt.jx1-1) go to 2080

            nloc=21
            call xdetal(i,j,ix1,jx1,rx1 ,zx1 ,dp1,nloc,
     *                  cur1,cur2,cur3,cur4)

                 go to 9010

 2080            continue

           if(kodex2.ne.0) go to 2090

           if(i.gt.ix2 .OR. i.lt.ix2-1) go to 2090
           if(j.gt.jx2   .OR. j.lt.jx2-1) go to 2090

            nloc=21
            call xdetal(i,j,ix2,jx2,rx2 ,zx2 ,dp2,nloc,
     *                  cur1,cur2,cur3,cur4)

                 go to 9010

 2090            continue

          ipr1=ipr(i,j)
          ipr2=ipr(i+1,j)
          ipr3=ipr(i+1,j+1)
          ipr4=ipr(i,j+1)

          isum=ipr1+ipr2+ipr3+ipr4
         if(isum.eq.0) go to 901

          z1=z(j)
          z2=z(j)
          z3=z(j+1)
          z4=z(j+1)

          z12=0.5d0*(z1+z2)
          z23=0.5d0*(z2+z3)
          z34=0.5d0*(z3+z4)
          z14=0.5d0*(z1+z4)

          z0=0.5d0*(z12+z34)

          u1=un(i,j)
          u2=un(i+1,j)
          u3=un(i+1,j+1)
          u4=un(i,j+1)

         if(ipr1.eq.0 .AND. u1.gt.0.d0) u1=0.d0
         if(ipr2.eq.0 .AND. u2.gt.0.d0) u2=0.d0
         if(ipr3.eq.0 .AND. u3.gt.0.d0) u3=0.d0
         if(ipr4.eq.0 .AND. u4.gt.0.d0) u4=0.d0

          u12=0.5d0*(u1+u2)
          u23=0.5d0*(u2+u3)
          u34=0.5d0*(u3+u4)
          u14=0.5d0*(u1+u4)

          u0=0.5d0*(u12+u34)

          sij4=0.25d0*dr(i)*dz(j)

          cur1=0.d0
          cur2=0.d0
          cur3=0.d0
          cur4=0.d0

         if(isum.eq.4) then

          cur1=sij4*0.25d0*(funcur(x1,u1)+funcur(x12,u12)
     +                     +funcur(x0,u0)+funcur(x14,u14) )

          cur2=sij4*0.25d0*(funcur(x2,u2)+funcur(x23,u23)
     +                     +funcur(x0,u0)+funcur(x12,u12) )

          cur3=sij4*0.25d0*(funcur(x3,u3)+funcur(x34,u34)
     +                     +funcur(x0,u0)+funcur(x23,u23) )

          cur4=sij4*0.25d0*(funcur(x4,u4)+funcur(x14,u14)
     +                     +funcur(x0,u0)+funcur(x34,u34) )

          !cur1= funcur(x1,u1)*sij4  
          !cur2= funcur(x2,u2)*sij4  
          !cur3= funcur(x3,u3)*sij4  
          !cur4= funcur(x4,u4)*sij4  

          go to 9010
         endif

c...quadr..(i,j)...

         if(ipr1.eq.1) then

          cur1=bcur(x1,x12,x0,x14, z1,z12,z0,z14, u1,u12,u0,u14)

         elseif(u12.gt.0.d0) then

          cur1=bcur(x12,x0,x14,x1, z12,z0,z14,z1, u12,u0,u14,u1)

         elseif(u0.gt.0.d0) then

          cur1=bcur(x0,x14,x1,x12, z0,z14,z1,z12, u0,u14,u1,u12)

         elseif(u14.gt.0.d0) then

          cur1=bcur(x14,x1,x12,x0, z14,z1,z12,z0, u14,u1,u12,u0)

         endif

c...quadr..(i+1,j)...

         if(ipr2.eq.1) then

          cur2=bcur(x2,x23,x0,x12, z2,z23,z0,z12, u2,u23,u0,u12)

         elseif(u23.gt.0.d0) then

          cur2=bcur(x23,x0,x12,x2, z23,z0,z12,z2, u23,u0,u12,u2)

         elseif(u0.gt.0.d0) then

          cur2=bcur(x0,x12,x2,x23, z0,z12,z2,z23, u0,u12,u2,u23)

         elseif(u12.gt.0) then

          cur2=bcur(x12,x2,x23,x0, z12,z2,z23,z0, u12,u2,u23,u0)

         endif

c...quadr..(i+1,j+1)...

         if(ipr3.eq.1) then

          cur3=bcur(x3,x34,x0,x23, z3,z34,z0,z23, u3,u34,u0,u23)

         elseif(u34.gt.0.d0) then

          cur3=bcur(x34,x0,x23,x3, z34,z0,z23,z3, u34,u0,u23,u3)

         elseif(u0.gt.0.d0) then

          cur3=bcur(x0,x23,x3,x34, z0,z23,z3,z34, u0,u23,u3,u34)

         elseif(u23.gt.0.d0) then

          cur3=bcur(x23,x3,x34,x0, z23,z3,z34,z0, u23,u3,u34,u0)

         endif

c...quadr..(i,j+1)...

         if(ipr4.eq.1) then

          cur4=bcur(x4,x14,x0,x34, z4,z14,z0,z34, u4,u14,u0,u34)

         elseif(u14.gt.0.d0) then

          cur4=bcur(x14,x0,x34,x4, z14,z0,z34,z4, u14,u0,u34,u4)

         elseif(u0.gt.0.d0) then

          cur4=bcur(x0,x34,x4,x14, z0,z34,z4,z14, u0,u34,u4,u14)

         elseif(u34.gt.0.d0) then

          cur4=bcur(x34,x4,x14,x0, z34,z4,z14,z0, u34,u4,u14,u0)

         endif

 9010   continue

          tokp=tokp+cur1+cur2+cur3+cur4

          curf(i,j) =     curf(i,j)    + cur1
          curf(i+1,j) =   curf(i+1,j)  + cur2
          curf(i+1,j+1) = curf(i+1,j+1)+ cur3
          curf(i,j+1) =   curf(i,j+1)  + cur4

 901    continue
 900    continue
c----------------------------------------------------------

        tokn=tok

        if(ngav1.eq.2 .OR. ngav1.eq.3) then
          if(iter.ge.iterbf .OR. nnstpp.ge.1) then
             uartm=clr*rl*rl+clz*zl
             tok=tok*(ucen-uem-uartm)/(um-uem-uartm)
          endif
        endif
                 
        wght=0.25d0

        if(ngav1.eq.4 .OR. ngav1.eq.5) then
          if(iter.ge.iterbf .OR. nnstpp.ge.1) then
             tok=tok*helinp/helout
             tok=tok*wght+tokn*(1.d0-wght)
          endif
        endif

        cnor=amu0*tok/tokp
	   !if(ngav1.eq.10) cnor=1.d0  !!<<<<<<<<

        !write(6,*) 'right:tok,tokp,cnor'
        !write(6,*)  tok,tokp,cnor 
        !write(6,*)'tokn,tok', tokn, tok
c        write(6,*)'h0,  h  ', helinp, helout

c----------------------------------------------------------

!!!!!!!!!!!!!like Rus!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	i_rus=0
	if(i_rus.eq.1)then

           tokp=0.d0

          do i=2,ni1
          do j=2,nj1

          curf(i,j) =  0.d0

	    iprij=ipr(i,j)
	    xi=r(i)
	    psi=un(i,j)
	    sij=dri(i)*dzj(j)

        if(iprij.eq.1) then
          curf(i,j) =  funcur( xi,psi )*sij
           tokp=tokp+curf(i,j)
        endif

          enddo 
          enddo 
        cnor=amu0*tok/tokp

	end if


!
!!!!!!!!!!!!!!!!!!like Rus!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!           f_cur=0.d0
!
!          do i=2,ni1
!          do j=2,nj1
!
!	    iprij=ipr(i,j)
!	    xi=r(i)
!	    psi=un(i,j)
!	    sij=dri(i)*dzj(j)
!
!        if(iprij.eq.1) then
!          f_cur=f_cur+funcur_f( xi,psi )*sij
!        endif
!
!          enddo 
!          enddo 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c---right hand side definition

         do 950 i=2,ni1
         do 951 j=2,nj1
           curf(i,j) = cnor*curf(i,j)
           il=nlin(i,j)

           if(iter.eq.1) then
           right(il)=-curf(i,j)
           else
           right(il)=-curf(i,j)*omg-(1.d0-omg)*curn(i,j)
           endif

           curn(i,j)=curf(i,j)
           curf(i,j)= curf(i,j)/(dri(i)*dzj(j))

         ! write(6,*) 'i,j,ipr(i,j),curf(i,j)'
         ! write(6,*)  i,j,ipr(i,j),curf(i,j)

 951    continue
 950    continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine xpoint(ux,rx,zx,ix,jx,dp,numxp,kodex)

         include 'double.inc'
          parameter(nshp=10)
         include 'param.inc'
         include 'comblc.inc'

          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)

          ix00=ix
          jx00=jx

          numitc=0
          kodex=0
 444    continue

          numitc=numitc+1

            if(numitc.ge.30) then
c           write(6,*) 'not correct definition x-point location'
c           write(6,*) 'code=444 (cell,containing x-point)'
            return
            endif

          if(rx.ge.rmax .OR. rx.le.rmin .OR.
     *       zx.ge.zmax .OR. zx.le.zmin) then
ccc           write(6,*)' xpoint',numxp,'out of box'
ccc           write(6,*)' rx,zx ',rx,zx

           ux=-1.d6
           kodex=1
           return
          endif

c---definition of cell, containig x-point

        do 300 i=1,ni1
          icx=i
        if( (rx.lt.r(i+1)) .AND. (rx.ge.r(i)) ) go to 301
 300    continue
 301    continue

        do 310 j=1,nj1
          jcx=j
        if( (zx.lt.z(j+1)) .AND. (zx.ge.z(j)) ) go to 311
 310    continue
 311    continue

          if(icx.eq.1 .OR. icx.eq.ni1 .OR.
     *       jcx.eq.1 .OR. jcx.eq.nj1) then
ccc           write(6,*)' xpoint',numxp,'in bound cell'
ccc           write(6,*)' icell,jcell ',icx,jcx

           ux=-1.d6
           kodex=1

           return
          endif

ccc       write(6,*) 'icelx,jcelx',icx,jcx,rx,zx

c---define nearest knote

         sdmin=rmax

        do 320 k=0,1
         rr=r(icx+k)
        do 325 l=0,1
         zz=z(jcx+l)
         dlx=dsqrt( (rr-rx)**2+(zz-zx)**2 )
       if(dlx.lt.sdmin) then
        sdmin=dlx
         ix=icx+k
         jx=jcx+l
       endif
 325    continue
 320    continue

          numit=0
 555    continue
          numit=numit+1

ccc       write(6,*) 'ix,jx',ix,jx
ccc       write(6,*) 'ix0,jx0',ix00,jx00

          nsh=1
          xs(nsh)=r(ix)
          ys(nsh)=z(jx)
         fun(nsh)=u(ix,jx)

        do 400 k=-1,1

          i= ix+k

        do 410 l=-1,1

          j= jx+l

          if(k.eq.0  .AND. l.eq.0 ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)
         fun(nsh)=u(i,j)

 410    continue
 400    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

        DET = dp(3)*dp(5) - dp(4)**2

       !   if(det.gt.0.d0) then
ccc    !       write(*,*)' xpoint',numxp,'det>0'
       !    ux=-1.d6
       !    kodex=1
       !    return
       !   endif

        Rx = Xs(1) + ( dp(2)*dp(4) - dp(1)*dp(5) )/DET
        Zx = Ys(1) + ( dp(1)*dp(4) - dp(2)*dp(3) )/DET

         rrx=xs(1)
         zzx=ys(1)

       ux=fun(1)+ dp(1)*(rx-rrx) + dp(2)*(zx-zzx)
     +          + 0.5d0*dp(3)*(rx-rrx)*(rx-rrx)
     +          +       dp(4)*(rx-rrx)*(zx-zzx)
     +          + 0.5d0*dp(5)*(zx-zzx)*(zx-zzx)

ccc       write(6,*) 'rx zx ux',rx,zx,ux

        if( rx.gt.r(ix+1) .OR. rx.lt.r(ix-1) ) go to 444
        if( zx.gt.z(jx+1) .OR. zx.lt.z(jx-1) ) go to 444

         sdmin=rmax

          do 500 k=-1,1
             rr=r(ix+k)
          do 510 l=-1,1
             zz=z(jx+l)
         dlx=dsqrt( (rr-rx)**2+(zz-zx)**2 )
       if(dlx.lt.sdmin) then
        sdmin=dlx
         ixp=ix+k
         jxp=jx+l
       endif
 510    continue
 500    continue

         if(ixp.ne.ix .OR. jxp.ne.jx) then

            if(numit.ge.20) then
c           write(6,*) 'not correct definition x-point location'
c           write(6,*) 'code=555 (nearest knote to x-point)'
            return
            endif

           ix=ixp
           jx=jxp

           go to 555

         endif

       if(ix.ne.ix00 .OR. jx.ne.jx00) then

        dold=dsqrt( (rx-r(ix00))**2 + (zx-z(jx00))**2 )
        dnew=dsqrt( (rx-r(ix))**2 + (zx-z(jx))**2 )
        deld01=dabs(dold-dnew)

        if(deld01.lt.0.001d0*dr(ix)) then
        ix=ix00
        jx=jx00
        go to 555
        endif

       endif

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        real*8 function bcur( r1,r2,r3,r4,z1,z2,z3,z4,
     +                            psi1,psi2,psi3,psi4 )
         implicit real*8(a-h,o-z)

        squtr(x1,x2,x3,y1,y2,y3)=
     =                     0.5d0*(y3*(x2-x1)+y1*(x3-x2)+y2*(x1-x3))

        xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1+1.d-8)

         iflag1=0
         iflag2=0

        if(psi2.gt.0.d0 .AND. psi3.gt.0.d0 .AND. psi4.gt.0.d0) then

          !r10=r3
          !z10=z3

          !r20=r3
          !z20=z3

          !psi10=psi3
          !psi20=psi3

         !go to 100

         cur1=funcur(r1,psi1)
         cur2=funcur(r2,psi2)
         cur3=funcur(r3,psi3)
         cur4=funcur(r4,psi4)

         sij=squtr(r1,r2,r3,z1,z2,z3)+squtr(r1,r3,r4,z1,z3,z4)
         bcur=0.25d0*sij*(cur1+cur2+cur3+cur4)
         return

        endif

c.....definition of first zero

       if(psi2.le.0.d0) then

         r10=xzer(r1,r2,psi1,psi2)
         z10=xzer(z1,z2,psi1,psi2)
         psi10=0.d0

       elseif(psi3.le.0.d0) then

         r10=xzer(r2,r3,psi2,psi3)
         z10=xzer(z2,z3,psi2,psi3)
         psi10=0.

       elseif(psi4.le.0.d0) then

         iflag1=1
         r10=xzer(r3,r4,psi3,psi4)
         z10=xzer(z3,z4,psi3,psi4)
         psi10=0.d0

       endif

c.....definition of second zero

       if(psi4.le.0.d0) then

         r20=xzer(r1,r4,psi1,psi4)
         z20=xzer(z1,z4,psi1,psi4)
         psi20=0.d0

       elseif(psi3.le.0.d0) then

         r20=xzer(r4,r3,psi4,psi3)
         z20=xzer(z4,z3,psi4,psi3)
         psi20=0.d0

       elseif(psi2.le.0.d0) then

         iflag2=1
         r20=xzer(r3,r2,psi3,psi2)
         z20=xzer(z3,z2,psi3,psi2)
         psi20=0.d0

       endif

 100   continue

         cur1=funcur(r1,psi1)
         cur2=funcur(r2,psi2)
         cur3=funcur(r3,psi3)
         cur4=funcur(r4,psi4)
         cur10=funcur(r10,psi10)
         cur20=funcur(r20,psi20)

       zbcur = squtr(r1,r20,r4,z1,z20,z4)*(cur1+cur20+cur4) +
     +         squtr(r1,r10,r20,z1,z10,z20)*(cur1+cur10+cur20) +
     +         squtr(r1,r2,r10,z1,z2,z10)*(cur1+cur2+cur10) +
     +  iflag1*squtr(r2,r3,r10,z2,z3,z10)*(cur2+cur3+cur10) +
     +  iflag2*squtr(r3,r4,r20,z3,z4,z20)*(cur3+cur4+cur20)

c      if(psi3.gt.0. .AND. psi2.lt.0. .AND. psi4.lt.0.) then
c
c        r30=xzer(r3,r2,psi3,psi2)
c        z30=xzer(z3,z2,psi3,psi2)
c
c        r40=xzer(r4,r3,psi4,psi3)
c        z40=xzer(z4,z3,psi4,psi3)
c
c        psi30=0.
c        psi40=0.
c
c        cur30=funcur(r30,psi30)
c        cur40=funcur(r40,psi40)
c
c      zbcur = zbcur +
c    +         squtr(r3,r40,r30,z3,z40,z30)*(cur3+cur40+cur30) +
c    +         squtr(r10,r30,r20,z10,z30,z20)*(cur10+cur30+cur20) +
c    +         squtr(r30,r40,r20,z30,z40,z20)*(cur30+cur40+cur20)
c
c      endif

       bcur=zbcur/3.d0
!!!tst
       !bcur=0.d0  !!!tst!!!!
!!!tst

       return
       end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine xdetal(i,j,ix,jx,rxpn ,zxpn ,dp,nloc,
     *                    cur1,cur2,cur3,cur4)

         include 'double.inc'
          parameter(nlocp=64)
         include 'param.inc'
         include 'comblc.inc'

          real*8  dp(5)
          real*8 rloc(nlocp),zloc(nlocp)
          real*8 uloc(nlocp,nlocp),curloc(nlocp,nlocp)

             psilim=0.05d0

            nlc=(nloc+1)/2

          drloc=(r(i+1)-r(i))/(nloc-1.d0)
          dzloc=(z(j+1)-z(j))/(nloc-1.d0)

          rloc(1)=r(i)
          zloc(1)=z(j)

           do 10 is=2,nloc
             rloc(is)=rloc(is-1)+drloc
             zloc(is)=zloc(is-1)+dzloc
 10        continue

          uix=u(ix,jx)
          rix=r(ix)
          zjx=z(jx)

           do 100 is=1,nloc
             ris=rloc(is)
           do 110 js=1,nloc
             zjs=zloc(js)
       uisjs=uix +dp(1)*(ris-rix) + dp(2)*(zjs-zjx)
     +           + 0.5*dp(3)*(ris-rix)*(ris-rix)
     +           +     dp(4)*(ris-rix)*(zjs-zjx)
     +           + 0.5*dp(5)*(zjs-zjx)*(zjs-zjx)

       uloc(is,js)=(uisjs-up)/(um-up)

 110       continue
 100       continue

           do 200 is=1,nloc
             ris=rloc(is)
           do 210 js=1,nloc
             zjs=zloc(js)
             psiloc=uloc(is,js)

           if(psiloc.le.0.d0) then
             curloc(is,js)=0.d0
             go to 210
           endif

           rlim=frlim(dp,zjs,rxpn,zxpn,rm,zm)
           rmlim=frlim(dp,zm,rxpn,zxpn,rm,zm)
           if( (ris -rlim)*(rm-rmlim).le.0.) then
ccc+       if(zjs.gt.zvmax .OR. zjs.lt.zvmin) then
             curloc(is,js)=0.d0
             go to 210
           endif

c          if(psiloc.le.psilim) then
c            curlim=funcur(ris,psilim)
c            curloc(is,js)=(curlim*psiloc)/psilim
c            go to 210
c          endif
             curloc(is,js)=funcur(ris,psiloc)

 210       continue
 200       continue

          sloc=drloc*dzloc

c..quadr (i,j)

           do 1000 is=1,nlc-1
           do 1000 js=1,nlc-1

             cur1=cur1+curloc(is,js)+curloc(is+1,js)+
     +                 curloc(is+1,js+1)+curloc(is,js+1)

 1000      continue

          cur1=cur1*0.25d0*sloc

c..quadr (i+1,j)

           do 2000 is=nlc,nloc-1
           do 2000 js=1,nlc-1

             cur2=cur2+curloc(is,js)+curloc(is+1,js)+
     +                 curloc(is+1,js+1)+curloc(is,js+1)

 2000      continue

          cur2=cur2*0.25d0*sloc

c..quadr (i+1,j+1)

           do 3000 is=nlc,nloc-1
           do 3000 js=nlc,nloc-1

             cur3=cur3+curloc(is,js)+curloc(is+1,js)+
     +                 curloc(is+1,js+1)+curloc(is,js+1)

 3000      continue

          cur3=cur3*0.25d0*sloc

c..quadr (i,j+1)

           do 4000 is=1,nlc-1
           do 4000 js=nlc,nloc-1

             cur4=cur4+curloc(is,js)+curloc(is+1,js)+
     +                 curloc(is+1,js+1)+curloc(is,js+1)

 4000      continue

          cur4=cur4*0.25d0*sloc

          return
          end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine right1

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

ccc--- right hand side for eq. Lu=Jf in rectangular box.

        do 100 i=2,ni1

         ri=r(i)

                j=2

         dzm=dz(1)
         il=nlin(i,j)

         right(il)=right(il)-ui(i,j-1)*dri(i)/(dzm*ri)

                j=nj1

         dzp=dz(nj1)
         il=nlin(i,j)

         right(il)=right(il)-ui(i,j+1)*dri(i)/(dzp*ri)

 100    continue


        do 200 j=2,nj1


                i=ni1

         drp=dr(ni1)
         rpl=r12(ni1)
         il=nlin(i,j)

         right(il)=right(il)-ui(i+1,j)*dzj(j)/(rpl*drp)

                i=2

         drm=dr(1)
         rmn=r12(1)
         il=nlin(i,j)

         right(il)=right(il)-ui(i-1,j)*dzj(j)/(rmn*drm)

 200    continue

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         real*8 function frlim(dp,ylim,rx,zx,rm,zm)

          implicit real*8(a-h,o-z)
          real*8 dp(5)

             Dxx=dp(3)
             Dxy=dp(4)
             Dyy=dp(5)

             disc=(Dxy/Dyy)**2 - Dxx/Dyy

          if(Disc.lt.0.) then

          !  write(6,*) ' FRLIM: ***ERROR disc lt.0'
          !  write(6,*) ' rx zx ',rx,zx
          !  stop

             cc=(zm-zx)/(rm-rx)

             cc=-1.d0/cc
             go to 100

          endif

             cdpls = -Dxy/Dyy + dsqrt(disc)
             cdmns = -Dxy/Dyy - dsqrt(disc)

             ang1 = 0.5d0*(datan(cdpls)+datan(cdmns))
             ang2 =-0.5d0*(datan(1.d0/cdpls)+datan(1.d0/cdmns))


c.........calculation D2u/Dl2(direction ang1    )

             c1=dtan(ang1)
             Dl2x=Dyy*c1*c1+2.d0*Dxy*c1+Dxx

c.........calculation D2u/Dl2(direction ang2    )

             c2=dtan(ang2)
             Dl2y=Dyy*c2*c2+2.*Dxy*c2+Dxx

          if(Dl2x.lt.0.) then

             cc=c1

          elseif(Dl2y.lt.0.) then

             cc=c2

          else

          !  write(6,*) ' FRLIM: ***ERROR both deriv. ge.0'
          !  write(6,*) ' rx zx ',rx,zx
          !  call wrd
          !  stop

             cc=(zm-zx)/(rm-rx)

             cc=-1.d0/cc

          endif

 100      continue

          frlim=rx+(ylim-zx)/cc

          return
          end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


