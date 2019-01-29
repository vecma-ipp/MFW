         subroutine psiloop(rlop,zlop,psilop,nlop)

         include 'double.inc'
         dimension rlop(*),zlop(*),psilop(*)

         call f_psloop_e(rlop,zlop,psilop,nlop)
         call psloop_p(rlop,zlop,psilop,nlop)

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine b_prob(rprob,zprob,hrprob,hzprob,nprob,bprob)

         include 'double.inc'
         dimension rprob(*),zprob(*),hrprob(*),hzprob(*),bprob(*)

         call bprob_e(rprob,zprob,hrprob,hzprob,nprob,bprob)
         call bprob_p(rprob,zprob,hrprob,hzprob,nprob,bprob)

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine bprob_p(rprob,zprob,hrprob,hzprob,nprob,bprob)

         include 'double.inc'
         parameter(nshp=10)
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         dimension rprob(*),zprob(*),hrprob(*),hzprob(*),bprob(*)
         dimension rsh(nshp),zsh(nshp),ush(nshp)
         dimension dp(5)

          sqrt(arg)=dsqrt(arg)

           ik_out=0

        do 100 ik=1,nprob

          r0=rprob(ik)
          z0=zprob(ik)

         call numcel(r0,z0,ic,jc)

         if(iprprob(ik).eq.1) then !!!

          if(ic.eq.nr) then 
           write(*,*) 'probe',ik,'is out of box'
           write(*,*) 'program is terminated'
           stop
          endif

          dismin=rm
         do ii=0,1
          is=ic+ii
         do jj=0,1
          js=jc+jj
          dist=sqrt( (r0-r(is,js))**2 + (z0-z(is,js))**2 )
          if(dist .lt. dismin) then
            dismin=dist
            ick=is
            jck=js
          endif
         enddo
         enddo

          if(jck.eq.nt) jck=2
          if(ick.eq.nr) ick=nr-1

          nsh=1
          rsh(nsh)=r(ick,jck)
          zsh(nsh)=z(ick,jck)
          ush(nsh)=psii(ick,jck)
         do ii=-1,1
          is=ick+ii
         do jj=-1,1
          js=jck+jj
          if(ii.ne.0 .or. jj.ne.0) then
          nsh=nsh+1
          rsh(nsh)=r(is,js)
          zsh(nsh)=z(is,js)
          ush(nsh)=psii(is,js)
          endif
         enddo
         enddo

          call deriv5(rsh,zsh,ush,nsh,5,dp)

            dudr=dp(1)+dp(3)*(r0-rsh(1))+dp(4)*(z0-zsh(1))
            dudz=dp(2)+dp(5)*(z0-zsh(1))+dp(4)*(r0-rsh(1)) 

            br=-dudz/r0
            bz= dudr/r0
            bpla=br*hrprob(ik)+bz*hzprob(ik)
            bprob(ik)=bprob(ik)+bpla

         else  !!!

           if(ic.ne.nr) then
            write(*,*) 'bprob_p:prob is in the box',ic,nr
            write(*,*) 'program is terminated'
            write(*,*) 'you need to consult program provider'
            stop
           endif

           ik_out=ik_out+1

            dudr=0.d0
            dudz=0.d0

          do 110 j=2,nt1

            dudr = dudr-adginr(ik_out,j)*(dgdn(j)+dgdn(j+1))*0.5d0
            dudz = dudz-adginz(ik_out,j)*(dgdn(j)+dgdn(j+1))*0.5d0

 110       continue

            br=-dudz/r0
            bz= dudr/r0
            bpla=br*hrprob(ik)+bz*hzprob(ik)
            bprob(ik)=bprob(ik)+bpla

         endif  !!!

 100       continue

           if(ik_out.ne.nprob_out) then
            write(*,*) 'bprob_p:ik_out.ne.nprob_out',ik_out,nprob_out
            write(*,*) 'program is terminated'
            write(*,*) 'you need to consult program provider'
            stop
           endif

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine blic_d(r0,z0, r1,r2,r3,r4, z1,z2,z3,z4,
     *                             u1,u2,u3,u4, u0, dudr,dudz)
          include 'double.inc'

          !dimension a(4,4),f(4),x(4),iwrk(4)

           eps=1.d-9
!!!!!!!!test!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           r1=1.d0
           r4=1.d0
           r2=2.d0
           r3=2.d0

           z1=1.d0
           z4=2.d0
           z2=1.d0
           z3=2.d0

           u1=1.d0
           u4=1.d0
           u2=0.d0
           u3=0.d0

           r0=1.5d0
           z0=1.5d0

!!!!!!!!!test!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ar=r3-r4+r1-r2+eps
      br=-r1+r2
      cr=-r1+r4
      dr= r1

      az=z3-z4+z1-z2+eps
      bz=-z1+z2
      cz=-z1+z4
      dz= z1

      au=u3-u4+u1-u2
      bu=-u1+u2
      cu=-u1+u4
      du= u1

      cx = dr-r0-cr/(-az*cr+ar*cz)*az*r0+cr/(-az*cr+ar*cz)*az*dr+cr/(-az
     #*cr+ar*cz)*ar*z0-cr/(-az*cr+ar*cz)*ar*dz

      bx = -ar/(-az*cr+ar*cz)*az*r0+ar/(-az*cr+ar*cz)*az*dr+ar**2/(-az*c
     #r+ar*cz)*z0-ar**2/(-az*cr+ar*cz)*dz+cr/(-az*cr+ar*cz)*az*br-cr/(-a
     #z*cr+ar*cz)*ar*bz+br

      ax = ar/(-az*cr+ar*cz)*az*br-ar**2/(-az*cr+ar*cz)*bz

      if(abs(ax/bx).lt.1.d-8) then
       x=-cx/bx
       go to 10
      endif

      det_=bx**2-4.d0*ax*cx
      if(det_.lt.0.d0) then
       write(*,*) 'blic_d: det<0'
       pause 'pause'
      endif
      x1 = 0.5d0*(-bx + sqrt(det_))/ax
      x2 = 0.5d0*(-bx - sqrt(det_))/ax
      if(x1.ge.0.d0 .And. x1.le.1.d0) then
       x=x1
      elseif(x2.ge.0.d0 .And. x2.le.1.d0) then
       x=x2
      else
       write(*,*) 'blic_d: x<0 or x>1'
       pause 'pause'
      endif
      
10     continue

      y = -(az*r0-az*br*x-az*dr-ar*z0+ar*bz*x+ar*dz)/(-az*cr+ar*cz)

      u0=au*x*y + bu*x + cu*y + du

      dudx=au*y + bu
      dudy=au*x + cu

      drdx=ar*y + br
      drdy=ar*x + cr
      
      dzdx=az*y + bz
      dzdy=az*x + cz

      det = drdx*dzdy - drdy*dzdx
      det_r = dudx*dzdy - dudy*dzdx
      det_z = drdx*dudy - drdy*dudx

      dudr=det_r/det
      dudz=det_z/det

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          subroutine blic_d_(r0,z0, r1,r2,r3,r4, z1,z2,z3,z4,
     *                             u1,u2,u3,u4, u0, dudr,dudz)
          include 'double.inc'

          dimension a(4,4),f(4),x(4),iwrk(4)


!!!!!!!!test!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!           r1=1.d0
!           r4=1.d0
!           r2=2.d0
!           r3=2.d0
!
!           z1=1.d0
!           z4=2.d0
!           z2=1.d0
!           z3=2.d0
!
!           u1=0.d0
!           u4=1.d0
!           u2=1.d0
!           u3=0.d0
!
!           r0=1.5d0
!           z0=1.5d0
!
!!!!!!!!!test!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






                a(1,1)=r1       !a(1,1) a(1,2) a(1,3) a(1,4)! !x(1)! !f(1)!
                a(2,1)=r2       !a(2,1) a(2,2) a(2,3) a(2,4)! !x(2)! !f(2)!
                a(3,1)=r3       !a(3,1) a(3,2) a(3,3) a(3,4)!*!x(3)!=!f(3)!
                a(4,1)=r4       !a(4,1) a(4,2) a(4,3) a(4,4)! !x(4)! !f(4)!

                a(1,2)=z1
                a(2,2)=z2
                a(3,2)=z3
                a(4,2)=z4

                a(1,3)=r1*z1
                a(2,3)=r2*z2
                a(3,3)=r3*z3
                a(4,3)=r4*z4

                a(1,4)=1.d0
                a(2,4)=1.d0
                a(3,4)=1.d0
                a(4,4)=1.d0

                f(1)=u1
                f(2)=u2
                f(3)=u3
                f(4)=u4

           call ge(4,4,a,f,x,iwrk)

                u0=x(1)*r0+x(2)*z0+x(3)*r0*z0+x(4)

                dudr=x(1)+x(3)*z0
                dudz=x(2)+x(3)*r0

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine f_psloop_e(rlop,zlop,psilop,nlop)

         include 'double.inc'
         include 'parrc1.inc'
         include 'comrec.inc'
         dimension rlop(*),zlop(*),psilop(*)

          ddx=x(2)-x(1)
          ddy=y(2)-y(1)

         do i=1,nlop

          r0=rlop(i)
          z0=zlop(i)

          ic=(r0-x(1))/ddx+1
          jc=(z0-y(1))/ddy+1

          r1=x(ic)
          r2=x(ic+1)

          z1=y(jc)
          z2=y(jc+1)

          u1=ue(ic,jc)
          u2=ue(ic+1,jc)
          u3=ue(ic+1,jc+1)
          u4=ue(ic,jc+1)

           psilop(i)=blin_(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4 )

         enddo

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine psloop_p(rlop,zlop,psilop,nlop)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         real*8 psilop(*),rlop(*),zlop(*)

          sqrt(arg)=dsqrt(arg)

           ik_out=0

        do 100 ik=1,nlop

          r0=rlop(ik)
          z0=zlop(ik)

         call numcel(r0,z0,ic,jc)

         if(iprlop(ik).eq.1) then !!!

          if(ic.eq.nr) then
           write(*,*) 'psiloop_p: bug'
           write(*,*) 'loop',ik,'is out of box'
           write(*,*) 'iprlop(ik)=',iprlop(ik)
           write(*,*) 'program is terminated'
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

          u1=psii(ic,jc)
          u2=psii(ic+1,jc)
          u3=psii(ic+1,jc+1)
          u4=psii(ic,jc+1)

           call blic_d(r0,z0,r1,r2,r3,r4,z1,z2,z3,z4,
     *                       u1,u2,u3,u4,u0,dudr,dudz)

          psilop(ik)=psilop(ik)+u0

         else  !!!

           if(ic.ne.nr) then
            write(*,*) 'psiloop_p:loop',ik,'is into the box',ic,nr
            write(*,*) 'program is terminated'
            write(*,*) 'you need to consult program provider'
            stop
           endif

           ik_out=ik_out+1

          do 110 j=2,nt1

         psilop(ik)=psilop(ik)
     *             -adginl(ik_out,j)*(dgdn(j)+dgdn(j+1))*0.5d0

 110       continue

         endif  !!!

 100       continue

           if(ik_out.ne.nlop_out) then
            write(*,*) 'psiloop_p:ik_out.ne.nlop_out',ik_out,nlop_out
            write(*,*) 'program is terminated'
            write(*,*) 'you need to consult program provider'
            stop
           endif

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine bprob_e(rprob,zprob,hrprob,hzprob,nprob,bprob)

         include 'double.inc'
         include 'parrc1.inc'
         include 'comrec.inc'
         dimension rprob(*),zprob(*),hrprob(*),hzprob(*),bprob(*)

          ddx=x(2)-x(1)
          ddy=y(2)-y(1)

         do i=1,nprob

          r0=rprob(i)
          z0=zprob(i)

          ic=(r0-x(1))/ddx+1
          jc=(z0-y(1))/ddy+1

          r1=x(ic)
          r2=x(ic+1)

          z1=y(jc)
          z2=y(jc+1)

          u1=ue(ic,jc)
          u2=ue(ic+1,jc)
          u3=ue(ic+1,jc+1)
          u4=ue(ic,jc+1)

           call blin_d(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4,dudr,dudz)

            br=-dudz/r0
            bz= dudr/r0
            bprob(i)=br*hrprob(i)+bz*hzprob(i)
         enddo

         return
         end

         subroutine extpol

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'parrc1.inc'
         include 'compol.inc'
         include 'compol_add.inc'
         include 'comrec.inc'


          ddx=x(2)-x(1)
          ddy=y(2)-y(1)

        do i=1,nr
         do j=1,nt

          r0=r(i,j)
          z0=z(i,j)

          ic=(r0-x(1))/ddx+1
          jc=(z0-y(1))/ddy+1

          r1=x(ic)
          r2=x(ic+1)

          z1=y(jc)
          z2=y(jc+1)

          u1=ue(ic,jc)
          u2=ue(ic+1,jc)
          u3=ue(ic+1,jc+1)
          u4=ue(ic,jc+1)

           psie(i,j)=blin_(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4 )

         enddo
        enddo

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine psi_inter

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'parrc1.inc'
         include 'compol.inc'
         include 'compol_add.inc'
         include 'comrec.inc'

        dimension psi_sur(ntp)

          ddx=x(2)-x(1)
          ddy=y(2)-y(1)

        !do i=1,nr
           i=iplas
       do j=1,nt

          r0=r(i,j)
          z0=z(i,j)

          ic=(r0-x(1))/ddx+1
          jc=(z0-y(1))/ddy+1

          r1=x(ic)
          r2=x(ic+1)

          z1=y(jc)
          z2=y(jc+1)

          u1=u(ic,jc)
          u2=u(ic+1,jc)
          u3=u(ic+1,jc+1)
          u4=u(ic,jc+1)

           psi_sur(j)=blin_(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4 )

         enddo
        !enddo

         return
         end



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine f_psiful

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'


        do i=1,nr
         do j=1,nt

            psi(i,j)=psii(i,j)+psie(i,j)

         enddo
        enddo


	!open(3,file='boncon.wr')
	!write(3,*) (psi(iplas,j),j=2,nt1)
	!close(3)
	!stop

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine blin_d(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4,dudr,dudz)
         include 'double.inc'

         s=(r2-r1)*(z2-z1)

         dudr=( (u2-u1)*(z2-z0)+(u3-u4)*(z0-z1) )/s
         dudz=( (u4-u1)*(r2-r0)+(u3-u2)*(r0-r1) )/s

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine artfil

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           parameter(nshp=ntp+1)
           include 'compol.inc'
           include 'compol_add.inc'

          dimension xs(nshp),ys(nshp),fun(nshp),dpm(5)
	common
     *  /c_kpr/kpr

 1234    continue

          nsh=1

          xs(nsh)=r(1,1)
          ys(nsh)=z(1,1)
         fun(nsh)=psi(1,1)
          if(kpr.eq.1) then
          write(*,*) 'artfil:psi(1,1)',fun(nsh)
          endif

        do 200 j=2,nt1

          nsh=nsh+1
          xs(nsh)=r(2,j)
          ys(nsh)=z(2,j)
         fun(nsh)=psi(2,j)

 200    continue

          call deriv5(xs,ys,fun,nsh,5,dpm)

        clrn= -dpm(1)*0.5d0/rm
        clzn= -dpm(2)

         sigma=1.d0

        clr=sigma*clrn+(1.d0-sigma)*clr
        clz=sigma*clzn+(1.d0-sigma)*clz

        do 250 i=1,nr
        do 250 j=1,nt

         psi(i,j)=psi(i,j)+clz*z(i,j)+clr*r(i,j)*r(i,j)

 250    continue

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_xpoint(rx,zx,ix,jx,psx,tet0,kodex)

         include 'double.inc'
          parameter(nshp=20)
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         dimension dp(10),xs(nshp),ys(nshp),fun(nshp)

          sqrt(arg)=dsqrt(arg)

          numbit=0
          kodex=0

 100      numbit=numbit+1

          rxn=rx
          zxn=zx

          ixn=ix
          jxn=jx

          dr0=rx-rm
          dz0=zx-zm

          ro0=sqrt(dr0**2+dz0**2)

           tetp=dacos(dr0/ro0)
          if(dz0.lt.0.d0) then
           tet0=2.d0*pi-tetp
          else
           tet0=tetp
          endif

            if(tet0.lt.teta(1)) tet0=tet0+2.d0*pi
            if(tet0.gt.teta(nt)) tet0=tet0-2.d0*pi

         do j=1,nt1

          if(tet0.ge.teta(j) .AND. tet0.lt.teta(j+1)) jc=j

         enddo

          dro=1.d9

         !do i=iplas-3,nr1
         do i=3,nr1

          dromn=sqrt((r(i+1,jc)-rx)**2+(z(i+1,jc)-zx)**2)
          dropl=sqrt((r(i+1,jc+1)-rx)**2+(z(i+1,jc+1)-zx)**2)

           if(dropl.le.dro .AND. dropl.le.dromn) then

             dro=dropl
             ix=i+1
             jx=jc+1

           elseif(dromn.le.dro .AND. dromn.le.dropl) then

             dro=dromn
             ix=i+1
             jx=jc

           endif

         enddo

            if(jx.eq.nt) jx=2   
            if(jx.eq.1) jx=nt1   

            if(ix.eq.nr) then   !xpoint out of box

              kodex=1
              return

            endif

          nsh=1
          xs(nsh)=r(ix,jx)
          ys(nsh)=z(ix,jx)
         fun(nsh)=psi(ix,jx)

        do 400 k=-1,1

          i= ix+k

        do 410 l=-1,1

          j= jx+l

          if(k.eq.0 .AND. l.eq.0 ) go to 410
          if(i.gt.nr ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i,j)
          ys(nsh)=z(i,j)
         fun(nsh)=psi(i,j)

 410    continue
 400    continue


        do 500 k=-2,2,2

          i= ix+k

        !do 510 l=-2,2,2 

          !j= jx+l 
          j= jx

          !if(k.eq.0 .AND. l.eq.0 ) go to 510
          if(k.eq.0) go to 500
          if(i.gt.nr ) go to 500
          nsh=nsh+1
          xs(nsh)=r(i,j)
          ys(nsh)=z(i,j)
         fun(nsh)=psi(i,j)

 !510    continue 
 500    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

        DET = dp(3)*dp(5) - dp(4)**2

        Rx = Xs(1) + ( dp(2)*dp(4) - dp(1)*dp(5) )/DET
        Zx = Ys(1) + ( dp(1)*dp(4) - dp(2)*dp(3) )/DET

         rrx=xs(1)
         zzx=ys(1)

       psx=fun(1)+ dp(1)*(rx-rrx) + dp(2)*(zx-zzx)
     +           + 0.5d0*dp(3)*(rx-rrx)*(rx-rrx)
     +           +     dp(4)*(rx-rrx)*(zx-zzx)
     +           + 0.5d0*dp(5)*(zx-zzx)*(zx-zzx)

         ! write(6,*) 'rx zx psix',rx,zx,psx

        xxyy=dp(3)*dp(5)


        if( (ix.ne.ixn .OR. jx.ne.jxn).AND.(numbit.lt.10) ) then

           go to 100

        else

        xxyy=dp(3)*dp(5)
        !if(xxyy.ge.0.d0) kodex=1

           return

        endif

        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_xpoint2(rx,zx,ix,jx,psx,tet0,kodex)

         include 'double.inc'
          parameter(nshp=20)
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         dimension dp(10),xs(nshp),ys(nshp),fun(nshp)
         dimension dp_e(10)

          sqrt(arg)=dsqrt(arg)

          numbit=0
          kodex=0

 100      numbit=numbit+1

          rxn=rx
          zxn=zx

          ixn=ix
          jxn=jx


!!!!!!!!nearest node definition(ix,ix)

          dr0=rx-rm
          dz0=zx-zm

          ro0=sqrt(dr0**2+dz0**2)

           tetp=dacos(dr0/ro0)
          if(dz0.lt.0.d0) then
           tet0=2.d0*pi-tetp
          else
           tet0=tetp
          endif

            if(tet0.lt.teta(1)) tet0=tet0+2.d0*pi
            if(tet0.gt.teta(nt)) tet0=tet0-2.d0*pi

         do j=1,nt1

          if(tet0.ge.teta(j) .AND. tet0.lt.teta(j+1)) jc=j

         enddo

          dro=1.d9

         do i=iplas-3,nr1

          dromn=sqrt((r(i+1,jc)-rx)**2+(z(i+1,jc)-zx)**2)
          dropl=sqrt((r(i+1,jc+1)-rx)**2+(z(i+1,jc+1)-zx)**2)

           if(dropl.le.dro .AND. dropl.le.dromn) then

             dro=dropl
             ix=i+1
             jx=jc+1

           elseif(dromn.le.dro .AND. dromn.le.dropl) then

             dro=dromn
             ix=i+1
             jx=jc

           endif

         enddo

            if(jx.eq.nt) jx=2   
            if(jx.eq.1) jx=nt1   

            if(ix.eq.nr) then   !xpoint out of box

              kodex=1
              return

            endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


          nsh=1
          xs(nsh)=r(ix,jx)
          ys(nsh)=z(ix,jx)
         fun(nsh)=psii(ix,jx)

        do 400 k=-1,1

          i= ix+k

        do 410 l=-1,1

          j= jx+l

          if(k.eq.0 .AND. l.eq.0 ) go to 410
          if(i.gt.nr ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i,j)
          ys(nsh)=z(i,j)
         fun(nsh)=psii(i,j)

 410    continue
 400    continue


        do 500 k=-2,2,2

          i= ix+k

        !do 510 l=-2,2,2 

          !j= jx+l 
          j= jx

          !if(k.eq.0 .AND. l.eq.0 ) go to 510
          if(k.eq.0) go to 500
          if(i.gt.nr ) go to 500
          nsh=nsh+1
          xs(nsh)=r(i,j)
          ys(nsh)=z(i,j)
         fun(nsh)=psi(i,j)

 !510    continue 
 500    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


          call xpoint_e(psie_x,rx,zx,r_ix,z_jx,dp_e,kodex_e)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          dp1=dp(1)+dp_e(1)
          dp2=dp(2)+dp_e(2)
          dp3=dp(3)+dp_e(3)
          dp4=dp(4)+dp_e(4)
          dp5=dp(5)+dp_e(5)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        DET = dp(3)*dp(5) - dp(4)**2

        Rx = Xs(1) + ( dp(2)*dp(4) - dp(1)*dp(5) )/DET
        Zx = Ys(1) + ( dp(1)*dp(4) - dp(2)*dp(3) )/DET

         rrx=xs(1)
         zzx=ys(1)

       psx=fun(1)+ dp(1)*(rx-rrx) + dp(2)*(zx-zzx)
     +           + 0.5d0*dp(3)*(rx-rrx)*(rx-rrx)
     +           +     dp(4)*(rx-rrx)*(zx-zzx)
     +           + 0.5d0*dp(5)*(zx-zzx)*(zx-zzx)

         ! write(6,*) 'rx zx psix',rx,zx,psx

        xxyy=dp(3)*dp(5)


        if( (ix.ne.ixn .OR. jx.ne.jxn).AND.(numbit.lt.10) ) then

           go to 100

        else

        xxyy=dp(3)*dp(5)
        !if(xxyy.ge.0.d0) kodex=1

           return

        endif

        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine xpoint_e(ue_x,rx,zx,r_ix,z_jx,dp,kodex)

         include 'double.inc'
          parameter(nshp=10)
         include 'param.inc'
         include 'comblc.inc'

          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)

          kodex=0
 444    continue



          if(rx.ge.rmax .OR. rx.le.rmin .OR.
     *       zx.ge.zmax .OR. zx.le.zmin) then
ccc           write(6,*)' xpoint',numxp,'out of box'
ccc           write(6,*)' rx,zx ',rx,zx

           ue_x=-1.d6
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

           ue_x=-1.d6
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

 555    continue

ccc       write(6,*) 'ix,jx',ix,jx

          r_ix=r(ix)
          z_jx=z(jx)

          nsh=1
          xs(nsh)=r(ix)
          ys(nsh)=z(jx)
         fun(nsh)=ue(ix,jx)

        do 400 k=-1,1

          i= ix+k

        do 410 l=-1,1

          j= jx+l

          if(k.eq.0  .AND. l.eq.0 ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)
         fun(nsh)=ue(i,j)

 410    continue
 400    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

         rrx=xs(1)
         zzx=ys(1)

       ue_x=fun(1)+ dp(1)*(rx-rrx) + dp(2)*(zx-zzx)
     +          + 0.5d0*dp(3)*(rx-rrx)*(rx-rrx)
     +          +       dp(4)*(rx-rrx)*(zx-zzx)
     +          + 0.5d0*dp(5)*(zx-zzx)*(zx-zzx)

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine axpnt_e(ue_ax,r_ax,z_ax,i_ax,j_ax,dp,kodax)

         include 'double.inc'
          parameter(nshp=10)
         include 'param.inc'
         include 'comblc.inc'

          real*8 xs(nshp),ys(nshp),fun(nshp),dp(5)

          kodax=0
 444    continue



          if(r_ax.ge.rmax .OR. r_ax.le.rmin .OR.
     *       z_ax.ge.zmax .OR. z_ax.le.zmin) then
           write(*,*)'subroutine axpnt_e:'
           write(*,*)' axis is out of box'
           write(*,*)' r_ax,z_ax ',r_ax,z_ax

           kodax=1
           return
          endif

c---definition of cell, containig x-point

        do 300 i=1,ni1
          ic_ax=i
        if( (r_ax.lt.r(i+1)) .AND. (r_ax.ge.r(i)) ) go to 301
 300    continue
 301    continue

        do 310 j=1,nj1
          jc_ax=j
        if( (z_ax.lt.z(j+1)) .AND. (z_ax.ge.z(j)) ) go to 311
 310    continue
 311    continue

          if(ic_ax.eq.1 .OR. ic_ax.eq.ni1 .OR.
     *       jc_ax.eq.1 .OR. jc_ax.eq.nj1) then
           write(*,*)' ax_point in bound cell'
           write(*,*)' icell,jcell ',ic_ax,jc_ax

           kodax=1

           return
          endif


c---define nearest knote

         sdmin=rmax

        do 320 k=0,1
         rr=r(ic_ax+k)
        do 325 l=0,1
         zz=z(jc_ax+l)
         dl_ax=dsqrt( (rr-r_ax)**2+(zz-z_ax)**2 )
       if(dl_ax.lt.sdmin) then
        sdmin=dl_ax
         i_ax=ic_ax+k
         j_ax=jc_ax+l
       endif
 325    continue
 320    continue

 555    continue

ccc       write(6,*) 'ix,jx',ix,jx

          r_iax=r(i_ax)
          z_jax=z(j_ax)

          nsh=1
          xs(nsh)=r(i_ax)
          ys(nsh)=z(j_ax)
         fun(nsh)=ue(i_ax,j_ax)

        do 400 k=-1,1

          i= i_ax+k

        do 410 l=-1,1

          j= j_ax+l

          if(k.eq.0  .AND. l.eq.0 ) go to 410
          nsh=nsh+1
          xs(nsh)=r(i)
          ys(nsh)=z(j)
         fun(nsh)=ue(i,j)

 410    continue
 400    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

         rrax=xs(1)
         zzax=ys(1)

       ue_ax=fun(1)+ dp(1)*(r_ax-rrax) + dp(2)*(z_ax-zzax)
     +          + 0.5d0*dp(3)*(r_ax-rrax)*(r_ax-rrax)
     +          +       dp(4)*(r_ax-rrax)*(z_ax-zzax)
     +          + 0.5d0*dp(5)*(z_ax-zzax)*(z_ax-zzax)

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



