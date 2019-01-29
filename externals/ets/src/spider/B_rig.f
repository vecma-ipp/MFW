!! file contains following routines:
!!
!! funp
!! funf
!! rightg
!! rigbon 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine rightg

!! riht-hand side for problem L(g)=J

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

          common /comabw/ alf0p,alf1p,alf2p,bet0f,bet1f,bet2f
     *                   ,alw0p,alw1p,alw2p
	common
     *  /c_kpr/kpr

       common/com_flag/kastr
       common /com_0st/ key_0st,key_prs
       save cur_mu

         !!amu0=0.4d0*pi

          tokp=0.d0
          tokff=0.d0
          tokpp=0.d0
          tokww=0.d0

         do 1 il=1,neqp
            right(il)=0.d0
 1       continue


                if(kstep.eq.0 .OR. (ngav.eq.0 .AnD. kastr.eq.0)) then

                      !!!!!!!!!!!!!!!!!!!!
         do 2 i=1,iplas
         do 2 j=1,Nt
            cur(i,j)=0.d0
 2       continue

!! central point

          psn=psin(1,2)
          r0=r(1,2)

          curp=tabp(psn)
          curf=tabf(psn)
          curw=tabw(psn)

          dpdpsi(1)=curp
          dfdpsi(1)=curf
          dwdpsi(1)=curw

          curcen=r0*curp+curf/r0+curw*r0**3

              sqcen=0.d0

         do 10 j=2,nt1

            sqcen=sqcen+sq1(1,j)+sq4(1,j)

 10      continue

          tokp=curcen*sqcen
          tokff=curf*sqcen/r0
          tokpp=curp*sqcen*r0
          tokww=curw*sqcen*r0**3

          il=numlin(1,1,nr,nt)

          right(il)=curcen*sqcen

!!!!!!!!!!!!! regular points

         do 20 i=2,Iplas  !1 
         do 25 j=2,Nt1

         if(i.ne.iplas) then

          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
          r0=r(i,j)
          psn=psin(i,j)

         else

          sqk=sq2(i-1,j)+sq3(i-1,j-1)
          r0=r(i,j)
          psn=psin(i,j)
          r0=(3.d0*r(i,j)+r(i-1,j))*0.25d0
          psn=(3.d0*psin(i,j)+psin(i-1,j))*0.25d0

         endif

          curp=tabp(psn)
          curf=tabf(psn)
          curw=tabw(psn)

          cur(i,j)=r0*curp+curf/r0+curw*r0**3

         tokp=tokp+cur(i,j)*sqk
         tokff=tokff+curf*sqk/r0
         tokpp=tokpp+curp*sqk*r0
         tokww=tokww+curw*sqk*r0**3

            il=numlin(i,j,nr,nt)

          right(il)=cur(i,j)*sqk

 25      continue

          dpdpsi(i)=curp
          dfdpsi(i)=curf
          dwdpsi(i)=curw

 20      continue

          dpdpsi(iplas)=tabp(0.d0) !
          dfdpsi(iplas)=tabf(0.d0) !
          dwdpsi(iplas)=tabw(0.d0) !

              cnor=amu0*tok/tokp

C           write(*,*) 'cnor,tok,tokp'
C           write(*,*) cnor,tok,tokp

           if(key_0st.eq.1) cnor=1.d0

	  	tok_pl=0.d0


         do 30 j=1,Nt

            cur(1,j)=curcen*cnor


 30      continue

         do 40 i=2,iplas
         do 40 j=2,Nt1

            cur(i,j)=cur(i,j)*cnor


 40      continue

         do 45 i=1,iplas

            cur(i,1)=cur(i,nt1)
            cur(i,nt)=cur(i,2)

 45      continue

         do 50 il=1,neqpla

            right(il)=right(il)*cnor 

!test  psi=r**2

!          right(il)=0.d0

!test  psi=r**2

 50      continue

         do 55 i=1,iplas

            dpdpsi(i)=dpdpsi(i)*cnor !
            dfdpsi(i)=dfdpsi(i)*cnor !
            dwdpsi(i)=dwdpsi(i)*cnor !

 55      continue

          cur_mu=tokp*cnor
          tokp=tokp/amu0*cnor

                       !!!!!!!!!!!!!!!!!!!!!!!!
                     else   !!!!!!!!!!!!!!!! 
                       !!!!!!!!!!!!!!!!!!!!!!!!

          if(kastr.eq.1 .AND. key_prs.eq.1 .AND. erru.lt.5.d-3) then
             call pres_d_psi !
          endif

           !if(ngav.ne.0 ) then
             call procof(1,cur_mu)
           !endif
           
      dfdpsi_sur=-(fvac**2-f(iplas-1)**2)/psia(iplas-1)/psim


!!!!!!!!! central point

          curcen=rm*dpdpsi(1)+dfdpsi(1)/rm+dwdpsi(1)*rm**3

              sqcen=0.d0

         do 100 j=2,nt1

          sqcen=sqcen+sq1(1,j)+sq4(1,j)

 100      continue

            tokp=curcen*sqcen

            il=numlin(1,1,nr,nt)

          right(il)=curcen*sqcen

  	tok_pl=0.d0


         do 300 j=1,Nt

         cur(1,j)=curcen

 
 300     continue

!! regular points


         do 200 i=2,Iplas  !-1
         do 200 j=2,Nt1

         if(i.ne.iplas) then

          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
          r0=r(i,j)

          curp=dpdpsi(i)
          curf=dfdpsi(i)
          curw=dwdpsi(i)

         else

          sqk=sq2(i-1,j)+sq3(i-1,j-1)
         ! r0=r(i,j)
          r0=(3.d0*r(i,j)+r(i-1,j))*0.25d0

          curp=dpdpsi(i)
          curf=dfdpsi(i) !+dfdpsi_sur
          curw=dwdpsi(i)

         endif

          cur(i,j)=r0*curp+curf/r0+curw*r0**3

         tokp=tokp+cur(i,j)*sqk

            il=numlin(i,j,nr,nt)

          right(il)=cur(i,j)*sqk

 200     continue

C          write(6,*) 'rightp:tokp=',tokp

         do 450 i=1,iplas

         cur(i,1)=cur(i,nt1)
         cur(i,nt)=cur(i,2)


 450     continue
              cnor=cur_mu/tokp

          tokp=tokp/amu0

      if(ngav.eq.0 ) then
      
         !tokp=tok
          tokp=cur_mu/amu0
         do il=1,neqpla
             right(il)=right(il)*cnor 
         enddo
         do i=1,iplas
         do j=1,nt
            cur(i,j)=cur(i,j)*cnor
         enddo
         enddo

      endif

          if(kpr.eq.1) then
           write(*,*) 'cnor=',cnor
          endif
                          endif



         return
         end



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine rigbon

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       real*8 psib(ntp)

       do 20 j=2,nt1

        !psib(j)=psi(iplas,j)
        psib(j)=psip

!test psi=r*2
!        psib(j)=z(iplas,j)  !**2
!test psi=r*2

 20    continue

      !open(3,file='boncon.wr')
	!read(3,*) (psib(j),j=2,nt1)
	!close(3)

        psib(1)=psib(nt1)
        psib(nt)=psib(2)
                                        !
                                       !!
                      !               !!!
          i=iplas-1 !!!!!!!!!!!!!!!!!!!!!
                      !               !!!
                                       !!
                                        !
         do 100 j=2,nt1                

       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

           il=numlin(i,j,nr,nt)

         right(il)=right(il)-(a7*psib(j-1)+a8*psib(j)+a9*psib(j+1))

 100     continue

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine qst_bq

         include 'double.inc'
         include 'dim.inc'
         parameter(nshp=ntp+1)
         include 'compol.inc'

           common/efites/ fcefit,rc_efit,iefit
          dimension xs(nshp),ys(nshp),fun(nshp),dp(5)

           sqrt(xx)=dsqrt(xx)

          nsh=1
          xs(nsh)=r(1,1)
          ys(nsh)=z(1,1)
         fun(nsh)=psin(1,1)

        do 100 j=2,nt1

          nsh=nsh+1
          xs(nsh)=r(2,j)
          ys(nsh)=z(2,j)
         fun(nsh)=psin(2,j)

 100    continue

          call deriv5(xs,ys,fun,nsh,5,dp)

          Drr=dp(3)
          Drz=dp(4)
          Dzz=dp(5)

          tg2a=2.d0*drz/(drr-dzz)
          cos2a=1.d0/sqrt(1.d0+tg2a**2)
          sin2a=cos2a*tg2a

          Dxx=0.5d0*(Drr+Dzz)+0.5d0*cos2a*(Drr-Dzz)+sin2a*Drz
          Dyy=0.5d0*(Drr+Dzz)+0.5d0*cos2a*(Dzz-Drr)-sin2a*Drz

C          write(6,*) 'drr,dzz,drz',drr,dzz,drz
C          write(6,*) 'dxx,dyy',dxx,dyy

          bfcen=qcen*dsqrt(dxx*dyy)*(psim-psip)

          fcen=bfcen*rm
           fcen=fcefit

          dpsi=(psia(1)-psia(2))
          ps14=1.d0-0.25d0*dpsi

          ffp=tabf(ps14)

          f(1)=sqrt(fcen**2-ffp*cnor*(psim-psip)*dpsi)

          do 200 i=2,iplas

            if(i.ne.iplas) then

           pspl=0.5d0*(psia(i)+psia(i+1))*(psim-psip)
           psmn=0.5d0*(psia(i)+psia(i-1))*(psim-psip)

            else

           pspl=0.5d0*psia(i)*(psim-psip)
           psmn=0.5d0*psia(i-1)*(psim-psip)

            endif

           psn=psia(i)

          !ffp=funf(psn,alf0p,alf1p,alf2p,bet0f,bet1f,bet2f)
           ffp=dfdpsi(i)

           f(i)=sqrt(f(i-1)**2+2.d0*ffp*(pspl-psmn))

 200    continue

           Fvac=f(iplas)

          !open(1,file='q.pr')

	    flucfm=0.d0

          do 300 i=1,iplas

           flucfi=0.d0

          do 310 j=2,nt1

            if(i.ne.iplas) then

          r1=r(i,j)
          r2=r(i+1,j)
          r3=r(i+1,j+1)
          r4=r(i,j+1)
          r0=(r1+r2+r3+r4)*0.25d0
          flucfi=flucfi+s(i,j)*f(i)/r0

            else

          r0=r(i,j)
          flucfi=flucfi+(sq3(i-1,j-1)+sq2(i-1,j))*f(i)/r0

            endif

 310    continue

            if(i.ne.iplas) then

           q2pi=-flucfi/((psia(i+1)-psia(i))*(psim-psip))
           flucfm=flucfm+flucfi

          !write(1,*) 0.5d0*(psia(i)+psia(i+1)),0.5d0*q(i)/pi,i
            else

           q2pi=-2.d0*flucfi/((psia(i)-psia(i-1))*(psim-psip))

          !write(1,*) psia(i),0.5d0*q(i)/pi,i
            endif

          !q(i)=0.5d0*q2pi/pi
           q(i)=q2pi

 300    continue

          !close(1)

           return
           end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine psib_pla(pspl_av)

         include 'double.inc'
         include 'dim.inc'
         include 'parcur.inc'
         include 'compol.inc'

         common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                  a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

         common/com_bgr/ binadg(ntp,ntp),dgdn(ntp)
         dimension pspl_b(ntp)
 
          i=iplas        !!!!!!!!!!!!!!!!!!!!!!1

         do 10 j=2,nt1

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)

       g1=psi(i-1,j-1)
       g2=psi(i-1,j)
       g3=psi(i-1,j+1)

        sqk=sq2(i-1,j)+sq3(i-1,j-1)
        dltk=(dlt(i,j-1)+dlt(i,j))*0.5d0
        dgdnl=-(a1*g1+a2*g2+a3*g3) + cur(i,j)*sqk !
        dgdn(j)=dgdnl/dltk

 10      continue

        dgdn(1)=dgdn(nt1)
        dgdn(nt)=dgdn(2)

       do l=2,nt1

        psb=0.d0

       do jb=2,nt1

        psb=psb+binadg(jb,l)*(dgdn(jb)+dgdn(jb+1))*0.5d0

       enddo

        pspl_b(l)=psb

       enddo

        pspl_b(1)=pspl_b(nt1)
        pspl_b(nt)=pspl_b(2)

        pspl_av = avr_bnd(pspl_b)

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine psloop_e(rlop,zlop,psilop,nlop)

         include 'double.inc'
         include 'parrc1.inc'
         !include 'comrec.inc'
         common /comind/ ni,nj,ni1,nj1,ni2,nj2,nbnd,nkin,nkout
         common /comrz/ x(nip),y(njp),dx(nip),dy(njp),
     +                  dxi(nip),dyj(njp),x12(nip)

        common /compot/ u(nip,njp),ue(nip,njp),un(nip,njp),
     +                   ui(nip,njp),g(nip,njp),
     +                   ux0,ux1,ux2,up,um,xm,ym,
     +                  xx0,yx0,xx1,yx1,xx2,yx2,imax,jmax,
     +                  ix1,jx1,ix2,jx2,
     +                  xx10,yx10,xx20,yx20,xm0,ym0,
     +                  psi_bon
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

          ! psilop(i)=blin(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4 )
           psilop(i)=blin_(r0,z0,r1,r2,z1,z2,u1,u2,u3,u4 )

         enddo

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine psib_ext(psex_av)

         include 'double.inc'
         include 'dim.inc'
         !include 'parcur.inc'
         include 'compol.inc'

         dimension psex_b(ntp),rbon(ntp),zbon(ntp)

          i=iplas        !!!!!!!!!!!!!!!!!!!!!!1

         do j=1,nt
          rbon(j)=r(i,j)
          zbon(j)=z(i,j)
         enddo

         call psloop_e(rbon,zbon,psex_b,nt)

        psex_b(1)=psex_b(nt1)
        psex_b(nt)=psex_b(2)

        psex_av = avr_bnd(psex_b)

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine field_c

         include 'double.inc'
         include 'dim.inc'
         include 'compol.inc'

         common /com_mf/ bpol(nrp,ntp),btor(nrp,ntp),
     &                   br(nrp,ntp),bz(nrp,ntp),
     &                   rc(nrp,ntp),zc(nrp,ntp)

         common /com_trap/ trap(nrp)

         dimension Btot(nrp,ntp)

!magnetic field at cells

         do i=1,iplas-1
          do j=2,nt-1

           bp_ij=(psi(i+1,j)-psi(i,j))/st(i,j)
           bp_ij1=(psi(i+1,j+1)-psi(i,j+1))/st(i,j+1)

         if(i.ne.1) then
           bp_2= (
     &           bp_ij**2*( vol1(i,j)/sin1(i,j)+vol2(i,j)/sin2(i,j) )+
     &           bp_ij1**2*( vol3(i,j)/sin3(i,j)+vol4(i,j)/sin4(i,j) )
     &           )/vol(i,j)
         else
           bp_2= (
     &           bp_ij**2*(                     vol2(i,j)/sin2(i,j) )+
     &           bp_ij1**2*( vol3(i,j)/sin3(i,j)                     )
     &           )/vol(i,j)
         endif
           bpol_ij=dsqrt(bp_2)
           btor_ij=f(i)*s(i,j)/vol(i,j)

           bpol(i,j)=bpol_ij
           btor(i,j)=btor_ij
           btot_ij=dsqrt(bp_2+btor_ij**2)

          r1=r(i,j)
          r2=r(i+1,j)
          r3=r(i+1,j+1)
          r4=r(i,j+1)
          r0=(r1+r2+r3+r4)*0.25d0

          r12=(r1+r2)*0.5d0
          r23=(r3+r2)*0.5d0
          r34=(r3+r4)*0.5d0
          r14=(r1+r4)*0.5d0

          z1=z(i,j)
          z2=z(i+1,j)
          z3=z(i+1,j+1)
          z4=z(i,j+1)
          z0=(z1+z2+z3+z4)*0.25d0
        
          z12=(z1+z2)*0.5d0
          z23=(z3+z2)*0.5d0
          z34=(z3+z4)*0.5d0
          z14=(z1+z4)*0.5d0

          dr=r34-r12 
          dz=z34-z12
          drz= dsqrt(dr**2+dz**2)
          dr=dr/drz
          dz=dz/drz

          br(i,j)=bpol_ij*dr
          bz(i,j)=bpol_ij*dz

          rc(i,j)=r0
          zc(i,j)=z0

          enddo
         enddo

         do i=1,iplas-1
           bpol(i,1)=bpol(i,nt-1)
           btor(i,1)=btor(i,nt-1)
           br(i,1)=br(i,nt-1)
           bz(i,1)=bz(i,nt-1)
           rc(i,1)=rc(i,nt-1)
           zc(i,1)=zc(i,nt-1)
         enddo

        do i=1,iplas-1
           Bmax=0.d0
           Svol=0.d0         
          do j=2,nt-1
           B_ij=dsqrt(bpol(i,j)**2+btor(i,j)**2)
           Bmax=dmax1(Bmax,B_ij)
           Btot(i,j)=B_ij
           Svol=Svol+vol(i,j)
          enddo

           avr_v=0.d0
          do j=2,nt-1
           Bnor=Btot(i,j)/Bmax
           avr_v=avr_v+
     &          (b0ax/Btot(i,j))**2
     &         *( 1.d0-dsqrt(1.d0-Bnor)*(1.d0+.5d0*Bnor) )*vol(i,j)
          enddo
          
           trap(i)=avr_v/Svol
           
        enddo

        write(fname,'(a,a)') path(1:kname),'fields.wr'
        open(1,file=fname)
        !open(1,file='fields.wr',form='formatted')
         write(1,*) iplas-1,nt-1
         write(1,*) ((rc(i,j),i=1,iplas-1),j=1,nt-1)
         write(1,*) ((zc(i,j),i=1,iplas-1),j=1,nt-1)
         write(1,*) ((br(i,j),i=1,iplas-1),j=1,nt-1)
         write(1,*) ((bz(i,j),i=1,iplas-1),j=1,nt-1)
         write(1,*) ((btor(i,j),i=1,iplas-1),j=1,nt-1)
         write(1,*) ((bpol(i,j),i=1,iplas-1),j=1,nt-1)
         write(1,*) (trap(i),i=1,iplas-1)
        close(1)

           return
           end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


