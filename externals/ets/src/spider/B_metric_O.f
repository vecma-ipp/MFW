!!!this file contains the following routines:
!!!
!!!!!!!! metric
!!!!!!!! geom
!!!!!!!! funsq
!!!!!!!! numlin
!!!!!!!! matcof
!!!!!!!! matpla
!!!!!!!! numlin
!!!!!!!! prog1d
!!!!!!!! procof 
!!!!!!!! extrpc
!!!!!!!!!!!!!!!!

ch4astra         !subroutine metric
         subroutine metrix

        !-----------------

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         sqrt(xx)=dsqrt(xx)

                            !write(6,*) 'metric:enter'

         do 10 j=1,Nt
         do 10 i=1,iplas1

          DLr(i,j)=sqrt( (r(i+1,j)-r(i,j))**2+(z(i+1,j)-z(i,j))**2 )
          St(i,j)=DLr(i,j)*(r(i+1,j)+r(i,j))*0.5d0

 10      continue
                            !write(6,*) 'metric:10   '

         do 20 j=1,Nt1
         do 20 i=2,iplas

          DLt(i,j)=sqrt( (r(i,j+1)-r(i,j))**2+(z(i,j+1)-z(i,j))**2 )
          Sr(i,j)=DLt(i,j)*(r(i,j+1)+r(i,j))*0.5d0

 20      continue

                            !write(6,*) 'metric:20   '


         do 35 j=1,Nt1
         do 30 i=1,iplas1

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

          dl12=DLr(i,j)
          dl14=DLt(i,j)
          dl23=DLt(i+1,j)
          dl34=DLr(i,j+1)

          sq_1=funsq(r1,r12,r0,r14,z1,z12,z0,z14)
          sq_2=funsq(r2,r23,r0,r12,z2,z23,z0,z12)
          sq_3=funsq(r3,r34,r0,r23,z3,z34,z0,z23)
          sq_4=funsq(r4,r14,r0,r34,z4,z14,z0,z34)

          !S(i,j)=funsq(r1,r2,r3,r4,z1,z2,z3,z4)

           !sq1(i,j)=0.25d0*S(i,j)
           !sq2(i,j)=0.25d0*S(i,j)
           !sq3(i,j)=0.25d0*S(i,j)
           !sq4(i,j)=0.25d0*S(i,j)

          !vol1(i,j)=funsq(r1,r2,r4,r4,z1,z2,z4,z4)*(r1+r2+r4)/6.d0
          !vol2(i,j)=funsq(r1,r2,r3,r3,z1,z2,z3,z3)*(r1+r2+r3)/6.d0
          !vol3(i,j)=funsq(r2,r3,r4,r4,z2,z3,z4,z4)*(r2+r3+r4)/6.d0
          !vol4(i,j)=funsq(r1,r3,r4,r4,z1,z3,z4,z4)*(r1+r3+r4)/6.d0

         !vol1(i,j)=funsq(r1,r2,r4,r4,z1,z2,z4,z4)*r0*0.5d0
         !vol2(i,j)=funsq(r1,r2,r3,r3,z1,z2,z3,z3)*r0*0.5d0
         !vol3(i,j)=funsq(r2,r3,r4,r4,z2,z3,z4,z4)*r0*0.5d0
         !vol4(i,j)=funsq(r1,r3,r4,r4,z1,z3,z4,z4)*r0*0.5d0

          !vol(i,j)=(vol1(i,j)+vol2(i,j)+vol3(i,j)+vol4(i,j))

           !vol1(i,j)=0.25d0*vol(i,j)
           !vol2(i,j)=0.25d0*vol(i,j)
           !vol3(i,j)=0.25d0*vol(i,j)
           !vol4(i,j)=0.25d0*vol(i,j)

          dr12=r2-r1
          dz12=z2-z1
          !dl12=    (dr12**2+dz12**2)

          dr14=r4-r1
          dz14=z4-z1
          !dl14=    (dr14**2+dz14**2)

          dr23=r3-r2
          dz23=z3-z2
          !dl23=    (dr23**2+dz23**2)

          dr34=r3-r4
          dz34=z3-z4
          !dl34=    (dr34**2+dz34**2)


          DLm2=dl12*dl23
          cos_2=(dr12*dr23+dz12*dz23)/DLm2
          sin_2=1.d0-cos_2**2
          vol_2=DLm2*sqrt(sin_2)*(r1+r2+r4)/12.d0

          DLm3=dl34*dl23
          cos_3=(dr34*dr23+dz34*dz23)/DLm3
          sin_3=1.d0-cos_3**2
          vol_3=DLm3*sqrt(sin_3)*(r2+r3+r4)/12.d0

           if(i.ne.1) then

          DLm4=dl34*dl14
          cos_4=(dr34*dr14+dz34*dz14)/DLm4
          sin_4=1.d0-cos_4**2
          vol_4=DLm4*sqrt(sin_4)*(r1+r3+r4)/12.d0

          DLm1=dl12*dl14
          cos_1=(dr12*dr14+dz12*dz14)/DLm1
          sin_1=1.d0-cos_1**2
          vol_1=DLm1*sqrt(sin_1)*(r1+r2+r4)/12.d0

           else
                    !cos1(i,j)=0.d0
                    cos_1=0.d0       ! DPC
                    sin_1=1.d0
                    vol_1=0.d0
                    !
                    !cos2(i,j)=0.d0
                    !sin2(i,j)=1.d0
                    !
                    !cos3(i,j)=0.d0
                    !sin3(i,j)=1.d0
                    !
                    !cos4(i,j)=0.d0
                    cos_4=0.d0       ! DPC
                    sin_4=1.d0
                    vol_4=0.d0
           endif

          cos2(i,j)=cos_2
          sin2(i,j)=sin_2
          vol2(i,j)=vol_2

          cos3(i,j)=cos_3
          sin3(i,j)=sin_3
          vol3(i,j)=vol_3

          cos4(i,j)=cos_4
          sin4(i,j)=sin_4
          vol4(i,j)=vol_4

          cos1(i,j)=cos_1
          sin1(i,j)=sin_1
          vol1(i,j)=vol_1

          vol(i,j)=vol_1+vol_2+vol_3+vol_4

          sq1(i,j)=sq_1
          sq2(i,j)=sq_2
          sq3(i,j)=sq_3
          sq4(i,j)=sq_4

          S(i,j)=sq_1+sq_2+sq_3+sq_4

 30      continue
 35      continue

          return
          end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine geom_b
!!!!cylindr geometry

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         sqrt(xx)=dsqrt(xx)
                            !write(6,*) 'metric:enter'

         do 10 i=1,Nr1
         do 10 j=1,Nt

          DLr(i,j)=sqrt( (r(i+1,j)-r(i,j))**2+(z(i+1,j)-z(i,j))**2 )
          St(i,j)=DLr(i,j)*rm

 10      continue

                            !write(6,*) 'metric:10   '

         do 20 i=2,Nr
         do 20 j=1,Nt1

          DLt(i,j)=sqrt( (r(i,j+1)-r(i,j))**2+(z(i,j+1)-z(i,j))**2 )
          Sr(i,j)=DLt(i,j)*rm

 20      continue

                            !write(6,*) 'metric:20   '

         do 30 i=1,Nr1
         do 35 j=1,Nt1

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

          sq1(i,j)=funsq(r1,r12,r0,r14,z1,z12,z0,z14)
          sq2(i,j)=funsq(r2,r23,r0,r12,z2,z23,z0,z12)
          sq3(i,j)=funsq(r3,r34,r0,r23,z3,z34,z0,z23)
          sq4(i,j)=funsq(r4,r14,r0,r34,z4,z14,z0,z34)

          S(i,j)=funsq(r1,r2,r3,r4,z1,z2,z3,z4)

          vol1(i,j)=funsq(r1,r2,r4,r4,z1,z2,z4,z4)*(rm+rm+rm)/6.d0
          vol2(i,j)=funsq(r1,r2,r3,r3,z1,z2,z3,z3)*(rm+rm+rm)/6.d0
          vol3(i,j)=funsq(r2,r3,r4,r4,z2,z3,z4,z4)*(rm+rm+rm)/6.d0
          vol4(i,j)=funsq(r1,r3,r4,r4,z1,z3,z4,z4)*(rm+rm+rm)/6.d0

          vol(i,j)=(vol1(i,j)+vol2(i,j)+vol3(i,j)+vol4(i,j))

          dr12=r2-r1
          dz12=z2-z1

          dl12=    (dr12**2+dz12**2)

          dr14=r4-r1
          dz14=z4-z1
          dl14=    (dr14**2+dz14**2)

          dr23=r3-r2
          dz23=z3-z2
          dl23=    (dr23**2+dz23**2)

          dr34=r3-r4
          dz34=z3-z4
          dl34=    (dr34**2+dz34**2)


          cos2(i,j)=(dr12*dr23+dz12*dz23)/sqrt(dl12*dl23)
          sin2(i,j)=1.-cos2(i,j)**2

          cos3(i,j)=(dr34*dr23+dz34*dz23)/sqrt(dl34*dl23)
          sin3(i,j)=1.-cos3(i,j)**2

           if(i.eq.1) go to 35

          cos4(i,j)=(dr34*dr14+dz34*dz14)/sqrt(dl34*dl14)
          sin4(i,j)=1.-cos4(i,j)**2

          cos1(i,j)=(dr12*dr14+dz12*dz14)/sqrt(dl12*dl14)
          sin1(i,j)=1.-cos1(i,j)**2



                     cos1(i,j)=0.d0
                     sin1(i,j)=1.d0

                     cos2(i,j)=0.d0
                     sin2(i,j)=1.d0

                     cos3(i,j)=0.d0
                     sin3(i,j)=1.d0

                     cos4(i,j)=0.d0
                     sin4(i,j)=1.d0

 35      continue

 30      continue

          return
          end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!         REAL FUNCTION FUNSQ*8(R1,R2,R3,R4,Z1,Z2,Z3,Z4)
!         include 'double.inc'
!
!         R13=R3-R1
!         R24=R4-R2
!         Z13=Z3-Z1
!         Z24=Z4-Z2
!
!         ZS=R13*Z24-R24*Z13
!
!         IF(ZS.LT.0.) WRITE(6,*) '***ERROR:SQUARE<0',zs
!
!         FUNSQ=ZS*0.5D0
!
!         RETURN
!         END
!

         subroutine matrot

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'
           common/com_rot/ ar1(nrp,ntp),ar2(nrp,ntp),ar3(nrp,ntp),
     +                     ar4(nrp,ntp),ar5(nrp,ntp),ar6(nrp,ntp)
           dimension B_tet(nrp,ntp),B_ro(nrp,ntp),Rot_B(nrp,ntp)
           dimension Z_err(nrp,ntp)

         do i=2,iplas1
         do j=2,nt1

!!!! B_tet(i-1,j-1)

         ar1(i,j)= cos2(i-1,j-1)*vol2(i-1,j-1)/(Sr(i,j-1)*sin2(i-1,j-1))

!!!! B_tet(i-1,j)

         ar2(i,j)=( -cos2(i-1,j)/Sr(i,j)-1.d0/St(i-1,j) )
     &            *vol2(i-1,j)/sin2(i-1,j) +
     &            ( -1.d0/St(i-1,j) )*vol1(i-1,j)/sin1(i-1,j)+
     &            (  cos3(i-1,j-1)/Sr(i,j-1)-1.d0/St(i-1,j) )
     &            *vol3(i-1,j-1)/sin3(i-1,j-1) +
     &            ( -1.d0/St(i-1,j) )*vol4(i-1,j-1)/sin4(i-1,j-1)

!!!! B_tet(i-1,j+1)

         ar3(i,j)=-cos3(i-1,j)*vol3(i-1,j)/(Sr(i,j)*sin3(i-1,j))

!!!! B_tet(i,j-1)

         ar4(i,j)= cos1(i,j-1)*vol1(i,j-1)/(Sr(i,j-1)*sin1(i,j-1))

!!!! B_tet(i,j)

         ar5(i,j)=( -cos1(i,j)/Sr(i,j)+1.d0/St(i,j) )
     &            *vol1(i,j)/sin1(i,j) +
     &            (  1.d0/St(i,j) )*vol2(i,j)/sin2(i,j)+
     &            (  cos4(i,j-1)/Sr(i,j-1)+1.d0/St(i,j) )
     &            *vol4(i,j-1)/sin4(i,j-1) +
     &            (  1.d0/St(i,j) )*vol3(i,j-1)/sin3(i,j-1)


!!!! B_tet(i,j+1)

         ar6(i,j)=-cos4(i,j)*vol4(i,j)/(Sr(i,j)*sin4(i,j))


         enddo
         enddo


         do i=2,iplas1
         do j=1,nt1

          B_ro(i,j)=(psi(i,j+1)-psi(i,j))/Sr(i,j)

         enddo
         enddo

         do i=1,iplas1
         do j=1,nt

          B_tet(i,j)=(psi(i+1,j)-psi(i,j))/St(i,j)

         enddo
         enddo

         do i=2,iplas1
         do j=2,nt1

          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)

          Bt1=B_tet(i-1,j-1)
          Bt2=B_tet(i-1,j)
          Bt3=B_tet(i-1,j+1)
          Bt4=B_tet(i,j-1)
          Bt5=B_tet(i,j)
          Bt6=B_tet(i,j+1)

          Rot_B(i,j)=-( ar1(i,j)*Bt1+ar2(i,j)*Bt2+ar3(i,j)*Bt3+
     &                 ar4(i,j)*Bt4+ar5(i,j)*Bt5+ar6(i,j)*Bt6 )/sqk

          Z_err(i,j)=Rot_B(i,j)-cur(i,j)

         enddo
         enddo

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine matcof

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)


         do 10 j=1,nt1
         do 10 i=1,iplas1

          s12=St(i,j)
          s14=Sr(i,j)
          s34=St(i,j+1)
          s23=Sr(i+1,j)

          if(i.ne.1) then

       a12(i,j)=( (-1.d0/s12 + cos1(i,j)/s14)*vol1(i,j)/sin1(i,j) +
     +            (-1.d0/s12 - cos2(i,j)/s23)*vol2(i,j)/sin2(i,j) )/s12

       a23(i,j)=( (-1.d0/s23 - cos2(i,j)/s12)*vol2(i,j)/sin2(i,j) +
     +            (-1.d0/s23 + cos3(i,j)/s34)*vol3(i,j)/sin3(i,j) )/s23

       a34(i,j)=( (-1.d0/s34 + cos3(i,j)/s23)*vol3(i,j)/sin3(i,j) +
     +            (-1.d0/s34 - cos4(i,j)/s14)*vol4(i,j)/sin4(i,j) )/s34

       a14(i,j)=( (-1.d0/s14 + cos1(i,j)/s12)*vol1(i,j)/sin1(i,j) +
     +            (-1.d0/s14 - cos4(i,j)/s34)*vol4(i,j)/sin4(i,j) )/s14

       a13(i,j)=  (cos2(i,j)/(s12*s23))*vol2(i,j)/sin2(i,j) +
     +            (cos4(i,j)/(s34*s14))*vol4(i,j)/sin4(i,j)

       a24(i,j)=  (-cos1(i,j)/(s12*s14))*vol1(i,j)/sin1(i,j) +
     +            (-cos3(i,j)/(s34*s23))*vol3(i,j)/sin3(i,j)

          else

       a12(i,j)=( (-1.d0/s12 - cos2(i,j)/s23)*vol2(i,j)/sin2(i,j) )/s12

       a23(i,j)=( (-1.d0/s23 - cos2(i,j)/s12)*vol2(i,j)/sin2(i,j) +
     +            (-1.d0/s23 + cos3(i,j)/s34)*vol3(i,j)/sin3(i,j) )/s23

       a34(i,j)=( (-1.d0/s34 + cos3(i,j)/s23)*vol3(i,j)/sin3(i,j) )/s34

       a13(i,j)=  (cos2(i,j)/(s12*s23))*vol2(i,j)/sin2(i,j)

       a24(i,j)=  (-cos3(i,j)/(s34*s23))*vol3(i,j)/sin3(i,j)

          endif

 10      continue

        return
        end


         subroutine procof(icq,cur_mu)

         include 'double.inc'
         include 'dim.inc'
         include 'compol.inc'

         if(ngav.eq.0) cur_mu=tok*amu0
           call psib_pla(pspl_av)

         if(ngav.eq.1) then
	     call procof_I(icq)
         elseif(ngav.eq.2) then
	     call procof_fl(icq)
         elseif(ngav.eq.-10 .AND. erru.lt.5.d-3) then
 	call promat_j(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psim,iter,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu )
         elseif(ngav.eq.-1 .AND. erru.lt.5.d-3) then
 	call promat(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psim,iter,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu )
         elseif(ngav.eq.-2 .AND. erru.lt.5.d-3) then
 	call promat(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psim,iter,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu )
         elseif(ngav.eq.-3 .AND. erru.lt.5.d-3) then
          cur_mu=tok*amu0
 	call promat_I(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psim,iter,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu )
         endif

         return
	   end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine procof_fl(icq)

!!!flux conserving

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       common/compsf/ psf(nrp),sqtor(nrp)

       dimension amn(nrp),a0(nrp),apl(nrp),capp(nrp)
       dimension avrc(nrp),delsc(nrp),avrk(nrp),delsk(nrp),delv(nrp)
       dimension bmn(nrp),b0(nrp),bpl(nrp),wrk1(nrp),wrk2(nrp)
       dimension rhs(nrp),delv3(nrp)
       dimension dfdpsn(nrp),delf(nrp)
       dimension qs(nrp),qsn(nrp),psfn(nrp),dpsdas(nrp)

          sqrt(xx)=dsqrt(xx)

          ! write(6,*)' procof_fl:enter'
          ! write(6,*)'psiax,,psip',psiax,psip

            do i=1,iplas

              dfdpsn(i)=dfdpsi(i)
              if(iter.eq.1) dfdpsn(i)=0.d0
              qs(i)=q(i)
	        psfn(i)=psia(i)
	        dpsdas(i)=dpsda(i)

            enddo

 964      continue

            do i=1,iplas

              qsn(i)=qs(i)

            enddo

         do 30 i=1,iplas1

          zdelsc=0.d0
          zavrc=0.d0

         do 35 j=2,Nt1

          r1=r(i,j)
          r2=r(i+1,j)
          r3=r(i+1,j+1)
          r4=r(i,j+1)
          r0=(r1+r2+r3+r4)*0.25d0

          zdelsc=zdelsc+s(i,j)
          zavrc=zavrc+s(i,j)/r0

 35      continue

           avrc(i)=zavrc/zdelsc
           delsc(i)=zdelsc

 30      continue

           sqtor(1)=0.d0

         do 33 i=2,iplas
           sqtor(i)=sqtor(i-1)+delsc(i-1)
 33      continue

         do 40 i=2,Iplas

          zdelv=0.d0
          zdelv3=0.d0
          zdelsk=0.d0
          zavrk=0.d0
          zapro=0.d0

         do 45 j=2,Nt1

         if(i.ne.iplas) then
          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
          r0=r(i,j)
         else
          sqk=sq2(i-1,j)+sq3(i-1,j-1)
          r0=(3.d0*r(i,j)+r(i-1,j))*0.25d0
         endif

          zdelsk=zdelsk+sqk
          zdelv=zdelv +sqk*r0
          zdelv3=zdelv3 +sqk*r0**3
          zavrk=zavrk+sqk/r0
          zapro=zapro

 45      continue

           avrk(i)=zavrk/zdelsk
           delsk(i)=zdelsk
           delv(i)=zdelv
           delv3(i)=zdelv3
           rhs(i-1)=0.d0

 40      continue

         do 10 i=2,iplas1

          samn=0.d0
          sa0 =0.d0
          sapl=0.d0

         do 110 j=2,nt1

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)
       a4=a23(i-1,j-1)+a14(i,j-1)
       a6=a14(i,j)+a23(i-1,j)
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)
       a5=-(a1+a2+a3+a4+a6+a7+a8+a9)

       zamn=a1+a2+a3
       za0 =a4+a5+a6
       zapl=a7+a8+a9

       samn=samn+zamn
       sa0 =sa0 +za0
       sapl=sapl+zapl

 110     continue

       Qmn=-qs(i-1)/(avrc(i-1)*delsc(i-1))
       Qpl=-qs(i) / (avrc(i)*delsc(i))
       Qk=(Qmn*dpsdas(i-1)+Qpl*dpsdas(i))/(dpsdas(i-1)+dpsdas(i))

       afmn=Qk*Qmn*avrk(i)*delsk(i)
       afpl=Qk*Qpl*avrk(i)*delsk(i)

       af0=-(afmn+afpl)

       amn(i)=samn - afmn
       a0(i) =sa0  - af0
       apl(i)=sapl - afpl

       capp(i)=sapl*delsc(i)

         if(i.eq.iplas-1) then

       cappl=sapl*delsc(i)
       capmn=samn*delsc(i-1)

         endif

 10      continue

         npro=iplas-2

         do 100 i=1,npro

          if(i.ne.1) then
         bmn(i)=amn(i+1)
          else
         bmn(i)=0.d0
          endif

         b0(i)=-a0(i+1)

          if(i.ne.npro) then
         bpl(i)=apl(i+1)
          else
         bpl(i)=0.d0
          endif

         rhs(i)=rhs(i)-dpdpsi(i+1)*delv(i+1)-dwdpsi(i+1)*delv3(i+1)

	!write(6,*) i
	!write(6,*) bmn(i),b0(i),bpl(i)

 100     continue

         rhs(1)=rhs(1)+amn(2)*psiax
         rhs(npro)=rhs(npro)+apl(iplas-1)*psip

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           call prog1d(npro,psf(2),bpl,bmn,b0,rhs,wrk1,wrk2)

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         psf(1)=psiax
         psf(iplas)=psip

         psfn(1)=1.d0
         psfn(iplas)=0.d0

             do i=2,iplas1

               psfn(i)=(psf(i)-psip)/(psiax-psip)

             enddo

         do 300 i=1,iplas1

         f(i)=-qs(i)*(psf(i+1)-psf(i))/(delsc(i)*avrc(i))

 300     continue

         do 400 i=2,iplas1

       Qmn=-qs(i-1)/(avrc(i-1)*delsc(i-1))
       Qpl=-qs(i)/(avrc(i)*delsc(i))
       Qk=(Qmn*dpsdas(i-1)+Qpl*dpsdas(i))/(dpsdas(i-1)+dpsdas(i))

       dfdpsi(i)=Qk*(f(i)-f(i-1))

 400     continue

!!!!!!!!!!///////////////////////////////////////////

!!! equation in central node

       ac0=0.d0
       skcen=0.d0

         do 20 j=2,nt1

       acj=a12(1,j)+a24(1,j)+a34(1,j-1)+a13(1,j-1)
       ac0=ac0-acj

       skcen=skcen+sq1(1,j)+sq4(1,j-1)

 20      continue

       dfdpsi(1)=
     * ( (ac0*(psiax-psf(2)))/skcen-rm*dpdpsi(1)-dwdpsi(1)*rm**3 )*rm

!!! equation on bound node!
!!!!!!!!!!!!!!!!!!!!!!!!!!!

         i=iplas

          samn=0.d0
          sa0 =0.d0

         do 50 j=2,nt1

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)

       zamn=a1+a2+a3
       za0 =-zamn
       samn=samn+zamn
       sa0 =sa0 +za0

 50      continue

       !cappa=cappl+delsk(i)*(cappl-capmn)/delsk(i-1)

       call extrpc( sqtor(i),capp(i),
     *              sqtor(i),sqtor(i-1),sqtor(i-2),sqtor(i-3),
     *                       capp(i-1) ,capp(i-2) ,capp(i-3)  )

!      call extrpc( psia(i),capp(i),
!    *              psia(i),psia(i-1),psia(i-2),psia(i-3),
!    *                       capp(i-1) ,capp(i-2) ,capp(i-3)  )

        cappa=capp(i)

                              !!!!!!!!!!!!!!!!!!!!!!!!

                       if(icq.eq.0) then

        DpsiDs=(
     *   samn*(psip-psf(i-1))+avrk(i)*delsk(i)*dfdpsi(i)
     *   +delv(i)*dpdpsi(i)+delv3(i)*dwdpsi(i)
     *         ) /cappa

        f(i)=sqrt( f(i-1)**2+dfdpsi(i)*(psip-psf(i-1)) )

        q(i)=-f(i)*avrk(i)/DpsiDs

        write(6,*) 'q(iplas)=',q(i)

!          pause ' '

                       endif

       !q(i)=q(i-1)+delsk(i)*(q(i-1)-q(i-2))/delsk(i-1)

                              !!!!!!!!!!!!!!!!!!!!!!!!

        Qn=-q(i)/avrk(i)
        Qmn=-q(i-1)/avrc(i-1)

       !Q14= 0.5d0*(Qn+Qmn)

        q14=-0.5d0*(q(i)+q(i-1))

        avr14=avrk(i)

!      f(i)=(samn*(psip-psf(i-1))+delv(i)*dpdpsi(i)-Q14*avr14*f(i-1))
!    *     /( cappa/Qn-Q14*avr14 )


       f(i)=
     * ( samn*(psip-psf(i-1))
     *    +delv(i)*dpdpsi(i)+delv3(i)*dwdpsi(i)-Q14*f(i-1) )
     *     /( cappa/Qn-Q14 )

!      dfdpsi(i)=Q14*(f(i)-f(i-1))/delsk(i)
       dfdpsi(i)=(q14/avr14)*(f(i)-f(i-1))/delsk(i)

                psiai=psia(i)-0.25d0*(psia(i)- psia(i-1))

        call extrp2( psiai,dfdpsi(i),
     *               psia(i-3),psia(i-2),psia(i-1),
     *                       dfdpsi(i-3) ,dfdpsi(i-2) ,dfdpsi(i-1)  )
      

                f(i)=dsqrt(f(i-1)**2+dfdpsi(i)*(psip- psf(i-1)))

!!!!!!!!!!!!!>>>>>>>>

        !dfdpsi(i)=0.d0

!!!!!!!!!!!!!>>>>>>>>

            if(itin.le.2) then

		  vrh=1.00d0

	      else

            vrh=1.00d0

	      endif

           do i=1,iplas

             dfdpsi(i)=vrh*dfdpsi(i)+(1.d0-vrh)*dfdpsn(i)

           enddo

           do i=1,iplas1

             delf(i)=dfdpsi(i)-dfdpsn(i)

           enddo
             delf(iplas)=0.d0

            !if(itin.eq.1) wgt=1.0d0 

              ! do ki=1,2
           do i=2,iplas1

           wgt=1.0d0

            dmon=(delf(i+1)-delf(i))/(delf(i)-delf(i-1))

           !if(dmon.lt.-0.5d0.and.dmon.gt.-2.0d0) wgt=0.5d0
           if(dmon.lt.0.d0) wgt=0.5d0

           delf(i)=wgt*delf(i)+(1.d0-wgt)*
     * (delf(i-1)*(psf(i+1)-psf(i))+delf(i+1)*(psf(i)-psf(i-1)))/
     *          (psf(i+1)-psf(i-1))

           enddo
              ! enddo


           do i=2,iplas1

             dfdpsi(i)=dfdpsn(i)+delf(i)

           enddo


                       !if(icq.ne.0) then


 !       if(ngav.eq.2) then
             !open(1,file='ddps1.pr')
             !do 567 i=1,iplas
 !567           !write(1,*) dfdpsi(i),f(i),i
              ! write(1,*) 'q(iplas)=',q(iplas)
            ! close(1)
            !  stop
 !       endif


       return
       end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine procof_I(icq)

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       common/compsf/ psf(nrp),sqtor(nrp)

       dimension amn(nrp),a0(nrp),apl(nrp),Qcapp(nrp)
       dimension avrc(nrp),delsc(nrp),avrk(nrp),delsk(nrp),delv(nrp)
       dimension bmn(nrp),b0(nrp),bpl(nrp),wrk1(nrp),wrk2(nrp)
       dimension rhs(nrp),delv3(nrp)
       dimension dfdpsn(nrp),vw(nrp)
       dimension dfdpsw(nrp),delf(nrp)

          sqrt(xx)=dsqrt(xx)
           ! write(6,*) 'procof_I:enter'
            do i=1,iplas

              dfdpsw(i)=dfdpsi(i)
              dfdpsn(i)=dfdpsi(i)
	        psf(i)=psia(i)*(psim-psip)+psip 

            enddo

c!!!!!!!   dS(i+1/2) and <1/R>(i+1/2)   !!!!!!!!!!!

         do 30 i=1,iplas1

          zdelsc=0.d0
          zavrc=0.d0

         do 35 j=2,Nt1

          r1=r(i,j)
          r2=r(i+1,j)
          r3=r(i+1,j+1)
          r4=r(i,j+1)

          r0=(r1+r2+r3+r4)*0.25d0

          zdelsc=zdelsc+s(i,j)
          zavrc=zavrc+s(i,j)/r0

 35      continue

           avrc(i)=zavrc/zdelsc
           delsc(i)=zdelsc

 30      continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           sqtor(1)=0.d0

         do 33 i=2,iplas

           sqtor(i)=sqtor(i-1)+delsc(i-1)

 33      continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c!!!!!!!   dS(i), dV(i) and <1/R>(i)   !!!!!!!!!!!

         do 40 i=2,Iplas

          zdelv=0.d0
          zdelv3=0.d0
          zdelsk=0.d0
          zavrk=0.d0

         do 45 j=2,Nt1

         if(i.ne.iplas) then

          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
          r0=r(i,j)

         else

          sqk=sq2(i-1,j)+sq3(i-1,j-1)
          r0=(3.d0*r(i,j)+r(i-1,j))*0.25d0

         endif

          zdelsk=zdelsk+sqk
          zdelv=zdelv +sqk*r0
          zdelv3=zdelv3 +sqk*r0**3
          zavrk=zavrk+sqk/r0

 45      continue

           avrk(i)=zavrk/zdelsk
           delsk(i)=zdelsk
           delv(i)=zdelv
           delv3(i)=zdelv3
           rhs(i-1)=0.d0  !right hand side for averaged equation

 40      continue

         do 10 i=2,iplas1

          samn=0.d0
          sa0 =0.d0
          sapl=0.d0

         do 110 j=2,nt1

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)
       a4=a23(i-1,j-1)+a14(i,j-1)
       a6=a14(i,j)+a23(i-1,j)
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)
       a5=-(a1+a2+a3+a4+a6+a7+a8+a9)

       zamn=a1+a2+a3
       za0 =a4+a5+a6
       zapl=a7+a8+a9

       samn=samn+zamn
       sa0 =sa0 +za0
       sapl=sapl+zapl

 110     continue

       Qmn=-q(i-1)/(avrc(i-1)*delsc(i-1))
       Qpl=-q(i) / (avrc(i)*delsc(i))
       Qk=(Qmn*dpsda(i-1)+Qpl*dpsda(i))/(dpsda(i-1)+dpsda(i))

       afmn=Qk*Qmn*avrk(i)*delsk(i)
       afpl=Qk*Qpl*avrk(i)*delsk(i)

       af0=-(afmn+afpl)

       amn(i)=samn !- afmn
       a0(i) =sa0  !- af0
       apl(i)=sapl !- afpl

       Qcapp(i)=Qpl/sapl

         if(i.eq.iplas-1) then

       Qcappl=Qpl/sapl
       Qcapmn=Qmn/samn

         endif

 10      continue

       if(itin.eq.1) then

          errdf=0.d0

         do i=2,iplas1

	   !plcu=apl(i)*(psf(i+1)-psf(i))
	   !write(6,*) 'tok',plcu,i
	   !pause ' pause'

          zff=amn(i)*psf(i-1)+a0(i)*psf(i)+apl(i)*psf(i+1)

          zdfdps=( zff-dpdpsi(i)*delv(i)-dwdpsi(i)*delv3(i) )
     *          /(avrk(i)*delsk(i))

	   !znvz=zff-dpdpsi(i)*delv(i)-dfdpsn(i)*avrk(i)*delsk(i)

          dfdpsi(i)=zdfdps
	    deldf=dabs(zdfdps-dfdpsn(i))

	 !write(6,*) 'deldf',deldf,dfdpsn(i),i
	 !write(6,*) 'nev.L(psi)',znvz,zff
	 !pause ' pause'

          errdf=dmax1(deldf,errdf)
          dfdpsn(i)=dfdpsi(i)
          dfdpsi(i)=0.d0  !!!!!!!!!!!!!<<<<<<<<<<<<

         enddo

	 !write(6,*) 'errdf',errdf
	!pause ' pause'
	 endif
	  !! return

       !!!!!!!!!!
!        i=iplas
!       !!!!!!!!!!   
!    
!          zff=tok-apl(i-1)*(psf(i)-psf(i-1))
!
!          dfdpsi(i)=(zff-dpdpsi(i)*delv(i))/(avrk(i)*delsk(i))
!	    deldf=dabs(dfdpsi(i)-dfdpsn(i))
!	!write(6,*) 'deldf*',deldf,i
!	!write(6,*) '*',dfdpsi(i),dfdpsn(i)
!	!pause ' pause'
!
!
!
!       Qcappa=Qcappl+delsk(i)*(Qcappl-Qcapmn)/delsk(i-1)
!
!       call extrpc( sqtor(i),Qcapp(i),
!     *              sqtor(i),sqtor(i-1), sqtor(i-2), sqtor(i-3),
!     *                       Qcapp(i-1) ,Qcapp(i-2) ,Qcapp(i-3)  )
!
!      call extrpc( psia(i),Qcapp(i),
!    *              psia(i),psia(i-1),  psia(i-2),  psia(i-3),
!    *                      Qcapp(i-1) ,Qcapp(i-2) ,Qcapp(i-3)  )
!
!           !ff2n=f(i-1)**2+dfdpsn(i)*(psf(i)-psf(i-1))
!
!           !f2n=(tok*Qcappa)**2
!           f2n=(tok*Qcapp(i))**2
!
!	!write(6,*) 'fnold fn',ff2n,f2n
!
!           f(i)=dsqrt(f2n)
!	     f(i-1)=dsqrt( f2n-dfdpsi(i)*(psf(i)-psf(i-1)) )
!
            !!amu0=0.4d0*pi

 736     continue

          errdf=0.d0

           f2n1=(amu0*tok*Qcapp(iplas-1))**2
	     f(iplas-1)=dsqrt(f2n1)

           do i=iplas-1,2,-1

	   f(i-1)=dsqrt( f(i)**2-dfdpsi(i)*(psf(i+1)-psf(i-1)) )

           enddo

         psf(iplas)=0.d0

           do i=iplas1,1,-1

         dftor=f(i)*delsc(i)*avrc(i)
	   delpsi=dftor/q(i)
         psf(i)=psf(i+1)+delpsi 

           enddo

         do i=2,iplas1

	   !plcu=apl(i)*(psf(i+1)-psf(i))
	   !write(6,*) 'tok',plcu,i
	   !pause ' pause'

          zff=amn(i)*psf(i-1)+a0(i)*psf(i)+apl(i)*psf(i+1)

          dfdpsi(i)=( zff-dpdpsi(i)*delv(i)-dwdpsi(i)*delv3(i) )
     *             /(avrk(i)*delsk(i))
	    deldf=dabs(dfdpsi(i)-dfdpsn(i))

	!write(6,*) 'deldf',deldf,errdf,i
	!pause ' pause'

          errdf=dmax1(deldf,errdf)
          dfdpsn(i)=dfdpsi(i)

         enddo

	!write(6,*) 'errdf',errdf
	!pause ' pause'

         if(errdf.gt.1.d-7) go to 736

	   i=iplas      !!!<<<<<<<<<<<<<

        call extrp2( psia(i),dfdpsi(i),
     *               psia(i-3),psia(i-2),psia(i-1),
     *                       dfdpsi(i-3) ,dfdpsi(i-2) ,dfdpsi(i-1)  )

!!!!!!!!!!///////////////////////////////////////////

!!! equation in central node

       ac0=0.d0
       skcen=0.d0

         do 20 j=2,nt1

       acj=a12(1,j)+a24(1,j)+a34(1,j-1)+a13(1,j-1)
       ac0=ac0-acj

       skcen=skcen+sq1(1,j)+sq4(1,j-1)

 20      continue

       dfdpsi(1)=( ac0*(psf(1)-psf(2))/skcen
     *             -rm*dpdpsi(1)-dwdpsi(1)*rm**3 )*rm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           do i=1,iplas1

             delf(i)=dfdpsi(i)-dfdpsw(i)

           enddo
             delf(iplas)=0.d0

            !if(itin.eq.1) wgt=1.0d0 

              ! do ki=1,2
           do i=2,iplas1

           wgt=1.0d0

            dmon=(delf(i+1)-delf(i))/(delf(i)-delf(i-1))

           !if(dmon.lt.-0.5d0.and.dmon.gt.-2.0d0) wgt=0.5d0
           if(dmon.lt.0.d0) wgt=0.5d0

           delf(i)=wgt*delf(i)+(1.d0-wgt)*
     * (delf(i-1)*(psf(i+1)-psf(i))+delf(i+1)*(psf(i)-psf(i-1)))/
     *          (psf(i+1)-psf(i-1))

           enddo
              ! enddo


           do i=2,iplas1

             dfdpsi(i)=dfdpsw(i)+delf(i)

           enddo




        ! write(6,*) 'procof:exit'

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!









         subroutine metcof_

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'


       dimension ax(nrp),gr_a(nrp)
       dimension delsc(nrp),delv(nrp),dvda(nrp)



          sqrt(xx)=dsqrt(xx)

        do i=1,iplas        
         ax(i)=(i-1.d0)/(iplas-1.d0)
        enddo


           i=1

           zdelv=0.d0
           sss=0.d0
           sss0=0.d0
           
           dadr=0.d0
           dadz=0.d0

           u0=ax(1) 

          do j=2,nt1

           u1=ax(2) 
           u2=ax(2)

           r1=r(2,j) 
           r2=r(2,j+1)
		  
           z1=z(2,j) 
           z2=z(2,j+1) 

           dadr=dadr+0.5d0*(u1+u2)*(z2-z1)
           dadz=dadz-0.5d0*(u1+u2)*(r2-r1)

           sss=sss+funsq(r1,r2,0.d0,0.d0,z1,z2,0.d0,0.d0)
           sqk=sq1(i,j)+sq4(i,j-1)
           zdelv=zdelv +sqk*r(1,2)
		 	
          enddo

           dadr=dadr/sss
           dadz=dadz/sss

            gr_a(1)=dadr**2+dadz**2
            delv(1)=zdelv


        do i=1,iplas

          zdelv=0.d0
          zavgr=0.d0

        do j=2,nt1

           r1=r(i-1,j) 
           r2=r(i,j-1) 
           r3=r(i+1,j) 
           r4=r(i,j+1) 
           	      
           z1=z(i-1,j) 
           z2=z(i,j-1) 
           z3=z(i+1,j) 
           z4=z(i,j+1)
		  
           u1=ax(i-1) 
           u2=ax(i) 
           u3=ax(i+1) 
           u4=ax(i) 
           	      
           if(i.eq.iplas) then

           r3=r(i,j) 
           z3=z(i,j) 
           u3=ax(i) 

           endif
		 		  
           sss=funsq(r1,r2,r3,r4,z1,z2,z3,z4)
           
           dadr= 0.5d0*((u1+u2)*(z2-z1)+(u2+u3)*(z3-z2)+
     *                   (u3+u4)*(z4-z3)+(u4+u1)*(z1-z4))/sss

           dadz=-0.5d0*((u1+u2)*(r2-r1)+(u2+u3)*(r3-r2)+
     *                   (u3+u4)*(r4-r3)+(u4+u1)*(r1-r4))/sss

           gr2=dadr**2+dadz**2

         if(i.ne.iplas) then
          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
          r0=r(i,j)
         else
          sqk=sq2(i-1,j)+sq3(i-1,j-1)
          r0=(3.d0*r(i,j)+r(i-1,j))*0.25d0
         endif

          zdelv=zdelv +sqk*r0
          zavgr=zavgr+gr2*sqk*r0

        enddo
            gr_a(i)=zavgr/zdelv
            delv(i)=zdelv
        enddo

         do i=1,iplas-1
          dvda(i)=(delv(i+1)-delv(i))/(ax(i+1)-ax(i))
        enddo
    
       return
       end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine qunew(qs,psfn)

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         dimension qs(1),psfn(1)

	       qs(1)=q(1)
	       qs(iplas)=q(iplas)

	    do i=2,iplas1 

	       psx=0.5d0*(psfn(i)+psfn(i+1))

            do is=1,iplas1

             q0=q(is)
             qpl=q(is+1)

	       ps0=0.5d0*(psia(is)+psia(is+1))

	        if(is.ne.iplas1) then

	       pspl=0.5d0*(psia(is+1)+psia(is+2))

	        else

	       pspl=1.d0

	        endif

              if(psx.le.ps0 .AND. psx.ge.pspl) then

	       qs(i)=(q0*(pspl-psx)+qpl*(psx-ps0))/(pspl-ps0)

             go to 10

              endif

		  enddo

 10          continue

          enddo		  	   		    

           return
	     end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!**********************************************************************
!     PROGONKA - USUAL
!
!     A(J)*U(J+1)-C(J)*U(J)+B(J)*U(J-1)=-F(J) , J=1,M
!
!      B(1)=0 , A(M)=0
!
!**********************************************************************

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
