!!!this file contains the following routines:
!!!
!!!!!!!! metric
!!!!!!!! geom
!!!!!!!! funsq
!!!!!!!! numlin
!!!!!!!! matcof
!!!!!!!! matpla
!!!!!!!! matrix
!!!!!!!! numlin
!!!!!!!! prog1d
!!!!!!!! procof
!!!!!!!! extrpc
!!!!!!!!!!!!!!!!

         subroutine f_metric
        !-----------------

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         sqrt(xx)=dsqrt(xx)
                            !write(*,*) 'metric:enter'

         do 10 i=1,Nr1
         do 10 j=1,Nt

          DLr(i,j)=sqrt( (r(i+1,j)-r(i,j))**2+(z(i+1,j)-z(i,j))**2 )
          St(i,j)=DLr(i,j)*(r(i+1,j)+r(i,j))*0.5d0

 10      continue
                            !write(*,*) 'metric:10   '

         do 20 i=2,Nr
         do 20 j=1,Nt1

          DLt(i,j)=sqrt( (r(i,j+1)-r(i,j))**2+(z(i,j+1)-z(i,j))**2 )
          Sr(i,j)=DLt(i,j)*(r(i,j+1)+r(i,j))*0.5d0

 20      continue
                            !write(*,*) 'metric:20   '

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

          vol1(i,j)=funsq(r1,r2,r4,r4,z1,z2,z4,z4)*(r1+r2+r4)/6.d0
          vol2(i,j)=funsq(r1,r2,r3,r3,z1,z2,z3,z3)*(r1+r2+r3)/6.d0
          vol3(i,j)=funsq(r2,r3,r4,r4,z2,z3,z4,z4)*(r2+r3+r4)/6.d0
          vol4(i,j)=funsq(r1,r3,r4,r4,z1,z3,z4,z4)*(r1+r3+r4)/6.d0

         !vol1(i,j)=funsq(r1,r2,r4,r4,z1,z2,z4,z4)*r0*0.5d0
         !vol2(i,j)=funsq(r1,r2,r3,r3,z1,z2,z3,z3)*r0*0.5d0
         !vol3(i,j)=funsq(r2,r3,r4,r4,z2,z3,z4,z4)*r0*0.5d0
         !vol4(i,j)=funsq(r1,r3,r4,r4,z1,z3,z4,z4)*r0*0.5d0


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
          sin2(i,j)=1.d0-cos2(i,j)**2

          cos3(i,j)=(dr34*dr23+dz34*dz23)/sqrt(dl34*dl23)
          sin3(i,j)=1.d0-cos3(i,j)**2

           if(i.eq.1) go to 35

          cos4(i,j)=(dr34*dr14+dz34*dz14)/sqrt(dl34*dl14)
          sin4(i,j)=1.d0-cos4(i,j)**2

          cos1(i,j)=(dr12*dr14+dz12*dz14)/sqrt(dl12*dl14)
          sin1(i,j)=1.d0-cos1(i,j)**2


                    !cos1(i,j)=0.d0
                    !sin1(i,j)=1.d0
                    !
                    !cos2(i,j)=0.d0
                    !sin2(i,j)=1.d0
                    !
                    !cos3(i,j)=0.d0
                    !sin3(i,j)=1.d0
                    !
                    !cos4(i,j)=0.d0
                    !sin4(i,j)=1.d0

 35      continue

 30      continue


          return
          end



         subroutine f_geom

!!!!cylindr geometry

           include'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include'dim.inc'
           include'compol.inc'
           include 'compol_add.inc'

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

          vol1(i,j)=funsq(r1,r2,r4,r4,z1,z2,z4,z4)*(rm+rm+rm)/6.
          vol2(i,j)=funsq(r1,r2,r3,r3,z1,z2,z3,z3)*(rm+rm+rm)/6.
          vol3(i,j)=funsq(r2,r3,r4,r4,z2,z3,z4,z4)*(rm+rm+rm)/6.
          vol4(i,j)=funsq(r1,r3,r4,r4,z1,z3,z4,z4)*(rm+rm+rm)/6.

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_matcof

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)


         do 10 i=1,nr1
         do 10 j=1,nt1

          s12=St(i,j)
          s14=Sr(i,j)
          s34=St(i,j+1)
          s23=Sr(i+1,j)

          if(i.ne.1) then

       a12(i,j)=( (-1.d0/s12+cos1(i,j)/s14)*vol1(i,j)/sin1(i,j) +
     +            (-1.d0/s12-cos2(i,j)/s23)*vol2(i,j)/sin2(i,j) )/s12

       a23(i,j)=( (-1.d0/s23-cos2(i,j)/s12)*vol2(i,j)/sin2(i,j) +
     +            (-1.d0/s23+cos3(i,j)/s34)*vol3(i,j)/sin3(i,j) )/s23

       a34(i,j)=( (-1.d0/s34+cos3(i,j)/s23)*vol3(i,j)/sin3(i,j) +
     +            (-1.d0/s34-cos4(i,j)/s14)*vol4(i,j)/sin4(i,j) )/s34

       a14(i,j)=( (-1.d0/s14+cos1(i,j)/s12)*vol1(i,j)/sin1(i,j) +
     +            (-1.d0/s14-cos4(i,j)/s34)*vol4(i,j)/sin4(i,j) )/s14

       a13(i,j)=  (cos2(i,j)/(s12*s23))*vol2(i,j)/sin2(i,j) +
     +            (cos4(i,j)/(s34*s14))*vol4(i,j)/sin4(i,j)

       a24(i,j)=  (-cos1(i,j)/(s12*s14))*vol1(i,j)/sin1(i,j) +
     +            (-cos3(i,j)/(s34*s23))*vol3(i,j)/sin3(i,j)

          else

       a12(i,j)=( (-1.d0/s12-cos2(i,j)/s23)*vol2(i,j)/sin2(i,j) )/s12

       a23(i,j)=( (-1.d0/s23-cos2(i,j)/s12)*vol2(i,j)/sin2(i,j) +
     +            (-1.d0/s23+cos3(i,j)/s34)*vol3(i,j)/sin3(i,j) )/s23

       a34(i,j)=( (-1.d0/s34+cos3(i,j)/s23)*vol3(i,j)/sin3(i,j) )/s34


       a13(i,j)=  (cos2(i,j)/(s12*s23))*vol2(i,j)/sin2(i,j)

       a24(i,j)=  (-cos3(i,j)/(s34*s23))*vol3(i,j)/sin3(i,j)

          endif

 10      continue

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_matrix

!!!!!!    IL  -- number of equation (matrix line)
!!!!!!    IM  -- element number in one-dimensional array A (a(im))
!!!!!! IA(IL) -- number of first nonzero element in line IL
!!!!!! JA(IM) -- number of matrix column for element a(im)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)




!!!!!!!!!!! equation for central point

           il=1
           im=1

           ia(1)=1
           ja(1)=1
           a(1)=0.d0

         do 20 j=2,nt1

          im=im+1
       ja(im)=numlin(2,j,nr,nt)
       a(im)=a12(1,j)+a24(1,j)+a34(1,j-1)+a13(1,j-1)
       a(1)=a(1)-a(im)

 20      continue

!!!!!!!!!!! the main loop

         do 100 i=2,nr1

           !!!!!
               !
           j=2 !
               !
           !!!!!

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)
       a4=a23(i-1,j-1)+a14(i,j-1)
       a6=a14(i,j)+a23(i-1,j)
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

       a5=-(a1+a2+a3+a4+a6+a7+a8+a9)

           il=il+1
           ia(il)=im+1

          if(i.ne.2) then


!2!cof. to (i-1,j)

           im=im+1
       a(im)=a2
       ja(im)=numlin(i-1,j,nr,nt)

!3!cof. to (i-1,j+1)

           im=im+1
       a(im)=a3
       ja(im)=numlin(i-1,j+1,nr,nt)

!1!cof. to (i-1,j-1)

           im=im+1
       a(im)=a1
       ja(im)=numlin(i-1,j-1,nr,nt)

!5!cof. to (i,j)

           im=im+1
       a(im)=a5
       ja(im)=numlin(i,j,nr,nt)

!6!cof. to (i,j+1)

           im=im+1
       a(im)=a6
       ja(im)=numlin(i,j+1,nr,nt)

!4!cof. to (i,j-1)

           im=im+1
       a(im)=a4
       ja(im)=numlin(i,j-1,nr,nt)

         if(i.eq.nr1) go to 71

!8!cof. to (i+1,j)

           im=im+1
       a(im)=a8
       ja(im)=numlin(i+1,j,nr,nt)

!9!cof. to (i+1,j+1)

           im=im+1
       a(im)=a9
       ja(im)=numlin(i+1,j+1,nr,nt)

!7!cof. to (i+1,j-1)

           im=im+1
       a(im)=a7
       ja(im)=numlin(i+1,j-1,nr,nt)

 71      continue


          else


!1!cof. to central point

           im=im+1
       a(im)=a1+a2+a3
       ja(im)=numlin(1,j,nr,nt)

!5!cof. to (2,j)

           im=im+1
       a(im)=a5
       ja(im)=numlin(2,j,nr,nt)

!6!cof. to (2,j+1)

           im=im+1
       a(im)=a6
       ja(im)=numlin(2,j+1,nr,nt)

!4!cof. to (2,j-1)

           im=im+1
       a(im)=a4
       ja(im)=numlin(2,j-1,nr,nt)

!8!cof. to (3,j)

           im=im+1
       a(im)=a8
       ja(im)=numlin(3,j,nr,nt)

!9!cof. to (3,j+1)

           im=im+1
       a(im)=a9
       ja(im)=numlin(3,j+1,nr,nt)

!7!cof. to (3,j-1)

           im=im+1
       a(im)=a7
       ja(im)=numlin(3,j-1,nr,nt)


          endif


         do 110 j=3,nt2

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)
       a4=a23(i-1,j-1)+a14(i,j-1)
       a6=a14(i,j)+a23(i-1,j)
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

       a5=-(a1+a2+a3+a4+a6+a7+a8+a9)

           il=il+1
           ia(il)=im+1

          if(i.ne.2) then


!1!cof. to (i-1,j-1)

           im=im+1
       a(im)=a1
       ja(im)=numlin(i-1,j-1,nr,nt)

!2!cof. to (i-1,j)

           im=im+1
       a(im)=a2
       ja(im)=numlin(i-1,j,nr,nt)

!3!cof. to (i-1,j+1)

           im=im+1
       a(im)=a3
       ja(im)=numlin(i-1,j+1,nr,nt)

!3!cof. to (i,j-1)

           im=im+1
       a(im)=a4
       ja(im)=numlin(i,j-1,nr,nt)

!5!cof. to (i,j)

           im=im+1
       a(im)=a5
       ja(im)=numlin(i,j,nr,nt)

!6!cof. to (i,j+1)

           im=im+1
       a(im)=a6
       ja(im)=numlin(i,j+1,nr,nt)

         if(i.eq.nr1) go to 7

!7!cof. to (i+1,j-1)

           im=im+1
       a(im)=a7
       ja(im)=numlin(i+1,j-1,nr,nt)

!8!cof. to (i+1,j)

           im=im+1
       a(im)=a8
       ja(im)=numlin(i+1,j,nr,nt)

!9!cof. to (i+1,j+1)

           im=im+1
       a(im)=a9
       ja(im)=numlin(i+1,j+1,nr,nt)

 7       continue


          else


!1!cof. to central point

           im=im+1
       a(im)=a1+a2+a3
       ja(im)=numlin(1,j,nr,nt)

!2!cof. to (2,j-1)

           im=im+1
       a(im)=a4
       ja(im)=numlin(2,j-1,nr,nt)

!3!cof. to (2,j)

           im=im+1
       a(im)=a5
       ja(im)=numlin(2,j,nr,nt)

!4!cof. to (2,j+1)

           im=im+1
       a(im)=a6
       ja(im)=numlin(2,j+1,nr,nt)

!5!cof. to (3,j-1)

           im=im+1
       a(im)=a7
       ja(im)=numlin(3,j-1,nr,nt)

!6!cof. to (3,j)

           im=im+1
       a(im)=a8
       ja(im)=numlin(3,j,nr,nt)

!7!cof. to (3,j+1)

           im=im+1
       a(im)=a9
       ja(im)=numlin(3,j+1,nr,nt)


          endif

 110     continue

           !!!!!!!
                 !
           j=nt1 !
                 !
           !!!!!!!

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)
       a4=a23(i-1,j-1)+a14(i,j-1)
       a6=a14(i,j)+a23(i-1,j)
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

       a5=-(a1+a2+a3+a4+a6+a7+a8+a9)

           il=il+1
           ia(il)=im+1

          if(i.ne.2) then


!3!cof. to (i-1,j+1)

           im=im+1
       a(im)=a3
       ja(im)=numlin(i-1,j+1,nr,nt)

!1!cof. to (i-1,j-1)

           im=im+1
       a(im)=a1
       ja(im)=numlin(i-1,j-1,nr,nt)

!2!cof. to (i-1,j)

           im=im+1
       a(im)=a2
       ja(im)=numlin(i-1,j,nr,nt)

!6!cof. to (i,j+1)

           im=im+1
       a(im)=a6
       ja(im)=numlin(i,j+1,nr,nt)

!4!cof. to (i,j-1)

           im=im+1
       a(im)=a4
       ja(im)=numlin(i,j-1,nr,nt)

!5!cof. to (i,j)

           im=im+1
       a(im)=a5
       ja(im)=numlin(i,j,nr,nt)

         if(i.eq.nr1) go to 72

!9!cof. to (i+1,j+1)

           im=im+1
       a(im)=a9
       ja(im)=numlin(i+1,j+1,nr,nt)

!7!cof. to (i+1,j-1)

           im=im+1
       a(im)=a7
       ja(im)=numlin(i+1,j-1,nr,nt)

!8!cof. to (i+1,j)

           im=im+1
       a(im)=a8
       ja(im)=numlin(i+1,j,nr,nt)

 72      continue


          else


!1!cof. to central point

           im=im+1
       a(im)=a1+a2+a3
       ja(im)=numlin(1,j,nr,nt)

!6!cof. to (2,j+1)

           im=im+1
       a(im)=a6
       ja(im)=numlin(2,j+1,nr,nt)

!4!cof. to (2,j-1)

           im=im+1
       a(im)=a4
       ja(im)=numlin(2,j-1,nr,nt)

!5!cof. to (2,j)

           im=im+1
       a(im)=a5
       ja(im)=numlin(2,j,nr,nt)

!9!cof. to (3,j+1)

           im=im+1
       a(im)=a9
       ja(im)=numlin(3,j+1,nr,nt)

!7!cof. to (3,j-1)

           im=im+1
       a(im)=a7
       ja(im)=numlin(3,j-1,nr,nt)

!8!cof. to (3,j)

           im=im+1
       a(im)=a8
       ja(im)=numlin(3,j,nr,nt)


          endif

 100     continue
                      !write(6,*) 'matrix:il=',il

          neq=il

       !write(6,*) 'number of equations:',neq,neqp
       !write(6,*) 'im=',im
       !write(6,*) 'lp=',lp

           il=il+1
           ia(il)=im+1





c            open(1,file='matr')
c
c           do 111 kl=1,neq
c
c             ic1=ia(kl)
c             ic2=ia(kl+1)-1
c
c             write(1,*) 'il=',kl
c             write(1,*) (ja(km),km=ic1,ic2)
c             write(1,*) (a(km),km=ic1,ic2)
c
c111     continue


         if( (itin/nitdel*nitdel+nitbeg) .EQ. itin ) then

             do km=1,im 

                aop0(km)=a(km)

             enddo

	!write(6,*) 'matrix:aop0:itin,nitdel',itin,nitdel

          elseif(itin.gt.nitbeg) then

             do km=1,im 

                daop(km)=a(km)-aop0(km)

             enddo
	!write(6,*) 'matrix:daop:itin',itin

		endif
		
		
		    

       return
       end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_procof(icq,cur_mu)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
           include 'compol_add.inc'

           tok_mu=tok*amu0

         call bongri
         call f_psib_ext(psex_av)
         call f_psib_pla(pspl_av)
         call cof_bon(cps_bon,bps_bon,dps_bon)
          psi_bon=psi_eav+pspl_av
         psibon=psi_bon
         psi_eav=psex_av




         if(ngav.eq.1) then
	     call f_procof_I(icq)
         elseif(ngav.eq.2) then
	     call f_procof_fl(icq)
         elseif(ngav.eq.-10 .AND. erru.lt.5.d-3) then
 	call promat_j(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psipla,itin,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu )
         elseif(ngav.eq.-1 .AND. erru.lt.5.d-3) then
 	call promat(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psipla,itin,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu )
         elseif(ngav.eq.-2 .AND. erru.lt.5.d-2) then
 	call promat(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psipla,itin,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu )
         elseif(ngav.eq.-3 .AND. erru.lt.5.d-3) then
 	call promat_I(iplas,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psipla,itin,
     *            kstep,fvac,psi_eav,pspl_av,psibon0,tok_mu )
         endif

         return
	   end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_procof_fl(icq)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       common/compsf/ psf,sqtor

       dimension amn(nrp),a0(nrp),apl(nrp),capp(nrp)
       dimension avrc(nrp),delsc(nrp),avrk(nrp),delsk(nrp),delv(nrp)
       dimension bmn(nrp),b0(nrp),bpl(nrp),wrk1(nrp),wrk2(nrp)
       dimension psf(nrp),rhs(nrp),sqtor(nrp)

          sqrt(xx)=dsqrt(xx)

	   psimx=psip+psipla

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
          zavrk=zavrk+sqk/r0
          zapro=zapro+aex(i,j)
 45      continue

           avrk(i)=zavrk/zdelsk
           delsk(i)=zdelsk
           delv(i)=zdelv
          !rhs(i-1)=-zapro
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

       Qmn=-q(i-1)/(avrc(i-1)*delsc(i-1))
       Qpl=-q(i) / (avrc(i)*delsc(i))
       Qk=0.5d0*(Qmn+Qpl)

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

         rhs(i)=-dpdpsi(i+1)*delv(i+1)+rhs(i)

 100     continue

         rhs(1)=rhs(1)+amn(2)*psimx
         rhs(npro)=rhs(npro)+apl(iplas-1)*psip

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           call prog1d(npro,psf(2),bpl,bmn,b0,rhs,wrk1,wrk2)

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         psf(1)=psimx
         psf(iplas)=psip

         do 300 i=1,iplas1

         f(i)=-q(i)*(psf(i+1)-psf(i))/(delsc(i)*avrc(i))

 300     continue



         do 400 i=2,iplas1

       Qmn=-q(i-1)/(avrc(i-1)*delsc(i-1))
       Qpl=-q(i)/(avrc(i)*delsc(i))
       Qk=0.5d0*(qmn+qpl)

       dfdpsi(i)=Qk*(f(i)-f(i-1))

 400     continue

!!! equation in central node

       ac0=0.d0
       skcen=0.d0

         do 20 j=2,nt1

       acj=a12(1,j)+a24(1,j)+a34(1,j-1)+a13(1,j-1)
       ac0=ac0-acj

       skcen=skcen+sq1(1,j)+sq4(1,j-1)

 20      continue

       aexcen=aex(1,2)

      !dfdpsi(1)=((ac0*(psimx-psf(2))-aexcen)/skcen-rm*dpdpsi(1))*rm
       dfdpsi(1)=((ac0*(psimx-psf(2))       )/skcen-rm*dpdpsi(1))*rm

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
     *   +delv(i)*dpdpsi(i)
     *         ) /cappa

        f(i)=sqrt( f(i-1)**2+dfdpsi(i)*(psip-psf(i-1)) )

        q(i)=-f(i)*avrk(i)/DpsiDs
        write(*,*) 'q(iplas)=',q(i)
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

       f(i)=(samn*(psip-psf(i-1))+delv(i)*dpdpsi(i)-Q14*f(i-1))
     *     /( cappa/Qn-Q14 )

!      dfdpsi(i)=Q14*(f(i)-f(i-1))/delsk(i)
       dfdpsi(i)=(q14/avr14)*(f(i)-f(i-1))/delsk(i)

               
                psiai=psia(i)-0.25d0*(psia(i)- psia(i-1))
               
        call extrp2( psiai,dfdpsi(i),
     *               psia(i-3),psia(i-2),psia(i-1),
     *                       dfdpsi(i-3) ,dfdpsi(i-2) ,dfdpsi(i-1)  )
       
                f(i)=dsqrt(f(i-1)**2+dfdpsi(i)*(psip- psf(i-1)))
               
            
               
               
            ! open(1,file='ddps1.pr')
            ! do i=1,iplas
            !   write(1,*) dfdpsi(i),f(i),i
	      ! enddo
            !   write(1,*) 'q(iplas)=',q(iplas)
            ! close(1)
 
            ! open(1,file='ddp.wr')
            !  write(1,*) iplas
            !  write(1,*) (q(i),i=1,iplas)
            !  write(1,*) (f(i),i=1,iplas)
            !  write(1,*) (dfdpsi(i),i=1,iplas)
            !  write(1,*) (dpdpsi(i),i=1,iplas)
            !close(1)
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine f_procof_i(icq)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       common/compsf/ psf(nrp),sqtor(nrp)

       dimension amn(nrp),a0(nrp),apl(nrp),Qcapp(nrp)
       dimension avrc(nrp),delsc(nrp),avrk(nrp),delsk(nrp),delv(nrp)
       dimension bmn(nrp),b0(nrp),bpl(nrp),wrk1(nrp),wrk2(nrp)
       dimension rhs(nrp)
       dimension dfdpsn(nrp),vw(nrp)

          sqrt(xx)=dsqrt(xx)

            do i=1,iplas
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
          zavrk=zavrk+sqk/r0

 45      continue

           avrk(i)=zavrk/zdelsk
           delsk(i)=zdelsk
           delv(i)=zdelv
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

 736     continue

       if(itin.eq.1) then

          errdf=0.d0

         do i=2,iplas1

	   !plcu=apl(i)*(psf(i+1)-psf(i))
	   !write(6,*) 'tok',plcu,i
	   !pause ' pause'

          zff=amn(i)*psf(i-1)+a0(i)*psf(i)+apl(i)*psf(i+1)

          zdfdps=( zff-dpdpsi(i)*delv(i) )/(avrk(i)*delsk(i))
	   !znvz=zff-dpdpsi(i)*delv(i)-dfdpsn(i)*avrk(i)*delsk(i)
          dfdpsi(i)=zdfdps
	    deldf=dabs(zdfdps-dfdpsn(i))
	 !write(6,*) 'deldf',deldf,dfdpsn(i),i
	 !write(6,*) 'nev.L(psi)',znvz,zff
	 !pause ' pause'
          errdf=dmax1(deldf,errdf)
          dfdpsn(i)=dfdpsi(i)

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
          errdf=0.d0

           f2n1=(tok*Qcapp(iplas-1))**2
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

          dfdpsi(i)=( zff-dpdpsi(i)*delv(i) )/(avrk(i)*delsk(i))
	    deldf=dabs(dfdpsi(i)-dfdpsn(i))
	!write(6,*) 'deldf',deldf,errdf,i
	!pause ' pause'
          errdf=dmax1(deldf,errdf)
          dfdpsn(i)=dfdpsi(i)

         enddo
	!write(6,*) 'errdf',errdf
	!pause ' pause'

               if(errdf.gt.1.d-7) go to 736   !!!------***
	   
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


       dfdpsi(1)=(  ac0*(psf(1)-psf(2))/skcen-rm*dpdpsi(1))*rm


         return
	   end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
