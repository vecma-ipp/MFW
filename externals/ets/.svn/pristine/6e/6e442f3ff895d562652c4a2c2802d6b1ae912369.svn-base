!! file contains following routines:
!!
!! funp
!! funf
!! f_rightg
!! f_rightp
!! rigext
!! toksur

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_rightg

!! riht-hand side for problem L(g)=J

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'
	common
     *  /c_kpr/kpr
       common/com_flag/kastr
!       save cur_mu
       real*8 cur_mu
        
          tokp=0.d0

         do 1 il=1,neqp

          right(il)=0.d0

 1       continue

         do 2 i=1,Nr
         do 2 j=1,Nt

          cur(i,j)=0.d0

 2       continue

            !call aprod

                if(kstep.eq.0 .OR. (ngav.eq.0 .AnD. kastr.eq.0)) then
                       !if(ngav.eq.0) then
ch4astra                       if(kstep.eq.0) then
                      !!!!!!!!!!!!!!!!!!!!
!! central point

          curp=tabp(1.d0)
          curf=tabf(1.d0)

          dpdpsi(1)=curp
          dfdpsi(1)=curf

          curcen=rm*curp+curf/rm

              sqcen=0.d0

         do 10 j=2,nt1

          sqcen=sqcen+sq1(1,j)+sq4(1,j)

 10      continue

          tokp=curcen*sqcen
          tokff=curf*sqcen/rm
          tokpp=curp*sqcen*rm

            il=numlin(1,1,nr,nt)

          right(il)=curcen*sqcen

!! regular points

         do 20 i=2,Iplas
         do 25 j=2,Nt1

         if(i.ne.iplas) then
          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
          r0=r(i,j)
          psn=psin(i,j)
         else
          sqk=sq2(i-1,j)+sq3(i-1,j-1)
          r0=(3.d0*r(i,j)+r(i-1,j))*0.25d0
          psn=(3.d0*psin(i,j)+psin(i-1,j))*0.25d0
	     !psn=0.d0
         endif

          curp=tabp(psn)
          curf=tabf(psn)

          cur(i,j)=r0*curp+curf/r0

         tokp=tokp+cur(i,j)*sqk
         tokff=tokff+curf*sqk/r0
         tokpp=tokpp+curp*sqk*r0

            il=numlin(i,j,nr,nt)
          right(il)=cur(i,j)*sqk

 25      continue

          dpdpsi(i)=curp
          dfdpsi(i)=curf

 20      continue

              cnor=amu0*tok/tokp
              cur_mu=amu0*tok

          if(kpr.eq.1) then
           write(*,*) 'cnor,tok,tokp'
           write(*,*) cnor,tok,tokp/amu0
          endif


         do 30 j=1,Nt

         cur(1,j)=curcen*cnor

 30      continue

         do 40 i=2,iplas
         do 40 j=2,Nt1

         cur(i,j)=cur(i,j)*cnor

 40      continue

         do 45 i=1,nr

         cur(i,1)=cur(i,nt1)
         cur(i,nt)=cur(i,2)

 45      continue

         do 50 il=1,neq

          right(il)=right(il)*cnor   !-zpro(il)

 50      continue

         do 55 i=1,iplas

          dpdpsi(i)=dpdpsi(i)*cnor
          dfdpsi(i)=dfdpsi(i)*cnor

 55      continue

          tokp=tok

                        else!!!!!!!!!!!!!!!!!!!
                       !!!!!!!!!!!!!!!!!!!!!!!!
                       
            if(kastr.eq.1) then
             call pres_d_psi
            endif

ch4astra
            call f_procof(1,cur_mu)
            !if(ngav.ne.0) call f_procof(1,cur_mu)

!! central point

          curcen=rm*dpdpsi(1)+dfdpsi(1)/rm

              sqcen=0.d0

         do 100 j=2,nt1

          sqcen=sqcen+sq1(1,j)+sq4(1,j)

 100      continue

            tokp=curcen*sqcen

            il=numlin(1,1,nr,nt)

          right(il)=curcen*sqcen

         do 300 j=1,Nt

         cur(1,j)=curcen

 300     continue

!! regular points

         do 200 i=2,Iplas
         do 200 j=2,Nt1

         if(i.ne.iplas) then
          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)
          r0=r(i,j)
         else
          sqk=sq2(i-1,j)+sq3(i-1,j-1)
          r0=(3.d0*r(i,j)+r(i-1,j))*0.25d0
         endif

          curp=dpdpsi(i)
          curf=dfdpsi(i)

          cur(i,j)=r0*curp+curf/r0

         tokp=tokp+cur(i,j)*sqk

            il=numlin(i,j,nr,nt)
          right(il)=cur(i,j)*sqk

 200     continue

          !write(*,*) 'rightp:tokp=',tokp

         !!!!!!!!!!!!!!!!!!!!!!!!!!
         if(ngav.eq.2) call toksur!
         !!!!!!!!!!!!!!!!!!!!!!!!!!

              cnor=cur_mu/tokp


         do 450 i=1,nr

         cur(i,1)=cur(i,nt1)
         cur(i,nt)=cur(i,2)

 450     continue

          tokp=tokp/amu0
          write(*,*) 'rightp:tok tokp ',tok,tokp
          
      if(ngav.eq.0 ) then
      
          tokp=cur_mu/amu0
          
         do i=1,iplas
         do j=1,Nt
         cur(i,j)=cur(i,j)*cnor
         enddo
         enddo
         
         do il=1,neq
          right(il)=right(il)*cnor  !   !-zpro(il)
         enddo
         
      endif

          if(kpr.eq.1) then
           write(*,*) 'cnor cur_mu',cnor,cur_mu
          endif

                          endif

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine f_rightp

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'


       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       real*8 psib(ntp)

          i=nr        !!!!!!!!!!!!!!!!!!!!!!

         do 10 j=2,nt1

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)

       g1=g(i-1,j-1)
       g2=g(i-1,j)
       g3=g(i-1,j+1)

        dltk=(dlt(i,j-1)+dlt(i,j))*0.5d0
        dgdnl=a1*g1+a2*g2+a3*g3
        dgdn(j)=dgdnl/dltk

 10      continue

        dgdn(1)=dgdn(nt1)
        dgdn(nt)=dgdn(2)

       do 20 j=2,nt1

        psb=0.d0

       do 22 jb=2,nt1

        psb=psb+binadg(jb,j)*(dgdn(jb)+dgdn(jb+1))*0.5d0

 22    continue

        psib(j)=psb
        psii(i,j)=-psb

 20    continue

        psib(1)=psib(nt1)
        psib(nt)=psib(2)

        psii(i,1)=-psib(nt1)
        psii(i,nt)=-psib(2)             !
                                       !!
                                      ! !
          i=nr1  !!!!!!!!!!!!!!!!!!!!!  !
                                      ! !
         do 100 j=2,nt1                !!
                                        !
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

           il=numlin(i,j,nr,nt)

         right(il)= (a7*psib(j-1)+a8*psib(j)+a9*psib(j+1))

 100     continue


!         do 200 il=1,neq

!         right(il)=right(il)!-zpro(il)

! 200     continue


        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine rigext

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       real*8 psib(ntp)

         do 1 il=1,neqp

          right(il)=0.d0

 1       continue

       do 20 j=2,nt1

        psib(j)=psie(iplas,j)

 20    continue

        psib(1)=psib(nt1)
        psib(nt)=psib(2)
                                        !
                                       !!
                                      ! !
          i=iplas-1  !!!!!!!!!!!!!!!!!  !
                                      ! !
         do 100 j=2,nt1                !!
                                        !
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

           il=numlin(i,j,nr,nt)

         right(il)=-(a7*psib(j-1)+a8*psib(j)+a9*psib(j+1))

 100     continue

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine f_rigbon

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       real*8 psib(ntp)


       do 20 j=2,nt1

        !psib(j)=psi(iplas,j)
        psib(j)=psip

 20    continue

      !open(3,file='boncon.wr')
	!read(3,*) (psib(j),j=2,nt1)
	!close(3)




        psib(1)=psib(nt1)
        psib(nt)=psib(2)
                                        !
                                       !!
                                      ! !
          i=iplas-1  !!!!!!!!!!!!!!!!!  !
                                      ! !
         do 100 j=2,nt1                !!
                                        !
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

           il=numlin(i,j,nr,nt)

         right(il)=right(il)-(a7*psib(j-1)+a8*psib(j)+a9*psib(j+1))

 100     continue

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine toksur

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

           sqrt(xx)=dsqrt(xx)

              i=iplas
                   
              toksfi=0.d0
                   
        do 10 j=2,nt1

              il=numlin(i,j,nr,nt)

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)
       a4=a23(i-1,j-1)
       a6=a23(i-1,j)

       a5=-(a1+a2+a3+a4+a6)

       ps1=psii(i-1,j-1)
       ps2=psii(i-1,j)
       ps3=psii(i-1,j+1)

       ps4=psii(i,j-1)
       ps5=psii(i,j)
       ps6=psii(i,j+1)

       DpiDni=a1*ps1+a2*ps2+a3*ps3+a4*ps4+a5*ps5+a6*ps6-right(il)

       ps1=psie(i-1,j-1)
       ps2=psie(i-1,j)
       ps3=psie(i-1,j+1)

       ps4=psie(i,j-1)
       ps5=psie(i,j)
       ps6=psie(i,j+1)

       DpeDni=a1*ps1+a2*ps2+a3*ps3+a4*ps4+a5*ps5+a6*ps6

       a4=a14(i,j-1)
       a6=a14(i,j)
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

       a5=-(a4+a6+a7+a8+a9)

       ps4=psii(i,j-1)
       ps5=psii(i,j)
       ps6=psii(i,j+1)

       ps7=psii(i+1,j-1)
       ps8=psii(i+1,j)
       ps9=psii(i+1,j+1)

       DpiDne=-(a4*ps4+a5*ps5+a6*ps6+a7*ps7+a8*ps8+a9*ps9)

       DpsDni=DpiDni+DpeDni
       DpsDne=DpiDne+DpeDni

                  !write(6,*) 'De,Di,j',dpsdne,dpsdni,j

        sqk=sq2(i-1,j)+sq3(i-1,j-1)

        dlt0=(dlt(i,j-1)+dlt(i,j))*0.5d0
        r0=r(i,j)

          !fpv= Fvac**2-f(i)**2
          !fpv=-7.10d0

       curs(j)=(dlt0/r0**2)*fpv/(DpsDni+DpsDne)
                  !write(6,*) 'cursurf',curs(j)

       right(il)=right(il)+curs(j)*dlt0

       cur(i,j)=curs(j)*dlt0/sqk+cur(i,j)
       tokp=tokp!+curs(j)*dlt0
       toksfi=toksfi+curs(j)*dlt0

 10     continue

         curs(1)=curs(nt1)
         curs(nt)=curs(2)

                   fvv=sqrt(f(iplas)**2+fpv)
                   write(6,*) 'Fp,Fvac,fv',f(iplas),Fvac,fvv
         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine rightp_test

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

       real*8 psib(ntp)

          i=nr        !!<<<<<<<<<<<<<

         do 10 j=2,nt1

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)

       g1=g(i-1,j-1)
       g2=g(i-1,j)
       g3=g(i-1,j+1)

        dltk=(dlt(i,j-1)+dlt(i,j))*0.5d0
        dgdnl=a1*g1+a2*g2+a3*g3
        dgdn(j)=dgdnl/dltk

 10      continue

        dgdn(1)=dgdn(nt1)
        dgdn(nt)=dgdn(2)

       do 20 j=2,nt1

        psb=0.d0

       do 22 jb=2,nt1

        psb=psb+binadg(jb,j)*(dgdn(jb)+dgdn(jb+1))*0.5d0

 22    continue

        psib(j)=psb
        psii(i,j)=-psb

 20    continue

        psib(1)=psib(nt1)
        psib(nt)=psib(2)

        psii(i,1)=-psib(nt1)
        psii(i,nt)=-psib(2)

	  
!!%%%%%%%%%%%%%%%%%%%%%% test for g=r**2	  
	  	  
        do j=2,nt1
        psib(j)=-r(i,j)**2
        g(i,j)=-psib(j)
        enddo	  
	  
        psib(1)=psib(nt1)
        psib(nt)=psib(2)

        g(i,1)=g(i,nt1)
        g(i,nt)=g(i,2)
	  
!!%%%%%%%%%%%%%%%%%%%%%% test for g=r**2	  
	  
	               
                                        !
                                       !!
                                      ! !
          i=nr1  !!!!!!!!!!!!!!!!!!!!!  !
                                      ! !
         do 100 j=2,nt1                !!
                                        !
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

           il=numlin(i,j,nr,nt)

         right(il)= (a7*psib(j-1)+a8*psib(j)+a9*psib(j+1))

 100     continue


!         do 200 il=1,neq

!         right(il)=right(il)!-zpro(il)

! 200     continue


        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine rightg_test

!! riht-hand side for problem L(g)=J

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         do il=1,neqp

          right(il)=0.d0

         enddo

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_psib_ext(psex_av)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         dimension psex_b(ntp),rbon(ntp),zbon(ntp)
         dimension pspl_b(ntp),psi_bon(ntp)

          i=iplas        !!!!!!!!!!!!!!!!!!!!!!

         do j=1,nt
          rbon(j)=r(i,j)
          zbon(j)=z(i,j)
         psex_b(j)=psie(i,j)
         pspl_b(j)=psii(i,j)
         psi_bon(j)=pspl_b(j)+psex_b(j)
         enddo

        psex_b(1)=psex_b(nt1)
        psex_b(nt)=psex_b(2)

        psex_av = avr_bnd(psex_b)

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_psib_pla(pspl_av)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                  a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

         common/com_bgr/ bin_adg(ntp,ntp),dg_dn(ntp)
         dimension pspl_b(ntp)

          i=iplas        !!!!!!!!!!!!!!!!!!!!!!

         do 10 j=2,nt1

        pspl_b(j)=psii(i,j)

 10      continue

        pspl_b(1)=pspl_b(nt1)
        pspl_b(nt)=pspl_b(2)

        pspl_av = avr_bnd(pspl_b)

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine g_test

!! riht-hand side for problem L(g)=J

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

           dimension dif_g(nrp,ntp)

          errg_max=0.d0

         do i=1,nr
         do j=2,nt1

          dif_g(i,j)=g(i,j)-r(i,j)**2
          dif_abs=dabs(dif_g(i,j))
          errg_max=dmax1(errg_max,dif_abs)

          psi(i,j)=dif_g(i,j)

         enddo
         enddo

         do i=1,nr
          psi(i,1)=psi(i,nt1)
          psi(i,nt)=psi(i,2)

         enddo

         call f_wrd

        return
        end

         subroutine put_psib0(psi0_bnd)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         psibon0=psi0_bnd
	            
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




