         subroutine f_solve(isol,wdm)

!--ISOL-->if ISOL equal 1 reodering and factorization are asumed
!--       to be done previosly.(PATH=3 will be used in SDRVD)
!--
!--       if ISOL equal 0 reodering and factorization will be done
!--       (PATH=1 will be used in ODRVD and SDRVD)
!--
!--WDM    two dimensional grid array containing rezult of solving

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         common /comwrc/ rsp,p,ip

         real*8 zw(neqp),rsp(nspp),wdm(nrp,ntp)

         integer p(neqp),ip(neqp),isp(nspp),ipath,flag,esp

         equivalence (rsp(1),isp(1))


          ! write(*,*) 'solve:enter'
   

           if( itin/nitdel*nitdel+nitbeg .eq. itin 
     * 		.OR. itin.lt.nitbeg ) then
         

           ipath=3

         if(isol.ne.0) go to 20

          call odrvd(neq,ia,ja,a,p,ip,nspp,isp,1,flag)

           !do 10 i=1,neqp
           !  ip(i)=i
           !   p(i)=i
!10        continue

          !write(6,*) 'odrv flag=',flag

           ipath=1
          !isol=1

 20        continue


         call sdrvd(neq,p,ip,ia,ja,a,right,zw,nspp,
     *              isp,rsp,esp,ipath,flag)

c           do 860 i=1,neq
c          write(6,*) 'zw(i) i',i,zw(i)
c860       continue

          ! write(6,*) 'sdrv: flag,esp',flag,esp

        !call f_nev(zw)


         else

             call solvit(isol,zw)

         endif

c raspakovka reshenia

           do 500 i=1,Nr1
           do 500 j=2,Nt1

           ieq=numlin(i,j,nr,nt)

           wdm(i,j)=zw(ieq)

 500       continue

           do 600 i=1,Nr

           wdm(i,1)=wdm(i,nt1)
           wdm(i,nt)=wdm(i,2)

 600       continue

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine solvit(isol,zw)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         common /comwrc/ rsp,p,ip

         real*8 zw(neqp),zyy(neqp),rsp(nspp),wpp(neqp),wzz(neqp),
     *          wrr(neqp),zuu(neqp)

         integer p(neqp),ip(neqp),isp(nspp),ipath,flag,esp

         equivalence (rsp(1),isp(1))

	  !write(6,*) 'solvit:enter'
       
           itmax=10
	     relerr=1.d-8

              do il=1,neq
	         wrr(il)=right(il)
 
              enddo			     

	  if(isol.eq.0) then

           do i=1,Nr1
           do j=2,Nt1

           ieq=numlin(i,j,nr,nt)
             
           zw(ieq)=g(i,j)

           enddo
           enddo

        elseif(isol.eq.1) then

           do  i=1,Nr1
           do  j=2,Nt1

           ieq=numlin(i,j,nr,nt)
             
           zw(ieq)=psii(i,j)

           enddo
           enddo
        endif

          do il=1,neq

            i1=ia(il)
            i2=ia(il+1)-1

            znes=0.d0
         
          do im=i1,i2

            ic=ja(im)

            znes=znes+daop(im)*zw(ic)

         enddo

            zyy(il)=wrr(il)-znes

         enddo

           call sdrvd(neq,p,ip,ia,ja,aop0,zyy,zw,nspp,
     *              isp,rsp,esp,3,flag)

           !if(itin/2*2.ne.itin) 
		  return

		 itk=0      
           ido=0

 77            call dpcgrc(ido,neq,zw,wpp,wrr,wzz,relerr,itmax)

	    !write(6,*) 'ido',ido
            !  pause ' '

        if(ido.eq.1) then

          do 10 il=1,neq

            i1=ia(il)
            i2=ia(il+1)-1

            znes=0.d0
         
          do 100 im=i1,i2

            ic=ja(im)

            znes=znes+a(im)*wpp(ic)

 100      continue

            wzz(il)=znes

 10      continue

             go to 77

       elseif(ido.eq.2) then

           call sdrvd(neq,p,ip,ia,ja,aop0,wrr,wzz,nspp,
     *              isp,rsp,esp,3,flag)
           ! write(6,*) 'sdrv: flag,esp',flag,esp
		 
              if(itk.eq.1) go to 1054

          do il=1,neq

            i1=ia(il)
            i2=ia(il+1)-1

            znes=0.d0
         
          do im=i1,i2

            ic=ja(im)

            znes=znes+daop(im)*wzz(ic)

         enddo

            zyy(il)=znes

         enddo

           call sdrvd(neq,p,ip,ia,ja,aop0,zyy,zuu,nspp,
     *              isp,rsp,esp,3,flag)
             !write(6,*) 'sdrv: flag,esp',flag,esp 

           do il=1,neq

            wzz(il)=wzz(il)-zuu(il)

           enddo

 1054      continue

                  ! zermax=0.d0
                  ! znvmax=0.d0
               ! do il=1,neq
          
               !    znev=dabs(wrr(il))  
               !    znvmax=dmax1(znev,znvmax)

                   !zerr=dabs(zw(il)-zw0(il))  
                   !zermax=dmax1(zerr,zermax)

               ! enddo

              itk=itk + 1
	         ! write(6,*) 'nev,err iter ',znvmax,zermax,itk

             
	       ! if(znvmax.lt.relerr) go to 537
            go to 77
		  
         endif

 537         continue

              !write(6,*) 'nev,err iter ',znvmax,zermax,itk
              !! write(6,*) ' itk ',itk

                  ! pause ' '
                 ! stop
	       return
             end  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_nev(zw)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         real*8 zw(neqp)

         do 10 il=1,neq

         i1=ia(il)
         i2=ia(il+1)-1

         znev=0.d0
         znmx=0.d0

         do 100 im=i1,i2

         ic=ja(im)

         znev=znev+a(im)*zw(ic)

       !   if(il.eq.1) then
       !
       ! write(6,*) 'im a(im)',im,a(im)
       ! write(6,*) 'ic zw(ic)',ic,zw(ic)
       ! write(6,*) 'znev   '  ,znev
       !   endif

 100     continue

         znev=right(il)-znev
         znab=dabs(znev)
c        write(6,*) '***right znev ***',il,right(il),znev
         znmx=dmax1(znmx,znab)

 10      continue
           !write(6,*) 'nev:',znmx

         return
         end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_solint

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         common /comwrp/ rsp1,p1,ip1

         real*8 zw(neqp),rsp1(nspp)

         integer p1(neqp),ip1(neqp),isp1(nspp),ipath,flag,esp

         equivalence (rsp1(1),isp1(1))
	common
     *  /c_kpr/kpr

           ! write(6,*) 'solint:enter'

           if( itin/nitdel*nitdel+nitbeg .eq. itin 
     * 		.OR. itin.lt.nitbeg ) then

          call odrvd(neqpla,ia,ja,a,p1,ip1,nspp,isp1,1,flag)

           !do 10 i=1,neqpla
           !  ip1(i)=i
           !   p1(i)=i
! 10        continue

          ! write(6,*) 'odrv flag=',flag

           ipath=1

 20        continue

         call sdrvd(neqpla,p1,ip1,ia,ja,a,right,zw,nspp,
     *              isp1,rsp1,esp,ipath,flag)

c           do 860 i=1,neq
c          write(6,*) 'zw(i) i',i,zw(i)
c860       continue

         !  write(6,*) 'sdrv: flag,esp',flag,esp
         else

	     call soleit(zw)

	   endif

c raspakovka reshenia
              errpss=0.d0

           do 500 i=1,iplas-1
           do 500 j=2,Nt1

           ieq=numlin(i,j,nr,nt)

            delpsi=dabs(psi(i,j)-zw(ieq))
            errpss=dmax1(errpss,delpsi)

           psi(i,j)=zw(ieq)

 500       continue

           do 600 i=1,iplas1

           psi(i,1)=psi(i,nt1)
           psi(i,nt)=psi(i,2)

 600       continue


          if(kpr.eq.1) then
          write(*,*) 'solint:errpss',errpss
          endif


         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine solext

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         common /comwrp/ rsp1,p1,ip1

         real*8 zw(neqp),rsp1(nspp)

         integer p1(neqp),ip1(neqp),isp1(nspp),ipath,flag,esp

         equivalence (rsp1(1),isp1(1))

           write(*,*) 'solext:enter'

           if( itin/nitdel*nitdel+nitbeg .eq. itin 
     * 		.OR. itin.lt.nitbeg ) then

          call odrvd(neqpla,ia,ja,a,p1,ip1,nspp,isp1,1,flag)

           !do 10 i=1,neqpla
           !  ip1(i)=i
           !   p1(i)=i
! 10        continue

           !write(6,*) 'odrv flag=',flag

           ipath=1

 20        continue

         call sdrvd(neqpla,p1,ip1,ia,ja,a,right,zw,nspp,
     *              isp1,rsp1,esp,ipath,flag)

c           do 860 i=1,neq
c          write(6,*) 'zw(i) i',i,zw(i)
c860       continue

           write(*,*) 'sdrv: flag,esp',flag,esp
         else

	     call soleit(zw)

	   endif

c raspakovka reshenia

           do 500 i=1,iplas-1
           do 500 j=2,Nt1

           ieq=numlin(i,j,nr,nt)

           psie(i,j)=zw(ieq)

 500       continue

           do 600 i=1,Nr

           psie(i,1)=psie(i,nt1)
           psie(i,nt)=psie(i,2)

 600       continue

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine soleit(zw)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         common /comwrp/ rsp1,p1,ip1

         real*8 zw(neqp),zyy(neqp),rsp1(nspp),wpp(neqp),wzz(neqp),
     *          wrr(neqp),zuu(neqp)

         integer p1(neqp),ip1(neqp),isp1(nspp),ipath,flag,esp

         equivalence (rsp1(1),isp1(1))

	  !write(6,*) 'soleit:enter'
       
           itmax=10
	     relerr=1.d-8


           do i=1,Nr1
           do j=2,Nt1

           ieq=numlin(i,j,nr,nt)
             
           zw(ieq)=psie(i,j)

           enddo
           enddo

              do il=1,neqpla
	         wrr(il)=right(il)
 
              enddo			     

          do il=1,neqpla

            i1=ia(il)
            i2=ia(il+1)-1

            znes=0.d0
         
          do im=i1,i2

            ic=ja(im)

            znes=znes+dapp(im)*zw(ic)

         enddo

            zyy(il)=wrr(il)-znes

         enddo

           call sdrvd(neqpla,p1,ip1,ia,ja,app0,zyy,zw,nspp,
     *              isp1,rsp1,esp,3,flag)

           !if(itin/2*2.ne.itin) 
		  return
           
		 itk=0      
           ido=0

 77            call dpcgrc(ido,neqpla,zw,wpp,wrr,wzz,relerr,itmax)

	    !write(6,*) 'ido',ido
             ! pause 'soleit '

        if(ido.eq.1) then

          do 10 il=1,neqpla

            i1=ia(il)
            i2=ia(il+1)-1

            znes=0.d0
         
          do 100 im=i1,i2

            ic=ja(im)

            znes=znes+a(im)*wpp(ic)

 100      continue

            wzz(il)=znes

 10      continue

             go to 77

       elseif(ido.eq.2) then

           call sdrvd(neqpla,p1,ip1,ia,ja,app0,wrr,wzz,nspp,
     *              isp1,rsp1,esp,3,flag)
          ! write(6,*) 'sdrv: flag,esp',flag,esp 

             if(itk.eq.1) go to 1056

          do il=1,neqpla

            i1=ia(il)
            i2=ia(il+1)-1

            znes=0.d0
         
          do im=i1,i2

            ic=ja(im)

            znes=znes+dapp(im)*wzz(ic)

         enddo

            zyy(il)=znes

         enddo

           call sdrvd(neqpla,p1,ip1,ia,ja,app0,zyy,zuu,nspp,
     *              isp1,rsp1,esp,3,flag)
          ! write(6,*) 'sdrv: flag,esp',flag,esp 

           do il=1,neqpla

            wzz(il)=wzz(il)-zuu(il)

           enddo

 1056      continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 do il=1,neq
! 
!          i1=ia(il)
!          i2=ia(il+1)-1
! 
!          znes=0.d0
!          
!          do im=i1,i2
! 
!          ic=ja(im)
! 
!          if(ic.eq.il) then
! 	    adiag=a(im)
!           go to 110
! 	   endif	
! 		 
!           enddo
! 
!  110       wzz(il)=wrr(il)/adiag
! 
!                 enddo 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         !         zermax=0.d0
         !         znvmax=0.d0
         !      do il=1,neq
         !
         !         znev=dabs(wrr(il))  
         !         znvmax=dmax1(znev,znvmax)

         !         zerr=dabs(zw(il)-zw0(il))  
         !         zermax=dmax1(zerr,zermax)

         !      enddo

              itk=itk + 1
	      ! write(6,*) 'nev,err iter ',znvmax,zermax,itk

             
	       ! if(znvmax.lt.relerr) go to 537
            go to 77
		  
         endif

 537         continue

              !write(6,*) 'nev,err iter ',znvmax,zermax,itk
               !write(6,*) ' iter ',itk

                 ! pause ' '
                 ! stop
	       return
             end  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine ctra

           include'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include'dim.inc'
           include'compol.inc'
           include 'compol_add.inc'

c        real*8 a(lp)
         real*8 zw(neqp),ssw(neqp)
c        equivalence(a(1),vol1(1,1))

               sqcen=0.d0
         do 30 j=2,nt1

          sqcen=sqcen+sq1(1,j)+sq4(1,j)

 30      continue
           ssw(1)=sqcen

           do 20 i=2,Nr1
           do 20 j=2,Nt1

          sqk=sq1(i,j)+sq2(i-1,j)+sq3(i-1,j-1)+sq4(i,j-1)

           il=numlin(i,j,nr,nt)
           ssw(il)=sqk
 20      continue

           do 500 i=1,Nr1
           do 500 j=2,Nt1

           il=numlin(i,j,nr,nt)

          !zw(il)=1.
          !zw(il)=ro(i,j)**2
          !zw(il)=z(i,j)
          !zw(il)=r(i,j)**2
           zw(il)=psie(i,j)

 500       continue



         znmx=0.d0
         do 10 il=1,neq

         i1=ia(il)
         i2=ia(il+1)-1

         znev=0.d0

         do 100 im=i1,i2

         ic=ja(im)

         znev=znev+a(im)*zw(ic)

       !   if(il.eq.1) then
       !
       ! write(6,*) 'im a(im)',im,a(im)
       ! write(6,*) 'ic zw(ic)',ic,zw(ic)
       ! write(6,*) 'znev   '  ,znev
       !   endif

 100     continue
         znev=znev/ssw(il)
         znab=dabs(znev)
         !write(1,*) '*** znev ***',il,znev
         znmx=dmax1(znmx,znab)

 10      continue

           !write(1,*) 'nev:',znmx

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine aprod

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

       common/comaaa/ a12(nrp,ntp),a23(nrp,ntp),a34(nrp,ntp),
     +                a14(nrp,ntp),a13(nrp,ntp),a24(nrp,ntp)

c        real*8 a(lp)
         real*8 zw(nrp,ntp)
c        equivalence(a(1),vol1(1,1))

           do 500 i=1,Nr
           do 500 j=1,Nt

           zw(i,j)=psie(i,j)+clr*r(i,j)**2+clz*z(i,j)
          !zw(i,j)=          clr*r(i,j)**2+clz*z(i,j)
          !zw(i,j)=              r(i,j)**2
          !zw(i,j)=psie(i,j)
          !zw(i,j)=greeni(0.5d0,0.d0,r(i,j),z(i,j))

 500       continue

         do 10 i=1,nr1

        if(i.eq.1) then

           asum=0.d0
           a0=0.d0

         do 20 j=2,nt1

       aj=a12(1,j)+a24(1,j)+a34(1,j-1)+a13(1,j-1)
       a0=a0-aj
       asum=asum+aj*zw(2,j)

 20      continue

       asum=asum+a0*zw(1,2)

       zpro(1)=asum

        else

         do 30 j=2,nt1

       a1=a13(i-1,j-1)
       a2=a34(i-1,j-1)+a12(i-1,j)
       a3=a24(i-1,j)
       a4=a23(i-1,j-1)+a14(i,j-1)
       a6=a14(i,j)+a23(i-1,j)
       a7=a24(i,j-1)
       a8=a34(i,j-1)+a12(i,j)
       a9=a13(i,j)

       a5=-(a1+a2+a3+a4+a6+a7+a8+a9)

       u1=zw(i-1,j-1)
       u2=zw(i-1,j)
       u3=zw(i-1,j+1)
       u4=zw(i,j-1)
       u5=zw(i,j)
       u6=zw(i,j+1)
       u7=zw(i+1,j-1)
       u8=zw(i+1,j)
       u9=zw(i+1,j+1)

       il=numlin(i,j,nr,nt)

       zpro(il)=a1*u1+a2*u2+a3*u3+a4*u4+a5*u5+a6*u6+a7*u7+a8*u8+a9*u9

 30      continue

        endif

 10      continue

           do 200 i=1,Nr1
           do 200 j=2,Nt1

           ieq=numlin(i,j,nr,nt)

           aex(i,j)=zpro(ieq)

 200       continue

           do 600 i=1,Nr1

           aex(i,1)=aex(i,nt1)
           aex(i,nt)=aex(i,2)

 600       continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine aprod0

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

c        real*8 a(lp)
         real*8 zw(neqp)
c        equivalence(a(1),vol1(1,1))


           do 500 i=1,Nr1
           do 500 j=2,Nt1

           il=numlin(i,j,nr,nt)

           zw(il)=psie(i,j)+clr*r(i,j)**2+clz*z(i,j)
          !zw(il)=          clr*r(i,j)**2+clz*z(i,j)
          !zw(il)=              r(i,j)**2
          !zw(il)=psie(i,j)
          !zw(il)=greeni(0.5d0,0.d0,r(i,j),z(i,j))

 500       continue

         do 10 il=1,neq

         i1=ia(il)
         i2=ia(il+1)-1

          asum=0.d0

         do 100 im=i1,i2
          ic=ja(im)
          asum=asum+a(im)*zw(ic)
 100     continue

         zpro(il)=asum

 10      continue

           do 50 j=2,Nt1
           il=numlin(nr1,j,nr,nt)
           zpro(il)=0.d0
 50        continue

           do 200 i=1,Nr1
           do 200 j=2,Nt1

           ieq=numlin(i,j,nr,nt)

           aex(i,j)=zpro(ieq)

 200       continue

           do 600 i=1,Nr1

           aex(i,1)=aex(i,nt1)
           aex(i,nt)=aex(i,2)

 600       continue

         return
         end


