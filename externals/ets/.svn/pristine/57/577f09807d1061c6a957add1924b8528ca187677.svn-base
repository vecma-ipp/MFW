!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine solint(imov)

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         common /comwrp/ rsp1,p1,ip1
         real*8 zw(neqp),rsp1(nspp)
         dimension wght(nrp)
         integer p1(neqp),ip1(neqp),isp1(nspp),ipath,flag,esp
         equivalence (rsp1(1),isp1(1))
	common
     *  /c_kpr/kpr
        
           ! write(6,*) 'solint:enter'

           if(imov.eq.0) then
           if(itin.eq.1) then
           ipath=1
          call odrvd(neqpla,ia,ja,a,p1,ip1,nspp,isp1,1,flag)
           else
           ipath=3
           endif
          call sdrvd(neqpla,p1,ip1,ia,ja,a,right,zw,nspp,
     *              isp1,rsp1,esp,ipath,flag)
             go to 1000
           endif

           if( itin/nitdel*nitdel+nitbeg .eq. itin 
     * 		.OR. itin.lt.nitbeg 
     * 		.OR. errm.gt.7.7d-2 ) then


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

c          do 860 i=1,neq
c             write(6,*) 'zw(i) i',i,zw(i)
c860       continue

          if(kpr.eq.1) then
          write(6,*) 'sdrv: flag,esp',flag,esp
          endif
         else

	     call solbit(zw)

	   endif

 1000      continue

c raspakovka reshenia

              errpss=0.d0
         do i=1,iplas-1
           if(ngav.eq.0) then        
             wght(i)=0.5d0
             !wght(i)=0.25d0
           else
             wght(i)=1.d0/(2.0d0+sqrt(1.d0-psia(i))*q(i)/q(1))
             if(iswtch.eq.1)  wght(i)=0.5d0  
           endif

	      if(iter.eq.1) wght(i)=1.0d0
         enddo



           do 500 i=1,iplas-1

           do 500 j=2,Nt1

           ieq=numlin(i,j,nr,nt)

            delpsi=dabs(psi(i,j)-zw(ieq))
            errpss=dmax1(errpss,delpsi)

        psi(i,j)=zw(ieq)*wght(i)+(1.d0-wght(i))*psi(i,j)

 500       continue

           do 600 i=1,iplas1

           psi(i,1)=psi(i,nt1)
           psi(i,nt)=psi(i,2)

 600       continue

          !write(6,*) 'solint:errpss',errpss

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine solbit(zw)

           include 'double.inc'
           include 'dim.inc'
           include 'compol.inc'

         common /comwrp/ rsp1,p1,ip1
         real*8 zw(neqp),zyy(neqp),rsp1(nspp),wpp(neqp),wzz(neqp),
     *          wrr(neqp),zuu(neqp)
         integer p1(neqp),ip1(neqp),isp1(nspp),ipath,flag,esp
         equivalence (rsp1(1),isp1(1))

	  !write(6,*) 'solbit:enter'

           itmax=10

	     relerr= 1.d-8


           do i=1,Nr1
           do j=2,Nt1

           ieq=numlin(i,j,nr,nt)

           zw(ieq)=psi(i,j)

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
C              write(6,*) ' **iter ',itk

                 ! pause ' '
                 ! stop
	       return
             end  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine nev_b(zw)

         include 'double.inc'
         include 'dim.inc'
         include 'compol.inc'

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

C        write(6,*) 'nev:',znmx

         return
         end








