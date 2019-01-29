      subroutine promat (n,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psim,iter,
     *                   kstep,fvac,psi_eav,pspl_av,psibon0,cur_mu)
!      diffusion & equilibrium both equations solvinq 
!      Newton metod with linearisation
! error-correcting 
      implicit real*8 (a-h,o-z),integer*4 (i-n)
      include 'dimpl1.inc'
       
       common /com_fixbon/ key_fixfree
       common/com_flag/kastr

       common/selcon/ psi_d(nrp),fi_d(nrp),f_d(nrp),ri_d(nrp),
     *               ps_pnt(nrp),del_psb,psi_bn1
       common/savt0/ psi0(nrp),fi0(nrp),f0(nrp),ri0(nrp),q0(nrp),
     *               dpsidt(nrp),dfidt(nrp),rm0,ac0n,skcen0
!     *              ,kstep_prev
       common/com_but/ sigma(nrp),cbut_b(nrp)
      dimension dfdpsi(n),dpdpsi(n),f(n),flx_fi(n),psia(n),q(n)
      dimension wrk(512)
!      save kstep_prev
      integer kstep_prev


      real*8 y(nrp,2)
      real*8 a(nrp,4)
      real*8 b(nrp,4)
      real*8 c(nrp,4)
      real*8 h(nrp,2)
      real*8 zn(nrp,2)

      real*8 psi(nrp)
      real*8 fi (nrp)
      real*8 ri (nrp)
      real*8 alfa22(nrp)
      real*8 alfa33(nrp)
      real*8 alp33k(nrp)
      real*8 ds  (nrp)
      real*8 dsk (nrp)
      real*8 dv  (nrp)
      real*8 dvk (nrp)
      real*8 dpsi(nrp)
      real*8 dfi (nrp)
      real*8 dri (nrp)
      real*8 df  (nrp)
      real*8 Fk  (nrp)
      real*8 rIk (nrp)
      real*8 Qk  (nrp)
      real*8 delf(nrp)
      real*8 dfdpsn(nrp)

!---------------------------------------------------------------------
       !return
          key_boncon=1
        if(key_boncon.eq.0) then
         dpdpsi(n)=0.d0
        endif
	 epsel = 1.0d-10
	 dt = 1.d0
       cap_psi=0.0d0   !1.d0
       cap_fi =0.d0
       !if(iter.lt.10) then
       !cap_psi=0.d0
       !cap_fi =0.d0
       !endif	  

	 fbnd = fvac

         !call psib_pla(pspl_av)
         call cof_bon(cps_bon,bps_bon,dps_bon)
        pspl_tst=-cps_bon*(psia(n)-psia(n-1))*psim
     *           +bps_bon*dfdpsi(n)+dps_bon*dpdpsi(n)

        psi_ext=psi_eav

        psi_bnd=psi_ext+pspl_av

        !!!psi_bnd=0.d0  !!!*
        !!!psi_ext=-pspl_av  !!!*
	 	  
!       metric coefficients
       call metcof (alfa22,alfa33,ds,dv,ac0,skcen,ri,
     *                     alp33k,dsk,dvk)

       do i=1,n
       dfdpsn(i)=dfdpsi(i)
       psi(i)=psim*psia(i)+psi_bnd
	 fi(i)=flx_fi(i)
        if(kstep.ne.kstep_prev) then
           jumpstep=kstep - kstep_prev
	   del_psb=psi_bn1-psibon0
       if((jumpstep.eq.1 .AnD. kstep.gt.1) .OR. key_fixfree.eq.0)then
          psibon0=psi_bn1	   	            
       endif
	   psi0(i)=psim*psia(i)+psibon0
	   fi0(i)=fi(i)
	   F0(i) =f(i)
	   rI0(i) =rI(i)
	   Q0(i) =q(i)
	  !dpsidt(i)=(psi(i)-psi0(i))/dt
        dpsidt(i)=0.d0
	  dfidt(i )=0.d0
        endif
        !xa=dfloat(i-1)/dfloat(n-1)	  	  
        xa=dsqrt(flx_fi(i)/flx_fi(n))	  	  
      if(kastr.eq.1) then
	 sigma(i) =fun_sig(xa,i)  
	 !sigma(i) =fun_sig_r(xa)  
	 cbut_b(i) =fun_jb(xa,i)  
	 !cbut_b(i) =fun_jb_rus(xa,dvk(i),dsk(i))  
      else
	 sigma(i) =fun_sig_a(xa,i)  
 	 cbut_b(i) =fun_jb_a(xa,i)  
      endif	 
	 enddo

        if(key_boncon.eq.0) then
         cbut_b(n)=0.d0
         sigma(n)=-1.d-6
        endif

        if(iter.eq.1) then
          rm0=rm
          skcen0=skcen
          ac0n=ac0
        endif	  

!---------------------------------------------------------------------

        do i=1,n
	  wrk(i)=dsqrt(dabs(alfa22(i)))   
	  !wrk(i)=(alfa22(i)**2-alfa22(i-1)**2)/ dsk(i)  
	  enddo


        !do i=1,n-1
	  !fi(i)=fi(n)*(i-1.d0)**2/(n-1.d0)**2   
	  !enddo

        do i=1,n-1
	  f(i)=alfa33(i)*(fi(i+1)-fi(i))/ds(i)   
	  ri(i)=alfa22(i)*(psi(i+1)-psi(i))/ds(i)   
	  q(i)=-(fi(i+1)-fi(i))/(psi(i+1)-psi(i))   
	  enddo

        F(N)=fbnd 

	  ffprim = rm*( ac0*(psi(1)-psi(2))/skcen - rm*dpdpsi(1) )
	  delf(1) = ffprim - dfdpsi(1) 
	  dfdpsi(1)=ffprim
        do i=2,n-1
	  ffprim = (f(i)**2-f(i-1)**2)/(psi(i+1)-psi(i-1))   
	  delf(i) = ffprim - dfdpsi(i) 
	  dfdpsi(i) =  ffprim  
	  !dfdpsn(i) =alp33k(i)*(ri(i)-ri(i-1))/dsk(i) 
	  enddo
	  
        alfa22n=alfa22(n)  !<<<<<<<<<<<<<
        !rI(n)=dsqrt(ri(n-1)**2+alfa22n/alp33k(n)*(F(N)**2-f(n-1)**2))
	  dfdpsi(n) =  (f(n)**2-f(n-1)**2)/(psi(n)-psi(n-1))


!---------------------------------------------------------------------
       do ks = 1,100             ! iteration cycle begins
	       
       do i=2,n-1
	  dpsi(i)=0.5d0*(psi(i+1)-psi(i-1))
	  dfi(i) =0.5d0*(fi(i+1)-fi(i-1))
	  dri(i) =ri(i)-ri(i-1)
	  df(i)  =f(i)-f(i-1)  
	  Fk(i) =0.5d0*(f(i)+f(i-1))
	  rIk(i) =0.5d0*(rI(i)+rI(i-1))
	  !Qk(i) =0.5d0*(q(i)+q(i-1))
	  Qk(i) =-(dsk(i)/alp33k(i))*(f(i)+f(i-1))/(psi(i+1)-psi(i-1))
	 enddo

!      matrix completing:   a(i)*y(i-1)-c(i)*y(i)+b(i)*y(i+1)=-h(i)
	 do i=2,n-1

        dpsdt=dpsidt(i)
        if(dpsdt.GE.0.d0) then
	   capfi=-cap_fi 
        else
	   capfi= cap_fi 
        endif

        dfldt=dfidt(i)
        if(dfldt.LE.0.d0) then
	   capsi=-cap_psi 
        else
	   capsi= cap_psi 
        endif

        d2psi=0.5d0*(psi(i+1)-2.d0*psi(i)+psi(i-1))        
        d2fi=0.5d0*(fi(i+1)-2.d0*fi(i)+fi(i-1))        

	a(i,1)= 0.5d0*dfidt(i)-( Fk(i)+0.5d0*df(i) )*
     +   	                 ( alfa22(i-1)/ds(i-1) )/sigma(i)     !a11
     +   	   -0.5d0*dfidt(i)*capsi 

	a(i,2)=-0.5d0*dpsidt(i)+( rIk(i)+0.5d0*dri(i) )*
     +	                   ( alfa33(i-1)/ds(i-1) )/sigma(i)   !a12
     +   	   +0.5d0*dpsidt(i)*capfi 

	a(i,3)=-0.5d0*dri(i)/dpsi(i) + alfa22(i-1)/ds(i-1)     !a21

	a(i,4)= Qk(i)*alfa33(i-1)/ds(i-1)
     +  +0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))*(alfa33(i-1)/ds(i-1))  !a22

!---------------------------------------------------------------------
	b(i,1)=-0.5d0*dfidt(i)+( 0.5d0*df(i)-Fk(i) )*
     +                       ( alfa22(i)/ds(i) )/sigma(i)       !b11
     +   	   -0.5d0*dfidt(i)*capsi 

	b(i,2)= 0.5d0*dpsidt(i)+( rIk(i)-0.5d0*dri(i) )*
     +                        ( alfa33(i)/ds(i) )/sigma(i)      !b12
     +   	   +0.5d0*dpsidt(i)*capfi 

	b(i,3)= 0.5d0*dri(i)/dpsi(i)+alfa22(i)/ds(i)           !b21

	b(i,4)= Qk(i)*alfa33(i)/ds(i)
     +  -0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))*(alfa33(i)/ds(i))    !b22

!---------------------------------------------------------------------
	c(i,1)=-dfi(i)/dt-( Fk(i)*(alfa22(i)/ds(i)+alfa22(i-1)/ds(i-1))   !c11
     +   -0.5d0*df(i)*( alfa22(i)/ds(i)-alfa22(i-1)/ds(i-1) ) )/sigma(i)  !(-0.5..)
     +   -dfidt(i)*capsi !-capfi*d2fi/dt

	c(i,2)= dpsi(i)/dt+
     +	    ( 0.5d0*dri(i)*(alfa33(i-1)/ds(i-1)-alfa33(i)/ds(i)) +    !c12
     +          rIk(i)*(alfa33(i)/ds(i)+alfa33(i-1)/ds(i-1)) )/sigma(i)
     +   +dpsidt(i)*capfi !+capsi*d2psi/dt

	c(i,3)= alfa22(i)/ds(i)+alfa22(i-1)/ds(i-1)                       !c21

	c(i,4)= Qk(i)*( alfa33(i)/ds(i)+alfa33(i-1)/ds(i-1) )             !c22
     +  +0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))
     +  *( alfa33(i-1)/ds(i-1)-alfa33(i)/ds(i) )
!---------------------------------------------------------------------
	  Fk0 =0.5d0*(f0(i)+f0(i-1))
	  rIk0 =0.5d0*(rI0(i)+rI0(i-1))
	  Qk0 =0.5d0*(q0(i)+q0(i-1))
	  dri0 =ri0(i)-ri0(i-1)
	  df0  =f0(i)-f0(i-1)  

	h(i,1)= dpsidt(i)*dfi(i)-dfidt(i)*dpsi(i)
     +	   +dpsidt(i)*capfi*d2fi-dfidt(i)*capsi*d2psi
     +       +( cbut_b(i)*dvk(i) )/sigma(i)
!     +	 -Fk(i)**2*( ri(i)/f(i)-ri(i-1)/f(i-1) )/sigma(i)
     +	 -(f(i)*f(i-1))*( ri(i)/f(i)-ri(i-1)/f(i-1) )/sigma(i)
!     +	 -( Fk(i)*dri(i)-rIk(i)*df(i) )/sigma(i)
!     +	 -( Fk0*dri0-rIk0*df0 )/sigma(i)

!	h(i,2)= Qk(i)*df(i)+dri(i)-dpdpsi(i)*(dv(i)+dv(i-1))*0.5d0
	h(i,2)= dri(i)-dfdpsi(i)*dsk(i)/alp33k(i)-dpdpsi(i)*dvk(i)
!---------------------------------------------------------------------
	enddo

!       boundary equations
	 b(1,1)= rm*ac0/(sigma(1)*skcen)
	 b(1,2)= 0.d0
	 b(1,3)= 0.d0
	 b(1,4)= 0.d0

	 c(1,1)= rm*ac0/(sigma(1)*skcen)-1.d0/dt     !c11
	 c(1,2)= 0.d0                                !c12
	 c(1,3)= 0.d0                                !c21
	 c(1,4)= 1.d0                                !c22

	 h(1,1)= dpsidt(1)-rm*ac0*(psi(1)-psi(2))/(sigma(1)*skcen)
     *                  +cbut_b(1)*rm**2/f(1)/sigma(1)
!     *                  -rm0*ac0n*(psi0(1)-psi0(2))/(sigma(1)*skcen0)
	 h(1,2)= 0.d0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if(key_boncon.eq.0) then
	 a(n,1)= cps_bon                        !a11                      
	 a(n,2)= 0.d0                           !a12                       
	 a(n,3)= 0.d0                           !a21                       
	 a(n,4)= 1.d0                           !a22                           
                                                                          
	 c(n,1)= cps_bon+1.d0                   !c11                           
	 c(n,2)= 0.d0                           !c12                          
	 c(n,3)= 0.d0                           !c21                           
	 c(n,4)= 1.d0                           !c22                           
                                                                        
	 h(n,1)= psi_ext+cps_bon*psi(n-1)-(cps_bon+1.d0)*psi(n)
     &        +dps_bon*dpdpsi(n)   
	 h(n,2)= fbnd*ds(n-1)/alfa33(n-1)-(fi(n)-fi(n-1))         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else
ch4astra       include 'pcof_e_in.f'  !!*!  
       include 'pcof_e_in.inc'  !!*!  
      endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        if(ks.eq.1) then
!       do i=1,n
!        !h(i,2)=0.d0
!	 enddo
!        !h(1,1)=0.d0
!        endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      progonka
       call mtrx_prog (n,y,a,b,c,h)
             

        do i=1,n
	  psi(i)= psi(i) + y(i,1)   ! y(i,1) = delta(psi(i))
	  fi(i) = fi(i)  + y(i,2)   ! y(i,2) = delta(fi (i))
	  dpsidt(i)=(psi(i)-psi0(i))/dt
	  dfidt(i )=(fi(i)-fi0(i))/dt
	  enddo

        do i=1,n-1
	  f(i)=alfa33(i)*(fi(i+1)-fi(i))/ds(i)   
	  ri(i)=alfa22(i)*(psi(i+1)-psi(i))/ds(i)   
	  q(i)=-(fi(i+1)-fi(i))/(psi(i+1)-psi(i))   
	  enddo

	  ffprim=rm*(ac0*(psi(1)-psi(2))/skcen-rm*dpdpsi(1))
	  delf(1) = ffprim - dfdpsi(1) 
	  dfdpsi(1)=ffprim
	  !dfdpsi(1)=0.5d0*(dfdps+dfdpsi(1))
        do i=2,n-1
	  ffprim = (f(i)**2-f(i-1)**2)/(psi(i+1)-psi(i-1))   
	  delf(i) = ffprim - dfdpsi(i) 
	  dfdpsi(i) =  ffprim  
	  !dfdpsn(i) =alp33k(i)*(ri(i)-ri(i-1))/dsk(i) 
	  enddo
	  !dfdpsi(n) =  dfdpsi(n-1)  !<<<<<<<<<<<<<
        alfa22n=alfa22(n)  !<<<<<<<<<<<<<
        F(N)=fbnd 
        rI(n)=dsqrt( ri(n-1)**2+alfa22n/alp33k(n)*(F(N)**2-f(n-1)**2)
     *      +alfa22n*dpdpsi(n)*(psi(n)-psi(n-1))*dvk(n)/dsk(n) )
	  dfdpsi(n) =  (f(n)**2-f(n-1)**2)/(psi(n)-psi(n-1))

         deltamax = 0.d0
	   do k=1,2
	   do i=1,n
	   aby = dabs(y(i,k))
	   if (aby.ge.deltamax) deltamax=aby
	   enddo
	   enddo

	do i=2,n-1

        dpsdt=dpsidt(i)
        if(dpsdt.GE.0.d0) then
	   capfi=-cap_fi 
        else
	   capfi= cap_fi 
        endif

        dfldt=dfidt(i)
        if(dfldt.LE.0.d0) then
	   capsi=-cap_psi 
        else
	   capsi= cap_psi 
        endif

        d2psi= (psi(i+1)-2.d0*psi(i)+psi(i-1))        
        d2fi= (fi(i+1)-2.d0*fi(i)+fi(i-1))        

      znv=(dpsidt(i)*(fi(i+1)-fi(i-1))-dfidt(i)*(psi(i+1)-psi(i-1)))
     *-(f(i)*f(i-1))*( ri(i)/f(i)-ri(i-1)/f(i-1) )*2.0d0
!     *-((f(i)+f(i-1))*(ri(i)-ri(i-1))-(f(i)-f(i-1))*(ri(i)+ri(i-1)))
     * /sigma(i)
     +	 +dpsidt(i)*capfi*d2fi-dfidt(i)*capsi*d2psi
!     *-( (f0(i)+f0(i-1))*(ri0(i)-ri0(i-1))
!     *                          -(f0(i)-f0(i-1))*(ri0(i)+ri0(i-1)) )
!     * /sigma
      zn(i,1)=0.5d0*znv

!      zn(i,2)= (ri(i)-ri(i-1)) + 0.5d0*(q(i)+q(i-1))*(f(i)-f(i-1))
!     *       - dpdpsi(i)*(dv(i)+dv(i-1))*0.5d0
      zn(i,2)= (ri(i)-ri(i-1)) - dfdpsi(i)*dsk(i)/alp33k(i)
     *       - dpdpsi(i)*dvk(i)
	enddo

         ddivmax = 0.d0
	   do k=1,2
	   do i=1,n
	   abd = dabs(zn(i,k))
	   if (abd.ge.ddivmax) ddivmax=abd
	   enddo
	   enddo

         cur_mu=ri(n)

	 if (deltamax.le.epsel) exit
	 !if (ddivmax.le.epsel) exit

        if(ks.eq.199) then
         write(*,*) 'no newton iterations convergence in promat' 
         write(*,*) 'execution was terminated'
         write(*,*) 'ks=',ks
         stop
        endif	
	     	    
       enddo                     ! iteration cycle ends
!---------------------------------------------------------------------

        kstep_prev=kstep
        psi_bn1=psi(n)

 10     continue
           f_wght=0.75d0
         do i=1,n
           dfdpsi(i)=f_wght*dfdpsi(i)+(1.d0-f_wght)*dfdpsn(i)        
	   enddo

!        For selfconsistency checking

         do i=1,n
           psi_d(i)=psi(i)-psi(n)         
           fi_d(i)=fi(i)         
           f_d(i)=f(i)         
           ri_d(i)=ri(i)         
           ps_pnt(i)=dpsidt(i)         
	   enddo

!---------------------------------------------------------------------
       !write(*,*) 'promat'
      return
      end

! +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      subroutine promat_I (n,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,psim,iter,
     *                   kstep,fvac,psi_eav,pspl_av,psibon0,tok)
!      diffusion & equilibrium both equations solvinq 
!      Newton metod with linearisation
! error-correcting 
      implicit real*8 (a-h,o-z),integer*4 (i-n)
      include 'dimpl1.inc'

      common/selcon/ psi_d(nrp),fi_d(nrp),f_d(nrp),ri_d(nrp),
     *               ps_pnt(nrp),del_psb,psi_bn1
       common/savt0/ psi0(nrp),fi0(nrp),f0(nrp),ri0(nrp),q0(nrp),
     *              dpsidt(nrp),dfidt(nrp),rm0,ac0n,skcen0
!     *              ,kstep_prev
       common/com_but/ sigma(nrp),cbut_b(nrp)
       common /com_fixbon/ key_fixfree
       common/com_flag/kastr
      dimension dfdpsi(n),dpdpsi(n),f(n),flx_fi(n),psia(n),q(n)
      dimension wrk(512)
!      save kstep_prev
      integer kstep_prev

      real*8 y(nrp,2)
      real*8 a(nrp,4)
      real*8 b(nrp,4)
      real*8 c(nrp,4)
      real*8 h(nrp,2)
      real*8 zn(nrp,2)

      real*8 psi(nrp)
      real*8 fi (nrp)
      real*8 ri (nrp)
      real*8 alfa22(nrp)
      real*8 alfa33(nrp)
      real*8 alp33k(nrp)
      real*8 ds  (nrp)
      real*8 dsk (nrp)
      real*8 dv  (nrp)
      real*8 dvk (nrp)
      real*8 dpsi(nrp)
      real*8 dfi (nrp)
      real*8 dri (nrp)
      real*8 df  (nrp)
      real*8 Fk  (nrp)
      real*8 rIk (nrp)
      real*8 Qk  (nrp)
      real*8 delf(nrp)
      real*8 dfdpsn(nrp)

!---------------------------------------------------------------------

	 epsel = 1.0d-10
	 dt = 1.d0
       cap_psi=0.d0   !1.d0
       cap_fi =0.d0
       !if(iter.lt.10) then
       !cap_psi=0.d0
       !cap_fi =0.d0
       !endif	  

	 fbnd = fvac

        ! call psib_pla(pspl_av)
!         call cof_bon(cps_bon,bps_bon,dps_bon)
!        pspl_tst=-cps_bon*(psia(n)-psia(n-1))*psim
!     *           +bps_bon*dfdpsi(n)+dps_bon*dpdpsi(n)
!
!        psi_ext=psi_eav
!
!        psi_bnd=psi_ext+pspl_av

         if(kstep.eq.1)then
          psi_bnd=psibon0
         else
          psi_bnd=psi_bn1
         endif

          psi_eav=psi_bnd-pspl_av
	 	  
!       metric coefficients
       call metcof (alfa22,alfa33,ds,dv,ac0,skcen,ri,
     *                     alp33k,dsk,dvk)

       do i=1,n
       dfdpsn(i)=dfdpsi(i)
       psi(i)=psim*psia(i)+psi_bnd
	 fi(i)=flx_fi(i)

        if(kstep.ne.kstep_prev) then
           jumpstep=kstep - kstep_prev
	   del_psb=psi_bn1-psibon0
       if((jumpstep.eq.1 .AnD. kstep.gt.1) .OR. key_fixfree.eq.0)then
          psibon0=psi_bn1	   	            
       endif
	   psi0(i)=psim*psia(i)+psibon0
	   fi0(i)=fi(i)
	   F0(i) =f(i)
	   rI0(i) =rI(i)
	   Q0(i) =q(i)
	  !dpsidt(i)=(psi(i)-psi0(i))/dt
        dpsidt(i)=0.d0
	  dfidt(i )=0.d0
        endif

!        xa=dfloat(i-1)/dfloat(n-1)	  	  
!	 sigma(i) =fun_sig(xa,i)  
!	 cbut_b(i) =fun_jb(xa,i)  

        xa=dsqrt(flx_fi(i)/flx_fi(n))	  	  
      if(kastr.eq.1) then
	 sigma(i) =fun_sig(xa,i)  
	 !sigma(i) =fun_sig_r(xa)  
	 cbut_b(i) =fun_jb(xa,i)  
	 !cbut_b(i) =fun_jb_rus(xa,dvk(i),dsk(i))  
      else
	 sigma(i) =fun_sig_a(xa,i)  
 	 cbut_b(i) =fun_jb_a(xa,i)  
      endif

	 enddo


        if(iter.eq.1) then
          rm0=rm
          skcen0=skcen
          ac0n=ac0
        endif	  

!---------------------------------------------------------------------

        do i=1,n
	  wrk(i)=dsqrt(dabs(alfa22(i)))   
	  !wrk(i)=(alfa22(i)**2-alfa22(i-1)**2)/ dsk(i)  
	  enddo


        do i=1,n-1
	  fi(i)=fi(n)*(i-1.d0)**2/(n-1.d0)**2   
	  enddo

        do i=1,n-1
	  f(i)=alfa33(i)*(fi(i+1)-fi(i))/ds(i)   
	  ri(i)=alfa22(i)*(psi(i+1)-psi(i))/ds(i)   
	  q(i)=-(fi(i+1)-fi(i))/(psi(i+1)-psi(i))   
	  enddo

        F(N)=fbnd 
        rI(n)=tok

	  ffprim=rm*(ac0*(psi(1)-psi(2))/skcen-rm*dpdpsi(1))
	  delf(1) = ffprim - dfdpsi(1) 
	  dfdpsi(1)=ffprim
        do i=2,n-1
	  ffprim = (f(i)**2-f(i-1)**2)/(psi(i+1)-psi(i-1))   
	  delf(i) = ffprim - dfdpsi(i) 
	  dfdpsi(i) =  ffprim  
	  dfdpsn(i) =alp33k(i)*(ri(i)-ri(i-1))/dsk(i) 
	  enddo
	  
        alfa22n=alfa22(n)  !<<<<<<<<<<<<<
	  dfdpsi(n) =  (f(n)**2-f(n-1)**2)/(psi(n)-psi(n-1))


!---------------------------------------------------------------------
       do ks = 1,100             ! iteration cycle begins
	       
       do i=2,n-1
	  dpsi(i)=0.5d0*(psi(i+1)-psi(i-1))
	  dfi(i) =0.5d0*(fi(i+1)-fi(i-1))
	  dri(i) =ri(i)-ri(i-1)
	  df(i)  =f(i)-f(i-1)  
	  Fk(i) =0.5d0*(f(i)+f(i-1))
	  rIk(i) =0.5d0*(rI(i)+rI(i-1))
	  !Qk(i) =0.5d0*(q(i)+q(i-1))
	  Qk(i) =-(dsk(i)/alp33k(i))*(f(i)+f(i-1))/(psi(i+1)-psi(i-1))
	 enddo

!      matrix completing:   a(i)*y(i-1)-c(i)*y(i)+b(i)*y(i+1)=-h(i)
	 do i=2,n-1

        dpsdt=dpsidt(i)
        if(dpsdt.GE.0.d0) then
	   capfi=-cap_fi 
        else
	   capfi= cap_fi 
        endif

        dfldt=dfidt(i)
        if(dfldt.LE.0.d0) then
	   capsi=-cap_psi 
        else
	   capsi= cap_psi 
        endif

        d2psi=0.5d0*(psi(i+1)-2.d0*psi(i)+psi(i-1))        
        d2fi=0.5d0*(fi(i+1)-2.d0*fi(i)+fi(i-1))        

	a(i,1)= 0.5d0*dfidt(i)-( Fk(i)+0.5d0*df(i) )*
     +   	                 ( alfa22(i-1)/ds(i-1) )/sigma(i)     !a11
     +   	   -0.5d0*dfidt(i)*capsi 

	a(i,2)=-0.5d0*dpsidt(i)+( rIk(i)+0.5d0*dri(i) )*
     +	                   ( alfa33(i-1)/ds(i-1) )/sigma(i)   !a12
     +   	   +0.5d0*dpsidt(i)*capfi 

	a(i,3)=-0.5d0*dri(i)/dpsi(i) + alfa22(i-1)/ds(i-1)     !a21

	a(i,4)= Qk(i)*alfa33(i-1)/ds(i-1)
     +  +0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))*(alfa33(i-1)/ds(i-1))  !a22

!---------------------------------------------------------------------
	b(i,1)=-0.5d0*dfidt(i)+( 0.5d0*df(i)-Fk(i) )*
     +                       ( alfa22(i)/ds(i) )/sigma(i)       !b11
     +   	   -0.5d0*dfidt(i)*capsi 

	b(i,2)= 0.5d0*dpsidt(i)+( rIk(i)-0.5d0*dri(i) )*
     +                        ( alfa33(i)/ds(i) )/sigma(i)      !b12
     +   	   +0.5d0*dpsidt(i)*capfi 

	b(i,3)= 0.5d0*dri(i)/dpsi(i)+alfa22(i)/ds(i)           !b21

	b(i,4)= Qk(i)*alfa33(i)/ds(i)
     +  -0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))*(alfa33(i)/ds(i))    !b22

!---------------------------------------------------------------------
	c(i,1)=-dfi(i)/dt-( Fk(i)*(alfa22(i)/ds(i)+alfa22(i-1)/ds(i-1))   !c11
     +   -0.5d0*df(i)*( alfa22(i)/ds(i)-alfa22(i-1)/ds(i-1) ) )/sigma(i)  !(-0.5..)
     +   -dfidt(i)*capsi !-capfi*d2fi/dt

	c(i,2)= dpsi(i)/dt+
     +	    ( 0.5d0*dri(i)*(alfa33(i-1)/ds(i-1)-alfa33(i)/ds(i)) +    !c12
     +          rIk(i)*(alfa33(i)/ds(i)+alfa33(i-1)/ds(i-1)) )/sigma(i)
     +   +dpsidt(i)*capfi !+capsi*d2psi/dt

	c(i,3)= alfa22(i)/ds(i)+alfa22(i-1)/ds(i-1)                       !c21

	c(i,4)= Qk(i)*( alfa33(i)/ds(i)+alfa33(i-1)/ds(i-1) )             !c22
     +  +0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))
     +  *( alfa33(i-1)/ds(i-1)-alfa33(i)/ds(i) )
!---------------------------------------------------------------------
	  Fk0 =0.5d0*(f0(i)+f0(i-1))
	  rIk0 =0.5d0*(rI0(i)+rI0(i-1))
	  Qk0 =0.5d0*(q0(i)+q0(i-1))
	  dri0 =ri0(i)-ri0(i-1)
	  df0  =f0(i)-f0(i-1)  

	h(i,1)= dpsidt(i)*dfi(i)-dfidt(i)*dpsi(i)
     +	   +dpsidt(i)*capfi*d2fi-dfidt(i)*capsi*d2psi
     +       +( cbut_b(i)*dvk(i) )/sigma(i)
!     +	 -Fk(i)**2*( ri(i)/f(i)-ri(i-1)/f(i-1) )/sigma(i)
     +	 -(f(i)*f(i-1))*( ri(i)/f(i)-ri(i-1)/f(i-1) )/sigma(i)
!     +	 -( Fk(i)*dri(i)-rIk(i)*df(i) )/sigma(i)
!     +	 -( Fk0*dri0-rIk0*df0 )/sigma(i)

!	h(i,2)= Qk(i)*df(i)+dri(i)-dpdpsi(i)*(dv(i)+dv(i-1))*0.5d0
	h(i,2)= dri(i)-dfdpsi(i)*dsk(i)/alp33k(i)-dpdpsi(i)*dvk(i)
!---------------------------------------------------------------------
	enddo

!       boundary equations
	 b(1,1)= rm*ac0/(sigma(1)*skcen)
	 b(1,2)= 0.d0
	 b(1,3)= 0.d0
	 b(1,4)= 0.d0

	 c(1,1)= rm*ac0/(sigma(1)*skcen)-1.d0/dt     !c11
	 c(1,2)= 0.d0                                !c12
	 c(1,3)= 0.d0                                !c21
	 c(1,4)= 1.d0                                !c22

	 h(1,1)= dpsidt(1)-rm*ac0*(psi(1)-psi(2))/(sigma(1)*skcen)
     *                  +cbut_b(1)*rm**2/f(1)/sigma(1)
!     *                  -rm0*ac0n*(psi0(1)-psi0(2))/(sigma(1)*skcen0)
	 h(1,2)= 0.d0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	 a(n,1)= cps_bon          ! 0.d0        !a11                      
!	 a(n,2)= 0.d0                           !a12                       
!	 a(n,3)= 0.d0                           !a21                       
!	 a(n,4)=-alfa33(n-1)/ds(n-1)            !a22                            
!                                                                          
!	 c(n,1)=cps_bon-1.d0                    !c11                                                  
!	 c(n,2)= 0.d0                           !c12                              
!	 c(n,3)= 0.d0                           !c21                                                  
!	 c(n,4)=-alfa33(n-1)/ds(n-1)            !c22                                 
!                                                                        
!	 h(n,1)=-psi_ext+cps_bon*psi(n-1)-(cps_bon-1.d0)*psi(n)   !0.d0     
!	 h(n,2)=-fbnd+alfa33(n-1)*(fi(n)-fi(n-1))/ds(n-1)    !dfi(n-1) ?     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       !!!include 'cof_in.f'   
ch4astra       include 'pcof_e_I_in.f'   !!!
       include 'pcof_e_I_in.inc'   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if(ks.eq.1) then
       do i=1,n
        !h(i,2)=0.d0
	 enddo
        !h(1,1)=0.d0
        endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      progonka
       call mtrx_prog (n,y,a,b,c,h)
             

        do i=1,n
	  psi(i)= psi(i) + y(i,1)   ! y(i,1) = delta(psi(i))
	  fi(i) = fi(i)  + y(i,2)   ! y(i,2) = delta(fi (i))
	  dpsidt(i)=(psi(i)-psi0(i))/dt
	  dfidt(i )=(fi(i)-fi0(i))/dt
	  enddo

        do i=1,n-1
	  f(i)=alfa33(i)*(fi(i+1)-fi(i))/ds(i)   
	  ri(i)=alfa22(i)*(psi(i+1)-psi(i))/ds(i)   
	  q(i)=-(fi(i+1)-fi(i))/(psi(i+1)-psi(i))   
	  enddo

	  ffprim=rm*(ac0*(psi(1)-psi(2))/skcen-rm*dpdpsi(1))
	  delf(1) = ffprim - dfdpsi(1) 
	  dfdpsi(1)=ffprim
	  !dfdpsi(1)=0.5d0*(dfdps+dfdpsi(1))
        do i=2,n-1
	  ffprim = (f(i)**2-f(i-1)**2)/(psi(i+1)-psi(i-1))   
	  delf(i) = ffprim - dfdpsi(i) 
	  dfdpsi(i) =  ffprim  
	  dfdpsn(i) =alp33k(i)*(ri(i)-ri(i-1))/dsk(i) 
	  enddo
	  !dfdpsi(n) =  dfdpsi(n-1)  !<<<<<<<<<<<<<
        !F(N)=fbnd 
        !rI(n)=tok
        alfa22n=alfa22(n)  !<<<<<<<<<<<<<
	  dfdpsi(n) =  (f(n)**2-f(n-1)**2)/(psi(n)-psi(n-1))

         deltamax = 0.d0
	   do k=1,2
	   do i=1,n
	   aby = dabs(y(i,k))
	   if (aby.ge.deltamax) deltamax=aby
	   enddo
	   enddo

	do i=2,n-1

        dpsdt=dpsidt(i)
        if(dpsdt.GE.0.d0) then
	   capfi=-cap_fi 
        else
	   capfi= cap_fi 
        endif

        dfldt=dfidt(i)
        if(dfldt.LE.0.d0) then
	   capsi=-cap_psi 
        else
	   capsi= cap_psi 
        endif

        d2psi= (psi(i+1)-2.d0*psi(i)+psi(i-1))        
        d2fi= (fi(i+1)-2.d0*fi(i)+fi(i-1))        

      znv=(dpsidt(i)*(fi(i+1)-fi(i-1))-dfidt(i)*(psi(i+1)-psi(i-1)))
     *-(f(i)*f(i-1))*( ri(i)/f(i)-ri(i-1)/f(i-1) )*2.0d0
!     *-((f(i)+f(i-1))*(ri(i)-ri(i-1))-(f(i)-f(i-1))*(ri(i)+ri(i-1)))
     * /sigma(i)
     +	 +dpsidt(i)*capfi*d2fi-dfidt(i)*capsi*d2psi
!     *-( (f0(i)+f0(i-1))*(ri0(i)-ri0(i-1))
!     *                          -(f0(i)-f0(i-1))*(ri0(i)+ri0(i-1)) )
!     * /sigma
      zn(i,1)=0.5d0*znv

!      zn(i,2)= (ri(i)-ri(i-1)) + 0.5d0*(q(i)+q(i-1))*(f(i)-f(i-1))
!     *       - dpdpsi(i)*(dv(i)+dv(i-1))*0.5d0
      zn(i,2)= (ri(i)-ri(i-1)) - dfdpsi(i)*dsk(i)/alp33k(i)
     *       - dpdpsi(i)*dvk(i)
	enddo

         ddivmax = 0.d0
	   do k=1,2
	   do i=1,n
	   abd = dabs(zn(i,k))
	   if (abd.ge.ddivmax) ddivmax=abd
	   enddo
	   enddo

	 if (deltamax.le.epsel) exit
	 !if (ddivmax.le.epsel) exit

        if(ks.eq.199) then
         write(*,*) 'no newton iterations convergence in promat' 
         write(*,*) 'execution was terminated'
         write(*,*) 'ks=',ks
         stop
        endif	
	     	    
       enddo                     ! iteration cycle ends
!---------------------------------------------------------------------

        kstep_prev=kstep
        psi_bn1=psi(n)

 10     continue

!        For selfconsistency checking

         do i=1,n
           psi_d(i)=psi(i)-psi(n)         
           fi_d(i)=fi(i)         
           f_d(i)=f(i)         
           ri_d(i)=ri(i)         
           ps_pnt(i)=dpsidt(i)         
	   enddo

!---------------------------------------------------------------------
      return
      end

! +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      subroutine mtrx_prog (n,y,a,b,c,f)
!      matrix progonka for two equations
      implicit real*8 (a-h,o-z),integer*4 (i-n)
      include 'dimpl1.inc'
      dimension y(nrp,2),a(nrp,4),b(nrp,4),c(nrp,4),f(nrp,2)
      real*8 alfa(nrp,4)
      real*8 beta(nrp,2)
!---------------------------------------------------------------------
!       a(i)*y(i-1)-c(i)*y(i)+b(i)*y(i+1)=-f(i), i=2,n-1
!       -c(1)*y(1)+b(1)*y(2)=-f(1), a(n)*y(n-1)-c(n)*y(n)=-f(n)
!---------------------------------------------------------------------
! a(i), b(i), c(i) -> matrix [2*2]; y(i), f(i) -> vector [2]
! {a(i)11  a(i)12  a(i)21  a(i)22} -> {a(i)1  a(i)2  a(i)3  a(i)4}
!---------------------------------------------------------------------
	 det=c(1,1)*c(1,4)-c(1,2)*c(1,3)   ! inverse c(1)
	 c1i1= c(1,4)/det
	 c1i2=-c(1,2)/det
	 c1i3=-c(1,3)/det
	 c1i4= c(1,1)/det
!---------------------------------------------------------------------
       alfa(1,1) = c1i1*b(1,1)+c1i2*b(1,3)
       alfa(1,2) = c1i1*b(1,2)+c1i2*b(1,4)
       alfa(1,3) = c1i3*b(1,1)+c1i4*b(1,3)
       alfa(1,4) = c1i3*b(1,2)+c1i4*b(1,4)

	 beta(1,1) = c1i1*f(1,1)+c1i2*f(1,2)
	 beta(1,2) = c1i3*f(1,1)+c1i4*f(1,2)
!---------------------------------------------------------------------
	 do j=2,n
	 cmb1 = c(j,1) - a(j,1)*alfa(j-1,1)-a(j,2)*alfa(j-1,3)
	 cmb2 = c(j,2) - a(j,1)*alfa(j-1,2)-a(j,2)*alfa(j-1,4)
	 cmb3 = c(j,3) - a(j,3)*alfa(j-1,1)-a(j,4)*alfa(j-1,3)
	 cmb4 = c(j,4) - a(j,3)*alfa(j-1,2)-a(j,4)*alfa(j-1,4)

	 det=cmb1*cmb4-cmb2*cmb3   !  inverse {c(i)-a(i)*alfa(i-1)}
	 cmbi1= cmb4/det
	 cmbi2=-cmb2/det
	 cmbi3=-cmb3/det
	 cmbi4= cmb1/det

	 alfa(j,1) = cmbi1*b(j,1)+cmbi2*b(j,3)
	 alfa(j,2) = cmbi1*b(j,2)+cmbi2*b(j,4)
	 alfa(j,3) = cmbi3*b(j,1)+cmbi4*b(j,3)
	 alfa(j,4) = cmbi3*b(j,2)+cmbi4*b(j,4)

	 fab1 = f(j,1) + a(j,1)*beta(j-1,1)+a(j,2)*beta(j-1,2)
	 fab2 = f(j,2) + a(j,3)*beta(j-1,1)+a(j,4)*beta(j-1,2)

	 beta(j,1) = cmbi1*fab1+cmbi2*fab2
	 beta(j,2) = cmbi3*fab1+cmbi4*fab2
	 enddo
!---------------------------------------------------------------------
       y(n,1) = beta(n,1)
	 y(n,2) = beta(n,2)

	 do j=n-1,1,-1
       y(j,1) = alfa(j,1)*y(j+1,1)+alfa(j,2)*y(j+1,2) + beta(j,1)
       y(j,2) = alfa(j,3)*y(j+1,1)+alfa(j,4)*y(j+1,2) + beta(j,2)
	 enddo
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        real*8 function fun_sig(a,i)
         implicit real*8(a-h,o-z)
         include 'dim.inc'
         common /com_tim/ dtim,ctim
	   common /com_sigcd/ C_sig(nrp),T_el(nrp),C_bts(nrp),C_driv(nrp)
         
         fun_sig=-C_sig(i)*amu0/dtim
	    t= T_el(i)        
         fun_sig_tst=-(8.d0*pi/3.d0)*dabs(t)**1.5d0*(1.d-4/dtim)

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        real*8 function fun_sig_(a)
         implicit real*8(a-h,o-z)
         pi=3.14159265359d0
         a0=0.8d0
         a1=0.97d0
	   sigm0 =-5.5d6  !-1.d3  -1.d3 -5.d5
	   sigm1 =-1.5d3  !-1.d3  -1.d3 -1.d4 
         if(a.le.a0)then           
          sigma= sigm0
         elseif(a.gt.a0 .AND. a.le.a1)then           
          sigma=sigm1+(sigm0-sigm1)*
!     *    ( 0.5d0*(1.d0+dcos(pi*(a-a0)/(a1-a0))) ) !**0.5d0  
     *     dabs( dcos(0.5d0*pi*(a-a0)/(a1-a0)) )**1.5  
         elseif(a.gt.a1)then           
          sigma= sigm1
         endif           
         !sigma=sigm1 + sigm0*(1.d0-a**3)  
         fun_sig=sigma
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        real*8 function fun_sig__(a)
         implicit real*8(a-h,o-z)
         pi=3.14159265359d0
         a0=0.95d0
         a1=0.97d0
	   sigm0 =-5.d6  !-1.d3  -1.d3 -5.d5
	   sigm1 =-1.d2  !-1.d3  -1.d3 -1.d4 
         if(a.le.a0)then           
          sigma= sigm0
         elseif(a.gt.a0 .AND. a.le.a1)then           
          sigma=sigm1+(sigm0-sigm1)*
!     *    ( 0.5d0*(1.d0+dcos(pi*(a-a0)/(a1-a0))) ) !**0.5d0  
     *     ( dcos(0.5d0*pi*(a-a0)/(a1-a0)) )**3  
         elseif(a.gt.a1)then           
          sigma= sigm1
         endif           
         !sigma=sigm1 + sigm0*(1.d0-a**3)  
         fun_sig=sigma
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        real*8 function fun___sig(a)
         implicit real*8(a-h,o-z)
         pi=3.14159265359d0
         a0=0.0d0
         a1=0.5d0  !1.0d0!
         a2=0.5075d0  !1.0d0!
	   t0 =5.0d3  
	   t1 =3.0d3  
	   t2 =1.0d3
	   tb =1.d2
         if(a.ge.a2)then
          t= tb + (t2-tb)*(1.d0-((a-a2)/(1.d0-a2))**2)
         elseif(a.le.a0)then           
          t= t0
         elseif(a.gt.a0 .AND. a.lt.a1)then           
          t=t1 + (t0-t1)*(1.d0-((a-a0)/(a1-a0))**2)  
         elseif(a.ge.a1 .AND. a.lt.a2)then           
          t=t2+(a-a2)*(t1-t2)/(a1-a2)
         endif           
         fun_sig=-(8.d0*pi/3.d0)*dabs(t)**1.5d0
         fun_sig=fun_sig/10.d0
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !real*8 function fun_sig(a,i)
        real*8 function fun_sig_a(a,i)
         implicit real*8(a-h,o-z)
         common /com_tim/ dtim,ctim
         pi=3.14159265359d0
         aeps=1.d-8
         a0=0.0d0
         a1=1.0d0  !1.0d0!
         a2=1.0d0  !1.0d0!
	   t0 =3.0d3  
	   t1 =1.0d2  
	   t2 =1.0d1
	   tb =1.d1

         !if(a.ge.a2)then
         ! t= tb + (t2-tb)*(1.d0-((a-a2)/(1.d0-a2+aeps))**2)
         !elseif(a.le.a0)then           
         ! t= t0
         !elseif(a.gt.a0 .AND. a.lt.a1)then           
         ! t=t1 + (t0-t1)*(1.d0-((a-a0)/(a1-a0))**2)  
         !elseif(a.ge.a1 .AND. a.lt.a2)then           
         ! t=t2+(a-a2)*(t1-t2)/(a1-a2)
         !endif   
	           
         if(a.le.a0)then           
          t= t0
         elseif(a.gt.a0 .AND. a.lt.a1)then           
          t=t1 + (t0-t1)*(1.d0-((a-a0)/(a1-a0))**2)  
         elseif(a.ge.a1)then           
          t=t1
         endif 
         
	   !!t=tb  	   
	             
         fun_sig=-(8.d0*pi/3.d0)*dabs(t)**1.5d0
         fun_sig_a=fun_sig*(1.d-4/dtim)

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        real*8 function fun_sigm(a)
         implicit real*8(a-h,o-z)
         pi=3.14159265359d0
         t0=3500.d0
         tp=0.2d0*t0
         ts=0.12d0*t0
         c=0.02d0
         d=0.98d0
         te=0.5d0*tp*( dtanh( (d-1.d0)/c ) +1.d0 )
         pa=2.d0
         pb=2.d0
         t=(t0-tp-ts+te)*(1-a**pa)**pb +.5*tp*(dtanh((d-a)/c) + 1)+ts-te
	   
	   	             
         fun_sig=-(8.d0*pi/3.d0)*dabs(t)**1.5d0
         fun_sig=fun_sig/2.5d0 !
         !!fun_sig=-8.d0  
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        real*8 function fun_jb(a,i)
         implicit real*8(a-h,o-z)
         include 'dim.inc'
	   common /com_sigcd/ C_sig(nrp),T_el(nrp),C_bts(nrp),C_driv(nrp)

         fun_jb=(C_bts(i)+C_driv(i))*amu0

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        real*8 function fun_jb_a(a,i)
         implicit real*8(a-h,o-z)
         include 'dimpl1.inc'
          common/com_jb/ BJ_av(nrp),curfi_av(nrp)

         pi=3.14159265359d0
         ampl=0.0d0 !0.d0  !1.5d0 0.5d0
         a0=0.50d0
         w=0.05d0
         fun=BJ_av(1)*( 1.d0-dtanh(((a0-a)/w)**2) )
         !fun=BJ_av(1)*dsin(pi*a)   !**0.5d0
         fun_jb_a=ampl*fun

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine promat_j (n,dfdpsi,dpdpsi,f,flx_fi,psia,q,rm,
     *                   psim,iter,kstep,fvac,psi_eav,pspl_av,psibon0)
!      diffusion & equilibrium both equations solvinq 
!      Newton metod with linearisation
      implicit real*8 (a-h,o-z),integer*4 (i-n)
      include 'dimpl1.inc'

       common /com_jb/ BJ_av(nrp),curfi_av(nrp)
       common/savt0/ psi0(nrp),fi0(nrp),f0(nrp),ri0(nrp),q0(nrp),
     *              dpsidt(nrp),dfidt(nrp),rm0,ac0n,skcen0
!     *              ,kstep_prev
       common/com_but/ sigma(nrp),cbut_b(nrp)
      dimension dfdpsi(n),dpdpsi(n),f(n),flx_fi(n),psia(n),q(n)
      dimension wrk(512)
!      save kstep_prev
      integer kstep_prev

      real*8 y(nrp,2)
      real*8 a(nrp,4)
      real*8 b(nrp,4)
      real*8 c(nrp,4)
      real*8 h(nrp,2)
      real*8 zn(nrp,2)

      real*8 psi(nrp)
      real*8 fi (nrp)
      real*8 ri (nrp)
      real*8 alfa22(nrp)
      real*8 alfa33(nrp)
      real*8 alp33k(nrp)
      real*8 ds  (nrp)
      real*8 dsk (nrp)
      real*8 dv  (nrp)
      real*8 dvk (nrp)
      real*8 dpsi(nrp)
      real*8 dfi (nrp)
      real*8 dri (nrp)
      real*8 df  (nrp)
      real*8 Fk (nrp)
      real*8 rIk (nrp)
      real*8 Qk (nrp)
      real*8 delf(nrp)
      real*8 dfdpsn(nrp)

!---------------------------------------------------------------------

	 epsel = 1.0d-11
	 !dt = 1.d0
	 dt = 1.d20
       cap_psi=0.d0   !1.d0
       cap_fi =0.d0
       !if(iter.lt.10) then
       !cap_psi=0.d0
       !cap_fi =0.d0
       !endif	  

	 fbnd = fvac
         !call psib_pla(pspl_av)
         call cof_bon(cps_bon,bps_bon,dps_bon)
       pspl_tst=-cps_bon*(psia(n)-psia(n-1))*psim
     *          +bps_bon*dfdpsi(n)+dps_bon*dpdpsi(n)

        !!!psi_bnd=psibon
        !psi_bnd=psi_ext+pspl_av
        psi_bnd=0.d0   !<<<***>>>>

!       metric coefficients
       call metcof (alfa22,alfa33,ds,dv,ac0,skcen,ri,
     *                     alp33k,dsk,dvk)

       do i=1,n
       dfdpsn(i)=dfdpsi(i)
       psi(i)=psim*psia(i)+psi_bnd
	 fi(i)=flx_fi(i)
        if(kstep.ne.kstep_prev) then
	   psi0(i)=psim*psia(i)+psibon0
	   fi0(i)=fi(i)
	   F0(i) =f(i)
	   rI0(i) =rI(i)
	   Q0(i) =q(i)
        !dpsidt(i)=0.d0
	  dpsidt(i)=(psi(i)-psi0(i))/dt
	  dfidt(i )=0.d0
        endif
        xa=dfloat(i-1)/dfloat(n-1)	  	  
	 !sigma(i) =fun_sig(xa,i)  
	 sigma(i) =1.d0  
	 !cbut_b(i) =fun_jb(xa,i)  
	 cbut_b(i) =BJ_av(i) !+fun_jb(xa,i)  
	 enddo

        if(iter.eq.1) then
          rm0=rm
          skcen0=skcen
          ac0n=ac0
        endif	  

!---------------------------------------------------------------------

        do i=1,n
	  wrk(i)=dsqrt(dabs(alfa22(i)))   
	  !wrk(i)=(alfa22(i)**2-alfa22(i-1)**2)/ dsk(i)  
	  enddo


        do i=1,n-1
	  fi(i)=fi(n)*(i-1.d0)**2/(n-1.d0)**2   
	  enddo

        do i=1,n-1
	  f(i)=alfa33(i)*(fi(i+1)-fi(i))/ds(i)   
	  ri(i)=alfa22(i)*(psi(i+1)-psi(i))/ds(i)   
	  q(i)=-(fi(i+1)-fi(i))/(psi(i+1)-psi(i))   
	  enddo

        F(N)=fbnd 

	  ffprim=rm*(ac0*(psi(1)-psi(2))/skcen-rm*dpdpsi(1))
	  delf(1) = ffprim - dfdpsi(1) 
	  dfdpsi(1)=ffprim
        do i=2,n-1
	  ffprim = (f(i)**2-f(i-1)**2)/(psi(i+1)-psi(i-1))   
	  delf(i) = ffprim - dfdpsi(i) 
	  dfdpsi(i) =  ffprim  
	  dfdpsn(i) =alp33k(i)*(ri(i)-ri(i-1))/dsk(i) 
	  enddo
	  
        alfa22n=alfa22(n)  !<<<<<<<<<<<<<
        !rI(n)=dsqrt(ri(n-1)**2+alfa22n/alp33k(n)*(F(N)**2-f(n-1)**2))
	  dfdpsi(n) =  (f(n)**2-f(n-1)**2)/(psi(n)-psi(n-1))


!---------------------------------------------------------------------
       do ks = 1,100             ! iteration cycle begins
	       
       do i=2,n-1
	  dpsi(i)=0.5d0*(psi(i+1)-psi(i-1))
	  dfi(i) =0.5d0*(fi(i+1)-fi(i-1))
	  dri(i) =ri(i)-ri(i-1)
	  df(i)  =f(i)-f(i-1)  
	  Fk(i) =0.5d0*(f(i)+f(i-1))
	  rIk(i) =0.5d0*(rI(i)+rI(i-1))
	  !Qk(i) =0.5d0*(q(i)+q(i-1))
	  Qk(i) =-(dsk(i)/alp33k(i))*(f(i)+f(i-1))/(psi(i+1)-psi(i-1))
	 enddo

!      matrix completing:   a(i)*y(i-1)-c(i)*y(i)+b(i)*y(i+1)=-h(i)
	 do i=2,n-1

        dpsdt=dpsidt(i)
        if(dpsdt.GE.0.d0) then
	   capfi=-cap_fi 
        else
	   capfi= cap_fi 
        endif

        dfldt=dfidt(i)
        if(dfldt.LE.0.d0) then
	   capsi=-cap_psi 
        else
	   capsi= cap_psi 
        endif

        d2psi=0.5d0*(psi(i+1)-2.d0*psi(i)+psi(i-1))        
        d2fi=0.5d0*(fi(i+1)-2.d0*fi(i)+fi(i-1))        

	a(i,1)= 0.5d0*dfidt(i)-( Fk(i)+0.5d0*df(i) )*
     +   	                 ( alfa22(i-1)/ds(i-1) )/sigma(i)     !a11
     +   	   -0.5d0*dfidt(i)*capsi 

	a(i,2)=-0.5d0*dpsidt(i)+( rIk(i)+0.5d0*dri(i) )*
     +	                   ( alfa33(i-1)/ds(i-1) )/sigma(i)   !a12
     +   	   +0.5d0*dpsidt(i)*capfi 

	a(i,3)=-0.5d0*dri(i)/dpsi(i) + alfa22(i-1)/ds(i-1)     !a21

	a(i,4)= Qk(i)*alfa33(i-1)/ds(i-1)
     +  +0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))*(alfa33(i-1)/ds(i-1))  !a22

!---------------------------------------------------------------------
	b(i,1)=-0.5d0*dfidt(i)+( 0.5d0*df(i)-Fk(i) )*
     +                       ( alfa22(i)/ds(i) )/sigma(i)       !b11
     +   	   -0.5d0*dfidt(i)*capsi 

	b(i,2)= 0.5d0*dpsidt(i)+( rIk(i)-0.5d0*dri(i) )*
     +                        ( alfa33(i)/ds(i) )/sigma(i)      !b12
     +   	   +0.5d0*dpsidt(i)*capfi 

	b(i,3)= 0.5d0*dri(i)/dpsi(i)+alfa22(i)/ds(i)           !b21

	b(i,4)= Qk(i)*alfa33(i)/ds(i)
     +  -0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))*(alfa33(i)/ds(i))    !b22

!---------------------------------------------------------------------
	c(i,1)=-dfi(i)/dt-( Fk(i)*(alfa22(i)/ds(i)+alfa22(i-1)/ds(i-1))   !c11
     +   -0.5d0*df(i)*( alfa22(i)/ds(i)-alfa22(i-1)/ds(i-1) ) )/sigma(i)  !(-0.5..)
     +   -dfidt(i)*capsi !-capfi*d2fi/dt

	c(i,2)= dpsi(i)/dt+
     +	    ( 0.5d0*dri(i)*(alfa33(i-1)/ds(i-1)-alfa33(i)/ds(i)) +    !c12
     +          rIk(i)*(alfa33(i)/ds(i)+alfa33(i-1)/ds(i-1)) )/sigma(i)
     +   +dpsidt(i)*capfi !+capsi*d2psi/dt

	c(i,3)= alfa22(i)/ds(i)+alfa22(i-1)/ds(i-1)                       !c21

	c(i,4)= Qk(i)*( alfa33(i)/ds(i)+alfa33(i-1)/ds(i-1) )             !c22
     +  +0.5d0*(df(i)/dpsi(i))*(dsk(i)/alp33k(i))
     +  *( alfa33(i-1)/ds(i-1)-alfa33(i)/ds(i) )
!---------------------------------------------------------------------
	  Fk0 =0.5d0*(f0(i)+f0(i-1))
	  rIk0 =0.5d0*(rI0(i)+rI0(i-1))
	  Qk0 =0.5d0*(q0(i)+q0(i-1))
	  dri0 =ri0(i)-ri0(i-1)
	  df0  =f0(i)-f0(i-1)  

	h(i,1)= dpsidt(i)*dfi(i)-dfidt(i)*dpsi(i)
     +	   +dpsidt(i)*capfi*d2fi-dfidt(i)*capsi*d2psi
!     +	 -Fk(i)**2*( ri(i)/f(i)-ri(i-1)/f(i-1) )/sigma(i)
     +	 -(f(i)*f(i-1))*( ri(i)/f(i)-ri(i-1)/f(i-1) )/sigma(i)
     +	 +( cbut_b(i)*dvk(i) )/sigma(i)
!     +	 -( Fk(i)*dri(i)-rIk(i)*df(i) )/sigma(i)
!     +	 -( Fk0*dri0-rIk0*df0 )/sigma(i)

!	h(i,2)= Qk(i)*df(i)+dri(i)-dpdpsi(i)*(dv(i)+dv(i-1))*0.5d0
	h(i,2)= dri(i)-dfdpsi(i)*dsk(i)/alp33k(i)-dpdpsi(i)*dvk(i)
!---------------------------------------------------------------------
	enddo

!       boundary equations
	 b(1,1)= rm*ac0/(sigma(1)*skcen)
	 b(1,2)= 0.d0
	 b(1,3)= 0.d0
	 b(1,4)= 0.d0

	 c(1,1)= rm*ac0/(sigma(1)*skcen)-1.d0/dt     !c11
	 c(1,2)= 0.d0                                !c12
	 c(1,3)= 0.d0                                !c21
	 c(1,4)= 1.d0                                !c22

	 h(1,1)= dpsidt(1)-rm*ac0*(psi(1)-psi(2))/(sigma(1)*skcen)
     *                  +cbut_b(1)*rm**2/f(1)/sigma(1)
!     *                  -rm0*ac0n*(psi0(1)-psi0(2))/(sigma(1)*skcen0)
	 h(1,2)= 0.d0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	 a(n,1)= cps_bon          ! 0.d0        !a11                      
!	 a(n,2)= 0.d0                           !a12                       
!	 a(n,3)= 0.d0                           !a21                       
!	 a(n,4)=-alfa33(n-1)/ds(n-1)            !a22                            
!                                                                          
!	 c(n,1)=cps_bon-1.d0                    !c11                                                  
!	 c(n,2)= 0.d0                           !c12                              
!	 c(n,3)= 0.d0                           !c21                                                  
!	 c(n,4)=-alfa33(n-1)/ds(n-1)            !c22                                 
!                                                                        
!	 h(n,1)=-psi_ext+cps_bon*psi(n-1)-(cps_bon-1.d0)*psi(n)   !0.d0     
!	 h(n,2)=-fbnd+alfa33(n-1)*(fi(n)-fi(n-1))/ds(n-1)    !dfi(n-1) ?     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       !!!include 'cof_in.f'   
ch4astra       include 'pcof_jb_in.f'   !!!
       include 'pcof_jb_in.inc'   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if(ks.eq.1) then
       do i=1,n
        !h(i,2)=0.d0
	 enddo
        !h(1,1)=0.d0
        endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      progonka
       call mtrx_prog (n,y,a,b,c,h)
             

        do i=1,n
	  psi(i)= psi(i) + y(i,1)   ! y(i,1) = delta(psi(i))
	  fi(i) = fi(i)  + y(i,2)   ! y(i,2) = delta(fi (i))
	  dpsidt(i)=(psi(i)-psi0(i))/dt
	  dfidt(i )=(fi(i)-fi0(i))/dt
	  enddo

        do i=1,n-1
	  f(i)=alfa33(i)*(fi(i+1)-fi(i))/ds(i)   
	  ri(i)=alfa22(i)*(psi(i+1)-psi(i))/ds(i)   
	  q(i)=-(fi(i+1)-fi(i))/(psi(i+1)-psi(i))   
	  enddo

	  ffprim=rm*(ac0*(psi(1)-psi(2))/skcen-rm*dpdpsi(1))
	  delf(1) = ffprim - dfdpsi(1) 
	  dfdpsi(1)=ffprim
	  !dfdpsi(1)=0.5d0*(dfdps+dfdpsi(1))
        do i=2,n-1
	  ffprim = (f(i)**2-f(i-1)**2)/(psi(i+1)-psi(i-1))   
	  delf(i) = ffprim - dfdpsi(i) 
	  dfdpsi(i) =  ffprim  
	  dfdpsn(i) =alp33k(i)*(ri(i)-ri(i-1))/dsk(i) 
	  enddo
	  !dfdpsi(n) =  dfdpsi(n-1)  !<<<<<<<<<<<<<
        alfa22n=alfa22(n)  !<<<<<<<<<<<<<
        F(N)=fbnd 
        !rI(n)=dsqrt(ri(n-1)**2+alfa22n/alp33k(n)*(F(N)**2-f(n-1)**2))
	  dfdpsi(n) =  (f(n)**2-f(n-1)**2)/(psi(n)-psi(n-1))

         deltamax = 0.d0
	   do k=1,2
	   do i=1,n
	   aby = dabs(y(i,k))
	   if (aby.ge.deltamax) deltamax=aby
	   enddo
	   enddo

	do i=2,n-1

        dpsdt=dpsidt(i)
        if(dpsdt.GE.0.d0) then
	   capfi=-cap_fi 
        else
	   capfi= cap_fi 
        endif

        dfldt=dfidt(i)
        if(dfldt.LE.0.d0) then
	   capsi=-cap_psi 
        else
	   capsi= cap_psi 
        endif

        d2psi= (psi(i+1)-2.d0*psi(i)+psi(i-1))        
        d2fi= (fi(i+1)-2.d0*fi(i)+fi(i-1))        

      znv=(dpsidt(i)*(fi(i+1)-fi(i-1))-dfidt(i)*(psi(i+1)-psi(i-1)))
     *-(f(i)*f(i-1))*( ri(i)/f(i)-ri(i-1)/f(i-1) )*2.0d0
!     *-((f(i)+f(i-1))*(ri(i)-ri(i-1))-(f(i)-f(i-1))*(ri(i)+ri(i-1)))
     * /sigma(i)
     +	 +dpsidt(i)*capfi*d2fi-dfidt(i)*capsi*d2psi
!     *-( (f0(i)+f0(i-1))*(ri0(i)-ri0(i-1))
!     *                          -(f0(i)-f0(i-1))*(ri0(i)+ri0(i-1)) )
!     * /sigma
      zn(i,1)=0.5d0*znv

!      zn(i,2)= (ri(i)-ri(i-1)) + 0.5d0*(q(i)+q(i-1))*(f(i)-f(i-1))
!     *       - dpdpsi(i)*(dv(i)+dv(i-1))*0.5d0
      zn(i,2)= (ri(i)-ri(i-1)) - dfdpsi(i)*dsk(i)/alp33k(i)
     *       - dpdpsi(i)*dvk(i)
	enddo

         ddivmax = 0.d0
	   do k=1,2
	   do i=1,n
	   abd = dabs(zn(i,k))
	   if (abd.ge.ddivmax) ddivmax=abd
	   enddo
	   enddo

	 if (deltamax.le.epsel) exit
	 !if (ddivmax.le.epsel) exit

        rI(n)=dsqrt(ri(n-1)**2+alfa22(n)/alp33k(n)*(F(N)**2-f(n-1)**2)
     *      +alfa22n*dpdpsi(n)*(psi(n)-psi(n-1))*dvk(n)/dsk(n) )

        if(ks.eq.99) then
         write(*,*) 'no newton iterations convergence in promat' 
         write(*,*) 'execution was terminated'
         write(*,*) 'ks=',ks
         stop
        endif	
	     	    
       enddo                     ! iteration cycle ends
!---------------------------------------------------------------------

        kstep_prev=kstep

 10     continue
!---------------------------------------------------------------------
      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine put_key_fix(k)
          common /com_fixbon/ key_fixfree
          integer k,key_fixfree
            key_fixfree=k
         return
         end
