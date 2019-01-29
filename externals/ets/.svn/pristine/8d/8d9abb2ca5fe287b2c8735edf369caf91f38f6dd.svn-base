!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! grdef axdef regrid remesh prgrid regrid0 grid 
!!!! fdefln ada reform cpr bonspl loop95 deriv5 ge
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine renet

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'
           include 'parrc1.inc'
           include 'comrec.inc'
	common
     *  /c_kpr/kpr

          do i=iplas,1,-1      
            if(psia(i).ge.0.05d0) then
             i_bc=i
             exit
            endif
          enddo

            do j=2,nt1
             ronor(nr,j)=1.57d0
             ro(nr,j)=ro(i_bc,j)*ronor(nr,j)
            enddo

            do i=2,iplas
            do j=2,nt1
             ro(i,j)=ro(iplas,j)*ronor(i,j)
            enddo
            enddo

            do i=iplas+1,nr-1
            do j=2,nt1
             ropla=ro(iplas,j)
             robon=ro(nr,j)
             delro=(robon-ropla)/(nr-iplas)
             ro(i,j)=ropla+delro*(i-iplas)
             ronor(i,j)=ro(i,j)/ropla
            enddo
            enddo

            do i=2,nr
            do j=2,nt1
             r(i,j)=ro(i,j)*cos(teta(j))+rm
             z(i,j)=ro(i,j)*sin(teta(j))+zm
            enddo
            enddo

          do i=1,nr

           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)

           ronor(i,1)=ronor(i,nt1)
           ronor(i,nt)=ronor(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

          enddo


         rb_max=rm
         rb_min=rm
         zb_max=zm
         zb_min=zm

         do j=2,nt1
          if(r(nr,j).gt.rb_max) rb_max=r(nr,j)
          if(r(nr,j).lt.rb_min) rb_min=r(nr,j)
          if(z(nr,j).gt.zb_max) zb_max=z(nr,j)
          if(z(nr,j).lt.zb_min) zb_min=z(nr,j)
         enddo

         if(rb_max.gt.xmax
     *         .OR.
     *      zb_max.gt.ymax
     *         .OR.
     *      rb_min.lt.xmin 
     *         .OR. 
     *      zb_min.lt.ymin ) then 
        
          if(kpr.eq.1) then
          write(*,*) 'eqq is large then rectangular box' 
          call f_wrd
          pause 'pause '
          endif
         endif

!!!!!!!!! angle theta redefinition
        key_ang=1
        if(key_ang.eq.1) then
          tet0=teta(2)
         do j=2,nt1
          teta(j)=tet0+2.d0*pi*(j-2)/(nt-2.d0)
         enddo
          teta(1)=teta(nt1)-2.d0*pi
          teta(nt)=teta(2)+2.d0*pi

            do i=2,nr
            do j=2,nt1
             r(i,j)=ro(i,j)*cos(teta(j))+rm
             z(i,j)=ro(i,j)*sin(teta(j))+zm
            enddo
            enddo

          do i=1,nr

           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)

           ronor(i,1)=ronor(i,nt1)
           ronor(i,nt)=ronor(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

          enddo
         endif


        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_regrid(erro)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
         parameter(ntp4=ntp+4,ntp6=ntp4*6)
           !!include 'comlmtr.inc'
         common /com_sp3/ RRKsp(ntp4),CCKsp(ntp4)
           include 'compol.inc'
           include 'compol_add.inc'

         dimension ron(ntp),rop(ntp),rn(ntp),zn(ntp),tetn(ntp)
         dimension rob(ntp),teti(ntp),roi(ntp)
         dimension roh(nrp),rh(nrp),zh(nrp)
         dimension roplt(nrp,ntp)
         dimension psiold(nrp,ntp)
         dimension dp(5)
	   real*8 cwksp(4)
	common
     *  /c_kpr/kpr

!         xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1)

         cos(xx)=dcos(xx)
         sin(xx)=dsin(xx)
         atan(xx)=datan(xx)
         asin(xx)=dasin(xx)
         sqrt(xx)=dsqrt(xx)

             npro=nr-iplas+1

           if(ngav.eq.0) then
            alfa=0.995d0
           else
            alfa=0.5d0
           endif

         !   if(itin.gt.1) then
         !    do i=1,nr 
         !    do j=1,nt 
         !     psi(i,j)=alfa*psi(i,j)+(1.0d0-alfa)*psiold(i,j)
         !    enddo
         !    enddo
         !   endif

          rmold=rm
          zmold=zm
c           write(6,*) 'rm,zm,psim',rm,zm,psim

             call axdef(rma,zma,psima,dp)
c           write(6,*) 'rma,zma,psima',rma,zma,psima


	     rm=rma*alfa+rmold*(1.0d0-alfa)
	     zm=zma*alfa+zmold*(1.0d0-alfa)
	     psim=psima

             erro=dsqrt((rmold-rm)**2+(zmold-zm)**2)

          if(kpr.eq.1) then
           write(*,*) 'erro mag.axix',erro
          endif
!	   pause ' pau'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         rxold=rx0
         zxold=zx0

         call f_xpoint(rx1,zx1,ixp1,jxp1,psix1,tetx1,kodex1)

         call f_xpoint(rx2,zx2,ixp2,jxp2,psix2,tetx2,kodex2)

          if(kpr.eq.1) then
           write(*,*) 'kodex1,kodex2',kodex1,kodex2
          endif
!	   pause ' pau'

         if(kodex1.eq.0 .AND. kodex2.eq.0) then
          if(psix1.ge.psix2) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
	    else
           psix0=psix2
           rx0=rx2
           zx0=zx2
          endif
         elseif(kodex1.eq.0) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
         elseif(kodex2.eq.0) then
           psix0=psix2
           rx0=rx2
           zx0=zx2
         endif
             errox=dsqrt((rxold-rx0)**2+(zxold-zx0)**2)
          if(kpr.eq.1) then
           write(*,*) 'erro x-point',errox
          endif
C          write(*,*) 'rx zx',rx0,zx0,psix0

	     psip=psim-alp*(psim-psix0)

	   if(nctrl.eq.1) then
          do llim=1,nblm
	      call fdefln(psiblm,rblm(llim),zblm(llim))   
           if(psiblm.gt.psip ) then              
            psip=psiblm
            numlim=llim 
           endif		                             
          enddo
         endif		                             

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          
           errpsi=0.d0

         do i=1,nr
         do j=1,nt

	    psnn=psin(i,j)
          psin(i,j)=(psi(i,j)-psip)/(psim-psip)
		delpsn=dabs(psin(i,j)-psnn)

           if(delpsn.gt.errpsi) then
             ierm=i
             jerm=j
	       errpsi=delpsn
           endif
		!errpsi=dmax1(errpsi,delpsn)
		
         enddo
         enddo

          if(kpr.eq.1) then
         write(6,*) 'i,j errpsi',ierm,jerm,errpsi,(psim-psip)
          endif		                             
C	   pause ' pau'

cc      definition of new angle grid teta(j)

         do j=1,nt
           tetn(j)=teta(j)
         enddo

        do j=1,nt

         drx=r(nr,j)-rm
         dzx=z(nr,j)-zm

         rob(j)=sqrt(drx**2+dzx**2)
         teta(j)=atan(dzx/drx)

        enddo

            do j=2,nt  !<---

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+pi
         endif

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+pi
         endif
            
	      enddo      !<---


           teta(1)=teta(nt1)-2.d0*pi
           teta(nt)=teta(2)+2.d0*pi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
!          i=2
!          zps =psia(i)
!
!          ps0=psip+zps*(psim-psip) 
!
!            do j=1,nt 
!
!        ttj=teta(j) 
!       ro2j=(ps0-psim)/( 0.5d0*dp(3)*cos(ttj)**2
!    +                +     dp(4)*cos(ttj)*sin(ttj)
!    +                + 0.5d0*dp(5)*sin(ttj)**2 )
!
!         rop(j)=dsqrt(ro2j)     
!
!
!            r(i,j)=rm+rop(j)*cos(teta(j))
!            z(i,j)=zm+rop(j)*sin(teta(j))
!
!            enddo  ! j-loop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ccc    grid moving along rais

          do i=2,iplas

           zps =psia(i)

          do j=1,nt     !<------ j-loop

               do is=1,nr1
 	  
            ps0 =psin(is,j)
            !psmn=psin(is-1,j)
            pspl=psin(is+1,j)
 
            ro0 =ro(is,j)
            !romn=ro(is-1,j)
            ropl=ro(is+1,j)
                   
 	              if(zps.le.ps0 .and. zps.ge.pspl) then
               zro=((pspl-zps)*ro0-(ps0-zps)*ropl)/(pspl-ps0)
                 go to 822
 	              endif
           !zro=qvadin(zps, psmn,ps0,pspl, romn,ro0,ropl) 
                  enddo
 822     continue


         !   ps0 =psin(i,j)
         !   psmn=psin(i-1,j)
         !   pspl=psin(i+1,j)

         !  ro0 =ro(i,j)
         !  romn=ro(i-1,j)
         !  ropl=ro(i+1,j)

         !    gradpl=-(pspl-ps0)/(ropl-ro0)
         !    gradmn=-(ps0-psmn)/(ro0-romn)

         !   grad=dmax1(gradpl,gradmn)

         ! !alfa=1.0d0+0.45d0*(q(i)-q(1))/q(1)          
         !  alfa=1.5d0      

	   !  if(ngav.eq.1) grad=grad*alfa

	   !  zro=ro0-(psia(i)-ps0)/grad

           ron(j)=zro*alfa+ro(i,j)*(1.d0-alfa)


          enddo  !<------ j-loop

        do j=1,nt

            rnj=rmold+ron(j)*cos(tetn(j))
            znj=zmold+ron(j)*sin(tetn(j))

         drx=rnj-rm
         dzx=znj-zm

         roi(j)=sqrt(drx**2+dzx**2)
         teti(j)=atan(dzx/drx)

        enddo

            do j=2,nt  !<---

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+pi
         endif

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+pi
         endif
            
	      enddo      !<---


           teti(1)=teti(nt1)-2.d0*pi
           teti(nt)=teti(2)+2.d0*pi

           roi(1)=roi(nt1)
           roi(nt)=roi(2)


          do j=2,nt1 !<------ j-loop

              tetv=teta(j)

            if(tetv.lt.teti(1)) tetv=tetv+2.d0*pi
            if(tetv.gt.teti(nt)) tetv=tetv-2.d0*pi

                do jj=1,nt1

	         
           tt0 =teti(jj)
           !ttmn=teti(j-1)
           ttpl=teti(jj+1)

           ro0 =roi(jj)
           !romn=roi(j-1)
           ropl=roi(jj+1)

	            if(tetv.le.ttpl .and. tetv.ge.tt0) then
              zro=((ttpl-tetv)*ro0+(tetv-tt0)*ropl)/(ttpl-tt0)
               go to 1047
	            endif
                enddo

 1047      continue

           ! zro=qvadin(teta(j), ttmn,tt0,ttpl, romn,ro0,ropl)
		 rop(j)=zro

                    roerr=dabs(ron(j)-ro(i,j))
	              if(roerr.gt.erro) then
	                 erro=roerr
                      ierm=i
                      jerm=j
	              endif

             roplt(i,j)=ron(j)-ro(i,j)


           r(i,j)=rop(j)*cos(teta(j))+rm
           z(i,j)=rop(j)*sin(teta(j))+zm

          enddo  !<------  j-loop

          enddo  ! i-loop

          if(kpr.eq.1) then
           write(*,*) 'erro max.',erro
           write(*,*) 'i,j erro',ierm,jerm,erro
          endif
C	   pause 'pau'

             do i=2,iplas
             do j=2,nt1

            ro(i,j)=dsqrt((r(i,j)-rm)**2+(z(i,j)-zm)**2)

             enddo  !<------  j-loop
             enddo  ! i-loop


            do j=2,nt1

            ro(nr,j)=rob(j)

           ro(1,j)=0.d0
           r(1,j)=rm
           z(1,j)=zm

            enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do j=2,nt1

         spro=ro(nr,j)-ro(iplas-1,j)

         droj=ro(iplas,j)-ro(iplas-1,j)
         cpro=cpr(droj,npro,spro)
         row=ro(iplas,j)
         dro=droj

        do 220 i=iplas+1,nr1

             dro=dro*cpro
             row=row+dro

             rh(i)=rm+row*cos(teta(j))
             zh(i)=zm+row*sin(teta(j))

             roh(i)=row

              do is=2,nr1

            ps0 =psin(is,j)
            pspl=psin(is+1,j)

            ro0 =ro(is,j)
            ropl=ro(is+1,j)

 	     if(row.le.ropl .and. row.ge.ro0) then
		  isw=is 
		  go to 755
	     endif

              enddo

 755     continue       

           ro2=ropl
           ro1=ro0

           ps2=pspl
           ps1=ps0

           psia(i)=( ps1*(ro2-row)+ps2*(row-ro1) )/(ro2-ro1)

 220      continue

          do 250 i=iplas+1,nr1

            roerr=dabs(roh(i)-ro(i,j))
            erro=dmax1(erro,roerr)

           ro(i,j)=roh(i)

           r(i,j)=roh(i)*cos(teta(j))+rm
           z(i,j)=roh(i)*sin(teta(j))+zm

           psin(i,j)=psia(i)

 250      continue

            enddo

           do i=1,nr
           do j=2,nt1
 
            ronor(i,j)=ro(i,j)/ro(iplas,j)
 
           enddo
           enddo

          do 25 i=1,nr

           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)

           ronor(i,1)=ronor(i,nt1)
           ronor(i,nt)=ronor(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

           roplt(i,1)=roplt(i,nt1)
           roplt(i,nt)=roplt(i,2)

 25       continue


           do 30 i=1,iplas
           do 30 j=1,nt
 
            psin(i,j)=psia(i)
 
  30       continue

           do i=1,nr
           do j=1,nt
 
            psiold(i,j)=psip+psin(i,j)*(psim-psip)
 
           enddo
           enddo



!       if(ngav.eq.10) then     
!	    open(1,file='erro.wr')
!         write(1,*) iplas,nt
!         write(1,*) ((r(i,j),i=1,iplas),j=1,nt)
!         write(1,*) ((z(i,j),i=1,iplas),j=1,nt)
!	   write(1,*) ((roplt(i,j),i=1,iplas),j=1,nt)
!          close(1)
!	  call f_wrd
!		pause 'erro.wr '
!         endif

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_remesh(erro)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

              psimax=psi(1,2)
	        imax=1
	        jmax=2

             do i=2,iplas1
             do j=2,nt1

                if(psi(i,j).gt.psimax) then
                   psimax=psi(i,j)
			     imax=i
			     jmax=j
		      endif

             enddo
             enddo



         ! if(kstep.ge.5 .AnD. kstep.le.8 .AnD. iter/2*2.eq.iter) then
         !      call f_prgrid(imax,jmax,erro)
         !    return
         ! endif



             if(imax.ge.2) then
                call f_prgrid(imax,jmax,erro)
             else
	          call f_regrid0(erro)
             endif

		 return
		 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_prgrid(imax,jmax,erro)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           parameter(nshp=ntp+1)
           !!include 'comlmtr.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         dimension ron(ntp),rop(ntp),rn(ntp),zn(ntp),tetn(ntp)
         dimension rob(ntp),teti(ntp),roi(ntp)
         dimension xs(nshp),ys(nshp),fun(nshp),dp(5)
         dimension roh(nrp),rh(nrp),zh(nrp)
         dimension roplt(nrp,ntp)
         dimension psiold(nrp,ntp)
	common
     *  /c_kpr/kpr
         dimension dp_ex(5)

          atan(xx)=datan(xx)
          acos(xx)=dacos(xx)
          sqrt(xx)=dsqrt(xx)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!! new position of magnetic axis !!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              write(17,*) 'prgrid:::'
             nctrli=nctrl

             npro=nr-iplas+1


           rimjm=r(imax,jmax)
           zimjm=z(imax,jmax)

         !call axpnt_e(ue_ax,rimjm,zimjm,i_ax,j_ax,dp_ex,kodax)


           rm0=r(1,1)
           zm0=z(1,1)
           psim0=psi(1,1)

           nsh=1

           xs(nsh)=r(imax,jmax)
           ys(nsh)=z(imax,jmax)
           fun(nsh)=psi(imax,jmax)

          if(imax.eq.1) then
           do j=2,nt1
           nsh=nsh+1
           xs(nsh)=r(2,j)
           ys(nsh)=z(2,j)
           fun(nsh)=psi(2,j)
           enddo
          elseif(imax.eq.2) then

           nsh=nsh+1
           xs(nsh)=r(1,2)
           ys(nsh)=z(1,2)
           fun(nsh)=psi(1,2)
           do j=2,nt1
           nsh=nsh+1
           xs(nsh)=r(3,j)
           ys(nsh)=z(3,j)
           fun(nsh)=psi(3,j)
           enddo

          else

        do k=-1,1
          i= imax+k
        do l=-1,1
          j= jmax+l
          if(i.ne.imax .OR. j.ne.jmax) then
           nsh=nsh+1
           xs(nsh)=r(i,j)
           ys(nsh)=z(i,j)
           fun(nsh)=psi(i,j)
	    endif
        enddo
        enddo
           
          endif

          call deriv5(xs,ys,fun,nsh,5,dp)

        !do kdp=1,5
        ! dp(kdp)=dp(kdp) + dp_ex(kdp)
        !enddo

        DET = dp(3)*dp(5) - dp(4)**2
        Rm = Xs(1) + ( dp(2)*dp(4) - dp(1)*dp(5) )/DET
        Zm = Ys(1) + ( dp(1)*dp(4) - dp(2)*dp(3) )/DET

          erro=dsqrt( (rm-rm0)**2+(zm-zm0)**2 )

           rmx=r(imax,jmax)
           zmx=z(imax,jmax)

       psim=fun(1)+ dp(1)*(rm-rmx) + dp(2)*(zm-zmx)
     +          + 0.5d0*dp(3)*(rm-rmx)*(rm-rmx)
     +          +       dp(4)*(rm-rmx)*(zm-zmx)
     +          + 0.5d0*dp(5)*(zm-zmx)*(zm-zmx)

          if(kpr.eq.1) then
         write(*,*) 'rm zm psim',rm,zm,psim
         write(*,*) 'prgrid:errma',erro
          endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!! new position of x-point       !!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         rxold=rx0
         zxold=zx0

         call f_xpoint(rx1,zx1,ixp1,jxp1,psix1,tetx1,kodex1)

         call f_xpoint(rx2,zx2,ixp2,jxp2,psix2,tetx2,kodex2)

          if(kpr.eq.1) then
           write(*,*) 'kodex1,kodex2',kodex1,kodex2
          endif
!	   pause ' pau'
          kodxp=kodex1*kodex2
       if(kodxp.ne.0) then

          if(kpr.eq.1) then
              write(*,*) ' all x-points out of box'
              write(*,*) ' only limiter case '
          endif
                  nctrli=1

           !write(6,*) ' program cannot run more '
           !stop

	     psip=-1.d12

	 else  !kodxp=0
           psipn=psip

         if(kodex1.eq.0 .AND. kodex2.eq.0) then
          if(psix1.ge.psix2) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
	    else
           psix0=psix2
           rx0=rx2
           zx0=zx2
          endif
         elseif(kodex1.eq.0) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
         elseif(kodex2.eq.0) then
           psix0=psix2
           rx0=rx2
           zx0=zx2
         endif
             errox=dsqrt((rxold-rx0)**2+(zxold-zx0)**2)
          if(kpr.eq.1) then
           write(*,*) 'erro x-point',errox
          endif
C          write(6,*) 'rx zx',rx0,zx0,psix0

	     psip=psim-alp*(psim-psix0)
          if(itin.gt.1) then
           zwgt=1.0d0
	     psipr=zwgt*psip+(1.d0-zwgt)*psipn
	     psip=dmax1(psip,psipr)
          endif
       endif

	   if(nctrl.eq.1) then
            numlim=0
          do llim=1,nblm
	      call fdefln(psiblm,rblm(llim),zblm(llim))   
           if(psiblm.gt.psip ) then              
            psip=psiblm
            numlim=llim 
           endif		                             
          enddo
         endif		                             
          if(kpr.eq.1) then
           write(*,*) 'numlim',numlim
          endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!! nomalized poloidal flux definition !!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          errpsi=0.d0

         do i=1,nr
         do j=1,nt

	    psnn=psin(i,j)
          psin(i,j)=(psi(i,j)-psip)/(psim-psip)
		delpsn=dabs(psin(i,j)-psnn)

           if(delpsn.gt.errpsi) then
             ierm=i
             jerm=j
	       errpsi=delpsn
           endif

		!errpsi=dmax1(errpsi,delpsn)

         enddo
         enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!! definition of new angle grid teta(j) !!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         do j=1,nt
           tetn(j)=teta(j)
         enddo

        do j=1,nt
         drx=r(nr,j)-rm
         dzx=z(nr,j)-zm
         robj=sqrt(drx**2+dzx**2)
         rob(j)=robj
         tetp=acos(drx/robj)
          if(dzx.lt.0.d0) then
          teta(j)=-tetp
          else
          teta(j)=tetp
          endif
        enddo

            do j=2,nt  !<---

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+2.d0*pi
         endif

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+2.d0*pi
         endif

	      enddo      !<---

           teta(1)=teta(nt1)-2.d0*pi
           teta(nt)=teta(2)+2.d0*pi

ccc    grid moving along rais

          psinm0=psin(1,2)

	    do i=3,iplas1
	     if(psia(i).lt.psinm0) then
	      !ibeg=i
	      ibeg=i+1
	      go to 10
	     endif
	    enddo

 10       continue

        do i=ibeg,iplas
          zps =psia(i)
         do j=1,nt     !<------ j-loop
          do is=1,nr1
            ps0 =psin(is,j)
            pspl=psin(is+1,j)
            ro0 =ro(is,j)
            ropl=ro(is+1,j)
 	     if(zps.le.ps0 .and. zps.ge.pspl) then
		  isc=is
		  go to 822
	     endif
          enddo

 822    continue

            grad=-(pspl-ps0)/(ropl-ro0)

	     zro=ro0-(psia(i)-ps0)/grad
           ron(j)=zro
         enddo   !<------ j-loop

        do j=1,nt

            rnj=rm0+ron(j)*cos(tetn(j))
            znj=zm0+ron(j)*sin(tetn(j))

         drx=rnj-rm
         dzx=znj-zm

         roi(j)=sqrt(drx**2+dzx**2)
         tetp=acos(drx/roi(j))
          if(dzx.lt.0.d0) then
          teti(j)=-tetp
          else
          teti(j)=tetp
          endif

        enddo

        do j=2,nt  !<---

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+2.d0*pi
         endif

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+2.d0*pi
         endif

	  enddo      !<---

           teti(1)=teti(nt1)-2.d0*pi
           teti(nt)=teti(2)+2.d0*pi

           roi(1)=roi(nt1)
           roi(nt)=roi(2)

        do j=2,nt1 !<------ j-loop

              tetv=teta(j)

            if(tetv.lt.teti(1)) tetv=tetv+2.d0*pi
            if(tetv.lt.teti(1)) tetv=tetv+2.d0*pi
            if(tetv.gt.teti(nt)) tetv=tetv-2.d0*pi
            if(tetv.gt.teti(nt)) tetv=tetv-2.d0*pi

          do jj=1,nt1

           tt0 =teti(jj)
           ttpl=teti(jj+1)

           ro0 =roi(jj)
           ropl=roi(jj+1)

	       if(tetv.le.ttpl .and. tetv.ge.tt0) then
              zro=((ttpl-tetv)*ro0+(tetv-tt0)*ropl)/(ttpl-tt0)
              go to 1047
	       endif
          enddo

 1047      continue

		 rop(j)=zro

           r(i,j)=rop(j)*cos(teta(j))+rm
           z(i,j)=rop(j)*sin(teta(j))+zm

         enddo  !<------  j-loop

        enddo  ! i-loop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           i=2
           zps =psia(i)
           ps0=psip+zps*(psim-psip)

             do j=1,nt

         ttj=teta(j)
        ro2j=(ps0-psim)/( 0.5d0*dp(3)*cos(ttj)**2
     +                +     dp(4)*cos(ttj)*sin(ttj)
     +                +   0.5d0*dp(5)*sin(ttj)**2 )

        rodeb=ro2j

          rop(j)=dsqrt(ro2j)

             r(i,j)=rm+rop(j)*cos(teta(j))
             z(i,j)=zm+rop(j)*sin(teta(j))

             enddo  ! j-loop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if(ibeg.gt.3) then

         do j=2,nt1

          ros1=(r(2,j)-rm)**2+(z(2,j)-zm)**2
          ros2=(r(ibeg,j)-rm)**2+(z(ibeg,j)-zm)**2
          psa1=psia(2)
          psa2=psia(ibeg)

          do i=3,ibeg-1

           psai=psia(i)
           rosi=( (psa2-psai)*ros1+(psai-psa1)*ros2 )/(psa2-psa1)
           rosi=dsqrt(rosi)

           r(i,j)=rm+rosi*cos(teta(j))
           z(i,j)=zm+rosi*sin(teta(j))

          enddo

         enddo

        endif

         do i=2,iplas
          do j=2,nt1

            ro(i,j)=dsqrt((r(i,j)-rm)**2+(z(i,j)-zm)**2)

         enddo
          enddo




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do j=2,nt1

         spro=rob(j)-ro(iplas-1,j)

         droj=ro(iplas,j)-ro(iplas-1,j)
         !cpro=cpr(droj,npro,spro)
         row=ro(iplas,j)
         !dro=droj
         dro=(rob(j)-ro(iplas,j))/(nr-iplas)

        do 220 i=iplas+1,nr1

             !dro=dro*cpro
             row=row+dro

             rh(i)=rm+row*cos(teta(j))
             zh(i)=zm+row*sin(teta(j))

             roh(i)=row

              do is=2,nr1

            ps0 =psin(is,j)
            pspl=psin(is+1,j)

            ro0 =ro(is,j)
            ropl=ro(is+1,j)

 	     if(row.le.ropl .and. row.ge.ro0) then
		  isw=is 
		  go to 755
	     endif

              enddo

 755     continue       

           ro2=ropl
           ro1=ro0

           ps2=pspl
           ps1=ps0

           psia(i)=( ps1*(ro2-row)+ps2*(row-ro1) )/(ro2-ro1)

 220      continue

          do 250 i=iplas+1,nr1

            roerr=dabs(roh(i)-ro(i,j))
            erro=dmax1(erro,roerr)

           ro(i,j)=roh(i)

           r(i,j)=roh(i)*cos(teta(j))+rm
           z(i,j)=roh(i)*sin(teta(j))+zm

           psin(i,j)=psia(i)

 250      continue

            enddo

            do j=2,nt1

            ro(nr,j)=rob(j)
            !r(iplas,j)=rob(j)*cos(teta(j))+rm
            !z(iplas,j)=rob(j)*sin(teta(j))+zm

           ro(1,j)=0.d0
           r(1,j)=rm
           z(1,j)=zm

            enddo

          do 25 i=1,nr

           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

           roplt(i,1)=roplt(i,nt1)
           roplt(i,nt)=roplt(i,2)

 25       continue

           do 30 i=1,iplas
           do 30 j=1,nt
 
            psin(i,j)=psia(i)
 
  30       continue

           do i=1,nr
           do j=1,nt
 
            psiold(i,j)=psip+psin(i,j)*(psim-psip)
 
           enddo
           enddo
 
           call f_wrd

       return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine regrid0_(erro)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           !!include 'comlmtr.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         dimension ron(ntp),rop(ntp),rn(ntp),zn(ntp),tetn(ntp)
         dimension rob(ntp),teti(ntp),roi(ntp)
         dimension roplt(nrp,ntp)
         dimension dp(5)
         dimension roh(nrp),rh(nrp),zh(nrp)
         dimension psiold(nrp,ntp)
	common
     *  /c_kpr/kpr

!         xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1)

         cos(xx)=dcos(xx)
         sin(xx)=dsin(xx)
         atan(xx)=datan(xx)
         asin(xx)=dasin(xx)
         acos(xx)=dacos(xx)
         sqrt(xx)=dsqrt(xx)

          ! write(*,*) 'regrid0:::'

             nctrli=nctrl

             npro=nr-iplas+1

           if(ngav.eq.0) then
            alfa=1.0d0
           else
            alfa=1.0d0
           endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           if(itin.gt.1) then
            do i=1,nr1        
            do j=1,nt
		   psi(i,j)=alfa*psi(i,j)+(1.d0-alfa)*psiold(i,j)
            enddo		          
            enddo		          
           endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          rmold=rm
          zmold=zm

          if(kpr.eq.1) then
           write(*,*) 'rm,zm,psim',rm,zm,psim
          endif
             call axdef(rma,zma,psima,dp)

          if(kpr.eq.1) then
           write(*,*) 'rma,zma,psima',rma,zma,psima
          endif
	     rm=rma
	     zm=zma
	     psim=psima

             erro=dsqrt((rmold-rm)**2+(zmold-zm)**2)

          if(kpr.eq.1) then
          write(*,*) 'erro mag.axis',erro
          endif
           errpsi=0.d0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         rxold=rx0
         zxold=zx0

         call f_xpoint(rx1,zx1,ixp1,jxp1,psix1,tetx1,kodex1)

         call f_xpoint(rx2,zx2,ixp2,jxp2,psix2,tetx2,kodex2)

          if(kpr.eq.1) then
           write(*,*) 'kodex1,kodex2',kodex1,kodex2
          endif

          kodxp=kodex1*kodex2
       if(kodxp.ne.0) then

          if(kpr.eq.1) then
              write(*,*) ' all x-points out of box'
              write(*,*) ' only limiter case '
          endif
                  nctrli=1

           !write(6,*) ' program cannot run more '
           !stop
	     psip=-1.d12

	 else  !kodxp=0

         if(kodex1.eq.0 .AND. kodex2.eq.0) then
          if(psix1.ge.psix2) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
	    else
           psix0=psix2
           rx0=rx2
           zx0=zx2
          endif
         elseif(kodex1.eq.0) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
         elseif(kodex2.eq.0) then
           psix0=psix2
           rx0=rx2
           zx0=zx2
         endif

             errox=dsqrt((rxold-rx0)**2+(zxold-zx0)**2)
          if(kpr.eq.1) then
           write(*,*) 'erro x-point',errox
           write(*,*) 'rx zx',rx0,zx0,psix0
          endif

	     psip=psim-alp*(psim-psix0)

       endif

	   if(nctrl.eq.1) then
           numlim=0
          do llim=1,nblm

	      call fdefln(psiblm,rblm(llim),zblm(llim))
		  
           if(psiblm.gt.psip) then              
            psip=psiblm
            numlim=llim 
           endif
		 		                             
          enddo
         endif		                             
          if(kpr.eq.1) then
           write(*,*) 'numlim',numlim
          endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


         do i=1,nr
         do j=1,nt

	    psnn=psin(i,j)
          psin(i,j)=(psi(i,j)-psip)/(psim-psip)
		delpsn=dabs(psin(i,j)-psnn)

           if(delpsn.gt.errpsi) then

             ierm=i
             jerm=j
	       errpsi=delpsn

           endif

         enddo
         enddo

          if(kpr.eq.1) then
         write(*,*) 'i,j errpsi',ierm,jerm,errpsi,(psim-psip)
          endif
cc      definition of new angle grid teta(j)

         do j=1,nt
           tetn(j)=teta(j)
         enddo

        do j=1,nt

         drx=r(nr,j)-rm
         dzx=z(nr,j)-zm

         robj=sqrt(drx**2+dzx**2)
         rob(j)=robj
         tetp=acos(drx/robj)
          if(dzx.lt.0.d0) then
          teta(j)=-tetp
          else
          teta(j)=tetp
          endif

        enddo

            do j=2,nt  !<---

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+2.d0*pi
         endif

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+2.d0*pi
         endif

	      enddo      !<---

           teta(1)=teta(nt1)-2.d0*pi
           teta(nt)=teta(2)+2.d0*pi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           i=2

           zps =psia(i)
           ps0=psip+zps*(psim-psip)

             do j=1,nt

         ttj=teta(j)

        ro2j=(ps0-psim)/( 0.5d0*dp(3)*cos(ttj)**2
     +                +     dp(4)*cos(ttj)*sin(ttj)
     +                + 0.5d0*dp(5)*sin(ttj)**2 )

          rop(j)=dsqrt(ro2j)

             r(i,j)=rm+rop(j)*cos(teta(j))
             z(i,j)=zm+rop(j)*sin(teta(j))

             enddo  ! j-loop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ccc    grid moving along rais

          do i=3,iplas

           zps =psia(i)

          do j=1,nt     !<------ j-loop

              do is=1,nr1

           ps0 =psin(is,j)
!           !psmn=psin(is-1,j)
           pspl=psin(is+1,j)

           ro0 =ro(is,j)
!           !romn=ro(is-1,j)
           ropl=ro(is+1,j)

	              if(zps.le.ps0 .and. zps.ge.pspl) then
              zro=((pspl-zps)*ro0-(ps0-zps)*ropl)/(pspl-ps0)
                go to 822
	              endif
!            !zro=qvadin(zps, psmn,ps0,pspl, romn,ro0,ropl)
                 enddo
 822              continue



           ron(j)=zro

          enddo  !<------ j-loop

        do j=1,nt

            rnj=rmold+ron(j)*cos(tetn(j))
            znj=zmold+ron(j)*sin(tetn(j))

         drx=rnj-rm
         dzx=znj-zm

         roi(j)=sqrt(drx**2+dzx**2)
         tetp=acos(drx/roi(j))
          if(dzx.lt.0.d0) then
          teti(j)=-tetp
          else
          teti(j)=tetp
          endif

        enddo

            do j=2,nt  !<---

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+2.d0*pi
         endif

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+2.d0*pi
         endif

	      enddo      !<---

           teti(1)=teti(nt1)-2.d0*pi
           teti(nt)=teti(2)+2.d0*pi

           roi(1)=roi(nt1)
           roi(nt)=roi(2)

          do j=2,nt1 !<------ j-loop

              tetv=teta(j)

            if(tetv.lt.teti(1)) tetv=tetv+2.d0*pi
            if(tetv.lt.teti(1)) tetv=tetv+2.d0*pi
            if(tetv.gt.teti(nt)) tetv=tetv-2.d0*pi
            if(tetv.gt.teti(nt)) tetv=tetv-2.d0*pi

                do jj=1,nt1

           tt0 =teti(jj)
           !ttmn=teti(j-1)
           ttpl=teti(jj+1)

           ro0 =roi(jj)
           !romn=roi(j-1)
           ropl=roi(jj+1)

	            if(tetv.le.ttpl .and. tetv.ge.tt0) then
              zro=((ttpl-tetv)*ro0+(tetv-tt0)*ropl)/(ttpl-tt0)
               go to 1047
	            endif
                enddo

 1047      continue

           ! zro=qvadin(teta(j), ttmn,tt0,ttpl, romn,ro0,ropl)

		 rop(j)=zro

                    roerr=dabs(ron(j)-ro(i,j))

	              if(roerr.gt.erro) then
	                 erro=roerr
                      ierm=i
                      jerm=j
	              endif

             roplt(i,j)=ron(j)-ro(i,j)

           r(i,j)=rop(j)*cos(teta(j))+rm
           z(i,j)=rop(j)*sin(teta(j))+zm

          enddo  !<------  j-loop

          enddo  ! i-loop

          if(kpr.eq.1) then
         write(*,*) 'erro max.',erro
         write(*,*) 'i,j erro',ierm,jerm,erro
          endif

             do i=2,iplas
             do j=2,nt1

            ro(i,j)=dsqrt((r(i,j)-rm)**2+(z(i,j)-zm)**2)
            ronor(i,j)=ro(i,j)/rob(j)
            psin(i,j)=psia(i)

             enddo
             enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do j=2,nt1

         spro=rob(j)-ro(iplas-1,j)

         droj=ro(iplas,j)-ro(iplas-1,j)
         cpro=cpr(droj,npro,spro)
         row=ro(iplas,j)
         dro=droj

        do 220 i=iplas+1,nr1

             dro=dro*cpro
             row=row+dro

             rh(i)=rm+row*cos(teta(j))
             zh(i)=zm+row*sin(teta(j))

             roh(i)=row

              do is=2,nr1

            ps0 =psin(is,j)
            pspl=psin(is+1,j)

            ro0 =ro(is,j)
            ropl=ro(is+1,j)

 	     if(row.le.ropl .and. row.ge.ro0) then
		  isw=is 
		  go to 755
	     endif

              enddo

 755     continue       

           ro2=ropl
           ro1=ro0

           ps2=pspl
           ps1=ps0

           psia(i)=( ps1*(ro2-row)+ps2*(row-ro1) )/(ro2-ro1)

 220      continue

          do 250 i=iplas+1,nr1

            roerr=dabs(roh(i)-ro(i,j))
            erro=dmax1(erro,roerr)

           ro(i,j)=roh(i)

           r(i,j)=roh(i)*cos(teta(j))+rm
           z(i,j)=roh(i)*sin(teta(j))+zm

           psin(i,j)=psia(i)

 250      continue

            enddo

            do j=2,nt1
           ro(nr,j)=rob(j)
            !r(nr,j)=rob(j)*cos(teta(j))+rm
            !z(nr,j)=rob(j)*sin(teta(j))+zm
           ro(1,j)=0.d0
           r(1,j)=rm
           z(1,j)=zm
            enddo

           do i=1,nr
           do j=2,nt1
 
            ronor(i,j)=ro(i,j)/ro(iplas,j)
 
           enddo
           enddo


          do 25 i=1,nr

           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)

           ronor(i,1)=ronor(i,nt1)
           ronor(i,nt)=ronor(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

           roplt(i,1)=roplt(i,nt1)
           roplt(i,nt)=roplt(i,2)

 25       continue

           do 30 i=1,iplas
           do 30 j=1,nt
 
            psin(i,j)=psia(i)
 
  30       continue

           do i=1,nr
           do j=1,nt
 
            psiold(i,j)=psip+psin(i,j)*(psim-psip)
 
           enddo
           enddo

           !f_call wrd

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_regrid0(erro)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           !!include 'comlmtr.inc'
           include 'compol.inc'
           include 'compol_add.inc'

         dimension ron(ntp),rop(ntp),rn(ntp),zn(ntp),tetn(ntp)
         dimension rob(ntp),teti(ntp),roi(ntp)
         dimension roplt(nrp,ntp)
         dimension dp(5)
         dimension roh(nrp),rh(nrp),zh(nrp)
         dimension psiold(nrp,ntp)
	common
     *  /c_kpr/kpr

!        save psiold
        real*8 psiold
!         xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1)

         cos(xx)=dcos(xx)
         sin(xx)=dsin(xx)
         atan(xx)=datan(xx)
         asin(xx)=dasin(xx)
         acos(xx)=dacos(xx)
         sqrt(xx)=dsqrt(xx)


             nctrli=nctrl

             npro=nr-iplas+1

           if(ngav.eq.0) then
            alfa=1.0d0
           else
            alfa=0.75d0
           endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           if(itin.gt.1) then
              wgt=0.5d0
            do i=1,iplas        
            do j=1,nt
		     !psi(i,j)=wgt*psi(i,j)+(1.d0-wgt)*psiold(i,j)
            enddo		          
            enddo		          
           endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          rmold=rm
          zmold=zm

          if(kpr.eq.1) then
           write(*,*) 'rm,zm,psim',rm,zm,psim
          endif

             call axdef(rma,zma,psima,dp)

          if(kpr.eq.1) then
           write(*,*) 'rma,zma,psima',rma,zma,psima
          endif

	     rm=rma
	     zm=zma
	     psim=psima

             erro=dsqrt((rmold-rm)**2+(zmold-zm)**2)

          if(kpr.eq.1) then
          write(*,*) 'erro mag.axis',erro
          endif

           errpsi=0.d0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         rxold=rx0
         zxold=zx0

         call f_xpoint(rx1,zx1,ixp1,jxp1,psix1,tetx1,kodex1)

         call f_xpoint(rx2,zx2,ixp2,jxp2,psix2,tetx2,kodex2)

          if(kpr.eq.1) then
           write(*,*) 'kodex1,kodex2',kodex1,kodex2
          endif

          kodxp=kodex1*kodex2
       if(kodxp.ne.0) then

          if(kpr.eq.1) then
              write(*,*) ' all x-points out of box'
              write(*,*) ' only limiter case '
          endif

                  nctrli=1

           !write(6,*) ' program cannot run more '
           !stop
	     psip=-1.d12

	 else  !kodxp=0
           psipn=psip

         if(kodex1.eq.0 .AND. kodex2.eq.0) then
          if(psix1.ge.psix2) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
	    else
           psix0=psix2
           rx0=rx2
           zx0=zx2
          endif
         elseif(kodex1.eq.0) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
         elseif(kodex2.eq.0) then
           psix0=psix2
           rx0=rx2
           zx0=zx2
         endif

             errox=dsqrt((rxold-rx0)**2+(zxold-zx0)**2)
          if(kpr.eq.1) then
           write(*,*) 'erro x-point',errox
           write(*,*) 'rx zx',rx0,zx0,psix0
          endif

	     psip=psim-alp*(psim-psix0)

          if(itin.gt.1) then
           zwgt=1.0d0
	     psipr=zwgt*psip+(1.d0-zwgt)*psipn
	     psip=dmax1(psip,psipr)
           alp_pr=(psim-psip)/(psim-psix0)
          endif

       endif

	   if(nctrl.eq.1) then
           numlim=0
          do llim=1,nblm

	      call fdefln(psiblm,rblm(llim),zblm(llim))
		  
           if(psiblm.gt.psip) then              
            psip=psiblm
            numlim=llim 
           endif
		 		                             
          enddo
         endif		                             
          if(kpr.eq.1) then
           write(*,*) 'numlim',numlim
          endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


         do i=1,nr
         do j=1,nt

	    psnn=psin(i,j)
          psin(i,j)=(psi(i,j)-psip)/(psim-psip)
		delpsn=dabs(psin(i,j)-psnn)

           if(delpsn.gt.errpsi) then

             ierm=i
             jerm=j
	       errpsi=delpsn

           endif

         enddo
         enddo

          if(kpr.eq.1) then
         write(*,*) 'i,j errpsi',ierm,jerm,errpsi,(psim-psip)
          endif

cc      definition of new angle grid teta(j)

         do j=1,nt
           tetn(j)=teta(j)
         enddo

        do j=1,nt

         drx=r(nr,j)-rm
         dzx=z(nr,j)-zm

         robj=sqrt(drx**2+dzx**2)
         rob(j)=robj
         tetp=acos(drx/robj)
          if(dzx.lt.0.d0) then
          teta(j)=-tetp
          else
          teta(j)=tetp
          endif

        enddo

            do j=2,nt  !<---

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+2.d0*pi
         endif

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+2.d0*pi
         endif

	      enddo      !<---

           teta(1)=teta(nt1)-2.d0*pi
           teta(nt)=teta(2)+2.d0*pi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           i=2

           zps =psia(i)
           ps0=psip+zps*(psim-psip)

             do j=1,nt

         ttj=teta(j)

        ro2j=(ps0-psim)/( 0.5d0*dp(3)*cos(ttj)**2
     +                +     dp(4)*cos(ttj)*sin(ttj)
     +                + 0.5d0*dp(5)*sin(ttj)**2 )

          rop(j)=dsqrt(ro2j)

             r(i,j)=rm+rop(j)*cos(teta(j))
             z(i,j)=zm+rop(j)*sin(teta(j))

             enddo  ! j-loop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ccc    grid moving along rais

          do i=3,iplas

           zps =psia(i)

          do j=1,nt     !<------ j-loop

              do is=1,nr1

           ps0 =psin(is,j)
!           !psmn=psin(is-1,j)
           pspl=psin(is+1,j)

           ro0 =ro(is,j)
!           !romn=ro(is-1,j)
           ropl=ro(is+1,j)

	              if(zps.le.ps0 .and. zps.ge.pspl) then
              zro=((pspl-zps)*ro0-(ps0-zps)*ropl)/(pspl-ps0)
                go to 822
	              endif
!            !zro=qvadin(zps, psmn,ps0,pspl, romn,ro0,ropl)
                 enddo
 822              continue



           ron(j)=zro

          enddo  !<------ j-loop

        do j=1,nt

            rnj=rmold+ron(j)*cos(tetn(j))
            znj=zmold+ron(j)*sin(tetn(j))

         drx=rnj-rm
         dzx=znj-zm

         roi(j)=sqrt(drx**2+dzx**2)
         tetp=acos(drx/roi(j))
          if(dzx.lt.0.d0) then
          teti(j)=-tetp
          else
          teti(j)=tetp
          endif

        enddo

            do j=2,nt  !<---

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+2.d0*pi
         endif

         if(teti(j).lt.teti(j-1)) then
            teti(j)=teti(j)+2.d0*pi
         endif

	      enddo      !<---

           teti(1)=teti(nt1)-2.d0*pi
           teti(nt)=teti(2)+2.d0*pi

           roi(1)=roi(nt1)
           roi(nt)=roi(2)

          do j=2,nt1 !<------ j-loop

              tetv=teta(j)

            if(tetv.lt.teti(1)) tetv=tetv+2.d0*pi
            if(tetv.lt.teti(1)) tetv=tetv+2.d0*pi
            if(tetv.gt.teti(nt)) tetv=tetv-2.d0*pi
            if(tetv.gt.teti(nt)) tetv=tetv-2.d0*pi

                do jj=1,nt1

           tt0 =teti(jj)
           !ttmn=teti(j-1)
           ttpl=teti(jj+1)

           ro0 =roi(jj)
           !romn=roi(j-1)
           ropl=roi(jj+1)

	            if(tetv.le.ttpl .and. tetv.ge.tt0) then
              zro=((ttpl-tetv)*ro0+(tetv-tt0)*ropl)/(ttpl-tt0)
               go to 1047
	            endif
                enddo

 1047      continue

           ! zro=qvadin(teta(j), ttmn,tt0,ttpl, romn,ro0,ropl)

		 rop(j)=zro

                    roerr=dabs(ron(j)-ro(i,j))

	              if(roerr.gt.erro) then
	                 erro=roerr
                      ierm=i
                      jerm=j
	              endif

             roplt(i,j)=ron(j)-ro(i,j)

           r(i,j)=rop(j)*cos(teta(j))+rm
           z(i,j)=rop(j)*sin(teta(j))+zm

          enddo  !<------  j-loop

          enddo  ! i-loop

          if(kpr.eq.1) then
         write(6,*) 'erro max.',erro
         write(6,*) 'i,j erro',ierm,jerm,erro
          endif

             do i=2,iplas
             do j=2,nt1

            ro(i,j)=dsqrt((r(i,j)-rm)**2+(z(i,j)-zm)**2)
            ronor(i,j)=ro(i,j)/rob(j)
            psin(i,j)=psia(i)

             enddo
             enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do j=2,nt1

         spro=rob(j)-ro(iplas-1,j)

         droj=ro(iplas,j)-ro(iplas-1,j)
         !cpro=cpr(droj,npro,spro)
         row=ro(iplas,j)
         dro=(rob(j)-ro(iplas,j))/(nr-iplas)

        do 220 i=iplas+1,nr1

             !dro=dro*cpro
             row=row+dro

             rh(i)=rm+row*cos(teta(j))
             zh(i)=zm+row*sin(teta(j))

             roh(i)=row

              do is=2,nr1

            ps0 =psin(is,j)
            pspl=psin(is+1,j)

            ro0 =ro(is,j)
            ropl=ro(is+1,j)

 	     if(row.le.ropl .and. row.ge.ro0) then
		  isw=is 
		  go to 755
	     endif

              enddo

 755     continue       

           ro2=ropl
           ro1=ro0

           ps2=pspl
           ps1=ps0

           psia(i)=( ps1*(ro2-row)+ps2*(row-ro1) )/(ro2-ro1)

 220      continue

          do 250 i=iplas+1,nr1

            roerr=dabs(roh(i)-ro(i,j))
            erro=dmax1(erro,roerr)

           ro(i,j)=roh(i)

           r(i,j)=roh(i)*cos(teta(j))+rm
           z(i,j)=roh(i)*sin(teta(j))+zm

           psin(i,j)=psia(i)

 250      continue

            enddo

            do j=2,nt1
           ro(nr,j)=rob(j)
            !r(nr,j)=rob(j)*cos(teta(j))+rm
            !z(nr,j)=rob(j)*sin(teta(j))+zm
           ro(1,j)=0.d0
           r(1,j)=rm
           z(1,j)=zm
            enddo

           do i=1,nr
           do j=2,nt1
 
            ronor(i,j)=ro(i,j)/ro(iplas,j)
 
           enddo
           enddo


          do 25 i=1,nr

           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)

           ronor(i,1)=ronor(i,nt1)
           ronor(i,nt)=ronor(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

           roplt(i,1)=roplt(i,nt1)
           roplt(i,nt)=roplt(i,2)

 25       continue

           do 30 i=1,iplas
           do 30 j=1,nt
 
            psin(i,j)=psia(i)
 
  30       continue

           do i=1,nr
           do j=1,nt
 
            psiold(i,j)=psip+psin(i,j)*(psim-psip)
 
           enddo
           enddo

           !f_call wrd

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine grid_spdr

           include 'double.inc'

        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'parrc1.inc'
           include 'comrec.inc'
           include 'compol_add.inc'
	common
     *  /c_kpr/kpr

            cos(xx)=dcos(xx)
            sin(xx)=dsin(xx)
            sqrt(xx)=dsqrt(xx)


             !call rdfb

            rm=r(1,2)
            zm=z(1,2)

          do i=iplas,1,-1      
            if(psia(i).ge.0.05d0) then
             i_bc=i
             exit
            endif
          enddo

            do j=2,nt1
             ronor(nr,j)=1.57d0
             ro(nr,j)=ro(i_bc,j)*ronor(nr,j)
            enddo

            !do i=2,iplas
            !do j=2,nt1
            ! ro(i,j)=ro(iplas,j)*ronor(i,j)
            !enddo
            !enddo

            do i=iplas+1,nr-1
            do j=2,nt1
             ropla=ro(iplas,j)
             robon=ro(nr,j)
             delro=(robon-ropla)/(nr-iplas)
             ro(i,j)=ropla+delro*(i-iplas)
             ronor(i,j)=ro(i,j)/ropla
            enddo
            enddo

            do i=iplas+1,nr
            do j=2,nt1
             r(i,j)=ro(i,j)*cos(teta(j))+rm
             z(i,j)=ro(i,j)*sin(teta(j))+zm
            enddo
            enddo

          do i=1,nr

           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)

           ronor(i,1)=ronor(i,nt1)
           ronor(i,nt)=ronor(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

          enddo


         rb_max=rm
         rb_min=rm
         zb_max=zm
         zb_min=zm

         do j=2,nt1
          if(r(nr,j).gt.rb_max) rb_max=r(nr,j)
          if(r(nr,j).lt.rb_min) rb_min=r(nr,j)
          if(z(nr,j).gt.zb_max) zb_max=z(nr,j)
          if(z(nr,j).lt.zb_min) zb_min=z(nr,j)
         enddo

         if(rb_max.gt.xmax
     *         .OR.
     *      zb_max.gt.ymax
     *         .OR.
     *      rb_min.lt.xmin 
     *         .OR. 
     *      zb_min.lt.ymin ) then 
        
          if(kpr.eq.1) then
          write(*,*) 'egg is large then rectangular box' 
          call f_wrd
          pause 'pause '
          endif

         endif

              rx1=xx1  !   x-points from rect. grid
              zx1=yx1  !
                       !
              rx2=xx2  !
              zx2=yx2  !
                       !
              rx0=xx0  !
              zx0=yx0  !

              ixp1=0
              jxp1=0

              ixp2=0
              jxp2=0

              psim=um  
              psip=up  
         ! call f_wrd

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_grid(igdf,nstep)

           include 'double.inc'

        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           parameter(nshp=10)
           parameter(ntp4=ntp+4,ntp6=ntp4*6)
           include 'parrc1.inc'

           include 'compol.inc'
           include 'compol_add.inc'
           include 'comrec.inc'
         common /com_sp3/ RRKsp(ntp4),CCKsp(ntp4)

          dimension xs(nshp),ys(nshp),fun(nshp),dp(5)
            real*8 ron(ntp)
	common
     *  /c_kpr/kpr

            frbon(r0,as,tr,tet,skv)=
     *            r0+(r0/as)*dcos(tet+tr*dsin(tet)-skv*dsin(2.d0*tet))
            fzbon(r0,z0,as,el,tet)=z0+(r0/as)*el*dsin(tet)

            cos(xx)=dcos(xx)
            sin(xx)=dsin(xx)
            sqrt(xx)=dsqrt(xx)

            if(nstep.eq.0) then
         if(igdf.eq.0)then

	        do i=1,iplas
           psia(i)=(iplas-i)/(iplas-1.d0)
              enddo

	        do i=1,iplas1
	     dpsda(i)=-1.d0
              enddo

         elseif(igdf.eq.1 .OR. igdf.eq.2)then

	        do i=1,iplas
           psia(i)=1.d0-((i-1)/(iplas-1.d0))**2
              enddo

	        do i=1,iplas1
           dpsda(i)=(1-2*i)/(iplas-1.d0)
              enddo
           
         endif
         endif

                dtet=2.d0*pi/(nt-2)

                teta(1)=-dtet

            do 30 j=2,nt

             teta(j)=teta(j-1)+dtet

 30         continue

            teta(1)=teta(nt1)-2.d0*pi
            teta(nt)=teta(2)+2.d0*pi

              rm=xm    !
              zm=ym    !  position of magn. axis
                       !
              rx1=xx1  !  and x-points from rect. grid
              zx1=yx1  !
                       !
              rx2=xx2  !
              zx2=yx2  !
                       !
              rx0=xx0  !
              zx0=yx0  !

              ixp1=0
              jxp1=0

              ixp2=0
              jxp2=0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        write(fname,'(a,a)') path(1:kname),'shegg.dat'
        open(1,file=fname)
            !open(1,file='shegg.dat')

              read(1,*) rc0
              read(1,*) zc0

              read(1,*) asp0

              read(1,*) el_up
              read(1,*) tr_up

              read(1,*) el_lw
              read(1,*) tr_lw

              read(1,*) skv

            close(1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           do j=1,nt
            tet=teta(j)
	   tri=0.5d0*(tr_up+tr_lw+(tr_up-tr_lw)*sin(tet))
	   ell=0.5d0*(el_up+el_lw+(el_up-el_lw)*sin(tet))
            rrr=frbon(rc0,asp0,tri,tet,skv)
            r(nr,j)=rrr
            zzz=fzbon(rc0,zc0,asp0,ell,tet)
            z(nr,j)=zzz
           enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do j=1,nt
         drx=r(nr,j)-rm
         dzx=z(nr,j)-zm
         teta(j)=atan(dzx/drx)
        enddo

            do j=2,nt  !<---

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+pi
         endif

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+pi
         endif
            
	      enddo      !<---

           teta(1)=teta(nt1)-2.d0*pi
           teta(nt)=teta(2)+2.d0*pi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


          nsh=1
          xs(nsh)=x(imax)
          ys(nsh)=y(jmax)
         fun(nsh)=u(imax,jmax)

        do k=-1,1

          ii= imax+k

        do l=-1,1

          jj= jmax+l

          if(ii.ne.imax .OR. jj.ne.jmax) then
          nsh=nsh+1
          xs(nsh)=x(ii)
          ys(nsh)=y(jj)
         fun(nsh)=u(ii,jj)
	    endif

        enddo
        enddo

          call deriv5(xs,ys,fun,nsh,5,dp)

           !write(6,*) dp


	     stpx=(x(imax+1)-x(imax))
	     stpy=(y(jmax+1)-y(jmax))

            do i=2,iplas

             u0=psia(i)
           ur0=up+u0*(um-up) 

             do j=1,nt 

         ttj=teta(j) 
        ro2j=(ur0-um)/( 0.5d0*dp(3)*cos(ttj)**2
     +                +     dp(4)*cos(ttj)*sin(ttj)
     +                + 0.5d0*dp(5)*sin(ttj)**2 )

          ron(j)=sqrt(ro2j)     


             r(i,j)=rm+ron(j)*cos(teta(j))
             z(i,j)=zm+ron(j)*sin(teta(j))

             ro(i,j)=ron(j)

             psin(i,j)=u0

             enddo  ! j-loop

	     ibeg=i+1

          if(kpr.eq.1) then
	 write(6,*) 'ro(1)',ro(i,1),ro(i,2),stpx,stpy
          endif

	   if( ron(2) .GT. dmax1(stpx,stpy) ) go to 2799

             enddo   !i-loop

 2799     continue

C                 pause 'pausew'

            do 100 i=ibeg,iplas

             u0=psia(i)

            call f_loop95(teta,nt,ron,u0)

        ! write(6,*) 'loop95 i=',i,u0

            do 110 j=1,nt

             r(i,j)=rm+ron(j)*cos(teta(j))
             z(i,j)=zm+ron(j)*sin(teta(j))

             ro(i,j)=ron(j)

             psin(i,j)=u0

 110        continue
 100        continue

            do 120 j=1,nt

             r(1,j)=rm
             z(1,j)=zm
             ro(1,j)=0.
             psin(1,j)=1.d0

 120        continue

             npro=nr-iplas+1

        do 200 j=1,nt

         droj=sqrt( (r(iplas,j)-r(iplas-1,j))**2+
     +              (z(iplas,j)-z(iplas-1,j))**2  )
           delbn=sqrt( (r(nr,j)-r(iplas,j))**2+
     +                 (z(nr,j)-z(iplas,j))**2  )

         cpro=cpr(droj,npro,delbn+droj)
         row=ron(j)
         dro=droj

        do 220 i=iplas+1,nr

             dro=dro*cpro
             row=row+dro

             r(i,j)=rm+row*cos(teta(j))
             z(i,j)=zm+row*sin(teta(j))

             ro(i,j)=row

             psin(i,j)=0.d0

 220    continue
 200    continue


        do 333 i=1,nr

             r(i,1)=r(i,nt1)
             z(i,1)=z(i,nt1)
             ro(i,1)=ro(i,nt1)

             r(i,nt)=r(i,2)
             z(i,nt)=z(i,2)
             ro(i,nt)=ro(i,2)

 333    continue

            teta(1)=teta(nt1)-2.d0*pi
            teta(nt)=teta(2)+2.d0*pi

                     psip=up

             call bonspl
	!pause 'bonspl'

              call f_wrd
              !stop
C	 pause 'wrd'

          return
          end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine ada(erro)

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
         parameter(ntp4=ntp+4,ntp6=ntp4*6)
           !!include 'comlmtr.inc'
         common /com_sp3/ RRKsp(ntp4),CCKsp(ntp4)
           include 'compol.inc'
           include 'compol_add.inc'

         dimension ron(nrp),rn(nrp),zn(nrp)
	   real*8 cwksp(4)
	common
     *  /c_kpr/kpr

         xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1)

         cos(xx)=dcos(xx)
         sin(xx)=dsin(xx)

             nctrli=nctrl
             npro=nr-iplas+1
             erro=0.d0

          psim=psi(1,1)

         call f_xpoint(rx1,zx1,ixp1,jxp1,psix1,tetx1,kodex1)

          if(kpr.eq.1) then
          write(*,*) 'kodex1',kodex1
          write(*,*) 'rx1 zx1',rx1,zx1,psix1
          endif

         call f_xpoint(rx2,zx2,ixp2,jxp2,psix2,tetx2,kodex2)

          if(kpr.eq.1) then
          write(*,*) 'kodex2',kodex2
          write(*,*) 'rx2 zx2',rx2,zx2,psix2
          endif

          kodxp=kodex1*kodex2
       if(kodxp.ne.0) then

          if(kpr.eq.1) then
              write(*,*) ' all x-points out of box'
              write(*,*) ' only limiter case '
          endif

                  nctrli=1

           !write(*,*) ' program cannot run more '
           !stop
	     psip=-1.d12

	 else  !kodxp=0

         if(kodex1.eq.0 .AND. kodex2.eq.0) then
	     if(psix1.gt.psix2) then
            psix0=psix1
            rx0=rx1
            zx0=zx1
	     else
            psix0=psix2
            rx0=rx2
            zx0=zx2
           endif
         elseif(kodex1.eq.0) then
           psix0=psix1
           rx0=rx1
           zx0=zx1
         elseif(kodex2.eq.0) then
           psix0=psix2
           rx0=rx2
           zx0=zx2
         endif

          if(kpr.eq.1) then
          write(*,*) 'kodex1,kodex2',kodex1,kodex2
          write(*,*) 'rx zx',rx0,zx0,psix0
          endif


	   if(ngav/10*10.eq.ngav) then
          psip=psim-alp*(psim-psix0)
	   else
	    psip=psi(iplas,jrolim)
	   endif

       endif

            numlim=0 
	   if(nctrl.eq.1) then
          do llim=1,nblm
	      call fdefln(psiblm,rblm(llim),zblm(llim))   
           if(psiblm.gt.psip ) then              
            psip=psiblm
            numlim=llim 
           endif		                             
          enddo
         endif		                             

        alpnew=(psim-psip)/(psim-psix0)

        do  i=1,nr
        do  j=1,nt

         psin(i,j)=(psi(i,j)-psip)/(psim-psip)

        enddo
        enddo
        
        write(*,*)'ada:'
        write(*,*)'ada:nctrli,nctrl',nctrli,nctrl
        write(*,*)'ada:alpnew,numlim',alpnew,numlim

           ron(1)=0.d0

          do 20 j=1,nt

          do 200 i=2,iplas

           zps =psia(i)

	  do is=1,nr1
 	  
            ps0 =psin(is,j)
            pspl=psin(is+1,j)

            ro0 =ro(is,j)
            ropl=ro(is+1,j)

 	     if(zps.le.ps0 .and. zps.ge.pspl) then
		  isc=is 
		  go to 822
	     endif

        enddo

 822    continue
           if(isc.eq.1) then
            grad=-(pspl-ps0)/(ropl-ro0)
	     else
            psmn=psin(isc-1,j)
            romn=ro(isc-1,j)
            gradpl=-(pspl-ps0)/(ropl-ro0)
            gradmn=-(ps0-psmn)/(ro0-romn)
            grad=dmax1(gradpl,gradmn)
	    endif

            grad=-(pspl-ps0)/(ropl-ro0)

	     zro=ro0-(zps-ps0)/grad
           ron(i)=zro

           !ps0 =psin(i,j)
           !psmn=psin(i-1,j)
          ! pspl=psin(i+1,j)

          ! ro0 =ro(i,j)
          ! romn=ro(i-1,j)
          ! ropl=ro(i+1,j)

          ! zro=qvadin(zps, psmn,ps0,pspl, romn,ro0,ropl)

            if(ngav.eq.0) then
             alfa=0.75d0
            else
             alfa=0.550d0
            endif
            ron(i)=alfa*zro+(1.d0-alfa)*ro(i,j)


           if(ron(i).lt.ron(i-1)) then
            ron(i)=ron(i-1)+1.d-8
          if(kpr.eq.1) then
            write(*,*) 'grid crash i,j',i,j
            pause ' '        
          endif
           endif

 200      continue

           if(ngav.gt.0 .AnD. j.eq.jrolim .AnD. kpr.eq.1) then
           write(*,*) 'ro(iplas),rolim',ron(iplas),rolim
           endif

         spro=ro(nr,j)-ron(iplas-1)

         droj=ron(iplas)-ron(iplas-1)
         !cpro=cpr(droj,npro,spro)
         row=ron(iplas)
         !dro=droj
         dro=(ro(nr,j)-row)/(nr-iplas)

        do 220 i=iplas+1,nr1

             !dro=dro*cpro
             row=row+dro

             rn(i)=rm+row*cos(teta(j))
             zn(i)=zm+row*sin(teta(j))

             ron(i)=row



              do is=isc,nr1

            ps0 =psin(is,j)
            pspl=psin(is+1,j)

            ro0 =ro(is,j)
            ropl=ro(is+1,j)

 	     if(row.le.ropl .and. row.ge.ro0) then
		  isw=is 
		  go to 755
	     endif

              enddo

 755     continue       

           ro2=ropl
           ro1=ro0

           ps2=pspl
           ps1=ps0


           psia(i)=( ps1*(ro2-row)+ps2*(row-ro1) )/(ro2-ro1)

 220      continue

          do 250 i=2,nr1

            roerr=dabs(ron(i)-ro(i,j))
            erro=dmax1(erro,roerr)

           ro(i,j)=ron(i)

           r(i,j)=ron(i)*cos(teta(j))+rm
           z(i,j)=ron(i)*sin(teta(j))+zm

           psin(i,j)=psia(i)

 250      continue

 20       continue




           teta(1)=teta(nt1)-2.d0*pi
           teta(nt)=teta(2)+2.d0*pi

          do i=1,nr
           ro(i,1)=ro(i,nt1)
           ro(i,nt)=ro(i,2)
           enddo

           do i=1,nr
           do j=2,nt1
 
            ronor(i,j)=ro(i,j)/ro(iplas,j)
 
           enddo
           enddo


          do 25 i=1,nr

           ronor(i,1)=ronor(i,nt1)
           ronor(i,nt)=ronor(i,2)

           r(i,1)=r(i,nt1)
           r(i,nt)=r(i,2)

           z(i,1)=z(i,nt1)
           z(i,nt)=z(i,2)

           psin(i,1)=psin(i,nt1)
           psin(i,nt)=psin(i,2)

 25       continue


          do 30 i=1,nr
          do 30 j=1,nt

           psi(i,j)=psip+psin(i,j)*(psim-psip)

 30       continue

            !call f_wrd
	     ! stop

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine reform

           include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
           include 'dim.inc'
           include 'compol.inc'
           include 'compol_add.inc'

           dimension rob(ntp)

           sqrt(xx)=dsqrt(xx)
           atan(xx)=datan(xx)
           cos(xx)=dcos(xx)
           sin(xx)=dsin(xx)

            do 100 j=1,nt

         drx=r(nr,j)-rm
         dzx=z(nr,j)-zm

         rob(j)=sqrt(drx**2+dzx**2)


           tetp=dacos(drx/rob(j))
          if(dzx.lt.0.d0) then
           tet0=2.d0*pi-tetp
          else
           tet0=tetp
          endif

         teta(j)=tet0

 100        continue

            do 110 j=2,nt

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+pi
         endif

         if(teta(j).lt.teta(j-1)) then
            teta(j)=teta(j)+pi
         endif
 110        continue

           teta(1)=teta(nt1)-2.d0*pi
           teta(nt)=teta(2)+2.d0*pi


            do 200 j=1,nt
            do 200 i=1,nr

         ro(i,j)=ro(i,j)*rob(j)/ro(nr,j)
         r(i,j)=rm+ro(i,j)*cos(teta(j))
         z(i,j)=zm+ro(i,j)*sin(teta(j))

 200        continue

         if(ngav/10*10.ne.ngav) then
          ro(iplas,jrolim)=rolim
          r(iplas,jrolim)=rm+ro(iplas,jrolim)*cos(teta(jrolim))
          z(iplas,jrolim)=zm+ro(iplas,jrolim)*sin(teta(jrolim))
         endif

         return
         end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine fdefln(psidf,rgv,zgv)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

          sqrt(arg)=dsqrt(arg)

             psidf=0.d0

          dr0=rgv-rm
          dz0=zgv-zm

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

          rob1=ro(nr,jc)
          rob2=ro(nr,jc+1)

          ropl1=ro(iplas,jc)
          ropl2=ro(iplas,jc+1)

          tet1=teta(jc)
          tet2=teta(jc+1)

         rob12=(rob1*(tet2-tet0)+rob2*(tet0-tet1))/(tet2-tet1)
         ropl12=(ropl1*(tet2-tet0)+ropl2*(tet0-tet1))/(tet2-tet1)

          if(ro0.lt.rob12) then !!!

         do i=nr,2,-1

          ro1=ro(i,jc)
          ro2=ro(i,jc+1)

          ro12=(ro1*(tet2-tet0)+ro2*(tet0-tet1))/(tet2-tet1)

          if(ro0.lt.ro12) ic=i-1

         enddo

          i_rlmp=1

        if(ic.gt.iplas) then
         do i=ic+1,iplas-1,-1
          der_psi=psi(i,jc)-psi(i-1,jc)       
           if(der_psi.gt.0.d0) then
            i_rlmp=0
            psidf=-1.0d12
           endif
         enddo
        endif

        if(i_rlmp.eq.1) then

          ro1=ro(ic,jc)
          ro2=ro(ic+1,jc)
          ro3=ro(ic+1,jc+1)
          ro4=ro(ic,jc+1)

          u1=psi(ic,jc)
          u2=psi(ic+1,jc)
          u3=psi(ic+1,jc+1)
          u4=psi(ic,jc+1)

          psidf=blin_tr(tet0,ro0,
     *                      tet1,tet2,ro1,ro2,ro3,ro4,u1,u2,u3,u4)
        endif

          else !!!!!!!!!!!!!!!!!!!!!

            psidf=-1.0d12
           return
		  
             rr=rgv
             zz=zgv

          do 110 j=2,nt1

        r0=r(nr,j)
        z0=z(nr,j)

        r1=r(nr,j+1)
        z1=z(nr,j+1)

         call bint(rr,zz,R0,Z0,r1,z1,Fint,1)

         psidf=psidf-Fint*(dgdn(j)+dgdn(j+1))*0.5d0

           !write(6,*) 'flux:'
           ! write(6,*) 'fint j:',fint
           ! write(6,*) 'dgdn:',dgdn(j)
           !write(6,*) 'psitok:',psitok(ik)

 110       continue

          endif !!!!!!!!!!!!!!!!!!!!

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine bonspl

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         parameter(ntp4=ntp+4,ntp6=ntp4*6)
         common /com_sp3/ RRKsp(ntp4),CCKsp(ntp4)
         include 'compol.inc'
         include 'compol_add.inc'
         real*8 WRK(ntp6)
         real*8 robb(ntp)

	!pause 'bonspl enter'

           do j=1,nt
           robb(j)=ro(nr,j)	      
           enddo 

        CALL E01BAF(nt,teta,robb,RRKsp,CCKsp,nt+4,WRK,6*nt+16,IFAIL)

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine f_loop95(tetpol,ntet,ro0,u0)

         include 'double.inc'
         include 'parrc1.inc'
         include 'comrec.inc'

         dimension rxb(nbndp2),zxb(nbndp2)

         real*8 ut(nip,njp)

         real*8 roxb(nbndp2),tetxb(nbndp2)

         real*8 RRK(nbndp4),CCK(nbndp4),WRK(nbndp6)

         real*8 CWK(4),tetpol(1),ro0(1)

         xzer(x1,x2,v1,v2) = (x1*v2-x2*v1)/(v2-v1)

         ! write(6,*) '***loop95:enter'

        ! write(6,*) 'loop95:imax,jmax',imax,jmax
        !write(6,*) 'loop95:rmax,zmax',rm,zm

          pi=3.14159265358d0

         imax1=imax-1
         jmax1=jmax-1

         do 10 i=imax1,imax
          if(xm.le.x(i+1) .AND. xm.gt.x(i)) icell=i
 10      continue

         do 20 j=jmax1,jmax
          if(ym.le.y(j+1) .AND. ym.gt.y(j)) jcell=j
 20      continue

        ! write(6,*) 'loop95:icell,jcell',icell,jcell

         i=icell
         j=jcell+1

cccc      u0=0.5
         do 15 ii=1,ni
         do 15 jj=1,nj
          ut(ii,jj)=un(ii,jj)-u0
 15      continue

         ig=1

 888         continue

                do 885 i=imax,ni1

          !   write(6,*) 'un(i,j)',un(i,j),un(i+1,j)

             if(ut(i,j)*ut(i+1,j).le.0.) then

              ic=i
              jc=j

        rxb(ig)=xzer(x(i+1),x(i),ut(i+1,j),ut(i,j))
        zxb(ig)=y(j)

              go to 886
             endif

 885            continue
          write(6,*) 'loop95: first point was not find'
            stop

 886            continue

              ic1=ic
              jc1=jc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          lin=1

 100      ig=ig+1

          i=ic
          j=jc

             if(ut(i+1,j)*ut(i,j).le.0. .AND. lin.ne.1) then

              ic=i
              jc=j-1

              lin=3

              rxb(ig)=xzer(x(i+1),x(i),ut(i+1,j),ut(i,j))
              zxb(ig)=y(j)

             elseif(ut(i+1,j+1)*ut(i+1,j).le.0..AND. lin.ne.2) then

              ic=i+1
              jc=j

              lin=4

              rxb(ig)=x(i+1)
              zxb(ig)=xzer(y(j+1),y(j),ut(i+1,j+1),ut(i+1,j))

             elseif(ut(i+1,j+1)*ut(i,j+1).le.0. .AND. lin.ne.3) then

              ic=i
              jc=j+1

              lin=1

              rxb(ig)=xzer(x(i+1),x(i),ut(i+1,j+1),ut(i,j+1))
              zxb(ig)=y(j+1)

             elseif(ut(i,j+1)*ut(i,j).le.0..AND. lin.ne.4) then

              ic=i-1
              jc=j

              lin=2

              rxb(ig)=x(i)
              zxb(ig)=xzer(y(j+1),y(j),ut(i,j+1),ut(i,j))

             endif

c        write(6,*) 'loop:ic,jc',ic,jc,ig

            if(jc.eq.jc1 .AND. ic.eq.ic1) then

              ig=ig+1

              rxb(ig)=rxb(2 )
              zxb(ig)=zxb(2 )

              go to 1212

            endif

             go to 100

 1212        nxb=ig

            do 200 ig=1,nxb

         drx=rxb(ig)-xm
         dzx=zxb(ig)-ym

         tetp=datan(dzx/drx)

       if(ig.ne.1) then
         if(drx.lt.0.d0) tetp=tetp+pi
         deltet=tetp-tetxb(ig-1)
         if(deltet.lt.0.d0) tetp=tetp+2.d0*pi
       endif

         tetxb(ig)=tetp

         roxb(ig)=dsqrt(drx**2+dzx**2)

c         write(6,*) 'ig,ro,tet',ig,roxb(ig),tetxb(ig)

 200        continue


        CALL E01BAF(Nxb,tetxb,roxb,RRK,CCK,nxb+4,WRK,6*nxb+16,IFAIL)

c       write(6,*) 'ifail=',ifail

        DO 210 j=1,Ntet
           tetp=tetpol(j)
           if(tetp.lt.tetxb(1)) then
             tetp=tetp+2.d0*pi
           elseif(tetp.gt.tetxb(nxb)) then
             tetp=tetp-2.d0*pi
           endif
          CALL E02BCF(Nxb+4,RRK,CCK,tetp     ,0,CWk,IFAIL)
            ro0(j)=cwk(1)
c       write(6,*) 'ifail=',ifail,j
 210    CONTINUE

           return
           end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


