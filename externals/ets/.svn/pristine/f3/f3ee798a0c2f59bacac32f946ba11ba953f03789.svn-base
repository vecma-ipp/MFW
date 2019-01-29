	 subroutine cntrlr_iter(kon_ini,kgrid,voltpf)

       include 'double.inc'
       include 'parevo.inc'
       include 'dimcp.inc'
       parameter(nconp=ngapp+npfc0+1)
       parameter(ntimp=1000)

       common /com_contr/ key_cont
       common /com_cam/ camtok
       common /com_cpdiv/ divgap(ngapp,ntimp),divpfc(npfc0,ntimp),
     &                    vpfc(npfc0,ntimp),xtim(ntimp),velz(ntimp),
     &                    placurr(ntimp),rm_t(ntimp),zm_t(ntimp),
     &                    cam_tok(ntimp)
       common /com_conpar/ R_ref,Z_ref,tok_ref,Dgap_ref,currpf_ref
       dimension voltpf(*)
       dimension currpf(npfc0),currpf_ref(npfc0)
       dimension pf_turns(npfc0)
       dimension Dgap(ngapp),Dgap_ref(ngapp)
       dimension Rcp(ngapp),Zcp(ngapp)

!       save R_ax,Z_ax,R_cc,Z_cc
       real*8 R_ax,Z_ax,R_cc,Z_cc
       !save R_ref,Z_ref,tok_ref
       !save Dgap_ref,currpf_ref
!       save pf_turns
       real*8 pf_turns
!       save i_en
       integer i_en

       i_en=i_en+1

!!!!!controllable variables:{gaps(1:6),Jpl,pfc(1:11)}
!
           call get_tim(dt,time)
           call get_RZcp(Rcp,Zcp,ngap)
          if(kgrid.eq.0) then 
           call get_psix(r_xp,z_xp,psi_xp)
          elseif(kgrid.eq.1) then 
           call get_fpsix(r_xp,z_xp,psi_xp)
          endif

        do i=1,ngap
         rg=Rcp(i)
         zg=Zcp(i)
         rt=rg
         zt=zg
         ut=psi_xp
        if(kgrid.eq.0) then 
         call gap(ut,rt,zt,rg,zg) 
         !call gap(ut,rt,zt,rg,zg) !!!!!!!!for tcv vde !!!!!!!!!!!!!!!!!!
        elseif(kgrid.eq.1) then 
         call f_gap(ut,rt,zt,rg,zg)
        endif
         gap_sign=dsign(1.d0,psi_xp-ut)
         Dgap(i)=dsqrt((rg-rt)**2+(zg-zt)**2)*gap_sign 
         !!!!!!!!for tcv vde!!!!!!!!!!!!!!!!!!Dgap(i)=0.
        enddo

           Z_ax_old=Z_ax 
           R_ax_old=R_ax 
           Z_cc_old=Z_cc 
           R_cc_old=R_cc 
           call get_shape(R_cc,Z_cc,R_ax,Z_ax,platok)
           call get_pfc(currpf,n_pf)

           Vz_ax=(Z_ax-Z_ax_old)/dt
           Vz_cc=(Z_cc-Z_cc_old)/dt
!
!!!!!controllable variables


!!!!!reference variables:{gaps_ref(1:6),Jpl_ref,pfc_ref(1:11)}
!
        !if(i_en.eq.1)then
        if(kon_ini.eq.0)then
         do i=1,ngap
         Dgap_ref(i)=Dgap(i)
         enddo
        !Dgap_ref(3)=0.15d0 

         do i=1,n_pf
         currpf_ref(i)=currpf(i)
         enddo

         tok_ref=platok
         R_ref=R_ax
         Z_ref=Z_ax

         Vz_ax=0.d0
         Vz_cc=0.d0
!         
!!!!!reference variables
         do i=1,n_pf
         voltpf(i)=0.d0
         enddo

         go to 1000
        endif
         !go to 1000 !!!!!!!!for tcv vde!!!!!!!!!!!!!!!!!!

	!if(time.le.0.25d0) then
      !  voltpf(7)= -10.d0    !!pf2
      !  voltpf(8)= -10.d0    !!pf3
      !  voltpf(9)=  10.d0    !!pf4
      !  voltpf(10)= 10.d0    !!pf5
      !endif
      !   go to 1000   !temporary:vde
 
         call congap_m( ngap,Dgap,Dgap_ref,dt,time,
     &                n_pf,currpf,currpf_ref,
     &                platok,tok_ref,R_ax,Z_ax,R_ref,Z_ref,Vz_cc,
     &                voltpf )


         !call convert( dt,time,n_pf,Vz_cc,voltpf )
         call convert_m( dt,time,n_pf,Vz_cc,voltpf )
	   call volt_lim(voltpf,n_pf)
      
 1000   continue      
      
        if(i_en.le.ntimp) then
        
         do i=1,ngap
         Divgap(i,i_en)=Dgap(i)-Dgap_ref(i) !
         enddo

         do i=1,n_pf
         divpfc(i,i_en)=currpf(i)-currpf_ref(i)
         enddo
        
         do i=1,n_pf
         vpfc(i,i_en)=voltpf(i)
         enddo
      
         xtim(i_en)=time
         
         velz(i_en)=Vz_cc
         placurr(i_en)=platok-tok_ref !
         rm_t(i_en)=R_ax
         zm_t(i_en)=Z_ax
         cam_tok(i_en)=camtok

        endif
   
        if(i_en/5*5.eq.i_en) then
	   call wrd_cp(i_en,ngap,n_pf)
        endif
     
     
 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine congap( ngap,Dgap,Dgap_ref,dt,time,
     &                     n_pf,currpf,currpf_ref,
     &                     platok,tok_ref,R_ax,Z_ax,R_ref,Z_ref,Vz_cc,
     &                     voltpf )

       include 'double.inc'
       parameter(nconp=20)
       
       dimension voltpf(*)
       dimension currpf(*),currpf_ref(*)
       dimension Dgap(*),Dgap_ref(*)
       
       dimension Ac(nconp,nconp),Bc(nconp,nconp)
       dimension Cc(nconp,nconp),Dc(nconp,nconp)
       dimension x(nconp),y(nconp),u(nconp),usat(nconp)
       dimension xp(nconp)
!       save i_en
       integer i_en
!       save Ac,Bc,Cc,Dc
!       save x
       real*8 Ac,Bc,Cc,Dc
       real*8 x
!       save nx,ny
       integer nx,ny

       i_en=i_en+1

       if(i_en.eq.1) then
       
        write(fname,'(a,a)') path(1:kname),'ABCD.wr'
        open(1,file=fname)
         read(1,*) n,m
         read(1,*) ((Ac(i,j),i=1,n),j=1,m)
         read(1,*) n,m
         read(1,*) ((Bc(i,j),i=1,n),j=1,m)
         read(1,*) n,m
         read(1,*) ((Cc(i,j),i=1,n),j=1,m)
         read(1,*) n,m
         read(1,*) ((Dc(i,j),i=1,n),j=1,m)
        close(1)

         nx=n_pf
         ny=m

       endif

          iy=0
       do i=1,ngap
          iy=iy+1
        y(iy)=Dgap(i)-Dgap_ref(i)  !
       enddo        
          iy=iy+1
        y(iy)=platok-tok_ref
        do i=1,n_pf
          iy=iy+1
        y(iy)=currpf(i)-currpf_ref(i)
       enddo        
       
       do i=1,nx
         Ax=0.d0
         By=0.d0
        do j=1,nx
         Ax=Ax+Ac(i,j)*x(j)
        enddo        
        do j=1,ny
         By=By+Bc(i,j)*y(j)
        enddo        
         xp(i)=x(i)+dt*(Ax+By)
       enddo        

       do i=1,nx
         Cx=0.d0
         Dy=0.d0
        do j=1,nx
         Cx=Cx+Cc(i,j)*xp(j)
        enddo        
        do j=1,ny
         Dy=Dy+Dc(i,j)*y(j)
        enddo        
         u(i)=Cx+Dy
       enddo        

       do i=1,nx
         x(i)=xp(i)
       enddo        

       do i=1,n_pf
        voltpf(i)=u(i)
       enddo

 	  return
	  end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine congap_k( ngap,Dgap,Dgap_ref,dt,time,
     &                     n_pf,currpf,currpf_ref,
     &                     platok,tok_ref,R_ax,Z_ax,R_ref,Z_ref,Vz_cc,
     &                     voltpf )

       include 'double.inc'
       parameter(nconp=20)
       
       dimension voltpf(*)
       dimension currpf(*),currpf_ref(*)
       dimension Dgap(*),Dgap_ref(*)
       
       dimension Ac(nconp,nconp),Bc(nconp,nconp)
       dimension Cc(nconp,nconp),Dc(nconp,nconp)
       dimension x(nconp),y(nconp),u(nconp),usat(nconp)
       dimension xp(nconp)
!       save i_en
!       save Ac,Bc,Cc,Dc
!       save x
!       save nx,ny
!       save t_delay,k_delay
       integer i_en
       real*8 Ac,Bc,Cc,Dc
       real*8 x
       integer nx,ny
       real*8 t_delay,k_delay
       
        t_delay=165.d-3
        k_delay=t_delay/dt
        
       i_en=i_en+1

       if(i_en.eq.1) then
       
        write(fname,'(a,a)') path(1:kname),'ahatmat'
        open(1,file=fname)
         !read(1,*) n,m
         n=11
         m=11
         read(1,*) ((Ac(i,j),j=1,m),i=1,n)
        close(1)

        write(fname,'(a,a)') path(1:kname),'bhatmat'
        open(1,file=fname)
         !read(1,*) n,m
         n=11
         m=18
         read(1,*) ((Bc(i,j),j=1,m),i=1,n)
        close(1)

        write(fname,'(a,a)') path(1:kname),'chatmat'
        open(1,file=fname)
         !read(1,*) n,m
         n=11
         m=11
         read(1,*) ((Cc(i,j),j=1,m),i=1,n)
        close(1)

        write(fname,'(a,a)') path(1:kname),'dhatmat'
        open(1,file=fname)
         !read(1,*) n,m
         n=11
         m=18
         read(1,*) ((Dc(i,j),j=1,m),i=1,n)
        close(1)

         nx=n_pf
         ny=m

       endif

          iy=0
       do i=1,ngap
          iy=iy+1
        y(iy)=Dgap(i)  !-Dgap_ref(i)
       enddo        
          iy=iy+1
        y(iy)=platok-tok_ref
        do i=1,n_pf
          iy=iy+1
        y(iy)=currpf(i)-currpf_ref(i)
       enddo        
       
       do i=1,nx
         Ax=0.d0
         By=0.d0
        do j=1,nx
         Ax=Ax+Ac(i,j)*x(j)
        enddo        
        do j=1,ny
         By=By+Bc(i,j)*y(j)
        enddo        
         xp(i)=x(i)+dt*(Ax+By)
       enddo        

       do i=1,nx
         Cx=0.d0
         Dy=0.d0
        do j=1,nx
         Cx=Cx+Cc(i,j)*xp(j)
        enddo        
        do j=1,ny
         Dy=Dy+Dc(i,j)*y(j)
        enddo        
         u(i)=Cx+Dy
       enddo        

       do i=1,nx
         x(i)=xp(i)
       enddo        

      if((i_en-1)/k_delay*k_delay.eq.i_en-1) then
      do i=1,n_pf
        voltpf(i)=u(i)
      enddo
      endif

 	  return
	  end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine congap_m( ngap,Dgap,Dgap_ref,dt,time,
     &                     n_pf,currpf,currpf_ref,
     &                     platok,tok_ref,R_ax,Z_ax,R_ref,Z_ref,Vz_cc,
     &                     voltpf )

       include 'double.inc'
       parameter(nconp=20)
       
       dimension voltpf(*)
       dimension currpf(*),currpf_ref(*)
       dimension Dgap(*),Dgap_ref(*)
       
       dimension Ac(nconp,nconp),Bc(nconp,nconp)
       dimension Cc(nconp,nconp),Dc(nconp,nconp)
       dimension x(nconp),y(nconp),u(nconp),uk(nconp),usat(nconp)
       dimension xp(nconp)
!       save i_en
!       save Ac,Bc,Cc,Dc
!       save x
!       save nx,ny
       integer i_en
       real*8 Ac,Bc,Cc,Dc
       real*8 x
       integer nx,ny

        tau=2.d-2
       i_en=i_en+1

       if(i_en.eq.1) then
       
        write(fname,'(a,a)') path(1:kname),'ABCD_m.wr'
        open(1,file=fname)
        !open(2,file='ABCD_mf.wr')
         read(1,*) n,m
         nx=n
         read(1,*) ((Ac(i,j),i=1,n),j=1,m)
         
         !write(2,*) n,m
         !write(2,*) ((Ac(i,j),i=1,n),j=1,m)
         
         read(1,*) n,m
         ny=m
         read(1,*) ((Bc(i,j),i=1,n),j=1,m)
         
         !write(2,*) n,m
         !write(2,*) ((Bc(i,j),i=1,n),j=1,m)
         
         read(1,*) n,m
         read(1,*) ((Cc(i,j),i=1,n),j=1,m)
         
         !write(2,*) n,m
         !write(2,*) ((Cc(i,j),i=1,n),j=1,m)
        
         read(1,*) n,m
         read(1,*) ((Dc(i,j),i=1,n),j=1,m)
         
         !write(2,*) n,m
         !write(2,*) ((Dc(i,j),i=1,n),j=1,m)
        !close(2)
        close(1)

         !pause 'pause:abcd '
       endif

          iy=0
       do i=1,ngap
          iy=iy+1
        y(iy)=Dgap(i)-Dgap_ref(i)  
       enddo        
          iy=iy+1
        y(iy)=platok-tok_ref
   
       do i=1,6
         iy=iy+1
        y(iy)=currpf(i+5)-currpf_ref(i+5)  !pf(1:6)
       enddo
 
         iy=iy+1
        y(iy)=currpf(5)-currpf_ref(5)  !CS3L
         iy=iy+1
        y(iy)=currpf(3)-currpf_ref(3)  !CS2L
         iy=iy+1
        y(iy)=currpf(1)-currpf_ref(1)  !CS1
         iy=iy+1
        y(iy)=currpf(2)-currpf_ref(2)  !CS2U
         iy=iy+1
        y(iy)=currpf(4)-currpf_ref(4)  !CS3U
 
       do i=1,nx
         Ax=0.d0
         By=0.d0
        do j=1,nx
         Ax=Ax+Ac(i,j)*x(j)
        enddo        
        do j=1,ny
         By=By+Bc(i,j)*y(j)
        enddo        
         xp(i)=x(i)+tau*(Ax+By)
       enddo        

       do i=1,n_pf
         Cx=0.d0
         Dy=0.d0
        do j=1,nx
         Cx=Cx+Cc(i,j)*xp(j)
        enddo        
        do j=1,ny
         Dy=Dy+Dc(i,j)*y(j)
        enddo        
         uk(i)=Cx+Dy
       enddo        

       do i=1,nx
         x(i)=xp(i)
       enddo 
              
         do i=1,6
          u(i+5)=uk(i) ! pf1:pf6
         enddo
          u(5)=uk(7)   !CS3L
          u(3)=uk(8)   !CS2L
          u(1)=uk(9)   !CS1
          u(2)=uk(10)  !CS2U
          u(4)=uk(11)  !CS3U

       do i=1,n_pf
        voltpf(i)=u(i)
       enddo

 	  return
	  end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine convert( dt,time,n_pf,Vz_cc,voltpf )

       include 'double.inc'
       parameter(nconp=20)
       
       dimension voltpf(*)
       
       dimension Acc(nconp,nconp),Bcc(nconp,nconp)
       dimension Ccc(nconp,nconp),Dcc(nconp,nconp)
       dimension x(nconp),y(nconp),u(nconp),usat(nconp)
       dimension xp(nconp)
!       save i_en
!       save Acc,Bcc,Ccc,Dcc
!       save x
!       save nx,ny
       integer i_en
       real*8 Acc,Bcc,Ccc,Dcc
       real*8 x
       integer nx,ny

       i_en=i_en+1

       if(i_en.eq.1) then
       
        write(fname,'(a,a)') path(1:kname),'ABCDz.wr'
        open(1,file=fname)
         read(1,*) n,m
         read(1,*) ((Acc(i,j),i=1,n),j=1,m) ![4x4]
         read(1,*) n,m
         read(1,*) ((Bcc(i,j),i=1,n),j=1,m) ![4x1]
         read(1,*) n,m
         read(1,*) ((Ccc(i,j),i=1,n),j=1,m) ![1x4]
         read(1,*) n,m
         read(1,*) ((Dcc(i,j),i=1,n),j=1,m) ![1x1]
        close(1)

         nx=4
         ny=1
        Vz_cc=0.d0

       endif

       do i=1,ny
        y(i)=Vz_cc
       enddo        
       
       do i=1,nx
         Ax=0.d0
         By=0.d0
        do j=1,nx
         Ax=Ax+Acc(i,j)*x(j)
        enddo        
        do j=1,ny
         By=By+Bcc(i,j)*y(j)
        enddo        
         xp(i)=x(i)+dt*(Ax+By)
       enddo        

       do i=1,nx
         Cx=0.d0
         Dy=0.d0
        do j=1,nx
         Cx=Cx+Ccc(j,i)*xp(j)
        enddo        
        do j=1,ny
         Dy=Dy+Dcc(i,j)*y(j)
        enddo        
         u(i)=Cx+Dy
       enddo        

       do i=1,nx
         x(i)=xp(i)
       enddo        

        voltpf(7)=voltpf(7)-u(1) /106.d0    !!pf2
        voltpf(8)=voltpf(8)-u(1) /185.d0    !!pf3
        voltpf(9)=voltpf(9)+u(1) /169.d0    !!pf4
        voltpf(10)=voltpf(10)+u(1) /217.d0  !!pf5

 	  return
	  end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine convert_m( dt,time,n_pf,Vz_cc,voltpf )

       include 'double.inc'
       parameter(nconp=20)
       
       dimension voltpf(*)
       dimension pf_turns(40)
       
       dimension Acc(nconp,nconp),Bcc(nconp,nconp)
       dimension Ccc(nconp,nconp),Dcc(nconp,nconp)
       dimension x(nconp),y(nconp),u(nconp),usat(nconp)
       dimension xp(nconp)
!       save i_en
!       save Acc,Bcc,Ccc,Dcc
!       save x
!       save nx,ny
       integer i_en
       real*8 Acc,Bcc,Ccc,Dcc
       real*8 x
       integer nx,ny

        tau=2.d-2

       i_en=i_en+1

       if(i_en.eq.1) then
       
        write(fname,'(a,a)') path(1:kname),'ABCDz_m.wr'
        open(1,file=fname)
         read(1,*) n,m
         read(1,*) ((Acc(i,j),i=1,n),j=1,m) ![4x4]
         read(1,*) n,m
         read(1,*) ((Bcc(i,j),i=1,n),j=1,m) ![4x1]
         read(1,*) n,m
         read(1,*) ((Ccc(i,j),i=1,n),j=1,m) ![11x4]
         read(1,*) n,m
         read(1,*) ((Dcc(i,j),i=1,n),j=1,m) ![11x1]
        close(1)

         nx=4
         ny=1
        Vz_cc=0.d0

         do i=7,12
          pf_turns(i)=548.d0
         enddo
          pf_turns(1)=249.d0
          pf_turns(2)=106.d0
          pf_turns(3)=185.d0
          pf_turns(4)=169.d0
          pf_turns(5)=217.d0
          pf_turns(6)=425.d0

       endif

       do i=1,ny
        y(i)=Vz_cc
       enddo        
       
       do i=1,nx
         Ax=0.d0
         By=0.d0
        do j=1,nx
         Ax=Ax+Acc(i,j)*x(j)
        enddo        
        do j=1,ny
         By=By+Bcc(i,j)*y(j)
        enddo        
         xp(i)=x(i)+tau*(Ax+By)
       enddo        

       do i=1,n_pf
         Cx=0.d0
         Dy=0.d0
        do j=1,nx
         Cx=Cx+Ccc(i,j)*xp(j)
        enddo        
        do j=1,ny
         Dy=Dy+Dcc(i,j)*y(j)
        enddo        
         u(i)=Cx+Dy
       enddo        

       do i=1,nx
         x(i)=xp(i)
       enddo        

         do i=1,6
          voltpf(i+5)=voltpf(i+5)+u(i) ! pf1:pf6
         enddo
          voltpf(5)=voltpf(5)+u(7)  !CS3L
          voltpf(3)=voltpf(3)+u(8)  !CS2L
          voltpf(1)=voltpf(1)+u(9)  !CS1
          voltpf(2)=voltpf(2)+u(10) !CS2U
          voltpf(4)=voltpf(4)+u(11) !CS3U

!a.	vPF2 = vPF2shape – vZ/106. 
!b.	vPF3 = vPF3shape – vZ/185. 
!c.	vPF4 = vPF4shape + vZ/169. 
!d.	vPF5 = vPF5shape + vZ/217.

 	  return
	  end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!	 subroutine cntrlr_iter_m(voltpf)
!
!       include 'double.inc'
!       include 'parevo.inc'
!       parameter(ngapp=10)
!       parameter(nconp=ngapp+npfc0+1)
!
!       dimension voltpf(*)
!       dimension currpf(npfc0),currpf_ref(npfc0)
!       dimension Dgap(ngapp),Dgap_ref(ngapp)
!       dimension Rcp(ngapp),Zcp(ngapp)
!
!       save R_ax,Z_ax,R_cc,Z_cc
!
!!!!!!!!!!!!!!!DINA variables!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!       dimension vchopper(npfc0),u_1(npfc0),u_kd(npfc0),u_help(npfc0)
!       dimension zvconverter(npfc0)
!       dimension pf(npfc0),curr_ref_p(npfc0)
!       dimension gaps(ngapp),gaps_ref_p(ngapp)
!       dimension error(nconp)
!       dimension zvresist(npfc0)
!       dimension pf_turns(npfc0)
!       dimension pf_lim(npfc0)
!       dimension state_vert(nconp),state_old_vert(nconp)
!       dimension state(nconp),state_old(nconp)
!       save i_en
!       save curr_ref_p,gaps_ref_p
!       save R_ref,Z_ref,cip1,rmag,zmag
!       save state_vert,state_old_vert
!       save state,state_old
!       save pf_lim,pf_turns,zvresist
!       save zvconverter,u_1,u_kd,u_help,vchopper
!       save error
!       i_en=i_en+1
!!
!!!!!!!!!!!!!!!DINA variables!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!!!!!!controllable variables:{gaps(1:6),Jpl,pfc(1:11)}
!
!           call get_tim(dt,time)
!           call get_RZcp(Rcp,Zcp,ngap)
!           call get_psix(r_xp,z_xp,psi_xp)
!
!        do i=1,ngap
!         rg=Rcp(i)
!         zg=Zcp(i)
!         rt=rg
!         zt=zg
!         ut=psi_xp
!         call gap(ut,rt,zt,rg,zg)
!         Dgap(i)=dsqrt((rg-rt)**2+(zg-zt)**2)
!        enddo
!
!           Z_ax_old=Z_ax 
!           R_ax_old=R_ax 
!           Z_cc_old=Z_cc 
!           R_cc_old=R_cc 
!           call get_shape(R_cc,Z_cc,R_ax,Z_ax,platok)
!           call get_pfc(currpf,n_pf)
!
!           Vz_ax=(Z_ax-Z_ax_old)/dt
!           Vz_cc=(Z_cc-Z_cc_old)/dt
!
!
!!!!!!controllable variables
!
!!!!!!transformation to DINA variables format:

!c vchopper(i) output voltage to PF coils
!c gaps  are gaps in time a
!c zvel - velocity
!c rref nd zref are R and Z reference positions
!c tpl plasma current 
!c cip1 reference value for plams current
!c npf number of PF coils
!c pf coils currents 
!c rmag,zmag  - Magnetic axes coordinates
!c elong - elongation
!c r_cur,z_cur are current cenroid coorinates
!
!          npf=n_pf+1
!          klim=1
!          kpr=1
!          tpl=platok*1.d3
!          rmag=R_ax*1.d2
!          zmag=Z_ax*1.d2
!          r_cur=R_cc*1.d2
!          z_cur=Z_cc*1.d2
!          tt=time*1.d3
!          zvel=Vz_cc*1.d-1
!
!         do i=1,ngap
!          gaps(i)=Dgap(i)*1.d2
!         enddo
!
!         do i=1,6
!          pf(i)=currpf(i+5)*1.d3
!         enddo
!          pf(7)=currpf(5)*1.d3  !CS3L
!          pf(8)=currpf(3)*1.d3  !CS2L
!          pf(9)=currpf(1)*1.d3  !CS1L
!          pf(10)=currpf(1)*1.d3 !CS1U
!          pf(11)=currpf(2)*1.d3 !CS2U
!          pf(12)=currpf(4)*1.d3 !CS3U
!
!         do i=7,12
!          pf_turns(i)=548.d0
!         enddo
!          pf_turns(1)=249.d0
!          pf_turns(2)=106.d0
!          pf_turns(3)=185.d0
!          pf_turns(4)=169.d0
!          pf_turns(5)=217.d0
!          pf_turns(6)=425.d0
!
!        if(i_en.eq.1)then
!         rref=rmag
!         zref=zmag
!         cip1=tpl
!         do i=1,npf
!         curr_ref_p(i)=pf(i)
!         enddo
!         do i=1,ngap
!          gaps_ref_p(i)=gaps(i)
!         enddo
!         gaps_ref_p(3)=7.d0 !
!          zvel=0.d0
!        endif
!         
!
!
!        call SCEN_CONTROL_c(
!     &       tpl,cip1,zref,rref,
!     &       npf,vchopper,r_cur,z_cur,
!     &       u_1,u_kd,error,zvel,elong,tt,gaps,gaps_ref_p,
!     &       pf,curr_ref_p,klim,
!     &       state,state_old,pf_turns,zvconverter,zvresist,
!     &       u_help,state_vert,state_old_vert,rmag,zmag,pf_lim,kpr)
!
!         do i=1,6
!          voltpf(i+5)=vchopper(i)
!         enddo
!          voltpf(5)=vchopper(7)  !CS3L
!          voltpf(3)=vchopper(8)  !CS2L
!          voltpf(1)=vchopper(9)  !CS1L
!          voltpf(1)=vchopper(10) !CS1U
!          voltpf(2)=vchopper(11) !CS2U
!          voltpf(4)=vchopper(12) !CS3U
!
!        
!        if(i_en.eq.1)then
!         do i=1,n_pf
!          voltpf(i)=0.d0
!         enddo
!        endif
!
! 	 return
!	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 !subroutine get_RZcp_m(Rcp,Zcp,ngap)
	 subroutine get_RZcp(Rcp,Zcp,ngap)

       include 'double.inc'
       dimension Rcp(*),Zcp(*)
   
       ngap=6
   
         Rcp(1)=4.344 
         Rcp(2)=5.575 
         Rcp(3)=8.2806
         Rcp(4)=7.5095
         Rcp(5)=5.3315
         Rcp(6)=4.0599 

         Zcp(1)=-3.458 
         Zcp(2)=-3.891
         Zcp(3)= 0.4665
         Zcp(4)= 2.9971
         Zcp(5)= 4.5804
         Zcp(6)= 0.7777
               
 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 !subroutine get_RZcp(Rcp,Zcp,ngap)
	 subroutine get_RZcp_k(Rcp,Zcp,ngap)

       include 'double.inc'
       dimension Rcp(*),Zcp(*)
   
       ngap=6
   
         Rcp(1)=4.200
         Rcp(2)=5.600
         Rcp(3)=8.200
         Rcp(4)=7.170
         Rcp(5)=5.420
         Rcp(6)=4.200

         Zcp(1)=-3.800
         Zcp(2)=-4.500
         Zcp(3)=0.420
         Zcp(4)=3.050
         Zcp(5)=4.050
         Zcp(6)=1.000
               
 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 subroutine get_shape(R_cc,Z_cc,R_ax,Z_ax, platok)

       include 'double.inc'
       include 'dim.inc'
       include 'compol.inc'
 
        R_ax=rm
        Z_ax=zm
 
        platok=tokp
 
        Rmax=rm
        Zmax=zm
        Rmin=rm
        Zmin=zm
 
       do j=2,nt1
        Rmax=dmax1(Rmax,r(iplas,j))
        Zmax=dmax1(Zmax,z(iplas,j))
        Rmin=dmin1(Rmin,r(iplas,j))
        Zmin=dmin1(Zmin,z(iplas,j))
       enddo
 
        R_cc=0.5d0*(Rmax+Rmin)              
        Z_cc=0.5d0*(Zmax+Zmin)              

 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 subroutine get_pfc(currpf,npf)

       include 'double.inc'
       INCLUDE 'prm.inc'
       INCLUDE 'comevl.inc'
       common/comst1/ PJK(NJLIM),PJKP1(NJLIM),PJKP(NJLIM),PJKD(NJLIM)
       dimension currpf(*)
 
        npf=nequi
 
        do i=1,nequi
         currpf(i) = PJK(i)
        enddo

 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 subroutine volt_lim(voltpf,npf)

       include 'double.inc'
       include 'parevo.inc'
       !dimension voltpf(*)
       dimension voltpf(npfc0)
       dimension usat(npfc0)
 
         usat(1)=2.7d0
         usat(2)=2.7d0
         usat(3)=2.7d0
         usat(4)=2.7d0
         usat(5)=2.7d0
         usat(6)=6.0d0
         usat(7)=53.8d0
         usat(8)=30.8d0
         usat(9)=33.7d0
         usat(10)=26.3d0
         usat(11)=3.5d0

       do i=1,npf
        if(dabs(voltpf(i)).gt.usat(i)) then
        voltpf(i)=dsign(usat(i),voltpf(i))
        endif
       enddo

!a.	vCS1 = +/- 2.7 V/t, 
!b.	vCS2U = +/- 2.7 V/t, 
!c.	vCS2L = +/- 2.7 V/t, 
!d.	vCS3U = +/- 2.7 V/t,
!e.	 vCS3L = +/- 2.7 V/t,
!f.	 vPF1 = +/- 6.0 V/t, 
!g.	vPF2 = +/-53.8 V/t,, 
!h.	vPF3 = +/- 30.8 V/t, 
!i.	vPF4 = +/- 33.7 V/t, 
!j.	vPF5 = +/- 26.3 V/t, 
!k.	vPF6 = +/- 3.5 V/t.

 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 subroutine wrd_cp(i_en,ngap,npf)
	
       include 'double.inc'
       parameter(ngapp=10)
       parameter(ntimp=1000)
       include 'parevo.inc'
       common /com_cam/ camtok
       common /com_cpdiv/ divgap(ngapp,ntimp),divpfc(npfc0,ntimp),
     &                    vpfc(npfc0,ntimp),xtim(ntimp),velz(ntimp),
     &                    placurr(ntimp),rm_t(ntimp),zm_t(ntimp),
     &                    cam_tok(ntimp)
 
       open(1,file='contim.wr')
        write(1,*) i_en,ngap,npf
        write(1,*) ((divgap(i,j),i=1,ngap),j=1,i_en)
        write(1,*) ((divpfc(i,j),i=1,npf),j=1,i_en)
        write(1,*) ((vpfc(i,j),i=1,npf),j=1,i_en)
        write(1,*) (xtim(j),j=1,i_en)
        write(1,*) (velz(j),j=1,i_en)
        write(1,*) (placurr(j),j=1,i_en)
        write(1,*) (rm_t(j),j=1,i_en)
        write(1,*) (zm_t(j),j=1,i_en)
        write(1,*) (cam_tok(j),j=1,i_en)
       close(1)

 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 subroutine get_conp(R_ref,Z_ref,tok_ref,Dgap_ref,currpf_ref)
       include 'double.inc'
       include 'parevo.inc'
       include 'dimcp.inc'
       common /com_conpar/ Rc_ref,Zc_ref,cur_ref,gap_ref,pf_ref
       dimension pf_ref(npfc0),currpf_ref(*)
       dimension Dgap_ref(*),gap_ref(ngapp)

         R_ref=Rc_ref
         Z_ref=Zc_ref
         tok_ref=cur_ref
         
        do i=1,npfc0
         currpf_ref(i)=pf_ref(i)
        enddo
         
        do i=1,ngapp
         Dgap_ref(i)=gap_ref(i)
        enddo
        
 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 subroutine put_conp(R_ref,Z_ref,tok_ref,Dgap_ref,currpf_ref)
       include 'double.inc'
       include 'parevo.inc'
       include 'dimcp.inc'
       common /com_conpar/ Rc_ref,Zc_ref,cur_ref,gap_ref,pf_ref
       dimension pf_ref(npfc0),currpf_ref(*)
       dimension Dgap_ref(*),gap_ref(ngapp)

         Rc_ref=R_ref
         Zc_ref=Z_ref
         cur_ref=tok_ref
         
        do i=1,npfc0
         pf_ref(i)=currpf_ref(i)
        enddo
         
        do i=1,ngapp
         gap_ref(i)=Dgap_ref(i)
        enddo
        
 	 return
	 end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine put_key_con(k)
          include 'iopath.inc'
          common /com_contr/ key_cont
          integer k
            key_cont=k
         return
         end
