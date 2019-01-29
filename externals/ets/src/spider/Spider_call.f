       subroutine spider(nstep,time,dt,key_dmf,k_grid,k_auto,k_fixfree,
     &                   dpsdt,key_ini,voltpf)  

C---------------------------------------------------------------
C   MAIN  PROGRAM  OF  THE EVOLUTION CODE  "PET"
C---------------------------------------------------------------

       IMPLICIT REAL*8( A-H, O-Z )
	  common /com_0st/ key_0st,key_prs
        dimension  contvals_mat(2500),d_pf_mat(500)
        dimension  voltpf(*)
        dimension  d_cam_mat(500)
!        save i_enter
        integer, save :: i_enter
        data i_enter /0/
        
        i_enter=i_enter+1
        
      if(i_enter .eq. 1) then
         do i=1,19
	 voltpf(i)=0.d0
         enddo
      endif
        
c  kpr=1 for debugging, kpr=0 no printing

	    !kpr=1
          !call  kpr_calc(kpr)

       !tau_con=2.0d-2
       !k_con=(tau_con+1.d-8)/dt
       !k_con=999999

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              call put_tim(dt,time)
              call put_key_fix(k_fixfree)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       if(nstep.eq.0) then
          KLUCH = 0
       else
          KLUCH = 1
       endif

         if(k_fixfree.eq.0) then    
          call B_STEPON( KLUCH, k_auto, nstep, dt, time,
     *                   rax,zax ,key_dmf,dpsdt)
         ! call cur_avg
         ! call  wrb
        !write(*,*) '**'
        !write(*,*) 'nstep,time',nstep,time
        !write(*,*) '**'
        
        
        if(key_0st.eq.1 .AnD. nstep.eq.0) then
           nnstep=1        
           kkey_dmf=-10        
           KLUCH = 1
        
        
          call B_STEPON( KLUCH, k_auto, nnstep, dt, time,
     *                   rax,zax ,kkey_dmf,dpsdt)
        
        
        
        
        
           nstep=0        
        
        
        endif
        
        
        
        
          return
         endif

c---------------------
       !k_auto= 0
       
c----------------
!!!             ! k_grid= 0   rect. grid
!!!             ! k_grid= 1   adap. grid


       if(KLUCH.eq.0) then    !initialization
         
!
!!!!! basic free bound rectan, equilibrium ( KLUCH=0 )
!
        !key_ini=1

       if(key_ini .le. 0) then

          call  sstepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,key_dmf)

       else
          call  cf_init( k_auto, nstep, dt, time,
     *                     voltpf, d_pf_mat,d_cam_mat )

       endif

          !call  wrfb
          call  wrrec

c       stop

       k_auto= 0  ! don't change!

        if(k_grid.eq.1) then

          call  f_stepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,rax,zax,key_dmf)
        endif

	   !call cntrlr_iter(0,k_grid,voltpf)

        elseif(KLUCH.eq.1) then   !time steping



	print *,' Next step: stepon',nstep

      !   nstep1=i_enter-1
      !if(nstep1/k_con*k_con .eq. nstep1) then
      !   do i=1,19
	! voltpf(i)=0.d0
      !   enddo
	! call cntrlr_iter(1,k_grid,voltpf)
      !endif

        if(k_grid.eq.0) then
          call  sstepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,key_dmf)
        elseif(k_grid.eq.1) then

          call  f_stepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,rax,zax,key_dmf)
         call cur_avg
         call f_wrd
        endif

	!print *,' After f_stepon'


        if(nstep/20*20.eq.nstep) then 
          call wrd_tim
        endif

        write(*,*) '**'
        write(*,*) 'nstep,time',nstep,time
        write(*,*) '**'

        endif        !time steping





       return
       end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_psix(r_xp,z_xp,psi_xp)

        include 'double.inc'
        include 'param.inc'
        include 'comblc.inc'

         r_xp=rx0
         z_xp=zx0
         psi_xp=ux0

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_fpsix(r_xp,z_xp,psi_xp)

        include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'

         r_xp=rx0
         z_xp=zx0
         psi_xp=psix0

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_psib(r_ax,z_ax,psi_b)

        include 'double.inc'
        include 'param.inc'
        include 'comblc.inc'

         r_ax=rm
         z_ax=zm
         psi_b=up

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine  kpr_calc(kpr_xx)

      !include 'double.inc'
	common
     *  /c_kpr/kpr
	
	kpr=kpr_xx

	!print *,' kpr  FOR DEBUGING',kpr
	return
	end



	subroutine pau()
	return
	end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine put_name(name,ksym)
          include 'iopath.inc'
          character*40 name
          integer ksym
            path=name
            kname=ksym
         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine put_Ipl(placur)

        include 'double.inc'
        common /com_curpl/ cur_pl         

         cur_pl=placur

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_Ipl(placur)

        include 'double.inc'
        common /com_curpl/ cur_pl         

         placur=cur_pl

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



