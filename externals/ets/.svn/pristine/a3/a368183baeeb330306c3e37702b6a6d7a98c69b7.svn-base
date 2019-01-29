         program main_spider
C---------------------------------------------------------------
C   MAIN  PROGRAM  OF  THE EVOLUTION CODE  "SPIDER"
C---------------------------------------------------------------

         include 'double.inc'
       
            parameter(mdim=50)           
            include 'dimpl1.inc'       
            dimension af_r(nrp,0:mdim),bf_r(nrp,0:mdim)
            dimension af_z(nrp,0:mdim),bf_z(nrp,0:mdim)
       
        dimension  contvals_mat(2500),voltpf(500),d_pf_mat(500)
        dimension  d_cam_mat(500)
        dimension  Rcp(10),Zcp(10)
         character*40 prename
         character*40   eqdfn
         
c  kpr=1 for debugging, kpr=0 no printing

	    kpr=1
	    prename=''
          kname=1
      call aspid_flag(0)
      call  kpr_calc(kpr)
      call  put_name(prename,kname)
        
            call ppf2modul
            call bnd2modul

       !key_dmf=-2 
       key_dmf=0 

       nstop = 1500

       nstep=0
       time=0.d0
       dt=2.0d-2
       tau_con=2.0d-2
       k_con=(tau_con+1.d-8)/dt
       !k_con=100000 
       !k_con=1
              call put_tim(dt,time)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         !call prof_rec


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       KLUCH = 0
       k_fixfree = 1
       
        call put_key_fix(k_fixfree)

!----------------only fixed boundary adaptive grid case
!
!
         if(k_fixfree.eq.0) then    
       k_auto= 1
          call B_STEPON( KLUCH, k_auto, nstep, dt, time,
     *                   rax,zax ,key_dmf,dpsdt)
          call get_flfi(flfi_m)
          call cur_avg
          call  wrb
          !call field_c
        write(*,*) 'flux_fi',flfi_m
        write(*,*) 'nstep done,time',nstep,time
        write(*,*) '**'
       
         !   mg=6       
         !call furgrid(mdim,mg,af_r,bf_r,af_z,bf_z)
       
       
       KLUCH = 1
       k_auto= 0
       !dpsdt=-3.2d0
       dpsdt=0.d0
       !dpsdt=100.d-3/dt
       
      !call dmf_test_rd(nstop)
 
       do nstep = 1,nstop
               time=time+dt
              call put_tim(dt,time)
              !call savepsi
          !if(nstep.eq.1) call press_p
          !if(nstep.eq.5) key_dmf=-2
      
          call B_STEPON( KLUCH, k_auto, nstep, dt, time,
     *                   rax,zax ,key_dmf,dpsdt)
          call cur_avg
          call get_flfi(flfi_m)
          !call field_c
          !call  pla_volt(dt)
          call  wrb
        write(*,*) 'flux_fi',flfi_m
        write(*,*) '**'
        write(*,*) 'nstep done,time',nstep,time
        write(*,*) '**'

      !call dmf_test_wr(nstep,nstop)
       enddo
       stop
        endif
!        
!        
!---------------------only fixed boundary adaptive grid case
       k_auto= 1
c----------------
       k_grid= 0   ! rect. grid
       !!!   k_grid= 1   ! adap. grid

          !call tab_build
!
!!!!! basic free bound rectan, equilibrium ( KLUCH=0 )
!
        key_ini=1

       if(key_ini .eq. 0) then
          call  sstepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,key_dmf)
        else
          call  cf_init( k_auto, nstep, dt, time,
     *                     voltpf, d_pf_mat,d_cam_mat )
        endif
          !call  wrfb
          call  wrrec
          eqdfn='eqdsk_128x129-tin047_I=1,5.wr'
          !!call eqdsk_rebild
          !call eqdsk_build(eqdfn)
          call cur_avg
          call  wrb
          call wr_spik
          call coil_force
          !call field_c
          !call get_www(roomega)

       stop !!
	 call cntrlr_iter(0,k_grid,voltpf)

          !call get_bouL_bra(contvals_mat,g1r,g1z,g2r,g2z,g5r,g5z)

       k_auto= 0

        if(k_grid.eq.1) then
!
!!!!! basic free bound, adaptive equilibrium ( KLUCH=0 )
!

          call  f_stepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,rax,zax,key_dmf)
          call cur_avg
          call f_wrd
        endif

         !call eqdsk_build('eqdsk_spidat.dat.1480')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         pause 'pause:initial equilibrium '
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       KLUCH = 1

       do nstep = 1,nstop
         

         nstep1=nstep !-1
      if(nstep1/k_con*k_con .eq. nstep1) then
         do i=1,19
	 voltpf(i)=0.d0
         enddo
	 call cntrlr_iter(1,k_grid,voltpf)
      endif
      
              time=time+dt
              call put_tim(dt,time)
	print *,' Next step: stepon',nstep

        if(k_grid.eq.0) then

          call  sstepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,key_dmf)
	print *,'  stepon done'
         call cur_avg
         call wrd
         call wrb
	print *,'  wrd done'
        elseif(k_grid.eq.1) then

          call  f_stepon( KLUCH, k_auto,nstep,dt,time,
     *                  voltpf, d_pf_mat ,d_cam_mat,rax,zax,key_dmf)
         call cur_avg
         call f_wrd
        endif

	print *,' After stepon'


          !call get_bouL_bra(contvals_mat,g1r,g1z,g2r,g2z,g5r,g5z)

        if(nstep/5*5.eq.nstep) then 
          call wrd_tim
          !pause 'pause'
        endif
        write(*,*) '**'
        write(*,*) 'nstep done,time',nstep,time
        write(*,*) '**'
       enddo

       stop
       end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine dmf_test_wr(nstep,nstop)

        include 'double.inc'
        include 'dim.inc'
        include 'compol.inc'
        common/psi_test/ psi_ext_bon(300)

          psi_ext_bon(nstep)=psi_eav

        if(nstep.eq.nstop) then
         open(1,file='bonpsi.wr',form='formatted') 
          write(1,*) (psi_ext_bon(i),i=1,nstop)
         close(1)
        endif

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine dmf_test_rd(nstop)

        include 'double.inc'
        include 'dim.inc'
        include 'compol.inc'
        common/psi_test/ psi_ext_bon(300)

         open(1,file='bonpsi.wr',form='formatted') 
          read(1,*) (psi_ext_bon(i),i=1,nstop)
         close(1)

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine press_p
        include 'double.inc'
        include 'dim.inc'
        include 'compol.inc'
          
         ampl=2.50d0 !0.d0  !1.5d0 0.5d0
         xx0=0.50d0
         w=0.1d0
         do i=1,iplas
          xx =dfloat(i)/dfloat(iplas)
          delp_prim = ampl*( 1.d0-dtanh(((xx0-xx)/w)**4) )
         dpdpsi(i)=dpdpsi(i)+delp_prim
         enddo
          
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine prof_rec
        include 'double.inc'
         parameter(np=1000)
          dimension ps(np),fp(np),pp(np)
          dimension ps1(np),fp1(np),pp1(np)
          
           alp=0.75d0
           alp=0.d0

         open(1,file='tabppf_tin047p.dat')
	     read(1,*) n
	     do i=1,n
	      read(1,*) ps(i),pp(i),fp(i)
           enddo
         close(1)
          
         open(1,file='tabppf_tin047.dat')
	     read(1,*) n
	     do i=1,n
	      read(1,*) ps1(i),pp1(i),fp1(i)
           enddo
         close(1)
           
          
	     do i=1,n
            fp(i)=alp*fp(i) + (1.d0-alp)*fp1(i)         
           enddo
          
         open(1,file='tabppf.dat')
	     write(1,*) n
	     do i=1,n
	      write(1,*) ps(i),pp(i),fp(i)
           enddo
         close(1)
     
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine ppf2modul
         
         use ppf_modul
                
        include 'double.inc'
        
        write(fname,'(a,a)') path(1:kname),'tabppf.dat'
        open(1,file=fname,form='formatted')
          !open(1,file='tabppf.dat')
              read(1,*) nutab
       allocate( pstab(nutab), pptab(nutab), fptab(nutab) )
           do i=1,nutab
              read(1,*) pstab(i),pptab(i),fptab(i)
           enddo
          close(1)                                                 
       !deallocate( pstab, pptab, fptab )


        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine bnd2modul
         
         use bnd_modul
                
        include 'double.inc'
        
        write(fname,'(a,a)') path(1:kname),'tab_bnd.dat'
        open(1,file=fname,form='formatted')
          !open(1,file='tabppf.dat')
              read(1,*) nbtab
       allocate( rbtab(nbtab), zbtab(nbtab) )
           do i=1,nbtab
              read(1,*) rbtab(i),zbtab(i)
           enddo
          close(1)                                                 
       !deallocate( rbtab, zbtab )


        return
        end