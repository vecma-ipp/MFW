subroutine helena21itm(equil_in,equil_out)
!------------------------------------------------------------------------
! helena wrapper for HELENA2 (version helena21.f90)
!
! uses ITM data structures, using an ITM standard equilibrium
! for the input and output.
!
!                                             G. Huysmans, CEA Cadarache
!                                             Date   : 24/6/2008
!                                             Status : in progress
!------------------------------------------------------------------------

use euITM_schemas                       ! module containing the equilibrium type definitions
use itm_constants
use helena21_mod
implicit none

type (type_equilibrium),pointer :: equil_in(:), equil_out(:)

character*131       :: helena_par
integer             :: nr_grid, np_grid, n_psi_out, n_tht_out, n_bnd, n_prof_in, iopt_p, iopt_f, i, j, nchi, node
real (r8)              :: quad_u_in, quad_l_in, Bgeo_in
real (r8), allocatable :: fraction_circ(:),   surface_powers(:,:), surface_integrals(:,:)
real (r8), allocatable :: volume_powers(:,:), volume_integrals(:,:), moments(:,:), vprime(:), dpsi_drho(:)
integer             :: n_var_surfaces, n_int_surfaces, n_var_volumes, n_int_volumes, n_moments
real (r8), allocatable :: RRflux(:,:),ZZflux(:,:),PSflux(:,:)
real (r8)              :: amin_out, Rgeo_out, Zgeo_out, area_out, volume_out, betap_out, xip_out, xli_out, betat_out
real (r8)              :: R_axis_out, Z_axis_out, B_axis_out, psi_axis_out, psi_bnd_out, PI
integer             :: ileft, iright

write(*,*) '**********************************************'
write(*,*) '*                HELENA_ITM (ALFA)           *'
write(*,*) '**********************************************'

PI = 2.e0_R8 * asin(1.e0_R8) 

if (.not. associated(equil_in)) then
  write(*,*) equil_in%eqgeometry%a_minor
  write(*,*) ' equil_in not defined'
  return
endif

nr_grid   = nint(size(equil_in(1)%profiles_1d%psi)/2.0)
!nr_grid   = 41
nr_grid   = 101
np_grid   = 33
!np_grid   = 65
n_psi_out = size(equil_in(1)%profiles_1d%psi)
n_tht_out = size(equil_in(1)%eqgeometry%boundary(1)%r)
!n_psi_out = 41
!n_tht_out = 33
n_var_surfaces = 3      ! the number of variables with a power appearing in the integralsHELENA2
n_int_surfaces = 8     ! the number of different integrals
n_moments      = 7

n_bnd = 0
!if (associated(equil_in(1)%eqgeometry%boundary(1)%r)) then
  n_bnd = size(equil_in(1)%eqgeometry%boundary(1)%r,1)
  write(*,*) ' n_bnd : ',n_bnd
!endif


allocate(surface_powers(n_var_surfaces,n_int_surfaces), surface_integrals(n_psi_out,n_int_surfaces))
allocate(vprime(n_psi_out),dpsi_drho(n_psi_out))

surface_powers(1:3,1)  = (/ -2, 0, 0 /)        !  R, B^2, grad(psi)
surface_powers(1:3,2)  = (/ -2, 0, 2 /)
surface_powers(1:3,3)  = (/  0, 0, 2 /)
surface_powers(1:3,4)  = (/  0,-1, 0 /)
surface_powers(1:3,5)  = (/  0, 1, 0 /)
surface_powers(1:3,6)  = (/  0,-1, 2 /)
surface_powers(1:3,7)  = (/  0, 0, 1 /)
surface_powers(1:3,8)  = (/  0, 0, 0 /)

n_var_volumes = 2   ! the number of variables with a power appearing in the integrals
n_int_volumes = 3   ! the number of different integrals

allocate(volume_powers(n_var_volumes,n_int_volumes), volume_integrals(n_psi_out,n_int_volumes))

volume_powers(1:2,1) =  (/  0, 0 /)       ! Volume        = Int(dV) = Int(R dA)
volume_powers(1:2,2) =  (/ -1, 1 /)       ! Toroidal_flux = Int(B / R dV)
volume_powers(1:2,3) =  (/ -1, 0 /)       ! Area          = Int( 1/R dV)

allocate(RRflux(4,n_psi_out*n_tht_out),ZZflux(4,n_psi_out*n_tht_out),PSflux(4,n_psi_out*n_tht_out))
allocate(moments(n_moments,n_psi_out),fraction_circ(n_psi_out))

if (associated(equil_in(1)%profiles_1d%pressure)) then
  n_prof_in = size(equil_in(1)%profiles_1d%pressure,1)
  if (.not. associated(equil_in(1)%profiles_1d%pprime)) allocate(equil_in(1)%profiles_1d%pprime(n_prof_in))
elseif (associated(equil_in(1)%profiles_1d%pprime)) then
  n_prof_in = size(equil_in(1)%profiles_1d%pprime,1)
endif
iopt_p = 1
iopt_f = 4

if (.not. associated(equil_in(1)%profiles_1d%jphi)) then
  write(*,*) ' jphi not associated '
  allocate(equil_in(1)%profiles_1d%jphi(n_prof_in))
  equil_in(1)%profiles_1d%jphi = 0.e0_R8
endif
np_grid   = 33
if (.not. associated(equil_in(1)%profiles_1d%ffprime)) allocate(equil_in(1)%profiles_1d%ffprime(n_prof_in))

if (.not. associated(equil_out)) allocate(equil_out(1))

allocate(equil_out(1)%profiles_1d%psi(n_psi_out))
allocate(equil_out(1)%profiles_1d%pressure(n_psi_out))
allocate(equil_out(1)%profiles_1d%pprime(n_psi_out))
allocate(equil_out(1)%profiles_1d%f_dia(n_psi_out))
allocate(equil_out(1)%profiles_1d%ffprime(n_psi_out))
allocate(equil_out(1)%profiles_1d%q(n_psi_out))
allocate(equil_out(1)%profiles_1d%jphi(n_psi_out))
allocate(equil_out(1)%profiles_1d%vprime(n_psi_out))

Bgeo_in = equil_in(1)%eqconstraint%bvac_r%measured / equil_in(1)%eqgeometry%geom_axis%r
write(*,*) 'Bgeo_in ',Bgeo_in

!write(*,*) ' using equil_in(1)%eqconstraint%bvac_r%measured !!! WARNING : USING BVAC_R as if BVAC'
!Bgeo_in = equil_in(1)%eqconstraint%bvac_r%measured !/ equil_in(1)%eqgeometry%geom_axis%r
  
call helena21(equil_in(1)%eqgeometry%boundary(1)%r(1:n_bnd),equil_in(1)%eqgeometry%boundary(1)%z(1:n_bnd),n_bnd,                 &
              equil_in(1)%eqgeometry%geom_axis%r, equil_in(1)%eqgeometry%geom_axis%z, equil_in(1)%eqgeometry%a_minor,      &
              equil_in(1)%eqgeometry%elongation,equil_in(1)%eqgeometry%tria_upper, equil_in(1)%eqgeometry%tria_lower,      &
              quad_u_in, quad_l_in,                                                                                        &
              Bgeo_in,                                                                                                     &
              equil_in(1)%profiles_1d%psi,      equil_in(1)%profiles_1d%pprime,   equil_in(1)%profiles_1d%ffprime,         &
              equil_in(1)%profiles_1d%pressure, equil_in(1)%profiles_1d%f_dia,    equil_in(1)%profiles_1d%jphi,            &
              n_prof_in, iopt_p, iopt_f,                                                                                   &
              nr_grid, np_grid,                                                                                            &
              equil_out(1)%profiles_1d%psi,      equil_out(1)%profiles_1d%pprime,  equil_out(1)%profiles_1d%ffprime,       &
              equil_out(1)%profiles_1d%pressure, equil_out(1)%profiles_1d%f_dia,   equil_out(1)%profiles_1d%jphi,          &
              equil_out(1)%profiles_1d%q, equil_out(1)%profiles_1d%vprime,                                                 &
	      fraction_circ, moments, n_moments,                                                                           &
              surface_powers, surface_integrals, n_var_surfaces, n_int_surfaces,                                           &
              volume_powers,  volume_integrals,  n_var_volumes,  n_int_volumes,  n_psi_out, n_tht_out,                     &
              amin_out, Rgeo_out, Zgeo_out,                                                                                &
              area_out, volume_out, betap_out, xip_out, xli_out, betat_out,                                                &
              R_axis_out, Z_axis_out, B_axis_out, psi_axis_out, psi_bnd_out,                                               &
              RRflux,ZZflux,PSflux)

write(*,*) ' helena21 done'                    
	      
write(*,*) n_psi_out, n_tht_out
write(*,*) ' magnetic axis : ',R_axis_out,Z_axis_out
              
allocate(equil_out(1)%datainfo%dataprovider(1))
equil_out(1)%datainfo%dataprovider(1) = ' HELENA2 '
equil_out(1)%time = equil_in(1)%time

equil_out(1)%global_param%area          = area_out
equil_out(1)%global_param%volume        = volume_out

equil_out(1)%global_param%psi_ax        = psi_axis_out
equil_out(1)%global_param%psi_bound     = psi_bnd_out
equil_out(1)%global_param%beta_pol      = betap_out
equil_out(1)%global_param%I_plasma      = xip_out
equil_out(1)%global_param%li            = xli_out
equil_out(1)%global_param%beta_tor      = betat_out
equil_out(1)%global_param%beta_normal   = 1.256637061*betat_out / xip_out
!equil_out(1)%global_param%q_min        = minval(QS(1:npsi))

equil_out(1)%global_param%mag_axis%position%r = R_axis_out
equil_out(1)%global_param%mag_axis%position%z = Z_axis_out
equil_out(1)%global_param%mag_axis%bphi       = B_axis_out
!equil_out(1)%global_param%mag_axis%q          = QS(1)


equil_out(1)%eqgeometry%a_minor      = amin_out
equil_out(1)%eqgeometry%geom_axis%r  = Rgeo_out
equil_out(1)%eqgeometry%geom_axis%z  = Zgeo_out

nchi = n_tht_out

allocate(equil_out(1)%eqgeometry%boundary(1),equil_out(1)%eqgeometry%boundary(1)%r(nchi),equil_out(1)%eqgeometry%boundary(1)%z(nchi))

do j=1, nchi
  node   = nchi*(n_psi_out-1) + j
  equil_out(1)%eqgeometry%boundary(1)%r(j) = RRflux(1,node)
  equil_out(1)%eqgeometry%boundary(1)%z(j) = ZZflux(1,node)
enddo

allocate(equil_out(1)%profiles_1d%volume(n_psi_out))
allocate(equil_out(1)%profiles_1d%rho_vol(n_psi_out))
allocate(equil_out(1)%profiles_1d%area(n_psi_out))
allocate(equil_out(1)%profiles_1d%phi(n_psi_out))
allocate(equil_out(1)%profiles_1d%gm1(n_psi_out))
allocate(equil_out(1)%profiles_1d%gm2(n_psi_out))
allocate(equil_out(1)%profiles_1d%gm3(n_psi_out))
allocate(equil_out(1)%profiles_1d%gm4(n_psi_out))
allocate(equil_out(1)%profiles_1d%gm5(n_psi_out))
allocate(equil_out(1)%profiles_1d%gm6(n_psi_out))
allocate(equil_out(1)%profiles_1d%gm7(n_psi_out))


equil_out(1)%profiles_1d%volume(1:n_psi_out) = volume_integrals(1:n_psi_out,1)
equil_out(1)%profiles_1d%area(1:n_psi_out)   = volume_integrals(1:n_psi_out,3)
equil_out(1)%profiles_1d%phi(1:n_psi_out)    = volume_integrals(1:n_psi_out,2) /   &
     (2.e0_R8 * PI)

!DPC added abs()
equil_out(1)%profiles_1d%rho_vol=sqrt(abs(equil_out(1)%profiles_1d%volume/equil_out(1)%profiles_1d%volume(n_psi_out)))

!--------------should not be normalised psi!
dpsi_drho = 2.e0_R8 * sqrt(equil_out(1)%profiles_1d%phi/(PI * Bgeo_in)) /   &
     equil_out(1)%profiles_1d%q  / abs(psi_axis_out - psi_bnd_out)
dpsi_drho = 2.e0_R8 * sqrt(equil_out(1)%profiles_1d%phi/(PI * Bgeo_in)) / equil_out(1)%profiles_1d%q 

call cos_zconversion(dpsi_drho,n_psi_out)
equil_out(1)%profiles_1d%vprime = equil_out(1)%profiles_1d%vprime * dpsi_drho
call cos_zconversion(equil_out(1)%profiles_1d%vprime,n_psi_out)


equil_out(1)%profiles_1d%gm1 = surface_integrals(1:n_psi_out,1)
equil_out(1)%profiles_1d%gm2 = surface_integrals(1:n_psi_out,2)  / dpsi_drho**2
equil_out(1)%profiles_1d%gm3 = surface_integrals(1:n_psi_out,3)  / dpsi_drho**2
equil_out(1)%profiles_1d%gm4 = surface_integrals(1:n_psi_out,4)
equil_out(1)%profiles_1d%gm5 = surface_integrals(1:n_psi_out,5)
equil_out(1)%profiles_1d%gm6 = surface_integrals(1:n_psi_out,6)  / dpsi_drho**2
equil_out(1)%profiles_1d%gm7 = surface_integrals(1:n_psi_out,7)  / dpsi_drho

call cos_zconversion(equil_out(1)%profiles_1d%gm6,n_psi_out)
call cos_zconversion(equil_out(1)%profiles_1d%gm7,n_psi_out)

!--temporary bugfix cronos_test
!equil_in(1)%profiles_1d%gm6 = equil_in(1)%profiles_1d%gm6 * equil_in(1)%profiles_1d%vprime


!write(*,*) ' equi_out profiles : '

!write(*,'(A,6e12.4)') ' GM1 : ',equil_out(1)%profiles_1d%gm1(n_psi_out),equil_in(1)%profiles_1d%gm1(size(equil_in(1)%profiles_1d%gm1)), &
!                               equil_out(1)%profiles_1d%gm1(n_psi_out)/equil_in(1)%profiles_1d%gm1(size(equil_in(1)%profiles_1d%gm1))
!write(*,'(A,6e12.4)') ' GM2 : ',equil_out(1)%profiles_1d%gm2(n_psi_out),equil_in(1)%profiles_1d%gm2(size(equil_in(1)%profiles_1d%gm2)), &
!                               equil_out(1)%profiles_1d%gm2(n_psi_out)/equil_in(1)%profiles_1d%gm2(size(equil_in(1)%profiles_1d%gm2))
!write(*,'(A,6e12.4)') ' GM3 : ',equil_out(1)%profiles_1d%gm3(n_psi_out),equil_in(1)%profiles_1d%gm3(size(equil_in(1)%profiles_1d%gm3)), &
!                               equil_out(1)%profiles_1d%gm3(n_psi_out)/equil_in(1)%profiles_1d%gm3(size(equil_in(1)%profiles_1d%gm3))
!write(*,'(A,6e12.4)') ' GM4 : ',equil_out(1)%profiles_1d%gm4(n_psi_out),equil_in(1)%profiles_1d%gm4(size(equil_in(1)%profiles_1d%gm4)), &
!                               equil_out(1)%profiles_1d%gm4(n_psi_out)/equil_in(1)%profiles_1d%gm4(size(equil_in(1)%profiles_1d%gm4))
!write(*,'(A,6e12.4)') ' GM5 : ',equil_out(1)%profiles_1d%gm5(n_psi_out),equil_in(1)%profiles_1d%gm5(size(equil_in(1)%profiles_1d%gm5)), &
!                               equil_out(1)%profiles_1d%gm5(n_psi_out)/equil_in(1)%profiles_1d%gm5(size(equil_in(1)%profiles_1d%gm5))
!write(*,'(A,6e12.4)') ' GM6 : ',equil_out(1)%profiles_1d%gm6(n_psi_out),equil_in(1)%profiles_1d%gm6(size(equil_in(1)%profiles_1d%gm6)), &
!                               equil_out(1)%profiles_1d%gm6(n_psi_out)/equil_in(1)%profiles_1d%gm6(size(equil_in(1)%profiles_1d%gm6))
!write(*,'(A,6e12.4)') ' GM7 : ',equil_out(1)%profiles_1d%gm7(n_psi_out),equil_in(1)%profiles_1d%gm7(size(equil_in(1)%profiles_1d%gm7)), &
!                               equil_out(1)%profiles_1d%gm7(n_psi_out)/equil_in(1)%profiles_1d%gm7(size(equil_in(1)%profiles_1d%gm7))
!write(*,'(A,2e12.4)') ' q   : ',equil_out(1)%profiles_1d%q(n_psi_out),equil_in(1)%profiles_1d%q(size(equil_in(1)%profiles_1d%q))
!write(*,'(A,2e12.4)') ' vp  : ',equil_out(1)%profiles_1d%vprime(n_psi_out),equil_in(1)%profiles_1d%vprime(size(equil_in(1)%profiles_1d%vprime))
!write(*,'(A,2e12.4)') ' phi : ',equil_out(1)%profiles_1d%phi(n_psi_out)!,equil_in(1)%profiles_1d%phi(size(equil_in(1)%profiles_1d%phi))
!write(*,'(A,2e12.4)') ' psi : ',equil_out(1)%profiles_1d%psi(n_psi_out)-equil_out(1)%profiles_1d%psi(1)

!call begplt('check.ps')

!call lincol(1)
!call lplot6(2,2,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%gm1(2),size(equil_in(1)%profiles_1d%psi)-1,'gm1')
!call lincol(0)
!call lplot6(2,2,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%gm1(2),-n_psi_out+1,'gm1')
!call lincol(0)

!call lplot6(2,3,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%gm2(2),n_psi_out-1,'gm2')
!call lincol(1)
!call lplot6(2,3,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%gm2(2),-size(equil_in(1)%profiles_1d%psi)+1,' ')
!call lincol(0)

!call lplot6(3,2,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%gm3(2),n_psi_out-1,'gm3')
!call lincol(1)
!call lplot6(3,2,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%gm3(2),-size(equil_in(1)%profiles_1d%psi)+1,' ')
!call lincol(0)

!call lplot6(3,3,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%gm4(2),n_psi_out-1,'gm4')
!call lincol(1)
!call lplot6(3,3,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%gm4(2),-size(equil_in(1)%profiles_1d%psi)+1,' ')
!call lincol(0)

!call lplot6(2,2,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%gm5(2),n_psi_out-1,'gm5')
!call lincol(1)
!call lplot6(2,2,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%gm5(2),-size(equil_in(1)%profiles_1d%psi)+1,' ')
!call lincol(0)

!call lplot6(3,2,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%gm6(2),n_psi_out-1,'gm6')
!call lincol(1)
!call lplot6(3,2,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%gm6(2),-size(equil_in(1)%profiles_1d%psi)+1,' ')
!call lincol(0)

!call lplot6(2,3,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%gm7(2),n_psi_out-1,'gm7')
!call lincol(1)
!call lplot6(2,3,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%gm7(2),-size(equil_in(1)%profiles_1d%psi)+1,' ')
!call lincol(0)

!call lincol(1)
!call lplot6(3,3,equil_in(1)%profiles_1d%psi(2) ,equil_in(1)%profiles_1d%vprime(2),+size(equil_in(1)%profiles_1d%psi)-1,'Vprime')
!call lincol(0)
!call lplot6(3,3,equil_out(1)%profiles_1d%psi(2),equil_out(1)%profiles_1d%vprime(2),-(n_psi_out-1),'Vprime')


call lincol(0)
call lincol(0)
call lincol(0)
call lincol(0)
call finplt


allocate(equil_out(1)%profiles_1d%r_inboard(n_psi_out))
allocate(equil_out(1)%profiles_1d%r_outboard(n_psi_out))
do i=1,n_psi_out
  ileft  = (i-1)*n_tht_out + n_tht_out /2.e0_R8 + 1
  iright = (i-1)*n_tht_out + 1  
  equil_out(1)%profiles_1d%r_inboard(i)   = RRflux(1,ileft)
  equil_out(1)%profiles_1d%r_outboard(i)  = RRflux(1,iright)
enddo

allocate(equil_out(1)%profiles_1d%ftrap(n_psi_out))
equil_out(1)%profiles_1d%ftrap = 1.e0_R8 - fraction_circ


WRITE(*,*) ' COORD_SYS : ',n_psi_out,nchi

!allocate(equil_out(1)%coord_sys%grid%dim1(npsi))
!allocate(equil_out(1)%coord_sys%grid%dim2(nchi))

allocate(equil_out(1)%coord_sys%position%R(n_psi_out,nchi))
allocate(equil_out(1)%coord_sys%position%Z(n_psi_out,nchi))

!allocate(equil_out(1)%coord_sys%g_11(npsi,nchi))
!allocate(equil_out(1)%coord_sys%g_12(npsi,nchi))
!allocate(equil_out(1)%coord_sys%g_22(npsi,nchi))
!allocate(equil_out(1)%coord_sys%g_33(npsi,nchi))
!allocate(equil_out(1)%coord_sys%jacobian(npsi,nchi))

!equil_out(1)%coord_sys%grid%dim1(1:npsi) = cs(1:npsi)**2
!equil_out(1)%coord_sys%grid%dim2(1:nchi) = chi(1:nchi)

do i=1,n_psi_out
  do j=1,nchi
    node = (i-1)*nchi + j

    equil_out(1)%coord_sys%position%R(i,j)    = RRflux(1,node)
    equil_out(1)%coord_sys%position%Z(i,j)    = ZZflux(1,node)

!    equil_out(1)%coord_sys%g_11(i,j) = gem11(index)    * (R_mag * B_mag)**2
!    equil_out(1)%coord_sys%g_12(i,j) = gem12(index)    *  B_mag
!    equil_out(1)%coord_sys%g_22(i,j) = gem22(index)    /  R_mag**2
!    equil_out(1)%coord_sys%g_33(i,j) = 1./gem33(index) /  R_mag**2
  enddo
enddo

!equil_out(1)%coord_sys%g_11(1,1:nchi) = 0.
!equil_out(1)%coord_sys%g_33(1,1:nchi) = 1. / R_mag**2
!equil_out(1)%coord_sys%R(1,1:nchi)    = R_mag
!equil_out(1)%coord_sys%Z(1,1:nchi)    = Z_mag

!DPC
equil_out(1)%profiles_1d%psi=-2.0_R8*itm_pi*equil_out(1)%profiles_1d%psi
equil_out(1)%profiles_1d%psi=equil_out(1)%profiles_1d%psi-equil_out(1)%profiles_1d%psi(1)

return
end

