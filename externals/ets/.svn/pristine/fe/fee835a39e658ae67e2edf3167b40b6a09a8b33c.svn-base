module helena_grid
  use itm_types
  implicit none
  integer             :: nr, np
  real (r8),pointer      :: RR(:,:), ZZ(:,:), PSI(:,:)
  real (r8)              :: R_min, R_max, Z_min, Z_max
endmodule helena_grid

module helena_boundary
  use itm_types
  implicit none
  integer             :: mf, n_bnd
  real (r8), allocatable :: fr(:), R_bnd(:), Z_bnd(:)
  real (r8)              :: Bgeo, Rgeo, Zgeo, amin, eps, ellip, tria_u, tria_l, quad_u, quad_l
  real (r8)              :: Reast, Rwest
end module helena_boundary

module helena_profiles
  use itm_types
  implicit none
  integer             :: n_prof
  real (r8),allocatable  :: dp_dpsi(:),fdf_dpsi(:),p_psi(:),f_psi(:),zjz_psi(:),q(:)
  real (r8)              :: p_bnd
endmodule helena_profiles

module helena_elements
  use itm_types
  implicit none
  integer             :: n_elms, n_nodes
  integer,allocatable :: elm_list(:,:), index_list(:), index_inverse(:)
  real (r8)              :: RS(4,2)
  integer             :: IJ(4,2)
  real (r8)              :: H(4,4,4,4),   HS(4,4,4,4),  HR(4,4,4,4)
  real (r8)              :: HRS(4,4,4,4), HRR(4,4,4,4), HSS(4,4,4,4)
end module helena_elements

module helena_matrix
  use itm_types
  implicit none
  integer             :: lda, n_matrix, n_diag
  real (r8),allocatable  :: AA(:,:), BB(:)
end module helena_matrix

module helena_axis
  use itm_types
  implicit none
  real (r8)              :: R_axis, Z_axis, ps_axis, r_ax, s_ax
  real (r8)              :: q_axis, B_axis, CRR_axis, CZZ_axis
  integer             :: ielm_ax
end module helena_axis

module helena_map
  use itm_types
  implicit none
  integer :: n_psi_map, n_chi_map
  real (r8), allocatable :: gem11(:,:), gem12(:,:), gem22(:,:), gem33(:,:)
  real (r8), allocatable :: q_map(:), dq_map(:), p_map(:), dp_map(:), f_map(:), df_map(:)
  real (r8), allocatable :: RR_map(:,:),ZZ_map(:,:)
end module helena_map

module helena_surfaces
  use itm_types
  implicit none
  integer   :: n_pieces_max
  parameter (n_pieces_max = 1000)

  type type_surface
    integer :: n_pieces
    integer :: elm(n_pieces_max)
    real (r8)    :: r(4,n_pieces_max), s(4,n_pieces_max)   ! 4 variables per line piece of the flux surface
  endtype

  integer             :: n_psi
  real (r8) ,allocatable :: psi_values(:)
  type (type_surface), allocatable :: flux_surfaces(:)

end module helena_surfaces

module helena_phys
  use itm_types
  implicit none
  real (r8) :: beta_p, beta_t, beta_n, current, volume, area
end module helena_phys

module helena_Gauss_4
  use itm_types
  implicit none
  real (r8) :: xgauss(4),wgauss(4)
  data xgauss /-0.861136311594053, -0.339981043584856, 0.339981043584856,  0.861136311594053 /
  data wgauss / 0.347854845137454,  0.652145154862546, 0.652145154862546,  0.347854845137454 /
end module helena_Gauss_4

module helena_constants
  use itm_types
  implicit none
  real (r8)       :: PI, MU_zero
  parameter ( PI      = 3.14159265358979323846264338327950288419716939937510 )
  parameter ( MU_zero = PI * 4.d-7)
end module helena_constants

module helena21_mod

contains

function fdfdpsi(psi_n)
use helena_profiles
use itm_types
implicit none
real (r8)    :: fdfdpsi, psi_n, dpsi
integer :: n_int

dpsi = 1. /float(n_prof - 1)

n_int = max(int((n_prof-1)*psi_n) + 1,1)
n_int = min(n_int,n_prof-1)

fdfdpsi = ( fdf_dpsi(n_int) + (psi_n - dpsi * (n_int-1))/dpsi * (fdf_dpsi(n_int+1)-fdf_dpsi(n_int)) )

return
end function fdfdpsi

function dpdpsi(psi_n)
use helena_profiles
use itm_types
implicit none
real (r8)    :: dpdpsi, psi_n, dpsi, tmp
integer :: n_int

dpsi = 1. /float(n_prof - 1)

n_int = max(int((n_prof-1)*psi_n) + 1,1)
n_int = min(n_int,n_prof-1)

dpdpsi = ( dp_dpsi(n_int) + (psi_n - dpsi * (n_int-1))/dpsi * (dp_dpsi(n_int+1)-dp_dpsi(n_int)) )

return
end function dpdpsi

function fdia(psi_n)
use helena_profiles
use helena_axis
use helena_boundary
use itm_types
implicit none
real (r8)    :: F2int, F0, fdia, psi_n, dpsi
integer :: n_int

if (psi_n .lt. 0.) write(*,*) ' FDIA : ',psi_n

dpsi = 1. /float(n_prof - 1)

n_int = max(int((n_prof-1)*psi_n) + 1,1)
n_int = min(n_int,n_prof-1)

F2int = ( f_psi(n_int) + (psi_n - dpsi * (n_int-1))/dpsi * (f_psi(n_int+1)-f_psi(n_int)) )

F0 = Rgeo * Bgeo

fdia = sqrt( abs(F0*F0 + 2. * F2int) )

return
end function fdia

function pressure(psi_n)
use helena_profiles
use helena_axis
use itm_types
implicit none
real (r8)    :: pressure, psi_n, dpsi
integer :: n_int

dpsi = 1. /float(n_prof - 1)

n_int = max(int((n_prof-1)*psi_n) + 1,1)
n_int = min(n_int,n_prof-1)

pressure = ( p_psi(n_int) + (psi_n - dpsi * (n_int-1))/dpsi * (p_psi(n_int+1)-p_psi(n_int)) )

return
end function pressure

function current_profile(psi_n)
use helena_profiles
use itm_types
implicit none
real (r8)    :: current_profile, psi_n, dpsi
integer :: n_int

dpsi = 1. /float(n_prof - 1)

n_int = max(int((n_prof-1)*psi_n) + 1,1)
n_int = min(n_int,n_prof-1)

current_profile = zjz_psi(n_int) + (psi_n - dpsi * (n_int-1))/dpsi * (zjz_psi(n_int+1)-zjz_psi(n_int))

return
end function current_profile
!****************************************************************************************************************

subroutine helena21(R_bnd_in,Z_bnd_in,n_bnd_in,                                                                  &
                    Rgeo_in,Zgeo_in, amin_in,                                                                    &
                    ellip_in,tria_u_in,tria_l_in,quad_u_in,quad_l_in,                                            &
                    Bgeo_in,                                                                                     &
                    psi_in, pprime_in, ffprime_in,                                                               &
                    pressure_in, fdia_in, current_in, n_prof_in, iopt_p, iopt_f,                                 &
                    nr_grid, np_grid,                                                                            &
                    psi_out, pprime_out, ffprime_out,                                                            &
                    pressure_out, fdia_out, current_out, qprof_out, vprime,                                      &
                    fraction_circ, moments, n_moments,                                                           &
                    surface_powers, surface_integrals, n_var_surfaces, n_int_surfaces,                           &
                    volume_powers, volume_integrals, n_var_volumes, n_int_volumes, n_psi_out, n_tht_out,         &
                    amin_out, Rgeo_out, Zgeo_out,                                                                &
                    area_out, volume_out, betap_out, xip_out, xli_out, beta_out,                                 &
                    R_axis_out, Z_axis_out, B_axis_out, psi_axis_out, psi_bnd_out,                               &
                    RRflux,ZZflux,PSflux)

!****************************************************************************************************************
!
! Helena version 2.0
!
! - removed all normalisations. Quantities are in S.I. units.
! - improved(?) the tracing of flux surfaces (no iterations, should be even more reliable)
!
!
! author  : Guido Huysmans (Association Euratom-CEA)
! date    : 05/02/2009
! version : 0.8
! status  : slowly progressing ...
!
!
! to be done :
!        - adjust radial integration of dpdpsi and fdfdpsi to changing ps_axis (done?)
!        - change modules to derived types and arguments to subroutines
!        - do the mapping to straight field line coordinates (generalised including Boozer, Hamada)
!        - finding all the new bugs...
!
!    !!!    - fdia and pressure spline if available, otherwise integrate from spline pprime,ffprime
!
!****************************************************************************************************************
use helena_grid
use helena_boundary
use helena_profiles
use helena_matrix
use helena_axis
use helena_elements
use helena_surfaces
use helena_phys
use helena_map
use helena_constants

use itm_types
implicit none

integer          :: n_int_surfaces, n_int_volumes, n_var_surfaces, n_var_volumes, n_moments, n_psi_out, n_tht_out, ioption
real (r8)           :: RRflux(4,*),ZZflux(4,*),PSflux(4,*)
real (r8)           :: R_bnd_in(*),Z_bnd_in(*),pressure_in(*),fdia_in(*),current_in(*),psi_in(*), pprime_in(*), ffprime_in(*)
real (r8)           :: Bgeo_in,Rgeo_in,Zgeo_in,amin_in,ellip_in,tria_u_in,tria_l_in,quad_u_in,quad_l_in
integer          :: n_bnd_in, n_prof_in, nr_grid, np_grid, iopt_p, iopt_f
real (r8)           :: surface_powers(n_var_surfaces,n_int_surfaces),surface_integrals(n_psi_out,n_int_surfaces)
real (r8)           :: volume_powers(n_var_volumes,n_int_volumes),   volume_integrals(n_psi_out,n_int_volumes)
real (r8)           :: psi_n, psi_out(*), pressure_out(*), fdia_out(*), current_out(*), pprime_out(*), ffprime_out(*)
real (r8)           ::  qprof_out(*), vprime(*), fraction_circ(*),moments(n_moments,*)
integer          :: i, j, index, n_iter, ifail, inode, n_chi, ileft, iright
real (r8)           :: s, error_iteration, error_large, error_current, small, ss, psi_plot
real (r8)           :: fdf_error, t1, t2, amix, fmix, ymin, ymax
logical          :: solve_only, rhs_only
!real (r8)           :: pressure, fdia, fdfdpsi, dpdpsi, current_profile
real (r8)           :: amin_out, Rgeo_out, Zgeo_out, area_out, volume_out, betap_out, xip_out, xli_out, beta_out
real (r8)           :: R_axis_out, Z_axis_out, B_axis_out, psi_axis_out, psi_bnd_out
real (r8),allocatable :: rho_out(:), xplot(:), qplot(:), pplot(:)
real (r8)           :: qtmp

call begplt('helena.ps')

!----------------------------------------- initialise
write(*,*) '**********************************************'
write(*,*) '*          HELENA 2.0 (alpha_21)             *'
write(*,*) '**********************************************'

write(*,*) ' nr, np        : ',nr_grid,np_grid
write(*,*) ' n_var_surfaces,n_int_surfaces : ',n_var_surfaces,n_int_surfaces
write(*,*) ' n_var_volumes, n_int_volumes  : ',n_var_volumes, n_int_volumes
write(*,*) ' n_psi, n_tht  : ',n_psi_out,n_tht_out

mf     = 128
nr     = nr_grid
np     = 2*int((np_grid+1)/2)
n_iter = 50
n_chi  = 64
error_iteration = 1.d-6
error_large     = 1.d-4
error_current   = 1.d-4

Bgeo = Bgeo_in

write(*,*) ' Bgeo : ',Bgeo

n_bnd = n_bnd_in
allocate(R_bnd(n_bnd),Z_bnd(n_bnd))
R_bnd(1:n_bnd) = R_bnd_in(1:n_bnd)
Z_bnd(1:n_bnd) = Z_bnd_in(1:n_bnd)

call fshape

n_prof = n_prof_in


call initialise_profiles(n_prof_in,iopt_p,iopt_f,psi_in, pprime_in, ffprime_in,pressure_in, fdia_in, current_in)

call initialise_grid

call initialise_elements

solve_only = .false.
rhs_only   = .false.
amix = 0.e0_R8
fmix = 0.e0_R8

!---------------------------------------- iteration for current profile
if (iopt_f .eq. 3) then

  do i=1, n_iter

    call GS_solve(n_iter,rhs_only,solve_only,error_large,amix,ifail)

    if (ifail .ne. 0 ) then
      amix = (1.e0_R8 + amix) /2.e0_R8
      write(*,'(A,f8.3)') ' increasing amix : ',amix
    endif

    call update_fdf(fmix,fdf_error)

    write(*,'(A,e14.6)') ' fdf_error : ',fdf_error
    if (fdf_error .lt. error_current) exit

  enddo

endif
  
n_iter     = 100
solve_only = .false.
rhs_only   = .false.

call GS_solve(n_iter,rhs_only,solve_only,error_iteration,amix,ifail)

!-------------------------------------- output quantities
call phys_values

!call plot_solution

n_psi = n_psi_out - 1           ! n_psi does not count the axis (only the flux surfaces traced)

if (allocated(psi_values))    deallocate(psi_values)
if (allocated(flux_surfaces)) deallocate(flux_surfaces)

allocate(psi_values(n_psi))

small = 1.d-8
write(*,*) 'main : i ss psi_values'
do i=1,n_psi
  ss = float(i)/float(n_psi)
  psi_values(i) = ps_axis - (1.e0_R8 - small) * ss*ss * ps_axis
  write(*,'(A,i4,2e14.6)') ' main : ',i,ss,psi_values(i)
enddo

call find_flux_surfaces
call plot_flux_surfaces

!write(*,*) ' size surface_integrals : ',size(surface_integrals)
!call fluxsurface_integrals(surface_powers,n_int_surfaces,n_var_surfaces,surface_integrals)
!write(*,*) ' fluxsurface integrals (1) : '
!do i=1,n_psi_out
!  write(*,'(i5,15e12.4)') i,surface_integrals(i,1:n_int_surfaces)
!enddo

write(*,*) 'profiles out : i psi_n psi_out fdia_out'
do i=1,n_psi+1
  if (i.eq.1) then
    psi_n = 0.e0_R8
    psi_out(i)      = - ps_axis + psi_in(n_prof_in) 
  else
    psi_n           = -(psi_values(i-1) - ps_axis) / ps_axis
    psi_out(i)      = - psi_values(i-1) + psi_in(n_prof_in) 
  endif
  pprime_out(i)   = dpdpsi(psi_n)
  ffprime_out(i)  = fdfdpsi(psi_n)
  pressure_out(i) = pressure(psi_n)
  fdia_out(i)     = fdia(psi_n)
  current_out(i)  = current_profile(psi_n)

  write(*,'(A,i4,8e14.6)') 'profiles out : ',i,psi_n,psi_out(i),fdia_out(i)
enddo

!call export_helena

call helena_remesh(n_psi_out,n_tht_out,RRflux,ZZflux,PSflux)

call helena_flux_surface_integrals(n_psi_out,n_tht_out,RRflux,ZZflux,PSflux, &
                                   surface_powers,n_int_surfaces,n_var_surfaces,surface_integrals, &
				   qprof_out, vprime) 

do i=1, n_psi_out
  qprof_out(i)   = fdia_out(i) * qprof_out(i) / (2.e0_R8*PI)
enddo

write(*,*) ' fluxsurface integrals (2) : ',n_int_surfaces
do i=1,n_psi_out
  write(*,'(i5,15e12.4)') i,surface_integrals(i,1:n_int_surfaces),qprof_out(i),vprime(i)
enddo
      
!call helena_mapping(RRflux,ZZflux,PSflux,n_tht_out,n_chi)

call helena_volume_integrals(n_psi_out,n_tht_out,RRflux,ZZflux,PSflux, &
                             volume_powers,volume_integrals,n_var_volumes,n_int_volumes,n_psi+1)

allocate(rho_out(n_psi_out))

write(*,*) 'i psi_out rho_out volume_integrals(1:3) qprof_out qtmp abs(qtmp-qprof_out)/qprof_out fdia_out'
do i=1, n_psi_out
  
  rho_out(i) = sqrt(volume_integrals(i,2)/ (2.e0_R8*PI) / (PI * Bgeo_in))          ! B at magnetic axis?
  
  qtmp = 0.e0_R8
  if ( (i .gt. 1) .and. (i .lt. n_psi_out)) then
    qtmp = -(volume_integrals(i+1,2) - volume_integrals(i-1,2)) / (4.*PI*PI) / (psi_out(i+1)-psi_out(i-1))
  endif
  
  write(*,'(i5,20f12.6)') i,psi_out(i),rho_out(i),volume_integrals(i,1:3),qprof_out(i),qtmp,abs(qtmp-qprof_out(i))/qprof_out(i),&
  fdia_out(i)
  
enddo


!call helena_moments(RRflux,ZZflux,n_psi_out,n_tht_out,moments,n_moments)

call helena_circulating(n_psi_out,n_tht_out,RRflux,ZZflux,PSflux,fraction_circ)
fraction_circ(1) = 1.e0_R8

area_out   = area
volume_out = volume
betap_out  = beta_p
xip_out    = current
xli_out    = 0.e0_R8
beta_out   = beta_t
R_axis_out = R_axis
Z_axis_out = Z_axis
B_axis_out = B_axis
psi_axis_out = ps_axis
psi_bnd_out  = psi_out(n_psi_out)
amin_out     = amin
Rgeo_out     = Rgeo
Zgeo_out     = Zgeo


allocate(xplot(2*n_psi_out),pplot(2*n_psi_out),qplot(2*n_psi_out))

do i=1,n_psi_out
  ileft  = (i-1)*n_tht_out + n_tht_out /2.e0_R8 + 1
  iright = (i-1)*n_tht_out + 1  
  xplot(n_psi_out+i)   = RRflux(1,ileft)
  xplot(n_psi_out-i+1) = RRflux(1,iright)
  pplot(n_psi_out+i)   = pressure_out(i)
  pplot(n_psi_out-i+1) = pressure_out(i)
  qplot(n_psi_out+i)   = qprof_out(i)
  qplot(n_psi_out-i+1) = qprof_out(i)
enddo

call lincol(0)
call lplot(3,2,1,xplot,pplot,2*n_psi_out,1,'pressure [Pa]',13,'R [m]',5,' ',1)
!DPC infinite loop? call lplot(3,3,1,xplot,qplot,2*n_psi_out,1,'q-profile',9,'R [m]',5,' ',1)

call lblbot('HELENA_2(alpha)',15)

call finplt

deallocate(AA,BB,fr,dp_dpsi,fdf_dpsi,p_psi,f_psi)
deallocate(elm_list, index_list, index_inverse)
deallocate(RR, ZZ, PSI)
!DPC
deallocate(flux_surfaces,R_bnd,Z_bnd,psi_values,zjz_psi)

end subroutine helena21

subroutine helena_mapping(RR,ZZ,PSI,n_tht,n_chi)
!-----------------------------------------------------------------------
! SUBROUTINE TO CALCULATE THE METRIC COEFFICIENTS NEEDED FOR CASTOR/MISHKA
! a straight field line flux surface coordinate system with a regular
! toroidal angle.
!-----------------------------------------------------------------------
use helena_constants
use helena_axis
use helena_profiles
use helena_surfaces
use helena_map
use helena_Gauss_4
use itm_types
implicit none

real (r8)               :: RR(4,*),ZZ(4,*),PSI(4,*)
real (r8), allocatable  :: CCHI(:,:), SCHI(:,:), XCHI(:,:), YCHI(:,:), CHIKN(:)
real (r8), allocatable  :: s_values(:)
integer, allocatable :: IJCHI(:,:)
logical              :: CHIN

real (r8)               :: sumq, sumqr, zsumq, zsumqr, psi_n, psir, psirr, ejac, er, bigr
real (r8)               :: s, maxerr, x, xr, xs, xrs, xrr, xss, y, yr, ys, yrs, yrr, yss
real (r8)               :: dum, dum1, dum2, dum3, dum4, QQ, DQQ, RB, DRB
real (r8)               :: DCHI, CHIR, CHIS, CHIRS, CHIRR, PSIX, PSIY, CHIX, CHIY, ZCHI
real (r8)               :: errj, serr, cerr, chiss, ejac2, A0, A1, A2, A3, S1, S2, S3
integer              :: n_tht, n_chi, i,j,k, i_elm, node1, node2, node3, node4, NO, NOM, NOP
integer              :: jbase, n1, n2, n3, n4, ierr, jerr, ifail
!real (r8)               :: fdia, fdfdpsi
real (r8) :: psi_tmp,psir_tmp,psis_tmp,psirs_tmp,psirr_tmp,psiss_tmp

write(*,*) '********************************'
write(*,*) '*        helena mapping        *'
write(*,*) '********************************'
write(*,*) ' n_tht, n_chi : ',n_tht, n_chi

!---------------------------------------------------------------------
MAXERR = -1.d20


allocate(s_values(n_psi+1),cchi(4,(n_psi+1)*n_tht),schi((n_psi+1),n_chi),xchi((n_psi+1),n_chi),ychi((n_psi+1),n_chi))
allocate(chikn(n_chi),ijchi((n_psi+1),n_chi))
allocate(q_map(n_psi+1),dq_map(n_psi+1))

do i=1,n_psi
  s_values(i) = float(i)/float(n_psi)
enddo

!------------------------------------------ - Q PROFILE ----------------
!                                           - DQ/DS PROFILE
!                                           - CHI VALUES AT THETA NODES
do i = 2, n_psi+1

  SUMQ  = 0.e0_R8
  SUMQR = 0.e0_R8
  psi_n = s_values(i-1)**2

  PSIR  = - ps_axis * 2.e0_R8*s_values(i-1) / (2.e0_R8*REAL(n_psi))
  PSIRR = - ps_axis * 2.e0_R8               / (2.e0_R8*REAL(n_psi))**2

  do j = 1, n_tht - 1

    node1 = (i-2)*n_tht + j
    node2 = node1 + 1
    node3 = node2 + n_tht
    node4 = node1 + n_tht
    
    write(*,'(A,i5,4e16.8)') ' node 1 : ',node1,PSI(:,node1)
    write(*,'(A,i5,4e16.8)') ' node 2 : ',node2,PSI(:,node2)
    write(*,'(A,i5,4e16.8)') ' node 3 : ',node3,PSI(:,node3)
    write(*,'(A,i5,4e16.8)') ' node 4 : ',node4,PSI(:,node4)

    do k = 1,4

      S = XGAUSS(K)

      CALL INTERP(RR(1,node1),RR(1,node2),RR(1,node3),RR(1,node4),+1.E0_R8,S, X,XR,XS,XRS,XRR,XSS)
      CALL INTERP(ZZ(1,node1),ZZ(1,node2),ZZ(1,node3),ZZ(1,node4),+1.E0_R8,S, Y,YR,YS,YRS,YRR,YSS)

CALL INTERP(PSI(1,node1),PSI(1,node2),PSI(1,node3),PSI(1,node4),1.e0_R8,S,psi_tmp,psir_tmp,psis_tmp,psirs_tmp,psirr_tmp,psiss_tmp)
  write(*,'(A,i5,12e12.4)') ' 1 : ',i,s_values(i-1),s,ps_axis,psi_tmp,psir_tmp,PSIR,psirr_tmp,PSIRR

      EJAC  = XR*YS - XS*YR
      ER    = XRR*YS + XR*YRS - XRS*YR - XS*YRR
      BIGR  = X
      SUMQ  = SUMQ  - WGAUSS(K) * EJAC / ( BIGR * ABS(PSIR))
      SUMQR = SUMQR + PSIRR * EJAC / ((PSIR**2)*BIGR)* WGAUSS(K)
      SUMQR = SUMQR - ER / (BIGR*PSIR)  	     * WGAUSS(K)
      SUMQR = SUMQR + EJAC * XR / ((BIGR**2)*PSIR)   * WGAUSS(K)

    enddo

    CCHI(1,(I-1)*n_tht+J+1) = SUMQ
    CCHI(2,(I-1)*n_tht+J+1) = SUMQR

    CALL INTERP(RR(1,node1),RR(1,node2),RR(1,node3),RR(1,node4),+1.e0_R8,1.e0_R8,X,XR,XS,XRS,XRR,XSS)
    CALL INTERP(ZZ(1,node1),ZZ(1,node2),ZZ(1,node3),ZZ(1,node4),+1.e0_R8,1.e0_R8,Y,YR,YS,YRS,YRR,YSS)


    CALL INTERP(PSI(1,node1),PSI(1,node2),PSI(1,node3),PSI(1,node4),  &
         1.e0_R8,1.e0_R8,psi_tmp,psir_tmp,psis_tmp,psirs_tmp,psirr_tmp,psiss_tmp)
    write(*,'(A,i5,12e12.4)') ' end : ',i,s_values(i-1),ps_axis,psi_tmp,  &
         psir_tmp,PSIR,psirr_tmp,PSIRR

  
    EJAC   = XR*YS - XS*YR
    ER     = XRR*YS + XR*YRS - XRS*YR - XS*YRR
    BIGR   = X

    ZSUMQ  = - EJAC / ( BIGR * ABS(PSIR))
    ZSUMQR = + PSIRR * EJAC / (PSIR**2 *BIGR) - ER / (BIGR*PSIR) + EJAC*XR/((BIGR**2) * PSIR)

    CCHI(3,(I-1)*n_tht+J+1) = ZSUMQ
    CCHI(4,(I-1)*n_tht+J+1) = ZSUMQR

  enddo

  CCHI(1,(I-1)*n_tht+1) = 0.e0_R8
  CCHI(2,(I-1)*n_tht+1) = 0.e0_R8

  node1 = (i-1)*n_tht + 1
  node2 = node1 + 1
  node3 = node2 + n_tht
  node4 = node1 + n_tht

  CALL INTERP(RR(1,node1),RR(1,node2),RR(1,node3),RR(1,node4),+1.e0_R8,-1.e0_R8,X,XR,XS,XRS,XRR,XSS)
  CALL INTERP(ZZ(1,node1),ZZ(1,node2),ZZ(1,node3),ZZ(1,node4),+1.e0_R8,-1.e0_R8,Y,YR,YS,YRS,YRR,YSS)

  EJAC   = XR*YS - XS*YR
  ER     = XRR*YS + XR*YRS - XRS*YR - XS*YRR
  BIGR   = X

  ZSUMQ  = - EJAC / (BIGR * ABS(PSIR))
  ZSUMQR = + PSIRR * EJAC / (PSIR**2 *BIGR) - ER / (BIGR*PSIR) + EJAC*XR/((BIGR**2) * PSIR)

  CCHI(3,(I-1)*n_tht+1) =  ZSUMQ
  CCHI(4,(I-1)*n_tht+1) =  ZSUMQR

  q_map(i)  = 0.5e0_R8/PI *   SUMQ  * fdia(psi_n)
  dq_map(i) = 0.5e0_R8/PI * ( SUMQR * fdia(psi_n) + SUMQ*fdfdpsi(psi_n)/fdia(psi_n) /(2.e0_R8*(n_psi-1)))

  write(*,*) i,q_map(i),dq_map(i)

enddo

q_map(1)  = fdia(0.e0_R8) /(2.e0_R8*SQRT(CRR_axis*CZZ_axis)*R_axis)
dq_map(1) = 0.e0_R8

do i = 2, n_psi

  psi_n = s_values(i)**2

  do j = 1, n_tht

    DUM = CCHI(1,i*n_tht)

    NO  = (i-1)*n_tht + j

    CCHI(1,NO) = 2.e0_R8*PI*CCHI(1,NO) / DUM
    DUM2       = CCHI(2,I*n_tht)
    CCHI(2,NO) = 2.e0_R8*PI*CCHI(2,NO) / DUM
    CCHI(3,NO) = 2.e0_R8*PI*CCHI(3,NO) / DUM
    CCHI(4,NO) = 2.e0_R8*PI*CCHI(4,NO) / DUM

    QQ  = q_map(i)
    DQQ = dq_map(i)
    RB  = fdia(psi_n)
    DRB = fdfdpsi(psi_n) / RB  /  (2*(n_psi-1))

    CCHI(2,NO)= +(DQQ/QQ - DRB/RB) * CCHI(1,NO) - CCHI(2,NO)
    CCHI(4,NO)= +(DQQ/QQ - DRB/RB) * CCHI(3,NO) - CCHI(4,NO)

  enddo

enddo

WRITE(*,'(A,f8.4)') '  Q ON AXIS     = ',q_map(1)
WRITE(*,'(A,f8.4)') '  Q AT BOUNDARY = ',q_map(n_psi)

!--------------------------- DETERMINE POSITIONS OF EQUIDISTANT CHI'S --
!                            AND CALCULATE MATRIX ELEMENTS -------------

allocate(gem11(n_psi,n_chi),gem12(n_psi,n_chi),gem22(n_psi,n_chi),gem33(n_psi,n_chi))
allocate(RR_map(n_psi,n_chi), ZZ_map(n_psi,n_chi))

do j=1,n_chi
  CHIKN(J) = 2.e0_R8 * PI * REAL(J-1)/REAL(n_chi)
enddo

do i=1,n_psi-1

  psi_n = s_values(i+1)**2
  PSIR  = - ps_axis * 2.e0_R8 * s_values(i+1) / (2.e0_R8*REAL(n_psi))

!-------------------------- FIRST POINT IS KNOWN -----------------------
  k = 1
  s = -1.e0_R8
  SCHI(i,1)  = S
  IJCHI(i,1) = K

  node1 = (i-1)*n_tht + k
  node2 = node1 + 1
  node3 = node2 + n_tht
  node4 = node1 + n_tht

  CALL INTERP(RR(1,node1),RR(1,node2),RR(1,node3),RR(1,node4),1.e0_R8,S,XCHI(i,1),XR,XS,XRS,XRR,XSS)
  CALL INTERP(ZZ(1,node1),ZZ(1,node2),ZZ(1,node3),ZZ(1,node4),1.e0_R8,S,YCHI(i,1),YR,YS,YRS,YRR,YSS)
  CALL INTERP(CCHI(1,node1),CCHI(1,node2),CCHI(1,node3),CCHI(1,node4),1.e0_R8,S,DCHI,CHIR,CHIS,CHIRS,CHIRR,CHISS)
  
  CALL INTERP(PSI(1,node1),PSI(1,node2),PSI(1,node3),PSI(1,node4),1.e0_R8,S,psi_tmp,psir_tmp,psis_tmp,psirs_tmp,psirr_tmp,psiss_tmp)

  write(*,'(A,i5,8e12.4)') ' 2 : ',i,s_values(i+1),ps_axis,psi_tmp,psir_tmp

!-----------------------------------------------------------------------
  EJAC  = (XR*YS-XS*YR)
  EJAC2 = EJAC**2

!----------------------------- DATA FOR VECTOR PLOT TO FILE 21 -----
  PSIX = PSIR * YS / EJAC
  PSIY = - PSIR * XS / EJAC
  CHIX = (CHIR * YS - CHIS * YR) / EJAC
  CHIY = (-CHIR * XS + CHIS * XR)/ EJAC
!        WRITE(21,61) SQRT(ZPSI),DCHI,XCHI(NO),YCHI(NO),PSIX,PSIY,CHIX,CHIY

!----------------------------------------------------------------------
  GEM11(i,1) = PSIR**2 * (XS**2 + YS**2) / EJAC2
  GEM12(i,1) = ( CHIR * PSIR * (XS**2 + YS**2) - CHIS * PSIR * (XR*XS + YR*YS) ) / EJAC2
  GEM33(i,1) = XCHI(i,1)
  RR_map(i,1)= XCHI(i,1)
  ZZ_map(i,1)= YCHI(i,1)

!------------------------------------ CHECK JACOBIAN -----------------
  DUM1 = fdia(psi_n)**2 / (q_map(i)**2 * GEM33(i,1))
  DUM2 = GEM11(i,1) * (CHIX**2 + CHIY**2)
  DUM3 = GEM12(i,1)
  DUM4 = DUM2 - DUM3*DUM3
  ERRJ = ABS(DUM4-DUM1)

  IF (ERRJ.GT.MAXERR) THEN
    MAXERR=ERRJ
    IERR = I
    JERR = J
    SERR = s_values(i)
    CERR = DCHI
  ENDIF

!---------------------------------------------------------------------
  JBASE = 2

  do K = 1,n_tht-1

    CHIN =.FALSE.

    do J = JBASE,n_chi

      ZCHI = CHIKN(J)

      IF (((CCHI(1,(I-1)*n_tht+K).LE.ZCHI).AND.(CCHI(1,(I-1)*n_tht+K+1) .GE.ZCHI)) ) THEN

  	CHIN = .TRUE.
  	NOM = (I-1)*n_tht + K
  	NOP = NOM + 1
  	A3 = (CCHI(1,NOM)+CCHI(3,NOM)-CCHI(1,NOP)+CCHI(3,NOP))/4.e0_R8
  	A2 = (- CCHI(3,NOM) + CCHI(3,NOP))/4.e0_R8
  	A1=(-3*CCHI(1,NOM)-CCHI(3,NOM)+3*CCHI(1,NOP)-CCHI(3,NOP))/4.e0_R8
  	A0=( 2*CCHI(1,NOM)+CCHI(3,NOM)+2*CCHI(1,NOP)-CCHI(3,NOP))/4.e0_R8 - ZCHI

  	CALL SOLVP3(A0,A1,A2,A3,S,S2,S3,IFAIL)

  	IF (IFAIL.EQ.0) THEN

  	  SCHI(i,j) = S
  	  IJCHI(i,j) = K

  	  node1 = (I-1)*n_tht + K
  	  node2 = node1 + 1
  	  node3 = node2 + n_tht
  	  node4 = node1 + n_tht

  	  CALL INTERP(RR(1,node1),RR(1,node2),RR(1,node3),RR(1,node4),-1.e0_R8,S,XCHI(i,j),XR,XS,XRS,XRR,XSS)
  	  CALL INTERP(ZZ(1,node1),ZZ(1,node2),ZZ(1,node3),ZZ(1,node4),-1.e0_R8,S,YCHI(i,j),YR,YS,YRS,YRR,YSS)
  	  CALL INTERP(CCHI(1,node1),CCHI(1,node2),CCHI(1,node3),CCHI(1,node4),-1.e0_R8,S,DCHI,CHIR,CHIS,CHIRS,CHIRR,CHISS)

!-----------------------------------------------------------------
  	  EJAC  = (XR*YS-XS*YR)
  	  EJAC2 = EJAC**2

!-------------------------- DATA FOR VECTOR PLOT TO FILE 21 -----
          PSIX = PSIR * YS / EJAC
          PSIY = - PSIR * XS / EJAC
          CHIX = (CHIR * YS - CHIS * YR) / EJAC
          CHIY = (-CHIR * XS + CHIS * XR)/ EJAC

!          WRITE(21,61) SQRT(ZPSI),DCHI,XCHI(NO),YCHI(NO),PSIX,PSIY,CHIX,CHIY
!----------------------------------------------------------------

          GEM11(i,j) =  PSIR**2 * (XS**2 + YS**2) / EJAC2
          GEM12(i,j) = ( CHIR * PSIR * (XS**2 + YS**2) - CHIS * PSIR * (XR*XS + YR*YS) ) / EJAC2
          GEM33(i,j) = XCHI(i,j)**2
          RR_map(i,j) = XCHI(i,j)
          ZZ_map(i,j) = YCHI(i,j)

!------------------------------ CHECK JACOBIAN -----------------
          DUM1 = fdia(psi_n)**2 / (q_map(i)**2 * GEM33(i,j))	!!!!!! check if psi_n defined
          DUM2 = GEM11(i,j) * (CHIX**2 + CHIY**2)
          DUM3 = GEM12(i,j)
          DUM4 = DUM2 - DUM3*DUM3
          ERRJ = ABS(DUM4-DUM1)
          IF (ERRJ.GT.MAXERR) THEN
            MAXERR=ERRJ
            IERR = I
            JERR = J
            SERR = s_values(i)
            CERR = DCHI
          ENDIF
!---------------------------------------------------------------------
        ELSE
          WRITE(20,*) 'ERROR IN SOLVP3 I,J,K : ',I,J,K,S,s2,s3
          WRITE(*,*) A0,A1,A2,A3,ZCHI
          WRITE(*,*) CCHI(1,(I-1)*n_tht+K),CCHI(1,(I-1)*n_tht+K+1)
        ENDIF
      ELSEIF (CHIN) THEN
        JBASE = J
        exit
      ENDIF

    enddo
  enddo
enddo

WRITE(*,*)
WRITE(*,*) '***************************************************'
WRITE(*,'(3e12.4,2i5)') MAXERR,SERR,CERR,IERR,JERR
WRITE(*,*) '***************************************************'

n_psi_map = n_psi
n_chi_map = n_chi

RETURN
END subroutine helena_mapping


subroutine helena_moments(RRflux,ZZflux,nr_flux,np_flux,moments,n_moments)
!----------------------------------------------------------------------
! subroutine finds the Shafranov shift, ellipticity and triangularity
! as a function of radius (flux).
!
! moments(1,:) : minor radius (R_east - R_west) / 2.
! moments(2,:) : Shafranov shift (R_east + R_west)/2 - R_geo
! moments(3,:) : (Z_east - Z_west)/2. - Z_geo
! moments(4,:) : Z_top/((R_east - R_west)/2.)
! moments(5,:) : Z_low/((R_east - R_west)/2.)
! moments(6,:) : (R_top - (R_east + R_west)/2.)/((R_east - R_west)/2.)
! moments(7,:) : (R_low - (R_east + R_west)/2.)/((R_east - R_west)/2.)
!----------------------------------------------------------------------
use helena_constants
use helena_axis
use itm_types
implicit none
real (r8)            :: RRflux(4,*),ZZflux(4,*),moments(n_moments,*)
integer           :: nr_flux, np_flux, n_moments
real (r8) r,sum1,sum2,sumr,b02max,s,ws,rrg1,drrg1_dr,drrg1_ds,drrg1_drs,drrg1_drr,drrg1_dss,  &
     zzg1,dzzg1_dr,dzzg1_ds,dzzg1_drs,dzzg1_drr,r_geo,z_geo,a_minor,  &
     zzm,zzmr,zzp,zzpr,aa,bb,cc,det,r_top,dummy,z_top,  &
     r_east,r_west,z_east,z_west,r_mid,z_mid,radius
integer i,j,n1,n2,n3,n4,ngs



R_geo   =    ( RRflux(1,(nr_flux-1)*np_flux + 1) + RRflux(1,(nr_flux-1)*np_flux + np_flux/2 + 1)) /2.
Z_geo   =    ( ZZflux(1,(nr_flux-1)*np_flux + 1) + ZZflux(1,(nr_flux-1)*np_flux + np_flux/2 + 1)) /2.
a_minor = abs( RRflux(1,(nr_flux-1)*np_flux + 1) - RRflux(1,(nr_flux-1)*np_flux + np_flux/2 + 1)) /2.

write(*,*) ' moments : ',R_geo,Z_geo

moments(n_moments,1:nr_flux) = 0.e0_R8

moments(1,1) = 0.e0_R8
moments(2,1) = RRflux(1,1) - R_geo
moments(3,1) = ZZflux(1,1) - Z_geo
moments(4,1) = sqrt(CRR_axis / CZZ_axis)
moments(5,1) = sqrt(CRR_axis / CZZ_axis)
moments(6,1) = 0.e0_R8
moments(7,1) = 0.e0_R8

do i = 2, nr_flux

  do j = 1, np_flux - 1

    n1 = (i-1)*np_flux + j
    n2 = (i-1)*np_flux + j + 1

    if (ZZflux(3,n1)*ZZflux(3,n2) .le. 0.e0_R8) then
!------------------------------------- QUAD. EQ FOR S VALUE AT MINIMUM -

      ZZM  = ZZflux(1,n1)
      ZZMR = ZZflux(3,n1)
      ZZP  = ZZflux(1,n2)
      ZZPR = ZZflux(3,n2)
      AA =  3.e0_R8 * (ZZM + ZZMR - ZZP + ZZPR ) / 4.e0_R8
      BB =  ( - ZZMR + ZZPR ) / 2.e0_R8
      CC =  ( - 3.e0_R8*ZZM - ZZMR + 3.e0_R8*ZZP - ZZPR) / 4.e0_R8
      det = BB*BB - 4.e0_R8*AA*CC
      s  = ROOT(AA,BB,CC,DET,1.e0_R8)

      if (abs(s) .GT. 1.e0_R8) then
        S = ROOT(AA,BB,CC,DET,-1.e0_R8)
      endif

      call CUB1D(RRflux(1,n1),RRflux(3,n1),RRflux(1,n2),RRflux(3,n2),s,R_top,dummy)
      call CUB1D(ZZflux(1,n1),ZZflux(3,n1),ZZflux(1,n2),ZZflux(3,n2),s,Z_top,dummy)

      R_east  = RRflux(1,(i-1)*np_flux + 1)
      R_west  = RRflux(1,(i-1)*np_flux + np_flux/2 + 1)
      Z_east  = ZZflux(1,(i-1)*np_flux + 1)
      Z_west  = ZZflux(1,(i-1)*np_flux + np_flux/2 + 1)

      R_mid   = (R_east + R_west)/2.e0_R8
      Z_mid   = (Z_east + Z_west)/2.e0_R8
      radius  = abs(R_east - R_west)/2.e0_R8

      write(*,'(A,i3,4f12.6)') 'mom : ',i,R_mid,Z_mid,R_top,Z_top

      moments(1,i) = radius
      moments(2,i) = (R_mid - R_geo)
      moments(3,i) = (Z_mid - Z_geo)

      if ( Z_top - Z_mid .gt. 0.e0_R8) then
        moments(4,i) =  (Z_top - Z_mid)  / radius
        moments(6,i) = -(R_top - R_mid)  / radius
      else
        moments(5,i) = -(Z_top - Z_mid)  / radius
        moments(7,i) = -(R_top - R_mid)  / radius
      endif

   endif
  enddo

enddo

!do i=1,nr_flux
!  write(*,'(i5,7f12.6)') i,moments(1:7,i)
!enddo

return
end subroutine helena_moments


subroutine helena_circulating(nr_flux,np_flux,RRflux,ZZflux,PSflux,fraction_circ)
!----------------------------------------------------------------------
! subroutine for the fraction of circulating (non-trapped) particles
!----------------------------------------------------------------------
use helena_Gauss_4
use helena_axis
use helena_constants
use itm_types
implicit none
real (r8),allocatable:: B0max(:),B02av(:),Rav(:),sumK(:)
real (r8)            :: RRflux(4,*),ZZflux(4,*),PSflux(4,*),fraction_circ(*)
integer           :: nr_flux, np_flux
!real (r8)            :: fdia
real (r8) r,sum1,sum2,sumr,b02max,s,ws,  &
     rrg1,drrg1_dr,drrg1_ds,drrg1_drs,drrg1_drr,drrg1_dss,  &
     zzg1,dzzg1_dr,dzzg1_ds,dzzg1_drs,dzzg1_drr,dzzg1_dss,  &
     psg1,dpsg1_dr,dpsg1_ds,dpsg1_drs,dpsg1_drr,dpsg1_dss,  &
     rzjac,gradps2,zjdchi,psi_n,bphi,b02,b0,bm,zlam,sum
integer i,j,n1,n2,n3,n4,ngs,nk,k

!---------------------- find Bmax and B^2 average on every surface

allocate(B0max(nr_flux),B02av(nr_flux),Rav(nr_flux))

r = +1.e0_R8

do i=1, nr_flux - 1

  sum1 = 0.e0_R8
  sum2 = 0.e0_R8
  sumr = 0.e0_R8
  B02max = -1.d20

  do j=1, np_flux

    n1 = (i-1)*np_flux + j
    n2 = (i-1)*np_flux + j + 1
    n3 = (i  )*np_flux + j + 1
    n4 = (i  )*np_flux + j

    if (j .eq. np_flux) then
      n1 = (i  )*np_flux
      n2 = (i  )*np_flux - np_flux + 1
      n3 = (i  )*np_flux + 1
      n4 = (i  )*np_flux + np_flux
    endif

!------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
    do ngs = 1, 4

      s  = xgauss(ngs)
      ws = wgauss(ngs)

      CALL INTERP(RRflux(1,N1),RRflux(1,N2),RRflux(1,N3),RRflux(1,N4),r,s,RRg1,dRRg1_dr,dRRg1_ds,dRRg1_drs,dRRg1_drr,dRRg1_dss)
      CALL INTERP(ZZflux(1,N1),ZZflux(1,N2),ZZflux(1,N3),ZZflux(1,N4),r,s,ZZg1,dZZg1_dr,dZZg1_ds,dZZg1_drs,dZZg1_drr,dZZg1_dss)
      CALL INTERP(PSflux(1,N1),PSflux(1,N2),PSflux(1,N3),PSflux(1,N4),r,s,PSg1,dPSg1_dr,dPSg1_ds,dPSg1_drs,dPSg1_drr,dPSg1_dss)

      RZjac =  dRRg1_dr * dZZg1_ds - dRRg1_ds * dZZg1_dr

      GRADPS2 = dPSg1_dr**2 * (dRRg1_ds**2 + dZZg1_ds**2) / RZjac**2

      ZJDCHI = RRg1 * RZjac / DABS(dPSg1_dr)

      psi_n = - (PSg1 - ps_axis) / ps_axis

      Bphi  = fdia(psi_n) / RRg1

      B02 = Bphi**2 + GRADPS2/RRg1**2

      sum1  = sum1 - ws * ZJDCHI
      sum2  = sum2 - ws * ZJDCHI * B02
      sumR  = sumR - ws * ZJDCHI * RRg1

      if (B02 .gt. B02MAX) B02MAX = B02

    enddo

  enddo

  B02av(i+1) = sum2 / sum1
  B0max(i+1) = DSQRT(DABS(B02MAX))
  Rav(i+1)   = sumR / sum1

enddo


!----------------------------- calculate average term in integral
nk=101
allocate(sumk(nk))

r = +1.e0_R8

write(*,*) 'i+1 sqrt(psi_n) B02av B0max fraction_circ'
do i= 1, nr_flux-1

  sum1 = 0.e0_R8

  sumk = 0.e0_R8

  do j=1, np_flux

    n1 = (i-1)*np_flux + j
    n2 = (i-1)*np_flux + j + 1
    n3 = (i  )*np_flux + j + 1
    n4 = (i  )*np_flux + j

    if (j .eq. np_flux) then
      n1 = (i  )*np_flux
      n2 = (i  )*np_flux - np_flux + 1
      n3 = (i  )*np_flux + 1
      n4 = (i  )*np_flux + np_flux
    endif

!------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
    do ngs=1,4

      s  = xgauss(ngs)
      ws = WGAUSS(ngs)

      call INTERP(RRflux(1,N1),RRflux(1,N2),RRflux(1,N3),RRflux(1,N4),r,s,RRg1,dRRg1_dr,dRRg1_ds,dRRg1_drs,dRRg1_drr,dRRg1_dss)
      call INTERP(ZZflux(1,N1),ZZflux(1,N2),ZZflux(1,N3),ZZflux(1,N4),r,s,ZZg1,dZZg1_dr,dZZg1_ds,dZZg1_drs,dZZg1_drr,dZZg1_dss)
      call INTERP(PSflux(1,N1),PSflux(1,N2),PSflux(1,N3),PSflux(1,N4),r,s,PSg1,dPSg1_dr,dPSg1_ds,dPSg1_drs,dPSg1_drr,dPSg1_dss)

      RZjac =  dRRg1_dr * dZZg1_ds - dRRg1_ds * dZZg1_dr

      GRADPS2 = dPSg1_dr**2 * (dRRg1_ds**2 + dZZg1_ds**2) / RZjac**2

      ZJDCHI = RRg1 * RZjac / DABS(dPSg1_dr)

      psi_n = - (PSg1 - ps_axis) / ps_axis

      Bphi  = fdia(psi_n) / RRg1

      B02 = Bphi**2 + GRADPS2/RRg1**2

      B0 = DSQRT(DABS(B02))
      BM = B0max(i+1)

      do k = 1, nk

	ZLAM = dble(K-1)/dble(NK-1)

	sumK(K) = sumK(K) - WS*ZJDCHI*DSQRT(DABS(1.-ZLAM*B0/BM))

      enddo

      sum1  = sum1 - ws * ZJDCHI
    enddo

  enddo

!------------------------------------------ integrate over lambda
  sum = 0.e0_R8

  do k=2,nk-1

    ZLAM = float(k-1)/float(nk-1)
    sum  = sum + ZLAM * sum1 / sumK(K)

  enddo

  fraction_circ(i+1) = (sum + 0.5e0_R8 * sum1/sumK(nk)) / float(nk-1)
  fraction_circ(i+1) = 0.75e0_R8 * fraction_circ(i+1) * B02av(i+1) / B0max(i+1)**2


  psi_n = - (PSg1 - ps_axis) / ps_axis
  write(*,'(i5,4f12.6)') i+1,sqrt(psi_n),B02av(i+1),B0max(i+1),fraction_circ(i+1)

enddo

return
end subroutine helena_circulating



subroutine helena_volume_integrals(nr_flux,np_flux,RR_flux,ZZ_flux,PS_flux, &
                                   powers,volume_integrals,n_var,n_int,n_psi)
!------------------------------------------------------------------------
! subroutines calculates some quantities which are volume integrals
! within a fluxsurface.
!
! requires a mesh aligned on fluxsurface (helena_remesh)
!
!        1)   R               2)   B_phi
!------------------------------------------------------------------------
use helena_Gauss_4
use helena_axis
use helena_constants
use itm_types
implicit none

real (r8)    :: RR_flux(4,*),ZZ_flux(4,*), PS_flux(4,*)
real (r8)    :: powers(n_var,n_int),volume_integrals(n_psi,n_int)
integer   :: nr_flux, np_flux, n_var, n_int, n_psi
!real (r8)           :: fdia
real (r8) r,wr,s,ws,rzjac,psi_n,bphi,  &
     rrg1,drrg1_dr,drrg1_ds,zzg1,dzzg1_dr,dzzg1_ds,psg1,dpsg1_dr,dpsg1_ds
integer i,j,n1,n2,n3,n4,ngr,ngs,m


write(*,*) ' helena volume integrals : ',nr_flux,np_flux

volume_integrals(1:n_psi,1:n_int) = 0.e0_R8

do i=1,nr_flux-1

  volume_integrals(i+1,1:n_int) = volume_integrals(i,1:n_int)
  
  do j=1,np_flux

    n1 = (i-1)*np_flux + j
    n2 = (i-1)*np_flux + j + 1
    n3 = (i  )*np_flux + j + 1
    n4 = (i  )*np_flux + j

    if (j .eq. np_flux) then
      n1 = (i  )*np_flux
      n2 = (i  )*np_flux - np_flux + 1
      n3 = (i  )*np_flux + 1
      n4 = (i  )*np_flux + np_flux
    endif

    do ngr=1,4

      r  = xgauss(ngr)
      wr = wgauss(ngr)

      do ngs=1,4

        s  = xgauss(ngs)
        ws = wgauss(ngs)

        CALL INTERP2(RR_flux(1,N1),RR_flux(1,N2),RR_flux(1,N3),RR_flux(1,N4),r,s,RRg1,dRRg1_dr,dRRg1_ds)
        CALL INTERP2(ZZ_flux(1,N1),ZZ_flux(1,N2),ZZ_flux(1,N3),ZZ_flux(1,N4),r,s,ZZg1,dZZg1_dr,dZZg1_ds)
        CALL INTERP2(PS_flux(1,N1),PS_flux(1,N2),PS_flux(1,N3),PS_flux(1,N4),r,s,PSg1,dPSg1_dr,dPSg1_ds)

        RZjac =   dRRg1_dr * dZZg1_ds - dRRg1_ds * dZZg1_dr
	psi_n = - (PSg1 - ps_axis) / ps_axis

	Bphi  = fdia(psi_n) / RRg1

	do m=1,n_int

         volume_integrals(i+1,m) = volume_integrals(i+1,m)   &
	                         + RRg1**powers(1,m) *  Bphi**powers(2,m) * wr * ws * 2.e0_R8 *PI * RRg1 * RZjac     ! 3D volume integral (not area)

        enddo

      enddo

    enddo

  enddo

enddo

return
end subroutine helena_volume_integrals

subroutine helena_remesh(nrnew,npnew,RRnew,ZZnew,PSInew)
!------------------------------------------------------------------------
! subroutine calculates a new flux surface grid
!------------------------------------------------------------------------
use helena_surfaces
use helena_elements
use helena_grid
use helena_axis
use itm_types
implicit none

real (r8)             :: RRnew(4,*),ZZnew(4,*),PSInew(4,*), abltg(3), xtmp, tol_t
real (r8),allocatable :: s_values(:),tht_start(:),tht_end(:)
real (r8),allocatable :: sp1(:),sp2(:),sp3(:),sp4(:)
real (r8) pi,dpsi_ds,tht_min,tht_max,rr1,rr2,ss1,ss2,  &
     rrg1,drrg1_dr,drrg1_ds,zzg1,dzzg1_dr,dzzg1_ds,rrg2,drrg2_dr,  &
     drrg2_ds,zzg2,dzzg2_dr,dzzg2_ds,tht1,tht2,tht_tmp,theta,  &
     drr1,dss1,drr2,dss2,drrg1_dt,dzzg1_dt,drrg2_dt,dzzg2_dt,  &
     rz1,rz2,drz1,drz2,rz0,a3,a2,a1,a0,t,t2,t3,ri,dri,si,  &
     dsi,drrg1_drs,drrg1_drr,drrg1_dss,dzzg1_drs,dzzg1_drr,  &
     dzzg1_dss,psg1,dpsg1_dr,dpsg1_ds,dpsg1_drs,dpsg1_drr,dpsg1_dss,  &
     check,rad2,th_z,th_r,th_rr,th_zz,th_rz,dth_ds,dth_dr,dth_drr,dth_drs,dth_dss,  &
     rzjac,ps_r,ps_z,ejac,ptjac,rt,st,dptjac_dr,dptjac_ds,rpt,spt,  &
     dr_dpt,dz_dpt,dr_dz,dr_dr,ds_dz,ds_dr,dps_drr,dps_dzz,cx,cy,tn,tn2,cn
integer nrnew,npnew,i,k,i_elm,node1,node2,node3,node4,j,ifail,inode

if (nrnew .ne. n_psi+1) write(*,*) ' MAJOR PROBLEM in REMESH! nrnew <> n_psi+1'

write(*,*) '**************************************'
write(*,*) '*    helena remesh : ',nrnew,npnew,'*'
write(*,*) '**************************************'

tol_t = 1.d-8             ! tolerance

allocate(s_values(n_psi+1),tht_start(n_pieces_max),tht_end(n_pieces_max))

RRnew(1:4,1:nrnew*npnew)  = 0.e0_R8
ZZnew(1:4,1:nrnew*npnew)  = 0.e0_R8
PSInew(1:4,1:nrnew*npnew) = 0.e0_R8

PI = 2.e0_R8*asin(1.e0_R8)

do i=1,n_psi+1
  s_values(i) = float(i-1)/float(n_psi)
enddo

allocate(sp1(n_psi+1),sp2(n_psi+1),sp3(n_psi+1),sp4(n_psi+1))

!call spline(n_psi+1,s_values,psi_values,0.,0.,1,sp1,sp2,sp3,sp4)

do i=1,n_psi

!    xtmp = spwert(n_psi,s_values(i),sp1,sp2,sp3,sp4,s_values,abltg)
!    dpsi_ds = abltg(1)

    dpsi_ds = - ps_axis * 2.e0_R8*s_values(i+1)   ! for an equidistant grid in s

    tht_min =  1d20
    tht_max = -1d20

    do k=1, flux_surfaces(i)%n_pieces

      rr1  = flux_surfaces(i)%r(1,k)
      rr2  = flux_surfaces(i)%r(3,k)

      ss1  = flux_surfaces(i)%s(1,k)
      ss2  = flux_surfaces(i)%s(3,k)

      i_elm = flux_surfaces(i)%elm(k)

      node1 = elm_list(1,i_elm)
      node2 = elm_list(2,i_elm)
      node3 = elm_list(3,i_elm)
      node4 = elm_list(4,i_elm)

      call INTERP2(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),rr1,ss1,RRg1,dRRg1_dr,dRRg1_ds)
      call INTERP2(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),rr1,ss1,ZZg1,dZZg1_dr,dZZg1_ds)
      call INTERP2(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),rr2,ss2,RRg2,dRRg2_dr,dRRg2_ds)
      call INTERP2(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),rr2,ss2,ZZg2,dZZg2_dr,dZZg2_ds)

      tht1 = atan2(ZZg1-Z_axis,RRg1-R_axis)
      tht2 = atan2(ZZg2-Z_axis,RRg2-R_axis)

      if (tht1 .lt. 0.e0_R8) tht1 = tht1 + 2.e0_R8*PI
      if (tht2 .lt. 0.e0_R8) tht2 = tht2 + 2.e0_R8*PI

      tht_start(k) = min(tht1,tht2)
      tht_end(k)   = max(tht1,tht2)

      if ((tht_end(k) - tht_start(k)) .gt. 3.e0_R8*PI/4.e0_R8) then
         tht_tmp      = tht_end(k)
	 tht_end(k)   = tht_start(k)
	 tht_start(k) = tht_tmp - 2.e0_R8*PI
      endif

      tht_min = min(tht_min,tht_start(k))
      tht_max = max(tht_max,tht_end(k))

    enddo

    do j=1, npnew

      theta = 2.e0_R8 * PI * float(j-1)/float(npnew)

      if (theta .gt. tht_max) theta = theta - 2.e0_R8*PI
      if (theta .lt. tht_min) theta = theta + 2.e0_R8*PI

      do k=1, flux_surfaces(i)%n_pieces

        if ( (theta .ge. tht_start(k)) .and. (theta .le. tht_end(k)) ) then

          rr1  = flux_surfaces(i)%r(1,k);   ss1  = flux_surfaces(i)%s(1,k)
          drr1 = flux_surfaces(i)%r(2,k);   dss1 = flux_surfaces(i)%s(2,k)
          rr2  = flux_surfaces(i)%r(3,k);   ss2  = flux_surfaces(i)%s(3,k)
          drr2 = flux_surfaces(i)%r(4,k);   dss2 = flux_surfaces(i)%s(4,k)

          i_elm = flux_surfaces(i)%elm(k)

          node1 = elm_list(1,i_elm)
          node2 = elm_list(2,i_elm)
          node3 = elm_list(3,i_elm)
          node4 = elm_list(4,i_elm)

          call INTERP2(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),rr1,ss1,RRg1,dRRg1_dr,dRRg1_ds)
          call INTERP2(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),rr1,ss1,ZZg1,dZZg1_dr,dZZg1_ds)
          call INTERP2(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),rr2,ss2,RRg2,dRRg2_dr,dRRg2_ds)
          call INTERP2(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),rr2,ss2,ZZg2,dZZg2_dr,dZZg2_ds)

          dRRg1_dt = dRRg1_dr * drr1 + dRRg1_ds * dss1
          dZZg1_dt = dZZg1_dr * drr1 + dZZg1_ds * dss1
          dRRg2_dt = dRRg2_dr * drr2 + dRRg2_ds * dss2
          dZZg2_dt = dZZg2_dr * drr2 + dZZg2_ds * dss2

	  RZ1  = RRg1     * tan(theta) - ZZg1
	  RZ2  = RRg2     * tan(theta) - ZZg2
	  dRZ1 = dRRg1_dt * tan(theta) - dZZg1_dt
	  dRZ2 = dRRg2_dt * tan(theta) - dZZg2_dt

	  RZ0  = R_axis  * tan(theta) - Z_axis

          a3 = (   RZ1 + dRZ1 -   RZ2 + dRZ2 )/4.e0_R8
          a2 = (       - dRZ1         + dRZ2 )/4.e0_R8
          a1 = (-3.e0_R8*RZ1 - dRZ1 + 3.e0_R8*RZ2 - dRZ2 )/4.e0_R8
          a0 = ( 2.e0_R8*RZ1 + dRZ1 + 2.e0_R8*RZ2 - dRZ2 )/4.e0_R8 - RZ0

          call SOLVP3(a0,a1,a2,a3,t,t2,t3,ifail)

	  if (abs(t) .le. 1.e0_R8+tol_t) then

            call CUB1D(rr1, drr1, rr2, drr2, t, ri, dri)
            call CUB1D(ss1, dss1, ss2, dss2, t, si, dsi)

            call INTERP(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),ri,si, &
	                RRg1,dRRg1_dr,dRRg1_ds,dRRg1_drs,dRRg1_drr,dRRg1_dss)
            call INTERP(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),ri,si, &
	                ZZg1,dZZg1_dr,dZZg1_ds,dZZg1_drs,dZZg1_drr,dZZg1_dss)
            call INTERP(PSI(:,node1),PSI(:,node2),PSI(:,node3),PSI(:,node4),ri,si, &
	                PSg1,dPSg1_dr,dPSg1_ds,dPSg1_drs,dPSg1_drr,dPSg1_dss)

            check = atan2(ZZg1- Z_axis,RRg1-R_axis)
	    if (check .lt. 0.e0_R8) check = check + 2.e0_R8*PI

	    RAD2    =   (RRg1 - R_axis)**2 + (ZZg1 - Z_axis)**2
	    TH_Z    =   (RRg1 - R_axis) / RAD2
	    TH_R    = - (ZZg1 - Z_axis) / RAD2

            TH_RR   = 2*(ZZg1 - Z_axis) * (RRg1 - R_axis) / RAD2**2
            TH_ZZ   = - TH_RR
            TH_RZ   = ( (ZZg1 - Z_axis)**2 - (RRg1 - R_axis)**2 ) / RAD2**2

            dTH_ds  = TH_R * dRRg1_ds + TH_Z * dZZg1_ds
            dTH_dr  = TH_R * dRRg1_dr + TH_Z * dZZg1_dr

            dTH_drr = TH_RR * dRRg1_dr * dRRg1_dr + TH_R * dRRg1_drr + 2.* TH_RZ * dRRg1_dr * dZZg1_dr &
	            + TH_ZZ * dZZg1_dr * dZZg1_dr + TH_Z * dZZg1_drr

            dTH_drs = TH_RR * dRRg1_dr * dRRg1_ds + TH_RZ * dRRg1_dr * dZZg1_ds + TH_R * dRRg1_drs &
	            + TH_RZ * dZZg1_dr * dRRg1_ds + TH_ZZ * dZZg1_dr * dZZg1_ds + TH_Z * dZZg1_drs

            dTH_dss = TH_RR * dRRg1_ds * dRRg1_ds + TH_R * dRRg1_dss + 2.* TH_RZ * dRRg1_ds * dZZg1_ds &
	            + TH_ZZ * dZZg1_ds * dZZg1_ds + TH_Z * dZZg1_dss

	    RZjac   = dRRg1_dr * dZZg1_ds - dRRg1_ds * dZZg1_dr

	    PS_R    = (  dZZg1_ds * dPSg1_dr - dZZg1_dr * dPSg1_ds) / RZjac
	    PS_Z    = (- dRRg1_ds * dPSg1_dr + dRRg1_dr * dPSg1_ds) / RZjac

	    Ejac   =  (PS_R * TH_Z - PS_Z * TH_R)
	    PTjac   = (dPSg1_dr * dTH_ds - dPSg1_ds * dTH_dr)

            RT      = - dPSg1_ds / PTjac
            ST      =   dPSg1_dr / PTjac

            dPTjac_dr = dPSg1_drr * dTH_ds + dPSg1_dr * dTH_drs - dPSg1_drs * dTH_dr - dPSg1_ds * dTH_drr

            dPTjac_ds = dPSg1_drs * dTH_ds + dPSg1_dr * dTH_dss - dPSg1_dss * dTH_dr - dPSg1_ds * dTH_drs

            RPT = (-dPTjac_dr * dTH_ds / PTjac**2 + dTH_drs/PTjac) * RT + (- dPTjac_ds * dTH_ds / PTjac**2 + dTH_dss/PTjac) * ST

            SPT = ( dPTjac_dr * dTH_dr / PTjac**2 - dTH_drr/ PTjac) * RT + (  dPTjac_ds * dTH_dr / PTjac**2 - dTH_drs/PTjac) * ST

            DR_dpt = - dRRg1_drr * dTH_ds * dPSg1_ds / PTjac**2 + dRRg1_drs * (dTH_ds * dPSg1_dr + dTH_dr * dPSg1_ds) / PTjac**2 &
                     + dRRg1_dr  * RPT    + dRRg1_ds * SPT - dRRg1_dss * dTH_dr * dPSg1_dr / PTjac**2

            DZ_dpt = - dZZg1_drr * dTH_ds * dPSg1_ds / PTjac**2 + dZZg1_drs * (dTH_ds * dPSg1_dr + dTH_dr * dPSg1_ds) / PTjac**2 &
                     + dZZg1_dr  * RPT    + dZZg1_ds * SPT - dZZg1_dss * dTH_dr * dPSg1_dr /PTjac**2


	    RRnew(1,npnew*(i) + j)  = RRg1
	    ZZnew(1,npnew*(i) + j)  = ZZg1

            RRnew(2,npnew*(i) + j) = (  TH_Z / Ejac) /  (2.e0_R8*(nrnew-1)) * dpsi_ds
            ZZnew(2,npnew*(i) + j) = (- TH_R / Ejac) /  (2.e0_R8*(nrnew-1)) * dpsi_ds

            RRnew(3,npnew*(i) + j) = + (- PS_Z / Ejac) /  (npnew/PI)
            ZZnew(3,npnew*(i) + j) = + (  PS_R / Ejac) /  (npnew/PI)

            RRnew(4,npnew*(i) + j) =  dR_dpt /(2.e0_R8*(nrnew-1)*(npnew)/PI)  * dpsi_ds
            ZZnew(4,npnew*(i) + j) =  dZ_dpt /(2.e0_R8*(nrnew-1)*(npnew)/PI)  * dpsi_ds

            PSInew(1,npnew*(i) + j) = psi_values(i)
            PSInew(2,npnew*(i) + j) = dpsi_ds / (2.e0_R8*(nrnew-1))
            PSInew(3,npnew*(i) + j) = 0.e0_R8
            PSInew(4,npnew*(i) + j) = 0.e0_R8

	  else

	    write(*,*) ' T TOO BIG : ',T,T2,T3

          endif

        endif

      enddo

    enddo

enddo

!----------------------------------- magnetic axis

node1 = elm_list(1,ielm_ax)
node2 = elm_list(2,ielm_ax)
node3 = elm_list(3,ielm_ax)
node4 = elm_list(4,ielm_ax)

call INTERP(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),r_ax,s_ax,RRg1,dRRg1_dr,dRRg1_ds,dRRg1_drs,dRRg1_drr,dRRg1_dss)
call INTERP(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),r_ax,s_ax,ZZg1,dZZg1_dr,dZZg1_ds,dZZg1_drs,dZZg1_drr,dZZg1_dss)
call INTERP(PSI(:,node1),PSI(:,node2),PSI(:,node3),PSI(:,node4),r_ax,s_ax,PSg1,dPSg1_dr,dPSg1_ds,dPSg1_drs,dPSg1_drr,dPSg1_dss)

ejac  = dRRg1_dr * dZZg1_ds - dRRg1_ds * DZZg1_dr
dr_dZ = - dRRg1_ds / ejac
dr_dR = + dZZg1_ds / ejac
ds_dZ = + dRRg1_dr / ejac
ds_dR = - dZZg1_dr / ejac

dPS_dRR = dPSg1_drr * dr_dR * dr_dR + 2.*dPSg1_drs * dr_dR * ds_dR + dPSg1_dss * ds_dR * ds_dR
dPS_dZZ = dPSg1_drr * dr_dZ * dr_dZ + 2.*dPSg1_drs * dr_dZ * ds_dZ + dPSg1_dss * ds_dZ * ds_dZ

CRR_axis = dPS_dRR / 2.e0_R8
CZZ_axis = dPS_dZZ / 2.e0_R8

!write(*,*) ' CRR, CZZ : ',CRR_axis,CZZ_axis

CX = CRR_axis
CY = CZZ_axis

do j=1,npnew

  inode = j
  RRnew(1,inode) = RRg1
  ZZnew(1,inode) = ZZg1

  theta = float(j-1)/float(npnew) * 2.e0_R8*PI

  TN  = TAN(theta)
  TN2 = TN**2
  CN  = COS(theta)

  if (theta .eq. PI/2.e0_R8) then
    RRnew(2,inode) = 0.e0_R8
    ZZnew(2,inode) = +1.e0_R8/(sqrt(abs(CY))*2.e0_R8*float(nrnew-1))
    RRnew(4,inode) = -1.e0_R8/(sqrt(abs(CY))*2.e0_R8*float(nrnew-1)*float(npnew)/PI)
    ZZnew(4,inode) = 0.e0_R8
  ELSEIF (theta .eq. (3.e0_R8*PI/2)) THEN
    RRnew(2,inode) = 0.e0_R8
    ZZnew(2,inode) = +1.e0_R8/(sqrt(abs(CY))*2.*float(nrnew-1))
    RRnew(4,inode) = -1.e0_R8/(sqrt(abs(CY))*2.*float(nrnew-1)*float(npnew)/PI)
    ZZnew(4,inode) = 0.e0_R8
  ELSE
    RRnew(2,inode) = + sign(1.e0_R8,CN)/(sqrt(abs(CX+CY*TN2))*2.e0_R8*float(nrnew-1))
    ZZnew(2,inode) = + abs(TN)/(sqrt(abs(CX+CY*TN2))*2.e0_R8*float(nrnew-1))
    RRnew(4,inode) = - (CX+CY*TN2)**(-1.5e0_R8) * CY * abs(TN) / (CN**2 * 2.e0_R8*float(nrnew-1)*float(npnew)/PI)
    ZZnew(4,inode) = + CX * (CX + CY*TN2)**(-1.5e0_R8) / (CN*abs(CN) * 2.e0_R8*float(nrnew-1)*float(npnew-1)/PI)
  ENDIF
  IF (theta .gt. PI) THEN
    ZZnew(2,inode) = - ZZnew(2,inode)
    RRnew(4,inode) = - RRnew(4,inode)
  ENDIF
  RRnew(3,inode) = 0.e0_R8
  ZZnew(3,inode) = 0.e0_R8
  PSInew(1,inode) = ps_axis

enddo

!call plot_grid(RRnew,ZZnew,nrnew,npnew)

return
end subroutine helena_remesh


subroutine export_helena
use helena_boundary
use helena_profiles
use helena_phys
use helena_grid
use helena_axis
use helena_constants
use itm_types
implicit none
integer :: i

write(*,*)  ' export to HELENA namelist'

open(20,file='helena.nml')
write(20,*) ' &SHAPE IAS = 1, ISHAPE = 2,'
write(20,'(A,i5,A,i5)') '    MFM= ',mf,', MHARM = ',mf/2
do i=1,mf/2
  write(20,'(A,i4,A,e14.6,A,i4,A,e14.6)') '    FM(',2*i-1,')=',fr(2*i-1)/amin,' FM(',2*i,')=',fr(2*i)/amin
enddo
write(20,*) ' &END'
write(20,*) ' &PROFILE '
write(20,*) '    IPAI=7, EPI=1.0, FPI=1.0'
write(20,*) '    IGAM=7'
write(20,*) '    ICUR=2, ECUR=1.0, FCUR=1.0'
write(20,*) '    NPTS = ',n_prof
do i=1,n_prof
  write(20,'(A,i4,A,e12.4,A,i4,A,e12.4,A,i4,A,e12.4)') &
        '    DPR(',i,') = ',dp_dpsi(i),', DF2(',i,') = ',fdf_dpsi(i),', ZJZ(',i,') = ',zjz_psi(i)
enddo
write(20,*) ' &END'
write(20,*) ' &PHYS '
write(20,'(A,f10.6)') '   EPS   = ',amin/Rgeo
write(20,'(A,f10.6)') '   ALFA  = ',abs(amin**2 * Bgeo / ps_axis)
write(20,'(A,f10.6)') '   B     = ',MU_zero * Rgeo**2 * dp_dpsi(1)/fdf_dpsi(1)
write(20,'(A,f10.6)') '   BETAP = ',beta_p
write(20,'(A,f10.6)') '   XIAB  = ',MU_zero * abs(current) / (amin * Bgeo)
write(20,'(A,f10.6)') '   RVAC  = ',Rgeo
write(20,'(A,f10.6)') '   BVAC  = ',Bgeo
write(20,*) ' &END'
write(20,*) ' &NUM'
write(20,*) '    NR    = 51, NP    = 33, NRMAP  = 101,   NPMAP = 129, NCHI = 128'
write(20,*) '    NRCUR = 51, NPCUR = 33, ERRCUR = 1.e-5, NITER = 100, NMESH = 100'
write(20,*) ' &END'
write(20,*) ' &PRI  NPR1=1 &END '
write(20,*) ' &PLOT NPL1=1 &END '
write(20,*) ' &BALL        &END '
close(20)
return
end subroutine export_helena

subroutine phys_values
use helena_grid
use helena_elements
use helena_Gauss_4
use helena_matrix
use helena_constants
use helena_axis
use helena_boundary
use helena_phys

use itm_types
implicit none
integer :: nodes(4)
real (r8)  :: XX(4,4), XR(4,4), XS(4,4), YR(4,4), YS(4,4), XJAC(4,4), PS(4,4), PSN(4,4)
real (r8)  :: sum_p_area, sum_p_vol, sum_c_area, sum_vol, sum_area, zjz
integer :: ngr, ngs, kf, kv, i, vertex
!real (r8)           :: dpdpsi, fdfdpsi, pressure

sum_area   = 0.e0_R8
sum_vol    = 0.e0_R8
sum_p_vol  = 0.e0_R8
sum_p_area = 0.e0_R8
sum_c_area = 0.e0_R8

do i=1, n_elms

  nodes(1) = elm_list(1,i)
  nodes(2) = elm_list(2,i)
  nodes(3) = elm_list(3,i)
  nodes(4) = elm_list(4,i)

  XR = 0.e0_R8
  XS = 0.e0_R8
  YR = 0.e0_R8
  YS = 0.e0_R8
  XX = 0.e0_R8
  PS = 0.e0_R8

  do ngr = 1, 4          ! 4 Gaussian points
    do ngs = 1, 4        ! 4 Gaussian points
      do kf = 1, 4       ! 4 basis functions
        do kv = 1, 4     ! 4 vertices

          vertex = nodes(kv)

          XX(ngr,ngs) = XX(ngr,ngs) + RR(kf,vertex)  * H(ngr,ngs,kv,kf)
          PS(ngr,ngs) = PS(ngr,ngs) + PSI(kf,vertex) * H(ngr,ngs,kv,kf)

	  XR(ngr,ngs) = XR(ngr,ngs) + RR(kf,vertex) * HR(ngr,ngs,kv,kf)
          XS(ngr,ngs) = XS(ngr,ngs) + RR(kf,vertex) * HS(ngr,ngs,kv,kf)
          YR(ngr,ngs) = YR(ngr,ngs) + ZZ(kf,vertex) * HR(ngr,ngs,kv,kf)
          YS(ngr,ngs) = YS(ngr,ngs) + ZZ(kf,vertex) * HS(ngr,ngs,kv,kf)

        enddo
      enddo

      XJAC(ngr,ngs) = XR(ngr,ngs)*YS(ngr,ngs) - XS(ngr,ngs)*YR(ngr,ngs)

    enddo
  enddo

  PSN(:,:) = (ps_axis - PS(:,:)) / ps_axis

  do ngr = 1, 4
    do ngs = 1, 4

       sum_vol    = sum_vol    + wgauss(ngr)*wgauss(ngs) * XJAC(ngr,ngs) * XX(ngr,ngs)
       sum_area   = sum_area   + wgauss(ngr)*wgauss(ngs) * XJAC(ngr,ngs)

       sum_p_area = sum_p_area + wgauss(ngr)*wgauss(ngs) * XJAC(ngr,ngs)               * pressure(psn(ngr,ngs))
       sum_p_vol  = sum_p_vol  + wgauss(ngr)*wgauss(ngs) * XJAC(ngr,ngs) * XX(ngr,ngs) * pressure(psn(ngr,ngs))

       zjz = fdfdpsi(psn(ngr,ngs)) / (MU_zero * XX(ngr,ngs)) + dpdpsi(psn(ngr,ngs)) * XX(ngr,ngs)

       sum_c_area  = sum_c_area +  wgauss(ngr)*wgauss(ngs) * XJAC(ngr,ngs) * zjz

    enddo
  enddo

enddo

volume  = 2.e0_R8 * PI * sum_vol
area    = sum_area
current = sum_c_area
beta_p  = 8.e0_R8 * PI / MU_zero * sum_p_area / (sum_c_area**2 )
beta_t  = 2.e0_R8 *      MU_zero * sum_p_area / (sum_area * Bgeo**2)
beta_n  = 100.e0_R8 * (4.e0_R8*PI/10.e0_R8) * beta_t / (MU_zero * abs(current) /  (amin * Bgeo))

write(*,'(A,f16.8,A)') ' total volume    : ',volume,' m^3'
write(*,'(A,f16.8,A)') ' total area      : ',area,' m^2'
write(*,'(A,e12.4,A)') ' total current   : ',current,' A'
write(*,'(A,f16.8)')   ' poloidal beta   : ',beta_p
write(*,'(A,f16.8)')   ' toroidal beta   : ',beta_t
write(*,'(A,f16.8)')   ' normalised beta : ',beta_n
write(*,'(A,3f16.8)')  ' magnetic axis   : ',R_axis,Z_axis,(R_axis-Rgeo)/amin

return
end subroutine phys_values




subroutine update_fdf(fmix,fdf_error)
use helena_profiles
use helena_axis
use helena_constants
use helena_surfaces
use itm_types
implicit none
real (r8), allocatable :: R_av(:),OR_av(:)
!real (r8)              :: current_profile, dpdpsi
real (r8)              :: psi_n, psi_n2, small, fdf_error, fdf_old, fmix
integer           :: n_int, i

write(*,*) ' UPDATE FDF'

n_psi = n_prof
n_int = 5

small = 1.d-8

allocate(psi_values(n_psi),R_av(n_psi),OR_av(n_psi))

do i=1,n_prof
  psi_n         = float(i-1)/float(n_prof-1)
  psi_values(i) = ps_axis - (1. - small) * psi_n  * ps_axis
enddo

call find_flux_surfaces
call fluxsurface_current(R_av, OR_av)

fdf_old   = 0.e0_R8
fdf_error = 0.e0_R8

do i=1,n_prof

  psi_n  = 1. - psi_values(i)/ps_axis
  psi_n2 = float(i-1)/float(n_prof-1)

  fdf_old = fdf_dpsi(i)

  fdf_dpsi(i) = MU_zero *(- current_profile(psi_n) - R_av(i) * dpdpsi(psi_n) ) / OR_av(i)

  fdf_dpsi(i) = (1.-fmix)*fdf_dpsi(i) + fmix * fdf_old

!  write(*,'(i4,8e12.4)') i,R_av,OR_av,fdf_dpsi(i),dp_dpsi(i),dpdpsi(psi_n),psi_n,psi_n2

  fdf_error= fdf_error + abs(fdf_old - fdf_dpsi(i))

enddo

fdf_error = fdf_error / float(n_prof)

!call lplot6(2,1,psi_values,fdf_dpsi,n_prof,'fdf')

deallocate(psi_values,R_av,OR_av)

return
end subroutine update_fdf

subroutine GS_solve(n_iter,rhs_only,solve_only,error_iteration,amix,ifail)
use helena_axis
use helena_grid
use itm_types
implicit none
integer :: i, n_iter, ifail
logical :: solve_only, rhs_only
real (r8)  :: error, error_iteration, amix, t_start, t_end

ifail = 999

call cpu_time(t_start)

do i = 1, n_iter

  call findaxis
  call matrix_GS(ps_axis,rhs_only)
  call solve_matrix(amix,solve_only,error)

!  write(*,'(A,i5,f12.8,e16.8)') ' GS_solve : ',i,R_axis,error

  if (error .lt. error_iteration) then
    write(*,'(A,i5,3e16.8)') ' GS_solve : ',i,R_axis,ps_axis,error
    ifail = 0
    exit
  endif

  solve_only = .true.
  rhs_only   = .true.

enddo

call cpu_time(t_end)

if (error .gt. error_iteration) then
  ifail = n_iter
  write(*,'(A,i5,3e16.8)') ' GS_solve failed : ',i,R_axis,ps_axis,error
endif

write(*,'(A,f12.8)') ' GS, CPU time : ',t_end-t_start

return

end subroutine GS_solve


subroutine find_flux_surfaces
use helena_grid
use helena_elements
use helena_surfaces
use helena_constants
use itm_types
implicit none
real (r8)  :: psimin, psimax, a0, a1, a2, a3
real (r8)  :: psi_test, dpsi_dr(4),dpsi_ds(4), dRR_dr(4), dRR_ds(4), dZZ_dr(4), dZZ_ds(4)
real (r8)  :: RR_psi(4), ZZ_psi(4), r_psi(4), s_psi(4), tht(4)
real (r8)  :: s, s2, s3, r_tmp, s_tmp, psr_tmp, pss_tmp, ttmp
integer :: i, j, ifound, iv, im, is, n1, n2, n3
integer :: ifail, itht(4), itmp


write(*,*) ' find_flux_surfaces : ',n_psi

if (.not. allocated(flux_surfaces)) allocate(flux_surfaces(n_psi))

do j=1, n_psi
  flux_surfaces(j)%n_pieces = 0
  flux_surfaces(j)%elm      = 0
  flux_surfaces(j)%r        = 0
  flux_surfaces(j)%s        = 0
enddo

do i=1, n_elms

  call psi_minmax(i,psimin,psimax)

  do j=1, n_psi

    ifound = 0

    if ((psi_values(j) .ge. psimin) .and. (psi_values(j) .le. psimax)) then

      do iv=1, 4

        im = MOD(iv,4) + 1
        n1 = elm_list(iv,i)
        n2 = elm_list(im,i)

	is = mod(iv,2) + 2

        if (iv .gt. 2) then
	  n3 = n2
	  n2 = n1
	  n1 = n3
	endif

        a3 = (   psi(1,n1) + psi(is,n1) -   psi(1,n2) + psi(is,n2))/4.
        a2 = (             - psi(is,n1)               + psi(is,n2))/4.
        a1 = (-3*psi(1,n1) - psi(is,n1) + 3*psi(1,n2) - psi(is,n2))/4.
        a0 = ( 2*psi(1,n1) + psi(is,n1) + 2*psi(1,n2) - psi(is,n2))/4. - psi_values(j)

        call SOLVP3(a0,a1,a2,a3,s,s2,s3,ifail)

        if (abs(s) .le. 1.) then

          ifound = ifound + 1

	  call flux_surface_add_point(s,i,iv,ifound,r_psi,s_psi,dpsi_dr,dpsi_ds)

        endif

        if (abs(s2) .le. 1.) then

          ifound = ifound + 1

	  call flux_surface_add_point(s2,i,iv,ifound,r_psi,s_psi,dpsi_dr,dpsi_ds)

	  if (abs(s3) .le. 1.) 	 write(*,*) ' another solution : ',s3

        endif

      enddo ! end of 4 edges

      if (ifound .eq. 2) then

        call flux_surface_add_line(i,j,r_psi(1:2),s_psi(1:2),dpsi_dr(1:2),dpsi_ds(1:2))

      elseif (ifound .eq. 4) then

! complicated : 2 line pieces but which point belongs to which line piece?

        tht(1:4) = atan2(s_psi(1:4),r_psi(1:4))

	where (tht .lt. 0.) tht = tht + 2.e0_R8*PI

        itht(1)= 1; itht(2) = 2; itht(3) = 3; itht(4) = 4

        if (tht(2) .lt. tht(1)) then
	  itmp = itht(1); itht(1) = itht(2) ; itht(2) = itmp;
        endif
        if (tht(4) .lt. tht(3)) then
	  itmp = itht(3); itht(3) = itht(4) ; itht(4) = itmp;
        endif
        if (tht(itht(3)) .lt. tht(itht(2))) then
	  itmp = itht(2); itht(2) = itht(3) ; itht(3) = itmp;
        endif
        if (tht(itht(2)) .lt. tht(itht(1))) then
	  itmp = itht(1); itht(1) = itht(2) ; itht(2) = itmp;
        endif
        if (tht(itht(4)) .lt. tht(itht(3))) then
	  itmp = itht(3); itht(3) = itht(4) ; itht(4) = itmp;
        endif
        if (tht(itht(3)) .lt. tht(itht(2))) then
	  itmp = itht(2); itht(2) = itht(3) ; itht(3) = itmp;
        endif

!        write(*,*) i,j
!        write(*,'(4f12.8)') tht
!        write(*,'(4i5)') itht
!        write(*,'(4f12.8)') tht(itht)
!        write(*,'(4f12.8)') r_psi
!        write(*,'(4f12.8)') s_psi
!        write(*,'(4f12.8)') dpsi_dr
!        write(*,'(4f12.8)') dpsi_ds

	call flux_surface_add_line(i,j,r_psi(itht(1:2)),s_psi(itht(1:2)),dpsi_dr(itht(1:2)),dpsi_ds(itht(1:2)))
	call flux_surface_add_line(i,j,r_psi(itht(3:4)),s_psi(itht(3:4)),dpsi_dr(itht(3:4)),dpsi_ds(itht(3:4)))

      endif

    endif

  enddo

enddo

return
end subroutine find_flux_surfaces

subroutine flux_surface_add_line(i,j,r_psi,s_psi,dpsi_dr,dpsi_ds)
use helena_grid
use helena_elements
use helena_surfaces
use itm_types
implicit none
integer :: i, j,node1, node2, node3, node4
real (r8)  :: psi_value, r_psi(*), s_psi(*), dpsi_dr(*), dpsi_ds(*)
real (r8)  :: rr1, rr2, ss1, ss2,sgn, drs, xl1 ,xl2, drr1, drr2, dss1, dss2, t
real (r8)  :: ri, si, dri, dsi, delta_ri, delta_si,psi_test, dummy_r, dummy_s, dl1, dl2

rr1  = r_psi(1)
rr2  = r_psi(2)
ss1  = s_psi(1)
ss2  = s_psi(2)

sgn = 1.e0_R8

if ((rr1 .eq. -1.e0_R8).and. (dpsi_ds(1).lt. 0.e0_R8)) then
  sgn = -1.e0_R8
elseif ((rr1 .eq. +1.e0_R8).and. (dpsi_ds(1).gt. 0.e0_R8)) then
  sgn = -1.e0_R8
endif
if ((ss1 .eq. -1.e0_R8).and. (dpsi_dr(1).gt. 0.e0_R8)) then
  sgn = -1.e0_R8
elseif ((ss1 .eq. +1.e0_R8).and. (dpsi_dr(1).lt. 0.e0_R8)) then
  sgn = -1.e0_R8
endif

drs = sgn * sqrt((rr2-rr1)**2 + (ss2-ss1)**2)

xl1 = 0.5e0_R8 * drs / sqrt(dpsi_dr(1)**2 + dpsi_ds(1)**2)  ! temporary fix; waiting for ideas
xl2 = 0.5e0_R8 * drs / sqrt(dpsi_dr(2)**2 + dpsi_ds(2)**2)

drr1 =   xl1 * dpsi_ds(1)
drr2 =   xl2 * dpsi_ds(2)
dss1 = - xl1 * dpsi_dr(1)
dss2 = - xl2 * dpsi_dr(2)

!------------------------------------------- attempt a correction step to correct drr1,drr2,dss1,dss2

t = 0.e0_R8                 ! make the curve fit exactly at t=0.

call CUB1D(rr1, drr1, rr2, drr2, t, ri, dri)
call CUB1D(ss1, dss1, ss2, dss2, t, si, dsi)

node1 = elm_list(1,i);  node2 = elm_list(2,i);  node3 = elm_list(3,i);  node4 = elm_list(4,i)

call INTERP2(psi(:,node1),psi(:,node2),psi(:,node3),psi(:,node4),ri,si,psi_test,dummy_r,dummy_s)

delta_ri = 0.5e0_R8 * (psi_values(j) - psi_test) / dummy_r
delta_si = 0.5e0_R8 * (psi_values(j) - psi_test) / dummy_s

call solveM2(0.25e0_R8*drr1,-0.25e0_R8*drr2,0.25e0_R8*dss1,-0.25e0_R8*dss2,delta_ri,delta_si,dl1,dl2)

if ((abs(dl1) .lt. 0.1e0_R8) .and. (abs(dl2).lt.0.1e0_R8)) then

  drr1 = (1.e0_R8+ dl1) * drr1
  dss1 = (1.e0_R8+ dl1) * dss1
  drr2 = (1.e0_R8+ dl2) * drr2
  dss2 = (1.e0_R8+ dl2) * dss2

endif

flux_surfaces(j)%n_pieces                         = flux_surfaces(j)%n_pieces + 1
flux_surfaces(j)%elm(flux_surfaces(j)%n_pieces)   = i
flux_surfaces(j)%r(1:4,flux_surfaces(j)%n_pieces) = (/ rr1, drr1, rr2, drr2 /)
flux_surfaces(j)%s(1:4,flux_surfaces(j)%n_pieces) = (/ ss1, dss1, ss2, dss2 /)

return
end subroutine flux_surface_add_line

subroutine flux_surface_add_point(s,i,iv,ifound,r_psi,s_psi,dpsi_dr,dpsi_ds)

use helena_elements
use helena_grid
use itm_types
implicit none

integer :: i, iv, ifound, node1, node2, node3, node4
real (r8)  :: s, r_psi(*), s_psi(*), dpsi_dr(*), dpsi_ds(*), psi_test

if (iv .eq. 1) then
  r_psi(ifound) = -1.e0_R8 ; s_psi(ifound) = s
elseif (iv .eq. 2) then
  r_psi(ifound) = s   ; s_psi(ifound) = 1.e0_R8
elseif (iv .eq. 3) then
  r_psi(ifound) = +1.e0_R8 ; s_psi(ifound) = s
elseif (iv .eq. 4) then
  r_psi(ifound) = s   ; s_psi(ifound) = -1.e0_R8
endif

node1 = elm_list(1,i);  node2 = elm_list(2,i);  node3 = elm_list(3,i);  node4 = elm_list(4,i)

call INTERP2(psi(:,node1),psi(:,node2),psi(:,node3),psi(:,node4), &
             r_psi(ifound),s_psi(ifound),psi_test,dpsi_dr(ifound),dpsi_ds(ifound))

return
end subroutine flux_surface_add_point

subroutine plot_flux_surfaces
use helena_surfaces
use helena_elements
use helena_grid
use itm_types
implicit none
integer          :: j, k,ip, nplot, node1, node2, node3, node4, i_elm
real (r8)           :: t, rr1, rr2, drr1, drr2, ss1, ss2, dss1, dss2, ri, si, dri, dsi, dummy1, dummy2
real (r8),allocatable :: rplot(:), zplot(:)
character*13     :: LABEL

nplot=5
allocate(rplot(nplot),zplot(nplot))

LABEL= 'Flux surfaces'

if (((Z_max-Z_min)/(R_max-R_min)) .lt. 1.5e0_R8 ) then
  CALL NFRAME(2,21,1,R_min,R_max,Z_min,Z_max,LABEL,13,'R [m]',5,'Z [m]',5)
else
  CALL NFRAME(22,1,1,R_min,R_max,Z_min,Z_max,LABEL,13,'R [m]',5,'Z [m]',5)
endif
  
!call plot_elements

do j=1,n_psi

  do k=1,flux_surfaces(j)%n_pieces

    i_elm = flux_surfaces(j)%elm(k)

    node1 = elm_list(1,i_elm)
    node2 = elm_list(2,i_elm)
    node3 = elm_list(3,i_elm)
    node4 = elm_list(4,i_elm)

    rr1  = flux_surfaces(j)%r(1,k)
    drr1 = flux_surfaces(j)%r(2,k)
    rr2  = flux_surfaces(j)%r(3,k)
    drr2 = flux_surfaces(j)%r(4,k)

    ss1  = flux_surfaces(j)%s(1,k)
    dss1 = flux_surfaces(j)%s(2,k)
    ss2  = flux_surfaces(j)%s(3,k)
    dss2 = flux_surfaces(j)%s(4,k)

    do ip = 1, nplot

      t = -1. + 2.*float(ip-1)/float(nplot-1)

      call CUB1D(rr1, drr1, rr2, drr2, t, ri, dri)
      call CUB1D(ss1, dss1, ss2, dss2, t, si, dsi)

      call INTERP2(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),ri,si,rplot(ip),dummy1,dummy2)
      call INTERP2(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),ri,si,zplot(ip),dummy1,dummy2)

    enddo

    call lincol(1)
    if (k .gt. 1) then
      if (i_elm .eq. flux_surfaces(j)%elm(k-1)) then
        call lincol(2)
	write(51,*) ' 1 setlinewidth '
      elseif (i_elm .eq. flux_surfaces(j)%elm(k+1)) then
        call lincol(3)
	write(51,*) ' 2 setlinewidth '
      else
        call lincol(1)
	write(51,*) ' .5 setlinewidth '
      endif
    endif

    call lplot6(22,1,rplot,zplot,-nplot,' ')

  enddo

enddo

return
end subroutine plot_flux_surfaces

subroutine fluxsurface_current(R_av, OR_av)
use helena_grid
use helena_elements
use helena_axis
use helena_surfaces
use helena_constants
use helena_Gauss_4
use itm_types
implicit none
real (r8)  :: R_av(*), OR_av(*), sum_dl, sum_Rav, sum_ORav
integer :: i_elm, j, k, n1, n2, n3, node1, node2, node3, node4
real (r8)  :: t,rr1, rr2, drr1, drr2, ss1, ss2, dss1, dss2, ri, si, dri, dsi, dl
real (r8)  :: RRgi, dRRgi_dr, dRRgi_ds, ZZgi, dZZgi_dr, dZZgi_ds, dRRgi_dt, dZZgi_dt
real (r8)  :: PSgi, dPSgi_dr, dPSgi_ds, PSI_R, PSI_Z, RZJAC, grad_psi
integer :: ig, ip


do j=1, n_psi

  sum_dl   = 0.e0_R8
  sum_Rav  = 0.e0_R8
  sum_ORav = 0.e0_R8

  do k=1, flux_surfaces(j)%n_pieces

    do ig = 1, 4

      t = xgauss(ig)

      rr1  = flux_surfaces(j)%r(1,k)
      drr1 = flux_surfaces(j)%r(2,k)
      rr2  = flux_surfaces(j)%r(3,k)
      drr2 = flux_surfaces(j)%r(4,k)

      ss1  = flux_surfaces(j)%s(1,k)
      dss1 = flux_surfaces(j)%s(2,k)
      ss2  = flux_surfaces(j)%s(3,k)
      dss2 = flux_surfaces(j)%s(4,k)

      call CUB1D(rr1, drr1, rr2, drr2, t, ri, dri)
      call CUB1D(ss1, dss1, ss2, dss2, t, si, dsi)

      i_elm = flux_surfaces(j)%elm(k)

      node1 = elm_list(1,i_elm)
      node2 = elm_list(2,i_elm)
      node3 = elm_list(3,i_elm)
      node4 = elm_list(4,i_elm)

!-----------------------------------------------------------
      call INTERP2(PSI(:,node1),PSI(:,node2),PSI(:,node3),PSI(:,node4),ri,si,PSgi,dPSgi_dr,dPSgi_ds)
      call INTERP2(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),ri,si,RRgi,dRRgi_dr,dRRgi_ds)
      call INTERP2(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),ri,si,ZZgi,dZZgi_dr,dZZgi_ds)

      dRRgi_dt = dRRgi_dr * dri + dRRgi_ds * dsi
      dZZgi_dt = dZZgi_dr * dri + dZZgi_ds * dsi

      dl = sqrt(dRRgi_dt**2 + dZZgi_dt**2)

      RZjac  = DRRgi_dr * dZZgi_ds - dRRgi_ds * dZZgi_dr

      PSI_R = (   dPSgi_dr * dZZgi_ds - dPSgi_ds * dZZgi_dr ) / RZjac
      PSI_Z = ( - dPSgi_dr * dRRgi_ds + dPSgi_ds * dRRgi_dr ) / RZjac

      grad_psi = sqrt(PSI_R * PSI_R + PSI_Z * PSI_Z)
!----------------------------------------------------------

      sum_dl   = sum_dl   + wgauss(ig)        * dl * RRgi / grad_psi
      sum_Rav  = Sum_Rav  + wgauss(ig) * RRgi * dl * RRgi / grad_psi
      sum_ORav = sum_ORav + wgauss(ig) / RRgi * dl * RRgi / grad_psi

    enddo

  enddo

  R_av(j)  = sum_Rav / sum_dl
  OR_av(j) = sum_ORav / sum_dl

enddo

R_av(1)  = R_axis
OR_av(1) = 1.e0_R8/ R_axis

return
end subroutine fluxsurface_current

subroutine fluxsurface_integrals(powers,n_int,n_var,results)
!----------------------------------------------------------------------
! subroutine calculats arbitrary flux surfcae quantities as defined by
! the matrix powers. The entries in the matrix define the power of
! the variables in the integrals
!
!  powers(n_int,n_var)
!    n_int : the number of flux surface quantities
!    n_var : the number of variables in the integrand
!
!    nvar = 1: R     2: B^2          3: grad(psi)
!---------------------------------------------------------------------
use helena_grid
use helena_elements
use helena_axis
use helena_surfaces
use helena_constants
use helena_Gauss_4
use itm_types
implicit none
integer :: n_int,n_var
real (r8)  :: powers(n_var,n_int), results(n_psi+1,n_int)
integer :: i_elm, j, k, n1, n2, n3, node1, node2, node3, node4
real (r8)  :: t,rr1, rr2, drr1, drr2, ss1, ss2, dss1, dss2, ri, si, dri, dsi, dl
real (r8)  :: RRgi, dRRgi_dr, dRRgi_ds, ZZgi, dZZgi_dr, dZZgi_ds, dRRgi_dt, dZZgi_dt
real (r8)  :: PSgi, dPSgi_dr, dPSgi_ds, PSI_R, PSI_Z, RZJAC, grad_psi, psi_n
real (r8)  :: sum_dl, B_tot2
integer :: i,m, ig, ip
!real (r8)           :: fdia

write(*,*) ' flux_surface_integrals : ',n_psi,n_int,n_var

results(1:n_psi+1,1:n_int)    = 0.e0_R8

do i=1, n_psi

  psi_n = 1.e0_R8 - psi_values(i)/ps_axis
  
  sum_dl = 0.e0_R8
  
  do k=1, flux_surfaces(i)%n_pieces

    do ig = 1, 4

      t = xgauss(ig)

      rr1  = flux_surfaces(i)%r(1,k)
      drr1 = flux_surfaces(i)%r(2,k)
      rr2  = flux_surfaces(i)%r(3,k)
      drr2 = flux_surfaces(i)%r(4,k)

      ss1  = flux_surfaces(i)%s(1,k)
      dss1 = flux_surfaces(i)%s(2,k)
      ss2  = flux_surfaces(i)%s(3,k)
      dss2 = flux_surfaces(i)%s(4,k)

      call CUB1D(rr1, drr1, rr2, drr2, t, ri, dri)
      call CUB1D(ss1, dss1, ss2, dss2, t, si, dsi)

      i_elm = flux_surfaces(i)%elm(k)

      node1 = elm_list(1,i_elm)
      node2 = elm_list(2,i_elm)
      node3 = elm_list(3,i_elm)
      node4 = elm_list(4,i_elm)

      call INTERP2(PSI(:,node1),PSI(:,node2),PSI(:,node3),PSI(:,node4),ri,si,PSgi,dPSgi_dr,dPSgi_ds)
      call INTERP2(RR(:,node1),RR(:,node2),RR(:,node3),RR(:,node4),ri,si,RRgi,dRRgi_dr,dRRgi_ds)
      call INTERP2(ZZ(:,node1),ZZ(:,node2),ZZ(:,node3),ZZ(:,node4),ri,si,ZZgi,dZZgi_dr,dZZgi_ds)

      dRRgi_dt = dRRgi_dr * dri + dRRgi_ds * dsi
      dZZgi_dt = dZZgi_dr * dri + dZZgi_ds * dsi

      dl = sqrt(dRRgi_dt**2 + dZZgi_dt**2)

      RZjac  = DRRgi_dr * dZZgi_ds - dRRgi_ds * dZZgi_dr

      PSI_R = (   dPSgi_dr * dZZgi_ds - dPSgi_ds * dZZgi_dr ) / RZjac
      PSI_Z = ( - dPSgi_dr * dRRgi_ds + dPSgi_ds * dRRgi_dr ) / RZjac

      grad_psi = sqrt(PSI_R * PSI_R + PSI_Z * PSI_Z)

      B_tot2 =  (fdia(psi_n) / RRgi)**2 + (grad_psi/ RRgi)**2

      sum_dl = sum_dl +  wgauss(ig) * dl

      do m = 1, n_int
        results(i+1,m) = results(i+1,m) + wgauss(ig) * dl   &
                       * RRgi**powers(1,m)                  &
                       * B_tot2**powers(2,m)                &
                       * grad_psi**powers(3,m)

      enddo

    enddo

  enddo
  
!  results(i+1,1:n_int) = results(i+1,1:n_int) / sum_dl

enddo

!----------------------------------- values on axis

do m=1, n_int
 
  results(1,m) =  R_axis**powers(1,m) * B_axis**powers(2,m)
  
  if (powers(3,m) .eq. -1.e0_R8) then
    results(1,m) = results(1,m) * PI / sqrt(CRR_axis*CZZ_Axis)
  elseif (powers(3,m) .gt. 0.e0_R8) then
    results(1,m) = 0.e0_R8
  endif

enddo

return
end subroutine fluxsurface_integrals

subroutine helena_flux_surface_integrals(nr_flux,np_flux,RR_flux,ZZ_flux,PS_flux, &
                                         powers,n_int,n_var,results,q, vprime)
!----------------------------------------------------------------------
! subroutine calculats arbitrary flux surfcae quantities as defined by
! the matrix powers. The entries in the matrix define the power of
! the variables in the integrals
!
! requires fluxsurface aligned mesh
!
!  powers(n_int,n_var)
!    n_int : the number of flux surface quantities
!    n_var : the number of variables in the integrand
!
!    nvar = 1: R     2: B^2          3: grad(psi)
!---------------------------------------------------------------------
use helena_grid
use helena_elements
use helena_axis
use helena_surfaces
use helena_constants
use helena_Gauss_4
use itm_types
implicit none
real (r8)  :: RR_flux(4,*),ZZ_flux(4,*),PS_flux(4,*)
integer :: n_int,n_var, nr_flux, np_flux
real (r8)  :: powers(n_var,n_int), results(nr_flux,n_int), q(nr_flux), vprime(nr_flux)
integer :: i, j, k, m, n1, n2, n3, n4, ngs
real (r8)  :: RRg1, dRRg1_dr, dRRg1_ds, ZZg1, dZZg1_dr, dZZg1_ds, dRRg1_dt, dZZg1_dt
real (r8)  :: PSg1, dPSg1_dr, dPSg1_ds, PSI_R, PSI_Z, RZJAC, grad_psi, ws
real (r8)  :: r, s, dl, sum_dl, B_tot2, Bphi, grad_psi2, zjdchi, psi_n
!real (r8)           :: fdia

write(*,*) ' flux_surface_integrals : ',n_psi,n_int,n_var
write(*,*) ' nr_flux : ',nr_flux

if (nr_flux .ne. n_psi+1) write(*,*) ' MAJOR PORBLEM in helena_fluxsurface_integral',nr_flux,n_psi


results(1:n_psi+1,1:n_int)    = 0.e0_R8
q      = 0.e0_R8
vprime = 0.e0_R8

r = + 1.e0_R8

do i=1, nr_flux-1

  sum_dl = 0.e0_R8
  
  do j=1, np_flux

    n1 = (i-1)*np_flux + j
    n2 = (i-1)*np_flux + j + 1
    n3 = (i  )*np_flux + j + 1
    n4 = (i  )*np_flux + j

    if (j .eq. np_flux) then
      n1 = (i  )*np_flux
      n2 = (i  )*np_flux - np_flux + 1
      n3 = (i  )*np_flux + 1
      n4 = (i  )*np_flux + np_flux
    endif

!------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
    do ngs = 1, 4

      s  = xgauss(ngs)
      ws = wgauss(ngs)

      call INTERP2(RR_flux(1,N1),RR_flux(1,N2),RR_flux(1,N3),RR_flux(1,N4),r,s,RRg1,dRRg1_dr,dRRg1_ds)
      call INTERP2(ZZ_flux(1,N1),ZZ_flux(1,N2),ZZ_flux(1,N3),ZZ_flux(1,N4),r,s,ZZg1,dZZg1_dr,dZZg1_ds)
      call INTERP2(PS_flux(1,N1),PS_flux(1,N2),PS_flux(1,N3),PS_flux(1,N4),r,s,PSg1,dPSg1_dr,dPSg1_ds)

      RZjac =  dRRg1_dr * dZZg1_ds - dRRg1_ds * dZZg1_dr
      
      dl  = SQRT(dRRg1_ds**2 + dZZg1_ds**2)

      if(RZjac .eq. 0.0_R8) then
         write(*,*) 'Warning: RZjac == 0, grad_psi2 set to 0.0'
         grad_psi2= 0.0_R8   ! DPC
      else
         grad_psi2= dPSg1_dr**2 * (dRRg1_ds**2 + dZZg1_ds**2) / RZjac**2
      endif

      if(dPSg1_dr .eq. 0.0_R8) then
         write(*,*) 'Warning: dPSg1_dr == 0, ZJDCHI set to 0.0'
         ZJDCHI = 0.0_R8   ! DPC
      else
         ZJDCHI = 2.e0_R8 * PI * RRg1 * RZjac / DABS(dPSg1_dr)
      endif
      
      psi_n = - (PSg1 - ps_axis) / ps_axis

      Bphi  = fdia(psi_n) / RRg1

      B_tot2 = Bphi**2 + grad_psi2/RRg1**2

      sum_dl = sum_dl +  ws * dl
            
      q(i+1)      = q(i+1)      + ws / (RRg1 * sqrt(grad_psi2)) * dl
      vprime(i+1) = vprime(i+1) + ws * ZJDCHI 
      
!      write(*,'(A,6i5,12f12.8)') ' test : ',i,j,n1,n2,n3,n4,r,s,PSg1,RRg1,RR_flux(1,N1),RR_flux(1,N2),RR_flux(1,N3),RR_flux(1,N4)

      do m = 1, n_int

!        results(i+1,m) = results(i+1,m) + ws * dl         &
!                       * RRg1**powers(1,m)                &
!                       * B_tot2**powers(2,m)              &
!                       * sqrt(grad_psi2)**powers(3,m)

        results(i+1,m) = results(i+1,m) + ws * ZJDCHI     &
                       * RRg1**powers(1,m)                &
                       * B_tot2**powers(2,m)              &
                       * sqrt(grad_psi2)**powers(3,m)

      enddo

    enddo

  enddo
  
  results(i+1,1:n_int) = results(i+1,1:n_int) / vprime(i+1)
  
enddo

!----------------------------------- values on axis
q(1) = PI / (R_axis * sqrt(CRR_axis*CZZ_Axis))

do m=1, n_int
 
  results(1,m) =  R_axis**powers(1,m) * B_axis**powers(2,m)
  
  if (powers(3,m) .eq. -1.e0_R8) then
    results(1,m) = results(1,m) * PI / sqrt(CRR_axis*CZZ_Axis)
  elseif (powers(3,m) .gt. 0.e0_R8) then
    results(1,m) = 0.e0_R8
  endif
enddo

return
end subroutine helena_flux_surface_integrals

subroutine SolveM2(a,b,c,d,e,f,x,y)
!-----------------------------------------------------------------------
! solves a 2x2 set of equations
!-----------------------------------------------------------------------
use itm_types
implicit none
real (r8) :: a,b,c,d,e,f,x,y, det

det = a*d - b*c

if (det .ne. 0.e0_R8) then
  x = (   d * e - b * f ) / det
  y = ( - c * e + a * f ) / det
else
  x = 0.e0_R8
  y = 0.e0_R8
endif

return
end subroutine SolveM2

SUBROUTINE SOLVP3(C0,C1,C2,C3,X1,X2,X3,IFAIL)
!-----------------------------------------------------------------------
! SOLVES A CUBIC EQUATION WITH A SOLUTION WITH -1.< X < 1
! CN : THE COEFFICIENT OF X**N, X : THE REAL SOLUTION WITH -1.< X < 1.
!-----------------------------------------------------------------------
use itm_types
implicit none
real (r8)  :: C0, C1, C2, C3, X1, X2, X3, dum, tol, angle
real (r8)  :: a0, a1, a2, aa, bb, cc, det, pi, p, q, u, v
integer :: ifail

x1    = 99.e0_R8
x2    = 999.e0_R8
x3    = 9999.e0_R8
tol   = 0.
ifail = 0

!------------------------------------- 2nd order poly for small c3
IF (ABS(C3)/(ABS(C1)+ABS(C2)+ABS(C3)) .LT. 1.d-9) THEN
  AA = C2
  BB = C1
  CC = C0
  DET = BB**2 - 4*AA*CC
  IF (DET.GE.0.) THEN
    X1 = ROOT(AA,BB,CC,DET,1.e0_R8)
    IF (ABS(X1).GT. 1.e0_R8 + TOL) THEN
      X1 = ROOT(AA,BB,CC,DET,-1.e0_R8)
    ENDIF
  ELSE
    IFAIL = 1
  ENDIF

ELSE
!------------------------------------- 3rd order poly solution
  PI = 2*ASIN(1.e0_R8)
  A0 = C0 / C3
  A1 = C1 / C3
  A2 = C2 / C3
  P = - (A2**2)/3.e0_R8 + A1
  Q = 2.e0_R8/27.e0_R8*(A2**3) - A2 * A1/3.e0_R8 + A0
  DET = (P/3.e0_R8)**3 + (Q/2.e0_R8)**2
  IF (DET .GE. 0) THEN
    U  = SIGN(1.e0_R8,-Q/2.e0_R8+SQRT(DET))*ABS(-Q/2.e0_R8 + SQRT(DET))**(1.e0_R8/3.e0_R8)
    V  = SIGN(1.e0_R8,-Q/2.e0_R8-SQRT(DET))*ABS(-Q/2.e0_R8 - SQRT(DET))**(1.e0_R8/3.e0_R8)
    X1 =  U + V - A2/3.e0_R8
    IF (ABS(X1) .GE. (1.e0_R8+TOL)) IFAIL = 1
  ELSE
    P = -P
    ANGLE = SIGN(1.e0_R8,P)*ACOS((Q/2.e0_R8)/SQRT(ABS(P)/3.e0_R8)**3)
    X1 = -2.e0_R8*SQRT(ABS(P)/3.e0_R8)*COS(ANGLE/3.e0_R8) - A2/3.e0_R8
    X2 = -2.e0_R8*SQRT(ABS(P)/3.e0_R8)*COS(2*PI/3.e0_R8 - ANGLE/3.e0_R8) - A2/3.e0_R8
    X3 = -2.e0_R8*SQRT(ABS(P)/3.e0_R8)*COS(2*PI/3.e0_R8 + ANGLE/3.e0_R8) - A2/3.e0_R8
  ENDIF
  IF (ABS(X1) .GT. ABS(X2)) THEN
    DUM = X1
    X1 = X2
    X2 = DUM
  ENDIF
  IF (ABS(X2) .GT. ABS(X3)) THEN
    DUM = X2
    X2 = X3
    X3 = DUM
  ENDIF
  IF (ABS(X1) .GT. ABS(X2)) THEN
    DUM = X1
    X1 = X2
    X2 = DUM
  ENDIF
ENDIF
IF (ABS(X1) .GT. (1.e0_R8 + TOL)) IFAIL=1

RETURN
END SUBROUTINE SOLVP3

FUNCTION ROOT(A,B,C,D,SGN)
!---------------------------------------------------------------------
! THIS FUNCTION GIVES BETTER ROOTS OF QUADRATICS BY AVOIDING
! CANCELLATION OF SMALLER ROOT
!---------------------------------------------------------------------
use itm_types
implicit none
real (r8) :: root,a, b, c, d, sgn

IF (B*SGN .GE. 0.e0_R8) THEN
  ROOT = -2.e0_R8*C/(B+SGN*SQRT(D))
ELSE
  ROOT = (-B + SGN*SQRT(D)) / (2.e0_R8 * A)
ENDIF
RETURN
END FUNCTION ROOT


subroutine psi_minmax(n,psimin,psimax)
use helena_grid
use helena_elements
use itm_types
implicit none
real (r8)  :: psimin, psimax, psma, psmi, psmima, psim, psimr, psip, psipr
real (r8)  :: aa, bb, cc, det, r, dummy
integer :: i, n, im, n1, n2

psimin = 1d10
psimax =-1d10

do i=1, 4

  im = MOD(I,4) + 1
  n1 = elm_list(i,n)
  n2 = elm_list(im,n)

  IF (i .eq. 1) THEN
    PSIM  = PSI(1,n1)
    PSIMR = PSI(3,n1)
    PSIP  = PSI(1,n2)
    PSIPR = PSI(3,n2)
  ELSEif (i .eq. 2) then
    PSIM  = PSI(1,n1)
    PSIMR = PSI(2,n1)
    PSIP  = PSI(1,n2)
    PSIPR = PSI(2,n2)
  ELSEIF (i .eq. 3) then
    PSIM  =   PSI(1,n1)
    PSIMR = - PSI(3,n1)
    PSIP  =   PSI(1,n2)
    PSIPR = - PSI(3,n2)
  ELSEIF (i .eq. 4) then
    PSIM  =   PSI(1,n1)
    PSIMR = - PSI(2,n1)
    PSIP  =   PSI(1,n2)
    PSIPR = - PSI(2,n2)
  ENDIF

  PSMA = MAX(PSIM,PSIP)
  PSMI = MIN(PSIM,PSIP)
  AA =  3.e0_R8 * (PSIM + PSIMR - PSIP + PSIPR ) / 4.e0_R8
  BB =  ( - PSIMR + PSIPR ) / 2.e0_R8
  CC =  ( - 3*PSIM - PSIMR + 3*PSIP - PSIPR) / 4.e0_R8
  DET = BB**2 - 4*AA*CC
  IF (DET .GE. 0.e0_R8) THEN
    R = (-BB + SQRT(BB**2-4*AA*CC) ) / (2*AA)
    IF (ABS(R) .GT. 1.e0_R8) THEN
      R = (-BB - SQRT(BB**2-4*AA*CC) ) / (2*AA)
    ENDIF
    IF (ABS(R) .LE. 1.e0_R8) THEN
      CALL CUB1D(PSIM,PSIMR,PSIP,PSIPR,R,PSMIMA,DUMMY)
      psma = max(psma,psmima)
      psmi = min(psmi,psmima)
    ENDIF
  ENDIF
  psimin = min(psimin,psmi)
  psimax = max(psimax,psma)
ENDDO

RETURN
END subroutine psi_minmax

SUBROUTINE tht_minmax(n,thtmin,thtmax)
use helena_grid
use helena_elements
use helena_axis
use helena_constants
use itm_types
implicit none
real (r8)  :: thtmin, thtmax, theta
integer :: i, node, n

thtmin = 1d10
thtmax =-1d10

do i=1,4
  node = elm_list(i,n)
  theta = ATAN2(ZZ(1,node)-Z_AXIS,RR(1,node)-R_AXIS)
  if (theta .lt. 0.) then
    theta = theta + 2.e0_R8*PI
  endif
  thtmin = min(theta,thtmin)
  thtmax = max(theta,thtmax)
enddo

return
end SUBROUTINE tht_minmax

subroutine initialise_profiles(n_prof_in,iopt_p,iopt_f,psi_in, pprime_in, ffprime_in,pressure_in, fdia_in, current_in)
!-------------------------------------------------------------------------------
! initialising the profiles contained in helena_profiles
!
!  iopt_p = 1  : use pressure and psi profile
!  iopt_p = 2  : use pprime profile
!
!  iopt_f = 1  : use f_dia and psi profile
!  iopt_f = 2  : use ffprime profile
!  iopt_f = 3  : use current profile and ffprime profile
!  iopt_f = 4  : use current profile and reinitialise ffprime
!-------------------------------------------------------------------------------
use helena_profiles
use helena_boundary
use helena_axis
use helena_constants
use itm_types
implicit none
real (r8)             :: psi, dpsi, sump, sumf, F0, f_old, f_tmp
real (r8),allocatable :: psi_norm(:), psi_prof(:), sp1(:) ,sp2(:), sp3(:), sp4(:)
real (r8)             :: pprime_in(*), ffprime_in(*), pressure_in(*), fdia_in(*), current_in(*), psi_in(*)
integer          :: i, n_prof_in, iopt_p, iopt_f
real (r8)             :: abltg(3), p_tmp, si, Rbnd_av, ORbnd_av, R_av, OR_av, ps0, ps1
!real (r8)             :: spwert

write(*,'(A)')      ' **************************************'
write(*,'(A,i7,A)') ' * initialising profiles',n_prof_in,'       *'
write(*,'(A)')      ' **************************************'

ps0     = psi_in(1)
ps1     = psi_in(n_prof_in)
ps_axis = abs(ps1 - ps0)

p_bnd   = pressure_in(n_prof_in)

allocate(psi_norm(n_prof_in))

do i=1,n_prof_in
  psi_norm(i) = (psi_in(i)- ps0)/(ps1 - ps0)
!  write(*,'(i5,8e12.4)') i,psi_in(i),psi_norm(i),pprime_in(i),ffprime_in(i),pressure_in(i),fdia_in(i),current_in(i)
enddo
  
allocate(sp1(n_prof_in),sp2(n_prof_in),sp3(n_prof_in),sp4(n_prof_in))

n_prof = 101
allocate(psi_prof(n_prof), dp_dpsi(n_prof), zjz_psi(n_prof), fdf_dpsi(n_prof))

if (iopt_p .eq. 1) then       ! calculate pprime from pressure profile

  write(*,*) ' HELENA : using pressure profile as input'

  call spline(n_prof_in,psi_norm,pressure_in,0.e0_R8,0.e0_R8,2,sp1,sp2,sp3,sp4)

  do i = 1, n_prof

    psi_prof(i) = float(i-1)/float(n_prof-1)

    p_tmp = spwert(n_prof_in,psi_prof(i),sp1,sp2,sp3,sp4,psi_norm,abltg)

    dp_dpsi(i) = abltg(1) / abs(ps_axis)

  enddo

  dp_dpsi(1) = dp_dpsi(3)
  dp_dpsi(2) = dp_dpsi(3)

else

  write(*,*) ' HELENA : using pprime profile as input'
    
  call spline(n_prof_in,psi_norm,pprime_in,0.e0_R8,0.e0_R8,2,sp1,sp2,sp3,sp4)

  do i = 1, n_prof

    psi_prof(i) = float(i-1)/float(n_prof-1)

    dp_dpsi(i) = spwert(n_prof_in,psi_prof(i),sp1,sp2,sp3,sp4,psi_norm,abltg)

  enddo

endif

if (iopt_f .eq. 1) then       ! calculate ffprime from fdia profile

  write(*,*) ' HELENA : using fdia profile as input'

  call spline(n_prof_in,psi_norm,fdia_in,0.0_R8,0.0_R8,2,sp1,sp2,sp3,sp4)

  do i = 1, n_prof

    psi_prof(i) = float(i-1)/float(n_prof-1)

    f_tmp = spwert(n_prof_in,psi_prof(i),sp1,sp2,sp3,sp4,psi_norm,abltg)

    fdf_dpsi(i) = f_tmp * abltg(1) / abs(ps_axis)

  enddo

  fdf_dpsi(1) = fdf_dpsi(3)
  fdf_dpsi(2) = fdf_dpsi(3)

elseif ((iopt_f .eq. 2) .or. (iopt_f .eq. 3)) then

  write(*,*) ' HELENA : using ffprime profile as input'
    
  call spline(n_prof_in,psi_norm,ffprime_in,0.e0_R8,0.e0_R8,2,sp1,sp2,sp3,sp4)

  do i = 1, n_prof

    psi_prof(i) = float(i-1)/float(n_prof-1)

    fdf_dpsi(i) = spwert(n_prof_in,psi_prof(i),sp1,sp2,sp3,sp4,psi_norm,abltg)

  enddo

elseif ((iopt_f .eq. 3) .or. (iopt_f .eq. 4)) then

!DPC this is a guess!  I'm also not sure why both this and the previous option overlap
  write(*,*) ' HELENA : using current profile as input'

  call spline(n_prof_in,psi_norm,current_in,0.e0_R8,0.e0_R8,2,sp1,sp2,sp3,sp4)

  Rbnd_av  = 0.e0_R8
  ORbnd_av = 0.e0_R8
  do i=1,n_bnd
    Rbnd_av  = Rbnd_av  + R_bnd(i)
    ORbnd_av = ORbnd_av + 1.e0_R8/ R_bnd(i)
  enddo
  Rbnd_av  = Rbnd_av  / float(n_bnd)
  ORbnd_av = ORbnd_av / float(n_bnd)

  do i = 1, n_prof

    zjz_psi(i) = spwert(n_prof_in,psi_prof(i),sp1,sp2,sp3,sp4,psi_norm,abltg)

    si = sqrt(psi_prof(i))

    R_av  = Rgeo    + si * ( Rbnd_av - Rgeo)
    OR_av = 1./Rgeo + si * (ORbnd_av - 1./Rgeo)

    if (iopt_f .eq. 4) fdf_dpsi(i) = ( MU_zero * ( - zjz_psi(i) - R_av * dp_dpsi(i) )/ OR_av )

  enddo

endif

allocate(p_psi(n_prof),f_psi(n_prof))

dpsi = 1./float(n_prof)
sump = 0.
sumf = 0.
p_psi = 0.
f_psi = 0.

p_psi(n_prof) = p_bnd

do i = 1, n_prof - 1
  sump = sump - (dp_dpsi(n_prof-i+1)+dp_dpsi(n_prof-i))*dpsi/2.
  p_psi(n_prof-i) = sump * abs(ps_axis) + p_bnd

  sumf = sumf - (fdf_dpsi(n_prof-i+1)+fdf_dpsi(n_prof-i))*dpsi/2.
  f_psi(n_prof-i) = sumf * abs(ps_axis)

!DPC  write(*,*) i, p_psi(n_prof-i),f_psi(n_prof-i)
enddo

call lplot6(2,2,psi_prof,dp_dpsi,n_prof,'dp/dpsi')
call lplot6(2,3,psi_prof,fdf_dpsi,n_prof,'fdf/dpsi')
call lplot6(3,2,psi_prof,p_psi,n_prof,'pressure')
call lplot6(3,3,psi_prof,f_psi,n_prof,'F**2-F0**2')

return
end subroutine initialise_profiles

subroutine plot_solution
use helena_grid
use helena_elements
use itm_types
implicit none
integer :: i, ngr, ngs, kf, kv, vertex, nodes(4), nc
real (r8)    :: psi_max, psi_min, Rmin, Rmax, Zmin, Zmax, zc(3)
real (r8)    :: XX(4,4), YY(4,4), PS(4,4)
character*19 :: label

LABEL= 'SOLUTION'

psi_max = maxval(psi(1,:))
psi_min = minval(psi(1,:))

CALL NFRAME(22,11,1,R_min,R_max,Z_min,Z_max,LABEL,19,'R [m]',5,'Z [m]',5)

nc = 3
zc(1) = -max(abs(psi_max),abs(psi_min))
zc(2) = 0.
zc(3) = abs(zc(1))

do i=1,n_elms

  nodes(1) = elm_list(1,i)
  nodes(2) = elm_list(2,i)
  nodes(3) = elm_list(3,i)
  nodes(4) = elm_list(4,i)

  XX = 0.
  YY = 0.
  PS = 0.

  do ngr = 1, 4          ! 4 Gaussian points
    do ngs = 1, 4        ! 4 Gaussian points

      do kf = 1, 4       ! 4 basis functions
        do kv = 1, 4     ! 4 vertices

           vertex = nodes(kv)

           XX(ngr,ngs) = XX(ngr,ngs) + RR(kf,vertex)  * H(ngr,ngs,kv,kf)
           YY(ngr,ngs) = YY(ngr,ngs) + ZZ(kf,vertex)  * H(ngr,ngs,kv,kf)
	   PS(ngr,ngs) = PS(ngr,ngs) + PSI(kf,vertex) * H(ngr,ngs,kv,kf)

        enddo
      enddo
    enddo
  enddo

  call cplotm(2,1,-2,XX,YY,4,-4,1,1,PS,4,zc,nc,'Solution',8,'R [m]',5,'Z [m]',5)

enddo
call lincol(0)
return
end subroutine plot_solution


subroutine plot_elements
use helena_grid
use helena_elements
use itm_types
implicit none
integer      :: i, n1, n2, n3, n4
real (r8)         :: Rmin, Rmax, Zmin, Zmax
character*19 :: label

LABEL= 'FINITE ELEMENT GRID'

Rmin = minval(RR(1,1:nr*np)); Rmax = maxval(RR(1,1:nr*np))
Zmin = minval(ZZ(1,1:nr*np)); Zmax = maxval(ZZ(1,1:nr*np))

CALL NFRAME(22,11,1,Rmin,Rmax,Zmin,Zmax,LABEL,19,'R [m]',5,'Z [m]',5)

do i=1, n_elms

    n1 = elm_list(1,i)
    n2 = elm_list(2,i)
    n3 = elm_list(3,i)
    n4 = elm_list(4,i)

    call PLOTCU(RR(1,n1), RR(3,n1), ZZ(1,n1), ZZ(3,n1), RR(1,n2), RR(3,n2), ZZ(1,n2), ZZ(3,n2))
    call PLOTCU(RR(1,n2), RR(2,n2), ZZ(1,n2), ZZ(2,n2), RR(1,n3), RR(2,n3), ZZ(1,n3), ZZ(2,n3))
    call PLOTCU(RR(1,n4), RR(3,n4), ZZ(1,n4), ZZ(3,n4), RR(1,n3), RR(3,n3), ZZ(1,n3), ZZ(3,n3))
    call PLOTCU(RR(1,n1), RR(2,n1), ZZ(1,n1), ZZ(2,n1), RR(1,n4), RR(2,n4), ZZ(1,n4), ZZ(2,n4))

enddo

return
end subroutine plot_elements


subroutine solve_matrix(amix,solve_only,error_iteration)
use helena_matrix
use helena_grid
use helena_elements
use itm_types
implicit none
logical :: solve_only
integer :: i, j, istart, info
real (r8)    :: error_iteration, amix

!write(*,*) n_matrix,n_diag,lda

if (.not. solve_only ) then
  call DPBTRF('L',n_matrix,n_diag,AA,lda,INFO) ! factorisation (lapack)
  if (info .ne. 0) write(*,*) ' dpbtrf : ',info
endif

call DPBTRS('L',n_matrix,n_diag,1,AA,lda,BB,n_matrix,INFO)  ! solve (lapack)
if (info .ne. 0) write(*,*) ' dpbtrs : ',info

error_iteration = 0

do i=1,n_nodes
  do j=1,4
    error_iteration = error_iteration + abs(PSI(j,index_inverse(i)) - BB(4*(i-1)+j))
    PSI(j,index_inverse(i)) = amix * PSI(j,index_inverse(i)) + (1. - amix) * BB(4*(i-1)+j)
  enddo
enddo

error_iteration = error_iteration / float(4*n_nodes)

return
end subroutine solve_matrix


subroutine matrix_GS(ps_axis,rhs_only)
use helena_grid
use helena_elements
use helena_Gauss_4
use helena_matrix
use helena_constants
use itm_types
implicit none

logical :: rhs_only
integer :: nodes(4)
real (r8)    :: XX(4,4), XR(4,4), XS(4,4), YR(4,4), YS(4,4), XJAC(4,4), PS(4,4), PSN(4,4)
real (r8)    :: VX(4,4,4,4), VY(4,4,4,4), RHS(4,4)
real (r8)    :: ps_axis, sumQ, sumK
integer :: ngr, ngs, kf, kv, lf ,lv, i, vertex, nrow, nrow2, ncol, ncol2
!real (r8) fdfdpsi,dpdpsi
integer n_dof,noff,jstart,j

lda      = 4*(np+1) + 9
n_dof    = 4*nr*np
n_matrix = n_dof
n_diag   = 4*(np+1) + 7

if (.not. allocated(AA)) allocate(AA(lda,n_dof))
if (.not. allocated(BB)) allocate(BB(n_dof))

if (.not. rhs_only) AA = 0.
BB = 0.

do i=1, n_elms

  nodes(1) = elm_list(1,i)
  nodes(2) = elm_list(2,i)
  nodes(3) = elm_list(3,i)
  nodes(4) = elm_list(4,i)

  XR = 0.
  XS = 0.
  YR = 0.
  YS = 0.
  XX = 0.
  PS = 0.

  do ngr = 1, 4          ! 4 Gaussian points
    do ngs = 1, 4        ! 4 Gaussian points
      do kf = 1, 4       ! 4 basis functions
        do kv = 1, 4     ! 4 vertices

          vertex = nodes(kv)

          XX(ngr,ngs) = XX(ngr,ngs) + RR(kf,vertex)  * H(ngr,ngs,kv,kf)
          PS(ngr,ngs) = PS(ngr,ngs) + PSI(kf,vertex) * H(ngr,ngs,kv,kf)

	  XR(ngr,ngs) = XR(ngr,ngs) + RR(kf,vertex) * HR(ngr,ngs,kv,kf)
          XS(ngr,ngs) = XS(ngr,ngs) + RR(kf,vertex) * HS(ngr,ngs,kv,kf)
          YR(ngr,ngs) = YR(ngr,ngs) + ZZ(kf,vertex) * HR(ngr,ngs,kv,kf)
          YS(ngr,ngs) = YS(ngr,ngs) + ZZ(kf,vertex) * HS(ngr,ngs,kv,kf)

        enddo
      enddo

      XJAC(ngr,ngs) = XR(ngr,ngs)*YS(ngr,ngs) - XS(ngr,ngs)*YR(ngr,ngs)

    enddo
  enddo

  PSN(:,:) = (ps_axis - PS(:,:)) / ps_axis

  do ngr = 1, 4
    do ngs = 1, 4

      RHS(ngr,ngs) = - ( fdfdpsi(PSN(ngr,ngs)) + MU_zero * XX(ngr,ngs)*XX(ngr,ngs)*dpdpsi(PSN(ngr,ngs)) )  &
                   *  wgauss(ngr)*wgauss(ngs) * XJAC(ngr,ngs) / XX(ngr,ngs)

    enddo
  enddo

  do kf = 1, 4
    do kv = 1, 4
      do ngr = 1, 4
        do ngs = 1, 4

          VX(ngr,ngs,kf,kv) =  YS(ngr,ngs) * HR(ngr,ngs,kv,kf)  - YR(ngr,ngs) * HS(ngr,ngs,kv,kf)
          VY(ngr,ngs,kf,kv) = -XS(ngr,ngs) * HR(ngr,ngs,kv,kf)  + XR(ngr,ngs) * HS(ngr,ngs,kv,kf)

        enddo
      enddo
    enddo
  enddo

  do kv = 1, 4

    NROW = 4*(index_list(nodes(kv)) - 1)

    do kf = 1, 4

      NROW2 = NROW + kf

      SUMQ = 0.

      do ngr = 1, 4
        do ngs = 1, 4

          SUMQ = SUMQ - RHS(ngr,ngs) * H(ngr,ngs,kv,kf)

        enddo
      enddo

      BB(NROW2) = BB(NROW2) + SUMQ

      if (.not. rhs_only) then

        do lv = 1, 4

          NCOL = 4*(index_list(nodes(lv)) - 1)

          do lf = 1, 4

            NCOL2 = NCOL + lf

            NOFF = NROW2 - NCOL2

            if (noff .ge. 0) then

              SUMK = 0.

              do ngr = 1, 4
                do ngs = 1, 4

                  SUMK = SUMK + wgauss(ngr)*wgauss(ngs) / (XJAC(ngr,ngs) * XX(ngr,ngs)) &
                       * (VX(ngr,ngs,lf,lv) * VX(ngr,ngs,kf,kv) + VY(ngr,ngs,lf,lv) * VY(ngr,ngs,kf,kv))

                enddo
              enddo

              AA(NOFF+1,NCOL2) = AA(NOFF+1,NCOL2) + SUMK

            endif

          enddo
        enddo
      endif
    enddo
  enddo

enddo

!------------------------------- boundary conditions
if (.not. rhs_only) then
  jstart = 4.*(nr-1)*np
  do j=1,np
    AA(1,jstart+4*j-1) = 1.e20
    AA(1,jstart+4*j-3) = 1.e20
  enddo
endif
return
end subroutine matrix_GS


subroutine initialise_elements
use helena_grid
use helena_elements
use helena_gauss_4
use itm_types
implicit none
integer :: i,j, ij1, ijn, jn, k, l, ngr, ngs, kv, kf, i0, j0
real (r8)    :: r, s, r0, s0

n_elms  = (nr-1)*np
n_nodes = nr*np
allocate(elm_list(4,n_elms), index_list(n_nodes), index_inverse(n_nodes))

k=0
do i=1,nr-1
  do j=1,np-1
    k = k + 1
    elm_list(1,k) = (i-1)*np + j
    elm_list(2,k) = (i-1)*np + j + 1
    elm_list(3,k) = (i  )*np + j + 1
    elm_list(4,k) = (i  )*np + j
  enddo
  k = k + 1
  elm_list(1,k) = (i  )*np
  elm_list(2,k) = (i  )*np - np + 1
  elm_list(3,k) = (i  )*np + 1
  elm_list(4,k) = (i  )*np + np
enddo

write(*,*) ' number of elements : ',k

do i = 1, nr

  ij1 = (i-1)*np  + 1         ! the first poloidal point
  index_list(ij1)    = ij1
  index_inverse(ij1) = ij1

  do j = 1, np/2               ! the upper part of the poloidal plane
    ij1 = (i-1)*np + j   + 1
    ijn = (i-1)*np + 2*j
    index_list(ij1)    = ijn
    index_inverse(ijn) = ij1
  enddo

  do j = 1, np/2 - 1
    ij1 = i*np - j + 1
    ijn = (i-1)*np + 2*j + 1
    index_list(ij1)    = ijn
    index_inverse(ijn) = ij1
  enddo

enddo

RS(1,1) = -1.; RS(1,2) = -1.; RS(2,1) = -1.; RS(2,2) = +1.
RS(3,1) = +1.; RS(3,2) = +1.; RS(4,1) = +1.; RS(4,2) = -1.
IJ(1,1) = 0;   IJ(1,2) = 0;   IJ(2,1) = 1;   IJ(2,2) = 0
IJ(3,1) = 0;   IJ(3,2) = 1;   IJ(4,1) = 1;   IJ(4,2) = 1

do ngr = 1, 4        ! 4 Gaussian points
  do ngs = 1, 4      ! 4 Gaussian points

    r = xgauss(ngr)
    s = xgauss(ngs)

    do kv=1,4    ! 4 corners

      R0 = RS(kv,1)
      S0 = RS(kv,2)

      do kf=1,4  ! 4 basis functions

        I0 = IJ(kf,1)
	J0 = IJ(kf,2)
        call CUBICH(I0,J0,R0,S0,r,s,H(ngr,ngs,kv,kf),  HR(ngr,ngs,kv,kf), HS(ngr,ngs,kv,kf),&
	                            HRS(ngr,ngs,kv,kf),HRR(ngr,ngs,kv,kf),HSS(ngr,ngs,kv,kf))

      enddo

    enddo
  enddo
enddo

return
end subroutine initialise_elements



subroutine initialise_grid
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
use helena_grid
use helena_boundary
use itm_types
implicit none
integer          :: i, j, m, node
real (r8)             :: theta(np), s(nr)
real (r8)             :: pi, dt, dr, rm, drm, drmt, drmtr, radius, thtj

allocate(RR(4,nr*np),ZZ(4,nr*np),PSI(4,nr*np))

pi = 2.*asin(1.)
dt = 2.*pi/real(np)
dr = 1./real(nr-1)

do j=1,np
  theta(j) = dt * real(j-1)
enddo

do i=1,nr
  s(i)   = float(i-1)/float(nr-1)
enddo

do i=1,nr
  do  j=1,np
    node   = np*(i-1) + j
    thtj   = theta(j)
    radius = s(i)

    RR(1,node) = Rgeo + radius * fr(1) * cos(thtj) / 2.
    RR(2,node) =                 fr(1) * cos(thtj) / 2.
    RR(3,node) =      - radius * fr(1) * sin(thtj) / 2.
    RR(4,node) =               - fr(1) * sin(thtj) / 2.
    ZZ(1,node) = Zgeo + radius * fr(1) * sin(thtj) / 2.
    ZZ(2,node) =                 fr(1) * sin(thtj) / 2.
    ZZ(3,node) =        radius * fr(1) * cos(thtj) / 2.
    ZZ(4,node) =                 fr(1) * cos(thtj) / 2.

!---------------------------- KEEP ELLIPTICITY ON AXIS -----------
    do m = 2, mf/2
      if (m .eq. 2) then
        rm   = radius * ( fr(2*M-1) * cos((M-1)*THTJ)           + fr(2*M) * sin((M-1)*THTJ) )
        drm  =          ( fr(2*M-1) * cos((M-1)*THTJ)           + fr(2*M) * sin((M-1)*THTJ))
        drmt = radius * (-fr(2*M-1) * (M-1)*sin((M-1)*THTJ)     + fr(2*M) * (M-1)*cos((M-1)*THTJ))
        drmtr=          (-fr(2*M-1) * (M-1)*sin((M-1)*THTJ)     + fr(2*M) * (M-1)*cos((M-1)*THTJ))
      else
        rm   =      radius**(M-1) * ( fr(2*M-1) * cos((M-1)*THTJ)       + fr(2*M) * sin((M-1)*THTJ) )
        drm  =(M-1)*radius**(M-2) * ( fr(2*M-1) * cos((M-1)*THTJ)       + fr(2*M) * sin((M-1)*THTJ))
        drmt =      radius**(M-1) * (-fr(2*M-1) * (M-1)*sin((M-1)*THTJ) + fr(2*M) *(M-1)*cos((M-1)*THTJ))
        drmtr=(M-1)*radius**(M-2) * (-fr(2*M-1) * (M-1)*sin((M-1)*THTJ) + fr(2*M) *(M-1)*cos((M-1)*THTJ))
      endif
      RR(1,node) = RR(1,node) + rm  * cos(THTJ)
      ZZ(1,node) = ZZ(1,node) + rm  * sin(THTJ)
      RR(2,node) = RR(2,node) + drm * cos(THTJ)
      ZZ(2,node) = ZZ(2,node) + drm * sin(THTJ)
      RR(3,node) = RR(3,node) - rm  * sin(THTJ) + drmt  * cos(THTJ)
      ZZ(3,node) = ZZ(3,node) + rm  * cos(THTJ) + drmt  * sin(THTJ)
      RR(4,node) = RR(4,node) - drm * sin(THTJ) + drmtr * cos(THTJ)
      ZZ(4,node) = ZZ(4,node) + drm * cos(THTJ) + drmtr * sin(THTJ)
    enddo
    RR(2,node) = RR(2,node) * dr/2.
    RR(3,node) = RR(3,node) * dt/2.
    RR(4,node) = RR(4,node) * dr/2. * dt/2.
    ZZ(2,node) = ZZ(2,node) * dr/2.
    ZZ(3,node) = ZZ(3,node) * dt/2.
    ZZ(4,node) = ZZ(4,node) * dr/2. * dt/2.

    PSI(1,node) = radius **2  - 1.
    PSI(2,node) = 2.* radius * dr / 2.
    PSI(3,node) = 0.
    PSI(4,node) = 0.

  enddo
enddo

R_min = minval(RR(1,:))
R_max = maxval(RR(1,:))
Z_min = minval(ZZ(1,:))
Z_max = maxval(ZZ(1,:))

return
end subroutine initialise_grid

subroutine plot_grid(R,Z,nr,np)
!-------------------------------------------------------------------
! THE X,Y GRID IS PLOTTED USING THE ISOPARAMETRIC REPRESENTATION
!-------------------------------------------------------------------
use itm_types
implicit none
real (r8)         :: R(4,*),Z(4,*), Rmin, Rmax, Zmin, Zmax
character*19 :: label
integer      :: nr, np, nbase, i, j

LABEL= 'FINITE ELEMENT GRID'

Rmin = minval(R(1,1:nr*np));  Rmax = maxval(R(1,1:nr*np))
Zmin = minval(Z(1,1:nr*np));  Zmax = maxval(Z(1,1:nr*np))

CALL NFRAME(22,1,1,Rmin,Rmax,Zmin,Zmax,LABEL,19,'R [m]',5,'Z [m]',5)

do i=1, nr
  do j=1, np-1
    nbase = j + (i-1)*np
    call PLOTCU(R(1,nbase),  R(3,nbase),  Z(1,nbase),  Z(3,nbase), &
                R(1,nbase+1),R(3,nbase+1),Z(1,nbase+1),Z(3,nbase+1))
  enddo
enddo
do i=1, nr-1
  do j=1, np-1
    nbase = j + (i-1)*np
    call PLOTCU(R(1,nbase),   R(2,nbase),    Z(1,nbase),   Z(2,nbase), &
                R(1,nbase+np),R(2,nbase+np), Z(1,nbase+np),Z(2,nbase+np))
  enddo
enddo

return
end subroutine plot_grid


subroutine plotcu(X1,X1S,Y1,Y1S,X2,X2S,Y2,Y2S)
!-----------------------------------------------------------------------
! PLOTS A CUBIC LINE FROM X1,Y1 TO X2,Y2 GIVEN BY THE ARRAYS XI(1..4)
! AND YI(1..4) in an existing frame of PPPLIB
!-----------------------------------------------------------------------
use itm_types
implicit none
integer :: nplot, i
parameter (nplot=21)
real (r8)    :: X1,X1S,Y1,Y1S,X2,X2S,Y2,Y2S
real (r8)    :: XP(nplot),YP(nplot), s, dummy

do i=1,nplot
  s = -1. + 2.*float(i-1)/float(nplot-1)
  call cub1D(X1,X1S,X2,X2S,s,xp(I),dummy)
  call cub1D(Y1,Y1S,Y2,Y2S,s,yp(I),dummy)
enddo
call lplot6(2,1,xp,yp,-nplot,' ')

return
end subroutine plotcu

SUBROUTINE CUBICH(I,J,R0,S0,R,S,H,HR,HS,HRS,HRR,HSS)
!------------------------------------------------------------------
! SUBROUTINE TO CALCULATE THE VALUE OF THE CUBIC POLYNOMIALS AND
! THE DERIVATIVES OF THE CORNER MARKED BY R0,S0 AT THE POSITION R,S
!------------------------------------------------------------------
use itm_types
implicit none
integer :: i,j
real (r8)    :: H,HR,HS,HRS,HI,HRI,HJ,HSJ,HRR,HSS,HRRI,HSSJ, R0, S0, R, S

IF (I.EQ.0) THEN
  HI = - (R+R0)**2 * (R*R0-2.) / 4.
  HRI = - (R+R0)*(R*R0-2.)/2. - R0*(R+R0)**2 / 4.
  HRRI = - 1.5 * R * R0
ELSE
  HI = + R0 * (R+R0)**2 * (R*R0 - 1.) / 4.
  HRI = + R0*(R+R0)*(R*R0-1.)/2. + (R+R0)**2 /4.
  HRRI = 1.5*R + .5*R0
ENDIF
IF (J.EQ.0) THEN
  HJ = - (S+S0)**2 * (S*S0-2.) / 4.
  HSJ = - (S+S0)*(S*S0-2.)/2. - S0*(S+S0)**2 / 4.
  HSSJ = - 1.5 * S * S0
ELSE
  HJ = + S0 * (S+S0)**2 * (S*S0 - 1.) / 4.
  HSJ = + S0*(S+S0)*(S*S0-1.)/2. + (S+S0)**2 / 4.
  HSSJ = 1.5*S + .5*S0
ENDIF
H   = HI   * HJ
HR  = HRI  * HJ
HS  = HI   * HSJ
HRS = HRI  * HSJ
HRR = HRRI * HJ
HSS = HI   * HSSJ
RETURN
END SUBROUTINE CUBICH

subroutine cub1D(X1,X1S,X2,X2S,S,X,XS)
!-----------------------------------------------------------------------
! CUBIC HERMITE INTERPOLATION IN ONE DIMENSION
!-----------------------------------------------------------------------
use itm_types
implicit none
real (r8)   :: X1,X1S,X2,X2S,S,X,XS
real (r8)   :: H0M,H0P,H1M,H1P,H0MS,H0PS,H1MS,H1PS

H0M  =  (S-1.e0_R8)**2 *(S+2.e0_R8) * 0.25e0_R8
H0MS =  (S-1.e0_R8)*(S+2.e0_R8)/2.e0_R8 + (S-1.e0_R8)**2 * 0.25e0_R8
H0P  = -(S+1.e0_R8)**2 *(S-2.e0_R8) * 0.25e0_R8
H0PS = -(S+1.e0_R8)*(S-2.e0_R8)/2.e0_R8 - (S+1.e0_R8)**2 * 0.25e0_R8
H1M  =  (S-1.e0_R8)**2 *(S+1.e0_R8) * 0.25e0_R8
H1MS =  (S-1.e0_R8)*(S+1.e0_R8)/2. + (S-1.e0_R8)**2 * 0.25e0_R8
H1P  =  (S+1.e0_R8)**2 *(S-1.e0_R8) * 0.25e0_R8
H1PS =  (S+1.e0_R8)*(S-1.e0_R8)/2.e0_R8 + (S+1.e0_R8)**2 * 0.25e0_R8

X  = X1*H0M  + X1S*H1M +  X2*H0P  + X2S*H1P
XS = X1*H0MS + X1S*H1MS + X2*H0PS + X2S*H1PS

return
end subroutine cub1D

subroutine fshape
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
use helena_boundary
use helena_constants
use itm_types
implicit none
real (r8)               :: xj, yj, ga
real (r8), allocatable  :: THETA(:), GAMMA(:), XV(:),YV(:)
real (r8)               :: angle, error, gamm
real (r8), allocatable  :: tht_tmp(:),  fr_tmp(:), work(:)
real (r8), allocatable  :: tht_sort(:), fr_sort(:), dfr_sort(:)
integer, allocatable :: index_order(:)
real (r8)               :: tht, Rbnd_av, ORbnd_av, values(4)
integer              :: m, igrinv, i, j, ishape, ieast(1), iwest(1), n_bnd_short
parameter (error = 1.d-8)

allocate(fr(mf+2))
allocate(theta(mf),gamma(mf),xv(mf),yv(mf))

if (n_bnd .gt. 1) then

  write(*,*) ' fshape : (R,Z) set given on ',n_bnd,' points'

  Reast = maxval(R_bnd)
  Rwest = minval(R_bnd)
  ieast = maxloc(R_bnd)
  iwest = minloc(R_bnd)
  Rgeo = (Reast + Rwest) /2.
  Zgeo = (Z_bnd(ieast(1))+Z_bnd(iwest(1)))/2.
  amin = (Reast - Rwest)/2.

  write(*,'(A,3f12.8)') ' Rgeo, Zgeo : ',Rgeo,Zgeo,amin

  allocate(tht_tmp(n_bnd),fr_tmp(n_bnd),work(3*n_bnd+6))
  allocate(tht_sort(n_bnd+2),fr_sort(n_bnd+2),dfr_sort(n_bnd+2))
  allocate(index_order(n_bnd+2))

  Rbnd_av  = 0.
  ORbnd_av = 0.

  do i=1,n_bnd

    tht_tmp(i) = atan2(Z_bnd(i)-Zgeo,R_bnd(i)-Rgeo)

!    if (i .gt. 1) then
!      if (tht_tmp(i) .lt. tht_tmp(i-1)) tht_tmp(i) = tht_tmp(i) + 2.*PI
!    endif

    fr_tmp(i)  = sqrt((R_bnd(i)-Rgeo)**2 + (Z_bnd(i)-Zgeo)**2)

!    write(*,'(i5,2f12.8)') i,tht_tmp(i),fr_tmp(i)

    Rbnd_av  =  Rbnd_av + R_bnd(i)
    ORbnd_av = ORbnd_av +  1. / R_bnd(i)

  enddo

  if (abs(tht_tmp(n_bnd) - tht_tmp(1)) .lt. 1.e-6)  n_bnd = n_bnd - 1

  Rbnd_av  = Rbnd_av  / float(n_bnd)
  ORbnd_av = ORbnd_av / float(n_bnd)

!  write(*,*)  ' Rbnd_av  : ',Rbnd_av
!  write(*,*)  ' ORbnd_av : ',ORbnd_av

  call qsort2(index_order,n_bnd,tht_tmp)

  do i=1,n_bnd
    tht_sort(i) = tht_tmp(index_order(i))
    fr_sort(i)  = fr_tmp(index_order(i))
!    write(*,*) i,tht_sort(i),fr_sort(i)
  enddo

  n_bnd_short = n_bnd
  do i=2,n_bnd

    if ((tht_sort(i) - tht_sort(1)) .gt. 2.*PI) then
      n_bnd_short = i - 1
      exit
    endif

  enddo

  if (abs(tht_sort(n_bnd_short)- tht_sort(1)) .lt. 1.e-6) then
    tht_sort(n_bnd_short) = tht_sort(1) + 2.*PI
    fr_sort(n_bnd_short)  = fr_sort(1)
  else
    tht_sort(n_bnd_short+1) = tht_sort(1) + 2.*PI
    fr_sort(n_bnd_short+1)  = fr_sort(1)
    n_bnd_short = n_bnd_short + 1
  endif

!  write(*,*) ' n_bnd, n_bnd_short : ',n_bnd, n_bnd_short

  call TB15A(n_bnd_short,tht_sort,fr_sort,dfr_sort,work,6)

  do i = 1, mf

   tht = 2.*PI * float(i-1)/float(mf)

   if (tht .lt. tht_sort(1))          tht = tht + 2.*PI
   if (tht .gt. tht_sort(n_bnd_short)) tht = tht - 2.*PI

   call TG02A(0,n_bnd_short,tht_sort,fr_sort,dfr_sort,tht,values)

   fr(i) = values(1)
  enddo

else

 write(*,*) ' fshape : using moments'

!------------------------------------ THETA(GAMMA(J)) ------------
  do J=1,MF

    GA = 2*PI*(J-1.)/REAL(MF)

    if (GA .le. PI) then
      XJ=         AMIN * COS(GA + TRIA_U*SIN(GA) + QUAD_U*SIN(2*GA))
      YJ= ELLIP * AMIN * SIN(GA)
    else
      XJ=         AMIN * COS(GA + TRIA_L*SIN(GA) + QUAD_L*SIN(2*GA))
      YJ= ELLIP * AMIN * SIN(GA)
    endif
    theta(J) = ATAN2(YJ,XJ)

  enddo

!-------- INVERSION OF THETA(GAMMA(J)) TO GAMMA(THETA(J)) ------  --
  CALL GRID2NV(THETA,GAMMA,MF,error,IGRINV)

  do J=1,MF
    GAMM=GAMMA(J)

    if (GA .le. PI) then
      XV(J) = AMIN * COS(GAMM + TRIA_U*SIN(GAMM) + QUAD_U*SIN(2*GAMM))
      YV(J) = ELLIP * AMIN * SIN(GAMM)
    else
      XV(J) = AMIN * COS(GAMM + TRIA_L*SIN(GAMM) + QUAD_L*SIN(2*GAMM))
      YV(J) = ELLIP * AMIN * SIN(GAMM)
   endif

  enddo

  DO J=1,MF
    FR(J) = SQRT(XV(J)**2 + YV(J)**2)
  enddo

  deallocate(theta, gamma, xv, yv)

endif

!-------------- FOURIER COEFFICIENTS FRFNUL AND FRF(M) OF FR(J).
call rft2(fr,mf,1)

do m=1,mf
  fr(m) = 2. * fr(m) / float(mf)
enddo
do m=2,mf,2
  fr(m) = - fr(m)
enddo
RETURN
END subroutine fshape

subroutine findaxis
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
use helena_grid
use helena_elements
use helena_boundary
use helena_Gauss_4
use helena_axis
use itm_types
implicit none
real (r8)  :: ps_tmp(4,4), psi_min, axis_min
integer :: nodes(4), i, ngr, ngs, kf, kv, vertex, elm_min, ij_axis(2), ifail
real (r8)  :: x(2), r, s, xerr, ferr, rs_tolerance, zpsir, zpsis
real (r8)  :: dRRg1_dr,dRRg1_ds,dRRg1_drs,dRRg1_drr,dRRg1_dss
real (r8)  :: dZZg1_dr,dZZg1_ds,dZZg1_drs,dZZg1_drr,dZZg1_dss
real (r8)  :: dPSg1_dr,dPSg1_ds,dPSg1_drs,dPSg1_drr,dPSg1_dss
real (r8)  :: ejac, dr_dZ, dr_dR, ds_dZ, ds_dR, dPS_dRR, dPS_dZZ
integer :: node1, node2, node3, node4
!real (r8)          :: fdia
logical :: early_exit
parameter (rs_tolerance = 1.d-8)

early_exit = .false.
!---------------------------------- first see if axis is in the same element as before
if ((ielm_ax .ge. 1) .and. (ielm_ax .le. n_elms)) then
  nodes(1) = elm_list(1,ielm_ax)
  nodes(2) = elm_list(2,ielm_ax)
  nodes(3) = elm_list(3,ielm_ax)
  nodes(4) = elm_list(4,ielm_ax)

  call mnewtax(PSI(1:4,nodes(1)),PSI(1:4,nodes(2)),PSI(1:4,nodes(3)),PSI(1:4,nodes(4)),r,s,xerr,ferr,ifail)

  if ((ifail .eq. 0) .and. ((abs(r)-1.) .le. rs_tolerance) .and. ((abs(s)-1.) .le. rs_tolerance)) then

    call INTERP2(PSI(1:4,nodes(1)),PSI(1:4,nodes(2)),PSI(1:4,nodes(3)),PSI(1:4,nodes(4)),r,s,ps_axis,ZPSIR,ZPSIS)
    call INTERP1(RR(1:4,nodes(1)),RR(1:4,nodes(2)),RR(1:4,nodes(3)),RR(1:4,nodes(4)),r,s,R_AXIS)
    call INTERP1(ZZ(1:4,nodes(1)),ZZ(1:4,nodes(2)),ZZ(1:4,nodes(3)),ZZ(1:4,nodes(4)),r,s,Z_AXIS)

    r_ax = r
    s_ax = s

    write(*,*) ' EARLY EXIT !'
    write(*,'(A,3f12.8)') ' magnetic axis : ',R_axis,Z_axis,ps_axis
    write(*,'(A,f12.8)') ' delta (EARLY EXIT) : ',(R_axis-Rgeo)/amin
    early_exit = .true.

  endif
endif

if (.not. early_exit) then

  psi_min = 1.d20

  do i=1,n_elms/2

    nodes(1) = elm_list(1,i)
    nodes(2) = elm_list(2,i)
    nodes(3) = elm_list(3,i)
    nodes(4) = elm_list(4,i)

    ps_tmp = 0.

    do ngr = 1, 4
      do ngs= 1, 4

        do kf = 1, 4       ! 4 basis functions
          do kv = 1, 4     ! 4 vertices

             vertex = nodes(kv)

	     ps_tmp(ngr,ngs) = ps_tmp(ngr,ngs) + psi(kf,vertex) * H(ngr,ngs,kv,kf)

          enddo
        enddo
      enddo
    enddo

    axis_min = minval(ps_tmp)

    if (psi_min .gt. axis_min) then
      psi_min = axis_min
      elm_min = i
      ij_axis = minloc(ps_tmp)
    endif

  enddo

  nodes(1) = elm_list(1,elm_min)
  nodes(2) = elm_list(2,elm_min)
  nodes(3) = elm_list(3,elm_min)
  nodes(4) = elm_list(4,elm_min)

  r=xgauss(ij_axis(1)) ; s=xgauss(ij_axis(2))
  
!  write(*,'(A,e14.6,i6,2e14.6)') 'minimum psi estimate : ',psi_min,elm_min,r,s

  call mnewtax(PSI(1:4,nodes(1)),PSI(1:4,nodes(2)),PSI(1:4,nodes(3)),PSI(1:4,nodes(4)),r,s,xerr,ferr,ifail)

  if (ifail .ne. 0 ) write(*,*) ' MNEWTAX : ifail = ',ifail

  r_ax = r
  s_ax = s
  ielm_ax = elm_min

!  write(*,*) ' find_axis : ',r_ax,s_ax,ielm_ax

endif

node1 = elm_list(1,ielm_ax)
node2 = elm_list(2,ielm_ax)
node3 = elm_list(3,ielm_ax)
node4 = elm_list(4,ielm_ax)

call INTERP(RR(:,nodes(1)),RR(:,nodes(2)),RR(:,nodes(3)),RR(:,nodes(4)),r_ax,s_ax, &
            R_axis,dRRg1_dr,dRRg1_ds,dRRg1_drs,dRRg1_drr,dRRg1_dss)
call INTERP(ZZ(:,nodes(1)),ZZ(:,nodes(2)),ZZ(:,nodes(3)),ZZ(:,nodes(4)),r_ax,s_ax, &
            Z_axis,dZZg1_dr,dZZg1_ds,dZZg1_drs,dZZg1_drr,dZZg1_dss)
call INTERP(PSI(:,nodes(1)),PSI(:,nodes(2)),PSI(:,nodes(3)),PSI(:,nodes(4)),r_ax,s_ax, &
            PS_axis,dPSg1_dr,dPSg1_ds,dPSg1_drs,dPSg1_drr,dPSg1_dss)

ejac  = dRRg1_dr * dZZg1_ds - dRRg1_ds * DZZg1_dr

if (ejac .ne. 0.e0_R8) then
  dr_dZ = - dRRg1_ds / ejac
  dr_dR = + dZZg1_ds / ejac
  ds_dZ = + dRRg1_dr / ejac
  ds_dR = - dZZg1_dr / ejac

  dPS_dRR = dPSg1_drr * dr_dR * dr_dR + 2.e0_R8*dPSg1_drs * dr_dR * ds_dR + dPSg1_dss * ds_dR * ds_dR
  dPS_dZZ = dPSg1_drr * dr_dZ * dr_dZ + 2.e0_R8*dPSg1_drs * dr_dZ * ds_dZ + dPSg1_dss * ds_dZ * ds_dZ

  CRR_axis = dPS_dRR / 2.
  CZZ_axis = dPS_dZZ / 2.

  B_axis = fdia(0.e0_R8) / R_axis
  q_axis = B_axis   /(2.e0_R8*SQRT(CRR_axis*CZZ_axis))
endif

write(*,'(A,4f14.8)') ' magnetic axis : ',R_axis,Z_axis,ps_axis,q_axis

RETURN
END subroutine findaxis

SUBROUTINE mnewtax(ps1, ps2, ps3, ps4, r, s, errx, errf, ifail)
!-------------------------------------------------------------------------
! ROUTINE TO SOLVE TWO NONLINEAR EQUATIONS USING NEWTONS METHOD FROM
! NUMERICAL RECIPES.
! LU DECOMPOSITION REPLACED BY EXPLICIT SOLUTION OF 2X2 MATRIX.
!-------------------------------------------------------------------------
use itm_types
implicit none
REAL (R8)  :: ps1(4), ps2(4), ps3(4), ps4(4)
REAL (R8)  :: r, s, x(2), FVEC(2),FJAC(2,2)
REAL (R8)  :: tolf,tolx, errf, errx
INTEGER :: ntrial, i, k, ifail
REAL (R8)  :: p(2)
real (r8) zpsi,zpsir,zpsis,zpsirs,zpsirr,zpsiss,temp,dis

ntrial = 50
tolx = 1.d-8
tolf = 1.d-16

x(1) = r
x(2) = s

ifail = 999

do k=1,ntrial

  call INTERP(PS1,PS2,PS3,PS4,x(1),x(2),ZPSI,ZPSIR,ZPSIS,ZPSIRS,ZPSIRR,ZPSISS)

!  write(*,'(i3,5e16.8)') k,x(1),x(2),zpsi,zpsir,zpsis

  FVEC(1)   = ZPSIR
  FVEC(2)   = ZPSIS
  FJAC(1,1) = ZPSIRR
  FJAC(1,2) = ZPSIRS
  FJAC(2,1) = ZPSIRS
  FJAC(2,2) = ZPSISS

  errf=abs(fvec(1))+abs(fvec(2))

  if (errf .le. tolf) then
    r = x(1)
    s = x(2)
!    write(*,'(A,2e16.8,i5)') ' newton (1) : ',errf,errx,k
    ifail = 0
    return
  endif
  
  p = -fvec

  temp = p(1)
  dis  = fjac(2,2)*fjac(1,1)-fjac(1,2)*fjac(2,1)
  if (dis .ne. 0.e0_R8) then
    p(1) = (fjac(2,2)*p(1)-fjac(1,2)*p(2))/dis
    p(2) = (fjac(1,1)*p(2)-fjac(2,1)*temp)/dis
  endif

  errx=abs(p(1)) + abs(p(2))

  p = min(p,+0.5e0_R8)
  p = max(p,-0.5e0_R8)

  x = x + p

  x = max(x,-1.e0_R8)
  x = min(x,+1.e0_R8)
    
  if (errx .le. tolx) then
    r = x(1)
    s = x(2)
!    write(*,'(A,2e16.8,i5)') ' newton (2) : ',errf,errx,k
    ifail = 0
    return
  endif

enddo

return
end SUBROUTINE mnewtax

SUBROUTINE INTERP(XN1,XN2,XN3,XN4,R,S,X,XR,XS,XRS,XRR,XSS)
!----------------------------------------------------------------------
! SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
! BY XI(1..4) AT THE FOUR NODES USING BI-CUBIC HERMITE ELEMENTS
!----------------------------------------------------------------------
use itm_types
implicit none
REAL (R8) :: XN1(4),XN2(4),XN3(4),XN4(4)
REAL (R8) :: R,S,X,XR,XS,XRS,XRR,XSS
REAL (R8) :: HI0M,HRI0M,HRRI0M,HI1M,HRI1M,HRRI1M,HJ0M,HSJ0M,HSSJ0M,HJ1M,HSJ1M,HSSJ1M
REAL (R8) :: HI0P,HRI0P,HRRI0P,HI1P,HRI1P,HRRI1P,HJ0P,HSJ0P,HSSJ0P,HJ1P,HSJ1P,HSSJ1P

HI0M   = - (R-1.e0_R8)**2 * (-R-2.e0_R8) * 0.25e0_R8
HRI0M  = - (R-1.e0_R8)*(-R-2.e0_R8)*0.5e0_R8 +(R-1.e0_R8)**2 * 0.25e0_R8
HRRI0M = + 1.5e0_R8 * R
HI1M   = - (R-1.e0_R8)**2 * (-R-1.e0_R8) * 0.25e0_R8
HRI1M  = - (R-1.e0_R8)*(-R-1.e0_R8)*0.5e0_R8 + (R-1.e0_R8)**2 *0.25e0_R8
HRRI1M = + 1.5e0_R8 * R - .5e0_R8

HJ0M   = - (S-1.e0_R8)**2 * (-S-2.e0_R8) * 0.25e0_R8
HSJ0M  = - (S-1.e0_R8)*(-S-2.e0_R8)*0.5 +(S-1.e0_R8)**2 * 0.25e0_R8
HSSJ0M = + 1.5e0_R8 * S
HJ1M   = - (S-1.e0_R8)**2 * (-S-1.e0_R8) * 0.25e0_R8
HSJ1M  = - (S-1.e0_R8)*(-S-1.e0_R8)*0.5e0_R8 + (S-1.e0_R8)**2 * 0.25e0_R8
HSSJ1M = + 1.5e0_R8 * S - .5e0_R8

HI0P   = - (R+1.e0_R8)**2 * (R-2.e0_R8) * 0.25e0_R8
HRI0P  = - (R+1.e0_R8)*(R-2.e0_R8)*0.5e0_R8 - (R+1.e0_R8)**2 * 0.25e0_R8
HRRI0P = - 1.5e0_R8 * R
HI1P   = + (R+1.e0_R8)**2 * (R-1.e0_R8) * 0.25e0_R8
HRI1P  = + (R+1.e0_R8)*(R-1.e0_R8)*0.5e0_R8 + (R+1.e0_R8)**2 * 0.25e0_R8
HRRI1P = + 1.5e0_R8 * R + .5e0_R8

HJ0P  = - (S+1.e0_R8)**2 * (S-2.e0_R8) * 0.25e0_R8
HSJ0P = - (S+1.e0_R8)*(S-2.e0_R8)*0.5e0_R8 - (S+1.e0_R8)**2 * 0.25e0_R8
HSSJ0P = - 1.5e0_R8 * S
HJ1P  = + (S+1.e0_R8)**2 * (S-1.e0_R8) * 0.25e0_R8
HSJ1P = + (S+1.e0_R8)*(S-1.e0_R8)*0.5e0_R8 + (S+1.e0_R8)**2 * 0.25e0_R8
HSSJ1P = + 1.5e0_R8 * S + .5e0_R8

X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2) + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) &
  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2) + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) &
  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2) + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) &
  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2) + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)

XR = HRI0M*HJ0M * XN1(1) + HRI1M*HJ0M * XN1(2) + HRI0M*HJ1M * XN1(3) + HRI1M*HJ1M * XN1(4) &
   + HRI0M*HJ0P * XN2(1) + HRI1M*HJ0P * XN2(2) + HRI0M*HJ1P * XN2(3) + HRI1M*HJ1P * XN2(4) &
   + HRI0P*HJ0M * XN4(1) + HRI1P*HJ0M * XN4(2) + HRI0P*HJ1M * XN4(3) + HRI1P*HJ1M * XN4(4) &
   + HRI0P*HJ0P * XN3(1) + HRI1P*HJ0P * XN3(2) + HRI0P*HJ1P * XN3(3) + HRI1P*HJ1P * XN3(4)

XS = HI0M*HSJ0M * XN1(1) + HI1M*HSJ0M * XN1(2) + HI0M*HSJ1M * XN1(3) + HI1M*HSJ1M * XN1(4) &
   + HI0M*HSJ0P * XN2(1) + HI1M*HSJ0P * XN2(2) + HI0M*HSJ1P * XN2(3) + HI1M*HSJ1P * XN2(4) &
   + HI0P*HSJ0M * XN4(1) + HI1P*HSJ0M * XN4(2) + HI0P*HSJ1M * XN4(3) + HI1P*HSJ1M * XN4(4) &
   + HI0P*HSJ0P * XN3(1) + HI1P*HSJ0P * XN3(2) + HI0P*HSJ1P * XN3(3) + HI1P*HSJ1P * XN3(4)

XRR = HRRI0M*HJ0M * XN1(1) + HRRI1M*HJ0M * XN1(2) + HRRI0M*HJ1M * XN1(3) + HRRI1M*HJ1M * XN1(4) &
    + HRRI0M*HJ0P * XN2(1) + HRRI1M*HJ0P * XN2(2) + HRRI0M*HJ1P * XN2(3) + HRRI1M*HJ1P * XN2(4) &
    + HRRI0P*HJ0M * XN4(1) + HRRI1P*HJ0M * XN4(2) + HRRI0P*HJ1M * XN4(3) + HRRI1P*HJ1M * XN4(4) &
    + HRRI0P*HJ0P * XN3(1) + HRRI1P*HJ0P * XN3(2) + HRRI0P*HJ1P * XN3(3) + HRRI1P*HJ1P * XN3(4)

XSS = HI0M*HSSJ0M * XN1(1) + HI1M*HSSJ0M * XN1(2) + HI0M*HSSJ1M * XN1(3) + HI1M*HSSJ1M * XN1(4) &
    + HI0M*HSSJ0P * XN2(1) + HI1M*HSSJ0P * XN2(2) + HI0M*HSSJ1P * XN2(3) + HI1M*HSSJ1P * XN2(4) &
    + HI0P*HSSJ0M * XN4(1) + HI1P*HSSJ0M * XN4(2) + HI0P*HSSJ1M * XN4(3) + HI1P*HSSJ1M * XN4(4) &
    + HI0P*HSSJ0P * XN3(1) + HI1P*HSSJ0P * XN3(2) + HI0P*HSSJ1P * XN3(3) + HI1P*HSSJ1P * XN3(4)

XRS = HRI0M*HSJ0M * XN1(1) + HRI1M*HSJ0M * XN1(2) + HRI0M*HSJ1M * XN1(3) + HRI1M*HSJ1M * XN1(4) &
    + HRI0M*HSJ0P * XN2(1) + HRI1M*HSJ0P * XN2(2) + HRI0M*HSJ1P * XN2(3) + HRI1M*HSJ1P * XN2(4) &
    + HRI0P*HSJ0M * XN4(1) + HRI1P*HSJ0M * XN4(2) + HRI0P*HSJ1M * XN4(3) + HRI1P*HSJ1M * XN4(4) &
    + HRI0P*HSJ0P * XN3(1) + HRI1P*HSJ0P * XN3(2) + HRI0P*HSJ1P * XN3(3) + HRI1P*HSJ1P * XN3(4)

RETURN
END SUBROUTINE INTERP


SUBROUTINE INTERP1(XN1,XN2,XN3,XN4,R,S,X)
!----------------------------------------------------------------------
! SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
! BY XI(1..4) AT THE FOUR NODES USING BI-CUBIC HERMITE ELEMENTS
!----------------------------------------------------------------------
use itm_types
implicit none
REAL (R8) XN1(4),XN2(4),XN3(4),XN4(4)
real (r8) r,s,x,hi0m,hi1m,hj0m,hj1m,hi0p,hi1p,hj0p,hj1p

HI0M  = - (R-1.)**2 * (-R-2.) * 0.25
HI1M  = - (R-1.)**2 * (-R-1.) * 0.25

HJ0M  = - (S-1.)**2 * (-S-2.) * 0.25
HJ1M  = - (S-1.)**2 * (-S-1.) * 0.25

HI0P  = - (R+1.)**2 * (R-2.)  * 0.25
HI1P  = + (R+1.)**2 * (R-1.)  * 0.25

HJ0P  = - (S+1.)**2 * (S-2.)  * 0.25
HJ1P  = + (S+1.)**2 * (S-1.)  * 0.25

X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2) + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) &
  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2) + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) &
  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2) + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) &
  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2) + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)

RETURN
END SUBROUTINE INTERP1

SUBROUTINE INTERP2(XN1,XN2,XN3,XN4,R,S,X,XR,XS)
!----------------------------------------------------------------------
! SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
! BY XI(1..4) AT THE FOUR NODES USING BI-CUBIC HERMITE ELEMENTS
!----------------------------------------------------------------------
use itm_types
implicit none
REAL (R8) XN1(4),XN2(4),XN3(4),XN4(4)
real (r8) r,s,x,xr,xs,hi0m,hri0m,hi1m,hri1m,hj0m,hsj0m,hj1m,hsj1m,hi0p,hri0p,  &
     hi1p,hri1p,hj0p,hsj0p,hj1p,hsj1p

HI0M  = - (R-1.)**2 * (-R-2.) * 0.25
HRI0M = - (R-1.)*(-R-2.)*0.5 +(R-1.)**2 * 0.25
HI1M  = - (R-1.)**2 * (-R-1.) * 0.25
HRI1M = - (R-1.)*(-R-1.)*0.5 + (R-1.)**2 * 0.25
HJ0M  = - (S-1.)**2 * (-S-2.) * 0.25
HSJ0M = - (S-1.)*(-S-2.)*0.5 +(S-1.)**2  * 0.25
HJ1M  = - (S-1.)**2 * (-S-1.) * 0.25
HSJ1M = - (S-1.)*(-S-1.)*0.5 + (S-1.)**2 * 0.25

HI0P  = - (R+1.)**2 * (R-2.) * 0.25
HRI0P = - (R+1.)*(R-2.)*0.5 - (R+1.)**2 * 0.25
HI1P  = + (R+1.)**2 * (R-1.) * 0.25
HRI1P = + (R+1.)*(R-1.)*0.5 + (R+1.)**2 * 0.25

HJ0P  = - (S+1.)**2 * (S-2.) * 0.25
HSJ0P = - (S+1.)*(S-2.)*0.5 - (S+1.)**2 * 0.25
HJ1P  = + (S+1.)**2 * (S-1.) * 0.25
HSJ1P = + (S+1.)*(S-1.)*0.5 + (S+1.)**2 * 0.25

X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2) + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) &
  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2) + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) &
  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2) + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) &
  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2) + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)

XR = HRI0M*HJ0M * XN1(1) + HRI1M*HJ0M * XN1(2) + HRI0M*HJ1M * XN1(3) + HRI1M*HJ1M * XN1(4) &
   + HRI0M*HJ0P * XN2(1) + HRI1M*HJ0P * XN2(2) + HRI0M*HJ1P * XN2(3) + HRI1M*HJ1P * XN2(4) &
   + HRI0P*HJ0M * XN4(1) + HRI1P*HJ0M * XN4(2) + HRI0P*HJ1M * XN4(3) + HRI1P*HJ1M * XN4(4) &
   + HRI0P*HJ0P * XN3(1) + HRI1P*HJ0P * XN3(2) + HRI0P*HJ1P * XN3(3) + HRI1P*HJ1P * XN3(4)

XS = HI0M*HSJ0M * XN1(1) + HI1M*HSJ0M * XN1(2) + HI0M*HSJ1M * XN1(3) + HI1M*HSJ1M * XN1(4) &
   + HI0M*HSJ0P * XN2(1) + HI1M*HSJ0P * XN2(2) + HI0M*HSJ1P * XN2(3) + HI1M*HSJ1P * XN2(4) &
   + HI0P*HSJ0M * XN4(1) + HI1P*HSJ0M * XN4(2) + HI0P*HSJ1M * XN4(3) + HI1P*HSJ1M * XN4(4) &
   + HI0P*HSJ0P * XN3(1) + HI1P*HSJ0P * XN3(2) + HI0P*HSJ1P * XN3(3) + HI1P*HSJ1P * XN3(4)

RETURN
END SUBROUTINE INTERP2

SUBROUTINE INTERP3(XN1,XN2,XN3,XN4,YN1,YN2,YN3,YN4,PN1,PN2,PN3,PN4,R,S, &
                   X,XR,XS,YR,YS,PS)
!----------------------------------------------------------------------
! SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
! BY XI(1..4) AT THE FOUR NODES USING BI-CUBIC HERMITE ELEMENTS
!----------------------------------------------------------------------
use itm_types
implicit none
REAL (R8) XN1(4),XN2(4),XN3(4),XN4(4)
REAL (R8) YN1(4),YN2(4),YN3(4),YN4(4)
REAL (R8) PN1(4),PN2(4),PN3(4),PN4(4)
real (r8) r,s,x,xr,xs,yr,ys,ps,hi0m,hri0m,hi1m,hri1m,hj0m,hsj0m,  &
     hj1m,hsj1m,hi0p,hri0p,hi1p,hri1p,hj0p,hsj0p,hj1p,hsj1p

HI0M  = - (R-1.)**2 * (-R-2.) * 0.25
HRI0M = - (R-1.)*(-R-2.)*0.5 +(R-1.)**2 * 0.25
HI1M  = - (R-1.)**2 * (-R-1.) * 0.25
HRI1M = - (R-1.)*(-R-1.)*0.5 + (R-1.)**2 * 0.25
HJ0M  = - (S-1.)**2 * (-S-2.) * 0.25
HSJ0M = - (S-1.)*(-S-2.)*0.5 +(S-1.)**2  * 0.25
HJ1M  = - (S-1.)**2 * (-S-1.) * 0.25
HSJ1M = - (S-1.)*(-S-1.)*0.5 + (S-1.)**2 * 0.25

HI0P  = - (R+1.)**2 * (R-2.) * 0.25
HRI0P = - (R+1.)*(R-2.)*0.5 - (R+1.)**2 * 0.25
HI1P  = + (R+1.)**2 * (R-1.) * 0.25
HRI1P = + (R+1.)*(R-1.)*0.5 + (R+1.)**2 * 0.25

HJ0P  = - (S+1.)**2 * (S-2.) * 0.25
HSJ0P = - (S+1.)*(S-2.)*0.5 - (S+1.)**2 * 0.25
HJ1P  = + (S+1.)**2 * (S-1.) * 0.25
HSJ1P = + (S+1.)*(S-1.)*0.5 + (S+1.)**2 * 0.25

X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2) + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) &
  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2) + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) &
  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2) + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) &
  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2) + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)

XR = HRI0M*HJ0M * XN1(1) + HRI1M*HJ0M * XN1(2) + HRI0M*HJ1M * XN1(3) + HRI1M*HJ1M * XN1(4) &
   + HRI0M*HJ0P * XN2(1) + HRI1M*HJ0P * XN2(2) + HRI0M*HJ1P * XN2(3) + HRI1M*HJ1P * XN2(4) &
   + HRI0P*HJ0M * XN4(1) + HRI1P*HJ0M * XN4(2) + HRI0P*HJ1M * XN4(3) + HRI1P*HJ1M * XN4(4) &
   + HRI0P*HJ0P * XN3(1) + HRI1P*HJ0P * XN3(2) + HRI0P*HJ1P * XN3(3) + HRI1P*HJ1P * XN3(4)

XS = HI0M*HSJ0M * XN1(1) + HI1M*HSJ0M * XN1(2) + HI0M*HSJ1M * XN1(3) + HI1M*HSJ1M * XN1(4) &
   + HI0M*HSJ0P * XN2(1) + HI1M*HSJ0P * XN2(2) + HI0M*HSJ1P * XN2(3) + HI1M*HSJ1P * XN2(4) &
   + HI0P*HSJ0M * XN4(1) + HI1P*HSJ0M * XN4(2) + HI0P*HSJ1M * XN4(3) + HI1P*HSJ1M * XN4(4) &
   + HI0P*HSJ0P * XN3(1) + HI1P*HSJ0P * XN3(2) + HI0P*HSJ1P * XN3(3) + HI1P*HSJ1P * XN3(4)

PS = HI0M*HJ0M * PN1(1) + HI1M*HJ0M * PN1(2) + HI0M*HJ1M * PN1(3) + HI1M*HJ1M * PN1(4) &
   + HI0M*HJ0P * PN2(1) + HI1M*HJ0P * PN2(2) + HI0M*HJ1P * PN2(3) + HI1M*HJ1P * PN2(4) &
   + HI0P*HJ0M * PN4(1) + HI1P*HJ0M * PN4(2) + HI0P*HJ1M * PN4(3) + HI1P*HJ1M * PN4(4) &
   + HI0P*HJ0P * PN3(1) + HI1P*HJ0P * PN3(2) + HI0P*HJ1P * PN3(3) + HI1P*HJ1P * PN3(4)

YR = HRI0M*HJ0M * YN1(1) + HRI1M*HJ0M * YN1(2) + HRI0M*HJ1M * YN1(3) + HRI1M*HJ1M * YN1(4) &
   + HRI0M*HJ0P * YN2(1) + HRI1M*HJ0P * YN2(2) + HRI0M*HJ1P * YN2(3) + HRI1M*HJ1P * YN2(4) &
   + HRI0P*HJ0M * YN4(1) + HRI1P*HJ0M * YN4(2) + HRI0P*HJ1M * YN4(3) + HRI1P*HJ1M * YN4(4) &
   + HRI0P*HJ0P * YN3(1) + HRI1P*HJ0P * YN3(2) + HRI0P*HJ1P * YN3(3) + HRI1P*HJ1P * YN3(4)

YS = HI0M*HSJ0M * YN1(1) + HI1M*HSJ0M * YN1(2) + HI0M*HSJ1M * YN1(3) + HI1M*HSJ1M * YN1(4) &
   + HI0M*HSJ0P * YN2(1) + HI1M*HSJ0P * YN2(2) + HI0M*HSJ1P * YN2(3) + HI1M*HSJ1P * YN2(4) &
   + HI0P*HSJ0M * YN4(1) + HI1P*HSJ0M * YN4(2) + HI0P*HSJ1M * YN4(3) + HI1P*HSJ1M * YN4(4) &
   + HI0P*HSJ0P * YN3(1) + HI1P*HSJ0P * YN3(2) + HI0P*HSJ1P * YN3(3) + HI1P*HSJ1P * YN3(4)

RETURN
END SUBROUTINE INTERP3


SUBROUTINE RFT2(DATA,NR,KR)
!*****************************************************************
! REAL FOURIER TRANSFORM.                                        *
! INPUT:  NR REAL COEFFICIENTS                                   *
!             DATA(1),DATA(1+KR),....,DATA(1+(NR-1)*KR).         *
! OUTPUT: NR/2+1 COMPLEX COEFFICIENTS                            *
!            (DATA(1),      DATA(1+KR))                          *
!            (DATA(1+2*KR), DATA(1+3*KR))                        *
!             .............................                      *
!            (DATA(1+NR*KR),DATA(1+(NR+1)*KR).                   *
! THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
! (NR+1)*KR+1 ELEMENTS. (I.E., NR+2 IF INCREMENT KR=1).          *
! LASL ROUTINE MAY 75, CALLING FFT2 AND RTRAN2.                  *
!*****************************************************************
use itm_types
implicit none
real (r8)    :: DATA(*)
integer :: kr,nr, ktran

CALL FFT2(DATA(1),DATA(KR+1),NR/2,-(KR+KR))
CALL RTRAN2(DATA,NR,KR,1)
RETURN
END SUBROUTINE RFT2

SUBROUTINE RTRAN2(DATA,NR,KR,KTRAN)
!*****************************************************************
! INTERFACE BETWEEN RFT2, RFI2, AND FFT2.                        *
! THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
! (NR+1)*KR+1 ELEMENTS.                                          *
! LASL ROUTINE MAY 75, CALLED FROM RFT2 AND RFI2.                *
!*****************************************************************
use itm_types
implicit none
real (r8)    :: data(*), theta, dc, ds, ws, wc, sumr, difr, sumi, difi
real (r8)    :: tr, ti, wca
integer :: nr, kr, ktran, ks, n, nmax, kmax, k, nk

KS=2*KR
N=NR/2
NMAX=N*KS+2
KMAX=NMAX/2
THETA=1.5707963267949/FLOAT(N)
DC=2.*SIN(THETA)**2
DS=SIN(2.*THETA)
WS=0.

IF (KTRAN .LE. 0) THEN
   WC=-1.0
   DS=-DS
ELSE
   WC=1.0
   DATA(NMAX-1)=DATA(1)
   DATA(NMAX-1+KR)=DATA(KR+1)
ENDIF
DO K=1,KMAX,KS
   NK=NMAX-K
   SUMR=.5*(DATA(K)+DATA(NK))
   DIFR=.5*(DATA(K)-DATA(NK))
   SUMI=.5*(DATA(K+KR)+DATA(NK+KR))
   DIFI=.5*(DATA(K+KR)-DATA(NK+KR))
   TR=WC*SUMI-WS*DIFR
   TI=WS*SUMI+WC*DIFR
   DATA(K)=SUMR+TR
   DATA(K+KR)=DIFI-TI
   DATA(NK)=SUMR-TR
   DATA(NK+KR)=-DIFI-TI
   WCA=WC-DC*WC-DS*WS
   WS=WS+DS*WC-DC*WS
   WC=WCA
enddo
return
end SUBROUTINE RTRAN2

SUBROUTINE FFT2 (DATAR,DATAI,N,INC)
!*****************************************************************
! FFT2 FORTRAN VERSION CLAIR NIELSON MAY 75.                     *
!*****************************************************************
use itm_types
implicit none
real (r8)    :: DATAR(*), DATAI(*)
integer :: n, ninc
real (r8) tempr,tempi,theta,sinth,wstpr,wstpi,wr,wi
integer inc,ktran,ks,ip0,ip3,irev,i,ibit,ip1,ip2,i1,i3,j0,j1

KTRAN=ISIGN(-1,INC)
KS=IABS(INC)
IP0=KS
IP3=IP0*N
IREV=1

      DO I=1,IP3,IP0
         IF(I.LT.IREV) THEN
            TEMPR=DATAR(I)
            TEMPI=DATAI(I)
            DATAR(I)=DATAR(IREV)
            DATAI(I)=DATAI(IREV)
            DATAR(IREV)=TEMPR
            DATAI(IREV)=TEMPI
         ENDIF
         IBIT=IP3/2
   10    IF(IREV.GT.IBIT) THEN
            IREV=IREV-IBIT
            IBIT=IBIT/2
            IF(IBIT.GE.IP0) GOTO 10
         ENDIF
         IREV=IREV+IBIT
      enddo
      IP1=IP0
      THETA=REAL(KTRAN)*3.1415926535898
   30 IF(IP1.GE.IP3) return
      IP2=IP1+IP1
      SINTH=SIN(.5*THETA)
      WSTPR=-2.*SINTH*SINTH
      WSTPI=SIN(THETA)
      WR=1.
      WI=0.
      DO I1=1,IP1,IP0
         DO I3=I1,IP3,IP2
            J0=I3
            J1=J0+IP1
            TEMPR=WR*DATAR(J1)-WI*DATAI(J1)
            TEMPI=WR*DATAI(J1)+WI*DATAR(J1)
            DATAR(J1)=DATAR(J0)-TEMPR
            DATAI(J1)=DATAI(J0)-TEMPI
            DATAR(J0)=DATAR(J0)+TEMPR
            DATAI(J0)=DATAI(J0)+TEMPI
         enddo
         TEMPR=WR
         WR=WR*WSTPR-WI*WSTPI+WR
         WI=WI*WSTPR+TEMPR*WSTPI+WI
      enddo
      IP1=IP2
      THETA=.5*THETA
      GOTO 30
RETURN
END SUBROUTINE FFT2


SUBROUTINE FSUM2(F,T,FFNUL,FFCOS,FFSIN,MHARM)
!-----------------------------------------------------------------------
! FOURIER SYNTHESIS OF GENERAL  FUNCTION F(T) AT SINGLE POINT T.
!-----------------------------------------------------------------------
use itm_types
implicit none
integer :: mharm, m
real (r8)    :: ffnul, ffcos(*), ffsin(*), f, t, s, c, co, ca, si, sum

CO=COS(T)
SI=SIN(T)
C=1.
S=0.
SUM=.5*FFNUL
do m=1,mharm
  CA=C*CO-S*SI
  S=S*CO+C*SI
  C=CA
  SUM=SUM+FFCOS(M)*C + FFSIN(M)*S
enddo
F=SUM
RETURN
END SUBROUTINE FSUM2

SUBROUTINE GRID2NV(TIN,TOUT,JPTS,ACC,IGRD)
!------------------------------------------------------------------------
!  THE FUNCTION TIN(TOUT), GIVEN ON THE GRID TOUT=2*PI*(J-1)/JPTS,
!  IS INVERTED TO GIVE TOUT(TIN) ON THE GRID TIN=2*PI*(I-1)/JPTS.
!  THIS IS DONE BY DETERMINING THE ZEROS OF THE FUNCTION
!     Y(T)=T+SUM(GF(M)*SIN(M*T))-2*PI*(I-1)/JPTS,
!  WHERE GF(M) ARE THE FOURIER COEFFICIENTS OF G(T)=TIN(T)-T.
!-----------------------------------------------------------------------
use itm_types
implicit none
integer jmax,ninv
PARAMETER (JMAX=1024,NINV=100)
DIMENSION TIN(*),TOUT(*),T(JMAX+1),G(JMAX+1),GFCOS(JMAX/2-1),GFSIN(JMAX/2-1)
EQUIVALENCE(T(1),G(1))
real (r8) tin,tout,t,g,gfcos,gfsin,acc,pi,t1,sum1,y1,t0,y0,t2,sum2,y2,gfnul
integer jpts,igrd,mharm,jj,i,j1,igrdnv,ifirst,icirc,j,n

PI=2. * asin(1.)
MHARM=JPTS/2-1

DO JJ=2,JPTS
  IF (TIN(JJ-1).GT.TIN(JJ))  TIN(JJ)=TIN(JJ)+2*PI
ENDDO

DO I=1,JPTS
  G(I)=TIN(I)-2.*PI*(I-1.)/JPTS
ENDDO

CALL RFT(G,GFNUL,GFCOS,GFSIN,JPTS,MHARM)

do I=1,JPTS+1
  T(I)=2.*PI*(I-1.)/JPTS
enddo
J1=1
IGRDNV=1
IFIRST=0
ICIRC= - (INT(TIN(1)/(2*PI)+10000) -  9999)
IF (ABS(TIN(1)).LT.1E-12) ICIRC=0

      DO 80 I=1,JPTS
        J=J1
        T1=T(J) + ICIRC*2*PI
        CALL FSUM2(SUM1,T1,GFNUL,GFCOS,GFSIN,MHARM)
        Y1=T1+SUM1-T(I)
   30   CONTINUE
          T0=T1
          Y0=Y1
          IF (ABS(Y0).LE.ACC) THEN
            TOUT(I)=T0
            GOTO 80
          ENDIF
          IF (J.NE.JPTS+1) GOTO 31
            IF (IFIRST.EQ.0)  THEN
              J=1
              ICIRC=ICIRC+1
              IFIRST=1
            ELSE
              GOTO 90
            ENDIF
   31     J=J+1
          T1=T(J) + ICIRC*2*PI
          CALL FSUM2(SUM1,T1,GFNUL,GFCOS,GFSIN,MHARM)
          Y1=T1+SUM1-T(I)
          IF(SIGN(1.0_R8,Y0).EQ.SIGN(1.0_R8,Y1)) GOTO 30
        J1=J-1
        DO 40 N=1,NINV
          T2=T0-(T1-T0)*Y0/(Y1-Y0)
          CALL FSUM2(SUM2,T2,GFNUL,GFCOS,GFSIN,MHARM)
          Y2=T2+SUM2-T(I)
          IF(ABS(Y2).LE.ACC) GOTO 50
          IF(SIGN(1.0_R8,Y2).EQ.SIGN(1.0_R8,Y1)) GOTO 45
          T0=T2
          Y0=Y2
          GOTO 40
   45     T1=T2
          Y1=Y2
   40   CONTINUE
   50   TOUT(I)=T2
        IF(N.GT.IGRDNV) IGRDNV=N
   80 CONTINUE
   90 RETURN

END SUBROUTINE GRID2NV

SUBROUTINE RFT(F,FFNUL,FFCOS,FFSIN,JPTS,MHARM)
!-----------------------------------------------------------------------
!  CALCULATES FOURIER COSINE AND SINE COEFFICIENTS FFCOS AND
!  FFSIN OF THE ARRAY FF CORRESPONDING TO THE  FUNCTION
!  F(T)=.5*FFNUL+SUM(FFCOS(M)*COS(M*T)+FFSIN(M)*SIN(M*T))
!  WHERE MHARM.LE.JPTS/2-1, FFNUL=FF(0) AND T=2*PI*(J-1)/JPTS.
!  THE INPUT ARRAY F(J) IS NOT DESTROYED BY CALLING RFTCOS.
!  TYPICAL USE IS FOR MHARM MUCH SMALLER THAN JPTS/2-1, SO THAT
!  RFT2 CANNOT BE USED DIRECTLY.
!-----------------------------------------------------------------------
use itm_types
implicit none
integer :: jpts, jmax, mharm, j, m
parameter (JMAX=1024)
real (r8)    :: F(*),FFNUL,FFCOS(*),FFSIN(*),FSTORE(JMAX+2)
real (r8)    :: fac

DO J=1,JPTS
  FSTORE(J)=F(J)
enddo
CALL RFT2(FSTORE,JPTS,1)
FAC=2./JPTS
FFNUL=FSTORE(1)*FAC
DO M=1,MHARM
  FFCOS(M)=FSTORE(2*M+1)*FAC
  FFSIN(M) = - FSTORE(2*M+2)*FAC
ENDDO
RETURN
END SUBROUTINE RFT

SUBROUTINE SPLINE(N,X,Y,ALFA,BETA,TYP,A,B,C,D)
!-----------------------------------------------------------------------
!     INPUT:
!
!     N     ANZAHL DER KNOTEN
!     X     ARRAY DER X-WERTE
!     Y     ARRAY DER Y-WERTE
!     ALFA  RANDBEDINGUNG IN X(1)
!     BETA        "       IN X(N)
!     TYP   =  0  NOT-A-KNOT SPLINE
!              1  ALFA, BETA 1. ABLEITUNGEN VORGEGEBEN
!              2    "    "   2.     "           "
!              3    "    "   3.     "           "
!
!     BEMERKUNG: MIT TYP = 2 UND ALFA = BETA = 0 ERHAELT MAN
!           EINEN NATUERLICHEN SPLINE
!
!     OUTPUT:
!
!     A, B, C, D     ARRAYS DER SPLINEKOEFFIZIENTEN
!       S = A(I) + B(I)*(X-X(I)) + C(I)*(X-X(I))**2+ D(I)*(X-X(I))**3
!
!     BEI ANWENDUNGSFEHLERN WIRD DAS PROGRAMM MIT ENTSPRECHENDER
!     FEHLERMELDUNG ABGEBROCHEN
!-----------------------------------------------------------------------
use itm_types
implicit none
      INTEGER  N, TYP
      REAL (R8)     X(N), Y(N), ALFA, BETA, A(N), B(N), C(N), D(N)
      INTEGER  I, IERR
      REAL (R8)     H(1001)


      IF((TYP.LT.0).OR.(TYP.GT.3)) THEN
         WRITE(*,*) ' ERROR IN ROUTINE SPLINE: WRONG TYPE'
         STOP
      ENDIF

      IF (N.LT.3) THEN
         WRITE(*,*) ' ERROR IN ROUTINE  SPLINE: N < 3 '
         STOP
      ENDIF

!     BERECHNE DIFFERENZ AUFEINENDERFOLGENDER X-WERTE UND
!     UNTERSUCHE MONOTONIE

      DO I = 1, N-1
        H(I) = X(I+1)- X(I)
        IF ( H(I).LE. 0.0) THEN
         WRITE(*,*) ' NON-MONOTONIC COORDINATE IN SPLINE: X(I-1)>=X(I)'
         STOP
       ENDIF
      ENDDO

!     AUFSTELLEN DES GLEICHUNGSSYSTEMS

      DO I = 1, N-2
         A(I) = 3.0 * ((Y(I+2)-Y(I+1)) / H(I+1) - (Y(I+1)-Y(I)) / H(I))
         B(I) = H(I)
         C(I) = H(I+1)
         D(I) = 2.0 * (H(I) + H(I+1))
      ENDDO

!     BERUECKSICHTIGEN DER RANDBEDINGUNGEN
!
!     NOT-A-KNOT

      IF (TYP.EQ.0) THEN
         A(1)   = A(1) * H(2) / (H(1) + H(2))
         A(N-2) = A(N-2) * H(N-2) / (H(N-1) + H(N-2))
         D(1)   = D(1) - H(1)
         D(N-2) = D(N-2) - H(N-1)
         C(1)   = C(1) - H(1)
         B(N-2) = B(N-2) - H(N-1)
      ENDIF

!     1. ABLEITUNG VORGEGEBEN

      IF (TYP.EQ.1) THEN
         A(1)   = A(1) - 1.5 * ((Y(2)-Y(1)) / H(1) - ALFA)
         A(N-2) = A(N-2) - 1.5 * (BETA - (Y(N)-Y(N-1)) / H(N-1))
         D(1)   = D(1) - 0.5 * H(1)
         D(N-2) = D(N-2) - 0.5 * H(N-1)
      ENDIF

!     2. ABLEITUNG VORGEGEBEN

      IF (TYP.EQ.2) THEN
         A(1)   = A(1) - 0.5 * ALFA * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)
      ENDIF

!     3. ABLEITUNG VORGEGEBEN

      IF (TYP.EQ.3 ) THEN
         A(1)   = A(1) + 0.5 * ALFA * H(1) * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)* H(N-1)
         D(1)   = D(1) + H(1)
         D(N-2) = D(N-2) + H(N-1)
      ENDIF

!     BERECHNUNG DER KOEFFIZIENTEN

      CALL SGTSL(N-2,B,D,C,A,IERR)

      IF(IERR.NE.0) THEN
         WRITE(*,21)
         STOP
      ENDIF

!     UEBERSCHREIBEN DES LOESUNGSVEKTORS

      CALL DCOPY(N-2,A,1,C(2),1)

!     IN ABHAENGIGKEIT VON DEN RANDBEDINGUNGEN WIRD DER 1. UND
!     DER LETZTE WERT VON C KORRIGIERT

      IF (TYP.EQ.0) THEN
         C(1) = C(2) + H(1) * (C(2)-C(3)) / H(2)
         C(N) = C(N-1) + H(N-1) * (C(N-1)-C(N-2)) / H(N-2)
      ENDIF

      IF (TYP.EQ.1) THEN
         C(1) = 1.5*((Y(2)-Y(1)) / H(1) - ALFA) / H(1) - 0.5 * C(2)
         C(N) = -1.5*((Y(N)-Y(N-1)) / H(N-1)-BETA) / H(N-1)-0.5*C(N-1)
      ENDIF

      IF (TYP.EQ.2) THEN
         C(1) = 0.5 * ALFA
         C(N) = 0.5 * BETA
      ENDIF

      IF (TYP.EQ.3) THEN
         C(1) = C(2) - 0.5 * ALFA * H(1)
         C(N) = C(N-1) + 0.5 * BETA * H(N-1)
      ENDIF

      CALL DCOPY(N,Y,1,A,1)

      DO I = 1, N-1
         B(I) = (A(I+1)-A(I)) / H(I) - H(I) * (C(I+1)+2.0 * C(I)) / 3.0
         D(I) = (C(I+1)-C(I)) / (3.0 * H(I))
      ENDDO

      B(N) = (3.0 * D(N-1) * H(N-1) + 2.0 * C(N-1)) * H(N-1) + B(N-1)

      RETURN

   21 FORMAT(1X,'ERROR IN SGTSL: MATRIX SINGULAR')
    END SUBROUTINE SPLINE



FUNCTION SPWERT(N,XWERT,A,B,C,D,X,ABLTG)
!-----------------------------------------------------------------------
!     INPUT:
!
!     N           ANZAHL DER KNOTENPUNKTE
!     XWERT       STELLE AN DER FUNKTIONSWERTE BERECHNET WERDEN
!     A, B, C, D  ARRAYS DER SPLINEKOEFFIZIENTEN (AUS SPLINE)
!     X           ARRAY DER KNOTENPUNKTE
!
!     OUTPUT:
!
!     SPWERT   FUNKTIONSWERT AN DER STELLE XWERT
!     ABLTG(I) WERT DER I-TEN ABLEITUNG BEI XWERT
!-----------------------------------------------------------------------
use itm_types
implicit none
REAL (R8) SPWERT
      INTEGER  N
      REAL (R8)     XWERT, A(N), B(N), C(N), D(N), X(N), ABLTG(3)
      INTEGER  I, K, M
      real (r8) xx

!     SUCHE PASSENDES INTERVALL (BINAERE SUCHE)

      I = 1
      K = N

   10 M = (I+K) / 2

      IF(M.NE.I) THEN
         IF(XWERT.LT.X(M)) THEN
            K = M
         ELSE
            I = M
         ENDIF
         GOTO 10
      ENDIF

      XX = XWERT - X(I)

      ABLTG(1) = (3.0 * D(I) * XX + 2.0 * C(I)) * XX + B(I)
      ABLTG(2) = 6.0 * D(I) * XX + 2.0 * C(I)
      ABLTG(3) = 6.0 * D(I)

      SPWERT = ((D(I)*XX + C(I))*XX + B(I))*XX + A(I)

      RETURN
    END FUNCTION SPWERT

SUBROUTINE SGTSL(N,C,D,E,B,INFO)
use itm_types
implicit none
INTEGER N,INFO
REAL (R8) C(*),D(*),E(*),B(*)
!
!     SGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND
!     SIDE WILL FIND THE SOLUTION.
!
!     ON ENTRY
!
!        N       INTEGER
!                IS THE ORDER OF THE TRIDIAGONAL MATRIX.
!
!        C       REAL(N)
!                IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.
!                C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.
!                ON OUTPUT C IS DESTROYED.
!
!        D       REAL(N)
!                IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.
!                ON OUTPUT D IS DESTROYED.
!
!        E       REAL(N)
!                IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.
!                E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.
!                ON OUTPUT E IS DESTROYED.
!
!        B       REAL(N)
!                IS THE RIGHT HAND SIDE VECTOR.
!
!     ON RETURN
!
!        B       IS THE SOLUTION VECTOR.
!
!        INFO    INTEGER
!                = 0 NORMAL VALUE.
!                = K IF THE K-TH ELEMENT OF THE DIAGONAL BECOMES
!                    EXACTLY ZERO.  THE SUBROUTINE RETURNS WHEN
!                    THIS IS DETECTED.
!
!     LINPACK. THIS VERSION DATED 08/14/78 .
!     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
!
!     NO EXTERNALS
!     FORTRAN ABS
!
!     INTERNAL VARIABLES
!
      INTEGER K,KB,KP1,NM1,NM2
      REAL (R8) T
!     BEGIN BLOCK PERMITTING ...EXITS TO 100

         INFO = 0
         C(1) = D(1)
         NM1 = N - 1
         IF (NM1 .LT. 1) GO TO 40
            D(1) = E(1)
            E(1) = 0.0E0
            E(N) = 0.0E0

            DO 30 K = 1, NM1
               KP1 = K + 1

!              FIND THE LARGEST OF THE TWO ROWS

               IF (ABS(C(KP1)) .LT. ABS(C(K))) GO TO 10

!                 INTERCHANGE ROW

                  T = C(KP1)
                  C(KP1) = C(K)
                  C(K) = T
                  T = D(KP1)
                  D(KP1) = D(K)
                  D(K) = T
                  T = E(KP1)
                  E(KP1) = E(K)
                  E(K) = T
                  T = B(KP1)
                  B(KP1) = B(K)
                  B(K) = T
   10          CONTINUE

!              ZERO ELEMENTS

               IF (C(K) .NE. 0.0E0) GO TO 20
                  INFO = K
!     ............EXIT
                  GO TO 100
   20          CONTINUE
               T = -C(KP1)/C(K)
               C(KP1) = D(KP1) + T*D(K)
               D(KP1) = E(KP1) + T*E(K)
               E(KP1) = 0.0E0
               B(KP1) = B(KP1) + T*B(K)
   30       CONTINUE
   40    CONTINUE
         IF (C(N) .NE. 0.0E0) GO TO 50
            INFO = N
         GO TO 90
   50    CONTINUE

!           BACK SOLVE

            NM2 = N - 2
            B(N) = B(N)/C(N)
            IF (N .EQ. 1) GO TO 80
               B(NM1) = (B(NM1) - D(NM1)*B(N))/C(NM1)
               IF (NM2 .LT. 1) GO TO 70
               DO 60 KB = 1, NM2
                  K = NM2 - KB + 1
                  B(K) = (B(K) - D(K)*B(K+1) - E(K)*B(K+2))/C(K)
   60          CONTINUE
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE

RETURN
END SUBROUTINE SGTSL

SUBROUTINE TB15A(N,X,F,D,W,LP)
!------------------------------------------------------------------
! HSL routine for cubic spline with periodic boundary conditions
! first point must be the same as last : f(1)=f(n)
!    N : number of points
!    X : coordinate (input)
!    F : the function values to be splined (input)
!    D : the derivatives at the points (output)
!    W : workspace (dimension 3N)
!   LP : unit number for output
!------------------------------------------------------------------
use itm_types
implicit none
REAL (R8) ZERO,ONE,TWO,THREE
PARAMETER (ZERO=0.0E0,ONE=1.0E0,TWO=2.0E0,THREE=3.0E0)
INTEGER LP,N
REAL (R8) D(N),F(N),W(*),X(N)
REAL (R8) A3N1,F1,F2,H1,H2,P
INTEGER I,J,K,N2

WRITE(*,*) F(1),F(N)
IF (N.LT.4) THEN
  WRITE (LP,'(A39)')  'RETURN FROM TB15AD BECAUSE N TOO SMALL'
  W(1) = ONE
  RETURN
END IF
DO I = 2,N
  IF (X(I).LE.X(I-1)) THEN
    WRITE (LP,'(A29,I3,A13)') ' RETURN FROM TB15AD BECAUSE  ',I,' OUT OF ORDER'
    write(*,*) X
    W(1) = TWO
    RETURN
  END IF
ENDDO
IF (F(1).NE.F(N)) THEN
  WRITE (LP,'(A40)')  'RETURN FROM TB15AD BECAUSE F(1).NE.F(N)'
  W(1) = THREE
  RETURN
END IF
DO I = 2,N
  H1 = ONE/ (X(I)-X(I-1))
  F1 = F(I-1)
  IF (I.EQ.N) THEN
    H2 = ONE/ (X(2)-X(1))
    F2 = F(2)
  ELSE
    H2 = ONE/ (X(I+1)-X(I))
    F2 = F(I+1)
  END IF
  W(3*I-2) = H1
  W(3*I-1) = TWO* (H1+H2)
  W(3*I) = H2
  D(I) = 3.0* (F2*H2*H2+F(I)* (H1*H1-H2*H2)-F1*H1*H1)
ENDDO
N2 = N - 2
K = 5
A3N1 = W(3*N-1)
DO I = 2,N2
  P = W(K+2)/W(K)
  W(K+3) = W(K+3) - P*W(K+1)
  D(I+1) = D(I+1) - P*D(I)
  W(K+2) = -P*W(K-1)
  P = W(K-1)/W(K)
  A3N1 = -P*W(K-1) + A3N1
  D(N) = D(N) - P*D(I)
  K = K + 3
ENDDO
P = (W(K+2)+W(K-1))/W(K)
A3N1 = A3N1 - P* (W(K+1)+W(K-1))
D(N) = (D(N)-P*D(N-1))/A3N1
DO I = 3,N
  J = N + 2 - I
  D(J) = (D(J)-W(3*J)*D(J+1)-W(3*J-2)*D(N))/W(3*J-1)
ENDDO
D(1) = D(N)
W(1) = ZERO
RETURN
END SUBROUTINE TB15A



SUBROUTINE TG02A(IX,N,U,S,D,X,V)
!------------------------------------------------------------------
! HSL subroutine to calculate splined values
!    N  : number of points
!    IX : negative 0 -> no initial guess for where xi is
!        positive -> gues for index close to value X
!   U   : the coordinates of the spline points
!   S   : the function values of the spline points
!   D   : the derivatives on the spline points
!   X   : the coordinate where the output is wanted
!   V(1-4) : value and derivatives of the spline interpolation
!------------------------------------------------------------------
use itm_types
implicit none
REAL (R8) X
INTEGER IX,N
REAL (R8) D(*),S(*),U(*),V(*)
REAL (R8) A,B,C,C3,E0_R8,D1,EPS,GAMA,H,HR,HRR,PHI,S0,S1,T,THETA
INTEGER I,IFLG,J,K

EPS = 1.E-33
K = 0
IFLG = 0
IF (X.LT.U(1)) GO TO 990
IF (X.GT.U(N)) GO TO 991
IF (IX.LT.0 .OR. IFLG.EQ.0) GO TO 12
IF (X.GT.U(J+1)) GO TO 1
IF (X.GE.U(J)) GO TO 18
GO TO 2

    1 J = J + 1
   11 IF (X.GT.U(J+1)) GO TO 1
      GO TO 7
   12 J = ABS(X-U(1))/ (U(N)-U(1))* (N-1) + 1
      J = MIN(J,N-1)
      IFLG = 1
      IF (X.GE.U(J)) GO TO 11
    2 J = J - 1
      IF (X.LT.U(J)) GO TO 2
    7 K = J
      H = U(J+1) - U(J)
      HR = 1./H
      HRR = (HR+HR)*HR
      S0 = S(J)
      S1 = S(J+1)
      E0_R8 = D(J)
      D1 = D(J+1)
      A = S1 - S0
      B = A - H*D1
      A = A - H*E0_R8
      C = A + B
      C3 = C*3.
   18 THETA = (X-U(J))*HR
      PHI = 1. - THETA
      T = THETA*PHI
      GAMA = THETA*B - PHI*A
      V(1) = THETA*S1 + PHI*S0 + T*GAMA
      V(2) = THETA*D1 + PHI*E0_R8 + T*C3*HR
      V(3) = (C* (PHI-THETA)-GAMA)*HRR
      V(4) = -C3*HRR*HR
      RETURN
  990 IF (X.LE.U(1)-EPS*MAX(ABS(U(1)),ABS(U(N)))) GO TO 99
      J = 1
      GO TO 7
  991 IF (X.GE.U(N)+EPS*MAX(ABS(U(1)),ABS(U(N)))) GO TO 995
      J = N - 1
      GO TO 7
  995 K = N
   99 IFLG = 0
      DO I = 1,4
        V(I) = 0.
      ENDDO
RETURN
END SUBROUTINE TG02A

SUBROUTINE QSORT2 (ORD,N,A)
use itm_types
implicit none
!
!==============SORTS THE ARRAY A(I),I=1,2,...,N BY PUTTING THE
!   ASCENDING ORDER VECTOR IN ORD.  THAT IS ASCENDING ORDERED A
!   IS A(ORD(I)),I=1,2,...,N; DESCENDING ORDER A IS A(ORD(N-I+1)),
!   I=1,2,...,N .  THIS SORT RUNS IN TIME PROPORTIONAL TO N LOG N .
!
!
!     ACM QUICKSORT - ALGORITHM #402 - IMPLEMENTED IN FORTRAN BY
!                                 WILLIAM H. VERITY
!                                 COMPUTATION CENTER
!                                 PENNSYLVANIA STATE UNIVERSITY
!                                 UNIVERSITY PARK, PA.  16802
!     With correction to that algorithm.
!
!
      DIMENSION ORD(N),POPLST(2,20)
      integer ORD,POPLST
!
!     To sort different input types change the following
!     specification statements; FOR EXAMPLE,  REAL A(N) or
!     CHARACTER *(L) A(N)  for REAL or CHARACTER sorting
!     respectively  similarly for X,XX,Z,ZZ,Y. L is the
!     character length of the elements of A.
!
      REAL (R8) A(N)
      REAL (R8) X,XX,Z,ZZ,Y
      integer n,ndeep,u1,l1,i,l,u,p,q,yp,ix,iz,ip,iq
!
      NDEEP=0
      U1=N
      L1=1
      DO 1  I=1,N
    1 ORD(I)=I
    2 IF (U1.GT.L1) GO TO 3
      RETURN
!
    3 L=L1
      U=U1
!
! PART
!
    4 P=L
      Q=U
      X=A(ORD(P))
      Z=A(ORD(Q))
      IF (X.LE.Z) GO TO 5
      Y=X
      X=Z
      Z=Y
      YP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=YP
    5 IF (U-L.LE.1) GO TO 15
      XX=X
      IX=P
      ZZ=Z
      IZ=Q
!
! LEFT
!
    6 P=P+1
      IF (P.GE.Q) GO TO 7
      X=A(ORD(P))
      IF (X.GE.XX) GO TO 8
      GO TO 6
    7 P=Q-1
      GO TO 13
!
! RIGHT
!
    8 Q=Q-1
      IF (Q.LE.P) GO TO 9
      Z=A(ORD(Q))
      IF (Z.LE.ZZ) GO TO 10
      GO TO 8
    9 Q=P
      P=P-1
      Z=X
      X=A(ORD(P))
!
! DIST
!
   10 IF (X.LE.Z) GO TO 11
      Y=X
      X=Z
      Z=Y
      IP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=IP
   11 IF (X.LE.XX) GO TO 12
      XX=X
      IX=P
   12 IF (Z.GE.ZZ) GO TO 6
      ZZ=Z
      IZ=Q
      GO TO 6
!
! OUT
!
   13 CONTINUE
      IF (.NOT.(P.NE.IX.AND.X.NE.XX)) GO TO 14
      IP=ORD(P)
      ORD(P)=ORD(IX)
      ORD(IX)=IP
   14 CONTINUE
      IF (.NOT.(Q.NE.IZ.AND.Z.NE.ZZ)) GO TO 15
      IQ=ORD(Q)
      ORD(Q)=ORD(IZ)
      ORD(IZ)=IQ
   15 CONTINUE
      IF (U-Q.LE.P-L) GO TO 16
      L1=L
      U1=P-1
      L=Q+1
      GO TO 17
   16 U1=U
      L1=Q+1
      U=P-1
   17 CONTINUE
      IF (U1.LE.L1) GO TO 18
!
! START RECURSIVE CALL
!
      NDEEP=NDEEP+1
      POPLST(1,NDEEP)=U
      POPLST(2,NDEEP)=L
      GO TO 3
   18 IF (U.GT.L) GO TO 4
!
! POP BACK UP IN THE RECURSION LIST
!
      IF (NDEEP.EQ.0) GO TO 2
      U=POPLST(1,NDEEP)
      L=POPLST(2,NDEEP)
      NDEEP=NDEEP-1
      GO TO 18
!
! END QSORT
END SUBROUTINE QSORT2

end module helena21_mod
