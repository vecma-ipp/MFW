!> Module implementing a simple equilibrium using CPOs
!>
!> \author Bruce Scott
!>
!> \version "$Id: bdseq.f90 $"
SUBROUTINE Bdseq (eq_in, eq_out, code_parameters)

!...  simple 1D equilibrium module, a set of integrals
!...  eq_in is set up by top level subroutine plasma_equil
!...  for now the input xml files are read and processed herein

!...  on output all the 1d profiles are filled plus 
!...  the minimal 2d profiles needed by a fluxtube turbulence code
!...  pi and tpi and mu_0 and the ITM_I4/R8 are defined in 
!...       Phys_Constants which calls ITM_Types

#ifdef MPI
  USE MPEs
#endif
  USE Phys_Constants
  USE Euitm_Schemas
  USE BDSEQ_Coeff
  USE write_structures

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq_in(:), eq_out(:)
  TYPE (type_param) :: code_parameters

  INTEGER(ITM_I4) :: npsi_eq
  INTEGER(ITM_I4) :: iter
  INTEGER(ITM_I4) :: i,j,ip,im
  REAL(R8) :: a00,b00,r00,z00
  REAL(R8) :: hra,heta,rho,dprime,det,coseta,sineta,vva
  REAL(R8) :: dpsi,pvol,parea,barea,iplasma

  REAL(R8), DIMENSION(:), POINTER :: &
       ra, eta, rtor, rvol, pressure, jtor, jpar, ii, &
       psi, phi, qq, volume, surface, area, vprime, aprime, ftrap
  REAL(R8), DIMENSION(:), POINTER :: &
       ffprime, pprime, dpsidr
  REAL(R8), DIMENSION(:), POINTER :: &
       gm1, gm2, gm3, gm4, gm5, gm6, gm7, gm8, gm9
  REAL(R8), DIMENSION(:,:), POINTER :: rr, zz, &
       g_11, g_12, g_22, g_13, g_23, g_33, bb2
  REAL(R8), DIMENSION(:), POINTER :: dtdr,dtdn
  REAL(R8), DIMENSION(:,:), POINTER :: theta,dtheta

!...  XML declarations

  integer(ITM_I4) :: return_status

  character(len = 132), target :: codename(1) = 'BDSEQ'
  character(len = 132), target :: codeversion(1) = '7 Apr 2011'

!...  if running MPI you need these

#ifdef MPI
  INTEGER :: mype,npes
  CALL MPI_Comm_size( MPI_COMM_WORLD, npes, ierr )
  CALL MPI_Comm_rank( MPI_COMM_WORLD, mype, ierr )
#endif

!...  allocations

  IF (.NOT. ASSOCIATED(eq_out)) THEN

!...  assign parms

  allocate(eq_out(1))
  allocate(eq_out(1)%codeparam%codename(1))
  allocate(eq_out(1)%codeparam%codeversion(1))
  if (.not. associated(code_parameters%parameters)) then
    write(*,*) 'ERROR: BDSEQ parameters not associated!'
    stop
  else
    allocate(eq_out(1)%codeparam%parameters(size( &
     code_parameters%parameters)))
  end if

!.  write(*,*) 'BDSEQ Parameters : ', code_parameters%parameters

!-- add to eq_out
  eq_out(1)%codeparam%codename = codename
  eq_out(1)%codeparam%codeversion = codeversion
  eq_out(1)%codeparam%parameters = code_parameters%parameters

!-- assign code parameters to internal variables
  call assign_equil_parameters(code_parameters, return_status)

  if (return_status /= 0) then
    write(*,*) 'ERROR: Could not assign BDSEQ parameters.'
    return
  end if

!.  write(*,*) 'done assigning BDSEQ parameters'

!...  write out input cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'EqIn' )
  call write_cpo(eq_in(1), 'EqIn' )
  call close_write_file

  END IF

!...  set grid size for equilibrium calculation and output
!...  idiot protection

  IF (nr_eq == 0) nr_eq = 65
  IF (neta_eq == 0) neta_eq = 256

!...  do all the calculations on the output structure
!...  fill all the 1d profiles

  allocate(eq_out(1)%profiles_1d%psi(nr_eq))
  allocate(eq_out(1)%profiles_1d%phi(nr_eq))
  allocate(eq_out(1)%profiles_1d%pressure(nr_eq))
  allocate(eq_out(1)%profiles_1d%F_dia(nr_eq))
  allocate(eq_out(1)%profiles_1d%pprime(nr_eq))
  allocate(eq_out(1)%profiles_1d%ffprime(nr_eq))
  allocate(eq_out(1)%profiles_1d%jphi(nr_eq))
  allocate(eq_out(1)%profiles_1d%jparallel(nr_eq))
  allocate(eq_out(1)%profiles_1d%q(nr_eq))
  allocate(eq_out(1)%profiles_1d%r_inboard(nr_eq))
  allocate(eq_out(1)%profiles_1d%r_outboard(nr_eq))
  allocate(eq_out(1)%profiles_1d%rho_vol(nr_eq))
  allocate(eq_out(1)%profiles_1d%rho_tor(nr_eq))
  allocate(eq_out(1)%profiles_1d%elongation(nr_eq))
  allocate(eq_out(1)%profiles_1d%tria_upper(nr_eq))
  allocate(eq_out(1)%profiles_1d%tria_lower(nr_eq))
  allocate(eq_out(1)%profiles_1d%volume(nr_eq))
  allocate(eq_out(1)%profiles_1d%surface(nr_eq))
  allocate(eq_out(1)%profiles_1d%area(nr_eq))
  allocate(eq_out(1)%profiles_1d%vprime(nr_eq))
  allocate(eq_out(1)%profiles_1d%aprime(nr_eq))
  allocate(eq_out(1)%profiles_1d%dpsidrho_tor(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm1(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm2(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm3(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm4(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm5(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm6(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm7(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm8(nr_eq))
  allocate(eq_out(1)%profiles_1d%gm9(nr_eq))
  allocate(eq_out(1)%profiles_1d%ftrap(nr_eq))
  allocate(eq_out(1)%profiles_1d%beta_pol(nr_eq))
  allocate(eq_out(1)%profiles_1d%li(nr_eq))
  allocate(eq_out(1)%profiles_1d%b_av(nr_eq))
  allocate(eq_out(1)%profiles_1d%b_min(nr_eq))
  allocate(eq_out(1)%profiles_1d%b_max(nr_eq))
  allocate(eq_out(1)%profiles_1d%omega(nr_eq))
  allocate(eq_out(1)%profiles_1d%omegaprime(nr_eq))
  allocate(eq_out(1)%profiles_1d%mach_a(nr_eq))
  allocate(eq_out(1)%profiles_1d%phi_flow(nr_eq))
  allocate(eq_out(1)%profiles_1d%s_flow(nr_eq))
  allocate(eq_out(1)%profiles_1d%h_flow(nr_eq))

  allocate(eq_out(1)%eqgeometry%boundary(1))
  allocate(eq_out(1)%eqgeometry%boundary(1)%r(neta_eq))
  allocate(eq_out(1)%eqgeometry%boundary(1)%z(neta_eq))

  allocate(eq_out(1)%coord_sys%position%r(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%position%z(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%g_11(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%g_12(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%g_22(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%g_13(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%g_23(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%g_33(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%jacobian(nr_eq, neta_eq))
  allocate(eq_out(1)%coord_sys%grid%dim1(nr_eq))
  allocate(eq_out(1)%coord_sys%grid%dim2(neta_eq))
  allocate(eq_out(1)%coord_sys%grid_type(2))

  IF (symmetry_coords) THEN
     eq_out(1)%coord_sys%grid_type(1) = "symmetry coordinates"
     eq_out(1)%coord_sys%grid_type(2) = "same as straight field line coords"
  ELSE
     eq_out(1)%coord_sys%grid_type(1) = "shifted circle coordinates"
     eq_out(1)%coord_sys%grid_type(2) = &
          "output in these coordinates NOT in straight field line coords"
  END IF

!...  done initialisation

  END IF

!...  this is a zero-epsilon model, no distinction between jtor and jpar
!...  jtor is called jphi in this CPO and jpar is called jparallel

!...  find grid size for equilibrium input

  npsi_eq=SIZE(eq_in(1)%profiles_1d%rho_tor)

!...  get the global parameters
!...  the convention on a00 is that it is the maximum value of
!...      rho_tor not the reference machine minor radius!
!...  for the purposes of this code, a00 is in fact whatever you loaded
!...      into eq_in a_minor

  a00=eq_in(1)%eqgeometry%a_minor
  b00=eq_in(1)%global_param%toroid_field%b0
  r00=eq_in(1)%global_param%toroid_field%r0
  IF (itm_is_valid(eq_in(1)%eqgeometry%geom_axis%z)) THEN
     z00=eq_in(1)%eqgeometry%geom_axis%z
  ELSE
     z00 = 0.0_R8
  END IF

  eq_out(1)%eqgeometry%a_minor=a00
  eq_out(1)%global_param%toroid_field%b0=b00
  eq_out(1)%global_param%toroid_field%r0=r00
  eq_out(1)%eqgeometry%geom_axis%r=r00
  eq_out(1)%eqgeometry%geom_axis%z=z00

  eq_out(1)%eqgeometry%elongation=1._R8
  eq_out(1)%eqgeometry%tria_upper=0._R8
  eq_out(1)%eqgeometry%tria_lower=0._R8

!...  associations

  psi => eq_out(1)%profiles_1d%psi
  phi => eq_out(1)%profiles_1d%phi
  pressure => eq_out(1)%profiles_1d%pressure
  ii => eq_out(1)%profiles_1d%F_dia
  pprime => eq_out(1)%profiles_1d%pprime
  ffprime => eq_out(1)%profiles_1d%ffprime
  jtor => eq_out(1)%profiles_1d%jphi
  jpar => eq_out(1)%profiles_1d%jparallel
  qq => eq_out(1)%profiles_1d%q

  ra => eq_out(1)%coord_sys%grid%dim1
  eta => eq_out(1)%coord_sys%grid%dim2

  rvol => eq_out(1)%profiles_1d%rho_vol
  rtor => eq_out(1)%profiles_1d%rho_tor
  volume => eq_out(1)%profiles_1d%volume
  surface => eq_out(1)%profiles_1d%surface
  area => eq_out(1)%profiles_1d%area
  vprime => eq_out(1)%profiles_1d%vprime
  aprime => eq_out(1)%profiles_1d%aprime
  ftrap => eq_out(1)%profiles_1d%ftrap

  gm1 => eq_out(1)%profiles_1d%gm1
  gm2 => eq_out(1)%profiles_1d%gm2
  gm3 => eq_out(1)%profiles_1d%gm3
  gm4 => eq_out(1)%profiles_1d%gm4
  gm5 => eq_out(1)%profiles_1d%gm5
  gm6 => eq_out(1)%profiles_1d%gm6
  gm7 => eq_out(1)%profiles_1d%gm7
  gm8 => eq_out(1)%profiles_1d%gm8
  gm9 => eq_out(1)%profiles_1d%gm9

  rr => eq_out(1)%coord_sys%position%r
  zz => eq_out(1)%coord_sys%position%z
  g_11 => eq_out(1)%coord_sys%g_11
  g_12 => eq_out(1)%coord_sys%g_12
  g_22 => eq_out(1)%coord_sys%g_22
  g_13 => eq_out(1)%coord_sys%g_13
  g_23 => eq_out(1)%coord_sys%g_23
  g_33 => eq_out(1)%coord_sys%g_33

  dpsidr => eq_out(1)%profiles_1d%dpsidrho_tor
  bb2 => eq_out(1)%coord_sys%g_23

!...  on input, rho_tor, p and J
!...  equidistant grid in geometric r which is rho_vol 
!...  here rho_vol and rho_tor_norm are assumed to be equivalent
!...       consistent with I = R_0 B_0

  hra=1.0_R8/(nr_eq-1)
  ra = (/ (hra*(i-1),i=1,nr_eq) /)

  heta=tpi/(neta_eq)
  eta = (/ (heta*(i-1),i=1,neta_eq) /)

  rvol = ra
  rtor = ra*a00

  hra=a00/(nr_eq-1)
  ra => rtor

  volume=tpi*pi*ra*ra*r00
  vva=tpi*pi*r00*a00*a00

!...  check profiles

  IF (write_diags) THEN

  WRITE (0,*) 'input'
  WRITE (0,*) 'rho_tor  pressure   current'
  DO i=1,npsi_eq
     WRITE (0,'(4g11.3)') &
          eq_in(1)%profiles_1d%rho_tor(i), &
          eq_in(1)%profiles_1d%pressure(i), &
          eq_in(1)%profiles_1d%jphi(i)
  END DO

  END IF

!...  interpolate p and J onto the final grid
!...  use jpar and jtor interchangeably

  CALL L3interp( eq_in(1)%profiles_1d%pressure, &
       eq_in(1)%profiles_1d%rho_tor, &
       npsi_eq, &
       eq_out(1)%profiles_1d%pressure, &
       eq_out(1)%profiles_1d%rho_tor, &
       nr_eq )

  CALL L3interp( eq_in(1)%profiles_1d%jphi, &
       eq_in(1)%profiles_1d%rho_tor, &
       npsi_eq, &
       eq_out(1)%profiles_1d%jphi, &
       eq_out(1)%profiles_1d%rho_tor, &
       nr_eq )

  jpar=jtor

  IF (write_diags) THEN

  WRITE (0,*) 'output'
  WRITE (0,*) 'ra  rho_tor  pressure   current'
  DO i=1,nr_eq
     WRITE (0,'(4g11.3)') eq_out(1)%profiles_1d%rho_vol(i), &
          eq_out(1)%profiles_1d%rho_tor(i), &
          eq_out(1)%profiles_1d%pressure(i), &
          eq_out(1)%profiles_1d%jphi(i)
  END DO

  END IF

!...  do the integrals using the gm? as scratch space

!...  find the pitch parameter from the current profile

  gm1= - (mu_0*r00/(2._R8*b00))*jtor

  qq(1)=0._R8
  DO i=1,nr_eq-1
     qq(i+1)=qq(i)+0.5*(gm1(i)+gm1(i+1))*(volume(i+1)-volume(i))
  END DO
  qq(1)=1._R8
  qq=volume/qq
  qq(1) = - 2._R8*b00/(mu_0*r00*jtor(1))
  
!...  find the toroidal flux

  phi=(pi*b00*a00*a00/vva)*volume

!...  find the poloidal flux

  psi(1)=0._R8
  gm1=1._R8/qq
  DO i=1,nr_eq-1
     psi(i+1)=psi(i)+0.5*(gm1(i)+gm1(i+1))*(phi(i+1)-phi(i))
  END DO

!...  dpsidr is now used for dPsi/drho which normalises the metric

  CALL L3deriv(psi,ra,nr_eq,dpsidr,ra,nr_eq)
  dpsidr(1)=0._R8

!...  find the Grad-Shafranov equation quantities
!...  hold dpdrho in gm5

  CALL L3deriv(pressure,ra,nr_eq,gm5,ra,nr_eq)
  pprime=gm5/(dpsidr+1.e-30_R8)
  pprime(1)=(pressure(2)-pressure(1))/(psi(2)-psi(1))

!  CALL L3deriv(pressure,psi,nr_eq,pprime,psi,nr_eq)

  ffprime = mu_0*r00*jtor/tpi - mu_0*r00*r00*pprime

!...  set the diamagnetic compression

  ii=(r00*b00)*(r00*b00)/2.0_R8
  DO i=nr_eq-1,1,-1
     ii(i)=ii(i+1) - 0.5*(ffprime(i)+ffprime(i+1))*(psi(i+1)-psi(i))
  END DO

  ii=SQRT(2.0_R8*ii)*SIGN(1.0_R8,b00)

!...  flux surface shape and shifts
!...  here use gm1 for delta
!...          gm2 for delta prime
!...          gm3 for integrand for delta prime
!...          gm4 for r^2/q^2
!...          gm5 for dp/dr

  gm1=(ra*ra/(2._R8*r00))
  gm4=(ra/qq)**2.
  gm3=(2._R8*mu_0*r00*r00/(b00*b00))*ra*gm5 - gm4
  gm2(1)=0._R8
  DO i=1,nr_eq-1
     gm2(i+1)=gm2(i)+0.5*(gm1(i+1)-gm1(i))*(gm3(i+1)+gm3(i))
  END DO
  gm4=gm4*ra
  gm4(1)=1._R8
  gm2=gm2/gm4
  gm2(1)=0._R8

  gm1(nr_eq)=0._R8
  DO i=nr_eq-1,1,-1
     gm1(i)=gm1(i+1)+0.5*(ra(i)-ra(i+1))*(gm2(i+1)+gm2(i))
  END DO

  eq_out(1)%profiles_1d%r_inboard=r00-ra + gm1
  eq_out(1)%profiles_1d%r_outboard=r00+ra + gm1
  eq_out(1)%profiles_1d%elongation=1._R8
  eq_out(1)%profiles_1d%tria_upper=0._R8
  eq_out(1)%profiles_1d%tria_lower=0._R8

!...  set axis parameters here since we still have delta in gm1

  eq_out(1)%global_param%mag_axis%position%r = r00 + gm1(1)
  eq_out(1)%global_param%mag_axis%position%z = z00
  eq_out(1)%global_param%mag_axis%q = qq(1)
  eq_out(1)%global_param%mag_axis%bphi = ii(1)/(r00 + gm1(1))

!...  find the quantities needed by the other modules 
!...  eg these are the quantities GEM needs from HELENA
!...  gm1 still contains the shifts
!...  gm2 still contains the derivatives of the shifts

  DO j=1,neta_eq
     sineta=SIN(eta(j))
     coseta=COS(eta(j))

     rr(:,j)=r00+ra*coseta + gm1
     zz(:,j)=z00+ra*sineta

     eq_out(1)%eqgeometry%boundary(1)%r(j)=r00+a00*coseta
     eq_out(1)%eqgeometry%boundary(1)%z(j)=z00+a00*sineta

     DO i=1,nr_eq
        rho=ra(i)
        dprime=gm2(i)

        det=(1.+dprime*coseta)**(-2.0)

        g_11(i,j)=det
        g_12(i,j)=det*dprime*sineta
        g_22(i,j)=det*(1.+2.*dprime*coseta+dprime*dprime)

        g_33(i,j)=rr(i,j)*(1.+dprime*coseta)

     END DO

     g_12(:,j)=g_12(:,j)/ra
     g_22(:,j)=g_22(:,j)/(ra*ra)

  END DO

  IF (symmetry_coords) THEN
     dtheta => g_13
     theta => g_23

     g_33=g_33/(rr*rr)
     gm9=1./SUM(g_33, dim=2)
     DO j=1,neta_eq
        dtheta(:,j)=tpi*g_33(:,j)*gm9
     END DO
     theta(:,1)=0.0_R8
     DO j=2,neta_eq
        theta(:,j)=theta(:,j-1)+(dtheta(:,j)+dtheta(:,j-1))/2.
     END DO

     ALLOCATE(dtdn(neta_eq))
     ALLOCATE(dtdr(neta_eq))

     DO i=1,nr_eq
        ip=i+1
        im=i-1
        IF (i == 1) im=i
        IF (i == nr_eq) ip=i

        dtdn=dtheta(i,:)/heta
        dtdr=(theta(ip,:)-theta(im,:))/(ra(ip)-ra(im))

        g_22(i,:)=(dtdr*dtdr*g_11(i,:)+2.0_R8*dtdr*dtdn*g_12(i,:) &
             +dtdn*dtdn*g_22(i,:))
        g_12(i,:)=(dtdr*g_11(i,:)+dtdn*g_12(i,:))

        CALL L3interp2( g_11(i,:), theta(i,:), eta, neta_eq )
        CALL L3interp2( g_12(i,:), theta(i,:), eta, neta_eq )
        CALL L3interp2( g_22(i,:), theta(i,:), eta, neta_eq )
        CALL L3interp2( g_33(i,:), theta(i,:), eta, neta_eq )
        CALL L3interp2( rr(i,:), theta(i,:), eta, neta_eq )
        CALL L3interp2( zz(i,:), theta(i,:), eta, neta_eq )
     END DO

     g_33=rr*rr

     DEALLOCATE(dtdn)
     DEALLOCATE(dtdr)

  END IF

  DO j=1,neta_eq
     bb2(:,j)=ii*ii+dpsidr*dpsidr*g_11(:,j)/(tpi*tpi)
  END DO
  bb2=bb2/(rr*rr)

  gm9=1./SUM(g_33, dim=2)

  gm1=SUM(g_33/(rr*rr), dim=2) * gm9
  gm2=SUM(g_33*g_11/(rr*rr), dim=2) * gm9
  gm3=SUM(g_33*g_11, dim=2) * gm9
  gm4=SUM(g_33/bb2, dim=2) * gm9
  gm5=SUM(g_33*bb2, dim=2) * gm9
  gm6=SUM(g_33*g_11/bb2, dim=2) * gm9
  gm7=SUM(g_33*SQRT(g_11), dim=2) * gm9
  gm8=SUM(g_33*rr, dim=2) * gm9

  bb2=SQRT(bb2)
  eq_out(1)%profiles_1d%b_av=SUM(g_33*bb2, dim=2) * gm9

  gm9=SUM(g_33/rr, dim=2) * gm9

  DO i=1,nr_eq
     eq_out(1)%profiles_1d%b_min(i)=MINVAL(bb2(i,:))
     eq_out(1)%profiles_1d%b_max(i)=MAXVAL(bb2(i,:))
  END DO

  DO j=1,neta_eq
     g_11(:,j)=g_11(:,j)*dpsidr*dpsidr
     g_12(:,j)=g_12(:,j)*dpsidr
  END DO

  g_13=0._R8
  g_23=0._R8
  g_33=1./(rr*rr)

  eq_out(1)%coord_sys%jacobian=SQRT( (g_11*g_22-g_12*g_12)*g_33 )

!...  find the quantities needed by the transport solver
!...  fs avg is trivial for this model

  surface=tpi*tpi*r00*ra
  area=pi*ra*ra

  CALL L3deriv(volume,psi,nr_eq,vprime,psi,nr_eq)
  CALL L3deriv(area,psi,nr_eq,aprime,psi,nr_eq)

!...  trapped fraction for zero nu star from Wesson p 671

  ftrap=rtor/r00
  ftrap=1._R8 - ((1._R8-ftrap)**2._R8) &
       /( SQRT((1._R8-ftrap)**2._R8)*(1._R8+1.46_R8*SQRT(ftrap)) )

!...  set remaining 1d profiles

  eq_out(1)%profiles_1d%omega=0.
  eq_out(1)%profiles_1d%omegaprime=0.
  eq_out(1)%profiles_1d%mach_a=0.
  eq_out(1)%profiles_1d%phi_flow=0.
  eq_out(1)%profiles_1d%s_flow=0.
  eq_out(1)%profiles_1d%h_flow=0.


!...  set remaining global parameters

  eq_out(1)%global_param%volume = tpi*pi*r00*a00*a00
  eq_out(1)%global_param%area = pi*a00*a00
  eq_out(1)%global_param%psi_ax = psi(1)
  eq_out(1)%global_param%psi_bound = psi(nr_eq)
  eq_out(1)%global_param%q_95 = qq(nr_eq)
  eq_out(1)%global_param%q_min = MINVAL(ABS(qq))*SIGN(1.0_R8,qq(nr_eq))

!...  global integrals  Wesson defs pp 116, 120
!...  beta normal is beta_tor B a/(I_p/1 MA)

  pvol=0._R8
  parea=0._R8
  barea=0._R8
  iplasma=0._R8
  DO i=1,nr_eq-1
     hra=volume(i+1)-volume(i)
     pvol=pvol+0.5_R8*hra*(pressure(i+1)+pressure(i))
     hra=area(i+1)-area(i)
     parea=parea+0.5_R8*hra*(pressure(i+1)+pressure(i))
     iplasma=iplasma+0.5_R8*hra*(jtor(i+1)+jtor(i))
     dpsi=(ra(i+1)+ra(i))/(qq(i+1)+qq(i))
     barea=barea+hra*dpsi*dpsi
  END DO
  pvol=pvol/eq_out(1)%global_param%volume
  barea=barea/eq_out(1)%global_param%area

  eq_out(1)%global_param%i_plasma = iplasma
  eq_out(1)%global_param%beta_tor = pvol*2.0_R8*mu_0/(b00*b00)
  eq_out(1)%global_param%beta_pol = 8.0_R8*pi*parea/(mu_0*iplasma*iplasma)
  eq_out(1)%global_param%li = barea*qq(nr_eq)*qq(nr_eq)/(a00*a00)

  eq_out(1)%global_param%beta_normal = &
       eq_out(1)%global_param%beta_tor*a00*ABS(b00/(iplasma*1.e-6_R8))

!...  stamp time

  eq_out(1)%time=eq_in(1)%time

!...  write out output cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'EqOut' )
  call write_cpo(eq_out(1), 'EqOut' )
  call close_write_file

  END IF

END SUBROUTINE Bdseq


!> periodic interpolation on second dimension
!>
!> \author Bruce Scott
!>
!> \version "$Id: bdseq.f90 $"
SUBROUTINE L3interp2(y_out,x_in,x_out,neta)

  USE Phys_Constants

  IMPLICIT NONE

  INTEGER(ITM_I4) :: neta
  REAL(R8), DIMENSION(neta), INTENT(IN) :: x_in,x_out
  REAL(R8), DIMENSION(neta), INTENT(INOUT) :: y_out

  REAL(R8), DIMENSION(:), ALLOCATABLE :: y_in

  REAL(R8) :: x,aintm,aint0,aint1,aint2,xm,x0,x1,x2
  INTEGER(ITM_I4) :: j,jm,j0,j1,j2

  ALLOCATE(y_in(neta))
  y_in=y_out

  j1=1
  DO j=1,neta
     x=x_out(j)
     DO WHILE (x >= x_in(j1) .AND. j1 < neta)
        j1=j1+1
     END DO
     IF (j1 == neta .AND. x >= x_in(neta)) j1=j1+1

     j2=j1+1
     j0=j1-1
     jm=j1-2

!...  fix periodicity

     IF (jm < 1) jm=jm+neta
     IF (j0 < 1) j0=j0+neta
     IF (j1 > neta) j1=j1-neta
     IF (j2 > neta) j2=j2-neta

!...  get positions

     x2=x_in(j2)
     x1=x_in(j1)
     x0=x_in(j0)
     xm=x_in(jm)

!...  fix periodicity

     IF (xm < 0.0_R8) xm=xm+tpi
     IF (x0 < 0.0_R8) x0=x0+tpi
     IF (x1 > tpi) x1=x1-tpi
     IF (x2 > tpi) x2=x2-tpi

!...  apply

     aintm=(x-x0)*(x-x1)*(x-x2)/((xm-x0)*(xm-x1)*(xm-x2))
     aint0=(x-xm)*(x-x1)*(x-x2)/((x0-xm)*(x0-x1)*(x0-x2))
     aint1=(x-xm)*(x-x0)*(x-x2)/((x1-xm)*(x1-x0)*(x1-x2))
     aint2=(x-xm)*(x-x0)*(x-x1)/((x2-xm)*(x2-x0)*(x2-x1))

     y_out(j)=aintm*y_in(jm)+aint0*y_in(j0) &
          +aint1*y_in(j1)+aint2*y_in(j2)

  END DO

  DEALLOCATE(y_in)

END SUBROUTINE L3interp2
