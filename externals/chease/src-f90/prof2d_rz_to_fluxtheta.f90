subroutine prof2d_rz_to_fluxtheta(RR,ZZ,f2darray_in,Rbnd,Zbnd,Raxis,Zaxis,flux_norm_out,theta_out,rhopolar_out,Rout,Zout, &
     & farray_out,tension_in)
  !
  ! subroutine prof2d_rz_to_fluxtheta(RR,ZZ,f2darray_in,Rbnd,Zbnd,Raxis,Zaxis,flux_norm_out,theta_out, &
  !      & rhopolar_out,Rout,Zout,farray_out[,tension_in])
  !
  ! Given psi_norm or flux_norm (in f2darray_in(:,:,1)) on (RR,ZZ), compute flux surfaces positions on flux_norm_out,theta_out
  !
  ! Inputs:
  !   RR(:):            R mesh (1D) of f2darray_in
  !   ZZ(:):            Z mesh (1D) of f2darray_in
  !   f2darray_in(:,:,1:nin): f2darray_in(:,:,1) is the flux_norm quantity for which flux surfaces are required, 
  !                           f2darray_in(:,:,2:nin) are quantities to interpolate on the new (flux_norm,theta) mesh
  !   Rbnd:             R of plasma boundary
  !   Zbnd:             Z of plasma boundary
  !   Raxis:            R of plasma magnetic axis
  !   Zaxis:            Z of plasma magnetic axis
  !   flux_norm_out(:): normalized flux value for which to find the contours
  !   theta_out(:):     output theta mesh
  !   tension_in (optional): (default=-0.1 if not given) tension used for interpolation. Can use -0.01 for less smoothing, -1.,-3. or -10 for more smoothing
  !
  ! Outputs:
  !   flux_norm_out(:): 1D mesh for the output normalized flux surfaces requested
  !   theta_out(:):     1D mesh for the output mesh requested
  !   rhopolar_out(:,:): polar value of rho on (flux_norm_out,theta_out) on the flux surfaces
  !   Rout(:,:):         R values on the flux surfaces = Raxis + rhopolar_out * cos(theta_out)
  !   Zout(:,:):         Z values on the flux surfaces = Zaxis + rhopolar_out * sin(theta_out)
  !   farray_out(:,:,1:nin): values of f2darray_in mapped on (flux_norm_out,theta_out) mesh
  !
  use itm_types
  !
  use interpos_module
  !
  IMPLICIT NONE
  REAL(R8), PARAMETER :: PI_PARAM=3.141592653589793238462643383279502884197_r8
  !
  interface
     subroutine interpos2d_cartesian(RR,ZZ,f2darray_in,Rout,Zout,f2darray_out,tension_in,kextrapol)
       use itm_types
       !
       use interpos_module
       !
       IMPLICIT NONE
       !
       real(R8), allocatable :: RR(:), ZZ(:), f2darray_in(:,:,:), Rout(:,:), Zout(:,:)
       real(R8), intent(IN), optional :: tension_in
       integer, intent(IN), optional :: kextrapol
       !
       real(R8), allocatable :: f2darray_out(:,:,:)
     end subroutine interpos2d_cartesian
     !
     SUBROUTINE INDEXX(N,ARRIN,INDX)
       !      -------------------------------
       !
       !     NUMERICAL RECIPES SORTING ROUTINE USING HEAPSORT
       !     METHOD.
       !
       USE itm_types
       IMPLICIT NONE
       INTEGER          ::     N
       INTEGER          ::     INDX
       REAL(R8)      ::     ARRIN
       DIMENSION ARRIN(N),INDX(N)
     end SUBROUTINE INDEXX
     !
  end interface
  !
  real(R8), allocatable :: RR(:), ZZ(:), f2darray_in(:,:,:), Rbnd(:), Zbnd(:), flux_norm_out(:), theta_out(:)
  real(R8), intent(IN) :: Raxis, Zaxis
  real(R8), intent(IN), optional :: tension_in
  !
  real(R8), allocatable :: rhopolar_out(:,:), Rout(:,:), Zout(:,:), farray_out(:,:,:)
  !
  integer :: i, j, k, nverbose, nR_in, nZ_in, ndims_f2darray_in(3), nin, nflux_out, ntheta, nbnd, nbnd_eff
  real(R8) :: tension_eff, twopi, zdx
  real(R8), allocatable :: rhobnd(:), thetabnd(:), thetabnd_sorted(:), rhobnd_sorted(:), rhobnd_out(:), &
       & sigma(:), rhomesh(:,:), Rrhotheta(:,:), Zrhotheta(:,:), f2darray_polar(:,:,:)
  integer, allocatable :: i_sorted(:)
  !
  twopi = 2._R8 * PI_PARAM
  nverbose = 3
  !
  tension_eff = -0.1_R8
  if (present(tension_in)) then
    tension_eff = tension_in
  end if
  ! check sizes
  nR_in = size(RR)
  nZ_in = size(ZZ)
  ndims_f2darray_in = shape(f2darray_in)
  if (ndims_f2darray_in(1) .NE. nR_in) then
    write(6,*) 'size(f2darray_in,1) should be equal to size(RR)'
    flush(6)
    return
  end if
  if (ndims_f2darray_in(2) .NE. nZ_in) then
    write(6,*) 'size(f2darray_in,2) should be equal to size(ZZ)'
    flush(6)
    return
  end if
  nin = ndims_f2darray_in(3)
  nflux_out = size(flux_norm_out)
  ntheta = size(theta_out)
  if ((size(rhopolar_out,1) .NE. nflux_out) .OR. (size(rhopolar_out,2) .NE. ntheta)) then
    write(6,*) 'shape(rhopolar_out) should be equal to (size(flux_norm_out),size(theta_out))'
    flush(6)
    return
  end if
  if ((size(Rout,1) .NE. nflux_out) .OR. (size(Rout,2) .NE. ntheta)) then
    write(6,*) 'shape(Rout) should be equal to (size(flux_norm_out),size(theta_out))'
    flush(6)
    return
  end if
  if ((size(Zout,1) .NE. nflux_out) .OR. (size(Zout,2) .NE. ntheta)) then
    write(6,*) 'shape(Zout) should be equal to (size(flux_norm_out),size(theta_out))'
    flush(6)
    return
  end if
  if ((size(farray_out,1) .NE. nflux_out) .OR. (size(farray_out,2) .NE. ntheta) .OR. (size(farray_out,3) .NE. nin)) then
    write(6,*) 'shape(farray_out) should be equal to (size(flux_norm_out),size(theta_out),nin)'
    flush(6)
    return
  end if
  !
  ! Compute (rhobound,thetabound)
  !
  nbnd = size(Rbnd)
  allocate(rhobnd(nbnd))
  allocate(thetabnd(nbnd))
  rhobnd=sqrt((Rbnd-Raxis)**2 + (Zbnd-Zaxis)**2);
  thetabnd=atan2(Zbnd-Zaxis,Rbnd-Raxis);
  do i=1,nbnd
    if (thetabnd(i) .LE. 0._R8) thetabnd(i) = thetabnd(i) + twopi
  END do
  ! sort theta mesh
  allocate(thetabnd_sorted(nbnd))
  allocate(rhobnd_sorted(nbnd))
  allocate(i_sorted(nbnd))
  call indexx(nbnd,thetabnd,i_sorted)
  thetabnd_sorted = thetabnd(i_sorted(1:nbnd))
  rhobnd_sorted = rhobnd(i_sorted(1:nbnd))
  nbnd_eff = nbnd
  if (abs(thetabnd_sorted(nbnd)-twopi-thetabnd_sorted(1)) .lt. 1e-07_R8) then
    if (NVERBOSE .GE. 0) write(6,*) 'in bndfit: end theta points given twice, remove it'
    nbnd_eff = nbnd_eff - 1
  end if
  ! check that there are not 2 points too close  in rho (although now also done in interpos)
  i=1
  zdx=TWOPI / real(nbnd_eff,r8)
  do j=2,nbnd_eff
    if (abs(thetabnd_sorted(j)-thetabnd_sorted(i)) .gt. 1e-6_R8*zdx) then
      i=i+1;
      thetabnd_sorted(i) = thetabnd_sorted(j)
      rhobnd_sorted(i) = rhobnd_sorted(j)
    end if
  end do
  if ((nbnd_eff .GT. i) .AND. (NVERBOSE.GE.0)) write(6,*) &
    & 'Some points along plasma boundary too close in theta and ignored in prof2dRZ_to_fluxtheta: nbnd_eff=', &
    & nbnd_eff,' reduced to ',i
  nbnd_eff = i;
  ! compute rho_LCFS at theta_out points
  allocate(rhobnd_out(ntheta))
  call interpos(thetabnd_sorted,rhobnd_sorted,nbnd_eff,nout=ntheta,tension=tension_eff, &
    & xout=theta_out,yout=rhobnd_out,nbc=-1,ybc=twopi)
  !
  ! create a polar rho mesh, using theta_out as theta mesh, to be able to find flux surfaces on each theta line
  ! use nflux_out point to have similar resolution as asked in output
  zdx = 1._R8 / real(nflux_out-1,R8)
  allocate(sigma(nflux_out))
  allocate(rhomesh(nflux_out,ntheta))
  allocate(Rrhotheta(nflux_out,ntheta))
  allocate(Zrhotheta(nflux_out,ntheta))
  sigma = (/0._R8, (real(i,R8)*zdx, i=1,nflux_out-2), 1._R8 /)
  do j=1,ntheta
    rhomesh(1:nflux_out,j) = sigma * rhobnd_out(j)
    Rrhotheta(1:nflux_out,j) = Raxis + rhomesh(1:nflux_out,j) * cos(theta_out(j))
    Zrhotheta(1:nflux_out,j) = Zaxis + rhomesh(1:nflux_out,j) * sin(theta_out(j))
  END do
  ! Compute flux_norm on Rrhotheta, Zrhotheta points using flux_norm(RR,ZZ). This also gives values on rhomesh,theta_out
  ! Interpolate each function given in farray_in(:,:,k)
  allocate(f2darray_polar(nflux_out,ntheta,nin))
  call interpos2d_cartesian(RR,ZZ,f2darray_in,Rrhotheta,Zrhotheta,f2darray_polar,tension_eff)
  ! make sure of edge value (hence useful to have normalized flux, phi or psi
  f2darray_polar(1,:,1) = 0._R8
  f2darray_polar(nflux_out,:,1) = 1._R8
  !
  ! Compute, for each theta line, the rho_polar value corresponding to the requested flux_norm_out values
  ! use sqrt(flux) as x-axis for interpolation since flux not good for inverse interpolation near axis (hence also useful to have normalized flux for B.C.)
  !
  do j=1,ntheta
    call interpos(sqrt(f2darray_polar(:,j,1)),rhomesh(:,j),nflux_out,nout=nflux_out,tension=tension_eff,xout=sqrt(flux_norm_out), &
         & yout=rhopolar_out(:,j),nbc=(/2, 2/),ybc=(/rhomesh(1,j), rhomesh(nflux_out,j) /))
    Rout(:,j) = Raxis + rhopolar_out(:,j) * cos(theta_out(j))
    Zout(:,j) = Zaxis + rhopolar_out(:,j) * sin(theta_out(j))
    farray_out(:,j,1) = flux_norm_out;
    do k=2,nin
      call interpos(rhomesh(:,j),f2darray_polar(:,j,k),nflux_out,nout=nflux_out,tension=tension_eff,xout=rhopolar_out(:,j), &
           & yout=farray_out(:,j,k),nbc=(/2, 2/),ybc=(/f2darray_polar(1,j,k), f2darray_polar(nflux_out,j,k) /))
    end do
  end do
  deallocate(rhobnd,thetabnd,thetabnd_sorted, rhobnd_sorted, rhobnd_out, &
       & sigma, rhomesh, Rrhotheta, Zrhotheta, f2darray_polar,i_sorted)
  
  return
  
end subroutine prof2d_rz_to_fluxtheta
