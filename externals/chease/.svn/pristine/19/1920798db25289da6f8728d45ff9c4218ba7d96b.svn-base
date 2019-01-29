subroutine interpos2d_cartesian(RR,ZZ,f2darray_in,Rout,Zout,f2darray_out,tension_in,kextrapol)
  !
  ! subroutine interpos2d_cartesian(RR,ZZ,f2darray_in,Rout,Zout,f2darray_out[,tension_in,kextrapol])
  !
  ! interpolate f(RR,ZZ) onto Rout,Zout assuming 1D meshes RR(:) and ZZ(:)
  ! f is each of k=1,nin from f2darray_in(:,:,k)
  ! Rout, Zout can be 2d series of points of a 1d chord. 
  ! To match assumed 2d dimension, the "1d" Rout case should be given with size(Rout,2)=1 and same for Zout
  !
  ! Inputs:
  !   RR(:):            R mesh (1D) of f2darray_in
  !   ZZ(:):            Z mesh (1D) of f2darray_in
  !   f2darray_in(:,:,1:nin): quantities to interpolate on the points (Rout,Zout)
  !   Rout(:,:):        R values on which the quantities are requested
  !   Zout(:,:):        Z values on which the quantities are requested
  !   tension_in (optional): (default=-0.1 if not given) tension used for interpolation. Can use -0.01 for less smoothing, -1.,-3. or -10 for more smoothing
  !   kextrapol (optional): extrapolation option (see below)
  !
  ! Outputs:
  !   f2darray_out(:,:,1:nin): values of f2darray_in interpolated on (Rout,Zout) points
  !
  ! extrapolation is performed according to kextrapol (same for R and Z at this stage):  
  !     kextrapol = 0 => constant outside the (R_in,Z_in) domain with f_extrapol = f_edge
  !     kextrapol = 1 => linear extrapolation with continuous derivative along the direction (either R or Z)
  !     kextrapol = 2 => quadratic extrapolation with continuous derivative
  !     kextrapol = 3 (default)=> cubic extrapolation nearby and then quadratic
  !     kextrapol = 4 => cubic extrapolation (only safe if does not extrapolate very far)
  !     kextrapol = 5 => no extrapolation. Get a warning if it does extrapolate
  !
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
  !
  integer :: k, jin, iout, jout, kextrapol_eff, nverbose, nR_in, nZ_in, ndims_f2darray_in(3), nin, &
    & nRZ_out_dim1, nRZ_out_dim2, iopt_interpos, nbc(2)
  real(R8) :: tension_eff, tension_R, tension_Z, ybc(2)
  real(R8), allocatable :: f2d_Rout_Zin(:,:)
  !
  nverbose = 3
  !
  tension_eff = -0.1_R8
  if (present(tension_in)) then
    tension_eff = tension_in
  end if
  !
  kextrapol_eff = 3
  if (present(kextrapol)) then
    kextrapol_eff = kextrapol
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
  nRZ_out_dim1 = size(Rout,1)
  nRZ_out_dim2 = size(Rout,2)
  if ((size(Zout,1) .NE. nRZ_out_dim1) .OR. (size(Zout,2) .NE. nRZ_out_dim2)) then
    write(6,*) 'shape(Zout) should be equal to shape(Rout)'
    flush(6)
    return
  end if
  if ((size(f2darray_out,1) .NE. nRZ_out_dim1) .OR. (size(f2darray_out,2) .NE. nRZ_out_dim2) .OR. & 
       &(size(f2darray_out,3) .NE. nin)) then
    write(6,*) 'shape(f2darray_out) should be equal to (size(Rout,1),size(Rout,2),nin)'
    flush(6)
    return
  end if
  !
  select case (kextrapol_eff)
  case(0)
    iopt_interpos = 63; ! constant f_extrapo = f_edge
  case(1)
    iopt_interpos = 23; ! linear with continuous derivative
  case(2)
    iopt_interpos = 33; ! quadratic with continuous derivative
  case(3)
    iopt_interpos = 13; ! cubic nearby then quadratic (default interpos)
  case(4)
    iopt_interpos = 43; ! cubic 
  case(5)
    iopt_interpos = 3; ! no extrapolation, get warning from interpos if extrapolating
  case default
    write(6,*) 'case kextrapol = ',kextrapol,' not yet implmented, check kextrapol in interpos2d_cartesian'
    flush(6)
    return
  end select
  !
  allocate(f2d_Rout_Zin(nRZ_out_dim1,nZ_in))
  do k=1,nin
    tension_R = tension_eff
    tension_Z = tension_eff
    nbc = (/0, 0/) ! to allow tension = 0, one should put (0, 0)
    ybc = (/0._R8, 0._R8 /)
    do jout=1,nRZ_out_dim2
      ! loop over j and first spline on 1st dimension
      do jin=1,nZ_in
        call interpos(RR,f2darray_in(:,jin,k),nR_in,nout=nRZ_out_dim1,tension=tension_eff,xout=Rout(:,jout), &
             & yout=f2d_Rout_Zin(:,jin),nbc=nbc,ybc=ybc,option=iopt_interpos)
      end do
      ! then interpolate on Z using f2d_Rout_Zin(Rout(1:nRZ_out_dim1,jout),ZZ(1:nZ_in))
      do iout=1,nRZ_out_dim1
        call interpos(ZZ,f2d_Rout_Zin(iout,:),nZ_in,xscal=Zout(iout,jout),tension=tension_eff,yscal=f2darray_out(iout,jout,k), &
             & nbcscal=nbc,ybcscal=ybc,option=iopt_interpos)
      end do
    end do
  end do
  !
  deallocate(f2d_Rout_Zin)
  return
  !
end subroutine interpos2d_cartesian
