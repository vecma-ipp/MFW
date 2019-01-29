! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Augment an inverse equilibrium with psi, Br, Bz and Bphi as a function of R and Z.
!>
!> \author D. Coster
!>
!> \version "$Id: equilibrium_augmenter.f90 593 2012-10-19 14:11:32Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
module equilibrium_augmenter

  use euitm_schemas
  use itm_types
  use itm_constants
  use deallocate_structures

contains

  subroutine augment_psi_rz(equilibrium)

    implicit none

    type (type_equilibrium) :: equilibrium
    integer, parameter      :: NR=65, NZ=129
    integer                 :: npsi, ndim1, ndim2
    integer                 :: idim1, idim2

    real (R8), allocatable  :: psi_in(:,:), f_dia_in(:,:), w(:,:)

    real (R8), allocatable  :: r(:), z(:), ans(:), tx(:), ty(:), c(:),  &
                               wrk1(:), wrk2(:), wrk(:)
    integer, allocatable    :: iwrk(:), jwrk(:)

    real (R8)               :: rmin, rmax, zmin, zmax, dr, dz, fp, s, r0, b0, psi_ax, psi_bnd, sign_psi
    integer                 :: kx, ky, m,nxest, nyest, nmax, u, v, km, ne, bx, by,  &
                               b1, b2, lwrk1, lwrk2, lwrk, kwrk, mwrk, nx, ny, ier, i, j

    
    npsi           = size(equilibrium%profiles_1d%psi, dim=1)
    ndim1          = size(equilibrium%coord_sys%position%R, dim=1)
    ndim2          = size(equilibrium%coord_sys%position%R, dim=2)

    if(npsi.ne.ndim1) then
       write(*,*) 'NPSI <> NDIM1: ',npsi, ndim1
       stop 'NPSI <> NDIM1'
    endif

    r0=equilibrium%global_param%toroid_field%r0
    b0=equilibrium%global_param%toroid_field%b0
    psi_ax=equilibrium%profiles_1d%psi(1)
    psi_bnd=equilibrium%profiles_1d%psi(NPSI)
    sign_psi=sign(1.0_R8, psi_bnd-psi_ax)

    allocate(psi_in(ndim1,ndim2), f_dia_in(ndim1,ndim2), w(ndim1,ndim2))
    allocate(r(NR), z(NZ))

    do idim2=1, ndim2
       psi_in(:,idim2) = equilibrium%profiles_1d%psi(:)
       f_dia_in(:,idim2) = equilibrium%profiles_1d%f_dia(:)
    enddo

    w=1.0_R8

    rmin=minval(equilibrium%coord_sys%position%R)
    rmax=maxval(equilibrium%coord_sys%position%R)
    zmin=minval(equilibrium%coord_sys%position%Z)
    zmax=maxval(equilibrium%coord_sys%position%Z)

    dr=rmax-rmin
    dz=zmax-zmin

    rmin=rmin-dr/20.0_R8
    rmax=rmax+dr/20.0_R8
    zmin=zmin-dz/20.0_R8
    zmax=zmax+dz/20.0_R8

    dr=(rmax-rmin)/(NR-1)
    dz=(zmax-zmin)/(NZ-1)

    do i=1, NR
       r(i)=rmin+(i-1)*dr
    enddo
    do i=1, NZ
       z(i)=zmin+(i-1)*dz
    enddo

    kx=3
    ky=3

    m = ndim1*ndim2
    nxest = max(2*(kx+1),kx+1+int(sqrt(m/2.0_R8)))
    nyest = max(2*(ky+1),ky+1+int(sqrt(m/2.0_R8)))
    nmax = max(nxest, nyest)
    u = nxest-kx-1
    v = nyest-ky-1
    km = max(kx,ky)+1
    ne = max(nxest,nyest)
    bx = kx*v+ky+1
    by = ky*u+kx+1
    if(bx.le.by) then
       b1 = bx
       b2 = b1+v-ky
    else
       b1 = by
       b2 = b1+u-kx
    endif
    lwrk1 = u*v*(2+b1+b2)+2*(u+v+km*(m+ne)+ne-kx-ky)+b2+1
    lwrk2 = u*v*(b2+1)+b2
    kwrk = m+(nxest-2*kx-1)*(nyest-2*ky-1)

    allocate(tx(nmax), ty(nmax), c((nxest-kx-1)*(nyest-ky-1)),   &
         wrk1(lwrk1), wrk2(lwrk2), iwrk(kwrk))

    s = sqrt(real(m,R8))/100

! spline psi
    call surfit(0, m,   &
         equilibrium%coord_sys%position%R, equilibrium%coord_sys%position%Z, psi_in,  &
         w, rmin, rmax, zmin, zmax, kx, ky, s, nxest, nyest, nmax, 1e-15_R8,   &
         nx, tx, ny, ty, c, fp, wrk1, lwrk1, wrk2, lwrk2, iwrk, kwrk, ier)

    write(*,*) 'surfit returned ier = ', ier
    write(*,*) 'nx, ny, fp = ', nx, ny, fp

    lwrk = NR*(kx+1)+NZ*(ky+1)+(nx-1)*(ny-1)
    mwrk = NR+NZ

    allocate(wrk(lwrk), jwrk(mwrk), ANS(NR*NZ))

    call bispev(tx, nx, ty, ny, c, kx, ky, R, NR, Z, NZ, ANS, wrk, lwrk, jwrk, mwrk, ier)

    write(*,*) 'bispev returned ier = ', ier
    
    if(associated(equilibrium%profiles_2d)) call deallocate_cpo(equilibrium%profiles_2d)
    allocate(equilibrium%profiles_2d(1))

    allocate(equilibrium%profiles_2d(1)%psi(NR,NZ))
    do i=1, NR
       do j=1, NZ
          equilibrium%profiles_2d(1)%psi(i,j) = ANS(NZ*(i-1)+j)
       enddo
    enddo

! br
    call parder(tx, nx, ty, ny, c, kx, ky, 0, 1, R, NR, Z, NZ, ANS, wrk, lwrk, jwrk, mwrk, ier)

    write(*,*) 'parder returned ier = ', ier

    allocate(equilibrium%profiles_2d(1)%br(NR,NZ))
    do i=1, NR
       do j=1, NZ
          equilibrium%profiles_2d(1)%br(i,j) = -ANS(NZ*(i-1)+j)/R(i)/2.0_R8/itm_pi
       enddo
    enddo

!bz
    call parder(tx, nx, ty, ny, c, kx, ky, 1, 0, R, NR, Z, NZ, ANS, wrk, lwrk, jwrk, mwrk, ier)

    write(*,*) 'parder returned ier = ', ier

    allocate(equilibrium%profiles_2d(1)%bz(NR,NZ))
    do i=1, NR
       do j=1, NZ
          equilibrium%profiles_2d(1)%bz(i,j) = ANS(NZ*(i-1)+j)/R(i)/2.0_R8/itm_pi
       enddo
    enddo

    s = sqrt(real(m,R8))/10000

! spline f_dia
    call surfit(0, m,   &
         equilibrium%coord_sys%position%R, equilibrium%coord_sys%position%Z, f_dia_in,  &
         w, rmin, rmax, zmin, zmax, kx, ky, s, nxest, nyest, nmax, 1e-15_R8,   &
         nx, tx, ny, ty, c, fp, wrk1, lwrk1, wrk2, lwrk2, iwrk, kwrk, ier)

    write(*,*) 'surfit returned ier = ', ier
    write(*,*) 'nx, ny, fp = ', nx, ny, fp

    deallocate(wrk, jwrk, ANS)

    lwrk = NR*(kx+1)+NZ*(ky+1)+(nx-1)*(ny-1)
    mwrk = NR+NZ

    allocate(wrk(lwrk), jwrk(mwrk), ANS(NR*NZ))

    call bispev(tx, nx, ty, ny, c, kx, ky, R, NR, Z, NZ, ANS, wrk, lwrk, jwrk, mwrk, ier)

    write(*,*) 'bispev returned ier = ', ier

    allocate(equilibrium%profiles_2d(1)%bphi(NR,NZ))
    do i=1, NR
       do j=1, NZ
          if(equilibrium%profiles_2d(1)%psi(i,j)*sign_psi .gt. psi_bnd*sign_psi) then
             equilibrium%profiles_2d(1)%bphi(i,j) = r0*b0/R(i)
          else
             equilibrium%profiles_2d(1)%bphi(i,j) = ANS(NZ*(i-1)+j)/R(i)
          endif
       enddo
    enddo

    allocate(equilibrium%profiles_2d(1)%grid_type(4))
    allocate(equilibrium%profiles_2d(1)%grid%dim1(NR))
    allocate(equilibrium%profiles_2d(1)%grid%dim2(NZ))
    equilibrium%profiles_2d(1)%grid_type(1)='1'
    equilibrium%profiles_2d(1)%grid_type(2)='rectangular'
    equilibrium%profiles_2d(1)%grid_type(3)='0'
    equilibrium%profiles_2d(1)%grid_type(4)='not relevant'
    equilibrium%profiles_2d(1)%grid%dim1=r
    equilibrium%profiles_2d(1)%grid%dim2=z
    
    return

  end subroutine augment_psi_rz

end module equilibrium_augmenter
