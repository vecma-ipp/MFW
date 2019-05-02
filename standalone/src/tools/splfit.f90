
module splfit

contains
  
  !> @brief     Determine the knot span index 
  !>
  !> @param[in]  n     number of control points 
  !> @param[in]  p     spline degree 
  !> @param[in]  U     Knot vector 
  !> @param[in]  uu    given knot 
  !> @param[out] span  span index 
  function find_span(n, p, uu, U) result (span)
  implicit none
    integer(kind=4), intent(in) :: n, p
    real   (kind=8), intent(in) :: uu, U(0:n+p+1)
    integer(kind=4)             :: span
    integer(kind=4) low, high

    if (uu >= U(n+1)) then
       span = n
       return
    end if
    if (uu <= U(p)) then
       span = p
       return
    end if
    low  = p
    high = n+1
    span = (low + high) / 2
    do while (uu < U(span) .or. uu >= U(span+1))
       if (uu < U(span)) then
          high = span
       else
          low  = span
       end if
       span = (low + high) / 2
    end do

  end function find_span

  !> @brief      Compute the nonvanishing basis functions
  !>
  !> @param[in]  p    spline degree 
  !> @param[in]  U    Knot vector 
  !> @param[in]  uu   Knot 
  !> @param[in]  i    span of a knot 
  !> @param[out] N    all (p+1) Splines non-vanishing at uu 
  subroutine basis_funs(i, uu , p, U, N)
    implicit none
    integer(kind=4), intent(in) :: i, p
    real   (kind=8), intent(in) :: uu, U(0:i+p)
    real   (kind=8), intent(out):: N(0:p)
    
    integer(kind=4) :: j, r
    real   (kind=8) :: left(p), right(p), saved, temp

    N(0) = 1.0
    do j = 1, p
       left(j)  = uu - U(i+1-j)
       right(j) = U(i+j) - uu
       saved = 0.0
       do r = 0, j-1
          temp = N(r) / (right(r+1) + left(j-r))
          N(r) = saved + right(r+1) * temp
          saved = left(j-r) * temp
       end do
       N(j) = saved
    end do

  end subroutine basis_funs

  !> @brief    Computes collocation matrix 
  !>
  !> @param[in]  n      number of control points 
  !> @param[in]  p      spline degree 
  !> @param[in]  knots  Knot vector 
  !> @param[in]  arr_x  array of sites for evaluation 
  !> @param[out] mat    mat is a dense matrix of size (n_points, m_points) 
  !>                    where m depends on the boundary condition
  subroutine collocation_matrix(n, p, knots, arr_x, mat)
  implicit none
    integer,                 intent(in)  :: n
    integer,                 intent(in)  :: p
    real(8), dimension(:),   intent(in)  :: knots
    real(8), dimension(:),   intent(in)  :: arr_x 
    real(8), dimension(:,:), intent(out) :: mat 
    
    integer :: i
    integer :: j
    integer :: span
    integer :: n_points
    real(8) :: x
    real(8), dimension(:,:), allocatable :: batx
    integer, dimension(:), allocatable :: spans
    
    n_points = size(arr_x, 1)
    allocate(batx(p+1,n_points))
    allocate(spans(n_points))
    
    do i = 1, n_points
      x = arr_x(i) 
      span = find_span(n-1, p, x, knots)
      spans(i) = span
      call basis_funs(span, x, p, knots, batx(:,i))
    end do
   
    mat = 0.0d0
    do i = 1, n_points
      span = spans(i)
      do j = 0, p
        mat(i,span-p+j+1) = batx(j+1,i)
      enddo
    enddo
    
    deallocate(spans)
    deallocate(batx)

  end subroutine collocation_matrix 
  
  ! Computes the Spline parameterization
  function sites(x, y, method) result(u)

    implicit none

    real(8), dimension(:), intent(in) :: x
    real(8), dimension(:), intent(in) :: y
    character(*), intent(in) :: method

    real(8), dimension(:), allocatable :: u
    real(8), dimension(:), allocatable :: di
    integer :: i, n, d

    n  = size(x, 1)  
    if (size(y) /= n) then
      write(*,*) "ERROR: x and y should have the same size."
      stop
    end if
    
    allocate(u(n))
    allocate(di(n-1))

    if (method == "uniform") then
      do i = 0, n-1
        u(i+1) = i/(n-1)
      enddo

    else
      u(:) = 0.0
      u(n) = 1.
      di(:) = 0.0
      d = 0.0

      if (method == "chord") then
        do i =2, n
          di(i-1) = dsqrt((x(i)-x(i-1))**2 + (y(i)-y(i-1))**2)
          d = d + di(i-1)
        enddo
      endif
      
      if (method == "centripetal") then
        do i = 2, n
          di(i-1) = dsqrt(dsqrt((x(i)-x(i-1))**2 + (y(i)-y(i-1))**2))
          d = d + di(i-1)
        enddo
      endif

      do i = 2, n-1
        u(i) = u(i-1) + di(i-1)/d
      enddo
      
     endif

     deallocate(di)
    return 
    
  end function sites
!
!  ! 
!  ! .......................................................
!  !> @brief    Curve fitting of a set of m points (coordinates are (x[i], y[i]) 
!  !           using Least Squares approximation. The endpoints are interpolated.
!  !>
!  !> @param[in]  x,y     The coordinates of the 1D curve.
!  !> @param[in]  n       The number of elements.
!  !> @param[in]  p       spline degree 
!  !> @param[in]  method  The parameterization method 
!  !> @param[out]  t  The knots  method
!  !> @param[out]  c  The control points.
!  subroutine approximate_curve(x, y, n, p, method, t, c):
!    implicit none
!
!    real(8), dimension(:), intent(in) :: x
!    real(8), dimension(:), intent(in) :: y
!    integer, intent(in) :: n
!    integer, intent(in) :: k
!    character(*), intent(in) :: method
!
!    real(8), dimension(:), allocatable, intent(out) :: t
!    real(8), dimension(:, :), allocatable, intent(out) :: c
!
!    ! The parameter values
!    u = sites(x, y, method)
!
!    ! Knots vector   
!    allocate(t(n+2*p))
!    do i =1, p+1
!      t(i) = 0.
!      t(n+p+i-1) = 1.
!    enddo
!    do i = 1, n-2
!      t(p+i+1) = 1.0*i/(n-1)
!    enddo
!
!    ! Number of vertices and control points
!    nv = size(x)
!    nc = n+p-1
!
!    ! Collocation matrix of size (nc, nv)
!    collocation_matrix(nv, p, t, u, Mc)
!
!    ! Get matrix and rhs for Least Squares Linear system
!    ! M = Dt*D (D = Mc(1:nv-1, 1:nc-1)
!    ! Removed lines and rows correspond to the interpolated endpoints
!    ! TODO optimize loops
!    do i=1, nc-1
!      do j=1, nc-1
!        do k=1, nv-1
!          M(i,j) = M(i,j) + Mc(k,i)*Mc(k,j)
!        enddo
!      enddo
!    enddo  
!
!    allocate(b(nc-2, 2))
!    allocate(z(nv-2, 2))
!
!    do i=2, nv-1
!      z(i-1, 1) = x(i) - Mc(i,1)*x(1) - Mc(i,nc)*x(m)
!      z(i-1, 2) = y(i) - Mc(i,1)*y(1) - Mc(i,nc)*y(m)
!    enddo
!
!    b(:, 1) = matmul(Mc(1:nc-1, 1:nv-1), z(:, 1))
!    b(:, 2) = matmul(Mc(1:nc-1, 1:nv-1), z(:, 2))
! 
!    ! ... Solve linear syestem
!    M    = csc_matrix(M)
!    M_op = splu(M)
!    C    =  M_op.solve(B)
!
!    ! ...  Control points
!    C = np.array([[x[0], y[0]]] + list(C) + [[x[-1], y[-1]]])
!  
!  subroutine approximate_curve

# ...
end module splfit
