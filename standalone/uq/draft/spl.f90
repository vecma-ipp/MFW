!> Module for curves approximation and evalution using non periodic splines.

module spl_module

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
    real(8), dimension(:,:), intent(inout) :: mat 
    
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
  subroutine sites(x, y, method, u)
    implicit none
    real(8), dimension(:), intent(in) :: x
    real(8), dimension(:), intent(in) :: y
    character(*), intent(in) :: method
    real(8), dimension(:), intent(inout) :: u

    real(8), dimension(:), allocatable :: di
    real(8) :: d
    integer :: i, n

    n  = size(x, 1)  
    if ((size(y) /= n).or.(size(u) /= n)) then
      write(*,*) "ERROR: x, y and u should have the same size."
      stop
    end if

    if (method == "uniform") then
      do i = 0, n-1
        u(i+1) = 1.0*i/(n-1)
      enddo

    else
      allocate(di(n-1))
      u = 0.0
      u(n) = 1.0
      di = 0.0
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

      deallocate(di)
      
    endif
  end subroutine sites

  !> @brief  Curve fitting of a set of m points (coordinates are (x[i], y[i]) 
  !          using Least Squares approximation. The endpoints are interpolated.
  !>
  !> @param[in]  X,Y     The coordinates of the 1D curve
  !> @param[in]  n       The number of control points (n>k)
  !> @param[in]  k       Spline degree 
  !> @param[in]  method  The parameterization method 
  !> @param[out] T       The Knots  vector
  !> @param[out] P       The control points
  subroutine approximate_curve(X, Y, n, k, method, T, P)
    implicit none
    real(8), dimension(:), intent(in) :: X
    real(8), dimension(:), intent(in) :: Y
    integer, intent(in) :: n
    integer, intent(in) :: k
    character(*), intent(in) :: method

    real(8), dimension(:), intent(inout) :: T
    real(8), dimension(:, :), intent(inout) :: P

    real(8), dimension(:), allocatable :: u
    real(8), dimension(:, :), allocatable :: C, A, R, B

    integer :: info, m, i 
    
    ! Check sizes
    if (size(T) /= n+k+1) then
      write(*,*) "ERROR: T has wrong size."
      stop
    end if
    if( (size(P,1) /= n).or.(size(P,2) /= 2)) then
      write(*,*) "ERROR: P has wrong size."
      stop
    end if

    ! Number of verticies
    m = size(X)

    ! The parameter values
    allocate(u(m))
    call sites(X, Y, method, u)
    
    ! Knots vector   
    do i =1, k+1
      T(i) = 0.0
      T(n+i) = 1.0
    enddo
    do i = 1, n-k-1
      T(k+i+1) = 1.0*i/(n-k)
    enddo
    
    ! Collocation matrix of size (m, n)
    allocate(C(m, n))
    call collocation_matrix(n, k, T, u, C)

    ! Get matrix and rhs for Least Squares Linear system
    ! A = Dt*D (D = C(2:n-1, 2:m-1)
    ! removed lines and rows correspond to the interpolated endpoints
    allocate(A(n-2, n-2))
    A = matmul(transpose(C(2:m-1, 2:n-1)), C(2:m-1, 2:n-1))
    
    ! RHS
    allocate(B(n-2, 2))
    allocate(R(m-2, 2))

    do i=2, m-1
      R(i-1, 1) = x(i) - C(i,1)*x(1) - C(i,n)*x(m)
      R(i-1, 2) = y(i) - C(i,1)*y(1) - C(i,n)*y(m)
    enddo

    B(:, 1) = matmul(transpose(C(2:m-1, 2:n-1)), R(:, 1))
    B(:, 2) = matmul(transpose(C(2:m-1, 2:n-1)), R(:, 2))

    ! Solve the linear syestem using  LAPACK
    call DPOSV('Upper', n-2, 2, A, n-2, B, n-2, INFO)
    
    ! Check for the exact singularity. 
    if( info.gt.0 ) then
         WRITE(*,*)'The leading minor of order ',INFO,' is not positive'
         WRITE(*,*)'definite; the solution could not be computed.'
         STOP
    end if
    
    ! Interpolate endpoints
    P(1,1) = X(1)
    P(1,2) = Y(1)
    P(n,1) = X(m)
    P(n,2) = Y(m)
    ! The rest of the Control points
    P(2:n-1,1) = B(:,1)
    P(2:n-1,2) = B(:,2)

    ! Dellocations
    deallocate(u) 
    deallocate(C) 
    deallocate(A) 
    deallocate(B) 
    deallocate(R) 

  end subroutine approximate_curve
  ! ================================================================================
  
  ! ================================================================================
  !> @brief     elevate the spline at X 
  !>
  !> @param[in] d manifold dimension for the control points  
  !> @param[in] n number of control points 
  !> @param[in] p spline degree 
  !> @param[in] U Initial Knot vector 
  !> @param[in] Q Initial Control points  
  !> @param[in] r dimension of X - 1 
  !> @param[in] X the positions on wich evaluation is done  
  !> @param[out] Cw Values  
  subroutine splev(d, n, p, U, Q, r, X, Cw)
    implicit none

    integer(kind=4), intent(in)  :: d
    integer(kind=4), intent(in)  :: n, p
    real   (kind=8), intent(in)  :: U(n+p+1)
    real   (kind=8), intent(in)  :: Q(d,n)
    integer(kind=4), intent(in)  :: r
    real   (kind=8), intent(in)  :: X(r)
    real   (kind=8), intent(out) :: Cw(d,r)
    integer(kind=4) :: i, j, span
    real   (kind=8) :: basis(p+1)
    
    Cw = 0.0
    do i = 1, r
      span = find_span(n-1, p, X(i), U)
      call basis_funs(span, X(i), p, U, basis)
      
      do j = 0, p
        Cw(:, i) = Cw(:, i) + basis(j+1) * Q(:,span-p+j+1)
      end do
    end do

  end subroutine splev

  !> @brief  Curve fitting of a set of m points (coordinates are (x[i], y[i]) 
  !          using Least Squares approximation. The endpoints are interpolated.
  !>
  !> @param[in]  x       The coordinates of the 1D curve
  !> @param[in]  n       The number of control points (n>p)
  !> @param[in]  p       Spline degree 
  !> @param[out] knots   The Knots  vector
  !> @param[out] c       The control points
  subroutine splrep(x, n, p, knots, c)
    implicit none
    real(8), dimension(:), intent(in) :: x
    integer, intent(in) :: n
    integer, intent(in) :: p

    real(8), dimension(:), intent(out) :: knots
    real(8), dimension(:), intent(out) :: c

    real(8), dimension(:), allocatable :: u, b, r 
    real(8), dimension(:, :), allocatable :: A, mat

    integer :: info, m, i 
    
    ! Number of verticies
    m = size(x)
    
    ! The parameter values
    allocate(u(m))
    do i = 0, m-1
      u(i+1) = 1.0*i/(m-1)
    enddo
    
    ! Knots vector   
    do i =1, p+1
      knots(i) = 0.0
      knots(n+i) = 1.0
    enddo
    
    do i = 1, n-p-1
      knots(p+i+1) = 1.0*i/(n-p)
    enddo
    
    ! Collocation matrix of size (m, n)
    allocate(mat(m, n))
    call collocation_matrix(n, p, knots, u, mat)
    
    ! Get matrix and rhs for Least Squares Linear system
    ! A = Dt*D (D = mat(2:n-1, 2:m-1)
    ! removed lines and rows correspond to the interpolated endpoints
    allocate(A(n-2, n-2))
    A = matmul(transpose(mat(2:m-1, 2:n-1)), mat(2:m-1, 2:n-1))
    
    ! RHS
    allocate(b(n-2))
    allocate(r(m-2))

    do i=2, m-1
      r(i-1) = x(i) - mat(i,1)*x(1) - mat(i,n)*x(m)
    enddo

    b(:) = matmul(transpose(mat(2:m-1, 2:n-1)), r(:))
    
    ! Solve the linear syestem using  LAPACK
    call DPOSV('Upper', n-2, 1, A, n-2, b, n-2, INFO)
    
    ! Check for the exact singularity. 
    if( info.gt.0 ) then
         WRITE(*,*)'The leading minor of order ',INFO,' is not positive'
         WRITE(*,*)'definite; the solution could not be computed.'
         STOP
    end if
    
    ! Interpolate endpoints
    c(1) = x(1)
    c(n) = x(m)
    ! The rest of the Control points
    c(2:n-1) = b(:)

    ! Dellocations
    deallocate(u) 
    deallocate(mat) 
    deallocate(A) 
    deallocate(b) 
    deallocate(r) 
  end subroutine splrep

end module spl_module
