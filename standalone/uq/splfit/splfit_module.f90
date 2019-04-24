! Fortran FITPACK wrapper
module splfit
  use euitm_schemas
  use itm_types
  implicit none

  interface
    subroutine splrep(x, y, w, xb, xe, k, t)
       use euitm_schemas
       type (type_equilibrium), pointer ::  eq_in(:), eq(:)
       type (type_param) :: code_parameters
    end subroutine splrep
  end interface

contains

  ! given the set of data points (x(i),y(i)) and the set of positive
  ! numbers w(i), subroutine splrep determines a smooth spline
  ! approximation of degree k on the interval xb <= x <= xe.
  subroutine splrep(x, y, w, xb, xe, k, t, c)

    use iso_c_binding

    implicit none
    real(R8), dimension(:), intent(in) :: x, y
    real(R8), optional, dimension(:),intent(in) :: w
    real(R8), optional, intent(in) :: xb, xe
    integer, optional,intent(in) :: k
    real(R8), dimension(:),intent(inout) :: t(:)
    real(R8), dimension(:),intent(out) :: c(:)
    
    real(R8), dimension(:), allocatable:: wrk
    integer,  dimension(:), allocatable:: iwrk

    integer :: iopt, m, nest, n, lwrk
    real(R8) :: s, fp, ier
    
    m = size(x, 1)  
    !check if size(y) == m 
    
    ! weights 
    if (present(w)) then
      !check if size(w) == m 
    else
      allocate(w(1))
      w(:) = 1.0
    end if
    
    ! Endpoints 
    if (.not.present(xb)) then
      xb = x(1)
    end if
    if (.not.present(xe)) then
      xb = x(m)
    end if
    
    ! Spline degree
    if (present(k)) then
      ! For Fitpack call, check if 1<=k<=5
    else
      ! cubic spline by default
      k = 3
    end if

    ! Knots
    if (present(t)) then
      ! Knots must be given for task=-1
      iopt = -1
      numknots = len(t)
      tf = empty((numknots + 2*k + 2,), float)
      _curfit_cache['t'][k+1:-k-1] = tf

    else
      iopt = 0
      nest = max(m + k + 1, 2*k + 3)
    end if


    wrk = empty((m*(k + 1) + nest*(7 + 3*k),), float)
    iwrk = empty((nest,), np.intc)
    
    call curfit(iopt,m,x,y,w,xb,xe,k,s,nest,n,t,c,fp,wrk,lwrk,iwrk,ier)

    print *,"return from splrep"
    
  end subroutine splrep
  ! ...

end module splfit
