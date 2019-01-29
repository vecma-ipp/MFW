!-----------------------------------------------------------------------
! GTS transport solver (1D stiff convection-diffusion-reaction solver)
!
! - using cubic Hermite finite elements
! - hyper diffusivity as stabilisation
! - Crank-Nicholson or Gears scheme as time evolution scheme
!
! Author  : Guido Huysmans (Association Euratom-CEA)
! version : 0.95
! Date    : 21-5-2009
!-----------------------------------------------------------------------
module GTS_parameters
  implicit none
  integer :: neq, norder, nvar, gts_status       ! neq is the number of equations
  parameter (norder=3)                           ! the order of the finite elements
  parameter (nvar=2)                             ! the number of variables per node
endmodule

module GTS_matrix
  implicit none
  real*8, ALLOCATABLE :: AA(:,:)                 ! fem matrix 
  real*8, ALLOCATABLE :: BB(:)                   ! right hand side
  integer             :: LDA, N, KL, KU          ! LDA    : the leading dimension of matrix A
endmodule                                        ! N      : the dimension of A and B
                                                 ! KL,KU  : the number of non-zero diagonals below,above the main diagonal
module GTS_Grid
  implicit none
  real*8, ALLOCATABLE :: x_grid(:,:),xg(:)       ! the positions of the gridpoints and gaussian points
  integer             :: n_grid                  ! the number of grid points
endmodule

module GTS_Gauss                                 ! contains the Gaussain quadrature points and weights
  implicit none
  integer,parameter :: Ngauss = 4                ! the order of the integration
  real*8,parameter  :: Xgauss(Ngauss) = (/ -.861136311594053D0, -.339981043584856D0, .339981043584856D0, .861136311594053D0 /)
  real*8,parameter  :: Wgauss(Ngauss) = (/  .347854845137454D0,  .652145154862546D0, .652145154862546D0, .347854845137454D0 /)
endmodule 

module GTS_values
  implicit none
  real*8, allocatable :: values(:), deltas(:)      ! the state vector
endmodule

module GTS_schema                                 ! parameters for the time integration scheme
  implicit none
  real*8 :: theta, zeta, gamma, hyper
endmodule

module GTS_coefficients                           ! the coefficients of the equations
  implicit none
  real*8, allocatable :: mass(:,:,:), diff(:,:,:), conv(:,:,:), reac(:,:,:), source(:,:), hyper2(:,:,:)
endmodule


subroutine solution6(solver,ifail)
!-----------------------------------------------------------------------
! main GTS routine advancing the variables by n_steps time steps
! - using single step Gears scheme (requires values from previous time step)
! - or two step step TRBDF2 scheme 
!-----------------------------------------------------------------------
use type_solver

use GTS_parameters
use GTS_Gauss
use GTS_Grid
use GTS_Matrix
use GTS_values
use GTS_schema
use GTS_coefficients

implicit none

type (numerics)   :: solver
integer           :: ifail, n_steps, n_in, istep, i, j, ia
real*8            :: timestep, time, timestep1, timestep2
real*8            :: dXds, bnd_err1, bnd_err2

!write(*,*) '*********************************'
!write(*,*) '*           GTS solver          *'
!write(*,*) '*********************************'

neq  = solver%ndim
n_in = solver%nrho

time     = 0.0d0                        ! DPC addition
timestep = 1.d0
n_steps  = 1
n_grid   = n_in
hyper    = 0.d-5 * solver%h
gamma    = 0.d0
theta    = 0.5d0
zeta     = 0.d0

if (.not. allocated(values)) allocate(values(n_grid * nvar * neq))          ! allocate state vector

if (.not. allocated(deltas)) then
   allocate(deltas(n_grid * nvar * neq))                                    ! allocate deltas previous step 
   deltas = 0.d0
endif

!do i=1,solver%nrho
!  write(*,*) solver%y(1,i),solver%ym(1,i),solver%y(1,i)-solver%ym(1,i)
!enddo


call GTS_Construct_Grid(solver%nrho,solver%rho)                             ! initialise the grid

call GTS_initialise_profiles(solver%nrho, solver%rho, solver%y, solver%ym)  ! copy input to state vector

call GTS_initialise_coefficients(solver%nrho, solver%rho, solver%a, solver%b, solver%c, solver%d, &
                                                          solver%e, solver%f, solver%g, solver%h) 

                                                          
do istep=1, n_steps                                                         ! time stepping

  call GTS_Construct_matrix(solver%u, solver%v, solver%w, time, timestep)   ! construct the FE matrix

  call GTS_Solve_matrix                                                     ! solve the system of equations

  values = values + bb                                                      ! add solution to state vector
  deltas = bb                                                               ! keep change in values 

  time = time + timestep 

enddo

do i=1,n_grid
  dXds = x_grid(2,i)  
  do j=1,neq                                             ! copy state vector to output    
    ia = 2*neq*(i-1)+j
    solver%ym(j,i) = solver%y(j,i)                       ! copy to previous timestep
    solver%y(j,i)  = values(ia)
    solver%dy(j,i) = values(ia+neq) / dXds
  enddo
enddo

if (allocated(values)) deallocate(values)   ! deallocate state vector
if (allocated(deltas)) deallocate(deltas)   ! deallocate state vector
if (allocated(mass))   deallocate(mass)
if (allocated(diff))   deallocate(diff)
if (allocated(reac))   deallocate(reac)
if (allocated(conv))   deallocate(conv)
if (allocated(source)) deallocate(source)
if (allocated(hyper2)) deallocate(hyper2)
if (allocated(AA))     deallocate(AA)
if (allocated(BB))     deallocate(BB)
if (allocated(x_grid)) deallocate(x_grid)
if (allocated(xg))     deallocate(xg)

return

contains

!************************************************************************
  REAL*8 FUNCTION SPWERT(N,XWERT,A,B,C,D,X,ABLTG)
!-----------------------------------------------------------------------
!     INPUT:
!
!     N           NUMBER OF GRID POINTS
!     XWERT       STELLE AN DER FUNKTIONSWERTE BERECHNET WERDEN
!    A, B, C, D  ARRAYS DER SPLINEKOEFFIZIENTEN (AUS SPLINE)
!     X           ARRAY DER KNOTENPUNKTE
!
!     OUTPUT:
!
!     SPWERT   FUNKTIONSWERT AN DER STELLE XWERT
!     ABLTG(I) I=1 : FIRST DERIVATIVE, ETC.
!-----------------------------------------------------------------------

    IMPLICIT NONE
    INTEGER  N
    REAL*8   XWERT, A(N), B(N), C(N), D(N), X(N), ABLTG(3), XX
    INTEGER  I, K, M

!     SUCHE PASSENDES INTERVALL (BINAERE SUCHE)

    I = 1
    K = N

10  M = (I+K) / 2

    IF(M.NE.I) THEN
       IF(XWERT.LT.X(M)) THEN
          K = M
       ELSE
          I = M
       ENDIF
       GOTO 10
    ENDIF

    XX = XWERT - X(I)

    ABLTG(1) = (3.0D0 * D(I) * XX + 2.0D0 * C(I)) * XX + B(I)
    ABLTG(2) =  6.0D0 * D(I) * XX + 2.0D0 * C(I)
    ABLTG(3) =  6.0D0 * D(I)

    SPWERT = ((D(I)*XX + C(I))*XX + B(I))*XX + A(I)

    RETURN
  END FUNCTION SPWERT
      
subroutine GTS_initialise_coefficients(n_in, x_in, gts_a, gts_b, gts_c, gts_d, gts_e, gts_f, gts_g, gts_h)
!-----------------------------------------------------------------------------
! subroutine to initialise the coeffients of the equations on the 
! Gaussian points
!-----------------------------------------------------------------------------
use GTS_parameters
use GTS_grid
use GTS_coefficients
use GTS_Gauss
use GTS_schema
implicit none
real*8            :: gts_a(neq,n_in), gts_b(neq,n_in), gts_c(neq,n_in), gts_d(neq,n_in)
real*8            :: gts_e(neq,n_in), gts_f(neq,n_in), gts_g(neq,n_in), gts_h
real*8            :: x_in(n_in), c_in(n_in), spline_a(n_in), spline_b(n_in), spline_c(n_in), spline_d(n_in)
real*8            :: derivs(3), scale
integer           :: n_in, index, in, j, m, npoints, i
!!!real*8            :: spwert

Npoints = Ngauss * (n_grid-1)

if (.not. allocated(mass))   allocate(mass(npoints,neq,neq))
if (.not. allocated(diff))   allocate(diff(npoints,neq,neq))
if (.not. allocated(reac))   allocate(reac(npoints,neq,neq))
if (.not. allocated(conv))   allocate(conv(npoints,neq,neq))
if (.not. allocated(source)) allocate(source(npoints,neq))
if (.not. allocated(hyper2)) allocate(hyper2(npoints,neq,neq))

mass = 0.d0
diff = 0.d0
conv = 0.d0
reac = 0.d0
source = 0.d0
hyper2 = 0.d0

do in=1,neq

  c_in(1:n_in) = gts_b(in,1:n_in) * gts_c(in,1:n_in)        ! mass coefficient

  scale = maxval(c_in)

  c_in = c_in / scale

  call spline(n_in,x_in,c_in,0.0d0,0.0d0,2,spline_a,spline_b,spline_c,spline_d)   

  do j=1, n_grid-1
    do m=1, Ngauss
      index = Ngauss*(j-1) + m
      mass(index,in,in) = SPWERT(n_in,xg(index),spline_a,spline_b,spline_c,spline_d,x_in,derivs)
    enddo
  enddo
    
  c_in(1:n_in) = gts_d(in,1:n_in) * gts_h                      ! diffusion coefficient
  
  c_in = c_in / scale

  call spline(n_in,x_in,c_in,0.0d0,0.0d0,2,spline_a,spline_b,spline_c,spline_d)   

  do j=1, n_grid-1
    do m=1, Ngauss
      index = Ngauss*(j-1) + m
      diff(index,in,in)   = SPWERT(n_in,xg(index),spline_a,spline_b,spline_c,spline_d,x_in,derivs)
      hyper2(index,in,in) = 0.001d0 * gts_h * derivs(2)**2
    enddo
  enddo

  c_in(1:n_in) = gts_e(in,1:n_in) * gts_h                      ! convection coefficient
   
  c_in = c_in / scale

  call spline(n_in,x_in,c_in,0.0d0,0.0d0,2,spline_a,spline_b,spline_c,spline_d)   

  do j=1, n_grid-1
    do m=1, Ngauss
      index = Ngauss*(j-1) + m
      conv(index,in,in)   = SPWERT(n_in,xg(index),spline_a,spline_b,spline_c,spline_d,x_in,derivs)
    enddo
  enddo
  
  c_in(1:n_in) = gts_c(in,1:n_in) * (gts_b(in,1:n_in) - gts_a(in,1:n_in) - gts_g(in,1:n_in) * gts_h )   ! reaction coefficient

  c_in = c_in / scale
  
  call spline(n_in,x_in,c_in,0.0d0,0.0d0,2,spline_a,spline_b,spline_c,spline_d)   

  do j=1, n_grid-1
    do m=1, Ngauss
      index = Ngauss*(j-1) + m
      reac(index,in,in) = SPWERT(n_in,xg(index),spline_a,spline_b,spline_c,spline_d,x_in,derivs)
    enddo
  enddo


  c_in(1:n_in) = gts_c(in,1:n_in) * gts_f(in,1:n_in) * gts_h    ! source coefficient
  
  c_in = c_in / scale

  call spline(n_in,x_in,c_in,0.0d0,0.0d0,2,spline_a,spline_b,spline_c,spline_d)   

  do j=1, n_grid-1
    do m=1, Ngauss
      index = Ngauss*(j-1) + m
      source(index, in) = SPWERT(n_in,xg(index),spline_a,spline_b,spline_c,spline_d,x_in,derivs)
    enddo
  enddo
 
enddo

return
end subroutine GTS_initialise_coefficients

subroutine GTS_initialise_profiles(n_in, x_in, y_in, ym_in)
!------------------------------------------------------------------------------
! subroutine to initialise the state vector
!------------------------------------------------------------------------------
use GTS_parameters
use GTS_grid
use GTS_values
implicit none
integer           :: n_in, i, j, ia
real*8            :: x_in(n_in), y_in(neq,n_in), ym_in(neq,n_in), delta(n_in)
real*8            :: y_a(n_in), y_b(n_in), y_c(n_in), y_d(n_in)
real*8            :: d_a(n_in), d_b(n_in), d_c(n_in), d_d(n_in)
real*8            :: derivs(3), dXds
!!!real*8            :: spwert

!------------------------------- convert to finite element grid and fill in derivatives
do j=1,neq
  
  call spline(n_in,x_in,y_in(j,1:n_in),0.0d0,0.0d0,2,y_a,y_b,y_c,y_d)   

  delta = y_in(j,:) - ym_in(j,:)
  call spline(n_in,x_in,delta,0.0d0,0.0d0,2,d_a,d_b,d_c,d_d)   
    
  do i=1,n_grid
     
    dXds = x_grid(2,i)
    
    ia = 2*neq*(i-1)+j

    values(ia)     = SPWERT(n_in,x_grid(1,i),y_a,y_b,y_c,y_d,x_in,derivs)
    values(ia+neq) = derivs(1) * dXds
    
    deltas(ia)     = SPWERT(n_in,x_grid(1,i),d_a,d_b,d_c,d_d,x_in,derivs)
    deltas(ia+neq) = derivs(1) * dXds
 
  enddo

enddo

return
end subroutine GTS_initialise_profiles

subroutine GTS_Element_Matrix(ife, time, timestep, ELM, RHS )
!----------------------------------------------------------
! subroutine calculates the element matrix for one element
!   ife    : the element number to be calculated (input)
!   norder : the order of the finite elements (input)
!   ELM    : the element matrix  (output)
!   RHS    : the right hand side (output)
!----------------------------------------------------------
use GTS_parameters
use GTS_Gauss
use GTS_Grid
use GTS_values
use GTS_coefficients
use GTS_schema
implicit none
real*8           :: ELM(2*nvar*neq,2*nvar*neq),RHS(2*nvar*neq)
real*8           :: FE(2*nvar), DFE(2*nvar), DDFE(2*nvar)
real*8           :: time, timestep, eq0(neq), deq0(neq), ddeq0(neq), delta_eq0(neq)
real*8           :: Xjac, dXdS, xright, xleft
real*8           :: sm, wm, xm
real*8           :: hyper_n(neq,neq)
real*8           :: V, DV, DDV
integer          :: mgauss, ia, ja, i, j, ife, m, k, l, in, jn, index

ELM = 0.d0
RHS = 0.d0

hyper_n = 0.d0
do in=1,neq
  hyper_n(in,in) = hyper
enddo

mgauss = Ngauss                                             ! the order of the Gaussian integration

do m=1,mgauss                                               ! loop over order of integration

  sm = Xgauss(m)                                            ! the gaussian integration points ( -1.< sm <1. )
  wm = Wgauss(m)                                            ! the weights

  call GTS_Finite_Elements(sm,FE,DFE,DDFE)                  ! the finite elements and its derivative

  index = (ife-1)*Ngauss + m
  
  xm   = x_grid(1,ife)   *  FE(1) + x_grid(2,ife)   *  FE(2) &  ! the value of the coordinate x at the gaussian point
       + x_grid(1,ife+1) *  FE(3) + x_grid(2,ife+1) *  FE(4)
  dXds = x_grid(1,ife)   * dFE(1) + x_grid(2,ife)   * dFE(2) &  ! scaling factor for derivatives in the master element
       + x_grid(1,ife+1) * dFE(3) + x_grid(2,ife+1) * dFE(4)
  
  Xjac = dXds                                               ! the jacobian 


  eq0   = 0.d0                                                ! the value in between the nodes is the sum of contributions from two nodes
  deq0  = 0.d0                                                ! the derivative
  ddeq0 = 0.d0                                                ! second derivative
  delta_eq0 =0.d0                                             ! value of delta previous timestep
 
  do in=1, neq                                              ! reconstructed values of all variables
    do k=1,2*nvar
      ia       = (ife-1) * nvar * neq + (k-1)*neq + in 
      eq0(in)       = eq0(in)       + values(ia) * FE(k)
      deq0(in)      = deq0(in)      + values(ia) * DFE(k)  / dXdS
      ddeq0(in)     = ddeq0(in)     + values(ia) * DDFE(k) / dXdS**2      ! careful only for linear X(S)
      delta_eq0(in) = delta_eq0(in) + deltas(ia) * FE(k)
    enddo
  enddo
   
  do in=1,neq
  
    do i=1,2*nvar

      ia = (i-1)*neq + in

      V   = FE(i)               
      DV  = DFE(i)  / dXds   
      DDV = DDFE(i) / dXds**2       ! careful only for linear X(S)
    
      do jn=1,neq

        do j=1,2*nvar

	  ja = (j-1)*neq + jn

          ELM(ia,ja) = ELM(ia,ja) + wm * Xjac * ( mass(index,in,jn) * V * FE(j) * (1.d0 + zeta) &
                                                + Diff(index,in,jn)  * theta * timestep  * DV * DFE(j) / dXdS  &
                                                - Conv(index,in,jn)  * theta * timestep  * DV *  FE(j)         &
                                                - Reac(index,in,jn)  * theta * timestep  * V  *  FE(j)       ) &

                             + wm * xm * Xjac * theta * timestep * hyper_n(in,jn) * DDV * DDFE(j)/dXds**2

!                            + wm * xm * Xjac * theta * timestep * hyper2(index,in,jn) * DDV * DDFE(j)/dXds**2
        enddo

      enddo

      RHS(ia) = RHS(ia) + wm * Xjac * timestep * (                       &
                                    - Diff(index,in,in) * deq0(in) * DV  &
                                    + Conv(index,in,in) *  eq0(in) * DV  &
                                    + Reac(index,in,in) *  eq0(in) * V   &
                                    + Source(index,in)             * V ) &
                        - wm * xm * Xjac * timestep * hyper_n(in,in) * ddeq0(in) * DDV &

!                        - wm * xm * Xjac * timestep * hyper2(index,in,in) * ddeq0(in) * DDV &

                        + wm * Xjac * V * zeta * mass(index,in,in) * delta_eq0(in)
    
    enddo

  enddo

enddo

return
end subroutine GTS_Element_Matrix


subroutine GTS_Construct_Grid(n_in,x_in)
!--------------------------------------------------
! subroutine to define the grid
!--------------------------------------------------
use GTS_parameters
use GTS_grid
use GTS_Gauss

implicit none
real*8            :: x_in(n_in)
integer           :: n_in, i, m, index,k, l
real*8            :: s_in(n_in), spline_a(n_in), spline_b(n_in), spline_c(n_in), spline_d(n_in)
real*8            :: FE(2*nvar), dFE(2*nvar), ddFE(2*nvar)
real*8            :: s_out, derivs(3),x_tmp,dxds
!!! real*8            :: spwert

!n_grid = n_in

if (.not. allocated(x_grid)) allocate(x_grid(2,n_grid))
if (.not. allocated(xg))     allocate(xg(Ngauss*(n_grid-1)))

do i=1,n_in                         ! the global equidistant coordinate
  s_in(i) = float(i-1)/float(n_in-1)
enddo

call spline(n_in,s_in,x_in,0.0d0,0.0d0,2,spline_a,spline_b,spline_c,spline_d)   

x_grid(1,1:n_grid) = x_in(1:n_in)

do i=1, n_grid
  s_out = float(i-1)/float(n_grid-1)
  x_tmp = SPWERT(n_in,s_out,spline_a,spline_b,spline_c,spline_d,s_in,derivs)
  x_grid(2,i) = derivs(1) / (2.d0 * float(n_grid-1))
enddo

xg = 0.d0                                             ! grid at the gaussian points 

do i=1, n_grid-1                                      ! definition of Gaussian points

  do m=1,Ngauss
  
    call GTS_Finite_Elements(xgauss(m),FE,DFE,DDFE)   ! the finite elements and its derivative

    index = Ngauss*(i-1) + m

    xg(index) = x_grid(1,i) * FE(1) + x_grid(2,i) * FE(2) + x_grid(1,i+1) * FE(3) + x_grid(2,i+1) * FE(4)
    
  enddo

enddo

return
end subroutine GTS_Construct_Grid


subroutine GTS_Construct_Matrix(u,v,w,time,timestep)
!------------------------------------------------------------------------
! subroutine to assemble the fem matrices and apply boundary conditions
!  u(i,1)*dy(i,1) + v(i,1)*y(i,1) = w(i,1)  (i=1,neq) 
!  u(i,2)*dy(i,2) + v(i,2)*y(i,2) = w(i,2)  (i=1,neq) 
!------------------------------------------------------------------------
use GTS_parameters
use GTS_Gauss
use GTS_Grid
use GTS_Matrix
use GTS_values
implicit none
real*8   :: ELM(2*nvar*neq,2*nvar*neq),RHS(2*nvar*neq) ! the element matrix and right hand side
real*8   :: u(neq,2), v(neq,2), w(neq,2), time, timestep, zbig
integer  :: Noff, nfe, ife, i, ia, j, ja, in


KU    = 1 + 2*(neq*nvar-1)                      ! the number of diagonals above the main diagonal
KL    = KU                                      ! the number of diagonals below the main diagonal
LDA   = 2*KL+KU+1                               ! the leading dimension
Noff  = KL+KU+1                                 ! the offset for band storage

N = n_grid * nvar * neq                         ! the number of variables

if (.not. allocated(AA)) ALLOCATE(AA(LDA,N))    ! allocate a band matrix
if (.not. allocated(BB)) ALLOCATE(BB(N))        ! allocate the righthandside vector

AA = 0.d0                                         ! initialise to zero
BB = 0.d0

nfe = n_grid - 1                                ! the number of finite elements

do ife=1,nfe                                    ! collect contributions from all elements

  call GTS_Element_Matrix(ife,time,timestep,ELM,RHS)! calculate the element matrix

  do i=1,2*nvar*neq                             ! add element matrix to main matrix

    ia = (ife-1) * nvar * neq + i 
    
    do j=1,2*nvar*neq

      ja = (ife-1) * nvar * neq + j 

      AA(Noff+ia-ja,ja) = AA(Noff+ia-ja,ja) + ELM(i,j)

    enddo

    BB(ia) = BB(ia) + RHS(i)
  
  enddo             

enddo                                           ! end of loop over elements

!---------------------- boundary conditions, penalty 'method' : v*dy + u*y = w
zbig=1.d20
do in=1,neq
  ia = neq + in 
  ja = neq + in
  AA(Noff+ia-ja,ja) = v(in,1) * zbig                                 / x_grid(2,1)
  BB(ia)            = w(in,1) * zbig - v(in,1) * values(ja) * zbig   / x_grid(2,1)

  ja = in
  AA(Noff+ia-ja,ja) = u(in,1) * zbig
  BB(ia)            = BB(ia) - u(in,1) * values(ja) * zbig

enddo

do in=1,neq
  ia = (n_grid-1) * nvar * neq + in 
  ja = (n_grid-1) * nvar * neq + in

  AA(Noff+ia-ja,ja) = u(in,2) * zbig
  BB(ia)            = w(in,2) * zbig - u(in,2) * values(ja) * zbig

  ja = (n_grid-1) * nvar * neq + in + neq
  AA(Noff+ia-ja,ja) = v(in,2) * zbig                         / x_grid(2,n_grid)
  BB(ia)            = BB(ia) - v(in,2) * values(ja) * zbig   / x_grid(2,n_grid)
  
enddo

return
end subroutine GTS_Construct_Matrix


subroutine GTS_Finite_Elements(xm,FE,DFE,DDFE)
!--------------------------------------------------
! the finite elements in the master element
! defined between -1 and +1
!--------------------------------------------------
implicit none
real*8 :: FE(*),DFE(*),DDFE(*), xm

  FE(1) = -0.25D0*(xm - 1.D0)**2 * (-xm - 2.D0); DFE(1) = -0.5D0*(xm - 1.D0) * (-xm - 2.D0) + 0.25D0*(xm - 1.D0)**2
  FE(3) = -0.25D0*(xm + 1.D0)**2 * ( xm - 2.D0); DFE(3) = -0.5D0*(xm + 1.D0) * ( xm - 2.D0) - 0.25D0*(xm + 1.D0)**2
  FE(2) = -0.25D0*(xm - 1.D0)**2 * (-xm - 1.D0); DFE(2) = -0.5D0*(xm - 1.D0) * (-xm - 1.D0) + 0.25D0*(xm - 1.D0)**2
  FE(4) = +0.25D0*(xm + 1.D0)**2 * ( xm - 1.D0); DFE(4) = +0.5D0*(xm + 1.D0) * ( xm - 1.D0) + 0.25D0*(xm + 1.D0)**2

  DDFE(1) = -0.5D0*(-xm - 2.D0) + 0.5D0*(xm - 1.D0) + 0.5D0*(xm - 1.D0)
  DDFE(3) = -0.5D0*( xm - 2.D0) - 0.5D0*(xm + 1.D0) - 0.5D0*(xm + 1.D0)
  DDFE(2) = -0.5D0*(-xm - 1.D0) + 0.5D0*(xm - 1.D0) + 0.5D0*(xm - 1.D0)
  DDFE(4) = +0.5D0*( xm - 1.D0) + 0.5D0*(xm + 1.D0) + 0.5D0*(xm + 1.D0)
 
return
end subroutine GTS_Finite_Elements

subroutine GTS_Solve_Matrix
!--------------------------------------------------
! Solves the banded system of equations
! (for general up-down asymmetric matrices)
! using Lapack routines dgbtrf/dgbtrs
!--------------------------------------------------
use GTS_Matrix
implicit none
integer, allocatable :: ipiv(:),iwork(:)
real*8,  allocatable :: work(:)
real*8               :: info

allocate(ipiv(N), work(3*N), iwork(N))

call dgbtrf(N,N,KL,KU,AA,LDA,ipiv,info)                           ! lapack routine for LU decomposition

call dgbtrs('N',N,KL,KU,1,AA,LDA,ipiv,BB,N,info)                  ! lapack solve equations (back substitution)

deallocate(ipiv, work, iwork)

return
end subroutine GTS_Solve_Matrix


!***********************************************************************
      SUBROUTINE SPLINE(N,X,Y,ALFA,BETA,TYP,A,B,C,D)
!-----------------------------------------------------------------------
!     INPUT:
!
!     N     NUMBER OF POINTS
!     X     ARRAY X VECTOR
!     Y     ARRAY Y VECTOR
!     ALFA  BOUNDARY CONDITION IN X(1)
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
!     A, B, C, D     ARRAYS OF SPLINE COEFFICIENTS
!       S = A(I) + B(I)*(X-X(I)) + C(I)*(X-X(I))**2+ D(I)*(X-X(I))**3
!
!     BEI ANWENDUNGSFEHLERN WIRD DAS PROGRAMM MIT ENTSPRECHENDER
!     FEHLERMELDUNG ABGEBROCHEN
!-----------------------------------------------------------------------
!
!
      IMPLICIT NONE
      
      INTEGER  N, TYP
      REAL*8   X(N), Y(N), ALFA, BETA, A(N), B(N), C(N), D(N)
      INTEGER  I, IERR
      REAL*8   H(N)

      IF((TYP.LT.0).OR.(TYP.GT.3)) THEN
         WRITE(*,*) 'ERROR IN ROUTINE SPLINE: FALSE TYP'
         STOP
      ENDIF

      IF (N.LT.3) THEN
         WRITE(*,*) 'ERROR IN ROUTINE  SPLINE: N < 3'
         STOP
      ENDIF


!     BERECHNE DIFFERENZ AUFEINENDERFOLGENDER X-WERTE UND
!     UNTERSUCHE MONOTONIE
!
      DO I = 1, N-1
         H(I) = X(I+1)- X(I)
         IF(H(I).LE.0.0D0) THEN
            WRITE(*,*) 'NON MONOTONIC ABCISSAE IN SPLINE: X(I-1)>=X(I)'
            STOP
         ENDIF
      ENDDO
!
!     AUFSTELLEN DES GLEICHUNGSSYSTEMS
!
      DO 20 I = 1, N-2
         A(I) = 3.0D0 * ((Y(I+2)-Y(I+1)) / H(I+1) - (Y(I+1)-Y(I)) / H(I))
         B(I) = H(I)
         C(I) = H(I+1)
         D(I) = 2.0D0 * (H(I) + H(I+1))
   20 CONTINUE
!
!     BERUECKSICHTIGEN DER RANDBEDINGUNGEN

!     NOT-A-KNOT

      IF(TYP.EQ.0) THEN
         A(1)   = A(1) * H(2) / (H(1) + H(2))
         A(N-2) = A(N-2) * H(N-2) / (H(N-1) + H(N-2))
         D(1)   = D(1) - H(1)
         D(N-2) = D(N-2) - H(N-1)
         C(1)   = C(1) - H(1)
         B(N-2) = B(N-2) - H(N-1)
      ENDIF

!     1. ABLEITUNG VORGEGEBEN

      IF(TYP.EQ.1) THEN
         A(1)   = A(1)   - 1.5D0 * ((Y(2)-Y(1)) / H(1) - ALFA)
         A(N-2) = A(N-2) - 1.5D0 * (BETA - (Y(N)-Y(N-1)) / H(N-1))
         D(1)   = D(1)   - 0.5D0 * H(1)
         D(N-2) = D(N-2) - 0.5D0 * H(N-1)
      ENDIF
!
!     2. ABLEITUNG VORGEGEBEN
!
      IF(TYP.EQ.2) THEN
         A(1)   = A(1)   - 0.5D0 * ALFA * H(1)
         A(N-2) = A(N-2) - 0.5D0 * BETA * H(N-1)
      ENDIF

!     3. ABLEITUNG VORGEGEBEN
!
      IF(TYP.EQ.3 ) THEN
         A(1)   = A(1)   + 0.5D0 * ALFA * H(1) * H(1)
         A(N-2) = A(N-2) - 0.5D0 * BETA * H(N-1)* H(N-1)
         D(1)   = D(1) + H(1)
         D(N-2) = D(N-2) + H(N-1)
      ENDIF

!     BERECHNUNG DER KOEFFIZIENTEN
!
      CALL DGTSL(N-2,B,D,C,A,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,21)
         STOP
      ENDIF

!     UEBERSCHREIBEN DES LOESUNGSVEKTORS

      CALL DCOPY(N-2,A,1,C(2),1)
!
!     IN ABHAENGIGKEIT VON DEN RANDBEDINGUNGEN WIRD DER 1. UND
!     DER LETZTE WERT VON C KORRIGIERT
!
      IF(TYP.EQ.0) THEN
         C(1) = C(2) + H(1) * (C(2)-C(3)) / H(2)
         C(N) = C(N-1) + H(N-1) * (C(N-1)-C(N-2)) / H(N-2)
      ENDIF

      IF(TYP.EQ.1) THEN
         C(1) =  1.5D0*((Y(2)-Y(1))   / H(1)  - ALFA) / H(1)  - 0.5D0 * C(2)
         C(N) = -1.5D0*((Y(N)-Y(N-1)) / H(N-1)- BETA) / H(N-1)- 0.5D0 * C(N-1)
      ENDIF

      IF(TYP.EQ.2) THEN
         C(1) = 0.5D0 * ALFA
         C(N) = 0.5D0 * BETA
      ENDIF

      IF(TYP.EQ.3) THEN
         C(1) = C(2)   - 0.5D0 * ALFA * H(1)
         C(N) = C(N-1) + 0.5D0 * BETA * H(N-1)
      ENDIF

      CALL DCOPY(N,Y,1,A,1)

      DO I = 1, N-1
         B(I) = (A(I+1)-A(I)) / H(I) - H(I) * (C(I+1)+2.0D0 * C(I)) / 3.0D0
         D(I) = (C(I+1)-C(I)) / (3.0D0 * H(I))
      END DO

      B(N) = (3.0D0 * D(N-1) * H(N-1) + 2.0D0 * C(N-1)) * H(N-1) + B(N-1)

      RETURN

   21 FORMAT(1X,'ERROR IN SGTSL: MATRIX SINGULAR')
      END SUBROUTINE SPLINE


!DECK DGTSL                                                             CAS02750
!** FROM NETLIB, TUE AUG 28 08:28:34 EDT 1990 ***                               
!** COPIED FROM SGTSL AND RENAMED             ***
!                                                                               
      SUBROUTINE DGTSL(N,C,D,E,B,INFO)                
      IMPLICIT NONE
      INTEGER N,INFO                                                            
      REAL*8 C(*),D(*),E(*),B(*)                                                  
                                                                              
!     SGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND                 
!     SIDE WILL FIND THE SOLUTION.                                              
!                                                                               
!     ON ENTRY                                                                  
!                                                                               
!        N       INTEGER                                                        
!                IS THE ORDER OF THE TRIDIAGONAL MATRIX.                        
!
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
      REAL*8 T                                                                    
!     BEGIN BLOCK PERMITTING ...EXITS TO 100                                    
                                                                               
         INFO = 0                                                               
         C(1) = D(1)                                                            
         NM1 = N - 1                                                            
         IF (NM1 .LT. 1) GO TO 40                                               
            D(1) = E(1)                                                         
            E(1) = 0.0D0                                                        
            E(N) = 0.0D0                                                        
                                                                               
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
                                                                               
               IF (C(K) .NE. 0.0D0) GO TO 20                                    
                  INFO = K                                                      
!     ............EXIT                                                          
                  GO TO 100                                                     
   20          CONTINUE                                                         
               T = -C(KP1)/C(K)                                                 
               C(KP1) = D(KP1) + T*D(K)                                         
               D(KP1) = E(KP1) + T*E(K)                                         
               E(KP1) = 0.0D0                                                   
               B(KP1) = B(KP1) + T*B(K)                                         
   30       CONTINUE                                                            
   40    CONTINUE                                                               
         IF (C(N) .NE. 0.0D0) GO TO 50                                          
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
      END SUBROUTINE DGTSL   

end subroutine solution6
