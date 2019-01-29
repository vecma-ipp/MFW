MODULE CGScr

  IMPLICIT NONE

  TYPE FluxSurfaceM
     REAL, DIMENSION(:), POINTER :: u,p,q,r
     REAL, DIMENSION(:,:), POINTER :: aa,uucs
  END TYPE FluxSurfaceM

  TYPE (FluxSurfaceM), DIMENSION(:), ALLOCATABLE, SAVE :: fsfcm

  INTEGER :: nsubstep = 500
  INTEGER :: nxpiter = 500, ier_cg2d = 0
  REAL :: eps_cg2d = 1.0d-10

  INTEGER :: idiags = 10
  LOGICAL :: write_diags = .false.

END MODULE CGScr


SUBROUTINE Mg2d

!...  pure 2D solve

!...  2-D Helmholtz equation
!...  pure CG method, one iteration only

!...  equidistant an-isotropic grid

!...  soln vectors defined in fsfc in vars
!...  on input, uux is the source, uuc is the guess
!...  on output, uuc becomes the solution
!...  not parallelised

  USE CGScr
  USE Vars

  IMPLICIT NONE

  LOGICAL, SAVE :: first_flag = .true.

  IF (write_diags) then
     if (first_flag) then
        OPEN (idiags,file='cgdiags.txt',form='formatted')
     else
        OPEN (idiags,file='cgdiags.txt',form='formatted',position='append')
     end if
  end if

!...  set up and solve matrices

  CALL CGLoad
  IF (first_flag) THEN
     CALL CGSolv(nxpiter)
     first_flag = .false.
  ELSE
     CALL CGSolv(nsubstep)
  END IF

  IF (write_diags) CLOSE(idiags)

END SUBROUTINE Mg2d


SUBROUTINE CGLoad

!...  perp laplacian with a multiplier and a diagonal element
!...  CG method
!...  do the load and solve separately

!...  not parallelised

  USE CGScr
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j
  INTEGER :: i0,j0,j1,j2

  LOGICAL, SAVE :: first_flag = .true.

!...  allocate matrices

  IF (first_flag) THEN
     ALLOCATE(fsfcm(nx))

     DO i=1,nx
        j=fsfc(i)%ny

        ALLOCATE(fsfcm(i)%u(j))
        ALLOCATE(fsfcm(i)%q(j))
        ALLOCATE(fsfcm(i)%p(j))
        ALLOCATE(fsfcm(i)%r(j))

        ALLOCATE(fsfcm(i)%uucs(0:n_neighbors,j))
        ALLOCATE(fsfcm(i)%aa(0:n_neighbors,j))

        fsfcm(i)%u=0.
        fsfcm(i)%q=0.
        fsfcm(i)%p=0.
        fsfcm(i)%r=0.

        fsfcm(i)%uucs=0.
        fsfcm(i)%aa=0.
     END DO

     first_flag = .false.
  END IF

!...  load matrix, solution and initial guess 

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     DO j=j1,j2
        fsfcm(i)%aa(0,j)=fsfc(i)%diag(j)*fsfc(i)%area(j)

        fsfcm(i)%aa(1:n_neighbors,j)= &
             - 0.5*(fsfc(i)%amult(1:n_neighbors,j)+fsfc(i)%amult(0,j)) &
             *fsfc(i)%aratio(1:n_neighbors,j)

        fsfcm(i)%aa(0,j)=fsfcm(i)%aa(0,j)-SUM(fsfcm(i)%aa(1:n_neighbors,j))

        fsfcm(i)%u(j)=fsfc(i)%uux(j)*fsfc(i)%area(j)
        fsfcm(i)%p(j)=fsfc(i)%uuc(j)

     END DO
  END DO

!...  done setup

END SUBROUTINE CGLoad


SUBROUTINE CGSolv(niter)

  USE Coeff, ONLY : xzero
  USE CGScr
  USE Vars

  IMPLICIT NONE

  INTEGER :: niter

  INTEGER :: i,j,i0,j0,j1,j2
  INTEGER :: kiter
  REAL :: r2,r2new,r2norm,r2_initial,pap,cgalpha,cgbeta

!...  initialisation; u0 is guess for u; held in p

!...  the p = r = u - A u0 step

!...  do q = A p

  CALL Xpmmult

!...  do r = u - q, put initial guess into u and r into p

  DO i=1,nx
     fsfcm(i)%r=fsfcm(i)%u-fsfcm(i)%q
     fsfcm(i)%u=fsfcm(i)%p
     fsfcm(i)%p=fsfcm(i)%r
  END DO

!...  the initial norm of r

  r2new=0.
  do i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     do j=j1,j2
        r2new=r2new+fsfcm(i)%r(j)*fsfcm(i)%r(j)
     end do
  end do

  r2norm=SQRT(ABS(r2new/REAL(nx0*nx0*3)))

  r2_initial=r2norm

!...  start the iteration

  if (write_diags) then
     write (idiags,*) ' '
     write (idiags,*) 'iteration      r2 norm'
     write (idiags,*) ' '
     write (idiags,'(i6,4x,2g20.12)') 0, r2norm
  end if

  do kiter=1,niter

!...  the q = A p step

     CALL Xpmmult

!...  the pap = p dot q step

     pap=0.
     do i=1+ngdx,nx0+ngdx
        j0=fsfc(i)%ny0
        j1=1+ngdy
        j2=j0+ngdy

        do j=j1,j2
           pap=pap+fsfcm(i)%p(j)*fsfcm(i)%q(j)
        end do
     end do

!...  save the r norm

     r2=r2new

!...  get the alpha = - (p,r)/(p,Ap)

     cgalpha=-r2/(xzero+pap)

!...  get the new u <-- u - alpha p
!...  and the new r <-- r + alpha q

     DO i=1,nx
        fsfcm(i)%u=fsfcm(i)%u-cgalpha*fsfcm(i)%p
        fsfcm(i)%r=fsfcm(i)%r+cgalpha*fsfcm(i)%q
     END DO

!...  get the new norm of r

     r2new=0.
     do i=1+ngdx,nx0+ngdx
        j0=fsfc(i)%ny0
        j1=1+ngdy
        j2=j0+ngdy

        do j=j1,j2
           r2new=r2new+fsfcm(i)%r(j)*fsfcm(i)%r(j)
        end do
     end do

!...  get the beta = (r,r)/(r0,r0)

     cgbeta=r2new/(xzero+r2)

!...  get the new p <-- r + beta p

     DO i=1,nx
        fsfcm(i)%p=fsfcm(i)%r+cgbeta*fsfcm(i)%p
     END DO

!...  end convergence loop

     r2norm=SQRT(ABS(r2new/REAL(nx0*nx0*3)))

     if (write_diags) &
          write (idiags,'(i6,4x,2g20.12)') kiter, r2norm

     if (r2norm < eps_cg2d) exit

  end do

!...  reload solution

  DO i=1,nx
     fsfc(i)%uuc=fsfcm(i)%u
  END DO

!...  do boundaries here

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     DO j=1,ngdy
        fsfc(i)%uuc(j)=fsfc(i)%uuc(j+j0)
        fsfc(i)%uuc(j2+j)=fsfc(i)%uuc(j2+j-j0)
     END DO
  END DO

  i0=1+ngdx
  DO i=1,ngdx
     DO j=1,fsfc(i)%ny
        fsfc(i)%uuc(j)=fsfc(i0)%uuc(j)
     END DO
  END DO
  i0=nx0+ngdx
  DO i=i0+1,i0+ngdx
     DO j=1,fsfc(i)%ny
        fsfc(i)%uuc(j)=0.
     END DO
  END DO

END SUBROUTINE CGSolv

SUBROUTINE Xpmmult

!...  does the matrix multiply q = A * p, exchange first

  USE CGScr
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,j0,j1,j2

  CALL Xpneighbors

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     do j=j1,j2
        fsfcm(i)%q(j)=SUM(fsfcm(i)%aa(0:n_neighbors,j) &
             *fsfcm(i)%uucs(0:n_neighbors,j))
     end do
  end do

END SUBROUTINE Xpmmult

SUBROUTINE Xpneighbors

!...  load neighbors for memory local derivatives for uuc

  USE CGScr
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,i0,i1,i2,j0,j1,j2,mu

!...  3D models do parallel direction communication

!...  boundary conditions on p

  DO i=1+ngdx,nx0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     DO j=1,ngdy
        fsfcm(i)%p(j)=fsfcm(i)%p(j+j0)
        fsfcm(i)%p(j2+j)=fsfcm(i)%p(j2+j-j0)
     END DO
  END DO

  i0=1+ngdx
  DO i=1,ngdx
     DO j=1,fsfc(i)%ny
        fsfcm(i)%p(j)=fsfcm(i0)%p(j)
     END DO
  END DO
  i0=nx0+ngdx
  DO i=i0+1,i0+ngdx
     DO j=1,fsfc(i)%ny
        fsfcm(i)%p(j)=0.
     END DO
  END DO

!...  load center point

  DO i=1,nx
     fsfcm(i)%uucs(0,:)=fsfcm(i)%p(:)
  END DO

!...  load drift plane neighbors

  i1=1
  IF (mypex == 0) THEN
     i1=1+ngdx
     i2=i1+1
     j1=1+ngdy
     DO mu=1,6
        j=mu+ngdy
        fsfcm(i1)%uucs(mu,j1)=fsfcm(i2)%uucs(0,j)
     END DO
     i1=2
  END IF

  DO i0=i1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0/6

     DO mu=0,5

        j1=mu*j0+1+ngdy
        j2=mu*j0+j0+ngdy

        DO j=j1,j2

           fsfcm(i)%uucs(1,j)=fsfcm(i)%uucs(0,j+1)
           fsfcm(i)%uucs(4,j)=fsfcm(i)%uucs(0,j-1)

           fsfcm(i)%uucs(5,j)=fsfcm(i+1)%uucs(0,j+mu)
           fsfcm(i)%uucs(6,j)=fsfcm(i+1)%uucs(0,j+1+mu)

           fsfcm(i+1)%uucs(2,j+mu)=fsfcm(i)%uucs(0,j)
           fsfcm(i+1)%uucs(3,j+1+mu)=fsfcm(i)%uucs(0,j)
        END DO
        fsfcm(i)%uucs(2,j2)=fsfcm(i-1)%uucs(0,j2-mu)
        fsfcm(i)%uucs(3,j1)=fsfcm(i)%uucs(0,j1-1)
        fsfcm(i)%uucs(4,j1)=fsfcm(i+1)%uucs(0,j1+mu-1)
!        fsfcm(i+1)%uucs(2,j2+mu+1)=fsfcm(i)%uucs(0,j2+1)
!        fsfcm(i+1)%uucs(3,j1+mu)=fsfcm(i+1)%uucs(0,j1+mu-1)
     END DO
  END DO

END SUBROUTINE Xpneighbors


