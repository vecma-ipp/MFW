SUBROUTINE Mg2d

!...  pure 2D solve

!...  2-D Helmholtz equation
!...  Watson Sparse Matrix Package version

!...  global geodesic grid

!...  soln vectors defined in Vars by fsfc
!...  on input, uux is the source, uuc is the guess
!...  on output, uuc becomes the solution
!...  not parallelised

  USE Coeff
  USE Vars

  IMPLICIT NONE

!...  counters and locals from inputs

  INTEGER :: verbosity = 0

  INTEGER :: i,j
  INTEGER :: i0,i1,i2,j0,j1,j2,l0,l1,mu
  REAL :: area,diag
  REAL, DIMENSION(:), ALLOCATABLE, SAVE :: amult

!...  wsmp stuff all to be saved including dims

  REAL, DIMENSION(:), ALLOCATABLE, SAVE :: ad
  REAL, DIMENSION(:,:), ALLOCATABLE, SAVE :: rhs

  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: lar,lac
  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: lperm,linvp
  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: mrp
  INTEGER, SAVE :: opt_wsmp(5)

  INTEGER, SAVE :: iopt=0,ier=0,info=0
  INTEGER, SAVE :: mmatrix_global,mmatrix,naux,niter,nrhs,nnzl,wspace,lda
  REAL, DIMENSION(:), ALLOCATABLE, SAVE :: waux,adiag

  LOGICAL, SAVE :: first_flag = .true.

!...  set boundary indices

  i1=1
  i2=nx0
  IF (mypex == 0) i1=2

!...  setup for first time through

  IF (first_flag) THEN

     ALLOCATE(amult(n_neighbors))

!...  special calls

  CALL wsetmaxthrds(1)
  CALL wsetnobigmal
#ifdef MPI
  CALL wsetmpicomm (commxdir)
  CALL pwsmp_initialize
#else
!  CALL wsmp_initialize
#endif

!...  allocate matrices

  l0=3*(nx00-1)*nx00

  mmatrix_global=l0+1
  mmatrix=l0+1

  lda=4*l0 + 6*(nx00-1)
  IF (mypex == 0) lda=lda+7
  IF (mypex == npesx-1) lda=lda-12*(nx00-1)-6

  nrhs=1

  ALLOCATE(ad(lda))
  ALLOCATE(rhs(mmatrix,nrhs))
  ALLOCATE(lperm(mmatrix_global))
  ALLOCATE(linvp(mmatrix_global))

  ALLOCATE(lar(mmatrix+1))
  ALLOCATE(lac(lda))

  ALLOCATE(mrp(1))

  opt_wsmp=0

!...  placeholders (they have to do more for certain options)

  naux=0
  ALLOCATE(waux(1))
  ALLOCATE(adiag(1))

!...  matrix logic and numbering

  l0=0
  l1=0

!...  the axis, first point, same as the first fsfc

  IF (mypex == 0) THEN

!...  diagonal, point 0
  l0=l0+1
  l1=l1+1
  lar(l0)=l1
  lac(l1)=l0

!...  the six points of the first full hexagon
  DO j=1,6
     l1=l1+1
     lac(l1)=l0+j
  END DO

  END IF

  DO i0=i1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0/6

!...  each of the six sides of the hex described by this fsfc
     DO mu=0,5
        j1=mu*j0+1+ngdy
        j2=mu*j0+j0+ngdy

!...  the j0 points within a side

        DO j=j1,j2
!...  diagonal, point 0
           l0=l0+1
           l1=l1+1
           lar(l0)=l1
           lac(l1)=l0

!...  neighbor 1 is always ahead along fsfc except at wraps at last point
           IF (j < j2 .OR. mu < 5) THEN
              l1=l1+1
              lac(l1)=l0+1
           END IF
!...  neighbor 2 is always behind
!...  neighbor 3 is always behind except when it wraps at first point
           IF (j == j1 .AND. mu == 0) THEN
              l1=l1+1
              lac(l1)=l0+6*j0-1
           END IF
!...  things on next fsfc drop on last active fsfc ...
           IF (i0 < nx0) THEN
!...  neighbor 4 is behind along fsfc except at begin of triangle ...
              IF (j == j1 .AND. mu > 0) THEN
                 l1=l1+1
                 lac(l1)=l0+6*j0+mu-1
              END IF
!...  neighbor 5 is always ahead
              l1=l1+1
              lac(l1)=l0+6*j0+mu
!...  neighbor 6 is always ahead
              l1=l1+1
              lac(l1)=l0+6*j0+mu+1
!...  neighbor 4 is one more circuit ahead at first point
              IF (j == j1 .AND. mu == 0) THEN
                 l1=l1+1
                 lac(l1)=l0+6*(2*j0+1)-1
              END IF
           END IF
        END DO
     END DO

  END DO

  write (0,*) mype,'(',mypex,'): dims are ',mmatrix,lda
  write (0,*) mype,'(',mypex,'): neqs is ',l0,'   n elements is ',l1

  lar(l0+1)=l1+1

!...  do in serial and save start point or get straight from global one
!  lac=lac+mmatrix*mypex

!...  ordering and symbolic

#ifdef MPI
  CALL pwscalz(mmatrix, lar, lac, opt_wsmp, lperm, linvp, nnzl, wspace, &
       waux, naux, info)
#else
  CALL wscalz(mmatrix, lar, lac, opt_wsmp, lperm, linvp, nnzl, wspace, &
       waux, naux, info)
#endif

  IF (info .NE. 0) THEN
     print *,'In setup the following ERROR was detected: ',info
     STOP
  END IF
  print *,'Number of nonzeros in factor L = ',nnzl
  print *,'Double words needed to factor = ',wspace

!...  Cholesky factorisation and backsolve to be done every step

  first_flag = .false.
  niter=0

  END IF

!...  Neumann inside for x

!...  matrix logic and numbering done once above

!...  load time dependent matrix

  l0=0
  l1=0

!...  the axis, first point, same as the first fsfc

  IF (mypex == 0) THEN

  i=1+ngdx
  j=1+ngdy
  area=fsfc(i)%area(j)
  amult=0.5*(fsfc(i)%amult(1:n_neighbors,j)+fsfc(i)%amult(0,j)) &
       *fsfc(i)%aratio(1:n_neighbors,j)
  diag=fsfc(i)%diag(j)

  IF (verbosity > 0) THEN
     WRITE (0,*) 'i0 = ',0
     WRITE (0,*) 'area = ',area
     WRITE (0,*) 'diag = ',diag
     WRITE (0,'("amult = ",6g12.4)') amult
     WRITE (0,'("R = ",7f10.4)') fsfc(i)%uu(:,nvars-2,j)
     WRITE (0,'("Z = ",7f10.4)') fsfc(i)%uu(:,nvars-1,j)
  END IF

!...  diagonal, point 0
  l0=l0+1
  l1=l1+1
  rhs(l0,1)=fsfc(i)%uux(j)*area
  ad(l1)=diag*area + SUM(amult)

!...  the six points of the first full hexagon
  DO j=1,6
     l1=l1+1
     ad(l1)= - amult(j)
  END DO

  END IF

  DO i0=i1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0/6

!...  each of the six sides of the hex described by this fsfc
     DO mu=0,5
        j1=mu*j0+1+ngdy
        j2=mu*j0+j0+ngdy

!...  the j0 points within a side

        DO j=j1,j2
           area=fsfc(i)%area(j)
           amult=0.5*(fsfc(i)%amult(1:n_neighbors,j)+fsfc(i)%amult(0,j)) &
                *fsfc(i)%aratio(1:n_neighbors,j)
           diag=fsfc(i)%diag(j)

           IF (verbosity > 0) THEN
              WRITE (0,*) 'i0 j0 = ',i0-1,j-ngdy
              WRITE (0,*) 'area = ',area
              WRITE (0,*) 'diag = ',diag
              WRITE (0,'("amult = ",6g12.4)') amult
              WRITE (0,'("R = ",7f10.4)') fsfc(i)%uu(:,nvars-2,j)
              WRITE (0,'("Z = ",7f10.4)') fsfc(i)%uu(:,nvars-1,j)
           END IF

!...  diagonal, point 0
           l0=l0+1
           l1=l1+1
           rhs(l0,1)=fsfc(i)%uux(j)*area
           ad(l1)=diag*area + SUM(amult)

!...  neighbor 1 is always ahead along fsfc except at wraps at last point
           IF (j < j2 .OR. mu < 5) THEN
              l1=l1+1
              ad(l1)= - amult(1)
           END IF
!...  neighbor 2 is always behind
!...  neighbor 3 is always behind except when it wraps at first point
           IF (j == j1 .AND. mu == 0) THEN
              l1=l1+1
              ad(l1)= - amult(3)
           END IF
!...  things on next fsfc drop on last active fsfc ...
           IF (i0 < nx0) THEN
!...  neighbor 4 is behind along fsfc except at begin of triangle ...
              IF (j == j1 .AND. mu > 0) THEN
                 l1=l1+1
                 ad(l1)= - amult(4)
              END IF
!...  neighbor 5 is always ahead
              l1=l1+1
              ad(l1)= - amult(5)
!...  neighbor 6 is always ahead
              l1=l1+1
              ad(l1)= - amult(6)
!...  neighbor 4 is one more circuit ahead at first point
              IF (j == j1 .AND. mu == 0) THEN
                 l1=l1+1
                 ad(l1)= - amult(4)
              END IF
           END IF
        END DO
     END DO

  END DO

!...  Cholesky factoring

#ifdef MPI
  CALL pwscchf(mmatrix, lar, lac, ad, lperm, linvp, waux, naux, info)
#else
  CALL wscchf(mmatrix, lar, lac, ad, lperm, linvp, waux, naux, info)
#endif
        IF (info .NE. 0) THEN
          print *,'In C-factoring the following ERROR was detected: ',info
          STOP
        END IF

!...  back solve

#ifdef MPI
  CALL pwsslv(mmatrix, lperm, linvp, rhs, mmatrix, nrhs, niter, waux, naux)
#else
  CALL wsslv(mmatrix, lperm, linvp, rhs, mmatrix, nrhs, niter, waux, naux)
#endif

!...  reload solution
!...  include bndys later move to bndys2d

  l0=0
  DO i0=1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     DO j=j1,j2
        l0=l0+1
        fsfc(i)%uuc(j)=rhs(l0,1)
     END DO
     DO j=1,ngdy
        fsfc(i)%uuc(j)=fsfc(i)%uuc(j+j0)
     END DO
     DO j=j0+ngdy+1,j0+ngdy+ngdy
        fsfc(i)%uuc(j)=fsfc(i)%uuc(j-j0)
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

END SUBROUTINE Mg2d
