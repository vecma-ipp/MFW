SUBROUTINE Mg2d

!...  pure 2D solve

!...  2-D Helmholtz equation
!...  PETSc version follows basic vec structures with no intermediates

!...  global geodesic grid

!...  soln vectors defined in Vars by fsfc
!...  on input, uux is the source, uuc is the guess
!...  on output, uuc becomes the solution
!...  not parallelised

  USE Coeff
  USE Vars

  IMPLICIT NONE

!...  locals

  LOGICAL, SAVE :: first_flag = .true.

!...  PETSc stuff

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"

      external UserInitializeLinearSolver
      external UserFinalizeLinearSolver
      external UserDoLinearSolver

  PetscErrorCode ::  petsc_ierr
  PetscMPIInt size,rank
  PetscInt, SAVE ::  mmatrix,lda
  PetscFortranAddr, SAVE :: userctx(5)

!...  setup for first time through

  IF (first_flag) THEN

!...  allocate matrices

     mmatrix=3*(nx00-1)*nx00+1

     lda=4*mmatrix + 6*(nx00-1)
     IF (mypex == 0) lda=lda+7
     IF (mypex == npesx-1) lda=lda-12*(nx00-1)-6

!  Dummy MPI stuff apparently needed

!     call PetscInitialize(PETSC_NULL_CHARACTER,petsc_ierr)
#ifdef gateway
     call PetscInitialize("/afs/efda-itm.eu/imp4/user/bscott/public/petsc.in",petsc_ierr)
#else
     call PetscInitialize("./petsc.in",petsc_ierr)
#endif
     call MPI_Comm_size(PETSC_COMM_WORLD,size,petsc_ierr)
     if (size .ne. 1) then
        call MPI_Comm_rank(PETSC_COMM_WORLD,rank,petsc_ierr)
        if (rank .eq. 0) then
           write(6,*) 'This is a uniprocessor example only!'
        endif
        SETERRQ(1,' ',petsc_ierr)
     endif

!  Create the empty sparse matrix and linear solver data structures

     call UserInitializeLinearSolver(mmatrix,userctx,petsc_ierr)
     IF (write_debug) write (0,*) 'did petsc init'

     first_flag = .false.

  END IF

!  load and do the solve itself (sees Vars)

  CALL UserDoLinearSolver(userctx,petsc_ierr)

END SUBROUTINE Mg2d


subroutine UserInitializeLinearSolver(Ntot,userctx,ierr)

  implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"

  PetscInt Ntot
  PetscErrorCode ierr
  PetscFortranAddr userctx(*)

!  Local variable declararions
  Mat     A
  Vec     b,x
  KSP    ksp
  LOGICAL :: write_debug = .false.
  LOGICAL :: write_diags = .false.

!  Create the sparse matrix. Preallocate 7 nonzeros per row.

  call MatCreateSeqAIJ(PETSC_COMM_SELF,Ntot,Ntot,7,                  &
       &     PETSC_NULL_INTEGER,A,ierr)

!  call MatSetOption(A, MAT_SYMMETRIC, PETSC_TRUE, ierr)

  IF (write_debug) write (0,*) 'did mat create'

!
!  Create vectors. Here we create vectors with no memory allocated.
!  This way, we can use the data structures already in the program
!  by using VecPlaceArray() subroutine at a later stage.
!
  call VecCreateSeq(PETSC_COMM_SELF,Ntot,b,ierr)
  IF (write_debug) write (0,*) 'did vec create'

  call VecDuplicate(b,x,ierr)
  IF (write_debug) write (0,*) 'did vec dupl'

!  Create linear solver context. This will be used repeatedly for all 
!  the linear solves needed.

  call KSPCreate(PETSC_COMM_SELF,ksp,ierr)
  IF (write_debug) write (0,*) 'did ksp create'

  userctx(1) = x
  userctx(2) = b
  userctx(3) = A
  userctx(4) = ksp
  userctx(5) = Ntot

end subroutine UserInitializeLinearSolver

! -----------------------------------------------------------------------
!   Solves -div (rho grad psi) = F using finite differences.
!   Elements taken directly from Vars

subroutine UserDoLinearSolver(userctx,ierr)

  USE Vars

  implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"

!...  inputs

  INTEGER :: isolve = 2

  INTEGER, SAVE :: iter, maxiter=1000
  REAL, SAVE  :: errbnd=1.0e-12

  PetscErrorCode ierr
  PetscFortranAddr userctx(*)

!...  locals

  REAL, DIMENSION(:), ALLOCATABLE, SAVE :: amult

  PC   pc
  KSP ksp
  Vec  b,x
  Mat  A
  PetscInt Ntot,one
  PetscInt l0,l1
  PetscScalar vv
  LOGICAL :: write_debug = .false.
  LOGICAL :: write_diags = .false.

!...  counters and locals from inputs

  INTEGER :: verbosity = 0

  INTEGER :: i,j,i0,i1,i2,j0,j1,j2,mu
  REAL :: area,diag

  LOGICAL, SAVE :: first_flag = .true.

!...  setup for first time through

  IF (first_flag) THEN

     ALLOCATE(amult(n_neighbors))

     first_flag = .false.

  END IF

  one  = 1
  x    = userctx(1)
  b    = userctx(2)
  A    = userctx(3)
  ksp  = userctx(4)
  Ntot = int(userctx(5))

!...  set boundary indices

  i1=1
  i2=nx0
  IF (mypex == 0) i1=2

!...  load and do solve

  l0=0

!.  vv = 0.
!.  call MatSetValues(A,Ntot,l0,Ntot,l0,vv,INSERT_VALUES,ierr)

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
  vv=fsfc(i)%uux(j)*area
  call VecSetValues(b,one,l0,vv,INSERT_VALUES,ierr)
  vv=fsfc(i)%uuc(j)
  call VecSetValues(x,one,l0,vv,INSERT_VALUES,ierr)
  vv=diag*area + SUM(amult)
  call MatSetValues(A,one,l0,one,l0,vv,INSERT_VALUES,ierr)

!...  the six points of the first full hexagon
  DO j=1,6
     l1=l0+j
     vv= - amult(j)
     call MatSetValues(A,one,l0,one,l1,vv,INSERT_VALUES,ierr)
     call MatSetValues(A,one,l1,one,l0,vv,INSERT_VALUES,ierr)
  END DO

  l0=l0+1
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
           vv=fsfc(i)%uux(j)*area
           call VecSetValues(b,one,l0,vv,INSERT_VALUES,ierr)
           vv=fsfc(i)%uuc(j)
           call VecSetValues(x,one,l0,vv,INSERT_VALUES,ierr)
           vv=diag*area + SUM(amult)
           call MatSetValues(A,one,l0,one,l0,vv,INSERT_VALUES,ierr)

!...  neighbor 1 is always ahead along fsfc except at wraps at last point
           IF (j < j2 .OR. mu < 5) THEN
              l1=l0+1
              vv= - amult(1)
              call MatSetValues(A,one,l0,one,l1,vv,INSERT_VALUES,ierr)
              call MatSetValues(A,one,l1,one,l0,vv,INSERT_VALUES,ierr)
           END IF
!...  neighbor 2 is always behind
!...  neighbor 3 is always behind except when it wraps at first point
           IF (j == j1 .AND. mu == 0) THEN
              l1=l0+6*j0-1
              vv= - amult(3)
              call MatSetValues(A,one,l0,one,l1,vv,INSERT_VALUES,ierr)
              call MatSetValues(A,one,l1,one,l0,vv,INSERT_VALUES,ierr)
           END IF
!...  things on next fsfc drop on last active fsfc ...
           IF (i0 < nx0) THEN
!...  neighbor 4 is behind along fsfc except at begin of triangle ...
              IF (j == j1 .AND. mu > 0) THEN
                 l1=l0+6*j0+mu-1
                 vv= - amult(4)
                 call MatSetValues(A,one,l0,one,l1,vv,INSERT_VALUES,ierr)
                 call MatSetValues(A,one,l1,one,l0,vv,INSERT_VALUES,ierr)
              END IF
!...  neighbor 5 is always ahead
              l1=l0+6*j0+mu
              vv= - amult(5)
              call MatSetValues(A,one,l0,one,l1,vv,INSERT_VALUES,ierr)
              call MatSetValues(A,one,l1,one,l0,vv,INSERT_VALUES,ierr)
!...  neighbor 6 is always ahead
              l1=l0+6*j0+mu+1
              vv= - amult(6)
              call MatSetValues(A,one,l0,one,l1,vv,INSERT_VALUES,ierr)
              call MatSetValues(A,one,l1,one,l0,vv,INSERT_VALUES,ierr)
!...  neighbor 4 is one more circuit ahead at first point
              IF (j == j1 .AND. mu == 0) THEN
                 l1=l0+6*(2*j0+1)-1
                 vv= - amult(4)
                 call MatSetValues(A,one,l0,one,l1,vv,INSERT_VALUES,ierr)
                 call MatSetValues(A,one,l1,one,l0,vv,INSERT_VALUES,ierr)
              END IF
           END IF
           l0=l0+1
        END DO
     END DO

  END DO

!
!     Assemble matrix and vectors
!

  call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
  call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

  call VecAssemblyBegin(b,ierr)
  call VecAssemblyEnd(b,ierr)
  call VecAssemblyBegin(x,ierr)
  call VecAssemblyEnd(x,ierr)

!
!     Set operators. Here the matrix that defines the linear system
!     also serves as the preconditioning matrix. Since all the matrices
!     will have the same nonzero pattern here, we indicate this so the
!     linear solvers can take advantage of this.
!
  call KSPSetOperators(ksp,A,A,SAME_NONZERO_PATTERN,ierr)

!
!     Set linear solver defaults for this problem (optional).
!     - Here we set it to use direct LU factorization for the solution
!

  IF (isolve == 0) THEN
     call KSPSetType(ksp,KSPPREONLY,ierr)
     call KSPGetPC(ksp,pc,ierr)
     call PCSetType(pc,PCLU,ierr)
  ELSE IF (isolve == 1) THEN
     call KSPSetType(ksp,KSPCG,ierr)
     call KSPSetInitialGuessNonzero(ksp,PETSC_FALSE,ierr)
     call KSPSetTolerances(ksp, errbnd, PETSC_DEFAULT_DOUBLE_PRECISION, &
          PETSC_DEFAULT_DOUBLE_PRECISION, maxiter, ierr)
     call KSPGetPC(ksp,pc,ierr)
     call PCSetType(pc,PCICC,ierr)
  ELSE IF (isolve == 2) THEN
     call KSPSetType(ksp,KSPCG,ierr)
     call KSPSetInitialGuessNonzero(ksp,PETSC_FALSE,ierr)
     call KSPSetTolerances(ksp, errbnd, PETSC_DEFAULT_DOUBLE_PRECISION, &
          PETSC_DEFAULT_DOUBLE_PRECISION, maxiter, ierr)
!.     call KSPGetPC(ksp,pc,ierr)
     call KSPSetFromOptions(ksp,ierr)
  ELSE
     write (0,*) 'no solver given!'
  END IF

!
!     Set runtime options, e.g.,
!        -ksp_type <type> -pc_type <type> -ksp_monitor -ksp_rtol <rtol>
!     These options will override those specified above as long as
!     KSPSetFromOptions() is called _after_ any other customization
!     routines.
! 
!     Run the program with the option -help to see all the possible
!     linear solver options.
!
  call KSPSetFromOptions(ksp,ierr)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!                      Solve the linear system
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  IF (write_diags) write (0,*) 'do solve'
  call KSPSolve(ksp,b,x,ierr)
  IF (isolve == 0) THEN
     IF (write_diags) write (0,*) 'done solve'
  ELSE
     call KSPGetIterationNumber(ksp, iter, ierr)
     IF (write_diags) write (0,*) 'did solve with ',iter,' iterations'
  END IF

!...  reload solution
!...  include bndys later move to bndys2d

  l0=0
  DO i0=1,nx0
     i=i0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     DO j=j1,j2
        call VecGetValues(x,one,l0,vv,ierr)
        fsfc(i)%uuc(j)=vv
        l0=l0+1
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

end subroutine UserDoLinearSolver

! ------------------------------------------------------------------------

subroutine UserFinalizeLinearSolver(userctx,ierr)

  implicit none

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"

  PetscErrorCode ierr
  PetscFortranAddr userctx(*)

!  Local variable declararions

  Vec  x,b
  Mat  A
  KSP ksp
  LOGICAL :: write_debug = .false.
  LOGICAL :: write_diags = .false.

  x    = userctx(1)
  b    = userctx(2)
  A    = userctx(3)
  ksp = userctx(4)

  call VecDestroy(x,ierr)
  call VecDestroy(b,ierr)
  call MatDestroy(A,ierr)
  call KSPDestroy(ksp,ierr)

  call PetscFinalize(ierr)
  IF (write_diags) write (0,*) 'did finalise'

end subroutine UserFinalizeLinearSolver
