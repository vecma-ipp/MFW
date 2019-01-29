PROGRAM Testvor

!...  tests Laplacian solver
!...  triangular grid

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: iplt
  INTEGER :: i,j
  REAL :: time
  REAL :: psi0,psi1

!...  open files and get parms

#ifdef MPI
  CALL MPI_Init(ierr)
  CALL MPI_Comm_size( MPI_COMM_WORLD, npes, ierr )
  CALL MPI_Comm_rank( MPI_COMM_WORLD, mype, ierr )
#endif

#ifdef PERF
  CALL PERFINIT

  CALL PERFON('main')
#endif

#ifdef MPI
  DO ipe=0,npes-1
     IF (ipe == mype) THEN
#endif
        OPEN (15,file='hdw.dat',form='formatted',status='old')
        READ (15,parm)
        CLOSE (15)
#ifdef MPI
     END IF
     CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  END DO
#endif

!...  system initialisation

#include <mppein.h90>

!...  geometry
!...  CPO profiles and boundary surface in metric

  CALL Metric

!...  allocations

  n_neighbors = 6

  INCLUDE 'vset.h90'

!...  initial state

  CALL Lay

!...  get the flux surfaces

  CALL GetPsi

  CALL FSDiags

  time=0.0
  CALL Psnaps(40,time)

#ifdef PERF
  CALL PERFOFF

  CALL PERFOUT('main')
#endif

#ifdef MPI
  CALL MPI_Finalize(ierr)
#endif

END PROGRAM Testvor


SUBROUTINE GetPsi

!...  solve delta star psi = mu_0 j

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,i0,j0,j1,j2

  DO i=1,nx
     j=fsfc(i)%ny
     j0=fsfc(i)%ny
     DO j=1,j0
!        fsfc(i)%uux(j)=1.0*fsfc(i)%rtor(j)
!        fsfc(i)%amult(:,j)=1.0*fsfc(i)%uu(:,nvars-2,j)
        fsfc(i)%uux(j)=1.0*rr0
        fsfc(i)%amult(:,j)=1.0*rr0
        fsfc(i)%diag=0.
     END DO
  END DO
  CALL MG2d
  DO i=1,nx
     fsfc(i)%uu(0,mupsi,:)=fsfc(i)%uuc
  END DO
  CALL Neighbors(mupsi,mupsi)

END SUBROUTINE GetPsi


SUBROUTINE Lay

!...  lay circular boundary and proportional fsfc

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,i0,j0,j1,j2,jp
  REAL :: heta,eta,eta0,ainterp0,ainterp1,r0,z0

  raxis=rr0
  zaxis=0.

  i=ngdx+nx0+1
  j0=fsfc(i)%ny
  hym=fsfc(i)%hy
  fsbndyeta=(/ (hym*(j-ngdy-1),j=1,j0) /)
  fsbndyr=raxis+COS(fsbndyeta)
  fsbndyz=zaxis+SIN(fsbndyeta)

  j0=SIZE(fsbndyr)-2*ngdy
  heta=tpi/REAL(j0)

  DO i=1,1+ngdx
     fsfc(i)%rtor=raxis
     fsfc(i)%ztor=zaxis
     fsfc(i)%btor=1.
  END DO

  DO i0=2,nx0+1
     i=i0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     DO j=j1,j2
        eta=fsfc(i)%eta(j)/heta
        jp=FLOOR(eta)
        eta0=REAL(jp)
        ainterp1=eta-eta0
        ainterp0=1.-ainterp1
        jp=jp+ngdy
        r0=ainterp0*fsbndyr(jp)+ainterp1*fsbndyr(jp+1)
        fsfc(i)%rtor(j)=raxis+ra(i)*(r0-raxis)
        z0=ainterp0*fsbndyz(jp)+ainterp1*fsbndyz(jp+1)
        fsfc(i)%ztor(j)=zaxis+ra(i)*(z0-zaxis)
        fsfc(i)%theta(j)=ATAN2(fsfc(i)%ztor(j)-zaxis,fsfc(i)%rtor(j)-raxis)
     END DO
     DO j=1,ngdy
        fsfc(i)%rtor(j)=fsfc(i)%rtor(j+j0)
        fsfc(i)%ztor(j)=fsfc(i)%ztor(j+j0)
        fsfc(i)%theta(j)=fsfc(i)%theta(j+j0)-tpi
     END DO
     DO j=j2+1,j2+ngdy
        fsfc(i)%rtor(j)=fsfc(i)%rtor(j-j0)
        fsfc(i)%ztor(j)=fsfc(i)%ztor(j-j0)
        fsfc(i)%theta(j)=fsfc(i)%theta(j-j0)+tpi
     END DO
     fsfc(i)%btor=rr0/fsfc(i)%rtor
     fsfc(i)%clocketa=fsfc(i)%theta
  END DO

!...  recover vars

  DO i=1,nx
     j0=fsfc(i)%ny
     DO j=1,j0
        fsfc(i)%uu(0,nvars-2,j)=fsfc(i)%rtor(j)
        fsfc(i)%uu(0,nvars-1,j)=fsfc(i)%ztor(j)
        fsfc(i)%uu(0,nvars,j)=fsfc(i)%btor(j)
     END DO
  END DO

  CALL Neighbors(1,neqs)
  CALL Neighbors(nvars-2,nvars)
  CALL Loops

END SUBROUTINE Lay


SUBROUTINE FSDiags

  USE Coeff
  USE Vars

  IMPLICIT NONE

  LOGICAL :: first_flag = .true.
  INTEGER :: iiter = 1

  INTEGER :: i,j,i0,j0,j1,j2,mu
  REAL :: r1,r2,dvol,darea,drr,dzz,dss,dqq,drr0,drr1,drr2
  INTEGER :: muc,mux,m0,mp,mm
  REAL :: area,areat,det,drdn,drdx,dzdn,dzdx,dndr,dndz,dxdr,dxdz
  REAL :: dpsidn,dpsidx,dpsidr,dpsidz
  REAL :: iplasma

  qtor=0.
  vtor=0.
  phitor=0.
  rho_cut=0.

  fsavgr0=0.
  fsavgr1=0.
  fsavgr2=0.

!...  integrals involving volumes

  muc=nvars-2
  mux=nvars-1

  DO i0=2,nx0+1
     i=i0+ngdx
     j0=fsfc(i)%ny0/6

     dvol=0.

     drr0=0.
     drr1=0.
     drr2=0.

     IF (i0 == nx0+1) THEN
        DO mu=0,5
           j1=mu*j0+1+ngdy
           j2=mu*j0+j0+ngdy
           DO j=j1,j2
              fsfc(i)%uu(1,:,j)=fsfc(i)%uu(0,:,j+1)
           END DO
           fsfc(i)%uu(2,:,j2)=fsfc(i-1)%uu(0,:,j2-mu)
        END DO
     END IF

     DO mu=0,5

        j1=mu*j0+1+ngdy
        j2=mu*j0+j0+ngdy

        DO j=j1,j2
           duu(:,1:nvars)=fsfc(i)%uu(:,1:nvars,j)

           DO m0=1,2
              IF (m0 == 2 .AND. j == j1) CYCLE
              mp=m0+1

              drr=(duu(0,muc)+duu(m0,muc)+duu(mp,muc))/3.
!              dzz=(duu(0,mux)+duu(m0,mux)+duu(mp,mux))/3.
              darea=( &
                   (duu(m0,muc)+duu(0,muc))*(duu(m0,mux)-duu(0,mux)) &
                   - (duu(m0,muc)-duu(0,muc))*(duu(m0,mux)+duu(0,mux)) &
                   + (duu(mp,muc)+duu(m0,muc))*(duu(mp,mux)-duu(m0,mux)) &
                   - (duu(mp,muc)-duu(m0,muc))*(duu(mp,mux)+duu(m0,mux)) &
                   + (duu(0,muc)+duu(mp,muc))*(duu(0,mux)-duu(mp,mux)) &
                   - (duu(0,muc)-duu(mp,muc))*(duu(0,mux)+duu(mp,mux)) )/4.

              drr0=drr0+darea*drr
              drr1=drr1+darea
              drr2=drr2+darea/drr

           END DO
        END DO
     END DO

     vtor(i)=vtor(i-1)+drr0*tpi
     fsavgr1(i)=drr1/drr0
     fsavgr2(i)=drr2/drr0
  END DO

  i0=1+ngdx
  fsavgr1(i0)=1./raxis
  fsavgr2(i0)=1./(raxis*raxis)
  fsavgr1(i0+1)=(fsavgr1(i0)+fsavgr1(i0+2))/2.
  fsavgr2(i0+1)=(fsavgr2(i0)+fsavgr2(i0+2))/2.

  DO i=1,ngdx
     vtor(i0-i)=vtor(i0+i)
     fsavgr1(i0-i)=fsavgr1(i0+i)
     fsavgr2(i0-i)=fsavgr2(i0+i)
  END DO

!...  psi

  DO i=1,nx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     psitor(i)=SUM(fsfc(i)%uu(0,mupsi,j1:j2))/REAL(j0)
  END DO

!...  toroidal field

  diidpsi = (jtor*fsavgr1-dpdpsi)/fsavgr2

  itor(ngdx+nx0+1:nx)=rr0*rr0/2.
  DO i0=nx0,1,-1
     i=i0+ngdx
     itor(i)=itor(i+1)+0.5*(diidpsi(i)+diidpsi(i+1))*(psitor(i)-psitor(i+1))
  END DO

  i0=1+ngdx
  DO i=1,ngdx
     itor(i0-i)=itor(i0+i)
  END DO
  itor=SQRT(2.*itor)

!...  toroidal flux

  dphidv = itor*fsavgr2/tpi

  DO i0=2,nx0+1
     i=i0+ngdx
     phitor(i)=phitor(i-1)+0.5*(dphidv(i)+dphidv(i-1))*(vtor(i)-vtor(i-1))
  END DO

  i0=1+ngdx
  DO i=1,ngdx
     phitor(i0-i)=phitor(i0+i)
  END DO

!...  pitch parameter

  i0=1+ngdx
  dvp0=tpi*(psitor(i0)-psitor)
  CALL L3deriv(phitor(i0),dvp0(i0),nx0+1,qtor(i0),dvp0(i0),nx0+1)

!  qtor(i0)=

  i0=1+ngdx
  DO i=1,ngdx
     qtor(i0-i)=qtor(i0+i)
  END DO

!...  distance cut

  DO i0=1,nx0+1
     i=i0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

     j=1+ngdy+j0/3
     drr=fsfc(i)%rtor(j)-raxis
     dzz=fsfc(i)%ztor(j)-zaxis

     rho_cut(i)=SQRT(drr*drr+dzz*dzz)
  END DO

!...  distance coordinates

  rho_vol=SQRT(vtor/MAXVAL(vtor))
  rho_tor=SQRT(phitor/MAXVAL(phitor))
  rho_cut=rho_cut/MAXVAL(rho_cut)

  i0=1+ngdx
  DO i=1,ngdx
     ra(i0-i)=-ra(i0+i)
     rho(i0-i)=-rho(i0+i)
     rho_vol(i0-i)=-rho_vol(i0+i)
     rho_tor(i0-i)=-rho_tor(i0+i)
     rho_cut(i0-i)=-rho_cut(i0+i)
  END DO

!...  Shafranov shift

  DO i=1,nx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     sshift(i)=(MAXVAL(fsfc(i)%rtor(j1:j2))+MINVAL(fsfc(i)%rtor(j1:j2)))/2. &
          - rr0
  END DO

!...  total current

  dphidv = jtor*fsavgr1/tpi

  iplasma=0.

  DO i0=2,nx0+1
     i=i0+ngdx
     iplasma=iplasma+0.5*(dphidv(i)+dphidv(i-1))*(vtor(i)-vtor(i-1))
  END DO

  write (0,*) 'total current = ',iplasma

!...  write info

  IF (first_flag) THEN
     OPEN (10,file=proffile,form='formatted')
     write (10,*) 'boundary sfc eta'
     write (10,110) fsbndyeta
     write (10,*) 'boundary sfc R'
     write (10,110) fsbndyr
     write (10,*) 'boundary sfc Z'
     write (10,110) fsbndyz
     first_flag = .false.
  ELSE
     OPEN (10,file=proffile,form='formatted',position='append')
     iiter=iiter+1
  END IF
  write (10,*) 'realisation'
  write (10,*) iiter
  write (10,*) 'ra'
  write (10,110) ra
  write (10,*) 'ra_cut'
  write (10,110) rho_cut
  write (10,*) 'rho'
  write (10,110) rho
  write (10,*) 'rho_vol'
  write (10,110) rho_vol
  write (10,*) 'rho_tor'
  write (10,110) rho_tor
  write (10,*) 'psi profile'
  write (10,110) psitor
  write (10,*) 'Phi profile'
  write (10,110) phitor
  write (10,*) 'q profile'
  write (10,110) qtor
  write (10,*) 'I profile'
  write (10,110) itor
  write (10,*) 'V profile'
  write (10,110) vtor
  write (10,*) 'J profile'
  write (10,110) jtor
  write (10,*) 'P profile'
  write (10,110) ptor
  write (10,*) '1/R profile'
  write (10,110) fsavgr1
  write (10,*) '1/R^2 profile'
  write (10,110) fsavgr2
  write (10,*) 'S shift profile'
  write (10,110) sshift
  CLOSE (10)

110 FORMAT(5g15.7)

END SUBROUTINE FSDiags

