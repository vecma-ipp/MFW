

SUBROUTINE GetPsi(psi0,psi1)

!...  solve delta star psi = mu_0 j

#ifdef ITMCPOs
  USE L3interps
#endif

  USE Coeff
  USE Vars

  IMPLICIT NONE

  REAL :: psi0,psi1

  INTEGER :: i,j,i0,j0,j1,j2
  REAL :: drr0,drr1,drr2,area,drr

!...  dpdpsi
!...  psitor=psi0*(1.-rho*rho)

  i0=1+ngdx
  CALL L3deriv(ptor(i0),psitor(i0),nx0+1,dpdpsi(i0),psitor(i0),nx0+1)
  dpdpsi(1:ngdx)=dpdpsi(i0)

  dpdpsi=beta*dpdpsi

!...  write profiles

  IF (write_diags) THEN

  OPEN (10,file=ffile,form='formatted',position='append')
  WRITE (10,*) 'GetPsi profiles'
  WRITE (10,*) 'current profile'
  WRITE (10,110) jtor
  WRITE (10,*) 'pressure profile'
  WRITE (10,110) ptor
  WRITE (10,*) 'psi profile'
  WRITE (10,110) psitor
  WRITE (10,*) 'beta pprime profile'
  WRITE (10,110) dpdpsi
  CLOSE (10)

  END IF

110 FORMAT(5g15.7)

!  WRITE (0,*) 'rho  1/R   1/R^2'
!  WRITE (0,*) 'rho  J   I   j'

  DO i=1+ngdx,nx0+ngdx
     j=fsfc(i)%ny
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

!...  find flux functions involving R

     drr0=0.
     drr1=0.
     drr2=0.

     DO j=j1,j2
        area=fsfc(i)%area(j)
        drr=fsfc(i)%rtor(j)
        drr0=drr0+area*drr
        drr1=drr1+area
        drr2=drr2+area/drr
     END DO
     fsavgr1(i)=drr1/drr0
     fsavgr2(i)=drr2/drr0

!...  find current for Amperes law  div R^-2 grad psi = -mu0 J^phi
!...  note Jacobian is R so this is pt R^-1 pt psi = -mu0 J^phi R
!...  also note that J_phi = R^2 J^phi and jtor is J^phi times R

!...  find current from flux functions and R -- MHD version uses J^phi

     dvpy(1:j)=1./(fsfc(i)%rtor(1:j)*fsfc(i)%rtor(1:j)*fsavgr2(i))
     fsfc(i)%uu(0,muj,1:j)= &
          (jtor(i)*fsavgr1(i)*dvpy(1:j) + dpdpsi(i)*(1.-dvpy(1:j))) &
          *fsfc(i)%rtor(1:j)

!     WRITE (0,110) ra(i),fsavg1(i),fsavg2(i)

!...  find current from flux functions and R -- simple MHD version J_phi

!     dvpy(1:j)=fsfc(i)%rtor(1:j)*fsfc(i)%rtor(1:j)
!     fsfc(i)%uu(0,muj,1:j)=(jtor(i)*rr0 &
!          + dpdpsi(i)*(dvpy(1:j)-SUM(dvpy(j1:j2))/REAL(j0))) &
!          /fsfc(i)%rtor(1:j)

!...  find current from flux functions and R -- reduced MHD version Jpl/B

!     dvpy(1:j)=1./(fsfc(i)%btor(1:j)*fsfc(i)%btor(1:j))
!     dvpy(1:j) = dvpy(1:j) - SUM(dvpy(j1:j2))/REAL(j0)
!     dvpy(1:j)=jtor(i) + dvpy(1:j)*dpdpsi(i)*itor(i)
!     dvpy(1:j)=dvpy(1:j)*fsfc(i)%btor(1:j)
!     fsfc(i)%uu(0,muj,1:j)=dvpy(1:j)

!     WRITE (0,110) ra(i),jtor(i),itor(i),fsfc(i)%uu(0,muj,j1)

!...  load RHS and matrix pieces, put Jacobian R factor in here
!...  whats in muj is already j^phi R = jtor
!...       which is the RHS for div (1/R^2) grad psi with the Jacobian R factor 
!...  amult gets R not R^2 due to the Jacobian R factor 

     j0=fsfc(i)%ny
     DO j=1,j0
        fsfc(i)%uux(j)=fsfc(i)%uu(0,muj,j)
        fsfc(i)%amult(:,j)=(1.0)/fsfc(i)%uu(:,nvars-2,j)
        fsfc(i)%diag=0.
        fsfc(i)%uuc(j)=psitor(i)
     END DO
  END DO

  i0=1+ngdx
  DO i=1,ngdx
     fsavgr1(i)=fsavgr1(i0)
     fsavgr2(i)=fsavgr2(i0)
  END DO
  i0=nx0+ngdx
  DO i=i0+1,nx
     fsavgr1(i)=fsavgr1(i0)
     fsavgr2(i)=fsavgr2(i0)
  END DO

  CALL MG2d
  psi0=0.
  DO i=1,nx
     fsfc(i)%uu(0,mupsi,:)=fsfc(i)%uuc
     fsfc(i)%uu(0,mutheta,:)=fsfc(i)%theta

     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     IF (i > ngdx .OR. i < nx0) psi0=MAX(psi0,MAXVAL(fsfc(i)%uuc(j1:j2)))
  END DO
     j1=1+ngdy
  psi1=fsfc(1+ngdx)%uuc(j1)
  CALL Neighbors(mupsi,mutheta)

  psitor=psi0*(1.-rho*rho)

  DO i=1,nx
     fsfc(i)%uu(:,mupressure,:)=ptor(i)
!     fsfc(i)%uu(:,muphige,:)=SQRT(1. - fsfc(i)%uu(:,mupsi,:)/psi0) - rho(i)
!     fsfc(i)%uu(:,muphige,:)=fsfc(i)%uu(:,mupsi,:) - psitor(i)
     fsfc(i)%uu(0,muphige,:)=fsfc(i)%uu(0,mupsi,:) - fsfc(i)%uu(0,mupsi,1+ngdy)
  END DO

  IF (write_diags) THEN
     OPEN (10,file=ffile,form='formatted',position='append')
     write (10,*) 'Max psi is',psi0
     write (10,*) 'Axis psi is',psi1
     CLOSE (10)
  END IF

END SUBROUTINE GetPsi

SUBROUTINE PutHalf

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,i0,j0,j1,j2

!  i0=2
!  i0=nx0/2
  i0=nx0
  i=i0+ngdx
  j0=fsfc(i)%ny0
  j1=1+ngdy
  j2=j0+ngdy
  OPEN (10,file=ffile,form='formatted',position='append')
  write (10,*) 'FS at i and ra = ',i,ra(i)
  write (10,*) 'R sfc',j0,' points'
  write (10,110) fsfc(i)%rtor(j1:j2)
  write (10,*) 'z sfc',j0,' points'
  write (10,110) fsfc(i)%ztor(j1:j2)
  write (10,*) 'theta sfc',j0,' points'
  write (10,110) fsfc(i)%theta(j1:j2)
!  write (10,110) fsfc(i)%theta(j1+1:j2+1)-fsfc(i)%theta(j1:j2)
  CLOSE (10)

110 FORMAT(5g15.7)

END SUBROUTINE PutHalf


SUBROUTINE MoveAxis(psi0,psi1)

!...  move axis or fsfc onto psi

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,i0,j0,j1,j2
  REAL :: psi0,psi1,theta0,deltar,deltaz
  INTEGER :: muc,mux,m0,mp,mm
  REAL :: area,areat,det,drdn,drdx,dzdn,dzdx,dndr,dndz,dxdr,dxdz
  REAL :: dpsidn,dpsidx,dpsidr,dpsidz,ddpsi,dss,drr,dzz,drr2,dzz2
  REAL :: dthetadn,dthetadx,dthetadr,dthetadz,ddtheta

  IF (write_diags) OPEN (10,file=ffile,form='formatted',position='append')

  muc=nvars-2
  mux=nvars-1

  IF (psi1 /= psi0) THEN
     IF (write_diags) THEN
        WRITE (0,*) 'moving axis'
        WRITE (10,*) 'moving axis'
        WRITE (10,*) 'psi0 and psi1 = ',psi0,psi1
     END IF

     i=1+ngdx
     j1=1+ngdy
     deltar=fsfc(i)%uu(0,nvars-2,j1)
     deltaz=fsfc(i)%uu(0,nvars-1,j1)

     DO i0=1,nx0
        i=i0+ngdx
        j0=fsfc(i)%ny0
        j1=1+ngdy
        j2=j0+ngdy

        DO j=j1,j2
           area=fsfc(i)%area2(j)
           duu(:,1:nvars)=fsfc(i)%uu(:,1:nvars,j)

           IF (duu(0,mupsi) == psi0) THEN
              IF (write_diags) WRITE (10,*) &
                   'used psi ',psi0,' at i0 j0 = ',i0,j-ngdy
              raxis=duu(0,nvars-2)
              zaxis=duu(0,nvars-1)
              deltar=raxis-deltar
              deltaz=zaxis-deltaz
           END IF
        END DO
     END DO

     DO i0=1,nx0
        i=i0+ngdx
        j0=fsfc(i)%ny0
        j1=1+ngdy
        j2=j0+ngdy

        DO j=j1,j2
           fsfc(i)%rtor(j)=fsfc(i)%rtor(j) + deltar*(1.-ra(i))
           fsfc(i)%ztor(j)=fsfc(i)%ztor(j) + deltaz*(1.-ra(i))
        END DO
        DO j=1,ngdy
           fsfc(i)%rtor(j)=fsfc(i)%rtor(j+j0)
           fsfc(i)%ztor(j)=fsfc(i)%ztor(j+j0)
        END DO
        DO j=j2+1,j2+ngdy
           fsfc(i)%rtor(j)=fsfc(i)%rtor(j-j0)
           fsfc(i)%ztor(j)=fsfc(i)%ztor(j-j0)
        END DO
     END DO

  ELSE
     IF (write_diags) THEN
        WRITE (0,*) 'moving fsfc'
        WRITE (10,*) 'moving fsfc'
     END IF

!...  move perp to fsfc while preserving angle

     DO i0=2,nx0
        i=i0+ngdx
        j0=fsfc(i)%ny0
        j1=1+ngdy
        j2=j0+ngdy

!        psi=fsfc(i)%uu(0,mupsi,j1)
        psi=psitor(i)

        DO j=j1,j2
           area=fsfc(i)%area2(j)
           duu(:,1:nvars)=fsfc(i)%uu(:,1:nvars,j)

           dpsidr=0.
           dpsidz=0.

           DO m0=1,6
              mp=m0+1
              mm=m0-1
              IF (m0 == 6) mp=1
              IF (m0 == 1) mm=6

              drdn=duu(m0,muc)-duu(0,muc)
              drdx=duu(mp,muc)-duu(0,muc)
              dzdn=duu(m0,mux)-duu(0,mux)
              dzdx=duu(mp,mux)-duu(0,mux)

              det=1./(drdn*dzdx-drdx*dzdn)
              dndr=dzdx*det
              dndz=-drdx*det
              dxdr=-dzdn*det
              dxdz=drdn*det

              areat=duu(m0,muc)*duu(mp,mux) - duu(mp,muc)*duu(m0,mux) &
                   + duu(mp,muc)*duu(0,mux) - duu(0,muc)*duu(mp,mux) &
                   + duu(0,muc)*duu(m0,mux) - duu(m0,muc)*duu(0,mux)
              areat=areat/2.
              areat=areat/area

              dpsidn=duu(m0,mupsi)-duu(0,mupsi)
              dpsidx=duu(mp,mupsi)-duu(0,mupsi)
              dpsidr=dpsidr+(dpsidn*dndr+dpsidx*dxdr)*areat
              dpsidz=dpsidz+(dpsidn*dndz+dpsidx*dxdz)*areat
           END DO

!...  move points perp to grad psi in direction grad phi cross grad psi
!...  grad psi is radially inward!

           dthetadz=duu(0,muc)-raxis
           dthetadr=zaxis-duu(0,mux)
           drr2=dthetadz*dthetadz + dthetadr*dthetadr

           areat=1./(dpsidr*dthetadz-dpsidz*dthetadr)

           ddpsi=psi - duu(0,mupsi)
!           ddtheta=drr2*(fsfc(i)%clocketa(j) - duu(0,mutheta))
           ddtheta=0.

           deltar=areat*(dthetadz*ddpsi-dpsidz*ddtheta)
           deltaz=areat*(dpsidr*ddtheta-dthetadr*ddpsi)

           fsfc(i)%rtor(j)=fsfc(i)%rtor(j) + deltar
           fsfc(i)%ztor(j)=fsfc(i)%ztor(j) + deltaz

           fsfc(i)%theta(j)=ATAN2(fsfc(i)%ztor(j)-zaxis,fsfc(i)%rtor(j)-raxis)
           IF (j > j1 .AND. fsfc(i)%theta(j) < fsfc(i)%theta(j-1)) &
                fsfc(i)%theta(j)=fsfc(i)%theta(j)+tpi
        END DO

!...  update angle

!        fsfc(i)%theta(j1:j2)= &
!             ATAN2(fsfc(i)%ztor(j1:j2)-zaxis,fsfc(i)%rtor(j1:j2)-raxis)

!...  periodicity

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
     END DO

     i=2+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     drr=SUM(fsfc(i)%rtor(j1:j2))/REAL(j0)
     dzz=SUM(fsfc(i)%ztor(j1:j2))/REAL(j0)
     i=3+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy
     drr2=SUM(fsfc(i)%rtor(j1:j2))/REAL(j0)
     dzz2=SUM(fsfc(i)%ztor(j1:j2))/REAL(j0)

     raxis=(4.*drr-drr2)/3.
     zaxis=(4.*dzz-dzz2)/3.
     i=1+ngdx
     fsfc(i)%rtor=raxis
     fsfc(i)%ztor=zaxis

!     DO i=1,nx
!        psitor(i)=fsfc(i)%uu(0,mupsi,1+ngdy)
!        ptor(i)=EXP(-wn*drr*drr)
!        ptor(i)=EXP(-wn*ra(i)*ra(i))
!        ptor(i)=EXP(-wn*(1.-psitor(i)/psi0))
!     END DO

  END IF

  IF (write_diags) THEN
     CLOSE (10)
     CALL PutHalf
  END IF

  DO i=1,nx
     j0=fsfc(i)%ny
     DO j=1,j0
        fsfc(i)%uu(0,mutheta,j)=fsfc(i)%theta(j)
        fsfc(i)%uu(0,nvars-2,j)=fsfc(i)%rtor(j)
        fsfc(i)%uu(0,nvars-1,j)=fsfc(i)%ztor(j)
        fsfc(i)%uu(0,nvars,j)=itor(i)/fsfc(i)%rtor(j)
     END DO
  END DO
  CALL Neighbors(nvars-2,nvars)
  CALL Loops

END SUBROUTINE MoveAxis

SUBROUTINE ReLay

!...  lay proportional fsfc

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i,j,i0,j0,j1,j2,jp
  REAL :: heta,eta,eta0,ainterp0,ainterp1,r0,z0

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
        IF (j > j1 .AND. fsfc(i)%theta(j) < fsfc(i)%theta(j-1)) &
             fsfc(i)%theta(j)=fsfc(i)%theta(j)+tpi
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

END SUBROUTINE ReLay


SUBROUTINE FSDiags

#ifdef ITMCPOs
  USE L3interps
#endif

  USE Coeff
  USE Vars

  IMPLICIT NONE

  LOGICAL, SAVE :: first_flag = .true.
  INTEGER, SAVE :: iiter = 1

  INTEGER :: i,j,i0,j0,j1,j2,mu
  REAL :: r1,r2,dvol,darea,drr,dzz,dss,dqq
  INTEGER :: muc,mux,m0,mp,mm
  REAL :: area,areat,det,drdn,drdx,dzdn,dzdx,dndr,dndz,dxdr,dxdz
  REAL :: dpsidn,dpsidx,dpsidr,dpsidz
  REAL :: iplasma

!...  integrals involving volumes

  qtor=0.
  vtor=0.
  phitor=0.
  rho_cut=0.

  muc=nvars-2
  mux=nvars-1

  DO i0=2,nx0+1
     i=i0+ngdx
     j0=fsfc(i)%ny0/6

     dvol=0.

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

              dvol=dvol+darea*drr
           END DO
        END DO
     END DO

     vtor(i)=vtor(i-1)+dvol*tpi
  END DO

  i0=1+ngdx
  DO i=1,ngdx
     vtor(i0-i)=vtor(i0+i)
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

  qaxis=(2.*itor(i0))/(jtor(i0)*raxis*raxis)
  phitor(i0+1)=qaxis*tpi*(psitor(i0)-psitor(i0+1))

  DO i=1,ngdx
     phitor(i0-i)=phitor(i0+i)
  END DO

!...  pitch parameter

  i0=1+ngdx
  dvp0=tpi*(psitor(i0)-psitor)
  CALL L3deriv(phitor(i0),dvp0(i0),nx0+1,qtor(i0),dvp0(i0),nx0+1)

  qtor(i0)=qaxis

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

!...  write info

  IF (write_diags) write (0,*) 'total current = ',iplasma

  IF (write_snaps) THEN
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

  END IF

END SUBROUTINE FSDiags

SUBROUTINE ReGrid(psi0)

#ifdef ITMCPOs
  USE L3interps
#endif

  USE Coeff
  USE Vars

  IMPLICIT NONE

  REAL :: psi0

  INTEGER :: i,j,i0,j0,j1,j2,mu

!...  change the target rho depending on the solution

  IF (write_debug) THEN
     write (0,*) 'rho before'
     write (0,110) rho
  END IF

  SELECT CASE(grid_choice)
     CASE("rho_tor")
        CALL L3interp(rho,rho_tor,nx,dvp0,ra,nx)
        dvp0=0.5*(dvp0+rho)
     CASE("rho_vol")
        CALL L3interp(rho,rho_vol,nx,dvp0,ra,nx)
        dvp0=0.5*(dvp0+rho)
     CASE("rho_cut")
        CALL L3interp(rho,rho_cut,nx,dvp0,ra,nx)
        dvp0=0.5*(dvp0+rho)
     CASE DEFAULT
!        dvp0=rho
  END SELECT

  IF (write_debug) THEN
     write (0,*) 'rho after'
     write (0,110) rho
  END IF

  IF (grid_choice /= "rho_pol") THEN
     rho=dvp0
     psitor=psi0*(1.-rho*rho)
  END IF

  IF (grid_choice /= "rho_tor") THEN
     CALL L3interp(j_eq,ra,nx,jtor,rho_tor,nx)
     CALL L3interp(p_eq,ra,nx,ptor,rho_tor,nx)
  END IF

110 FORMAT(5g15.7)

END SUBROUTINE ReGrid


#ifdef ITMCPOs
SUBROUTINE GetProfs(eq_in)

  USE Euitm_Schemas
  USE L3interps

  USE Coeff
  USE Vars

  IMPLICIT NONE

  TYPE (type_equilibrium) ::  eq_in

  INTEGER :: i,i0

  REAL, DIMENSION(:), POINTER :: jphi_eq

!...  which current profile

  IF (j_choice == 'jphi') THEN
     jphi_eq => eq_in%profiles_1d%jphi
  ELSE IF (j_choice == 'jparallel') THEN
     jphi_eq => eq_in%profiles_1d%jparallel
  ELSE
     WRITE (0,*) 'ERROR: bad j_choice: must be jphi or jparallel'
     STOP
  END IF

!...  update normalising parameters

  p00=MAXVAL(eq_in%profiles_1d%pressure)
  rho00=MAXVAL(eq_in%profiles_1d%rho_tor)

  beta=mu_0*p00/(b00*b00)
!  jj0=MAXVAL(jphi_eq)/j00
  jj0=ABS(jphi_eq(1))/j00

!...  get profiles

  npsi_eq=SIZE(eq_in%profiles_1d%rho_tor)

  i0=1+ngdx

!  dvp0=rho00*ra
  dvp0=rho00*rho_tor

  CALL L3interp( eq_in%profiles_1d%pressure, &
       eq_in%profiles_1d%rho_tor, &
       npsi_eq, &
       ptor(i0), dvp0(i0), nx0+1 )

  CALL L3interp( jphi_eq, &
       eq_in%profiles_1d%rho_tor, &
       npsi_eq, &
       jtor(i0), dvp0(i0), nx0+1 )

  i0=1+ngdx
  DO i=1,ngdx
     ptor(i0-i)=ptor(i0+i)
     jtor(i0-i)=jtor(i0+i)
  END DO

  jtor=jj0*jtor/jtor(i0)
  ptor=ptor/ptor(i0)

  j_eq=jtor
  p_eq=ptor

  IF (write_diags) THEN

     OPEN (10,file=ffile,form='formatted')
     WRITE (10,*) 'profile changes'
     WRITE (10,*) 'rho_tor  pressure   current'
     DO i=1,npsi_eq
        WRITE (10,'(3g11.3)') &
             eq_in%profiles_1d%rho_tor(i), &
             eq_in%profiles_1d%pressure(i), &
             jphi_eq(i)
     END DO

     WRITE (10,*) 'normalising constants  B    a    R   p  J',b00,a00,r00,p00,j00
     WRITE (10,*) 'normalising constants  beta   jj0  rho00',beta,jj0,rho00

     WRITE (10,*) 'rho       ptor    jtor'
     DO i=1,nx
        WRITE (10,'(3g11.3)') &
             dvp0(i),ptor(i),jtor(i)
     END DO
     CLOSE (10)

  END IF

END SUBROUTINE GetProfs
#endif
