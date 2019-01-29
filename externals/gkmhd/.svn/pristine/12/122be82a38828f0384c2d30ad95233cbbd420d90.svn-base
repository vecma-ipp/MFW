SUBROUTINE Metric

  USE Coeff
  USE Vars

  IMPLICIT NONE

!...  simple unit slab metric

  INTEGER :: i,j,i0,j0,j1,j2
  REAL :: p0,p1

  INCLUDE 'mset.h90'

!...  radial and aux square grid

  nx2=ngdx+1-mypex*nx0

  hx=1.0/REAL(nx00)
  ra=(/ (hx*(i-nx2),i=1,nx) /)
!  rho=(EXP(ra)-1.)/(EXP(1.)-1.)
!  rho=ra
!  rho=(1.-EXP(-ra))/(1.-EXP(-1.))
!  rho=(1.-EXP(-2.*ra))/(1.-EXP(-2.))
  rho=SQRT( (1.-EXP(-ra*ra))/(1.-EXP(-1.)) )

  nx2=nx00/2+ngdx-mypex*nx0
  ny2=ny00/2+ngdy

  hy=tpi/REAL(ny00)
  xx=(/ (hx*(i-nx2-0.5),i=1,nx) /)
  yy=(/ (hy*(j-ny2-0.5),j=1,ny) /)

!...  flux surface grid structure, vars done elsewhere

  DO i=1,nx
     i0=i-ngdx
     j0=MAX(1, (i0-1)*6)
     fsfc(i)%ny0=j0
     j2=MAX(1, j0/2)
     fsfc(i)%ny2=j2+ngdy
     j=j0+2*ngdy
     fsfc(i)%ny=j

     hym=tpi/REAL(j0)
     fsfc(i)%hy=hym

     ALLOCATE(fsfc(i)%clocketa(j))
     ALLOCATE(fsfc(i)%eta(j))
     ALLOCATE(fsfc(i)%rtor(j))
     ALLOCATE(fsfc(i)%ztor(j))
     ALLOCATE(fsfc(i)%btor(j))
     ALLOCATE(fsfc(i)%theta(j))

!     p0=1.+(elongation-1.)*ra(i)
!     p1=triangularity*ra(i)
     p0=elongation
     p1=triangularity

     fsfc(i)%eta=(/ (hym*(j0-ngdy-1),j0=1,j) /)
     fsfc(i)%rtor=rr0+ra(i)*COS(fsfc(i)%eta+p1*SIN(fsfc(i)%eta))
     fsfc(i)%ztor=p0*ra(i)*SIN(fsfc(i)%eta)
     fsfc(i)%btor=rr0/fsfc(i)%rtor
     fsfc(i)%theta=fsfc(i)%eta
     fsfc(i)%clocketa=fsfc(i)%theta
  END DO

  i=ngdx+nx0+1
  j0=fsfc(i)%ny
  ALLOCATE(fsbndyeta(j0))
  ALLOCATE(fsbndyr(j0))
  ALLOCATE(fsbndyz(j0))

!...  step sizes

  hxm=1.0/hx
  hx2m=0.5/hx
  hxm2=1.0/(hx*hx)

  hym=1.0/hy
  hy2m=0.5/hy
  hym2=1.0/(hy*hy)

!...  parameters

  INCLUDE 'SetParms.h90'

!...  CPO profiles and fs diags

  ALLOCATE (itor(nx))
  ALLOCATE (jtor(nx))
  ALLOCATE (ptor(nx))
  ALLOCATE (psitor(nx))
  ALLOCATE (dpdpsi(nx))

  ALLOCATE (diidpsi(nx))
  ALLOCATE (dphidv(nx))
  ALLOCATE (qtor(nx))
  ALLOCATE (phitor(nx))
  ALLOCATE (vtor(nx))
  ALLOCATE (rho_tor(nx))
  ALLOCATE (rho_vol(nx))
  ALLOCATE (rho_cut(nx))
  ALLOCATE (fsavgr0(nx))
  ALLOCATE (fsavgr1(nx))
  ALLOCATE (fsavgr2(nx))
  ALLOCATE (sshift(nx))

  ALLOCATE (j_eq(nx))
  ALLOCATE (p_eq(nx))

!...  done

  IF (mype == 0) WRITE (6,*) 'GKMHD metric done'

END SUBROUTINE Metric


#ifdef ITMCPOs
SUBROUTINE GetBndy(eq_in)
#else
SUBROUTINE GetBndy
#endif

#ifdef ITMCPOs
  USE Euitm_Schemas
  USE L3interps
#endif

  USE Coeff
  USE Vars

  IMPLICIT NONE

#ifdef ITMCPOs
  TYPE (type_equilibrium) ::  eq_in
#endif

  INTEGER :: i,j,i0,j0,j1,j2,ios
  INTEGER :: jj,location(1)
  REAL :: eta,aminor,rmajor

  REAL, DIMENSION(:), POINTER :: rr_eq,zz_eq,jphi_eq

  TYPE type_eqbndy
     REAL, DIMENSION(:), POINTER :: eta,rtor,ztor
  END TYPE type_eqbndy

  TYPE (type_eqbndy) ::  eq,eqb

!...  get lcfs bndy

  i=ngdx+nx0+1
  j0=fsfc(i)%ny
  hym=fsfc(i)%hy
  fsbndyeta=(/ (hym*(j-ngdy-1),j=1,j0) /)

  IF (write_diags) OPEN (10,file=ffile,form='formatted',position='append')

#ifdef ITMCPOs
  IF (write_diags) WRITE (10,*) 'using the CPOs'
  rr_eq => eq_in%eqgeometry%boundary(1)%r
  zz_eq => eq_in%eqgeometry%boundary(1)%z
  neta_eq=SIZE(rr_eq)
  IF (ABS(rr_eq(1)-rr_eq(neta_eq)) < 0.1*ABS(rr_eq(1)-rr_eq(2)) &
       .AND. ABS(zz_eq(1)-zz_eq(neta_eq)) < 0.1*ABS(zz_eq(1)-zz_eq(2))) THEN
     neta_eq=neta_eq-1
     IF (write_diags) WRITE (10,*) '    reduced neta by one'
  END IF
#else
  OPEN (unit = 17, file = 'plasma_boundary.in', &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)

  IF (ios /= 0) THEN    

     IF (write_diags) WRITE (10,*) 'using the formula'

     aminor=0.5
     rmajor=rr0*aminor

     fsbndyr=rr0+ra(i)*COS(fsbndyeta+triangularity*SIN(fsbndyeta))
     fsbndyz=elongation*ra(i)*SIN(fsbndyeta)

  ELSE

     IF (write_diags) WRITE (10,*) 'using the file'

     neta_eq = 0
     DO
!        READ(unit = 17, fmt='(E24.16, E27.16)', iostat = ios) eta, eta
        READ(17, *, iostat = ios) eta, eta
        IF (ios == -1) EXIT
        neta_eq = neta_eq + 1
     END DO
#endif

     IF (write_diags) WRITE (10,*) neta_eq,'points'

     ALLOCATE(eq%eta(neta_eq))
     ALLOCATE(eq%rtor(neta_eq))
     ALLOCATE(eq%ztor(neta_eq))

#ifdef ITMCPOs
     eq%rtor(1:neta_eq)=rr_eq(1:neta_eq)
     eq%ztor(1:neta_eq)=zz_eq(1:neta_eq)
#else
     REWIND 17

     DO j = 1, neta_eq
!        READ(17, '(E24.16, E27.16)') &
        READ(17, *) eq%rtor(j), eq%ztor(j)
     END DO
     CLOSE (17)
#endif

     raxis=(MAXVAL(eq%rtor)+MINVAL(eq%rtor))/2.
     zaxis=(MAXVAL(eq%ztor)+MINVAL(eq%ztor))/2.

     IF (ATAN2(eq%ztor(1)-zaxis,eq%rtor(1)-raxis) > &
          ATAN2(eq%ztor(2)-zaxis,eq%rtor(2)-raxis)) THEN
        IF (write_diags) THEN
           write (10,*) 'input axis R Z',raxis,zaxis
           write (10,*) 'input bndy R',eq%rtor
           write (10,*) 'input bndy Z',eq%ztor

           WRITE (10,*) 'switching order'
        END IF
        eq%eta(1:neta_eq)=eq%rtor(neta_eq:1:-1)
        eq%rtor(1:neta_eq)=eq%eta(1:neta_eq)
        eq%eta(1:neta_eq)=eq%ztor(neta_eq:1:-1)
        eq%ztor(1:neta_eq)=eq%eta(1:neta_eq)

        IF (write_diags) THEN
           write (10,*) 'input bndy R',eq%rtor
           write (10,*) 'input bndy Z',eq%ztor
        END IF
     END IF

     location=MINLOC(eq%ztor)
!     location=MAXLOC(eq%rtor)
!     location=MINLOC(eq%rtor)
     jj=location(1)
     IF (write_diags) WRITE (10,*) 'jj = ',jj
     eq%rtor=CSHIFT(eq%rtor,jj-1)
     eq%ztor=CSHIFT(eq%ztor,jj-1)
     location=MINLOC(eq%ztor)
!     location=MAXLOC(eq%rtor)
!     location=MINLOC(eq%rtor)
     jj=location(1)
     IF (write_diags) WRITE (10,*) 'jj = ',jj,jj
!     IF (jj > 0) STOP

!...  geometric parameters and normalisation

     aminor=(MAXVAL(eq%rtor)-MINVAL(eq%rtor))/2.
     rmajor=(MAXVAL(eq%rtor)+MINVAL(eq%rtor))/2.

     rr0=rmajor/aminor
     eq%rtor=eq%rtor/aminor
     eq%ztor=eq%ztor/aminor

!...  constant angle

!     eq%eta=(/ ((tpi/REAL(neta_eq))*(j-1),j=1,neta_eq) /)

     raxis=(MAXVAL(eq%rtor)+MINVAL(eq%rtor))/2.
     zaxis=(MAXVAL(eq%ztor)+MINVAL(eq%ztor))/2.

     eq%eta=ATAN2(eq%ztor-zaxis,eq%rtor-raxis)
     eq%eta=eq%eta-eq%eta(1)
     DO j=1,neta_eq
        IF (eq%eta(j) < 0.) eq%eta(j)=eq%eta(j)+tpi
     END DO

!...  put in interpolation guard cells

     j1=neta_eq+1
     j0=neta_eq

     ALLOCATE(eqb%eta(j1))
     ALLOCATE(eqb%rtor(j1))
     ALLOCATE(eqb%ztor(j1))

     DO j=1,j0
        eqb%eta(j)=eq%eta(j)
        eqb%rtor(j)=eq%rtor(j)
        eqb%ztor(j)=eq%ztor(j)
     END DO
     eqb%eta(j1)=eq%eta(1) + tpi
     eqb%rtor(j1)=eq%rtor(1)
     eqb%ztor(j1)=eq%ztor(1)

!...  put bndy onto code strips

     j0=SIZE(fsbndyeta)

     IF (write_diags) THEN
        write (10,*) 'input bndy grid',neta_eq,' output',j0
        write (10,*) 'input bndy eta',eqb%eta
        write (10,*) 'input bndy R',eqb%rtor
        write (10,*) 'input bndy Z',eqb%ztor
     END IF

     CALL L3interp(eqb%rtor,eqb%eta,j1,fsbndyr,fsbndyeta,j0)
     CALL L3interp(eqb%ztor,eqb%eta,j1,fsbndyz,fsbndyeta,j0)

     j0=fsfc(i)%ny0
     fsbndyr(1:ngdy)=fsbndyr(1+j0:ngdy+j0)
     fsbndyz(1:ngdy)=fsbndyz(1+j0:ngdy+j0)
     j1=j0+ngdy+1
     j2=j0+ngdy+ngdy
     fsbndyr(j1:j2)=fsbndyr(j1-j0:j2-j0)
     fsbndyz(j1:j2)=fsbndyz(j1-j0:j2-j0)

     IF (write_diags) THEN
        write (10,*) 'output bndy eta',fsbndyeta
        write (10,*) 'output bndy R',fsbndyr
        write (10,*) 'output bndy Z',fsbndyz
     END IF

     DEALLOCATE(eq%eta)
     DEALLOCATE(eq%rtor)
     DEALLOCATE(eq%ztor)

     DEALLOCATE(eqb%eta)
     DEALLOCATE(eqb%rtor)
     DEALLOCATE(eqb%ztor)

#ifdef ITMCPOs
#else
  END IF
#endif

  raxis=(MAXVAL(fsbndyr)+MINVAL(fsbndyr))/2.
!  zaxis=(MAXVAL(fsbndyz)+MINVAL(fsbndyz))/2.
  zaxis=0.

#ifdef ITMCPOs

!...  which current profile

  IF (j_choice == 'jphi') THEN
     jphi_eq => eq_in%profiles_1d%jphi
  ELSE IF (j_choice == 'jparallel') THEN
     jphi_eq => eq_in%profiles_1d%jparallel
  ELSE
     WRITE (0,*) 'ERROR: bad j_choice: must be jphi or jparallel'
     STOP
  END IF

!...  get normalising parameters

  a00=aminor
  b00=ABS(eq_in%global_param%toroid_field%b0)
  r00=rmajor
  z00=zaxis*aminor
  j00=b00/(mu_0*a00)
  p00=MAXVAL(eq_in%profiles_1d%pressure)
  rho00=MAXVAL(eq_in%profiles_1d%rho_tor)

  beta=mu_0*p00/(b00*b00)
!  jj0=MAXVAL(jphi_eq)/j00
  jj0=ABS(jphi_eq(1))/j00

!...  get profiles

  npsi_eq=SIZE(eq_in%profiles_1d%rho_tor)

  i0=1+ngdx

  dvp0=rho00*ra

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

     WRITE (10,*) 'input'
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

  END IF

#else

  dvp0=ra

  jtor=jj0*EXP(-dvp0*dvp0)*(1.-dvp0*dvp0)
!  jtor=jj0*EXP(-dvp0*dvp0)
  ptor=EXP(-wn*dvp0*dvp0/rr0)
#endif

  itor=rr0
  psitor=1.-rho*rho

  i=ngdx+nx0+1
  j0=fsfc(i)%ny

  IF (write_diags) THEN

     write (10,*) 'major and minor radii and axis',rmajor,aminor,raxis,zaxis
     write (10,*) 'LCFS at i and ra = ',i,ra(i)
     write (10,*) 'bndy sfc',j0,' points'
     write (10,110) fsbndyeta
     write (10,*) 'R bndy sfc',j0,' points'
     write (10,110) fsbndyr
     write (10,*) 'z bndy sfc',j0,' points'
     write (10,110) fsbndyz
     CLOSE (10)

  END IF

110 FORMAT(5g15.7)

END SUBROUTINE GetBndy


#ifndef ITMCPOs
SUBROUTINE L3interp(y_in,x_in,nr_in,y_out,x_out,nr_out)

  IMPLICIT NONE

  INTEGER :: nr_in,nr_out
  REAL, DIMENSION(nr_in), INTENT(IN) :: y_in,x_in
  REAL, DIMENSION(nr_out), INTENT(IN) :: x_out
  REAL, DIMENSION(nr_out), INTENT(OUT) :: y_out

  REAL :: x,aintm,aint0,aint1,aint2,xm,x0,x1,x2
  INTEGER :: j,jm,j0,j1,j2
  INTEGER :: jstart,jfirst,jlast,jstep

  IF (x_in(nr_in) > x_in(1)) THEN
     jstart=3
     jfirst=1
     jlast=nr_out
     jstep=1
  ELSE
     jstart=nr_out-2
     jfirst=nr_out
     jlast=1
     jstep=-1
  END IF

  j1=jstart
  DO j=jfirst,jlast,jstep
     x=x_out(j)
     DO WHILE (x >= x_in(j1) .AND. j1 < nr_in-1 .AND. j1 > 2) 
        j1=j1+jstep
     END DO

     j2=j1+jstep
     j0=j1-jstep
     jm=j1-2*jstep

!...  extrapolate inside or outside

     x2=x_in(j2)
     x1=x_in(j1)
     x0=x_in(j0)
     xm=x_in(jm)

     aintm=(x-x0)*(x-x1)*(x-x2)/((xm-x0)*(xm-x1)*(xm-x2))
     aint0=(x-xm)*(x-x1)*(x-x2)/((x0-xm)*(x0-x1)*(x0-x2))
     aint1=(x-xm)*(x-x0)*(x-x2)/((x1-xm)*(x1-x0)*(x1-x2))
     aint2=(x-xm)*(x-x0)*(x-x1)/((x2-xm)*(x2-x0)*(x2-x1))

     y_out(j)=aintm*y_in(jm)+aint0*y_in(j0) &
          +aint1*y_in(j1)+aint2*y_in(j2)

  END DO

END SUBROUTINE L3interp


SUBROUTINE L3deriv(y_in,x_in,nr_in,dydx_out,x_out,nr_out)

  IMPLICIT NONE

  INTEGER :: nr_in,nr_out
  REAL, DIMENSION(nr_in), INTENT(IN) :: y_in,x_in
  REAL, DIMENSION(nr_out), INTENT(IN) :: x_out
  REAL, DIMENSION(nr_out), INTENT(OUT) :: dydx_out

  REAL :: x,aintm,aint0,aint1,aint2,xm,x0,x1,x2
  INTEGER :: j,jm,j0,j1,j2
  INTEGER :: jstart,jfirst,jlast,jstep

  IF (x_in(nr_in) > x_in(1)) THEN
     jstart=3
     jfirst=1
     jlast=nr_out
     jstep=1
  ELSE
     jstart=nr_out-2
     jfirst=nr_out
     jlast=1
     jstep=-1
  END IF

  j1=jstart
  DO j=jfirst,jlast,jstep
     x=x_out(j)
     DO WHILE (x >= x_in(j1) .AND. j1 < nr_in-1 .AND. j1 > 2) 
        j1=j1+jstep
     END DO

     j2=j1+jstep
     j0=j1-jstep
     jm=j1-2*jstep

!...  extrapolate inside or outside

     x2=x_in(j2)
     x1=x_in(j1)
     x0=x_in(j0)
     xm=x_in(jm)

     aintm=((x-x1)*(x-x2)+(x-x0)*(x-x2)+(x-x0)*(x-x1)) &
          /((xm-x0)*(xm-x1)*(xm-x2))
     aint0=((x-x1)*(x-x2)+(x-xm)*(x-x2)+(x-xm)*(x-x1)) &
          /((x0-xm)*(x0-x1)*(x0-x2))
     aint1=((x-x0)*(x-x2)+(x-xm)*(x-x2)+(x-xm)*(x-x0)) &
          /((x1-xm)*(x1-x0)*(x1-x2))
     aint2=((x-x0)*(x-x1)+(x-xm)*(x-x1)+(x-xm)*(x-x0)) &
          /((x2-xm)*(x2-x0)*(x2-x1))

     dydx_out(j)=aintm*y_in(jm)+aint0*y_in(j0) &
          +aint1*y_in(j1)+aint2*y_in(j2)

  END DO

END SUBROUTINE L3deriv


SUBROUTINE Lderiv(y,x,dydx,nx)

  IMPLICIT NONE

  INTEGER :: nx
  REAL :: dxp,dxm,dx0
  REAL, DIMENSION(nx), INTENT(IN) :: x,y
  REAL, DIMENSION(nx), INTENT(OUT) :: dydx

  INTEGER :: i,ip,im

  DO i=2,nx-1
     ip=MIN(i+1,nx)
     im=MAX(i-1,1)
     dxp=x(ip)-x(i)
     dxm=x(i)-x(im)
     dx0=x(ip)-x(im)
     dydx(i)=(dxm*dxm*(y(ip)-y(i))+dxp*dxp*(y(i)-y(im)))/(dxm*dxp*dx0)
  END DO
  dydx(1)=2.*dydx(2) - dydx(3)
  dydx(nx)=2.*dydx(nx-1) - dydx(nx-2)

END SUBROUTINE Lderiv
#endif
