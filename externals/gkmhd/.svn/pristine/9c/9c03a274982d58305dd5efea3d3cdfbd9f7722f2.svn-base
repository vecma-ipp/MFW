SUBROUTINE PutEquil(eq)

  USE Euitm_Schemas
  USE L3interps

  USE Coeff
  USE Vars

  IMPLICIT NONE

  TYPE (type_equilibrium) ::  eq

  INTEGER :: i,j,i0,i1,i2,j0,j1,j2,mu
  INTEGER :: jj,location(1)
  REAL :: eta,aminor,rmajor,raxis_out,zaxis_out
  REAL :: r0,r1,r2,dvol,darea,drr,dzz,dss,drr0
  INTEGER :: muc,mux,m0,mp,mm
  REAL :: area,areat,det,drdn,drdx,dzdn,dzdx,dndr,dndz,dxdr,dxdz
  REAL :: dpsidn,dpsidx,dpsidr,dpsidz,dthdr,dthdz
  REAL :: b2,gm1,gm2,gm3,gm4,gm5,gm6,gm7,gm8,gm9,bav,bmin,bmax
  REAL :: drhodpsi,drhodpsi2,grr,g11
  REAL :: thxm
  REAL :: drdpsi,dzdpsi,drdth,dzdth,dth,gjac
  REAL :: dzp,dzm,theta_axis
  INTEGER :: np_eq

  REAL(R8), DIMENSION(:), ALLOCATABLE :: thetas
  REAL(R8), DIMENSION(:), ALLOCATABLE :: &
       strip_eta,strip_dthetadeta,strip_theta,strip_rr,strip_zz

  REAL(R8), DIMENSION(:), POINTER :: psi_eq, pressure_eq, jphi_eq, &
       ftrap, dpsidrho, gm2_eq
  REAL(R8) :: volume, pvol, parea, barea, length, wmag, i_plasma
  REAL(R8), DIMENSION(:,:), POINTER :: rr_eq,zz_eq

!...  allocate output CPO pieces

  nr_eq=nx0+1
!  nchi_eq=nx0
  i1=1+ngdx
  i2=i1+nx0

  allocate(eq%profiles_1d%psi(nr_eq))
  allocate(eq%profiles_1d%phi(nr_eq))
  allocate(eq%profiles_1d%pressure(nr_eq))
  allocate(eq%profiles_1d%F_dia(nr_eq))
  allocate(eq%profiles_1d%pprime(nr_eq))
  allocate(eq%profiles_1d%ffprime(nr_eq))
  allocate(eq%profiles_1d%jphi(nr_eq))
  allocate(eq%profiles_1d%jparallel(nr_eq))
  allocate(eq%profiles_1d%q(nr_eq))
  allocate(eq%profiles_1d%r_inboard(nr_eq))
  allocate(eq%profiles_1d%r_outboard(nr_eq))
  allocate(eq%profiles_1d%rho_vol(nr_eq))
  allocate(eq%profiles_1d%rho_tor(nr_eq))
  allocate(eq%profiles_1d%elongation(nr_eq))
  allocate(eq%profiles_1d%tria_upper(nr_eq))
  allocate(eq%profiles_1d%tria_lower(nr_eq))
  allocate(eq%profiles_1d%volume(nr_eq))
  allocate(eq%profiles_1d%dvdrho(nr_eq))
  allocate(eq%profiles_1d%surface(nr_eq))
  allocate(eq%profiles_1d%area(nr_eq))
  allocate(eq%profiles_1d%vprime(nr_eq))
  allocate(eq%profiles_1d%aprime(nr_eq))
  allocate(eq%profiles_1d%dpsidrho_tor(nr_eq))
  allocate(eq%profiles_1d%gm1(nr_eq))
  allocate(eq%profiles_1d%gm2(nr_eq))
  allocate(eq%profiles_1d%gm3(nr_eq))
  allocate(eq%profiles_1d%gm4(nr_eq))
  allocate(eq%profiles_1d%gm5(nr_eq))
  allocate(eq%profiles_1d%gm6(nr_eq))
  allocate(eq%profiles_1d%gm7(nr_eq))
  allocate(eq%profiles_1d%gm8(nr_eq))
  allocate(eq%profiles_1d%gm9(nr_eq))
  allocate(eq%profiles_1d%ftrap(nr_eq))
  allocate(eq%profiles_1d%beta_pol(nr_eq))
  allocate(eq%profiles_1d%li(nr_eq))
  allocate(eq%profiles_1d%b_av(nr_eq))
  allocate(eq%profiles_1d%b_min(nr_eq))
  allocate(eq%profiles_1d%b_max(nr_eq))
  allocate(eq%profiles_1d%omega(nr_eq))
  allocate(eq%profiles_1d%omegaprime(nr_eq))
  allocate(eq%profiles_1d%mach_a(nr_eq))
  allocate(eq%profiles_1d%phi_flow(nr_eq))
  allocate(eq%profiles_1d%s_flow(nr_eq))
  allocate(eq%profiles_1d%h_flow(nr_eq))
  allocate(eq%profiles_1d%rho_mass(nr_eq))

  IF (cpo_choice == "GEMT") THEN

  np_eq = 0
  DO i=i1,i2
     np_eq=np_eq+fsfc(i)%ny0
  END DO

  allocate(eq%coord_sys%grid%dim1(np_eq))
  allocate(eq%coord_sys%grid%dim2(np_eq))
  allocate(eq%coord_sys%grid_type(4))
  eq%coord_sys%grid_type(1) = "triangle grid in R Z"
  eq%coord_sys%grid_type(2) = "flux surfaces as RZ strips of length 6x radius"
  eq%coord_sys%grid_type(3) = "axis is a single point the 1st one"
  eq%coord_sys%grid_type(4) = "X point if present is the last one"

  j2=0
  DO i0=1,nx0+1
     i=i0+ngdx
     j0=fsfc(i)%ny0
     DO j1=1,j0
        j=j1+ngdy

        j2=j2+1
        eq%coord_sys%grid%dim1(j2)=fsfc(i)%rtor(j)*a00
        eq%coord_sys%grid%dim2(j2)=fsfc(i)%ztor(j)*a00
     END DO
  END DO

  write (0,*) 'dim check np_eq and j2 are ',np_eq,j2

  END IF

  IF (cpo_choice == "SFL") THEN

  ALLOCATE(thetas(nchi_eq))
  thetas=(/ ((j-1)*tpi/REAL(nchi_eq),j=1,nchi_eq) /)

  allocate(eq%coord_sys%grid%dim1(nr_eq))
  allocate(eq%coord_sys%grid%dim2(nchi_eq))

  eq%coord_sys%grid%dim1=rho(i1:i2)
  eq%coord_sys%grid%dim2=thetas

  allocate(eq%coord_sys%position%r(nr_eq, nchi_eq))
  allocate(eq%coord_sys%position%z(nr_eq, nchi_eq))
  allocate(eq%coord_sys%g_11(nr_eq, nchi_eq))
  allocate(eq%coord_sys%g_12(nr_eq, nchi_eq))
  allocate(eq%coord_sys%g_22(nr_eq, nchi_eq))
  allocate(eq%coord_sys%g_13(nr_eq, nchi_eq))
  allocate(eq%coord_sys%g_23(nr_eq, nchi_eq))
  allocate(eq%coord_sys%g_33(nr_eq, nchi_eq))
  allocate(eq%coord_sys%jacobian(nr_eq, nchi_eq))

  eq%coord_sys%g_11=0.
  eq%coord_sys%g_12=0.
  eq%coord_sys%g_13=0.
  eq%coord_sys%g_22=0.
  eq%coord_sys%g_23=0.
  eq%coord_sys%g_33=0.
  eq%coord_sys%jacobian=0.

  rr_eq => eq%coord_sys%position%r
  zz_eq => eq%coord_sys%position%z
  psi_eq => eq%profiles_1d%psi

  allocate(eq%coord_sys%grid_type(4))
  eq%coord_sys%grid_type(1) = "2"
  eq%coord_sys%grid_type(2) = "inverse"
  eq%coord_sys%grid_type(3) = "1"
  eq%coord_sys%grid_type(4) = "straight field line"
!  eq%coord_sys%grid_type(1) = "symmetry coordinates"
!  eq%coord_sys%grid_type(2) = "same as straight field line coords"

  END IF

!...  put parameters and axis position

  raxis_out=raxis*a00
  zaxis_out=zaxis*a00

  eq%eqgeometry%a_minor=a00
  eq%global_param%toroid_field%b0=b00
  eq%global_param%toroid_field%r0=r00
  eq%global_param%mag_axis%position%r=raxis_out
  eq%global_param%mag_axis%position%z=zaxis_out
  eq%global_param%mag_axis%bphi=itor(i1)*b00*a00/raxis_out
  eq%global_param%mag_axis%q=qtor(i1)

  eq%eqgeometry%geom_axis%r=raxis_out
  eq%eqgeometry%geom_axis%z=zaxis_out

!...  put simple profiles

  eq%profiles_1d%rho_tor=SQRT(phitor(i1:i2)*a00*a00/pi)
  eq%profiles_1d%rho_vol=rho_vol(i1:i2)
  eq%profiles_1d%volume=vtor(i1:i2)*a00*a00*a00

  eq%profiles_1d%pressure=p00*ptor(i1:i2)
  eq%profiles_1d%jphi=j00*jtor(i1:i2)
  eq%profiles_1d%psi=(psitor(i1)-psitor(i1:i2))*b00*a00*a00*tpi
  eq%profiles_1d%phi=phitor(i1:i2)*b00*a00*a00
  eq%profiles_1d%F_dia=itor(i1:i2)*b00*a00
  eq%profiles_1d%q=qtor(i1:i2)

  eq%profiles_1d%jparallel=j00*jtor(i1:i2)

!...  put profiles with derivatives

  dvp0(1:nr_eq)=eq%profiles_1d%psi
  dvp1(1:nr_eq)=eq%profiles_1d%F_dia*eq%profiles_1d%F_dia/2.
  CALL L3deriv(dvp1,dvp0,nr_eq,eq%profiles_1d%ffprime,dvp0,nr_eq)
  dvp1(1:nr_eq)=eq%profiles_1d%pressure
  CALL L3deriv(dvp1,dvp0,nr_eq,eq%profiles_1d%pprime,dvp0,nr_eq)
  dvp1(1:nr_eq)=eq%profiles_1d%rho_tor
  CALL L3deriv(dvp0,dvp1,nr_eq,eq%profiles_1d%dpsidrho_tor,dvp1,nr_eq)
  eq%profiles_1d%dpsidrho_tor(1)=0.
  dvp1(1:nr_eq)=eq%profiles_1d%volume
  CALL L3deriv(dvp1,dvp0,nr_eq,eq%profiles_1d%vprime,dvp0,nr_eq)
  dvp0(1:nr_eq)=eq%profiles_1d%rho_tor
  CALL L3deriv(dvp1,dvp0,nr_eq,eq%profiles_1d%dvdrho,dvp0,nr_eq)
  eq%profiles_1d%dvdrho(1)=0.

!...  load rho_tor

  DO i=1,nx
     fsfc(i)%uu(0,nvars,:)=rho_tor(i)
  END DO
  CALL Neighbors(nvars,nvars)
  i=nx0+1+ngdx
  j0=fsfc(i)%ny0/6
  DO mu=0,5
     j1=mu*j0+1+ngdy
     j2=mu*j0+j0+ngdy
     DO j=j1,j2
        fsfc(i)%uu(1,:,j)=fsfc(i)%uu(0,:,j+1)
     END DO
     fsfc(i)%uu(2,:,j2)=fsfc(i-1)%uu(0,:,j2-mu)
  END DO

!...  put geometrical profiles already to hand

  eq%profiles_1d%gm1=fsavgr2(i1:i2)
  eq%profiles_1d%gm9=fsavgr1(i1:i2)

!...  put other quantities involving triangle positions

  eq%profiles_1d%r_outboard(1)=eq%eqgeometry%geom_axis%r
  eq%profiles_1d%r_inboard(1)=eq%eqgeometry%geom_axis%r
  eq%profiles_1d%elongation(1)=1.0
  eq%profiles_1d%tria_upper(1)=0.0
  eq%profiles_1d%tria_lower(1)=0.0
  eq%profiles_1d%area(1)=0.0
  eq%profiles_1d%surface(1)=0.0
  eq%profiles_1d%li(1)=0.0

  muc=nvars-2
  mux=nvars-1

  DO i0=2,nx0+1
     i=i0+ngdx
     j0=fsfc(i)%ny0
     j1=1+ngdy
     j2=j0+ngdy

!...  strip positions

     ALLOCATE(strip_eta(2*j0))
     ALLOCATE(strip_dthetadeta(2*j0))
     ALLOCATE(strip_theta(2*j0))
     ALLOCATE(strip_rr(2*j0))
     ALLOCATE(strip_zz(2*j0))

     strip_eta(1:j0)=fsfc(i)%clocketa(j1:j2)
     strip_rr(1:j0)=fsfc(i)%rtor(j1:j2)*a00
     strip_zz(1:j0)=fsfc(i)%ztor(j1:j2)*a00

     strip_eta(j0+1:j0+j0)=strip_eta(1:j0) + tpi
     strip_rr(j0+1:j0+j0)=strip_rr(1:j0)
     strip_zz(j0+1:j0+j0)=strip_zz(1:j0)

!...  put surface geometric quantities

     r2=MAXVAL(strip_rr(1:j0))
     r1=MINVAL(strip_rr(1:j0))
     r0=2./(r1+r2)
     eq%profiles_1d%r_outboard(i0)=r2
     eq%profiles_1d%r_inboard(i0)=r1

     eq%profiles_1d%elongation(i0)= &
          (MAXVAL(strip_zz(1:j0))-MINVAL(strip_zz(1:j0)))/ &
          (MAXVAL(strip_rr(1:j0))-MINVAL(strip_rr(1:j0)))
     location=MAXLOC(strip_zz(1:j0))
     jj=location(1)
     eq%profiles_1d%tria_upper(i0)= - (strip_rr(jj)-raxis_out)*r0
     location=MINLOC(strip_zz(1:j0))
     jj=location(1)
     eq%profiles_1d%tria_lower(i0)= - (strip_rr(jj)-raxis_out)*r0

!...  put boundary surface and LCFS quantities

     IF (i0 == nr_eq) THEN
        allocate(eq%eqgeometry%boundary(1))
        allocate(eq%eqgeometry%boundary(1)%r(j0))
        allocate(eq%eqgeometry%boundary(1)%z(j0))

        eq%eqgeometry%elongation=eq%profiles_1d%elongation(i0)
        eq%eqgeometry%tria_upper=eq%profiles_1d%tria_upper(i0)
        eq%eqgeometry%tria_lower=eq%profiles_1d%tria_lower(i0)
        eq%eqgeometry%boundary(1)%r=fsbndyr(j1:j2)*a00
        eq%eqgeometry%boundary(1)%z=fsbndyz(j1:j2)*a00
     END IF

!...  put surface geometric quantities

     drr0=0.
     dss=0.
     area=0.
     gm2=0.
     gm3=0.
     gm4=0.
     gm5=0.
     gm6=0.
     gm7=0.
     gm8=0.
     bav=0.
     bmin=9.e99
     bmax=0.

     strip_dthetadeta=0.

     drhodpsi=qtor(i)/rho_tor(i)
     drhodpsi2=drhodpsi*drhodpsi

     j0=fsfc(i)%ny0/6
     DO mu=0,5

        j1=mu*j0+1+ngdy
        j2=mu*j0+j0+ngdy

        DO j=j1,j2
           jj=j-ngdy
           duu(:,1:nvars)=fsfc(i)%uu(:,1:nvars,j)

           DO m0=1,2
              IF (m0 == 2 .AND. j == j1) CYCLE
              mp=m0+1

              drdn=duu(m0,muc)-duu(0,muc)
              drdx=duu(mp,muc)-duu(0,muc)
              dzdn=duu(m0,mux)-duu(0,mux)
              dzdx=duu(mp,mux)-duu(0,mux)

              det=1./(drdn*dzdx-drdx*dzdn)
              dndr=dzdx*det
              dndz=-drdx*det
              dxdr=-dzdn*det
              dxdz=drdn*det

              dpsidn=duu(m0,mupsi)-duu(0,mupsi)
              dpsidx=duu(mp,mupsi)-duu(0,mupsi)
              dpsidr=dpsidn*dndr+dpsidx*dxdr
              dpsidz=dpsidn*dndz+dpsidx*dxdz

              dpsidn=duu(m0,nvars)-duu(0,nvars)
              dpsidx=duu(mp,nvars)-duu(0,nvars)
              r1=dpsidn*dndr+dpsidx*dxdr
              r2=dpsidn*dndz+dpsidx*dxdz

              g11=dpsidr*dpsidr+dpsidz*dpsidz
              grr=r1*r1+r2*r2
!              grr=g11*drhodpsi2

              drr=(duu(0,muc)+duu(m0,muc)+duu(mp,muc))/3.
              darea=( &
                   (duu(m0,muc)+duu(0,muc))*(duu(m0,mux)-duu(0,mux)) &
                   - (duu(m0,muc)-duu(0,muc))*(duu(m0,mux)+duu(0,mux)) &
                   + (duu(mp,muc)+duu(m0,muc))*(duu(mp,mux)-duu(m0,mux)) &
                   - (duu(mp,muc)-duu(m0,muc))*(duu(mp,mux)+duu(m0,mux)) &
                   + (duu(0,muc)+duu(mp,muc))*(duu(0,mux)-duu(mp,mux)) &
                   - (duu(0,muc)-duu(mp,muc))*(duu(0,mux)+duu(mp,mux)) )/4.

              area=area+darea
              darea=darea*drr
              drr0=drr0+darea
              r2=drr*drr
              b2=(itor(i)*itor(i)+g11)/r2

              gm2=gm2+darea*grr/r2
              gm3=gm3+darea*grr
              gm4=gm4+darea/b2
              gm5=gm5+darea*b2
              gm6=gm6+darea*grr/b2
              gm7=gm7+darea*SQRT(grr)
              gm8=gm8+darea*drr
              b2=SQRT(b2)
              bav=bav+darea*b2
              bmin=MIN(bmin, b2)
              bmax=MAX(bmax, b2)
           END DO

           drr=duu(1,muc)-duu(0,muc)
           dzz=duu(1,mux)-duu(0,mux)
           dss=dss+SQRT(drr*drr+dzz*dzz)*(duu(1,muc)+duu(0,muc))

           thxm= - strip_rr(jj)*strip_rr(jj)
           thxm=thxm/(strip_rr(jj)*fsfc(i)%area2(j))

           DO m0=1,6
              mp=m0+1
              mm=m0-1
              IF (m0 == 6) mp=1
              IF (m0 == 1) mm=6

              strip_dthetadeta(jj)=strip_dthetadeta(jj)+thxm*( &
                   (duu(mp,mutheta)-duu(mm,mutheta))*duu(m0,mupsi) )
           END DO

        END DO
     END DO
     eq%profiles_1d%area(i0)=eq%profiles_1d%area(i0-1)+area
     eq%profiles_1d%surface(i0)=dss
     eq%profiles_1d%gm2(i0)=gm2/drr0
     eq%profiles_1d%gm3(i0)=gm3/drr0
     eq%profiles_1d%gm4(i0)=gm4/drr0
     eq%profiles_1d%gm5(i0)=gm5/drr0
     eq%profiles_1d%gm6(i0)=gm6/drr0
     eq%profiles_1d%gm7(i0)=gm7/drr0
     eq%profiles_1d%gm8(i0)=gm8/drr0
     eq%profiles_1d%b_av(i0)=bav/drr0
     eq%profiles_1d%b_min(i0)=bmin
     eq%profiles_1d%b_max(i0)=bmax
!     eq%profiles_1d%li(i0)=

!...  finish theta

     IF (cpo_choice == "SFL") THEN

     IF (i0 < nr_eq) THEN
        j0=fsfc(i)%ny0
        strip_dthetadeta(j0+1:j0+j0)=strip_dthetadeta(1:j0)
        strip_theta(1)=0.
        DO j=2,j0+1
           strip_theta(j)=strip_theta(j-1)+(strip_eta(j)-strip_eta(j-1))/ &
                (strip_dthetadeta(j)+strip_dthetadeta(j-1))
        END DO
        strip_theta(1:j0)=strip_theta(1:j0)*tpi/strip_theta(j0+1)
        DO j=1,j0
           IF ((strip_zz(j+1)-zaxis_out)*(strip_zz(j)-zaxis_out) < 0.) THEN
              dzp=strip_zz(j+1)-zaxis_out
              dzm=zaxis_out-strip_zz(j)
              theta_axis=(dzp*strip_theta(j)+dzm*strip_theta(j+1))/(dzp+dzm)
              EXIT
           END IF
        END DO

!        dzp*(theta_axis - theta- ) = dzm*(theta+ - theta_axis)
!        dzp*theta- + dzm*theta+ = (dzp+dzm)*theta_axis
!        theta_axis = (dzp*theta- + dzm*theta+)/(dzp+dzm)

        strip_theta(j0+1:j0+j0)=strip_theta(1:j0) + tpi
!        strip_theta=strip_theta-strip_theta(1+j0/3)
        strip_theta=strip_theta-theta_axis

!...  put coord sys positions

        CALL L3interp(strip_rr,strip_theta,2*j0,dvpy1,thetas,nchi_eq)
        DO jj=1,nchi_eq
           rr_eq(i0,jj)=dvpy1(jj)
        END DO
        CALL L3interp(strip_zz,strip_theta,2*j0,dvpy1,thetas,nchi_eq)
        DO jj=1,nchi_eq
           zz_eq(i0,jj)=dvpy1(jj)
        END DO

     ELSE
        rr_eq(i0,:)=2.*rr_eq(i0-1,:)-rr_eq(i0-2,:)
        zz_eq(i0,:)=2.*zz_eq(i0-1,:)-zz_eq(i0-2,:)
     END IF

     END IF

!...  deallocate strip positions

     DEALLOCATE(strip_eta)
     DEALLOCATE(strip_dthetadeta)
     DEALLOCATE(strip_theta)
     DEALLOCATE(strip_rr)
     DEALLOCATE(strip_zz)

  END DO

!...  deallocate SFL theta 

  IF (cpo_choice == "SFL") DEALLOCATE(thetas)

!...  normalisation and derivatives for the area

  eq%profiles_1d%area=eq%profiles_1d%area*a00*a00
  eq%profiles_1d%surface=eq%profiles_1d%surface*a00*a00*pi

  dvp0(1:nr_eq)=eq%profiles_1d%psi

  dvp1(1:nr_eq)=eq%profiles_1d%area
  CALL L3deriv(dvp1,dvp0,nr_eq,eq%profiles_1d%aprime,dvp0,nr_eq)

!  dvp1(1:nr_eq)=eq%profiles_1d%phi
!  CALL L3deriv(dvp1,dvp0,nr_eq,eq%profiles_1d%q,dvp0,nr_eq)

!...  special handling and normalisation for the gms and B profiles

  eq%profiles_1d%gm2(1)=2.*eq%profiles_1d%gm2(3)-eq%profiles_1d%gm2(4)
  eq%profiles_1d%gm3(1)=2.*eq%profiles_1d%gm3(3)-eq%profiles_1d%gm3(4)
  eq%profiles_1d%gm4(1)=2.*eq%profiles_1d%gm4(3)-eq%profiles_1d%gm4(4)
  eq%profiles_1d%gm5(1)=2.*eq%profiles_1d%gm5(3)-eq%profiles_1d%gm5(4)
  eq%profiles_1d%gm6(1)=2.*eq%profiles_1d%gm6(3)-eq%profiles_1d%gm6(4)
  eq%profiles_1d%gm7(1)=2.*eq%profiles_1d%gm7(3)-eq%profiles_1d%gm7(4)
  eq%profiles_1d%gm8(1)=2.*eq%profiles_1d%gm8(3)-eq%profiles_1d%gm8(4)

  eq%profiles_1d%gm2(2)=(eq%profiles_1d%gm2(1)+eq%profiles_1d%gm2(3))/2.
  eq%profiles_1d%gm3(2)=(eq%profiles_1d%gm3(1)+eq%profiles_1d%gm3(3))/2.
  eq%profiles_1d%gm4(2)=(eq%profiles_1d%gm4(1)+eq%profiles_1d%gm4(3))/2.
  eq%profiles_1d%gm5(2)=(eq%profiles_1d%gm5(1)+eq%profiles_1d%gm5(3))/2.
  eq%profiles_1d%gm6(2)=(eq%profiles_1d%gm6(1)+eq%profiles_1d%gm6(3))/2.
  eq%profiles_1d%gm7(2)=(eq%profiles_1d%gm7(1)+eq%profiles_1d%gm7(3))/2.
  eq%profiles_1d%gm8(2)=(eq%profiles_1d%gm8(1)+eq%profiles_1d%gm8(3))/2.

  eq%profiles_1d%gm1=eq%profiles_1d%gm1/(a00*a00)
  eq%profiles_1d%gm2=eq%profiles_1d%gm2/(a00*a00)
  eq%profiles_1d%gm4=eq%profiles_1d%gm4/(b00*b00)
  eq%profiles_1d%gm5=eq%profiles_1d%gm5*(b00*b00)
  eq%profiles_1d%gm6=eq%profiles_1d%gm6/(b00*b00)
  eq%profiles_1d%gm8=eq%profiles_1d%gm8*a00
  eq%profiles_1d%gm9=eq%profiles_1d%gm9/a00

  eq%profiles_1d%b_av(1)=0.
  eq%profiles_1d%b_min(1)=0.
  eq%profiles_1d%b_max(1)=0.

  eq%profiles_1d%b_av=eq%profiles_1d%b_av*b00
  eq%profiles_1d%b_min=eq%profiles_1d%b_min*b00
  eq%profiles_1d%b_max=eq%profiles_1d%b_max*b00

  eq%profiles_1d%b_av(1)=ABS(eq%global_param%mag_axis%bphi)
  eq%profiles_1d%b_min(1)=ABS(eq%global_param%mag_axis%bphi)
  eq%profiles_1d%b_max(1)=ABS(eq%global_param%mag_axis%bphi)

!...  trapped fraction for zero nu star from Wesson p 671

  ftrap => eq%profiles_1d%ftrap

  ftrap=eq%profiles_1d%rho_vol*a00/r00
  ftrap=1._R8 - ((1._R8-ftrap)**2._R8) &
       /( SQRT((1._R8-ftrap)**2._R8)*(1._R8+1.46_R8*SQRT(ftrap)) )

!...  coord sys stuff on the theta grid

  IF (cpo_choice == "SFL") THEN

  rr_eq(1,:)=eq%global_param%mag_axis%position%r
  zz_eq(1,:)=eq%global_param%mag_axis%position%z

  dvp0(2:nr_eq-1)=1./(psi_eq(3:nr_eq)-psi_eq(1:nr_eq-2))
  dth=REAL(nchi_eq)/tpi
  DO j=1,nchi_eq
     j2=j+1
     IF (j == nchi_eq) j2=1
     j1=j-1
     IF (j == 1) j1=nchi_eq
     DO i=2,nr_eq-1
        drdpsi=(rr_eq(i+1,j)-rr_eq(i-1,j))*dvp0(i)
        dzdpsi=(zz_eq(i+1,j)-zz_eq(i-1,j))*dvp0(i)
        drdth=(rr_eq(i,j2)-rr_eq(i,j1))*dth
        dzdth=(zz_eq(i,j2)-zz_eq(i,j1))*dth

        gjac=1./(drdpsi*dzdth-drdth*dzdpsi)
        eq%coord_sys%jacobian(i,j)=gjac

        dpsidr=gjac*dzdth
        dpsidz= - gjac*drdth
        dthdr= - gjac*dzdpsi
        dthdz=gjac*drdpsi
        eq%coord_sys%g_11(i,j)=dpsidr*dpsidr+dpsidz*dpsidz
        eq%coord_sys%g_12(i,j)=dpsidr*dthdr+dpsidz*dthdz
     END DO
  END DO
  i0=nr_eq
  eq%coord_sys%g_11(i0,:) = &
       2.*eq%coord_sys%g_11(i0-1,:) - eq%coord_sys%g_11(i0-2,:)
  eq%coord_sys%g_12(i0,:) = &
       2.*eq%coord_sys%g_12(i0-1,:) - eq%coord_sys%g_12(i0-2,:)
  eq%coord_sys%jacobian(i0,:) = &
       2.*eq%coord_sys%jacobian(i0-1,:) - eq%coord_sys%jacobian(i0-2,:)

  eq%coord_sys%g_22=(eq%coord_sys%jacobian*eq%coord_sys%jacobian &
       - eq%coord_sys%g_12*eq%coord_sys%g_12)/(1.e-10+eq%coord_sys%g_11)

  eq%coord_sys%g_33=1./(rr_eq*rr_eq)

  eq%coord_sys%jacobian=rr_eq/(1.e-10+eq%coord_sys%jacobian)

  eq%coord_sys%jacobian(1,:) = &
       2.*eq%coord_sys%jacobian(2,:) - eq%coord_sys%jacobian(3,:)

  END IF

!...  set remaining global parameters

  area=eq%profiles_1d%area(nr_eq)
  volume=eq%profiles_1d%volume(nr_eq)

  eq%global_param%volume = volume
  eq%global_param%area = area
  eq%global_param%psi_ax = 0.0
  eq%global_param%psi_bound = eq%profiles_1d%psi(nr_eq)
  eq%global_param%q_min = MINVAL(eq%profiles_1d%q)
  dvp1(1)=0.95*eq%profiles_1d%psi(nr_eq)
  CALL L3interp(eq%profiles_1d%q,eq%profiles_1d%psi,nr_eq, &
       dvp2,dvp1,1)
  eq%global_param%q_95 = dvp2(1)

  pressure_eq => eq%profiles_1d%pressure
  jphi_eq => eq%profiles_1d%jphi
  dpsidrho => eq%profiles_1d%dpsidrho_tor

  gm2_eq => eq%profiles_1d%gm2

  pvol=0.
  parea=0.
  i_plasma=0.
  wmag=0.

!  bpol2 d3V = dV < g11/R^2 > = dV (dPsi/dr)^2 < grr/R^2 >
!                        = dV (dpsidrho^2) * gm2

  DO i0=2,nr_eq
     i1=i0-1
     darea=0.5*(eq%profiles_1d%area(i0)-eq%profiles_1d%area(i1))
     dvol=0.5*(eq%profiles_1d%volume(i0)-eq%profiles_1d%volume(i1))

     pvol = pvol + dvol * (pressure_eq(i0)+pressure_eq(i1))
     parea = parea + darea * (pressure_eq(i0)+pressure_eq(i1))
     i_plasma = i_plasma + darea * (jphi_eq(i0)+jphi_eq(i1))
     wmag = wmag + dvol * (gm2_eq(i0)+gm2_eq(i1)) &
          * (0.5*(dpsidrho(i0)+dpsidrho(i1)))**2.
  END DO

  length=SUM( SQRT( (rr_eq(i0,:)-CSHIFT(rr_eq(i0,:),-1))**2.0 &
       + (zz_eq(i0,:)-CSHIFT(zz_eq(i0,:),-1))**2.0 ) )

  barea=mu_0*i_plasma/length

  eq%global_param%i_plasma = i_plasma
  eq%global_param%w_mhd=1.5_R8*pvol
  eq%global_param%gamma=5.0_R8/3.0_R8

  parea=parea/area
  pvol=pvol/volume
  eq%global_param%beta_tor=2.0_R8*mu_0*pvol/(b00*b00)
  eq%global_param%beta_pol=2.0_R8*mu_0*parea/(barea*barea)
  eq%global_param%beta_normal=(200.0e6_R8*b00/i_plasma)*eq%global_param%beta_tor

  wmag=wmag/(mu_0*tpi*i_plasma*tpi*i_plasma)
  wmag=(2.0/mu_0)*wmag*(tpi*area/volume)

  eq%global_param%li=wmag

!...  set remaining 1d profiles

  eq%profiles_1d%beta_pol=eq%global_param%beta_pol
  eq%profiles_1d%li=eq%global_param%li

  eq%profiles_1d%omega=0.
  eq%profiles_1d%omegaprime=0.
  eq%profiles_1d%mach_a=0.
  eq%profiles_1d%phi_flow=0.
  eq%profiles_1d%s_flow=0.
  eq%profiles_1d%h_flow=0.
  eq%profiles_1d%rho_mass=0.

!...  diagnostic output otherwise return with the CPO

  IF (write_diags) THEN

  OPEN (10,file=ffile,form='formatted',position='append')
  WRITE (10,*) 'output'
  WRITE (10,*) 'rho_tor  pressure   current   q'
  DO i=1,nr_eq
     WRITE (10,'(4g11.3)') &
          eq%profiles_1d%rho_tor(i), &
          eq%profiles_1d%pressure(i), &
          eq%profiles_1d%jphi(i), &
          eq%profiles_1d%q(i)
  END DO
  WRITE (10,*) 'rho_tor  ptor   jtor   qtor'
  DO i=1,nx
     WRITE (10,'(4g11.3)') &
          rho_tor(i), ptor(i), jtor(i), qtor(i)
  END DO
  CLOSE (10)

  END IF

110 FORMAT(5g15.7)

END SUBROUTINE PutEquil

