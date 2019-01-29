SUBROUTINE bndfit(RIN,ZIN,KIN,RFIT,ZFIT,NBFIT,TENSION,R0,RZ0,KOPTION)
  !
  ! Perform periodic spline interpolation with tension on rho(theta) to smooth plasma boundary
  ! Use 0.5*[Rmax+Rmin; Zmax+Zmin] as origin for the theta, rho mesh
  !
  ! koption = 1: (optional, default if not provided) provide new R,Z points in [rfit(i),zfit(i)], i=1,nbfit
  !           2: return theta, rho in rfit, zfit respectively (used for mesh packing)
  !
  USE prec_const
  USE globals, ONLY : NVERBOSE
  USE interpos_module
  IMPLICIT NONE
  INTEGER, intent(in) :: KIN, NBFIT
  INTEGER, optional :: KOPTION
  REAL(RKIND), intent(in) :: RIN(KIN), ZIN(KIN)
  REAL(RKIND), intent(out) :: RFIT(NBFIT), ZFIT(NBFIT) 
  REAL(RKIND), intent(in), optional :: TENSION, R0, RZ0
  !
  REAL(RKIND) :: TENSION_eff, ZRGEOM, ZZGEOM, zdtheta, zdx
  REAL(RKIND), ALLOCATABLE :: thetain(:), rhoin(:), theta_sorted(:), rho_sorted(:), &
       & thetaout(:), rhoout(:)
  INTEGER, ALLOCATABLE :: i_sorted(:)
  INTEGER :: NIN_eff, I, J, ioption
  !
  if (present(TENSION)) then
    TENSION_eff = TENSION
  else
    TENSION_eff = -1._RKIND
  end if
  if (present(koption)) then
    ioption = koption
  else
    ioption = 1
  end if
  !
  ! Use interpos with periodic boundary conditions to smooth boundary, otherwise gives bad resolution for CHEASE
  !
  ZRGEOM = (MINVAL(RIN(1:KIN)) + MAXVAL(RIN(1:KIN)))/2._RKIND
  ZZGEOM = (MINVAL(ZIN(1:KIN)) + MAXVAL(ZIN(1:KIN)))/2._RKIND
  if (present(R0))print *,'ZRGEOM, ZZGEOM, R0, Rz0= ',ZRGEOM, ZZGEOM, R0, Rz0
  if (present(R0)) ZRGEOM = R0
  if (present(RZ0)) ZZGEOM = RZ0
  if (present(R0))print *,'ZRGEOM, ZZGEOM, R0, Rz0= ',ZRGEOM, ZZGEOM, R0, Rz0
  !
  allocate(thetain(KIN))
  allocate(rhoin(KIN))
  do i=1,KIN
    thetain(i) = atan2((ZIN(i)-ZZGEOM),(RIN(i)-ZRGEOM))
    if (thetain(i) .lt. 0._rkind) thetain(i) = thetain(i) + TWOPI
    rhoin(i)=sqrt((RIN(i)-ZRGEOM)**2 + (ZIN(i)-ZZGEOM)**2)
  end do
  NIN_eff = KIN
  if (abs(thetain(NIN_eff)-thetain(1)) .lt. 1e-07_rkind) then
    if (NVERBOSE .GE. 3) write(0,*) 'in bndfit: end theta points given twice, remove it'
    NIN_eff = NIN_eff - 1
  end if
  ! sort rho mesh
  allocate(theta_sorted(NIN_eff))
  allocate(rho_sorted(NIN_eff))
  allocate(i_sorted(NIN_eff))
  call indexx(NIN_eff,thetain(1:NIN_eff),i_sorted)
  theta_sorted = thetain(i_sorted(1:NIN_eff))
  rho_sorted = rhoin(i_sorted(1:NIN_eff))
  !  write(22,'(i5,1p2e15.4)') (i,theta_sorted(i),rho_sorted(i),i=1,NIN_eff)
  !
  ! check that there are not 2 points too close  in rho (although now also done in interpos)
  i=1
  zdx=TWOPI / real(NIN_eff,rkind)
  do j=2,NIN_eff
    if (abs(theta_sorted(j)-theta_sorted(i)) .gt. 1e-6*zdx) then
      i=i+1;
      theta_sorted(i) = theta_sorted(j)
      rho_sorted(i) = rho_sorted(j)
    end if
  end do
  if ((nin_eff .GT. i) .AND. (NVERBOSE.GE.0)) write(0,*) &
       & 'Some points too close in theta and ignored in bndfit: nin_eff=',nin_eff,' reduced to ',i
  NIN_eff = i;
  ! write(23,'(i5,1p2e15.4)') (i,theta_sorted(i),rho_sorted(i),i=1,NIN_eff)
  allocate(thetaout(NBFIT))
  allocate(rhoout(NBFIT))
  zdtheta = TWOPI/REAL(NBFIT,RKIND)
  do i=1,NBFIT
    thetaout(i) = REAL(i-1,RKIND) * zdtheta
  end do
  call interpos(theta_sorted,rho_sorted,NIN_eff,nout=NBFIT,tension=TENSION_eff, &
    & xout=thetaout,yout=rhoout,nbc=-1,ybc=twopi)
  ! write(24,'(i5,1p2e15.4)') (i,thetaout(i),rhoout(i),i=1,NIN_eff)
  if (ioption .eq. 2) then
    RFIT(1:NBFIT) = thetaout(1:NBFIT)
    ZFIT(1:NBFIT) = rhoout(1:NBFIT)
  else
    do i=1,NBFIT
      RFIT(i) = ZRGEOM + cos(thetaout(i))*rhoout(i)
      ZFIT(i) = ZZGEOM + sin(thetaout(i))*rhoout(i)
    end do
  end if
  !
end SUBROUTINE bndfit
