module    schemas       ! declaration of minimal CPOs

INTEGER, PARAMETER :: RKIND = SELECTED_REAL_KIND(10)
REAL(RKIND), PARAMETER :: PI=3.141592653589793238462643383279502884197_rkind
integer, parameter, private :: DP=kind(1.0D0)

type type_profiles_1d  !    
  real(DP),pointer  :: phi(:) => null()     ! /profiles_1d/phi - toroidal flux [Wb]; Time-dependent; Vector (npsi)
endtype

type type_rz2D  !    Structure for list of R,Z positions (2D)
  real(DP),pointer  :: r(:,:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:,:) => null()     ! /z - Altitude [m]
endtype

type type_coord_sys  !    
   type (type_rz2D)  :: position  ! /coord_sys/position - R and Z position of grid points; Time-dependent; Matrix (ndim1, ndim2)
endtype

type type_equilibrium  !    
  type (type_profiles_1d) :: profiles_1d  ! /equilibrium/profiles_1d
  type (type_coord_sys) :: coord_sys  ! /equilibrium/coord_sys - 
endtype

!type type_profiles_1d  !    
!  real(DP),pointer  :: psi(:) => null()     ! /profiles_1d/psi - Poloidal flux [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (npsi)
!  real(DP),pointer  :: phi(:) => null()     ! /profiles_1d/phi - toroidal flux [Wb]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: pressure(:) => null()     ! /profiles_1d/pressure - pressure profile as a function of the poloidal flux [Pa]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: F_dia(:) => null()     ! /profiles_1d/F_dia - diamagnetic profile (R B_phi) [T m]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: pprime(:) => null()     ! /profiles_1d/pprime - psi derivative of the pressure profile [Pa/Wb]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: ffprime(:) => null()     ! /profiles_1d/ffprime - psi derivative of F_dia multiplied with F_dia [T^2 m^2/Wb]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: jphi(:) => null()     ! /profiles_1d/jphi - flux surface averaged toroidal current density = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: jparallel(:) => null()     ! /profiles_1d/jparallel - flux surface averaged parallel current density = average(j.B) / B0, where B0 = equilibrium/global_param/toroid_field/b0 ; [A/m^2];
!  real(DP),pointer  :: q(:) => null()     ! /profiles_1d/q - Safety factor = dphi/dpsi [-]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: r_inboard(:) => null()     ! /profiles_1d/r_inboard - radial coordinate (major radius) at the height and on the left of the magnetic axis [m]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: r_outboard(:) => null()     ! /profiles_1d/r_outboard - radial coordinate (major radius) at the height and on the right of the magnetic axis [m]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: rho_tor(:) => null()     ! /profiles_1d/rho_tor - Toroidal flux coordinate [m], to be used by the ETS and in many CPOs (coreprof, ...). Defined as sqrt(phi/pi/B0), where B0 = equil
!  real(DP),pointer  :: rho_vol(:) => null()     ! /profiles_1d/rho_vol - Normalised radial coordinate related to the plasma volume. Defined as sqrt(volume / volume[LCFS]). Time-dependent; Vector (npsi)
!  real(DP),pointer  :: beta_pol(:) => null()     ! /profiles_1d/beta_pol - poloidal beta (inside the magnetic surface); Time-dependent; Vector (npsi)
!  real(DP),pointer  :: li(:) => null()     ! /profiles_1d/li - internal inductance (inside the magnetic surface); Time-dependent; Vector (npsi)
!  real(DP),pointer  :: elongation(:) => null()     ! /profiles_1d/elongation - Elongation; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: tria_upper(:) => null()     ! /profiles_1d/tria_upper - Upper triangularity profile; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: tria_lower(:) => null()     ! /profiles_1d/tria_lower - Lower triangularity profile; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: volume(:) => null()     ! /profiles_1d/volume - Volume enclosed in the flux surface [m^3]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: vprime(:) => null()     ! /profiles_1d/vprime - Radial derivative of the volume enclosed in the flux surface, i.e. dV/drho_tor [m^2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: area(:) => null()     ! /profiles_1d/area - Cross-sectional area of the flux surface [m^2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: aprime(:) => null()     ! /profiles_1d/aprime - Radial derivative of the cross-sectional area of the flux surface, i.e. darea/drho_tor [m^2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: surface(:) => null()     ! /profiles_1d/surface - Surface area of the flux surface [m^2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: ftrap(:) => null()     ! /profiles_1d/ftrap - Trapped particle fraction; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm1(:) => null()     ! /profiles_1d/gm1 - average(1/R^2); Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm2(:) => null()     ! /profiles_1d/gm2 - average(grad_rho^2/R^2); Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm3(:) => null()     ! /profiles_1d/gm3 - average(grad_rho^2); Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm4(:) => null()     ! /profiles_1d/gm4 - average(1/B^2) [T^-2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm5(:) => null()     ! /profiles_1d/gm5 - average(B^2) [T^2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm6(:) => null()     ! /profiles_1d/gm6 - average(grad_rho^2/B^2)  [T^-2]; Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm7(:) => null()     ! /profiles_1d/gm7 - average(grad_rho); Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm8(:) => null()     ! /profiles_1d/gm8 - average(R); Time-dependent; Vector (npsi)
!  real(DP),pointer  :: gm9(:) => null()     ! /profiles_1d/gm9 - average(1/R); Time-dependent; Vector (npsi)
!endtype

end module

