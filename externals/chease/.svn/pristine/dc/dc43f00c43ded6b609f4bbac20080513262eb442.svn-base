!
MODULE sigmaneomod
  !
  ! Compute neoclassical conductivity using formula in Ref.1: O. Sauter et al, Phys. Plasmas 6 (1999) 2834.
  !
  ! Outputs:
  !    signeo : neoclassical conductivity at each value of the input parameters
  !    sigsptz: Spitzer conductivity
  !    nuestar: electron collisionality (default set to 0.01 if q, R, eps not given)
  !
  ! Inputs:
  !  Required:
  !    ft()   : trapped fraction (Note, can use formula in Lin-Liu et al, Phys. Plasmas 2 (1995) 1666.)
  !    ne()   : local electron density
  !    Te()   : Local electron temperature
  !  Optionals:
  !    Zeff() : effective charge (default is 2.)
  !    q()    : Local safety factor (if q, R or eps is not given, nuestar set to 0.01)
  !    R()    : geometrical center of local flux surface (si q for def)
  !    eps()  : local inverse aspect ratio (a/R) (si q for def)
  !    Nin    : number of input values (default to size(ft) if args are arrays but nin omitted)
  !
  ! Note: q, R and eps are used to compute nue* according to Eqs.(18b and 18d) of Ref.1
  !
  ! examples of calls:
  !
  ! call sigmaneo(signeo,sigsptz,nuestar,ft,ne,te) (defaults : zeff=1.5, nuestar=0.01)
  ! call sigmaneo(signeo,sigsptz,nuestar,ft,ne,te,zeff) (default for nuestar=0.01)
  ! call sigmaneo(signeo,sigsptz,nuestar,ft,ne,te,zeff,q,R,eps)
  ! call sigmaneo(signeo,sigsptz,nuestar,ft,ne,te,NIN=nin) (defaults : zeff=1.5, nuestar=0.01)
  !
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: sigmaneo
  !
  INTERFACE sigmaneo
     ! scalar (_s) or vector (_v) inputs
     MODULE PROCEDURE sigmaneo_s, sigmaneo_v
  END INTERFACE

  !
CONTAINS
  SUBROUTINE sigmaneo_s(signeo,sigsptz,nuestar,ft,ne,te,zeff,q,R,eps)
    ! CASE: all are scalar input variables (or ft(i) in argument, as is interpreted as scalar)
    ! te in [eV], ne in [m**-3], R in [m]
    INTEGER, PARAMETER :: RKIND = SELECTED_REAL_KIND(10)
    REAL(RKIND), INTENT(OUT) :: signeo, sigsptz, nuestar
    REAL(RKIND), INTENT(IN)  :: ft, ne, te
    REAL(RKIND), OPTIONAL, INTENT(IN) :: zeff
    REAL(RKIND), OPTIONAL, INTENT(IN) :: q
    REAL(RKIND), OPTIONAL, INTENT(IN) :: R
    REAL(RKIND), OPTIONAL, INTENT(IN) :: eps
    !
    REAL(RKIND)  :: z_zeff, zNZ, zlnL, zft33eff
    !
    z_zeff = 2.0_RKIND
    IF ( PRESENT(zeff) ) z_zeff = zeff
    !
    zNZ = 0.58_RKIND + 0.74_RKIND / (0.76_RKIND + z_zeff)
    zlnL = 17._RKIND
    IF (ne.gt.0._RKIND .and. te.gt.0._RKIND) THEN
      zlnL = 31.3_RKIND - log(sqrt(abs(ne))/abs(te+1.0E-14_RKIND))
    ENDIF
    zlnL = max(zlnL, 15._RKIND)
    sigsptz = 1.9012E+04_RKIND * ABS(te)**1.5_RKIND / (z_zeff * zNZ * zlnL)
    !
    nuestar = 0.01_RKIND
    IF (PRESENT(q) .AND. PRESENT(R) .AND. PRESENT(eps)) &
         & nuestar = 6.921E-18_RKIND * abs(q) * R * ne * z_zeff * zlnL / (te*te * abs(eps)**1.5_RKIND+1.E-14_RKIND)
    !
    zft33eff = ft / &
         & (1._RKIND+(0.55_RKIND-0.1_RKIND*ft)*sqrt(nuestar) &
         &  + 0.45_RKIND*(1._RKIND-ft)*nuestar/z_zeff**1.5_RKIND)
    signeo = sigsptz * (1._RKIND - zft33eff*(1._RKIND+0.36_RKIND/z_zeff &
         &                        - zft33eff*(0.59_RKIND/z_zeff - 0.23_RKIND/z_zeff*zft33eff)))
    !
  END SUBROUTINE sigmaneo_s
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE sigmaneo_v(signeo,sigsptz,nuestar,ft,ne,te,zeff,q,R,eps,NIN)
    ! CASE: all are vectors (except NIN). If NIN not given, use SIZE(ft) as dimension
    ! te in [eV], ne in [m**-3], R in [m]
    INTEGER, PARAMETER :: RKIND = SELECTED_REAL_KIND(10)
    REAL(RKIND), DIMENSION (:), INTENT(OUT) :: signeo, sigsptz, nuestar
    REAL(RKIND), DIMENSION (:), INTENT(IN)  :: ft, ne, te
    REAL(RKIND), OPTIONAL, DIMENSION (:), INTENT(IN)  :: zeff, q, R, eps
    INTEGER    , OPTIONAL, INTENT(IN) :: nin
    !
    REAL(RKIND), DIMENSION(:), ALLOCATABLE :: z_zeff, zNZ, zlnL, zft33eff
    INTEGER :: inin
    !-----------------------------------------------------------------------
    !
    inin = size(ft)
    IF ( PRESENT(NIN) ) inin = NIN
    !
    IF( .NOT. ALLOCATED(z_zeff) ) ALLOCATE(z_zeff(inin))
    IF( .NOT. ALLOCATED(zNZ) ) ALLOCATE(zNZ(inin))
    IF( .NOT. ALLOCATED(zlnL) ) ALLOCATE(zlnL(inin))
    IF( .NOT. ALLOCATED(zft33eff) ) ALLOCATE(zft33eff(inin))
    !
    z_zeff = 2.0_RKIND
    IF ( PRESENT(zeff) ) z_zeff = zeff(1:inin)
    !
    zNZ = 0.58_RKIND + 0.74_RKIND / (0.76_RKIND + z_zeff)
    zlnL = 17._RKIND
    IF (ne(inin).gt.0._RKIND .and. te(inin).gt.0._RKIND) THEN
      zlnL = 31.3_RKIND - log(sqrt(abs(ne(1:inin)))/abs(te(1:inin)+1.0E-14_RKIND))
    ENDIF
    sigsptz(1:inin) = 1.9012E+04_RKIND * ABS(te(1:inin))**1.5_RKIND / (z_zeff * zNZ * zlnL)
    nuestar(1:inin) = 0.01_RKIND
    IF (PRESENT(q) .AND. PRESENT(R) .AND. PRESENT(eps)) &
         & nuestar(1:inin) = 6.921E-18_RKIND * abs(q(1:inin)) * R(1:inin) * ne(1:inin) * z_zeff * zlnL &
         & / (te(1:inin)*te(1:inin) * abs(eps(1:inin))**1.5_RKIND+1.E-14_RKIND)
    !
    zft33eff = ft(1:inin) / &
         & (1._RKIND+(0.55_RKIND-0.1_RKIND*ft(1:inin))*sqrt(nuestar(1:inin)) &
         &  + 0.45_RKIND*(1._RKIND-ft(1:inin))*nuestar(1:inin)/z_zeff**1.5_RKIND)
    signeo(1:inin) = sigsptz(1:inin) * (1._RKIND - zft33eff*(1._RKIND+0.36_RKIND/z_zeff &
         &                        - zft33eff*(0.59_RKIND/z_zeff - 0.23_RKIND/z_zeff*zft33eff)))
    !
    return
  END SUBROUTINE sigmaneo_v
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE sigmaneomod
