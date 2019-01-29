!
MODULE neobscoeffmod
  !
  ! Compute neoclassical bootstrap current coefficients for given collisionalities using formula in Ref.1:
  !             O. Sauter et al, Phys. Plasmas 6 (1999) 2834.
  ! Inputs:
  !    ft: trapped fraction ft (Note, one can use formula in YR Lin-Liu et al, Phys. Plasmas 2 (1995) 1666.)
  ! Optionals:
  !    Zeff : effective charge (default = 2. if omitted)
  !    nuestar : local electron collisionality (Eq. 18b of Ref. 1 or 0. for comparison) (0. if omitted)
  !    nuistar : local ion collisionality (Eq. 18c of Ref. 1 or 0. for comparison) (0. if omitted)
  !    Nin : number of input values (if not given assumes dim. of input arrays for vectors or 1 for scalars)
  !
  ! Outputs:
  !    L31, L32, L34, alfa : as defined in Ref.1, Eqs. (14-17)
  !
  ! examples of calls:
  !
  ! call neobscoeff(L31, L32, L34, alfa, ft, Zeff)
  ! call neobscoeff(L31, L32, L34, alfa, ft, Zeff, nuestar, nuistar)
  ! call neobscoeff(L31, L32, L34, alfa, ft, Zeff, Nin)
  ! call neobscoeff(L31, L32, L34, alfa, ft, NUESTAR=nuestar,NIN=Nin)
  !
  ! To use this module in a routine, include the follwing statement at the beginning of the routine:
  ! USE neobscoeffmod
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: neobscoeff
  !
  INTERFACE neobscoeff
     ! scalar (_s) or vector (_v) inputs
     MODULE PROCEDURE neobscoeff_s, neobscoeff_v
  END INTERFACE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CONTAINS

  SUBROUTINE neobscoeff_s(L31, L32, L34, ALFA, ft, Zeff, nuestar, nuistar)
    ! CASE: all are scalar input variables (nin not used)
    !       Note: if called with arg as ft(i), it is a scalar and goes through this routine
    INTEGER, PARAMETER :: RKIND = SELECTED_REAL_KIND(10)
    REAL(RKIND), INTENT(OUT) :: L31, L32, L34, ALFA
    REAL(RKIND), INTENT(IN)  :: ft
    REAL(RKIND), OPTIONAL, INTENT(IN)  :: Zeff, nuestar, nuistar
    !
    REAL(RKIND)  :: ZZ, znuestar, znuistar, zsqnuest, zeffp1, zsqnui, znui2ft6
    REAL(RKIND)  :: zft31eff, zft32ee_eff, zft32ei_eff, zft34eff, zalfa0
    !-----------------------------------------------------------------------
    !
    ZZ = 2.0_RKIND
    IF ( PRESENT(zeff) ) ZZ = zeff
    znuestar = 0.0_RKIND
    IF ( PRESENT(nuestar) ) znuestar = nuestar
    znuistar = 0.0_RKIND
    IF ( PRESENT(nuistar) ) znuistar = nuistar
    !
    zsqnuest = sqrt(znuestar)
    !
    !  effective trapped fractions
    !
    zsqnuest = sqrt(znuestar)
    zft31eff = ft / (1._RKIND+(1._RKIND-0.1_RKIND*ft)*zsqnuest &
         &  + 0.5_RKIND*(1._RKIND-ft)*znuestar/ZZ)
    zft32ee_eff = ft / (1._RKIND + 0.26_RKIND*(1._RKIND-ft)*zsqnuest &
         &  + 0.18_RKIND*(1._RKIND-0.37_RKIND*ft)*znuestar/sqrt(ZZ))
    zft32ei_eff = ft / (1._RKIND + (1._RKIND+0.6_RKIND*ft)*zsqnuest &
         &  + 0.85_RKIND*(1._RKIND-0.37_RKIND*ft)*znuestar*(1._RKIND+ZZ))
    zft34eff = ft / (1._RKIND+(1._RKIND-0.1_RKIND*ft)*zsqnuest &
         &  + 0.5_RKIND*(1._RKIND-0.5_RKIND*ft)*znuestar/ZZ)
    zalfa0 = - 1.17_RKIND*(1._RKIND-ft) / (1._RKIND-0.22_RKIND*ft-0.19_RKIND*ft**2)
    !
    !coefficients
    !
    zeffp1 = ZZ+1._RKIND
    L31 = zft31eff * ( (1._RKIND+1.4_RKIND/zeffp1) &
         & - zft31eff* (1.9_RKIND/zeffp1 - zft31eff * (0.3_RKIND/zeffp1 + 0.2_RKIND/zeffp1 * zft31eff)))
    L32 = (0.05_RKIND+0.62_RKIND*ZZ)/ZZ/(1._RKIND+0.44_RKIND*ZZ)*(zft32ee_eff-zft32ee_eff**4) &
         & +  zft32ee_eff**2*(1._RKIND-1.2_RKIND*zft32ee_eff+0.2_RKIND*zft32ee_eff**2) &
         &                  /(1._RKIND+0.22_RKIND*ZZ) &
         & - (0.56_RKIND+1.93_RKIND*ZZ)/ZZ/(1._RKIND+0.44_RKIND*ZZ)*(zft32ei_eff-zft32ei_eff**4) &
         & +  zft32ei_eff**2*(1._RKIND-0.55_RKIND*zft32ei_eff-0.45_RKIND*zft32ei_eff**2) &
         &                  * 4.95_RKIND/(1._RKIND+2.48_RKIND*ZZ) &
         & + 1.2_RKIND / (1._RKIND+0.5_RKIND*ZZ) * (zft32ee_eff**4-zft32ei_eff**4)
    L34 = zft34eff * ( (1._RKIND+1.4_RKIND/zeffp1) &
         & - zft34eff* (1.9_RKIND/zeffp1 - zft34eff * (0.3_RKIND/zeffp1 + 0.2_RKIND/zeffp1 * zft34eff)))
    zsqnui = sqrt(znuistar)
    znui2ft6 = znuistar**2 * ft**6
    ALFA = ((zalfa0 + 0.25_RKIND*(1._RKIND-ft**2)*zsqnui) &
         &                    / (1._RKIND+0.5_RKIND*zsqnui) + 0.315_RKIND*znui2ft6) &
         & / (1._RKIND + 0.15_RKIND*znui2ft6)
    !
    return
  END SUBROUTINE neobscoeff_s
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE neobscoeff_v(L31, L32, L34, ALFA, ft, Zeff, nuestar, nuistar, NIN)
    ! CASE: all are vector input variables
    !       If NIN not given, use size(ft) to determine length of vectors to compute
    INTEGER, PARAMETER :: RKIND = SELECTED_REAL_KIND(10)
    REAL(RKIND), DIMENSION (:), INTENT(OUT) :: L31, L32, L34, ALFA
    REAL(RKIND), DIMENSION (:), INTENT(IN)  :: ft
    REAL(RKIND), DIMENSION (:), OPTIONAL, INTENT(IN)  :: Zeff, nuestar, nuistar
    INTEGER,     OPTIONAL, INTENT(IN)  :: NIN
    !
    REAL(RKIND), DIMENSION(:), ALLOCATABLE  :: ZZ, znuestar, znuistar, zsqnuest, zeffp1, zsqnui, znui2ft6
    REAL(RKIND), DIMENSION(:), ALLOCATABLE  :: zft31eff, zft32ee_eff, zft32ei_eff, zft34eff, zalfa0
    INTEGER :: inin
    !-----------------------------------------------------------------------
    !
    inin = size(ft)
    IF ( PRESENT(NIN) ) inin = NIN
    !
    IF( .NOT. ALLOCATED(ZZ) ) ALLOCATE(ZZ(inin))
    IF( .NOT. ALLOCATED(znuestar) ) ALLOCATE(znuestar(inin))
    IF( .NOT. ALLOCATED(znuistar) ) ALLOCATE(znuistar(inin))
    ZZ = 2.0_RKIND
    IF ( PRESENT(zeff) ) ZZ = zeff(1:inin)
    znuestar = 0.0_RKIND
    IF ( PRESENT(nuestar) ) znuestar = nuestar(1:inin)
    znuistar = 0.0_RKIND
    IF ( PRESENT(nuistar) ) znuistar = nuistar(1:inin)
    !
    IF( .NOT. ALLOCATED(zsqnuest) ) ALLOCATE(zsqnuest(inin))
    zsqnuest = sqrt(znuestar)
    !
    !  effective trapped fractions
    !
    IF( .NOT. ALLOCATED(zft31eff) ) ALLOCATE(zft31eff(inin))
    IF( .NOT. ALLOCATED(zft32ee_eff) ) ALLOCATE(zft32ee_eff(inin))
    IF( .NOT. ALLOCATED(zft32ei_eff) ) ALLOCATE(zft32ei_eff(inin))
    IF( .NOT. ALLOCATED(zft34eff) ) ALLOCATE(zft34eff(inin))
    IF( .NOT. ALLOCATED(zalfa0) ) ALLOCATE(zalfa0(inin))

    zft31eff = ft(1:inin) / (1._RKIND+(1._RKIND-0.1_RKIND*ft(1:inin))*zsqnuest &
         &  + 0.5_RKIND*(1._RKIND-ft(1:inin))*znuestar/ZZ)
    zft32ee_eff = ft(1:inin) / (1._RKIND + 0.26_RKIND*(1._RKIND-ft(1:inin))*zsqnuest &
         &  + 0.18_RKIND*(1._RKIND-0.37_RKIND*ft(1:inin))*znuestar/sqrt(ZZ))
    zft32ei_eff = ft(1:inin) / (1._RKIND + (1._RKIND+0.6_RKIND*ft(1:inin))*zsqnuest &
         &  + 0.85_RKIND*(1._RKIND-0.37_RKIND*ft(1:inin))*znuestar*(1._RKIND+ZZ))
    zft34eff = ft(1:inin) / (1._RKIND+(1._RKIND-0.1_RKIND*ft(1:inin))*zsqnuest &
         &  + 0.5_RKIND*(1._RKIND-0.5_RKIND*ft(1:inin))*znuestar/ZZ)
    zalfa0 = - 1.17_RKIND*(1._RKIND-ft(1:inin)) / (1._RKIND-0.22_RKIND*ft(1:inin)-0.19_RKIND*ft(1:inin)**2)
    !
    !coefficients
    !
    IF( .NOT. ALLOCATED(zeffp1) ) ALLOCATE(zeffp1(inin))
    zeffp1 = ZZ+1._RKIND
    L31(1:inin) = zft31eff * ( (1._RKIND+1.4_RKIND/zeffp1) &
         & - zft31eff* (1.9_RKIND/zeffp1 - zft31eff * (0.3_RKIND/zeffp1 + 0.2_RKIND/zeffp1 * zft31eff)))
    L32(1:inin) = (0.05_RKIND+0.62_RKIND*ZZ)/ZZ/(1._RKIND+0.44_RKIND*ZZ)*(zft32ee_eff-zft32ee_eff**4) &
         & +  zft32ee_eff**2*(1._RKIND-1.2_RKIND*zft32ee_eff+0.2_RKIND*zft32ee_eff**2) &
         &                  /(1._RKIND+0.22_RKIND*ZZ) &
         & - (0.56_RKIND+1.93_RKIND*ZZ)/ZZ/(1._RKIND+0.44_RKIND*ZZ)*(zft32ei_eff-zft32ei_eff**4) &
         & +  zft32ei_eff**2*(1._RKIND-0.55_RKIND*zft32ei_eff-0.45_RKIND*zft32ei_eff**2) &
         &                  * 4.95_RKIND/(1._RKIND+2.48_RKIND*ZZ) &
         & + 1.2_RKIND / (1._RKIND+0.5_RKIND*ZZ) * (zft32ee_eff**4-zft32ei_eff**4)
    L34(1:inin) = zft34eff * ( (1._RKIND+1.4_RKIND/zeffp1) &
         & - zft34eff* (1.9_RKIND/zeffp1 - zft34eff * (0.3_RKIND/zeffp1 + 0.2_RKIND/zeffp1 * zft34eff)))
    IF( .NOT. ALLOCATED(zsqnui) ) ALLOCATE(zsqnui(inin))
    zsqnui = sqrt(znuistar)
    IF( .NOT. ALLOCATED(znui2ft6) ) ALLOCATE(znui2ft6(inin))
    znui2ft6 = znuistar**2 * ft(1:inin)**6
    ALFA(1:inin) = ((zalfa0 + 0.25_RKIND*(1._RKIND-ft(1:inin)**2)*zsqnui) &
         &                    / (1._RKIND+0.5_RKIND*zsqnui) + 0.315_RKIND*znui2ft6) &
         & / (1._RKIND + 0.15_RKIND*znui2ft6)
    !
    return
  END SUBROUTINE neobscoeff_v

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE neobscoeffmod
