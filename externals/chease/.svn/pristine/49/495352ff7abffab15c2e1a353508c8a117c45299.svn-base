SUBROUTINE BSCOEFF(pft,pq,pR,peps,pte,pne,pti,pni,pzeff,pzion, &
     & pl31_0,pl32_0,palfa_0,pl31,pl32,pl34,palfa)
  !
  !     WARNING: in MKSA
  !
  !     Compute Bootstrap coefficients using formulas from O. Sauter et al, Phys. Plasmas 7 (1999) 2834.
  !
  !     Assumes to compute on a single flux surface with:
  ! Inputs:
  !     pft   : trapped fraction
  !     pq    : safety factor
  !     pR    : Geometrical center of given flux surface in [m]
  !     peps  : Inverse aspect ratio of given flux surface
  !     pte   : Electron temperature [eV]
  !     pne   : Electron density [1/m**3]
  !     pti   : Ion temperature [eV]
  !     pni   : Main ion density [1/m**3]
  !     pzeff : Effective charge (used for Z in electronic terms)
  !     pzion : Main ion charge
  ! Outputs:
  !     pl31_0  : L31 coefficient assuming nuestar=0
  !     pl32_0  : L32 coefficient assuming nuestar=0
  !     palfa_0 : Alfa coefficient assuming nuestar=0
  !     pl31    : L31 coefficient
  !     pl32    : L32 coefficient
  !     pl34    : L34 coefficient (L34 for nuestar=0 is identical to L31_0)
  !     palfa   : Alfa coefficient
  !
  USE prec_const
  USE neobscoeffmod
  IMPLICIT NONE
  !
  REAL(RKIND), INTENT(IN)  :: pft,pq,pR,peps,pte,pne,pti,pni,pzeff,pzion
  REAL(RKIND), INTENT(OUT) :: pl31_0,pl32_0,palfa_0,pl31,pl32,pl34,palfa
  !
  REAL(RKIND)  :: znuestar, znuistar, zlnlam_e, zlnlam_i, zdummy
  !-----------------------------------------------------------------------
  !     
  !     basic parameters
  !
  zlnlam_e = 17._RKIND
  IF (pne.gt.0._RKIND .AND. pte.gt.0._RKIND) THEN
    zlnlam_e = 31.3_RKIND - log(sqrt(pne)/pte)
    zlnlam_e = max(10._RKIND,zlnlam_e)
  ENDIF
  zlnlam_i = 17._RKIND
  IF (pni.gt.0._RKIND .AND. pti.gt.0._RKIND) THEN
    zlnlam_i = 30._RKIND - log(pzion**3._RKIND*sqrt(pni)/ABS(pti)**1.5_RKIND)
    zlnlam_i = max(10._RKIND,zlnlam_i)
  ENDIF
  znuestar = 6.921E-18_RKIND * pq*pR*pne*pzeff*zlnlam_e / (pte*pte*peps**1.5_RKIND)
  znuistar = 4.900E-18_RKIND * pq*pR*pni*pzion**4*zlnlam_i / (pti*pti*peps**1.5_RKIND)
  !
  !     Compute coefficients for nustar=0
  !
  call neobscoeff(pl31_0,pl32_0,zdummy,palfa_0,pft,pzeff)
  !
  !     finite nustar
  call neobscoeff(pl31,pl32,pl34,palfa,pft,pzeff,znuestar,znuistar)
  !
  return
end SUBROUTINE BSCOEFF
