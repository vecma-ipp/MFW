MODULE Phys_constants

  USE ITM_Constants

  IMPLICIT NONE

  REAL(r8) :: pi=itm_pi
  REAL(r8) :: tpi=2.0_r8*itm_pi

  REAL(r8) :: mu_0 = itm_mu0
  REAL(r8) :: kb=itm_ev
  REAL(r8) :: ee=itm_qe
  REAL(r8) :: cc=1.0_r8, lcoul=14.0_r8

  REAL(r8) :: me=itm_me
  REAL(r8) :: md=itm_md

END MODULE Phys_constants
