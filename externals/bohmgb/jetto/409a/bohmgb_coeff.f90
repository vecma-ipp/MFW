MODULE BohmGB_Coeff

  USE ITM_Types

  IMPLICIT NONE

  LOGICAL, SAVE :: write_diags = .false.
  LOGICAL, SAVE :: write_cpos = .false.

  INTEGER(ITM_I4), SAVE :: nrho_transp = 0, nion_transp = 0
  REAL(R8), SAVE :: chi_coeff_e = 1., chi_coeff_i = 2.
  REAL(R8), SAVE :: chigb_coeff_e = 1., chigb_coeff_i = 1.
  REAL(R8), SAVE :: chi_d = 3., chiratio_phi = 0.7, chiratio_z = 1.

  INTEGER(ITM_I4), SAVE :: H_MODE = 0
  REAL(R8), SAVE :: RHO_PEDESTAL_TOP_NORMALIZED = 1.0

END MODULE BohmGB_Coeff

