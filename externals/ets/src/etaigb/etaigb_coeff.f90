MODULE ETAIGB_Coeff

  USE ITM_Types

  IMPLICIT NONE

  INTEGER(ITM_I4), SAVE :: nrho_transp = 0, nion = 0
  REAL(R8), SAVE :: thresh = 6., tfloor = 0., &
       beta_reduction = 10., etae_pinch = 3., chi_d = 3.

END MODULE ETAIGB_Coeff

