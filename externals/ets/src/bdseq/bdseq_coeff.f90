MODULE BDSEQ_Coeff

  USE ITM_Types

  IMPLICIT NONE

  LOGICAL, SAVE :: symmetry_coords = .true.
  LOGICAL, SAVE :: write_diags = .false.
  LOGICAL, SAVE :: write_cpos = .false.

  INTEGER(ITM_I4), SAVE :: nr_eq = 0, neta_eq = 0

END MODULE BDSEQ_Coeff

