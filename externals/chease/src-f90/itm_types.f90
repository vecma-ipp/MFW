!> Module implementing the ITM basic types
!>
!> Source:
!>  based on SOLPS b2mod_types.F
!>  pulled from ets r100 and extended with input from C. Konz, T. Ribeiro & B. Scott
!>
!> \author David Coster
!>
!> \version "$Id: itm_types.f90 787 2013-02-28 07:22:23Z osauter $"

module itm_types

  INTEGER,  PARAMETER :: ITM_I1 = SELECTED_INT_KIND (2)        ! Integer*1
  INTEGER,  PARAMETER :: ITM_I2 = SELECTED_INT_KIND (4)        ! Integer*2
  INTEGER,  PARAMETER :: ITM_I4 = SELECTED_INT_KIND (9)        ! Integer*4
  INTEGER,  PARAMETER :: ITM_I8 = SELECTED_INT_KIND (18)       ! Integer*8
  INTEGER,  PARAMETER :: R4 = SELECTED_REAL_KIND (6, 37)   ! Real*4
  INTEGER,  PARAMETER :: R8 = SELECTED_REAL_KIND (15, 300) ! Real*8

  INTEGER,  PARAMETER :: itm_int_invalid = -999999999
  REAL(R8), PARAMETER :: itm_r8_invalid = -9.0D40

  interface itm_is_valid
     module procedure itm_is_valid_int4, itm_is_valid_int8, itm_is_valid_real8
  end interface

contains

  logical function itm_is_valid_int4(in_int)
    implicit none
    integer(ITM_I4) in_int
    itm_is_valid_int4 = in_int .ne. itm_int_invalid
    return
  end function itm_is_valid_int4

  logical function itm_is_valid_int8(in_int)
    implicit none
    integer(ITM_I8) in_int
    itm_is_valid_int8 = in_int .ne. itm_int_invalid
    return
  end function itm_is_valid_int8

  logical function itm_is_valid_real8(in_real)
    implicit none
    real(R8) in_real
    itm_is_valid_real8 = abs(in_real - itm_r8_invalid) .gt. itm_r8_invalid * 1.0e-15_R8
    return
  end function itm_is_valid_real8

end module itm_types
