! 
! Contains the type definition for utilities used to copy itm CPO to IDS and vice versa, to provide missing data in a generic way
!

module itm_ids_utilities    ! declare the set of types common to all sub-trees

  integer, parameter, private :: RP = kind(1.0d0)

  type struct_prof0d
     character(len=132) :: prof0d_name = "" ! name of each extra_prof0d to check
     real(RP) :: prof0d ! scrolls through various extra 0D arrays
  end type struct_prof0d
  type struct_prof1d
     character(len=132) :: name = "" ! name of each extra_prof1d to check
     real(RP), pointer :: values(:) ! scrolls through various extra 1D arrays
  end type struct_prof1d
  type struct_prof2d
     character(len=132) :: prof2d_name = "" ! name of each extra_prof2d to check
     real(RP), pointer :: prof2d(:) ! scrolls through various extra 2D arrays
  end type struct_prof2d
  type extra_data_gen ! to be able to give scalar and arrays not available in itm CPO
     ! construct it as arrays of structures to be able to check the size for each extra element to check if present
     type(struct_prof0d),pointer :: prof0d_list(:) => null()
     type(struct_prof1d),pointer :: prof1d_list(:) => null()
     type(struct_prof2d),pointer :: prof2d_list(:) => null()
  end type extra_data_gen

end module itm_ids_utilities

