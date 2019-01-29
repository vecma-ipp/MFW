program check_equilibrium
!----------------------------------------------------------------------------
! This small program compares the newly calculated equilibrium
! file 'equilibrium.cpo' with the reference equilibrium file
! 'equilibrium_opteron' in the current directory.
! It uses the functions in the module error_analysis to find
! differences between the two equilibria.
! It helps you verify the quality of your equilibrium calculation and trace
! bugs with the existing test cases.
!----------------------------------------------------------------------------

  use itm_types
  use deallocate_structures
  use read_structures
!  use diff_structures
  use error_analysis
  use euitm_schemas

  implicit none

  type (type_equilibrium), pointer :: reference(:), test(:)
  character(len = 132) :: filename_reference_equilibrium
  character(len = 132) :: filename_new_equilibrium
  character(len = 132) :: path
  integer(itm_i4), parameter :: iu6 = 6

!-- define default behaviour without input arguments
  path = './' ! run locally
  filename_reference_equilibrium = 'equilibrium_opteron'
  filename_new_equilibrium = 'equilibrium.cpo'

!-- allocate empty CPOs
  allocate(reference(1))
  allocate(test(1))

!-- read equilibrium CPO from file (reference equilibrium)
  call set_read_verbosity(0)  ! no verbose output
  call open_read_file(12, trim(adjustl(path)) &
   // trim(adjustl(filename_reference_equilibrium)))
  call read_cpo(reference(1), 'equilibrium')
  call close_read_file

!-- read equilibrium CPO from file (test equilibrium)
  call set_read_verbosity(0)  ! no verbose output
  call open_read_file(12, trim(adjustl(path)) &
   // trim(adjustl(filename_new_equilibrium)))
  call read_cpo(test(1), 'equilibrium')
  call close_read_file

!-- compare both equilibrium CPOs
  call set_diff_verbosity(0) ! only summary output
  call reset_diff_counter
  call set_error_level(0._r8)
  call diff_cpo(reference(1), test(1), 'equilibrium', are_identical_float)
!  write(iu6, *) 'total number of differences: ', get_diff_counter()
  call diff_cpo(reference(1), test(1), 'equilibrium', average_relative_error_float)
!  write(iu6, *) 'final average relative error: ', get_error_level()
  call diff_cpo(reference(1), test(1), 'equilibrium', maximum_relative_error_float)
!  write(iu6, *) 'final maximum relative error: ', get_error_level()
  call diff_cpo(reference(1), test(1), 'equilibrium', chi_square_float)
!  write(iu6, *) 'final chi^2: ', get_error_level()

!-- final deallocate
  call deallocate_cpo(reference)
  call deallocate_cpo(test)

end program check_equilibrium
