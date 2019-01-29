!> This module is an example for the error_analysis module which
!> shall hold the user defined error analysis functions.
!>
!> Those functions can be arbitrary functions. They only have to follow
!> the following structure:
!>
!> func(reference_field, new_field, diff_counter)
!>
!> where reference_field and new_field must have the same type and be of
!> a real type (scalar or array, 1D - 7D) and diff_counter is an integer.
!>
!> You must define overloaded versions of each function for all
!> required real types (scalar, array, 1D - 7D) as shown in the example.
!> 
!> \author Christian Konz
!>
!> \version "$Id $"

module error_analysis

  use itm_types

  implicit none

  interface are_identical
    module procedure &
     are_identical_float_type, &
     are_identical_vecflt_type, &
     are_identical_matflt_type, &
     are_identical_array3dflt_type, &
     are_identical_array4dflt_type, &
     are_identical_array5dflt_type, &
     are_identical_array6dflt_type, &
     are_identical_array7dflt_type
  end interface

  interface average_relative_error
    module procedure &
     average_relative_error_float_type, &
     average_relative_error_vecflt_type, &
     average_relative_error_matflt_type, &
     average_relative_error_array3dflt_type, &
     average_relative_error_array4dflt_type, &
     average_relative_error_array5dflt_type, &
     average_relative_error_array6dflt_type, &
     average_relative_error_array7dflt_type
  end interface

  interface maximum_relative_error
    module procedure &
     maximum_relative_error_float_type, &
     maximum_relative_error_vecflt_type, &
     maximum_relative_error_matflt_type, &
     maximum_relative_error_array3dflt_type, &
     maximum_relative_error_array4dflt_type, &
     maximum_relative_error_array5dflt_type, &
     maximum_relative_error_array6dflt_type, &
     maximum_relative_error_array7dflt_type
  end interface

  interface chi_square
    module procedure &
     chi_square_float_type, &
     chi_square_vecflt_type, &
     chi_square_matflt_type, &
     chi_square_array3dflt_type, &
     chi_square_array4dflt_type, &
     chi_square_array5dflt_type, &
     chi_square_array6dflt_type, &
     chi_square_array7dflt_type
  end interface

contains

  function are_identical_float(diff_counter, error_level, &
   reference_field_float, new_field_float, &
   reference_field_array3dflt_type, new_field_array3dflt_type, &
   reference_field_array4dflt_type, new_field_array4dflt_type, &
   reference_field_array5dflt_type, new_field_array5dflt_type, &
   reference_field_array6dflt_type, new_field_array6dflt_type, &
   reference_field_array7dflt_type, new_field_array7dflt_type, &
   reference_field_matflt_type, new_field_matflt_type, &
   reference_field_vecflt_type, new_field_vecflt_type) &
   result (f_identical)

    implicit none

    real(r8), intent(in), optional :: reference_field_float, new_field_float
    real(r8), dimension(:, :, :), pointer, optional :: reference_field_array3dflt_type, new_field_array3dflt_type
    real(r8), dimension(:, :, :, :), pointer, optional :: reference_field_array4dflt_type, new_field_array4dflt_type
    real(r8), dimension(:, :, :, :, :), pointer, optional :: reference_field_array5dflt_type, new_field_array5dflt_type
    real(r8), dimension(:, :, :, :, :, :), pointer, optional :: reference_field_array6dflt_type, new_field_array6dflt_type
    real(r8), dimension(:, :, :, :, :, :, :), pointer, optional :: reference_field_array7dflt_type, new_field_array7dflt_type
    real(r8), dimension(:, :), pointer, optional :: reference_field_matflt_type, new_field_matflt_type
    real(r8), dimension(:), pointer, optional :: reference_field_vecflt_type, new_field_vecflt_type
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (present(reference_field_vecflt_type)) then
      f_identical = are_identical(reference_field_vecflt_type, new_field_vecflt_type, diff_counter, error_level)
    else if (present(reference_field_matflt_type)) then
      f_identical = are_identical(reference_field_matflt_type, new_field_matflt_type, diff_counter, error_level)
    else if (present(reference_field_array3dflt_type)) then
      f_identical = are_identical(reference_field_array3dflt_type, new_field_array3dflt_type, diff_counter, error_level)
    else if (present(reference_field_array4dflt_type)) then
      f_identical = are_identical(reference_field_array4dflt_type, new_field_array4dflt_type, diff_counter, error_level)
    else if (present(reference_field_array5dflt_type)) then
      f_identical = are_identical(reference_field_array5dflt_type, new_field_array5dflt_type, diff_counter, error_level)
    else if (present(reference_field_array6dflt_type)) then
      f_identical = are_identical(reference_field_array6dflt_type, new_field_array6dflt_type, diff_counter, error_level)
    else if (present(reference_field_array7dflt_type)) then
      f_identical = are_identical(reference_field_array7dflt_type, new_field_array7dflt_type, diff_counter, error_level)
    else
      f_identical = are_identical(reference_field_float, new_field_float, diff_counter, error_level)
    end if

  end function are_identical_float

  function are_identical_float_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), intent(in) :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (reference_field /= new_field) then
      diff_counter = diff_counter + 1
      write(f_identical, '(a132)') 'differ'
    else
      write(f_identical, '(a132)') 'are identical'
    end if

  end function are_identical_float_type

  function are_identical_vecflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), dimension(:), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (size(reference_field) /= size(new_field)) then
      write(f_identical, '(a132)') 'ERROR: field sizes differ'
    else
      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
        write(f_identical, '(a132)') 'differ'
      else
        write(f_identical, '(a132)') 'are identical'
      end if
    end if

  end function are_identical_vecflt_type

  function are_identical_matflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), dimension(:, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (size(reference_field) /= size(new_field)) then
      write(f_identical, '(a132)') 'ERROR: field sizes differ'
    else
      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
        write(f_identical, '(a132)') 'differ'
      else
        write(f_identical, '(a132)') 'are identical'
      end if
    end if

  end function are_identical_matflt_type

  function are_identical_array3dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), dimension(:, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (size(reference_field) /= size(new_field)) then
      write(f_identical, '(a132)') 'ERROR: field sizes differ'
    else
      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
        write(f_identical, '(a132)') 'differ'
      else
        write(f_identical, '(a132)') 'are identical'
      end if
    end if

  end function are_identical_array3dflt_type

  function are_identical_array4dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), dimension(:, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (size(reference_field) /= size(new_field)) then
      write(f_identical, '(a132)') 'ERROR: field sizes differ'
    else
      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
        write(f_identical, '(a132)') 'differ'
      else
        write(f_identical, '(a132)') 'are identical'
      end if
    end if

  end function are_identical_array4dflt_type

  function are_identical_array5dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), dimension(:, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (size(reference_field) /= size(new_field)) then
      write(f_identical, '(a132)') 'ERROR: field sizes differ'
    else
      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
        write(f_identical, '(a132)') 'differ'
      else
        write(f_identical, '(a132)') 'are identical'
      end if
    end if

  end function are_identical_array5dflt_type

  function are_identical_array6dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), dimension(:, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (size(reference_field) /= size(new_field)) then
      write(f_identical, '(a132)') 'ERROR: field sizes differ'
    else
      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
        write(f_identical, '(a132)') 'differ'
      else
        write(f_identical, '(a132)') 'are identical'
      end if
    end if

  end function are_identical_array6dflt_type

  function are_identical_array7dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_identical)

    implicit none

    real(r8), dimension(:, :, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_identical

    if (size(reference_field) /= size(new_field)) then
      write(f_identical, '(a132)') 'ERROR: field sizes differ'
    else
      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
        write(f_identical, '(a132)') 'differ'
      else
        write(f_identical, '(a132)') 'are identical'
      end if
    end if

  end function are_identical_array7dflt_type

  function average_relative_error_float(diff_counter, error_level, &
   reference_field_float, new_field_float, &
   reference_field_array3dflt_type, new_field_array3dflt_type, &
   reference_field_array4dflt_type, new_field_array4dflt_type, &
   reference_field_array5dflt_type, new_field_array5dflt_type, &
   reference_field_array6dflt_type, new_field_array6dflt_type, &
   reference_field_array7dflt_type, new_field_array7dflt_type, &
   reference_field_matflt_type, new_field_matflt_type, &
   reference_field_vecflt_type, new_field_vecflt_type) &
   result (f_average)

    implicit none

    real(r8), intent(in), optional :: reference_field_float, new_field_float
    real(r8), dimension(:, :, :), pointer, optional :: reference_field_array3dflt_type, new_field_array3dflt_type
    real(r8), dimension(:, :, :, :), pointer, optional :: reference_field_array4dflt_type, new_field_array4dflt_type
    real(r8), dimension(:, :, :, :, :), pointer, optional :: reference_field_array5dflt_type, new_field_array5dflt_type
    real(r8), dimension(:, :, :, :, :, :), pointer, optional :: reference_field_array6dflt_type, new_field_array6dflt_type
    real(r8), dimension(:, :, :, :, :, :, :), pointer, optional :: reference_field_array7dflt_type, new_field_array7dflt_type
    real(r8), dimension(:, :), pointer, optional :: reference_field_matflt_type, new_field_matflt_type
    real(r8), dimension(:), pointer, optional :: reference_field_vecflt_type, new_field_vecflt_type
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_average

    if (present(reference_field_vecflt_type)) then
      f_average = average_relative_error(reference_field_vecflt_type, new_field_vecflt_type, diff_counter, error_level)
    else if (present(reference_field_matflt_type)) then
      f_average = average_relative_error(reference_field_matflt_type, new_field_matflt_type, diff_counter, error_level)
    else if (present(reference_field_array3dflt_type)) then
      f_average = average_relative_error(reference_field_array3dflt_type, new_field_array3dflt_type, diff_counter, error_level)
    else if (present(reference_field_array4dflt_type)) then
      f_average = average_relative_error(reference_field_array4dflt_type, new_field_array4dflt_type, diff_counter, error_level)
    else if (present(reference_field_array5dflt_type)) then
      f_average = average_relative_error(reference_field_array5dflt_type, new_field_array5dflt_type, diff_counter, error_level)
    else if (present(reference_field_array6dflt_type)) then
      f_average = average_relative_error(reference_field_array6dflt_type, new_field_array6dflt_type, diff_counter, error_level)
    else if (present(reference_field_array7dflt_type)) then
      f_average = average_relative_error(reference_field_array7dflt_type, new_field_array7dflt_type, diff_counter, error_level)
    else
      f_average = average_relative_error(reference_field_float, new_field_float, diff_counter, error_level)
    end if

  end function average_relative_error_float

  function average_relative_error_float_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), intent(in) :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error

    character(len = 132) :: f_average

    av_rel_error = abs(new_field - reference_field) / (abs(new_field &
     + reference_field) / 2._r8 + abs(eps))

    if (reference_field /= new_field) then
      diff_counter = diff_counter + 1
    end if

    write(f_average, 1) av_rel_error

    error_level = (error_level + av_rel_error) / 2._r8

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_float_type

  function average_relative_error_vecflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error
    integer(itm_i4) :: i1

    character(len = 132) :: f_average

    if (size(reference_field) /= size(new_field)) then

      write(f_average, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      av_rel_error = 0._r8

      do i1 = 1, size(reference_field, 1)
        av_rel_error = av_rel_error + abs(new_field(i1) - reference_field(i1)) &
         / (abs(new_field(i1) + reference_field(i1)) / 2._r8 + abs(eps))
      end do

      av_rel_error = av_rel_error / size(reference_field, 1)

      write(f_average, 1) av_rel_error

      error_level = (error_level + av_rel_error) / 2._r8

    end if

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_vecflt_type

  function average_relative_error_matflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error
    integer(itm_i4) :: i1, i2

    character(len = 132) :: f_average

    if (size(reference_field) /= size(new_field)) then

      write(f_average, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      av_rel_error = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          av_rel_error = av_rel_error + abs(new_field(i1, i2) - reference_field(i1, i2)) &
           / (abs(new_field(i1, i2) + reference_field(i1, i2)) / 2._r8 + abs(eps))
        end do
      end do

      av_rel_error = av_rel_error / (size(reference_field, 1) &
       * size(reference_field, 2))

      write(f_average, 1) av_rel_error

      error_level = (error_level + av_rel_error) / 2._r8

    end if

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_matflt_type

  function average_relative_error_array3dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error
    integer(itm_i4) :: i1, i2, i3

    character(len = 132) :: f_average

    if (size(reference_field) /= size(new_field)) then

      write(f_average, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      av_rel_error = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            av_rel_error = av_rel_error + abs(new_field(i1, i2, i3) - reference_field(i1, i2, i3)) &
             / (abs(new_field(i1, i2, i3) + reference_field(i1, i2, i3)) / 2._r8 + abs(eps))
          end do
        end do
      end do

      av_rel_error = av_rel_error / (size(reference_field, 1) &
       * size(reference_field, 2) * size(reference_field, 3))

      write(f_average, 1) av_rel_error

      error_level = (error_level + av_rel_error) / 2._r8

    end if

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_array3dflt_type

  function average_relative_error_array4dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error
    integer(itm_i4) :: i1, i2, i3, i4

    character(len = 132) :: f_average

    if (size(reference_field) /= size(new_field)) then

      write(f_average, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      av_rel_error = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              av_rel_error = av_rel_error + abs(new_field(i1, i2, i3, i4) - reference_field(i1, i2, i3, i4)) &
               / (abs(new_field(i1, i2, i3, i4) + reference_field(i1, i2, i3, i4)) / 2._r8 + abs(eps))
            end do
          end do
        end do
      end do

      av_rel_error = av_rel_error / (size(reference_field, 1) &
       * size(reference_field, 2) * size(reference_field, 3) * size(reference_field, 4))

      write(f_average, 1) av_rel_error

      error_level = (error_level + av_rel_error) / 2._r8

    end if

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_array4dflt_type

  function average_relative_error_array5dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error
    integer(itm_i4) :: i1, i2, i3, i4, i5

    character(len = 132) :: f_average

    if (size(reference_field) /= size(new_field)) then

      write(f_average, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      av_rel_error = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                av_rel_error = av_rel_error + abs(new_field(i1, i2, i3, i4, i5) - reference_field(i1, i2, i3, i4, i5)) &
                 / (abs(new_field(i1, i2, i3, i4, i5) + reference_field(i1, i2, i3, i4, i5)) / 2._r8 + abs(eps))
              end do
            end do
          end do
        end do
      end do

      av_rel_error = av_rel_error / (size(reference_field, 1) &
       * size(reference_field, 2) * size(reference_field, 3) * size(reference_field, 4) * size(reference_field, 5))

      write(f_average, 1) av_rel_error

      error_level = (error_level + av_rel_error) / 2._r8

    end if

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_array5dflt_type

  function average_relative_error_array6dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error
    integer(itm_i4) :: i1, i2, i3, i4, i5, i6

    character(len = 132) :: f_average

    if (size(reference_field) /= size(new_field)) then

      write(f_average, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      av_rel_error = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                do i6 = 1, size(reference_field, 6)
                  av_rel_error = av_rel_error + abs(new_field(i1, i2, i3, i4, i5, i6) &
                   - reference_field(i1, i2, i3, i4, i5, i6)) &
                   / (abs(new_field(i1, i2, i3, i4, i5, i6) + reference_field(i1, i2, i3, i4, i5, i6)) &
                   / 2._r8 + abs(eps))
                end do
              end do
            end do
          end do
        end do
      end do

      av_rel_error = av_rel_error / (size(reference_field, 1) &
       * size(reference_field, 2) * size(reference_field, 3) * size(reference_field, 4) &
       * size(reference_field, 5) * size(reference_field, 6))

      write(f_average, 1) av_rel_error

      error_level = (error_level + av_rel_error) / 2._r8

    end if

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_array6dflt_type

  function average_relative_error_array7dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_average)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: av_rel_error
    integer(itm_i4) :: i1, i2, i3, i4, i5, i6, i7

    character(len = 132) :: f_average

    if (size(reference_field) /= size(new_field)) then

      write(f_average, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      av_rel_error = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                do i6 = 1, size(reference_field, 6)
                  do i7 = 1, size(reference_field, 7)
                    av_rel_error = av_rel_error + abs(new_field(i1, i2, i3, i4, i5, i6, i7) &
                     - reference_field(i1, i2, i3, i4, i5, i6, i7)) &
                     / (abs(new_field(i1, i2, i3, i4, i5, i6, i7) + reference_field(i1, i2, i3, i4, i5, i6, i7)) &
                     / 2._r8 + abs(eps))
                  end do
                end do
              end do
            end do
          end do
        end do
      end do

      av_rel_error = av_rel_error / (size(reference_field, 1) &
       * size(reference_field, 2) * size(reference_field, 3) * size(reference_field, 4) &
       * size(reference_field, 5) * size(reference_field, 6) * size(reference_field, 7))

      write(f_average, 1) av_rel_error

      error_level = (error_level + av_rel_error) / 2._r8

    end if

 1 format(' ---> average relative error: ', e9.2)

  end function average_relative_error_array7dflt_type

  function maximum_relative_error_float(diff_counter, error_level, &
   reference_field_float, new_field_float, &
   reference_field_array3dflt_type, new_field_array3dflt_type, &
   reference_field_array4dflt_type, new_field_array4dflt_type, &
   reference_field_array5dflt_type, new_field_array5dflt_type, &
   reference_field_array6dflt_type, new_field_array6dflt_type, &
   reference_field_array7dflt_type, new_field_array7dflt_type, &
   reference_field_matflt_type, new_field_matflt_type, &
   reference_field_vecflt_type, new_field_vecflt_type) &
   result (f_maximum)

    implicit none

    real(r8), intent(in), optional :: reference_field_float, new_field_float
    real(r8), dimension(:, :, :), pointer, optional :: reference_field_array3dflt_type, new_field_array3dflt_type
    real(r8), dimension(:, :, :, :), pointer, optional :: reference_field_array4dflt_type, new_field_array4dflt_type
    real(r8), dimension(:, :, :, :, :), pointer, optional :: reference_field_array5dflt_type, new_field_array5dflt_type
    real(r8), dimension(:, :, :, :, :, :), pointer, optional :: reference_field_array6dflt_type, new_field_array6dflt_type
    real(r8), dimension(:, :, :, :, :, :, :), pointer, optional :: reference_field_array7dflt_type, new_field_array7dflt_type
    real(r8), dimension(:, :), pointer, optional :: reference_field_matflt_type, new_field_matflt_type
    real(r8), dimension(:), pointer, optional :: reference_field_vecflt_type, new_field_vecflt_type
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_maximum

    if (present(reference_field_vecflt_type)) then
      f_maximum = maximum_relative_error(reference_field_vecflt_type, new_field_vecflt_type, diff_counter, error_level)
    else if (present(reference_field_matflt_type)) then
      f_maximum = maximum_relative_error(reference_field_matflt_type, new_field_matflt_type, diff_counter, error_level)
    else if (present(reference_field_array3dflt_type)) then
      f_maximum = maximum_relative_error(reference_field_array3dflt_type, new_field_array3dflt_type, diff_counter, error_level)
    else if (present(reference_field_array4dflt_type)) then
      f_maximum = maximum_relative_error(reference_field_array4dflt_type, new_field_array4dflt_type, diff_counter, error_level)
    else if (present(reference_field_array5dflt_type)) then
      f_maximum = maximum_relative_error(reference_field_array5dflt_type, new_field_array5dflt_type, diff_counter, error_level)
    else if (present(reference_field_array6dflt_type)) then
      f_maximum = maximum_relative_error(reference_field_array6dflt_type, new_field_array6dflt_type, diff_counter, error_level)
    else if (present(reference_field_array7dflt_type)) then
      f_maximum = maximum_relative_error(reference_field_array7dflt_type, new_field_array7dflt_type, diff_counter, error_level)
    else
      f_maximum = maximum_relative_error(reference_field_float, new_field_float, diff_counter, error_level)
    end if

  end function maximum_relative_error_float

  function maximum_relative_error_float_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), intent(in) :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, value

    character(len = 132) :: f_maximum

    max_rel_error = abs(new_field - reference_field) / (abs(new_field &
     + reference_field) / 2._r8 + abs(eps))
    value = new_field

    if (reference_field /= new_field) then
      diff_counter = diff_counter + 1
    end if

    write(f_maximum, 1) max_rel_error, value

    error_level = max(error_level, max_rel_error)

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_float_type

  function maximum_relative_error_vecflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, rel_error, value
    integer(itm_i4) :: i1

    character(len = 132) :: f_maximum

    if (size(reference_field) /= size(new_field)) then

      write(f_maximum, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      max_rel_error = 0._r8
      value = 0._r8

      do i1 = 1, size(reference_field, 1)
        rel_error = abs(new_field(i1) - reference_field(i1)) &
         / (abs(new_field(i1) + reference_field(i1)) / 2._r8 + abs(eps))
        if (rel_error > max_rel_error) then
          max_rel_error = rel_error
          value = new_field(i1)
        end if
      end do

      write(f_maximum, 1) max_rel_error, value

      error_level = max(error_level, max_rel_error)

    end if

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_vecflt_type

  function maximum_relative_error_matflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, rel_error, value
    integer(itm_i4) :: i1, i2

    character(len = 132) :: f_maximum

    if (size(reference_field) /= size(new_field)) then

      write(f_maximum, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      max_rel_error = 0._r8
      value = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          rel_error = abs(new_field(i1, i2) - reference_field(i1, i2)) &
           / (abs(new_field(i1, i2) + reference_field(i1, i2)) / 2._r8 + abs(eps))
          if (rel_error > max_rel_error) then
            max_rel_error = rel_error
            value = new_field(i1, i2)
          end if
        end do
      end do

      write(f_maximum, 1) max_rel_error, value

      error_level = max(error_level, max_rel_error)

    end if

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_matflt_type

  function maximum_relative_error_array3dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, rel_error, value
    integer(itm_i4) :: i1, i2, i3

    character(len = 132) :: f_maximum

    if (size(reference_field) /= size(new_field)) then

      write(f_maximum, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      max_rel_error = 0._r8
      value = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            rel_error = abs(new_field(i1, i2, i3) - reference_field(i1, i2, i3)) &
             / (abs(new_field(i1, i2, i3) + reference_field(i1, i2, i3)) / 2._r8 + abs(eps))
            if (rel_error > max_rel_error) then
              max_rel_error = rel_error
              value = new_field(i1, i2, i3)
            end if
          end do
        end do
      end do

      write(f_maximum, 1) max_rel_error, value

      error_level = max(error_level, max_rel_error)

    end if

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_array3dflt_type

  function maximum_relative_error_array4dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, rel_error, value
    integer(itm_i4) :: i1, i2, i3, i4

    character(len = 132) :: f_maximum

    if (size(reference_field) /= size(new_field)) then

      write(f_maximum, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      max_rel_error = 0._r8
      value = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              rel_error = abs(new_field(i1, i2, i3, i4) - reference_field(i1, i2, i3, i4)) &
               / (abs(new_field(i1, i2, i3, i4) + reference_field(i1, i2, i3, i4)) / 2._r8 + abs(eps))
              if (rel_error > max_rel_error) then
                max_rel_error = rel_error
                value = new_field(i1, i2, i3, i4)
              end if
            end do
          end do
        end do
      end do

      write(f_maximum, 1) max_rel_error, value

      error_level = max(error_level, max_rel_error)

    end if

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_array4dflt_type

  function maximum_relative_error_array5dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, rel_error, value
    integer(itm_i4) :: i1, i2, i3, i4, i5

    character(len = 132) :: f_maximum

    if (size(reference_field) /= size(new_field)) then

      write(f_maximum, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      max_rel_error = 0._r8
      value = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                rel_error = abs(new_field(i1, i2, i3, i4, i5) - reference_field(i1, i2, i3, i4, i5)) &
                 / (abs(new_field(i1, i2, i3, i4, i5) + reference_field(i1, i2, i3, i4, i5)) / 2._r8 + abs(eps))
                if (rel_error > max_rel_error) then
                  max_rel_error = rel_error
                  value = new_field(i1, i2, i3, i4, i5)
                end if
              end do
            end do
          end do
        end do
      end do

      write(f_maximum, 1) max_rel_error, value

      error_level = max(error_level, max_rel_error)

    end if

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_array5dflt_type

  function maximum_relative_error_array6dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, rel_error, value
    integer(itm_i4) :: i1, i2, i3, i4, i5, i6

    character(len = 132) :: f_maximum

    if (size(reference_field) /= size(new_field)) then

      write(f_maximum, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      max_rel_error = 0._r8
      value = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                do i6 = 1, size(reference_field, 6)
                  rel_error = abs(new_field(i1, i2, i3, i4, i5, i6) - reference_field(i1, i2, i3, i4, i5, i6)) &
                   / (abs(new_field(i1, i2, i3, i4, i5, i6) + reference_field(i1, i2, i3, i4, i5, i6)) / 2._r8 + abs(eps))
                  if (rel_error > max_rel_error) then
                    max_rel_error = rel_error
                    value = new_field(i1, i2, i3, i4, i5, i6)
                  end if
                end do
              end do
            end do
          end do
        end do
      end do

      write(f_maximum, 1) max_rel_error, value

      error_level = max(error_level, max_rel_error)

    end if

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_array6dflt_type

  function maximum_relative_error_array7dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_maximum)

    implicit none

    real(r8), parameter :: eps = 1.e-8
    real(r8), dimension(:, :, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: max_rel_error, rel_error, value
    integer(itm_i4) :: i1, i2, i3, i4, i5, i6, i7

    character(len = 132) :: f_maximum

    if (size(reference_field) /= size(new_field)) then

      write(f_maximum, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      max_rel_error = 0._r8
      value = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                do i6 = 1, size(reference_field, 6)
                  do i7 = 1, size(reference_field, 7)
                    rel_error = abs(new_field(i1, i2, i3, i4, i5, i6, i7) - reference_field(i1, i2, i3, i4, i5, i6, i7)) &
                     / (abs(new_field(i1, i2, i3, i4, i5, i6, i7) + reference_field(i1, i2, i3, i4, i5, i6, i7)) / 2._r8 + abs(eps))
                    if (rel_error > max_rel_error) then
                      max_rel_error = rel_error
                      value = new_field(i1, i2, i3, i4, i5, i6, i7)
                    end if
                  end do
                end do
              end do
            end do
          end do
        end do
      end do

      write(f_maximum, 1) max_rel_error, value

      error_level = max(error_level, max_rel_error)

    end if

 1 format(' ---> maximum relative error: ', e9.2, ' --- value at maximum relative error: ', e9.2)

  end function maximum_relative_error_array7dflt_type

  function chi_square_float(diff_counter, error_level, &
   reference_field_float, new_field_float, &
   reference_field_array3dflt_type, new_field_array3dflt_type, &
   reference_field_array4dflt_type, new_field_array4dflt_type, &
   reference_field_array5dflt_type, new_field_array5dflt_type, &
   reference_field_array6dflt_type, new_field_array6dflt_type, &
   reference_field_array7dflt_type, new_field_array7dflt_type, &
   reference_field_matflt_type, new_field_matflt_type, &
   reference_field_vecflt_type, new_field_vecflt_type) &
   result (f_chi2)

    implicit none

    real(r8), intent(in), optional :: reference_field_float, new_field_float
    real(r8), dimension(:, :, :), pointer, optional :: reference_field_array3dflt_type, new_field_array3dflt_type
    real(r8), dimension(:, :, :, :), pointer, optional :: reference_field_array4dflt_type, new_field_array4dflt_type
    real(r8), dimension(:, :, :, :, :), pointer, optional :: reference_field_array5dflt_type, new_field_array5dflt_type
    real(r8), dimension(:, :, :, :, :, :), pointer, optional :: reference_field_array6dflt_type, new_field_array6dflt_type
    real(r8), dimension(:, :, :, :, :, :, :), pointer, optional :: reference_field_array7dflt_type, new_field_array7dflt_type
    real(r8), dimension(:, :), pointer, optional :: reference_field_matflt_type, new_field_matflt_type
    real(r8), dimension(:), pointer, optional :: reference_field_vecflt_type, new_field_vecflt_type
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level

    character(len = 132) :: f_chi2

    if (present(reference_field_vecflt_type)) then
      f_chi2 = chi_square(reference_field_vecflt_type, new_field_vecflt_type, diff_counter, error_level)
    else if (present(reference_field_matflt_type)) then
      f_chi2 = chi_square(reference_field_matflt_type, new_field_matflt_type, diff_counter, error_level)
    else if (present(reference_field_array3dflt_type)) then
      f_chi2 = chi_square(reference_field_array3dflt_type, new_field_array3dflt_type, diff_counter, error_level)
    else if (present(reference_field_array4dflt_type)) then
      f_chi2 = chi_square(reference_field_array4dflt_type, new_field_array4dflt_type, diff_counter, error_level)
    else if (present(reference_field_array5dflt_type)) then
      f_chi2 = chi_square(reference_field_array5dflt_type, new_field_array5dflt_type, diff_counter, error_level)
    else if (present(reference_field_array6dflt_type)) then
      f_chi2 = chi_square(reference_field_array6dflt_type, new_field_array6dflt_type, diff_counter, error_level)
    else if (present(reference_field_array7dflt_type)) then
      f_chi2 = chi_square(reference_field_array7dflt_type, new_field_array7dflt_type, diff_counter, error_level)
    else
      f_chi2 = chi_square(reference_field_float, new_field_float, diff_counter, error_level)
    end if

  end function chi_square_float

  function chi_square_float_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), intent(in) :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2

    character(len = 132) :: f_chi2

    chi2 = (new_field - reference_field)**2

    if (reference_field /= new_field) then
      diff_counter = diff_counter + 1
    end if

    write(f_chi2, 1) chi2

    error_level = error_level + chi2

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_float_type

  function chi_square_vecflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), dimension(:), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2
    integer(itm_i4) :: i1

    character(len = 132) :: f_chi2

    if (size(reference_field) /= size(new_field)) then

      write(f_chi2, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      chi2 = 0._r8

      do i1 = 1, size(reference_field, 1)
        chi2 = chi2 + (new_field(i1) - reference_field(i1))**2
      end do

      write(f_chi2, 1) chi2

      error_level = error_level + chi2

    end if

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_vecflt_type

  function chi_square_matflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), dimension(:, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2
    integer(itm_i4) :: i1, i2

    character(len = 132) :: f_chi2

    if (size(reference_field) /= size(new_field)) then

      write(f_chi2, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      chi2 = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          chi2 = chi2 + (new_field(i1, i2) - reference_field(i1, i2))**2
        end do
      end do

      write(f_chi2, 1) chi2

      error_level = error_level + chi2

    end if

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_matflt_type

  function chi_square_array3dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), dimension(:, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2
    integer(itm_i4) :: i1, i2, i3

    character(len = 132) :: f_chi2

    if (size(reference_field) /= size(new_field)) then

      write(f_chi2, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      chi2 = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            chi2 = chi2 + (new_field(i1, i2, i3) - reference_field(i1, i2, i3))**2
          end do
        end do
      end do

      write(f_chi2, 1) chi2

      error_level = error_level + chi2

    end if

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_array3dflt_type

  function chi_square_array4dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), dimension(:, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2
    integer(itm_i4) :: i1, i2, i3, i4

    character(len = 132) :: f_chi2

    if (size(reference_field) /= size(new_field)) then

      write(f_chi2, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      chi2 = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              chi2 = chi2 + (new_field(i1, i2, i3, i4) - reference_field(i1, i2, i3, i4))**2
            end do
          end do
        end do
      end do

      write(f_chi2, 1) chi2

      error_level = error_level + chi2

    end if

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_array4dflt_type

  function chi_square_array5dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), dimension(:, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2
    integer(itm_i4) :: i1, i2, i3, i4, i5

    character(len = 132) :: f_chi2

    if (size(reference_field) /= size(new_field)) then

      write(f_chi2, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      chi2 = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                chi2 = chi2 + (new_field(i1, i2, i3, i4, i5) - reference_field(i1, i2, i3, i4, i5))**2
              end do
            end do
          end do
        end do
      end do

      write(f_chi2, 1) chi2

      error_level = error_level + chi2

    end if

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_array5dflt_type

  function chi_square_array6dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), dimension(:, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2
    integer(itm_i4) :: i1, i2, i3, i4, i5, i6

    character(len = 132) :: f_chi2

    if (size(reference_field) /= size(new_field)) then

      write(f_chi2, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      chi2 = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                do i6 = 1, size(reference_field, 6)
                  chi2 = chi2 + (new_field(i1, i2, i3, i4, i5, i6) - reference_field(i1, i2, i3, i4, i5, i6))**2
                end do
              end do
            end do
          end do
        end do
      end do

      write(f_chi2, 1) chi2

      error_level = error_level + chi2

    end if

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_array6dflt_type

  function chi_square_array7dflt_type(reference_field, new_field, &
   diff_counter, error_level) result(f_chi2)

    implicit none

    real(r8), dimension(:, :, :, :, :, :, :), pointer :: reference_field, new_field
    integer(itm_i4) :: diff_counter
    real(r8) :: error_level
    real(r8) :: chi2
    integer(itm_i4) :: i1, i2, i3, i4, i5, i6, i7

    character(len = 132) :: f_chi2

    if (size(reference_field) /= size(new_field)) then

      write(f_chi2, '(a132)') 'ERROR: field sizes differ'

    else

      if (any(reference_field /= new_field)) then
        diff_counter = diff_counter + 1
      end if

      chi2 = 0._r8

      do i1 = 1, size(reference_field, 1)
        do i2 = 1, size(reference_field, 2)
          do i3 = 1, size(reference_field, 3)
            do i4 = 1, size(reference_field, 4)
              do i5 = 1, size(reference_field, 5)
                do i6 = 1, size(reference_field, 6)
                  do i7 = 1, size(reference_field, 7)
                    chi2 = chi2 + (new_field(i1, i2, i3, i4, i5, i6, i7) - reference_field(i1, i2, i3, i4, i5, i6, i7))**2
                  end do
                end do
              end do
            end do
          end do
        end do
      end do

      write(f_chi2, 1) chi2

      error_level = error_level + chi2

    end if

 1 format(' ---> chi^2 : ', e9.2)

  end function chi_square_array7dflt_type

end module error_analysis
