module xml_path
  !> Module for parsing the xmllib type "tree" using the xml-path.
  !> The tree type can be read by euitm_xml_parse from the euitm_xml_parser module.
  !> The xml-path has to be the absolute path, but excluding the initial "/", 
  !> similar to the find_parameter routine in the euitm_xml_parser module.
  !>
  !> \author Thomas Johnson, johnso@kth.se
  !> \date 2014-10-12
  !>

  use euitm_xml_parser, only: find_parameter, tree, element
  use string_manipulation_tools, only: char2num, str2char, scan_str2num
  use itm_types, only: itm_int_invalid, itm_r8_invalid

  implicit none

  interface xmllib_read
     module procedure xmllib_read_int, xmllib_read_int_vec, &
          xmllib_read_real8, xmllib_read_real8_vec, &
          xmllib_read_boolean
  end interface

contains

  subroutine xmllib_read_int(parameter_list, param_path, val, error_flag)
    ! Input/Output
    type(tree) :: parameter_list
    character(len = 132), intent(in) :: param_path
    integer, intent(out) :: val
    integer, optional, intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val

    temp_pointer => parameter_list%first

    call find_parameter(param_path , str_val , temp_pointer)
    if ( len(trim(str_val)) == 0 ) then
       val = itm_int_invalid
       if (present(error_flag)) error_flag=-1
    else
       call char2num(str2char(str_val), val)
       if (present(error_flag)) error_flag=0
    endif
  end subroutine xmllib_read_int


  subroutine xmllib_read_int_vec(parameter_list, param_path, vecLength, val, error_flag)
    ! Input/Output
    type(tree),           intent(in)  :: parameter_list
    character(len = 132), intent(in)  :: param_path
    integer,              intent(in)  :: vecLength
    integer,              intent(out) :: val(vecLength)
    integer, optional,    intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val

    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    if ( len(trim(str_val)) == 0 ) then
       val = itm_int_invalid
       if (present(error_flag)) error_flag=-1
    else
       call scan_str2num(str_val, val, vecLength)
       if (present(error_flag)) error_flag=0
    endif
  end subroutine xmllib_read_int_vec


  subroutine xmllib_read_real8(parameter_list, param_path, val, error_flag)
    ! Input/Output
    type(tree),           intent(in)  :: parameter_list
    character(len = 132), intent(in)  :: param_path
    real(8),              intent(out) :: val
    integer, optional,    intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val

    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    if ( len(trim(str_val)) == 0 ) then
       val = itm_r8_invalid
       if (present(error_flag)) error_flag=-1
    else
       call char2num(str2char(str_val), val)
       if (present(error_flag)) error_flag=0
    endif
  end subroutine xmllib_read_real8


  subroutine xmllib_read_real8_vec(parameter_list, param_path, vecLength, val, error_flag)
    ! Input/Output
    type(tree),           intent(in)  :: parameter_list
    character(len = 132), intent(in)  :: param_path
    integer,              intent(in)  :: vecLength
    real(8),              intent(out) :: val(vecLength)
    integer, optional,    intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val

    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    if ( len(trim(str_val)) == 0 ) then
       val = itm_r8_invalid
       if (present(error_flag)) error_flag=-1
    else
       call scan_str2num(str_val, val, vecLength)
       if (present(error_flag)) error_flag=0
    endif
  end subroutine xmllib_read_real8_vec


  subroutine xmllib_read_boolean(parameter_list, param_path, val, error_flag)
    ! Input/Output
    type(tree) :: parameter_list
    character(len = 132), intent(in) :: param_path
    logical, intent(out) :: val
    integer, optional, intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val

    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    if ( len(trim(str_val)) == 0 ) then
       val=.FALSE.
       if (present(error_flag)) error_flag=-1
    else
       call char2num(str2char(str_val), val)
       if (present(error_flag)) error_flag=0
    endif
  end subroutine xmllib_read_boolean


end module xml_path
