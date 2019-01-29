module xmllib_pathquery
  !> Module for parsing the xmllib type "tree" using the xml-path.
  !> The tree type can be read by euitm_xml_parse from the euitm_xml_parser module.
  !> The xml-path has to be the absolute path, but excluding the initial "/", 
  !> similar to the find_parameter routine in the euitm_xml_parser module.
  !>
  !> \author Thomas Johnson, johnso@kth.se
  !> \date 2014-10-12
  !>

  use euitm_xml_parser, only: find_parameter, tree, element
  use string_manipulation_tools, only: char2num, str2char, scan_str2num, count_words_in_string
  use itm_types, only: itm_int_invalid, itm_r8_invalid

  implicit none

  interface xmlget
     module procedure xmlget_int, xmlget_int_vec, &
          xmlget_real8, xmlget_real8_vec, &
          xmlget_boolean
  end interface

  interface xmlget_allocatable
     module procedure xmlget_int_vec_allocatable, xmlget_real8_vec_allocatable
  end interface xmlget_allocatable

contains

  !> Number of words in the parameter with xml-path \c param_path within the xml-parameter 
  !> list \c parameter_list. Each word should be separated with at least on blank.
  integer function xmlsize(parameter_list, param_path)
    ! Input/Output
    type(tree) :: parameter_list
    character(len = 132) :: param_path

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val

    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    xmlsize = count_words_in_string(str_val)
  end function xmlsize

  !> Read a scalar integer parameter with xml-path \c param_path
  !> from the list of xml-parameter in \c parameter_list. 
  subroutine xmlget_int(parameter_list, param_path, val, error_flag)
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
  end subroutine xmlget_int

  !> Read a vector of integer parameters with xml-path \c param_path
  !> from the list of xml-parameter in \c parameter_list. 
  !> The vector elements should be separated by blanks.
  !> If the number of parameters in \c param_path (here denoted N) is smaller 
  !> than size(val), then the values of val(N+1:size(val)) are undefined.
  !> If the N > size(val), then only the size(val) first parameters will be read.
  subroutine xmlget_int_vec(parameter_list, param_path, val, vecLength, error_flag)
    ! Input/Output
    type(tree),           intent(in)  :: parameter_list
    character(len = 132), intent(in)  :: param_path
    integer,              intent(out) :: val(:)
    integer, optional,    intent(out) :: vecLength
    integer, optional,    intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val
    integer :: n

    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    if ( len(trim(str_val)) == 0 ) then
       val = itm_int_invalid
       if (present(vecLength)) vecLength=0
       if (present(error_flag)) error_flag=-1
    else
       call scan_str2num(str_val, val, n)
       if (present(vecLength)) vecLength=n
       if (present(error_flag)) error_flag=0
    endif
  end subroutine xmlget_int_vec


  !> Read a scalar double precission parameter with xml-path \c param_path
  !> from the list of xml-parameter in \c parameter_list. 
  subroutine xmlget_real8(parameter_list, param_path, val, error_flag)
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
  end subroutine xmlget_real8


  !> Read a vector of double precission parameters with xml-path \c param_path
  !> from the list of xml-parameter in \c parameter_list. 
  !> The vector elements should be separated by blanks.
  !> If the number of parameters in \c param_path (here denoted N) is smaller 
  !> than size(val), then the values of val(N+1:size(val)) are undefined.
  !> If the N > size(val), then only the size(val) first parameters will be read.
  subroutine xmlget_real8_vec(parameter_list, param_path, val, vecLength, error_flag)
    ! Input/Output
    type(tree),           intent(in)  :: parameter_list
    character(len = 132), intent(in)  :: param_path
    real(8),              intent(out) :: val(:)
    integer, optional,    intent(out) :: vecLength
    integer, optional,    intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val
    integer :: n

    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    if ( len(trim(str_val)) == 0 ) then
       val = itm_r8_invalid
       if (present(vecLength)) vecLength=0
       if (present(error_flag)) error_flag=-1
    else
       call scan_str2num(str_val, val, n)
       if (present(vecLength)) vecLength=n
       if (present(error_flag)) error_flag=0
    endif
  end subroutine xmlget_real8_vec


  !> Read a scalar boolean parameter with xml-path \c param_path
  !> from the list of xml-parameter in \c parameter_list. 
  subroutine xmlget_boolean(parameter_list, param_path, val, error_flag)
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
  end subroutine xmlget_boolean


  !> Read a vector of integer parameters with xml-path \c param_path
  !> from the list of xml-parameter in \c parameter_list. 
  !> The vector elements should be separated by blanks.
  !> The length of the output vector \c val is read determined from xml-element. 
  subroutine xmlget_int_vec_allocatable(parameter_list, param_path, val, error_flag)
    ! Input/Output
    type(tree),           intent(in)  :: parameter_list
    character(len = 132), intent(in)  :: param_path
    integer, allocatable, intent(out) :: val(:)
    integer, optional,    intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val
    integer :: vecLength

    vecLength = xmlsize(parameter_list,param_path)
    if (vecLength==0) then
       allocate(val(0))
       if (present(error_flag)) error_flag=-1
       return
    endif

    allocate(val(vecLength))
    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    call scan_str2num(str_val, val, vecLength)
    if (present(error_flag)) error_flag=0

  end subroutine xmlget_int_vec_allocatable


  !> Read a vector of double precission parameters with xml-path \c param_path
  !> from the list of xml-parameter in \c parameter_list. 
  !> The vector elements should be separated by blanks.
  !> The length of the output vector \c val is read determined from xml-element. 
  subroutine xmlget_real8_vec_allocatable(parameter_list, param_path, val, error_flag)
    ! Input/Output
    type(tree),           intent(in)  :: parameter_list
    character(len = 132), intent(in)  :: param_path
    real(8), allocatable, intent(out) :: val(:)
    integer, optional,    intent(out) :: error_flag

    ! Local
    type(element), pointer :: temp_pointer
    character(len = 132) :: str_val
    integer :: vecLength

    vecLength = xmlsize(parameter_list,param_path)
    if (vecLength==0) then
       allocate(val(0))
       if (present(error_flag)) error_flag=-1
       return
    endif
    allocate(val(vecLength))
    temp_pointer => parameter_list%first
    call find_parameter(param_path , str_val , temp_pointer)
    call scan_str2num(str_val, val, vecLength)
    if (present(error_flag)) error_flag=0

  end subroutine xmlget_real8_vec_allocatable


end module xmllib_pathquery
