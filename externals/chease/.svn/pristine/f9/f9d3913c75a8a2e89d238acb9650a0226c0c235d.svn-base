module xml_file_reader

!-----------------------------------------------------------------------
! This module provides the user with a simple way of filling the
! the structure type_param with the XML code parameters, the XML
! default parameters, and the W3C XML Schema
!-----------------------------------------------------------------------

  use itm_types
  use euitm_schemas

  private

  integer(itm_i4), parameter :: iu6 = 6
  integer(itm_i4), parameter :: unit_no = 20

  public :: fill_param, read_parameters

! modified by me

contains

  subroutine fill_param(code_parameters, input_name, default_name, &
   schema_name, path)
!-----------------------------------------------------------------------
! fill the structure code_parameters of type type_param with the
! contents of the files specified in input_name, default_name, and
! schema_name.
! The optional argument path can be used to specify a path other than
! local.
! If any name is left blank, then no call to read_parameters is carried
! out for the corresponding field.
!-----------------------------------------------------------------------

    implicit none

    type (type_param) :: code_parameters
    character(len = *), intent(in) :: input_name, default_name, schema_name
    character(len = *), intent(in), optional :: path

!-- read input parameters
    if (input_name /= '') then
      if (present(path)) then
        call read_parameters(trim(adjustl(path)) &
         // trim(adjustl(input_name)), code_parameters, 2)
      else
        call read_parameters(input_name, code_parameters, 2)
      end if
    else
      write(iu6, *) 'no input parameters provided'
    end if

!-- read default parameters
    if (default_name /= '') then
      if (present(path)) then
        call read_parameters(trim(adjustl(path)) &
         // trim(adjustl(default_name)), code_parameters, 3)
      else
        call read_parameters(default_name, code_parameters, 3)
      end if
    else
      write(iu6, *) 'no default parameters provided'
    end if

!-- read schema
    if (schema_name /= '') then
      if (present(path)) then
        call read_parameters(trim(adjustl(path)) &
         // trim(adjustl(schema_name)), code_parameters, 1)
      else
        call read_parameters(schema_name, code_parameters, 1)
      end if
    else
      write(iu6, *) 'no schema provided'
    end if

  end subroutine fill_param

  subroutine read_parameters(file_name, code_parameters, element_no)
!-----------------------------------------------------------------------
! reads the text file file_name (including path) into the character
! array code_parameters%schema if element_no == 1, into
! code_parameters%parameters for element_no == 2, else into
! code_parameters%default_param .
! (substructure required because of allocation)
! unit_no specifies the file handle
!-----------------------------------------------------------------------

    implicit none

    character(len = *), intent(in) :: file_name
    type (type_param) :: code_parameters
    integer(itm_i4) :: element_no
 
    character(len = 132) :: ascii_line
    integer(itm_i4) :: ios
    integer(itm_i4) :: n_lines
    integer(itm_i4) :: i
    integer(itm_i4) :: file_length

!-- read input file (e.g. XML, XSD file)
    open(unit = unit_no, file = trim(file_name), status = 'old', &
     action = 'read', iostat = ios)

    if (ios /= 0) then
      write(iu6, *) ' ERROR: ', trim(file_name), 'does not exist!'
      stop
    end if

    write(iu6, *) 'reading ', trim(file_name)

    n_lines = 0

    do
      read (unit_no, '(a)', iostat = ios) ascii_line
      if (ios == 0) then
        n_lines = n_lines + 1
      else
        exit
      end if
    end do

    rewind unit_no

    if (element_no == 1) then
      if (associated(code_parameters%schema)) &
       deallocate(code_parameters%schema)
      allocate(code_parameters%schema(n_lines))
    else if (element_no == 2) then
      if (associated(code_parameters%parameters)) &
       deallocate(code_parameters%parameters)
      allocate(code_parameters%parameters(n_lines))
    else
      if (associated(code_parameters%default_param)) &
       deallocate(code_parameters%default_param)
      allocate(code_parameters%default_param(n_lines))
    end if

    do i = 1, n_lines
      if (element_no == 1) then
        read (unit_no, '(a)', iostat = ios) code_parameters%schema(i)
      else if (element_no == 2) then
        read (unit_no, '(a)', iostat = ios) code_parameters%parameters(i)
      else
        read (unit_no, '(a)', iostat = ios) code_parameters%default_param(i)
      end if
    end do

    close(unit_no)

    write(iu6, *) 'done reading ', trim(file_name)

    return
  end subroutine read_parameters

end module xml_file_reader
