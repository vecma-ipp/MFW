module string_manipulation_tools

!----------------------------------------------------------------------------
! Set of interfaces for subroutines and functions to manipulate strings
! in FORTRAN.
!----------------------------------------------------------------------------

  use prec_const

  implicit none

  interface char2num
    module procedure char2bool, char2int, char2real
  end interface

  interface num2char
    module procedure int2char, real2char
  end interface

  interface scan_str2num
    module procedure scan_str2int, scan_str2real
  end interface

  interface scan_num2str
    module procedure scan_int2str, scan_real2str
  end interface scan_num2str

  type element
    character, dimension(:), allocatable :: cname, cvalue
    type(element), pointer :: parent, child, sibling
  end type element

!-- maximum length of string representations for numbers
  integer(ikind), parameter :: max_length_integer = 16
  integer(ikind), parameter :: max_length_real = 32

contains

  function char2str(carray) result(cstring)

    implicit none

    character, dimension(:) :: carray
    character(len = size(carray)) :: cstring

    integer(ikind) :: i
    
    do i = 1, size(carray)
      cstring(i : i) = carray(i)
    end do

  end function char2str

  function str2char(cstring) result(carray)

    implicit none

    character(len = *) :: cstring
    character, dimension(len(cstring)) :: carray

    integer(ikind) :: i
    
    do i = 1, len(cstring)
      carray(i) = cstring(i : i)
    end do

  end function str2char

  subroutine char2bool(carray, cbool)

    implicit none

    character, dimension(:) :: carray
    logical :: cbool

    character(len = size(carray)) :: cstring

    cstring = char2str(carray)

    read(cstring, *) cbool

  end subroutine char2bool

  subroutine char2int(carray, cint)

    implicit none

    character, dimension(:) :: carray
    integer(ikind) :: cint

    character(len = size(carray)) :: cstring

    cstring = char2str(carray)

    read(cstring, *) cint

  end subroutine char2int

  subroutine char2real(carray, creal)

    implicit none

    character, dimension(:) :: carray
    real(rkind) :: creal

    character(len = size(carray)) :: cstring

    cstring = char2str(carray)

    read(cstring, *) creal

  end subroutine char2real

  subroutine int2char(temp_pointer, ivalue)

    implicit none

    type(element), pointer :: temp_pointer
    integer(ikind), intent(in) :: ivalue

    character(len = max_length_integer) :: cstring
    integer(ikind) :: length

    write(cstring, *) ivalue

    cstring = adjustl(cstring)
    length = len_trim(cstring)

    allocate(temp_pointer%cvalue(length))
    temp_pointer%cvalue = str2char(cstring(1 : length))

  end subroutine int2char

  subroutine real2char(temp_pointer, rvalue)

    implicit none

    type(element), pointer :: temp_pointer
    real(rkind), intent(in) :: rvalue

    character(len = max_length_real) :: cstring
    integer(ikind) :: length

    write(cstring, *) rvalue

    cstring = adjustl(cstring)
    length = len_trim(cstring)

    allocate(temp_pointer%cvalue(length))
    temp_pointer%cvalue = str2char(cstring(1 : length))

  end subroutine real2char

  subroutine scan_str2int(str, value, nval)
!  Scans string str for integer values, separated by blanks,
!  and returns nval parameters in value
!  Converts text to numbers by internal Fortran READ 
    implicit none

    character(len = *) :: str
    integer(ikind) :: value(:)         
    integer(ikind) :: nval   
   
    character(len = len(str)) :: cval
    integer(ikind)  :: maxval, i, ie, ival, ios
   
    cval = str
    maxval = size(value)     
    nval = 0
! scan string cval
    do i = 1, maxval
      cval = adjustl(cval)                    ! remove leading blanks
      ie = scan(cval,' ')                     ! looking for end of first token
      if (cval == ' ') exit                    ! exit if empty string
      read(cval(1 : ie), *, iostat = ios) ival ! convert to integer
      if (ios /= 0) exit                       ! exit if any read error
      nval = nval + 1  
      value(nval) = ival
      cval = adjustl(cval(ie : ))              ! cut out value just found 
    end do

  end subroutine scan_str2int

  subroutine scan_str2real(str, value, nval)
!  Scans string str for double precision (rkind) values, separated 
!  by blanks, and returns nval parameters in value.
!  Converts text to numbers by internal Fortran READ 
    implicit none

    character(len = *) :: str
    real(rkind) :: value(:)
    integer(ikind) :: nval   
   
    character(len = len(str)) :: cval
    real(rkind) :: val
    integer(ikind) :: maxval, i, ie, ios
   
    cval = str
    maxval = size(value)      
    nval = 0
! scan string cval
    do i = 1, maxval
      cval = adjustl(cval)                    ! remove leading blanks
      ie = scan(cval, ' ')                    ! looking for end of first token
      if (cval == ' ') exit                    ! exit if empty string
      read(cval(1 : ie), *, iostat = ios) val  ! convert to real*8
      if (ios /= 0) exit                       ! exit if any read error
      nval = nval + 1  
      value(nval) = val
      cval = adjustl(cval(ie : ))              ! cut out value just found 
    end do
   
  end subroutine scan_str2real

  subroutine scan_int2str(temp_pointer, ivalues, n)

    implicit none

    type(element), pointer :: temp_pointer
    integer(ikind), dimension(:), intent(in) :: ivalues
    integer(ikind), intent(in) :: n

    character(len = n * max_length_integer) :: cstring
    character(len = max_length_integer) :: temp_string
    integer(ikind) :: i, length

    cstring = ' '
    length = 0

    do i = 1, n
      write(temp_string, *) ivalues(i)
      temp_string = adjustl(temp_string)
      cstring(length + 1 : ) = trim(temp_string) // ' '
      length = length + len_trim(temp_string) + 1
    end do

    allocate(temp_pointer%cvalue(length))
    temp_pointer%cvalue = str2char(cstring(1 : length))

  end subroutine scan_int2str

  subroutine scan_real2str(temp_pointer, rvalues, n)

    implicit none

    type(element), pointer :: temp_pointer
    real(rkind), dimension(:), intent(in) :: rvalues
    integer(ikind), intent(in) :: n

    character(len = n * max_length_real) :: cstring
    character(len = max_length_real) :: temp_string
    integer(ikind) :: i, length

    cstring = ' '
    length = 0

    do i = 1, n
      write(temp_string, *) rvalues(i)
      temp_string = adjustl(temp_string)
      cstring(length + 1 : ) = trim(temp_string) // ' '
      length = length + len_trim(temp_string) + 1
    end do

    allocate(temp_pointer%cvalue(length))
    temp_pointer%cvalue = str2char(cstring(1 : length))

  end subroutine scan_real2str

end module string_manipulation_tools
