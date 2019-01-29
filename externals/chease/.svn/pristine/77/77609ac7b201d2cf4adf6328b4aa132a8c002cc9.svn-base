module string_manipulation_tools

!----------------------------------------------------------------------------
! Set of interfaces for subroutines and functions to manipulate strings
! in FORTRAN.
!----------------------------------------------------------------------------

  use itm_types

  implicit none

  interface char2num
    module procedure char2bool, char2int, char2real
  end interface

  interface num2char
    module procedure bool2char, int2char, real2char
  end interface

  interface scan_str2num
    module procedure scan_str2int, scan_str2real
  end interface

  interface scan_str2varnum
    module procedure scan_str2varint, scan_str2varreal
  end interface

  interface scan_num2str
    module procedure scan_int2str, scan_real2str
  end interface scan_num2str

  type element
    character, dimension(:), allocatable :: cname, cvalue
    type(element), pointer :: parent, child, sibling
  end type element

!-- maximum length of string representations for numbers
  integer(itm_i4), parameter :: max_length_logical = 7
  integer(itm_i4), parameter :: max_length_integer = 16
  integer(itm_i4), parameter :: max_length_real = 32

contains

  function char2str(carray) result(cstring)

    implicit none

    character, dimension(:) :: carray
    character(len = size(carray)) :: cstring

    integer(itm_i4) :: i
    
    do i = 1, size(carray)
      cstring(i : i) = carray(i)
    end do

  end function char2str

  function str2char(cstring) result(carray)

    implicit none

    character(len = *) :: cstring
    character, dimension(len(cstring)) :: carray

    integer(itm_i4) :: i
    
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
    integer(itm_i4) :: cint

    character(len = size(carray)) :: cstring

    cstring = char2str(carray)

    read(cstring, *) cint

  end subroutine char2int

  subroutine char2real(carray, creal)

    implicit none

    character, dimension(:) :: carray
    real(R8) :: creal

    character(len = size(carray)) :: cstring

    cstring = char2str(carray)

    read(cstring, *) creal

  end subroutine char2real

  subroutine bool2char(temp_pointer, lvalue)

    implicit none

    type(element), pointer :: temp_pointer
    logical, intent(in) :: lvalue

    character(len = max_length_logical) :: cstring
    integer(itm_i4) :: length

    write(cstring, *) lvalue

    cstring = adjustl(cstring)
    length = len_trim(cstring)

    allocate(temp_pointer%cvalue(length))
    temp_pointer%cvalue = str2char(cstring(1 : length))

  end subroutine bool2char

  subroutine int2char(temp_pointer, ivalue)

    implicit none

    type(element), pointer :: temp_pointer
    integer(itm_i4), intent(in) :: ivalue

    character(len = max_length_integer) :: cstring
    integer(itm_i4) :: length

    write(cstring, *) ivalue

    cstring = adjustl(cstring)
    length = len_trim(cstring)

    allocate(temp_pointer%cvalue(length))
    temp_pointer%cvalue = str2char(cstring(1 : length))

  end subroutine int2char

  subroutine real2char(temp_pointer, rvalue)

    implicit none

    type(element), pointer :: temp_pointer
    real(R8), intent(in) :: rvalue

    character(len = max_length_real) :: cstring
    integer(itm_i4) :: length

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
    integer(itm_i4) :: value(:)         
    integer(itm_i4) :: nval   
   
    character(len = len(str)) :: cval
    integer(itm_i4)  :: maxval, i, ie, ival, ios
   
    cval = str
    maxval = size(value)     
    nval = 0
! scan string cval
    do i = 1, maxval
      cval = adjustl(trim(cval))               ! remove blanks
      if (cval == '') then
        exit                                   ! exit if empty string
      end if
      ie = scan(cval,' ')                      ! look for end of first token
      if (ie < 1) ie = len_trim(cval)
      read(cval(1 : ie), *, iostat = ios) ival ! convert to integer
      if (ios /= 0) exit                       ! exit if any read error
      nval = nval + 1  
      value(nval) = ival
      cval = adjustl(cval(ie + 1 : ))          ! cut out value just found 
    end do

  end subroutine scan_str2int

  subroutine scan_str2real(str, value, nval)
!  Scans string str for double precision (R8) values, separated 
!  by blanks, and returns nval parameters in value.
!  Converts text to numbers by internal Fortran READ 
    implicit none

    character(len = *) :: str
    real(R8) :: value(:)
    integer(itm_i4) :: nval   
   
    character(len = len(str)) :: cval
    real(R8) :: val
    integer(itm_i4) :: maxval, i, ie, ios
   
    cval = str
    maxval = size(value)      
    nval = 0
! scan string cval
    do i = 1, maxval
      cval = adjustl(trim(cval))               ! remove blanks
      if (cval == '') then
        exit                                   ! exit if empty string
      end if
      ie = scan(cval,' ')                      ! look for end of first token
      if (ie < 1) ie = len_trim(cval)
      read(cval(1 : ie), *, iostat = ios) val  ! convert to real*8
      if (ios /= 0) exit                       ! exit if any read error
      nval = nval + 1  
      value(nval) = val
      cval = adjustl(cval(ie + 1 : ))          ! cut out value just found 
    end do
   
  end subroutine scan_str2real

  subroutine scan_str2varint(str, value, nval)
!  Scans string str for integer values, separated by blanks,
!  and returns nval parameters in value
!  Converts text to numbers by internal Fortran READ 
    implicit none

    character(len = *) :: str
    integer(itm_i4), allocatable :: value(:)         
    integer(itm_i4) :: nval   

    character(len = len(str)) :: cval, cval_safe
    integer(itm_i4)  :: i, ie, ival, ios

    cval_safe = str

    cval = cval_safe
    nval = 0
!-- determine length of 1D array
    do
      cval = adjustl(trim(cval))               ! remove blanks
      if (cval == '') then
        exit                                   ! exit if empty string
      end if
      ie = scan(cval,' ')                      ! look for end of first token
      if (ie < 1) ie = len_trim(cval)
      read(cval(1 : ie), *, iostat = ios) ival ! convert to integer
      if (ios /= 0) exit                       ! exit if any read error
      nval = nval + 1  
      cval = adjustl(cval(ie + 1 : ))          ! cut out value just found 
    end do

    write(*, *) 'length of 1D array : ', nval

    if (allocated(value) .and. nval > size(value)) then
      write(*, *) 'Warning: re-allocation of 1D array necessary'
      deallocate(value)
    end if

    if (.not. allocated(value)) allocate(value(nval))

!-- restore cval
    cval = cval_safe

! scan string cval
    do i = 1, nval
      cval = adjustl(trim(cval))               ! remove blanks
      if (cval == '') then
        exit                                   ! exit if empty string
      end if
      ie = scan(cval,' ')                      ! look for end of first token
      if (ie < 1) ie = len_trim(cval)
      read(cval(1 : ie), *, iostat = ios) ival ! convert to integer
      if (ios /= 0) exit                       ! exit if any read error
      value(i) = ival
      cval = adjustl(cval(ie + 1 : ))          ! cut out value just found 
    end do

  end subroutine scan_str2varint

  subroutine scan_str2varreal(str, value, nval)
!  Scans string str for double precision (R8) values, separated 
!  by blanks, and returns nval parameters in value.
!  Converts text to numbers by internal Fortran READ 
    implicit none

    character(len = *) :: str
    real(R8), allocatable :: value(:)
    integer(itm_i4) :: nval   
   
    character(len = len(str)) :: cval, cval_safe
    real(R8) :: val
    integer(itm_i4) :: i, ie, ios
   
    cval_safe = str

    cval = cval_safe
    nval = 0
!-- determine length of 1D array
    do
      cval = adjustl(trim(cval))               ! remove blanks
      if (cval == '') then
        exit                                   ! exit if empty string
      end if
      ie = scan(cval,' ')                      ! look for end of first token
      if (ie < 1) ie = len_trim(cval)
      read(cval(1 : ie), *, iostat = ios) val  ! convert to real*8
      if (ios /= 0) exit                       ! exit if any read error
      nval = nval + 1  
      cval = adjustl(cval(ie + 1 : ))          ! cut out value just found 
    end do

    write(*, *) 'length of 1D array : ', nval

    if (allocated(value) .and. nval > size(value)) then
      write(*, *) 'Warning: re-allocation of 1D array necessary'
      deallocate(value)
    end if

    if (.not. allocated(value)) allocate(value(nval))

!-- restore cval
    cval = cval_safe

! scan string cval
    do i = 1, nval
      cval = adjustl(trim(cval))               ! remove blanks
      if (cval == '') then
        exit                                   ! exit if empty string
      end if
      ie = scan(cval,' ')                      ! look for end of first token
      if (ie < 1) ie = len_trim(cval)
      read(cval(1 : ie), *, iostat = ios) val  ! convert to real*8
      if (ios /= 0) exit                       ! exit if any read error
      value(i) = val
      cval = adjustl(cval(ie + 1 : ))          ! cut out value just found 
    end do

  end subroutine scan_str2varreal

  subroutine scan_int2str(temp_pointer, ivalues, n)

    implicit none

    type(element), pointer :: temp_pointer
    integer(itm_i4), dimension(:), intent(in) :: ivalues
    integer(itm_i4), intent(in) :: n

    character(len = n * max_length_integer) :: cstring
    character(len = max_length_integer) :: temp_string
    integer(itm_i4) :: i, length

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
    real(R8), dimension(:), intent(in) :: rvalues
    integer(itm_i4), intent(in) :: n

    character(len = n * max_length_real) :: cstring
    character(len = max_length_real) :: temp_string
    integer(itm_i4) :: i, length

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

  subroutine scan_str2str(str, sub_length, value, nval)
!  Scans string str for substrings of maximum length sub_length,
!  separated by blanks, and returns nval parameters in value.
!  Mind that the substrings may not contain any blanks!
    implicit none

    character(len = *), intent(in) :: str
    integer(itm_i4), intent(in) :: sub_length
    character(len = sub_length) :: value(:)
    integer(itm_i4) :: nval   
   
    character(len = len(str)) :: cval
    character(len = sub_length) :: val
    integer(itm_i4) :: maxval, i, ie, ios
   
    cval = str
    maxval = size(value)      
    nval = 0
! scan string cval
    do i = 1, maxval
      cval = adjustl(trim(cval))               ! remove blanks
      if (cval == '') then
        exit                                   ! exit if empty string
      end if
      ie = scan(cval,' ')                      ! look for end of first token
      if (ie < 1) ie = len_trim(cval)
      nval = nval + 1  
      value(nval) = cval(1 : ie)               ! assign substring
      cval = adjustl(cval(ie + 1 : ))          ! cut out substring just found 
    end do
   
  end subroutine scan_str2str


  !> Convert an XML file given in "block mode" (array of 132 character strings with embedded linebreaks,
  !> as e.g. passed by the FC2K wrapper) into a "line mode" representation (array or 132 character strings,
  !> every array entry is one line of the XML file).
  !> 
  !> If this routine encounters a line in the XML file that is more than 132 characters long, it will stop execution.
  subroutine convert_xml_block_to_lines( xml_block, xml_lines )
    character(132), dimension(:), intent(in) :: xml_block
    character(132), dimension(:), allocatable, intent(out) :: xml_lines

    ! internal
    integer :: llen, nlbreak, i, ic, mark
    character(132) :: line

    ! count number of linebreaks in block
    nlbreak = 0 ! number of line breaks
    llen = 0 ! length of current line
    do i = 1, size(xml_block)
        do ic = 1, 132
            if (xml_block(i)(ic:ic) == '\n') then
                llen = llen + ic - 1
                if (llen > 132) stop "convert_xml_block_to_lines: line in XML file with > 132 characters"
                nlbreak = nlbreak + 1
                llen = 0
            end if
        end do
    end do
    if (llen > 132) stop "convert_xml_block_to_lines: line in XML file with > 132 characters"
    
    allocate(xml_lines(nlbreak+1))

    nlbreak = 0
    llen = 0
    mark = 1
    do i = 1, size(xml_block)
        do ic = 1, 132
            if (xml_block(i)(ic:ic) == '\n') then
                ! copy last piece from the mark up to the line break
                line(llen + 1:llen + (ic - mark)) = xml_block(i)(mark : ic - 1)                
                llen = llen + (ic - mark)               
                
                ! store line
                nlbreak = nlbreak + 1
                xml_lines(nlbreak) = repeat(' ', 132)
                xml_lines(nlbreak)(1:llen) = line(1:llen)
                
                ! reset counters
                llen = 0
                mark = ic + 1
            end if
        end do
        ! end of string in block, store piece
        if (mark <= 132) then
            line(llen + 1 : llen + (132 - mark) + 1) = xml_block(i)(mark:132)
            llen = llen + (132 - mark) + 1
            mark = 1
        end if
    end do

    ! cleanup of last line which might not have a linebreak
    if (llen > 0) then
        xml_lines(nlbreak + 1) = repeat(' ', 132)
        xml_lines(nlbreak)(1:llen) = line(1:llen)
    end if
    
  end subroutine convert_xml_block_to_lines


end module string_manipulation_tools
