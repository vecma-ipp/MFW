module euitm_xml_parser

!----------------------------------------------------------------------------
! Set of routines for parsing an XML string according to
! EUITM specifications (http://www.efda-taskforce-itm.org)
!
! The parser uses a code specific W3C schema in XML format to build a tree
! which will hold the element names in the structure element cname and
! the value of the element in cvalue.
!
! When parsing the input XML file, the corresponding cvalue fields will be
! allocated and filled.
! Not allocated cvalue indicates a field which was not set or a field not at
! the tip of a branch.
!----------------------------------------------------------------------------
 
  use prec_const
  use string_manipulation_tools

  character(len=132) :: file_xml_schema

  type interval
    integer(ikind) :: ind_start, ind_end
  end type interval

  type tree
    type(element), pointer :: first
  end type tree
  integer(ikind) :: n_lines_schema

contains

  subroutine euitm_xml_parse(textv, nparm, parameter_list)
!------------------------------------------------------------------------
!  This subroutine parses the xml string textv in EUITM format 
!  (i.e. array of character strings of a certain length)
!  It first builds the document tree using the XML schema specified
!  in file_xml_schema.
!  It then parses the xml string textv, fills the parsed values into
!  the tree from the schema and returns the complete tree as a
!  parameter_list together with the number of parameters nparm.
!------------------------------------------------------------------------
   
    type(tree) :: parameter_list
    character(len = *)   :: textv(:)
    integer(ikind) :: nparm

    integer(ikind) :: i, length

    print *,'salut3, size(textv)=',size(textv)

    if (size(textv) < 1) then
      return
    else
      write(*, *) 'size of parameters : ', size(textv)
      write(*, *) 'number of XML lines : ', size(textv)
    end if

    call build_xml_tree(parameter_list)

    length = 0
    do i = 1, size(textv)
      if (textv(i) > ' ') then
        length = length + len(trim(adjustl(textv(i)))) + 1
      end if
    end do

    write(*, *) 'length of parameter_string = ', length

    call parse_xml_string(textv, length, nparm, parameter_list)

  end subroutine euitm_xml_parse

  subroutine build_xml_tree(parameter_list)
!-------------------------------------------------------------------------
! This subroutine reads the XML schema and builds a tree from it and
! returns it in parameter_list
!-------------------------------------------------------------------------
    implicit none

    integer(ikind), parameter :: in_schema = 8

    type(tree), intent(out) :: parameter_list

    character(len = 132), allocatable :: lines(:)
    character(len = 132) :: xml_line
    integer(ikind) :: i, n_lines, length, ios
    integer(ikind) :: branches
    
    type(element), pointer :: temp_pointer

!-- read schema

    open (unit = in_schema, file = file_xml_schema, status = 'old', &
     action = 'read', iostat = ios)

    if (ios /= 0) then    
      stop ' ERROR: Input schema does not exist! Cannot build tree. '
    end if
    print *,'salut2'

    n_lines = 0

    do
      read (in_schema, '(a)', iostat = ios) xml_line
      if (ios == 0) then
        n_lines = n_lines + 1
      else
        exit
      end if   
    end do

    write(*, *) 'number of XML lines in schema: ', n_lines
    n_lines_schema = n_lines

    rewind in_schema

    allocate(lines(n_lines))

    do i = 1, n_lines
      read (in_schema, '(a)', iostat = ios) lines(i)
    end do                                    

    close(in_schema)

!-- remove whitespace
    length = 0
    do i = 1, size(lines)
      if (lines(i) > ' ') then
        length = length + len(trim(adjustl(lines(i)))) + 1
      end if
    end do

    write(*, *) 'length of schema_string = ', length

    call build_tree(lines, length, branches, parameter_list)

    deallocate(lines)

!-- run through tree (for test purposes only)
    temp_pointer => parameter_list%first
    do
      if (associated(temp_pointer%child)) then
        temp_pointer => temp_pointer%child
      else if (associated(temp_pointer%sibling)) then
        temp_pointer => temp_pointer%sibling
      else if (associated(temp_pointer%parent)) then
        temp_pointer => temp_pointer%parent
        if (associated(temp_pointer%sibling)) then
          temp_pointer => temp_pointer%sibling
        end if
      end if
      if (associated(temp_pointer%parent, parameter_list%first)) exit
    end do

  end subroutine build_xml_tree

  subroutine build_tree(lines, length, branches, parameter_list)
!--------------------------------------------------------------------------
! This subroutine builds a tree from the schema stored in lines and
! returns it in parameter_list.
!--------------------------------------------------------------------------
    implicit none

    character(len = *), parameter :: root = 'parameters'  ! document element

    type(tree), intent(out) :: parameter_list
    character(len = *), intent(in)   :: lines(:)
    integer(ikind), intent(in) :: length
    integer(ikind), intent(out) :: branches

    type(element), pointer :: temp_pointer
    type(interval) :: full_string
    character(len = length) :: parameter_string
    integer(ikind) :: i, start_index, c_length

!-- concatenate strings in lines without leading or trailing blanks,
!   only separated by a single blank

    parameter_string = ''
    start_index = 1
    do i = 1, size(lines)
      if (lines(i) > ' ') then
        parameter_string(start_index : start_index &
         + len(trim(adjustl(lines(i))))) = ' ' // trim(adjustl(lines(i)))
        start_index = start_index + len(trim(adjustl(lines(i)))) + 1 
      end if
    end do    

    full_string%ind_start = 1
    full_string%ind_end = len(parameter_string)

!-- start list of parameters
    nullify(parameter_list%first)

    allocate(parameter_list%first)

    if (.not. associated (parameter_list%first)) then
      stop 'ERROR: pointer not associated'
    end if

!-- define document element
    allocate(parameter_list%first%cname(len(root)))
    parameter_list%first%cname = str2char(root)

    temp_pointer => parameter_list%first
    branches = 0

!-- recursively parse the W3C XML schema
    c_length = size(temp_pointer%cname)
    call parse_single_knot(parameter_string, temp_pointer, &
     c_length, branches)

  end subroutine build_tree

  recursive subroutine parse_single_knot(parameter_string, parent, length, &
   branches)
!---------------------------------------------------------------------------
! Recursive subroutine which searches parameter_string for occurrence of
! <xs:element name="temp_pointer%cname"
! if found, test whether bracket is closed with />, i.e. no children
! else look for closure </xs:element>.
! In the latter case: find all children (denoted by ref="child_name"),
! create the child knots and call parse_single_knot on child knots.
!---------------------------------------------------------------------------
    implicit none

    integer(ikind), intent(inout) :: branches

    type(element), pointer :: parent
    character(len = *), intent(in) :: parameter_string
    integer(ikind) :: length

    type(element), pointer :: child
    type(interval) :: found
    character(len = length) :: cname
    integer(ikind) :: i, j, no_of_children
    logical :: can_have_children

!-- newborns know no relatives
    nullify(parent%child)
    nullify(parent%sibling)
    nullify(parent%parent)
    cname = char2str(parent%cname)
    found%ind_start = index(parameter_string, '<xs:element name="' &
     // cname // '"')
    if (found%ind_start == 0) then
      write(*, *) 'ERROR: Element ', parent%cname,' not found.'
      return
    end if
    can_have_children = .true.
    found%ind_start = found%ind_start + len_trim('<xs:element name="' &
     // cname // '"')
    i = found%ind_start
    do
      if (parameter_string(i : i + 1) == '/>') then
        can_have_children = .false.
        exit
      else if (parameter_string(i : i) == '>') then
        exit
      else if (parameter_string(i : i) == '<') then
        write(*, *) 'ERROR: not well-formed XML schema.'
        return
      end if
      i = i + 1
    end do
    found%ind_start = i
    if (.not. can_have_children) then
      branches = branches + 1
      return               ! tip of branch reached
    else
!--   find end of element block
      found%ind_end = index(parameter_string(found%ind_start &
       : len_trim(parameter_string)), '</xs:element>')
      if (found%ind_end == 0) then
        write(*, *) 'ERROR: Element ', parent%cname,' not properly closed.'
        return
      end if
      found%ind_end = found%ind_end + found%ind_start - 1
      no_of_children = 0
!--   scan for occurrences of ' ref="', count number of children
      do
        i = index(parameter_string(found%ind_start : found%ind_end), &
         ' ref="') 
        if (i == 0) then
          if (no_of_children == 0) then
            branches = branches + 1
            return                         ! has no children
          else
            child%parent => parent         ! last child
            return
          end if
        else
          i = i + len(' ref="') + found%ind_start - 1
          j = index(parameter_string(i : found%ind_end), '"')
          if (j == 0) then
            write(*, *) 'ERROR: in ref, not well-formed XML.'
            return
          else
            j = j + i - 2
            if (no_of_children == 0) then
              allocate(parent%child)        ! first born child
              child => parent%child
            else
              allocate(child%sibling)       ! next child
              child => child%sibling 
            end if
            allocate(child%cname(j - i + 1))
            child%cname = str2char(parameter_string(i : j))
            no_of_children = no_of_children + 1
            length = size(child%cname)
            call parse_single_knot(parameter_string, child, &
             length, branches)
          end if
        end if
        found%ind_start = j + 2
      end do
    end if

  end subroutine parse_single_knot

  subroutine parse_xml_string(textv, length, nparm, parameter_list)
!---------------------------------------------------------------------------
!  This subroutine does the actual parsing after condensing textv into a
!  single string parameter_string.
!  It requires the resulting string to be a valid XML document following
!  the XML schema specified in file_xml_schema.
!  It browses through the tree which was built from this schema and
!  fills in the values cvalue from the XML document.
!---------------------------------------------------------------------------

    type(tree), intent(inout) :: parameter_list
    character(len = *), intent(in) :: textv(:)
    integer(ikind), intent(in) :: length
    integer(ikind), intent(out) :: nparm

    type(element), pointer :: temp_pointer
    type(interval) :: full_string, bounds
    character(len = length) :: parameter_string
    integer(ikind) :: i, start_index

!-- concatenate strings in textv without leading or trailing blanks,
!   only separated by a single blank

    parameter_string = ''
    start_index = 1
    do i = 1, size(textv)
      if (textv(i) > ' ') then
        parameter_string(start_index : start_index &
         + len(trim(adjustl(textv(i))))) = ' ' // trim(adjustl(textv(i)))
        start_index = start_index + len(trim(adjustl(textv(i)))) + 1 
      end if
    end do    

    full_string%ind_start = 1
    full_string%ind_end = len(parameter_string)

!-- set pointer at root of the tree
    if (.not. associated (parameter_list%first)) then
      stop 'ERROR: pointer not associated'
    else
      temp_pointer => parameter_list%first
    end if
     
!-- parse for document element
    print *,'temp_pointer%cname(:)= ',temp_pointer%cname(:)
    print *,'temp_pointer%cvalue(:)= ',temp_pointer%cvalue(:)
    call parse_xml_element(parameter_string, temp_pointer%cname, &
     full_string, bounds)

    if (bounds%ind_start == 0) then
      write(*, *) 'ERROR: No document element.'
      return
    end if

!-- start tree
    nparm = 0
    if (.not. associated (temp_pointer%child)) then
      stop 'ERROR: not a valid XML document.'
    else
      temp_pointer => temp_pointer%child
    end if

!-- recursively parse XML string based on tree
    call parse_xml_tree(parameter_string, temp_pointer, bounds, nparm)

!-- consistency check
    if (.not. associated(temp_pointer, parameter_list%first)) then
      write(*, *) 'ERROR: incomplete tree.'
      stop
    end if

    write(*,*) 'number of parameters in XML file: ', nparm
   
  end subroutine parse_xml_string

  recursive subroutine parse_xml_tree(parameter_string, tree_pointer, &
   search, nparm)
!--------------------------------------------------------------------------
!  This subroutine parses parameter_string based on the tree built
!  from the schema.
!  It operates recursively through the tree following the child and parent
!  pointers of the tree elements. The sibling pointers are processed
!  within a single call to parse_xml_tree until a parent pointer is
!  encountered.
!--------------------------------------------------------------------------
    implicit none

    type(element), pointer :: tree_pointer
    character(len = *), intent(in) :: parameter_string
    type(interval), intent(in) :: search
    integer(ikind), intent(inout) :: nparm

    type(interval) :: found

    do
      call parse_xml_element(parameter_string, tree_pointer%cname, &
       search, found)
      if (found%ind_start /= 0) then               ! element found
        if (associated(tree_pointer%child)) then   ! has children
          tree_pointer => tree_pointer%child       ! go down
          call parse_xml_tree(parameter_string, tree_pointer, found, nparm)
        else                                       ! contains data
          allocate(tree_pointer%cvalue(len_trim(adjustl(parameter_string(&
           found%ind_start : found%ind_end)))))
          tree_pointer%cvalue = &
           str2char(trim(adjustl(parameter_string(found%ind_start &
            : found%ind_end))))
          nparm = nparm + 1
        end if
      end if
      if (associated(tree_pointer%sibling)) then   ! has siblings
        tree_pointer => tree_pointer%sibling       ! move right
      else
        if (.not. associated(tree_pointer%parent)) then  ! no parent
          write(*, *) 'ERROR: missing parent.'
          return
        else
          tree_pointer => tree_pointer%parent      ! go up
          exit
        end if
      end if
    end do

  end subroutine parse_xml_tree

  subroutine parse_xml_element(parameter_string, ctag, search, found)
!--------------------------------------------------------------------------
!  This subroutine parses parameter_string for the first occurence of the
!  xml element <tag> within the search bounds search.
!  It ignores attributes to the element.
!  It returns the location of the element in found and 0 if not found
!--------------------------------------------------------------------------
    implicit none

    character(len = *) :: parameter_string
    character, dimension(:) :: ctag
    character(len = size(ctag)) :: tag
    type(interval) :: search, found
    integer(ikind) :: i   

    do i = 1, size(ctag)
      tag(i : i) = ctag(i)
    end do
    found%ind_start = index(parameter_string(search%ind_start &
     : search%ind_end), '<' // trim(tag) // '>')    ! try without attributes
    if (found%ind_start == 0) then
      found%ind_start = index(parameter_string(search%ind_start &
       : search%ind_end), '<' // trim(tag) // ' ')  ! try with attributes
    end if
    found%ind_end = index(parameter_string(search%ind_start &
     : search%ind_end), '</' // trim(tag) // '>') - 1
    if (found%ind_start > 0) then
      found%ind_start = search%ind_start + found%ind_start + len_trim(tag) &
       + 1
      do
        if (parameter_string(found%ind_start - 1 : found%ind_start) == '>') &
          exit
        found%ind_start = found%ind_start + 1
      end do
      found%ind_end = search%ind_start + found%ind_end - 1
    end if

  end subroutine parse_xml_element

  subroutine destroy_xml_tree(parameter_list)
!------------------------------------------------------------------------
!  This subroutine deallocates the XML tree in <parameter_list>.
!------------------------------------------------------------------------
    implicit none

    type(tree), intent(inout) :: parameter_list

    type(element), pointer :: temp_pointer
    integer(ikind) :: level

    temp_pointer => parameter_list%first
    level = 0

    call destroy_xml_element(temp_pointer, level)

  end subroutine destroy_xml_tree

  recursive subroutine destroy_xml_element(temp_pointer, level)
!-------------------------------------------------------------------------
! This recursive subroutine destroys a single XML element plus all its
! children through recursive calls.
! The destructor deallocates the fields <cname> and <cvalue> if allocated
! and finally deallocates the pointer to that element.
!-------------------------------------------------------------------------
    implicit none

    type(element), pointer :: temp_pointer
    integer(ikind) :: level

    type(element), pointer :: save_pointer

    do
      if (associated(temp_pointer%child)) then    ! is parent element
        temp_pointer => temp_pointer%child
        level = level + 1
        call destroy_xml_element(temp_pointer, level)
      end if
      if (allocated(temp_pointer%cname)) deallocate(temp_pointer%cname)
      if (allocated(temp_pointer%cvalue)) deallocate(temp_pointer%cvalue)
      if (associated(temp_pointer%sibling)) then
        save_pointer => temp_pointer
        temp_pointer => temp_pointer%sibling
        deallocate(save_pointer)
      else if (associated(temp_pointer%parent)) then
        level = level - 1
        save_pointer => temp_pointer
        temp_pointer => temp_pointer%parent
        exit
      else
        save_pointer => temp_pointer
        if (level > 0) write(*, *) 'ERROR: broken tree.'
        exit
      end if
    end do

    deallocate(save_pointer)

  end subroutine destroy_xml_element

end module euitm_xml_parser
