!-----------------------------------------------------------------------
! Module that interface to libxml2 
!    Edmondo Giovannozzi (ENEA-EUROFUSION 2010)
!
! Further modification and suggestion:
!    Edmondo Giovannozzi, Silvio Gori, Thomas Jonsson - 2015

module xml2eg_mdl
use iso_c_binding, only: C_PTR, C_CHAR, C_INT, C_NULL_CHAR, c_associated, C_NULL_PTR
implicit none
private

integer,  parameter :: DP = kind(0.0D0) ! double precision
integer,  parameter :: invalid_integer = -999999999
real(DP), parameter :: invalid_real8   = -9d40
real,     parameter :: invalid_real4   = -9d20

type type_xml2eg_document
!    private
    type(C_PTR) :: ptr = C_NULL_PTR
    logical :: error = .true.
end type

type type_xml2eg_element
!    private
    type(C_PTR) :: ptr = C_NULL_PTR
    logical :: error = .true.
end type

interface 
    
! ----  xmlDocPtr xmlParseFile(const char * filename)
    function libxml2_xmlParseFile(filename) result(docm) bind(C,name='xmlParseFile')
        use iso_c_binding
        implicit none
        character(kind=C_CHAR),intent(in) :: filename(*)
        type(C_PTR) :: docm
    end function

! ----  xmlDocPtr xmlParseMemory(const char * buffer, int size)
    function libxml2_xmlParseMemory(buffer, l_buffer) result(docm) bind(C,name='xmlParseMemory')
        use iso_c_binding
        implicit none
        character(kind=C_CHAR),intent(in) :: buffer(*)
        integer(C_INT), value :: l_buffer
        type(C_PTR) :: docm
    end function
    
! ----- xmlNodePtr xmlDocGetRootElement (xmlDocPtr doc)
    function libxml2_xmlDocGetRootElement(document) bind(C,name="xmlDocGetRootElement")
        use iso_c_binding
        implicit none
        type(C_PTR),value :: document
        type(C_PTR) :: libxml2_xmldocgetrootelement
    end function
! --- void xmlFreeDoc (xmlDocPtr cur)    
    subroutine libxml2_xmlFreeDoc(docm) bind(C,name='xmlFreeDoc')
        use iso_c_binding
        implicit none
        type(C_PTR), value :: docm
    end subroutine

    
! -- xmlNodePtr libxml2_f_findNodeByName(xmlNodePtr node, char *name) {
    function libxml2_f_findNodeByName(node, name, l_name) result(element) bind(C,name='libxml2_f_findNodeByName')
    use iso_c_binding
        implicit none
        type(C_PTR),value :: node
        character(kind=C_CHAR),intent(in) :: name(*)
        integer(C_INT),value :: l_name
        type(C_PTR) :: element
    end function
! -- void libxml2_f_NodeGetContent(xmlNodePtr node, char *content, size_t l_content) {
    subroutine libxml2_f_NodeGetContent(node, content, l_content) bind(C,name='libxml2_f_NodeGetContent')
    use iso_c_binding
        implicit none
        type(C_PTR),value :: node
        character(kind=C_CHAR),intent(inout) :: content(*)
        integer(c_size_t),value :: l_content
    end subroutine
!--    void libxml2_f_NodeGetContentRaw(xmlNodePtr node, xmlChar **cont, size_t *l_content)
    subroutine libxml2_f_NodeGetContentRaw(node, cont, l_content)  bind(C,name='libxml2_f_NodeGetContentRaw')
    use iso_c_binding, only: C_CHAR, C_PTR, c_size_t
        implicit none
        type(C_PTR),value :: node
        type(C_PTR),intent(out) :: cont ! it corresponds to "char ** cont" a pointer to a pointer to chars
        integer(c_size_t),intent(out) :: l_content
    end subroutine
!--    void libxml2_f_ContentCopy(xmlChar *cont, char* cstr, size_t l_str)
    subroutine libxml2_f_ContentCopy(cont, cstr, l_str) bind(C,name='libxml2_f_ContentCopy')
    use iso_c_binding, only: C_CHAR, C_PTR, c_size_t
        implicit none
        type(C_PTR),value :: cont
        character(kind=C_CHAR),intent(inout) :: cstr(*)
        integer(c_size_t),value :: l_str 
    end subroutine

end interface

interface  xml2eg_get
    module procedure xml2eg_getString
    module procedure xml2eg_getReal
    module procedure xml2eg_getRealVector
    module procedure xml2eg_getDouble
    module procedure xml2eg_getDoubleVector
    module procedure xml2eg_getInteger
    module procedure xml2eg_getIntegerVector
    module procedure xml2eg_getBoolean
end interface

public xml2eg_parse_file
public xml2eg_parse_memory
public xml2eg_free_doc
public type_xml2eg_document
public type_xml2eg_element
public xml2eg_findelement
public xml2eg_get
contains
!-----------------------------------------------------------------------
! Convert a fortran character(*) array to a C string, that is an array
! of character(kind=C_CHR).
! The output array is an allocatable array that can be passed to
! a C function. the C string is NULL terminated
!
    subroutine stringarray2cstring(str, chpointer)
        character(len=*),intent(in) :: str(:)
        character(kind=C_CHAR), allocatable,intent(out) :: chpointer(:)
        integer :: n, sz, k, i, nl, ik
        
        sz = size(str)
        n = 0
        do i=1,sz
            n = n + len_trim(str(i)) ! no ending space. It is added later so the length is not 1 longer than the actual length
        enddo
        
        n = n + 1 ! A point is added at the end (well to be on a sort of safe side)
                  ! it shouldn't be necessary but I had once a problem so I added it
                  ! it is, by the way, forbidden to have a C array of size 0. 
                  ! while this is allowed in Fortran.
                  
        allocate(chpointer(n)) ! the vector now is as long as it is needed.
        chpointer = ' '          ! As before is better to store a ' ' in the vector
                                 ! it shouldn't be necessary (but do it if you added the 
                                 ! "unnecessary" point, otherwise you may get in trouble during
                                 ! the parsing)
        chpointer(n) = C_NULL_CHAR ! the last point should be NULL
        
        k = 0  ! index inside the chpointer array
        do i=1,sz
            nl = len_trim(str(i))
            do concurrent (ik = 1:nl)
                chpointer(k + ik) = str(i)(ik:ik)
            end do
            k = k + nl 
        enddo
        
    end subroutine stringarray2cstring
!-------------------------------------------------------
! get an array of C_CHAR and create an allocatable string
    subroutine cstr2allocatable_string(cstr, str)
        character(kind=C_CHAR), intent(in) :: cstr(*)
        character(len=:),allocatable,intent(out) :: str
        integer :: n, i
        
        ! length without the ending NULL
        n = clength(cstr)
        
        ! The GFORTRAN needs the following instruction (version 4.7.2)
        ! other compilers can allocate str directly 
        call gfortran_charallocate(str, n)
        
        do concurrent (i=1:n)
            str(i:i) = cstr(i)
        enddo
        
    contains
        subroutine gfortran_charallocate(str, n)
            character(len=:),allocatable :: str
            integer,intent(in) :: n
            allocate(character(len=n) :: str)
        end subroutine
    end subroutine

!-----------------------------------------------------------------------
! length of a C string that is a character(kind=C_CHAR) array.
! the length doesn't include the ending NULL character
! The behaviour is similar to strlen, except that the size is 
! integer instead of C_SIZE_T
    function clength(cstr)
    implicit none
    character(kind=C_CHAR) :: cstr(*)
    integer :: clength
        integer :: i
        
        i = 0
        do 
            i = i + 1
            if (cstr(i) == C_NULL_CHAR) then
                clength = i - 1
                exit
            endif
        enddo
    end function
!-------------------------------------------------------    
    subroutine xml2eg_parse_file(filename, xml2eg_document)
    implicit none
    character(len=*),intent(in) :: filename
    type(type_xml2eg_document),intent(inout) :: xml2eg_document
      
    ! check if a document is present
    if (c_associated(xml2eg_document%ptr)) then
        call xml2eg_free_doc(xml2eg_document)
    endif
    
    xml2eg_document%ptr = libxml2_xmlParseFile(trim(filename)//C_NULL_CHAR)
    xml2eg_document%error = .not. c_associated(xml2eg_document%ptr)
    
    end subroutine

!-------------------------------------------------------    
    subroutine xml2eg_parse_memory(memory, xml2eg_document)
    use iso_c_binding
    implicit none
    type(type_xml2eg_document),intent(inout) :: xml2eg_document
    character(len=*),intent(in) :: memory(:)
    character, allocatable :: chpointer(:)
    
    call stringarray2cstring(memory, chpointer)
    
    ! deallocate any previous document
    if (c_associated(xml2eg_document%ptr)) then
        call xml2eg_free_doc(xml2eg_document)
    endif
    
    xml2eg_document%ptr = libxml2_xmlParseMemory(chpointer, size(chpointer))
    xml2eg_document%error = .not. c_associated(xml2eg_document%ptr)
    
    end subroutine
!-------------------------------------------------------
    subroutine xml2eg_free_doc(xml2eg_document)
        type(type_xml2eg_document) :: xml2eg_document
        
        ! the C routine get the C pointer by value, so the memory is deallocated
        ! but you have to explicitly set the pointer to NULL 
        
        call libxml2_xmlfreeDoc(xml2eg_document%ptr)
        
        ! explicitly set the pointer to NULL
        
        xml2eg_document%ptr = C_NULL_PTR
        xml2eg_document%error = .true.
        
    end subroutine
!-------------------------------------------------------    
    subroutine xml2eg_findelement(xml2eg_document, str, xml2eg_element)
    implicit none
    type(type_xml2eg_document),intent(inout) :: xml2eg_document
    character(len=*),intent(in)  :: str
    type(type_xml2eg_element),intent(out)    :: xml2eg_element
    
    type(type_xml2eg_element)  :: root

    integer :: istart, id, ns
    
    ns  = len_trim(str)
    
    root%ptr = libxml2_xmlDocGetRootElement(xml2eg_document%ptr)
    root%error = .not. c_associated(root%ptr)
    if (root%error) then
        xml2eg_element%error = .TRUE.
        return
    endif
    
    xml2eg_element = root
    istart = 1
    do while (.true.)
        id = index(str(istart:ns),'/')
        if (id == 0 .and. ns-istart >=0) then
            xml2eg_element%ptr  = libxml2_f_findNodeByName(xml2eg_element%ptr, &
                    trim(str(istart:ns))//C_NULL_CHAR, len(trim(str(istart:ns))))
            xml2eg_element%error = .not. c_associated(xml2eg_element%ptr)
            exit
        elseif (id>1) then
            xml2eg_element%ptr  = libxml2_f_findNodeByName(xml2eg_element%ptr, &
                    trim(str(istart:istart+id-2))//C_NULL_CHAR, len(trim(str(istart:istart+id-2))))
            xml2eg_element%error = .not. c_associated(xml2eg_element%ptr)
        else
        endif
        istart  = istart + id
        if (istart > ns) exit
    enddo
    end subroutine
!---------------------------------------------------------
    subroutine xml2eg_getElementContent(xml2eg_document, str, content, errorflag)
    use iso_c_binding, only: C_SIZE_T
    implicit none
    type(type_xml2eg_document),intent(inout) :: xml2eg_document
    character(len=*),intent(in) :: str
    character(len=:),allocatable,intent(out) :: content
    logical, intent(out) :: errorflag
    
        type(type_xml2eg_element) :: elem
        
        
        type(C_PTR) :: cont
        integer(C_SIZE_T) :: l_cont 
        
    
        call xml2eg_findelement(xml2eg_document, str, elem)
        if (elem%error) then
            content = ''
            errorflag=.TRUE.
            ! print *,'XML2EG getElementContent Node: ',trim(str),' is unreachable'
            return
        endif
        
        call libxml2_f_NodeGetContentRaw(elem%ptr, cont, l_cont)
        
        call gfortran_charallocate(content, l_cont)
        
        call libxml2_f_ContentCopy(cont, content, l_cont)
        
        errorflag=.FALSE.
    contains
    ! workaround of a gfortran V4.7 BUG see: Bug: 51055
        subroutine gfortran_charallocate(str, n)
            character(len=:),allocatable :: str
            integer(C_SIZE_T),intent(in) :: n
            allocate(character(len=n) :: str)
        end subroutine
    end subroutine
    
!-------------------------------------------------------
    subroutine xml2eg_getString(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document),intent(inout) :: xml2eg_document
    character(len=*) :: str
    character(len=*) :: out
    logical, optional,intent(out) :: errorflag
    
        character(len=:), allocatable :: content
        logical :: errorflag_internal
    
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        if (present(errorflag)) then
          errorflag=errorflag_internal
        end if
        if (errorflag_internal) then
          ! out = ''
        else
          out = content
        endif
    end subroutine
!--------------------------------------------------------
    subroutine xml2eg_getReal(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document),intent(inout) :: xml2eg_document
    character(len=*) :: str
    real, intent(out) :: out
    logical, optional :: errorflag
    
        character(len=:), allocatable :: content
        logical :: errorflag_internal
        
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        
        if (errorflag_internal) then
          ! out = invalid_real4
        else
           read(content,*) out
        endif
        
        if (present(errorflag)) then
           errorflag = errorflag_internal
        endif
    end subroutine
!--------------------------------------------------------
    subroutine xml2eg_getRealVector(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document) :: xml2eg_document
    character(len=*) :: str
    real :: out(:)
    logical, optional :: errorflag
    
        character(len=:), allocatable :: content
        integer :: ios
        
        logical :: errorflag_internal
        
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        
        if (errorflag_internal) then
          ! out = invalid_real4
        else
           read(content,*,iostat=ios) out
        endif
        
        if (present(errorflag)) then
           errorflag=errorflag_internal
        endif
    end subroutine
!--------------------------------------------------------
    subroutine xml2eg_getDouble(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document) :: xml2eg_document
    character(len=*) :: str
    real(DP) :: out
    logical, optional :: errorflag
    
        character(len=:), allocatable :: content
        integer :: ios
    
        logical :: errorflag_internal
        
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        
        if (errorflag_internal) then
          ! out = invalid_real8
        else
           read(content,*, iostat=ios) out
        endif
        
        if (present(errorflag)) then
           errorflag=errorflag_internal
        endif
        
    end subroutine
!--------------------------------------------------------
    subroutine xml2eg_getDoubleVector(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document) :: xml2eg_document
    character(len=*) :: str
    real(DP) :: out(:)
    logical, optional :: errorflag
    
        character(len=:), allocatable :: content
        integer :: ios
    
        logical :: errorflag_internal
        
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        
        if (errorflag_internal) then
          ! out = invalid_real8
        else
           read(content,*,iostat=ios) out
        endif
        
        if (present(errorflag)) then
           errorflag = errorflag_internal
        endif
        
    end subroutine
!--------------------------------------------------------
    subroutine xml2eg_getInteger(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document) :: xml2eg_document
    character(len=*) :: str
    integer :: out
    logical, optional :: errorflag
    
        character(len=:), allocatable :: content
        
        logical :: errorflag_internal
        
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        
        if (errorflag_internal) then
          ! out = invalid_integer
        else
            read(content,*) out
        endif
        
        if (present(errorflag)) then
           errorflag=errorflag_internal
        endif
    end subroutine
!--------------------------------------------------------
    subroutine xml2eg_getIntegerVector(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document) :: xml2eg_document
    character(len=*) :: str
    integer :: out(:)
    logical, optional :: errorflag
    
        character(len=:), allocatable :: content
        integer :: ios
        
        logical :: errorflag_internal
        
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        
        if (errorflag_internal) then
          ! out = invalid_integer
        else
           read(content,*, iostat=ios) out
        endif
        
        if (present(errorflag)) then
           errorflag = errorflag_internal
        endif
    end subroutine
!--------------------------------------------------------
    subroutine xml2eg_getBoolean(xml2eg_document, str, out, errorflag)
    implicit none
    type(type_xml2eg_document) :: xml2eg_document
    character(len=*) :: str
    logical :: out
    logical, optional :: errorflag
    
        character(len=:), allocatable :: content
        logical :: errorflag_internal
        
        call xml2eg_getElementContent(xml2eg_document, str, content, errorflag_internal)
        if (errorflag_internal) then
          ! out = .FALSE.
        else
           out = ( trim(adjustl(content)) == 'true' )
        endif
        
        if (present(errorflag)) then
           errorflag=errorflag_internal
        endif
    end subroutine
end module
