module c_tools
  use iso_c_binding
  implicit none

  interface 
     subroutine c_dealloc_charbuf(data) bind(c,name='dealloc_charbuf')
       use iso_c_binding
       type(c_ptr) :: data
     end subroutine c_dealloc_charbuf

     subroutine c_file2char(filename, data, size) bind(c,name='file2char')
       use iso_c_binding
       character(kind=c_char), dimension(*), intent(in) :: filename
       type(c_ptr), intent(out) :: data
       integer(kind=c_int), intent(out) :: size
     end subroutine c_file2char

     subroutine c_char2file(filename, data) bind(c,name='char2file')
       use iso_c_binding
       character(kind=c_char), dimension(*), intent(in) :: filename
       character(kind=c_char), dimension(*), intent(in) :: data
     end subroutine c_char2file

     subroutine c_dealloc_bytebuf(data) bind(c,name='dealloc_bytebuf')
       use iso_c_binding
       type(c_ptr) :: data
     end subroutine c_dealloc_bytebuf

     subroutine c_file2byte(filename, data, size) bind(c,name='file2byte')
       use iso_c_binding
       character(kind=c_char), dimension(*), intent(in) :: filename
       type(c_ptr), intent(out) :: data
       integer(kind=c_int), intent(out) :: size
     end subroutine c_file2byte

     subroutine c_byte2file(filename, data, size) bind(c,name='byte2file')
       use iso_c_binding
       character(kind=c_char), dimension(*), intent(in) :: filename
       integer(kind=c_signed_char), dimension(*), intent(in) :: data
       integer(kind=c_int), intent(in), value :: size
     end subroutine c_byte2file
  end interface

contains


  subroutine dealloc_ccharbuf(data)
    use iso_c_binding
    character(kind=c_char), pointer :: data(:)
    type(c_ptr) :: ptr

    ptr = c_loc(data(1))
    call c_dealloc_charbuf(ptr)
  end subroutine dealloc_ccharbuf

  subroutine dealloc_cbytebuf(data)
    use iso_c_binding
    integer(kind=c_signed_char), pointer :: data(:)
    type(c_ptr) :: ptr

    ptr = c_loc(data(1))
    call c_dealloc_bytebuf(ptr)
  end subroutine dealloc_cbytebuf


  subroutine filecharcopy(filename, copyname)
    character(*), intent(in) :: filename, copyname
    character(kind=c_char, len=128) :: c_filename, c_copyname
    integer(kind=c_int) :: c_size
    type(c_ptr) :: c_data_ptr
    character(kind=c_char), pointer :: c_data(:)

    c_filename = trim(filename)//c_null_char
    c_copyname = trim(copyname)//c_null_char

    call c_file2char(c_filename, c_data_ptr, c_size)
    call c_f_pointer(c_data_ptr, c_data, (/c_size/) )
    call c_char2file(c_copyname, c_data)
  end subroutine filecharcopy

  subroutine filebytecopy(filename, copyname)
    character(*), intent(in) :: filename, copyname
    character(kind=c_char, len=128) :: c_filename, c_copyname
    integer(kind=c_int) :: c_size
    type(c_ptr) :: c_data_ptr
    integer(kind=c_signed_char), pointer :: c_data(:)

    c_filename = trim(filename)//c_null_char
    c_copyname = trim(copyname)//c_null_char

    call c_file2byte(c_filename, c_data_ptr, c_size)
    call c_f_pointer(c_data_ptr, c_data, (/c_size/) )
    call c_byte2file(c_copyname, c_data, c_size)    
  end subroutine filebytecopy

  subroutine file2char(filename, data, size)
    character(*), intent(in) :: filename
    character(kind=c_char, len=128) :: c_filename
    character(kind=c_char), pointer :: data(:)
    integer, intent(out) :: size
    integer(kind=c_int) :: c_size
    type(c_ptr) :: data_ptr

    c_filename = trim(filename)//c_null_char    
    call c_file2char(c_filename, data_ptr, c_size)
    call c_f_pointer(data_ptr, data, (/c_size/))
    size = int(c_size)
  end subroutine file2char

  subroutine char2file(filename, data)
    character(*), intent(in) :: filename
    character(kind=c_char, len=128) :: c_filename
    character(kind=c_char), pointer :: data(:)
    
    c_filename = trim(filename)//c_null_char
    call c_char2file(c_filename, data)
  end subroutine char2file

  
  subroutine file2byte(filename, data, size)
    character(*), intent(in) :: filename
    character(kind=c_char, len=128) :: c_filename
    integer(kind=c_signed_char), pointer :: data(:)
    integer, intent(out) :: size
    integer(kind=c_int) :: c_size
    type(c_ptr) :: data_ptr

    c_filename = trim(filename)//c_null_char    
    call c_file2byte(c_filename, data_ptr, c_size)
    call c_f_pointer(data_ptr, data, (/c_size/))
    size = int(c_size)
  end subroutine file2byte

  subroutine byte2file(filename, data, size)
    character(*), intent(in) :: filename
    character(kind=c_char, len=128) :: c_filename
    integer(kind=c_signed_char), pointer :: data(:)
    integer, intent(in) :: size
    integer(kind=c_int) :: c_size

    c_size = size
    c_filename = trim(filename)//c_null_char
    call c_byte2file(c_filename, data, size)    
  end subroutine byte2file

end module c_tools
