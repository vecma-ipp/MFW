module string_binding
  use iso_c_binding
  implicit none

  integer, parameter :: BUF_SIZE = 1024*1024*10
  integer, parameter :: F_STR_SIZE = 128

contains

  subroutine str_c2f(cstr,fstr)  
    character(kind=c_char,len=1), dimension(*), intent(in) :: cstr
    character(F_STR_SIZE), intent(out) :: fstr
    integer :: i
    fstr = ' '
    loop: do i=1,F_STR_SIZE
       if (cstr(i) == c_null_char) then
          exit loop
       else
          fstr(i:i) = cstr(i)
       end if
    end do loop
  end subroutine str_c2f

  subroutine str_f2c(fstr, cstr)
    character(len=*), intent(in)  :: fstr
    character(kind=c_char, len=1) :: cstr(len_trim(fstr)+1)
    integer :: i,size

    size = len_trim(fstr)
    do i = 1, size
       cstr(i) = fstr(i:i)
    end do
    cstr(size + 1) = c_null_char

  end subroutine str_f2c


end module string_binding
