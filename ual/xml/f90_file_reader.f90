module f90_file_reader

  interface file2buffer
     module procedure &
          file2buffer_scalar_allocatable_len, file2buffer_array_len132
  end interface file2buffer

contains

  subroutine file2buffer_scalar_allocatable_len(filename,iounit,buffer)
    character(len=*) :: filename
    integer :: iounit
    character(len=:), allocatable :: buffer

    ! Local
    character(len=:), allocatable :: line
    integer :: ios

    open(unit=iounit, file=filename)
    buffer=''
    do
       call get_line(iounit, line, ios)
       if (ios /= 0) then
          close(iounit)
          exit
       end if
       buffer=buffer//line//' '
       deallocate(line)
    end do
  end subroutine file2buffer_scalar_allocatable_len

  subroutine file2buffer_array_len132(filename, iounit, buffer_pointer, buffer_alloc)
    character(len=*) :: filename
    integer :: iounit
    character(len=132), pointer    , optional :: buffer_pointer(:)
    character(len=132), allocatable, optional :: buffer_alloc(:)

    ! Local
    character(len=:), allocatable :: buffer_string
    integer :: j, n_lines

    call file2buffer(filename, iounit, buffer_string)
    n_lines = int( (len(buffer_string) + 132-1) / 132 )
    if (present(buffer_alloc)) then
       allocate( buffer_alloc(n_lines) )
    endif
    if (present(buffer_pointer)) then
       allocate( buffer_pointer(n_lines) )
    endif
    do j=1,n_lines
       i1 = (j-1)*132+1
       i2 = min(j*132,len(buffer_string))
       if (present(buffer_alloc)) then
          buffer_alloc(j) = buffer_string( i1:i2 )
       endif
       if (present(buffer_pointer)) then
          buffer_pointer(j) = buffer_string( i1:i2 )
       endif
    enddo

  end subroutine file2buffer_array_len132

  subroutine get_line(lun, line, iostat)
    integer, intent(in)           :: lun
    character(len=:), intent(out), allocatable :: line
    integer, intent(out)          :: iostat
    
    integer, parameter            :: buffer_len = 256
    character(len=buffer_len)     :: buffer
    integer                       :: size_read
    
    line = ''
    do
       read ( lun, '(A)',  &
            iostat = iostat,  &
            advance = 'no',  &
            size = size_read ) buffer
       if (is_iostat_eor(iostat)) then
          line = line // buffer(:size_read)
          iostat = 0
          exit
       else if (iostat == 0) then
          line = line // buffer
       else
          exit
       end if
    end do
  end subroutine get_line

end module f90_file_reader
